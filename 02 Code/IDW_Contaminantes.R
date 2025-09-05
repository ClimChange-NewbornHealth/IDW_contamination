# Cargar librerías necesarias
library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(gstat)
library(sp)
library(tidyverse)
library(magick)
library(viridis)

# Cargar datos geográficos
comunas <- st_read("geo_data/Comunas/comunas.shp", quiet = TRUE)
munis <- read_excel("geo_data/municipalidades.xlsx")

# Cargar datos de contaminantes
load("series_imputated_missforest_2010_2023.RData")
estaciones <- df %>%  distinct(station_id, station, lat, long)

comunas <- st_transform(comunas, crs = 4326)  # Asegurar CRS

# Filtrar comunas de Santiago y Puente Alto
comunas_santiago <- comunas %>%
  filter(Provincia == "Santiago" | Comuna == "Puente Alto")

munis <- munis %>%
  separate(municipalidad, into = c("lat", "long"), sep = ",", convert = TRUE) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

estaciones <- estaciones %>%  st_as_sf(coords = c("long", "lat"), crs = 4326)

idw_para_munis <- function(datos, munis, comuna_objetivo, fecha,
                           power = 2, max_radius = 10000, max_vecinos = 5) {
  
  # Verificar si la comuna objetivo existe en el dataset de municipalidades
  if (!(comuna_objetivo %in% munis$comuna)) {
    stop("La comuna no existe en 'munis'")
  }
  
  # Seleccionar la fila correspondiente a la comuna y transformar a coordenadas UTM (para cálculo de distancia)
  muni_sf <- munis %>%
    filter(comuna == comuna_objetivo) %>%
    st_transform(32719)  # EPSG:32719 = UTM zona 19 sur (Chile)
  
  # Filtrar los datos para la fecha deseada y eliminar filas sin valores
  datos_filtrados <- datos %>%
    filter(date == fecha, !is.na(valor)) %>%
    distinct(station, lat, long, valor)  # Eliminar duplicados si los hay
  
  # Si no hay datos para esa fecha, advertir y devolver la comuna con valor NA
  if (nrow(datos_filtrados) == 0) {
    warning("No hay datos disponibles para la fecha indicada.")
    muni_sf$interpolado <- NA
    return(muni_sf)
  }
  
  # Convertir las estaciones en un objeto sf, usando latitud y longitud como coordenadas
  # Luego transformarlas a UTM para usar en interpolación por distancia
  estaciones_sf <- st_as_sf(datos_filtrados, coords = c("long", "lat"), crs = 4326) %>%
    st_transform(32719)
  
  # Crear columna 'valor' para usar en la fórmula del modelo IDW
  estaciones_sf$valor <- estaciones_sf$valor
  
  # Crear el modelo de interpolación IDW con parámetros ajustables:
  # - idp = power (potencia de decaimiento con la distancia)
  # - nmax = máximo número de estaciones vecinas a considerar
  # - maxdist = radio máximo para considerar estaciones (en metros)
  modelo <- gstat(
    formula = valor ~ 1,  # Interpolación simple (solo en función de la distancia)
    locations = estaciones_sf,
    nmax = max_vecinos,
    maxdist = max_radius,
    set = list(idp = power)
  )
  
  # Aplicar el modelo a la municipalidad: predecir el valor interpolado
  resultado <- predict(modelo, muni_sf)
  
  # Guardar el resultado interpolado en la columna 'interpolado' del objeto sf
  muni_sf$interpolado <- resultado$var1.pred
  
  # Devolver la municipalidad con el valor interpolado de PM2.5
  return(muni_sf)
}

idw_iterado <- function(datos, munis,
                        power = 2,
                        max_radius = 10000,
                        max_vecinos = 5,
                        verbose = TRUE) {
  
  # Asegurar que las columnas mínimas existen
  required_cols <- c("station", "lat", "long")
  if (!all(required_cols %in% colnames(datos))) {
    stop("El data frame debe contener las columnas: station, lat, long")
  }
  
  # Extraer las fechas desde los nombres de las columnas
  fechas_unicas <- unique(datos$date)
  comunas_unicas <- unique(munis$comuna)
  
  # Lista para guardar resultados
  resultados <- list()
  
  total_iter <- length(comunas_unicas) * length(fechas_unicas)
  iter <- 0
  
  for (comuna in comunas_unicas) {
    for (fecha in fechas_unicas) {
      iter <- iter + 1
      if (verbose && iter %% 50 == 0) {
        message("Procesando ", iter, " de ", total_iter, " (", comuna, " - ", fecha, ")")
      }
      
      # Preparar datos para esa fecha
      datos_fecha <- datos %>%
        filter(date == fecha) %>%
        select(station, lat, long, valor, date)
      
      # Saltar si no hay datos válidos
      if (nrow(datos_fecha) == 0) {
        muni_na <- munis %>%
          filter(comuna == comuna) %>%
          mutate(interpolado = NA, fecha = as.Date(fecha))
        resultados[[length(resultados) + 1]] <- muni_na
        next
      }
      
      # Interpolar usando idw_para_munis
      resultado <- tryCatch({
        idw_para_munis(
          datos = datos_fecha,
          munis = munis,
          comuna_objetivo = comuna,
          fecha = as.Date(fecha),
          power = power,
          max_radius = max_radius,
          max_vecinos = max_vecinos
        )
      }, error = function(e) {
        muni_na <- munis %>%
          filter(comuna == comuna) %>%
          mutate(interpolado = NA, fecha = as.Date(fecha))
        return(muni_na)
      })
      
      resultado$fecha <- as.Date(fecha)
      resultados[[length(resultados) + 1]] <- resultado
    }
  }
  
  # Combinar resultados en un solo sf
  resultado_final <- do.call(rbind, resultados)
  return(resultado_final)
}

# Crear tabla pivotada con valores de PM2.5
df_pm25 <- df %>%
  filter(date >= as.Date("2015-01-01") & date <= as.Date("2015-02-01")) %>%
  select(station, lat, long, date, daily_pm25) %>%
  pivot_wider(
    names_from = date,
    values_from = daily_pm25
  ) %>%
  distinct(station, .keep_all = TRUE)  # Para evitar duplicados de coordenadas

df_pm25_largo <- df_pm25 %>%
  pivot_longer(
    cols = -c(station, lat, long),
    names_to = "date",
    values_to = "valor"
  ) %>%
  mutate(date = as.Date(date))

# Crear tabla pivotada con valores de O3
df_o3 <- df %>%
  filter(date >= as.Date("2015-01-01") & date <= as.Date("2015-02-01")) %>%
  select(station, lat, long, date, daily_o3) %>%
  pivot_wider(
    names_from = date,
    values_from = daily_o3
  ) %>%
  distinct(station, .keep_all = TRUE)

df_o3_largo <- df_o3 %>%
  pivot_longer(
    cols = -c(station, lat, long),
    names_to = "date",
    values_to = "valor"
  ) %>%
  mutate(date = as.Date(date))

idw_pm25 <- idw_iterado(
  datos = df_pm25_largo,       # Dataframe con datos PM2.5 y geometría
  munis = munis,      # Municipalidades con coordenadas
  power = 2,          # Parámetro de potencia 
  max_radius = 15000, # Radio máximo de búsqueda en metros (15km)
  max_vecinos = 4     # Número máximo de vecinos
)

idw_o3 <- idw_iterado(
  datos = df_o3_largo,         # Dataframe con datos O3 y geometría
  munis = munis,      # Municipalidades con coordenadas
  power = 2,          # Parámetro de potencia
  max_radius = 15000, # Radio máximo de búsqueda en metros (15km)
  max_vecinos = 4     # Número máximo de vecinos
)

idw_pm25_wide <- idw_pm25 %>%
  select(comuna, fecha, interpolado) %>%
  pivot_wider(names_from = comuna, values_from = interpolado)

idw_o3_wide <- idw_o3 %>%
  select(comuna, fecha, interpolado) %>%
  pivot_wider(names_from = comuna, values_from = interpolado)


#write.csv(idw_pm25_wide,"idw_pm25.csv", row.names = FALSE)
#write.csv(idw_o3_wide,"idw_o3.csv", row.names = FALSE)

# Crear carpeta para guardar imágenes temporales
dir.create("frames", showWarnings = FALSE)

# Fechas que quieres animar (puedes ajustar el rango aquí)
fechas_animadas <- unique(df$date)
fechas_animadas <- seq(as.Date("2015-01-01"), as.Date("2015-02-01"), by = "1 day")

# Convertir a data.frame quitando la geometría (guardamos comuna, interpolado y fecha)
idw_pm25_df <- idw_pm25 %>%
  st_drop_geometry()

# GIF PM2.5
# Crear y guardar mapas

for (i in seq_along(fechas_animadas)) {
  fecha_i <- fechas_animadas[i]
  
  comunas_valores <- comunas_santiago %>%
    left_join(
      idw_pm25_df %>% filter(fecha == fecha_i),
      by = c("Comuna" = "comuna")
    ) %>%
    mutate(
      centroide = st_centroid(geometry),
      lon = st_coordinates(centroide)[, 1],
      lat = st_coordinates(centroide)[, 2]
    )
  
  comunas_valores_df <- as.data.frame(comunas_valores)
  
  p <- ggplot() +
    geom_sf(data = comunas_valores, aes(fill = interpolado), color = "white") +
    geom_text(
      data = comunas_valores_df,
      aes(x = lon, y = lat, label = round(interpolado, 1)),
      color = "black", size = 1.5, fontface = "bold", inherit.aes = FALSE
    ) +
    scale_fill_viridis(option = "D", name = "PM2.5", na.value = "grey90", limits = c(5, 35)) +
    labs(
      title = "PM2.5 interpolado por comuna",
      subtitle = paste("Fecha:", fecha_i)
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(sprintf("frames/pm25_frame_%03d.png", i), plot = p, width = 6, height = 6)
}

# Leer imágenes y unirlas como gif
imagenes_pm25 <- list.files("frames", pattern = "^pm25_frame_\\d+\\.png$", full.names = TRUE)
gif_pm25 <- image_read(imagenes_pm25) %>%
  image_animate(fps = 2)

# Guardar GIF
image_write(gif_pm25, "pm25_animado.gif")
browseURL("pm25_animado.gif")

# Convertir a data.frame quitando la geometría (guardamos comuna, interpolado y fecha)
idw_o3_df <- idw_o3 %>%
  st_drop_geometry()

# GIF O3
# Crear y guardar mapas
for (i in seq_along(fechas_animadas)) {
  fecha_i <- fechas_animadas[i]
  
  comunas_valores <- comunas_santiago %>%
    left_join(
      idw_o3_df %>% filter(fecha == fecha_i),
      by = c("Comuna" = "comuna")
    ) %>%
    mutate(
      centroide = st_centroid(geometry),
      lon = st_coordinates(centroide)[, 1],
      lat = st_coordinates(centroide)[, 2]
    )
  
  comunas_valores_df <- as.data.frame(comunas_valores)
  
  p <- ggplot() +
    geom_sf(data = comunas_valores, aes(fill = interpolado), color = "white") +
    geom_text(
      data = comunas_valores_df,
      aes(x = lon, y = lat, label = round(interpolado, 1)),
      color = "black", size = 1.5, fontface = "bold", inherit.aes = FALSE
    ) +
    scale_fill_viridis(option = "A", name = "O3", na.value = "grey90", limits = c(4, 30)) +
    labs(
      title = "O3 interpolado por comuna",
      subtitle = paste("Fecha:", fecha_i)
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(sprintf("frames/o3_frame_%03d.png", i), plot = p, width = 6, height = 6)
}

# Leer imágenes y unirlas como gif
imagenes_o3 <- list.files("frames", pattern = "^o3_frame_\\d+\\.png$", full.names = TRUE)
gif_o3 <- image_read(imagenes_o3) %>%
  image_animate(fps = 2)

# Guardar GIF
image_write(gif_o3, "o3_animado.gif")
browseURL("o3_animado.gif")
