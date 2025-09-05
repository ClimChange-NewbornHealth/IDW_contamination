## From monitors to municipalities: assigning air pollution exposure in Chile using IDW

:mailbox_with_mail: Estela Blanco (<estela.blanco@uc.cl>) -  **Principal Investigator**

:paperclip: Felipe Cornejo -  **Principal Investigator**

:mailbox_with_mail: José Daniel Conejeros (<jdconejeros@uc.cl>) - **Research Collaborator - Repository Manager**

DOI GitHub Repo stars GitHub watchers GitHub forks GitHub commit activity GitHub contributors GitHub last commit GitHub language count GitHub top language GitHub License GitHub repo file or directory count GitHub code size in bytes

This repository contains code and outputs to assign daily municipal-level exposure to fine particulate matter (PM2.5) and ozone (O3) across Chile using inverse distance weighting (IDW) based on observations from the national air quality monitoring network.

Contact: José Daniel Conejeros (jdconejeros@uc.cl) — Repository Manager

License: MIT

### Background

Epidemiologic and policy analyses often require spatially complete air pollution exposures. However, monitoring stations are unevenly distributed. We use spatial interpolation to generate daily, municipality-level estimates of PM2.5 and O3 from observed concentrations at monitoring stations.

### Objective

Assign daily PM2.5 and O3 exposure at the municipal level for Chile by interpolating observations from the national monitoring network and exporting analysis-ready tables for linkage with population datasets (e.g., vital statistics).

### Methods

- **Data source**: Daily pollutant concentrations from Chile’s National Air Quality Information System (SINCA). See [SINCA – Sistema de Información Nacional de Calidad del Aire](https://sinca.mma.gob.cl/).
- **Interpolation approach**: Inverse Distance Weighting (IDW) implemented with the `gstat` package in R.
- **Target locations**: Municipalities represented by their reported coordinates/centroids (EPSG:4326 transformed to UTM 19S, EPSG:32719 for distance calculations).
- **Core parameters** (as configured in the provided script and adjustable):
  - Power (idp) = 2
  - Maximum neighbors (nmax) = 4–5
  - Maximum search radius (maxdist) = 10–15 km
- **Temporal scope**: Example window in the script is set to 2015-01-01 through 2015-02-01 for demonstration; the underlying dataset spans 2010–2023 and can be reconfigured to other periods.
- **Missingness**: The input time series provided in `01 Data` are imputed with missForest prior to interpolation (see `series_imputated_missforest_2010_2023.RData`).

Key functions implemented in `02 Code/IDW_Contaminantes.R`:

- `idw_para_munis(...)`: Performs IDW for a single municipality on a given date.
- `idw_iterado(...)`: Iterates IDW over all municipalities and dates to produce a full time series.

Software and main R packages:

- R, `sf`, `gstat`, `sp`, `dplyr`, `tidyr`, `ggplot2`, `readxl`, `stringr`, `tidyverse`, `magick`, `viridis`.

### Data and Code

- **Input data**: `01 Data/series_imputated_missforest_2010_2023.RData` (contains station-level daily series and station metadata: latitude, longitude, station id).
- **Main script**: `02 Code/IDW_Contaminantes.R` (builds daily municipal exposure for PM2.5 and O3; can export CSV and simple GIF maps for a specified date window).

### Outputs

Example artifacts produced by the script:

- `03 Output/idw_pm25.csv`: Daily municipal PM2.5 (wide, municipalities in columns).
- `03 Output/idw_o3.csv`: Daily municipal O3 (wide, municipalities in columns).
- `03 Output/births_PM25_idw_exposure.csv`: Example merged exposure file (if linkage performed externally).
- `03 Output/births_O3_idw_exposure.csv`: Example merged exposure file (if linkage performed externally).
- `pm25_animado.gif` and `o3_animado.gif`: Optional animated maps for a selected date range.

### Repository structure

```
01 Data/
  series_imputated_missforest_2010_2023.RData
02 Code/
  IDW_Contaminantes.R
03 Output/
  idw_pm25.csv
  idw_o3.csv
  births_PM25_idw_exposure.csv
  births_O3_idw_exposure.csv
LICENSE
README.md
```

### Reproducibility and usage

1) Open `02 Code/IDW_Contaminantes.R` in R/RStudio.
2) Ensure required packages are installed. For example:

```r
install.packages(c(
  "sf", "gstat", "sp", "dplyr", "tidyr", "ggplot2",
  "readxl", "stringr", "tidyverse", "magick", "viridis"
))
```

3) Provide the auxiliary geographic inputs referenced in the script:
   - `geo_data/Comunas/comunas.shp` (municipal boundaries)
   - `geo_data/municipalidades.xlsx` (municipality names and coordinates)

4) Adjust the date range in the script if needed (see the `filter(date >= ...)` lines) and tuning parameters (`power`, `max_radius`, `max_vecinos`).
5) Run the script to generate municipal-level IDW exposure time series and optional visualizations.

### Acknowledgements

- We acknowledge data from the Chilean National Air Quality Information System (SINCA) — [SINCA](https://sinca.mma.gob.cl/).

### Notes

- This README structure is inspired by the project “From the Atacama to Patagonia: understanding the effects of extreme temperatures on birth weight across climate regions in Chile,” available here: [CIIIA-ClimateBirthWeightAnalysis](https://github.com/ClimChange-NewbornHealth/CIIIA-ClimateBirthWeightAnalysis).

### License

MIT — see `LICENSE` for details.
