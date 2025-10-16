# SIDES-RUE — Information System on the Emergency Care Network of Minas Gerais

**Author:** Henrique Bracarense  
**Commissioned by:** Secretaria de Estado de Saúde de Minas Gerais (SES-MG)  
**Project:** Research Project on the SUS (Brazilian Unified Health System)  
**Live app:** https://geesc.shinyapps.io/sides_rue/  
**Type:** R Shiny interactive dashboard  
**Scope:** Emergency and Urgency Care Network (RUE) performance indicators — State of Minas Gerais, Brazil

---

## Overview

**SIDES-RUE** is an analytical web application built in **R Shiny** to visualize and interpret the main indicators of the **Emergency Care Network (Rede de Urgência e Emergência — RUE)** in Minas Gerais.  
It was developed under a research project on the SUS and commissioned by the **Secretariat of Health of Minas Gerais**.  

The system brings together regulatory, structural, and performance information related to RUE, focusing on **acute myocardial infarction (AMI/IAM)** and **stroke (CVA/AVC)** lines of care.

---

## Data Sources and Components

- **`indicadores_ppsus.xlsx`** — master workbook with indicators (hospitals, micro and macro health regions, and IBGE reference data).  
- **`timedata.rds`** — regulatory timeline data used in the “Regulatory Framework” tab.  
- **Shapefiles** (`mg.*`, `mg_micros.*`, `mg_macros.*`) — spatial boundaries for Minas Gerais, and health micro/macro regions (used by Leaflet).  
- **Static resources** in `www/`:
  - Icons for summary cards and markers (`icone_total.png`, `icone_evasao.png`, `icone_tx_transf.png`, `blue.png`, `green.png`, etc.).
  - SES-MG banner (`banner.jpeg`) and logos.
  - Federal and state legal documents (`portaria_*.pdf`, `deliberacao_*.pdf`).

---

## Functionalities

The application is organized into **five main tabs**, implemented in `app24.R`:

1. **Presentation (“Apresentação”)**  
   Introductory text with project goals, acknowledgments, and scope.

2. **Regulatory Framework (“Marco Regulatório”)**  
   Interactive **timeline** (`timevis`) of **federal and state ordinances**.  
   Items are grouped and color coded by jurisdiction (Federal vs. State).

3. **Hospital Establishments (“Estabelecimentos”)**  
   **Leaflet** map with hospital locations, grouped by habilitation/credential status for IAM/AVC and “Rede Resposta”.  
   Uses Minas Gerais’ shapefiles; supports exploration by micro/macro health region.

4. **Typology of Health Regions (“Tipologia”)**  
   Regional typology based on **service availability, performance, and hospital occupancy**.  
   Continuous color scale (red → green) emphasizes lower to higher ratings.  
   Aggregation switch for **macro** vs **micro** health regions.

5. **Indicators (“Indicadores”)**  
   Charts and tables for core metrics, including:
   - **Total volume of cases**
   - **Evasion (escape) rates**
   - **Transfer rates**
   Rendered with **Plotly** (interactive) and **DT** (data tables). Summary cards use custom icons.

---

## Key Packages

| Component | Packages |
|---|---|
| Core web framework | `shiny`, `shinydashboard`, `shinyWidgets`, `shinythemes`, `shinyjs` |
| Data import/manipulation | `tidyverse`, `readxl`, `data.table` |
| Maps | `leaflet`, `tmap`, `cartography`, `rgdal`, `sp`, `spdep`, `rgeos` |
| Timeline | `timevis` |
| Charts & Tables | `plotly`, `DT` |
| Export | `writexl` |

> Depending on your system, you may need GDAL/GEOS/PROJ libraries for geospatial packages (`rgdal`, `rgeos`, `sp`), and `sf` if you extend the app.

---

## File Structure

```
sides_rue_zip/
├─ app24.R                     # Main R Shiny app
├─ indicadores_ppsus.xlsx      # Indicator dataset (hospitals, micro/macro regions, IBGE)
├─ timedata.rds                # Regulatory timeline data
└─ www/
   ├─ banner.jpeg, logo.jpeg
   ├─ icone_total.png, icone_evasao.png, icone_tx_transf.png
   ├─ mg.*, mg_macros.*, mg_micros.* (shapefiles)
   ├─ portaria_*.pdf, deliberacao_*.pdf (regulatory docs)
   └─ marker/color assets (blue.png, green.png, etc.)
```

---

## How to Run Locally

1. Install **R ≥ 4.1** and the required packages:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "shinythemes", "shinyjs",
  "tidyverse", "readxl", "leaflet", "tmap", "cartography", "rgdal",
  "sp", "spdep", "rgeos", "timevis", "plotly", "DT", "data.table", "writexl"
))
```

2. Place all files in a single project directory.

3. Launch from R or RStudio:
```r
shiny::runApp('app24.R')
```

---

## Notes

- Geospatial files must remain under `www/` for Leaflet to load boundaries correctly.
- Excel sheet names and expected columns in `indicadores_ppsus.xlsx` should not be changed unless you adapt the parsing code.
- If you add new indicators, extend both the data workbook and the rendering logic in `app24.R` (plots, tables, and cards).

---

## Citation

If you use this application or any of its derived outputs, please cite:

> **Bracarense, Henrique** (2025). *SIDES-RUE — Information System on the Emergency Care Network of Minas Gerais*. R Shiny application, commissioned by the Secretariat of Health of Minas Gerais (SES-MG), under the Research Project on the SUS.  
> URL: https://geesc.shinyapps.io/sides_rue/

---

## License

Unless otherwise specified by SES-MG, this code is released under an MIT-style license for academic and non-commercial use.  
Check underlying datasets and regulatory documents for their individual licensing terms.
