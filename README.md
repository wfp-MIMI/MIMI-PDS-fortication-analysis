# MIMI-PDS Fortification Analysis

Replication code for the analysis of food fortification impact on micronutrient adequacy in India, using data from the 2022–23 Household Consumption and Expenditure Survey (HCES). The analysis focuses on the Public Distribution System (PDS) and evaluates fortified rice and wheat flour under India's current mandatory standard and an improved WFP specification.

## Overview

This repository supports three main objectives:

1. **Inadequacy mapping** — Estimate the prevalence of inadequate micronutrient intake (folate, vitamin B12, iron) across NSS regions under four scenarios: base case, rice fortified, wheat flour fortified, both fortified.
2. **PDS reach and consumption** — Characterise reach and per-capita consumption of PDS rice and wheat flour at state level and project the nutrient contribution under India vs. WFP fortification specifications.
3. **Ration card analysis** — Compare micronutrient inadequacy by ration card type (AAY, PHH, BPL, APL, SFSS) and estimate the impact of fortification for PDS-eligible households.

## Repository structure

```
.
├── data/                             
│   ├── nsso_202223_fct.xlsx          # India food composition table (NSSO 2022–23)
│   ├── conversion_factors.csv        # Dietary conversion factors
|   |                                 # None of the data below is included in the repo
│   ├── processed/
│   │   ├── ind_nss2223_base_case.rds          # Household micronutrient intake (base)
│   │   └── ind_nss2223_food_consumption.rds   # Food consumption by household and item
│   └── raw/
│       ├── HCES_2022_23/             # Raw survey stata files 
│       └── shapefiles/
│           ├── India-State-and-Country-Shapefile-Updated-Jan-2020-master/  # downloaded from https://github.com/AnujTiwari/India-State-and-Country-Shapefile-Updated-Jan-2020
│           └── ind_nss2223_nssregion.*       # Shapefiles matched from adm2 level to NSS-region originally from SHRUG https://www.devdatalab.org/shrug
├── functions/
│   ├── aggregated_inadequacy.R   # Survey-weighted inadequacy by group (scripts 1–2)
│   ├── general_inadequacy.R      # Survey-weighted inadequacy, flexible grouping (script 3)
│   └── contributions.R           # Fortified nutrient contribution per household
└── src/
    ├── 1_fortification.R         # Build fortification scenarios; save processed outputs
    ├── 2_inadequacy_maps.R       # Generate inadequacy and bivariate maps
    └── 3_ration_card_tables.R    # Ration card subgroup tables (objectives 1–3)
```

## Data access

The raw HCES 2022–23 microdata (`data/raw/HCES_2022_23/`) are not included in this repository. They are available from the Ministry of Statistics and Programme Implementation (MoSPI), Government of India:

> https://www.mospi.gov.in/web/mospi/download-tables-data/-/reports/view/templateTwo/25302?q=HCES


## Running the analysis

Scripts should be run in order from the project root. All paths are relative to the project root; use the `MIMI-PDS-fortication-analysis.Rproj` file to set the working directory automatically.

```
1_fortification.R   →  builds fortification scenario intake data
2_inadequacy_maps.R →  produces maps (requires output of step 1)
3_ration_card_tables.R → produces HTML tables (requires output of step 1)
```

Outputs are written to:
- `data/processed/` — intermediate RDS and CSV files
- `figures/` — maps (JPG/PNG) and HTML tables

## Dependencies

All required packages are installed automatically at the top of each script. Key packages:

| Package | Purpose |
|---------|---------|
| `tidyverse`, `dplyr` | Data manipulation |
| `srvyr`, `survey` | Survey-weighted estimation |
| `haven` | Reading Stata `.dta` files |
| `sf`, `tmap`, `biscale` | Spatial analysis and mapping |
| `readxl` | Reading the food composition table |
| `gt`, `flextable` | Formatted output tables |
| `devtools` | Fetching shared iron probability function from GitHub |

The iron full-probability inadequacy function (`fe_full_prob`) is sourced at runtime from the [MIMI-R-functions](https://github.com/MIMI-wfp/MIMI-R-functions) repository and requires an internet connection.

## Fortification specifications

Two fortification standards are implemented for both rice and wheat flour:

| Standard | Vehicle | Iron (mg/100g) | Folate (µg/100g) | Vitamin B12 (µg/100g) |
|----------|---------|---------------|-----------------|----------------------|
| India (mandatory) | Rice | 3.53 | 10 | 0.10 |
| India (mandatory) | Wheat flour | 1.76 | 8.3 | 0.09 |
| WFP (improved) | Rice | 7.00 | 130 | 1.00 |
| WHO (improved) | Wheat flour | 2.00 | 108 | 0.85 |

Post-processing retention factors are applied for losses of wheat


## Citation

TBC for publication

If using this code, please cite the underlying survey:
> Ministry of Statistics and Programme Implementation (2024). *Household Consumption and Expenditure Survey 2022–23*. Government of India.



## Contact

Gabriel Battcock — World Food Programme (WFP) / MIMI Project
