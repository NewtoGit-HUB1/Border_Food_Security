# Food Insecurity Analysis Along the U.S.–Mexico Border

## Project Overview

This project integrates data from three major U.S. government agencies to create a comprehensive dataset on food insecurity, poverty, and food access across U.S. counties, with focus on the U.S.–Mexico border region.

## Data Sources

- **U.S. Census Bureau**: Unemployment, Poverty, Population, Education
- **USDA Economic Research Service**: Food Access Research Atlas with food desert indicators and low-access populations
- **Feeding America**: Map the Meal Gap with food insecurity rates (2019-2023)

## Files

- `Deliverable 2- Data Inventory .R` - Main data cleaning and preparation script
- `Border_Food_Security_All_Data_Final_UPDATED.csv` - Complete merged dataset with all counties and 14 key variables

## Border Counties Definition

24 U.S. counties that directly border Mexico:
- **Texas** (15): El Paso, Hudspeth, Jeff Davis, Presidio, Brewster, Terrell, Val Verde, Kinney, Maverick, Webb, Zapata, Starr, Hidalgo, Cameron, Willacy
- **New Mexico** (3): Hidalgo, Luna, Dona Ana
- **Arizona** (4): Cochise, Santa Cruz, Pima, Yuma
- **California** (2): Imperial, San Diego

## Key Variables

- FIPS_Code, State, County, Year, Is_Border_County
- Unemployment_rate_2023, Poverty_Rate_Pct, Median_Income, Population, Education_Bachelor_Pct
- Low_Access_Pct, Food_Insecurity_Rate, Child_Food_Insecurity_Rate, Food_Insecure_Persons
- Snap_Threshold, Pct_FI_Below_Snap

## Team

Team 15: Manav Diler & Paige Wagner
