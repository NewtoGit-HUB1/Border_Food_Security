# ============================================================================
# FOOD INSECURITY ALONG THE U.S.–MEXICO BORDER
# Data Collection, Preparation, and Analysis Script
# Team 15: Manav Diler & Paige Wagner
# November 2025
# ============================================================================

library(readxl)
library(tidyverse)
library(stringr)

# Loading data from all sources
Unemployment <- read_excel("Border Food Security/Unemployment2023.xlsx", skip = 4)
Poverty <- read_excel("Border Food Security/Poverty2023.xlsx", skip = 4)
PopulationEstimates <- read_excel("Border Food Security/PopulationEstimates.xlsx", skip = 4)
Education2023 <- read_excel("Border Food Security/Education2023.xlsx", skip = 3)
FoodAccess <- read_excel("Border Food Security/2025-food-environment-atlas-data.xlsx",
                         sheet = "ACCESS", skip = 1)
FeedingAmerica <- read_excel("Border Food Security/MMG2025_Data_ToShare/MMG2025_2019-2023_Data_To_Share.xlsx",
                             sheet = "County")

# Standardizing FIPS codes across datasets
PopulationEstimates <- PopulationEstimates %>%
  rename(FIPS_Code = FIPStxt)

Education2023 <- Education2023 %>%
  rename(FIPS_Code = `FIPS Code`)

FoodAccess <- FoodAccess %>%
  rename(FIPS_Code = FIPS)

FeedingAmerica <- FeedingAmerica %>%
  rename(FIPS_Code = FIPS)

# Converting all FIPS codes to 5-character format
Unemployment <- Unemployment %>%
  mutate(FIPS_Code = str_pad(as.character(FIPS_Code), 5, pad = "0"))

Poverty <- Poverty %>%
  mutate(FIPS_Code = str_pad(as.character(FIPS_Code), 5, pad = "0"))

PopulationEstimates <- PopulationEstimates %>%
  mutate(FIPS_Code = str_pad(as.character(FIPS_Code), 5, pad = "0"))

Education2023 <- Education2023 %>%
  mutate(FIPS_Code = str_pad(as.character(FIPS_Code), 5, pad = "0"))

FoodAccess <- FoodAccess %>%
  mutate(FIPS_Code = str_pad(as.character(FIPS_Code), 5, pad = "0"))

FeedingAmerica <- FeedingAmerica %>%
  mutate(FIPS_Code = str_pad(as.character(FIPS_Code), 5, pad = "0"))

# Combining datasets by county
clean_data <- Unemployment %>%
  left_join(Poverty,             by = "FIPS_Code", suffix = c("", "_pov")) %>%
  left_join(PopulationEstimates, by = "FIPS_Code", suffix = c("", "_pop")) %>%
  left_join(Education2023,       by = "FIPS_Code", suffix = c("", "_edu")) %>%
  left_join(FoodAccess,          by = "FIPS_Code", suffix = c("", "_fa")) %>%
  left_join(FeedingAmerica,      by = "FIPS_Code", suffix = c("", "_mmg"))

# Building focused analysis dataset
analysis_data <- clean_data %>%
  distinct() %>%
  filter(!is.na(`Overall Food Insecurity Rate`)) %>%
  select(
    FIPS_Code,
    State       = "State",
    County      = "Area_Name",
    Year,
    Unemployment_rate_2023,
    PCTPOVALL_2023,
    Median_Household_Income_2022,
    POP_ESTIMATE_2023,
    `Percent of adults with a bachelor's degree or higher, 2019-23`,
    PCT_LACCESS_POP19,
    `Overall Food Insecurity Rate`,
    `Child Food Insecurity Rate`,
    `# of Food Insecure Persons Overall`,
    `SNAP Threshold`,
    `% FI ≤ SNAP Threshold`
  ) %>%
  rename(
    Poverty_Rate_Pct           = PCTPOVALL_2023,
    Median_Income              = Median_Household_Income_2022,
    Population                 = POP_ESTIMATE_2023,
    Education_Bachelor_Pct     = `Percent of adults with a bachelor's degree or higher, 2019-23`,
    Low_Access_Pct             = PCT_LACCESS_POP19,
    Food_Insecurity_Rate       = `Overall Food Insecurity Rate`,
    Child_Food_Insecurity_Rate = `Child Food Insecurity Rate`,
    Food_Insecure_Persons      = `# of Food Insecure Persons Overall`,
    Snap_Threshold             = `SNAP Threshold`,
    Pct_FI_Below_Snap          = `% FI ≤ SNAP Threshold`
  ) %>%
  mutate(
    Low_Access_Pct = na_if(Low_Access_Pct, -9999),
    Border_State   = if_else(State %in% c("TX", "NM", "AZ", "CA"),
                             "Border State", "Non-Border State")
  )

# Identifying all 24 U.S. border counties
border_counties_list <- c(
  # TEXAS (15 counties)
  "El Paso County, TX",
  "Hudspeth County, TX",
  "Jeff Davis County, TX",
  "Presidio County, TX",
  "Brewster County, TX",
  "Terrell County, TX",
  "Val Verde County, TX",
  "Kinney County, TX",
  "Maverick County, TX",
  "Webb County, TX",
  "Zapata County, TX",
  "Starr County, TX",
  "Hidalgo County, TX",
  "Cameron County, TX",
  "Willacy County, TX",
  # NEW MEXICO (3 counties)
  "Hidalgo County, NM",
  "Luna County, NM",
  "Dona Ana County, NM",
  # ARIZONA (4 counties)
  "Cochise County, AZ",
  "Santa Cruz County, AZ",
  "Pima County, AZ",
  "Yuma County, AZ",
  # CALIFORNIA (2 counties)
  "Imperial County, CA",
  "San Diego County, CA"
)

# Flagging border counties
analysis_data <- analysis_data %>%
  mutate(
    Is_Border_County = if_else(County %in% border_counties_list,
                               "Border County", "Non-Border")
  )

# Exporting final dataset
write_csv(analysis_data, "Border_Food_Security_All_Data_Final_UPDATED.csv")
