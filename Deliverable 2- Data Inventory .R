# ============================================================================
# FOOD INSECURITY ALONG THE U.S.–MEXICO BORDER
# Complete Data Collection, Preparation, and Cleaning Script
# Team 15: Manav Diler & Paige Wagner
# November 2025
# ============================================================================

library(readxl)
library(tidyverse)
library(stringr)

# ============================================================================
# STEP 1: LOAD ALL DATA SOURCES
# ============================================================================

cat("Loading data sources...\n")

Unemployment <- read_excel("Border Food Security/Unemployment2023.xlsx", skip = 4)
Poverty <- read_excel("Border Food Security/Poverty2023.xlsx", skip = 4)
PopulationEstimates <- read_excel("Border Food Security/PopulationEstimates.xlsx", skip = 4)
Education2023 <- read_excel("Border Food Security/Education2023.xlsx", skip = 3)
FoodAccess <- read_excel("Border Food Security/2025-food-environment-atlas-data.xlsx",
                         sheet = "ACCESS", skip = 1)
FeedingAmerica <- read_excel("Border Food Security/MMG2025_Data_ToShare/MMG2025_2019-2023_Data_To_Share.xlsx",
                             sheet = "County")

cat("✓ All data sources loaded\n\n")

# ============================================================================
# STEP 2: STANDARDIZE FIPS CODES
# ============================================================================

cat("Standardizing FIPS codes...\n")

# Rename FIPS columns where needed
PopulationEstimates <- PopulationEstimates %>%
  rename(FIPS_Code = FIPStxt)

Education2023 <- Education2023 %>%
  rename(FIPS_Code = `FIPS Code`)

FoodAccess <- FoodAccess %>%
  rename(FIPS_Code = FIPS)

FeedingAmerica <- FeedingAmerica %>%
  rename(FIPS_Code = FIPS)

# Force ALL FIPS_Code columns to be 5-character strings
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

cat("✓ FIPS codes standardized\n\n")

# ============================================================================
# STEP 3: MERGE ALL DATASETS
# ============================================================================

cat("Merging datasets...\n")

clean_data <- Unemployment %>%
  left_join(Poverty,           by = "FIPS_Code", suffix = c("", "_pov")) %>%
  left_join(PopulationEstimates, by = "FIPS_Code", suffix = c("", "_pop")) %>%
  left_join(Education2023,     by = "FIPS_Code", suffix = c("", "_edu")) %>%
  left_join(FoodAccess,        by = "FIPS_Code", suffix = c("", "_fa")) %>%
  left_join(FeedingAmerica,    by = "FIPS_Code", suffix = c("", "_mmg"))

cat("✓ Datasets merged:", nrow(clean_data), "rows,", ncol(clean_data), "columns\n\n")

# ============================================================================
# STEP 4: CREATE ANALYSIS DATASET WITH KEY VARIABLES
# ============================================================================

cat("Creating analysis dataset...\n")

# Find the correct State / County columns after the joins.
# After the first join, Unemployment$State and $Area_Name keep their names (no suffix),
# later datasets’ State/Area_Name get suffixed.
state_col  <- "State"
county_col <- "Area_Name"

if (!state_col %in% colnames(clean_data)) {
  stop("Could not find a 'State' column after merging – inspect clean_data names().")
}
if (!county_col %in% colnames(clean_data)) {
  stop("Could not find an 'Area_Name' column after merging – inspect clean_data names().")
}

analysis_data <- clean_data %>%
  distinct() %>%
  # keep only rows where Feeding America food insecurity is defined
  filter(!is.na(`Overall Food Insecurity Rate`)) %>%
  select(
    FIPS_Code,
    State       = all_of(state_col),
    County      = all_of(county_col),
    Year,  # from Feeding America (2019–2023)
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
    Poverty_Rate_Pct          = PCTPOVALL_2023,
    Median_Income             = Median_Household_Income_2022,
    Population                = POP_ESTIMATE_2023,
    Education_Bachelor_Pct    = `Percent of adults with a bachelor's degree or higher, 2019-23`,
    Low_Access_Pct            = PCT_LACCESS_POP19,
    Food_Insecurity_Rate      = `Overall Food Insecurity Rate`,
    Child_Food_Insecurity_Rate = `Child Food Insecurity Rate`,
    Food_Insecure_Persons     = `# of Food Insecure Persons Overall`,
    Snap_Threshold            = `SNAP Threshold`,
    Pct_FI_Below_Snap         = `% FI ≤ SNAP Threshold`
  ) %>%
  mutate(
    Border_State = if_else(State %in% c("TX", "NM", "AZ", "CA"),
                           "Border State", "Non-Border State")
  )

cat("✓ Analysis dataset created:", nrow(analysis_data), "rows\n\n")

# ============================================================================
# STEP 5: DEFINE 21 OFFICIAL U.S.–MEXICO BORDER COUNTIES
# ============================================================================

cat("Defining 21 border counties...\n")

border_counties_list <- c(
  # TEXAS (13 counties)
  "El Paso County, TX",
  "Hudspeth County, TX",
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
  # NEW MEXICO (3 counties)
  "Hidalgo County, NM",
  "Luna County, NM",
  "Dona Ana County, NM",
  # ARIZONA (4 counties)
  "Cochise County, AZ",
  "Santa Cruz County, AZ",
  "Pima County, AZ",
  "Yuma County, AZ",
  # CALIFORNIA (1 county)
  "San Diego County, CA"
)

cat("✓ Border counties defined:", length(border_counties_list), "\n\n")

# ============================================================================
# STEP 6: ADD BORDER COUNTY INDICATOR
# ============================================================================

cat("Adding border county indicator...\n")

analysis_data <- analysis_data %>%
  mutate(
    Is_Border_County = if_else(County %in% border_counties_list,
                               "Border County", "Non-Border")
  )

cat("✓ Border indicator added\n\n")

# ============================================================================
# STEP 7: DATA QUALITY CHECKS
# ============================================================================

cat("=== DATA QUALITY REPORT ===\n")
cat("Total rows:", nrow(analysis_data), "\n")
cat("Total columns:", ncol(analysis_data), "\n")
cat("Years covered:", paste(sort(unique(analysis_data$Year)), collapse = ", "), "\n")
cat("Total states:", n_distinct(analysis_data$State), "\n")
cat("Total counties:", n_distinct(analysis_data$County), "\n\n")

cat("=== BORDER COUNTY BREAKDOWN ===\n")
print(table(analysis_data$Is_Border_County))
cat("Distinct border counties:",
    n_distinct(analysis_data$County[analysis_data$Is_Border_County == "Border County"]), "\n")
cat("Border observations (county × year):",
    sum(analysis_data$Is_Border_County == "Border County"), "\n\n")

cat("=== MISSING VALUES BY COLUMN ===\n")
print(colSums(is.na(analysis_data)))
cat("\n")

cat("=== SAMPLE BORDER COUNTIES DATA ===\n")
print(
  analysis_data %>%
    filter(Is_Border_County == "Border County") %>%
    select(FIPS_Code, State, County, Year,
           Food_Insecurity_Rate, Poverty_Rate_Pct, Is_Border_County) %>%
    head(10)
)
cat("\n")

# ============================================================================
# STEP 8: EXPORT FINAL DATASETS
# ============================================================================

cat("=== EXPORTING DATASETS ===\n")

# Export complete dataset
write_csv(analysis_data, "Border_Food_Security_All_Data_Final.csv")
cat("✓ Exported: Border_Food_Security_All_Data_Final.csv\n")

# Export border counties only
border_data <- analysis_data %>% filter(Is_Border_County == "Border County")
write_csv(border_data, "Border_Counties_Only.csv")
cat("✓ Exported: Border_Counties_Only.csv\n")

# Export by border states (for convenience in Tableau/Flourish)
for (state in c("TX", "NM", "AZ", "CA")) {
  state_data <- analysis_data %>% filter(State == state)
  filename <- paste0("Border_Food_Security_", state, ".csv")
  write_csv(state_data, filename)
  cat("✓ Exported:", filename, "-", nrow(state_data), "rows\n")
}

# ============================================================================
# STEP 9: FINAL SUMMARY
# ============================================================================

cat("\n====================================================================\n")
cat("DATA PREPARATION COMPLETE\n")
cat("====================================================================\n")
cat("Total observations:", format(nrow(analysis_data), big.mark = ","), "\n")
cat("Total variables   :", ncol(analysis_data), "\n")
cat("Years             :", paste(sort(unique(analysis_data$Year)), collapse = ", "), "\n")
cat("States            :", n_distinct(analysis_data$State), "\n")
cat("Counties          :", n_distinct(analysis_data$County), "\n")
cat("Border counties   :", n_distinct(analysis_data$County[analysis_data$Is_Border_County == 'Border County']), "\n")
cat("Border obs        :", sum(analysis_data$Is_Border_County == 'Border County'), "\n")
cat("====================================================================\n\n")
