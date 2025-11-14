# LIBRARIES
library(here)
library(dplyr)
library(tidyr)


# LOAD DATA
# food data
food_data <- read.csv(here("Data", "ar6_food_demand_comparison_regions.csv"))
# scenario meta data
scenario_meta_data <- read.csv(here(
  "Data",
  "ar6_metadata_25.09.2025.csv"
))
# AR6 data
df <- read.csv(here("Data", "AR6_Snapshot_22.02.2024.csv"))

mapping_df <- read.csv(here("Data", "dle_thresholds.csv"))
sdp_mapping_df <- read.csv(here("Data", "sdp_thresholds.csv"))



# WRANGLE DATA

# ensure all column names are lowercase
colnames(food_data) <- tolower(colnames(food_data))
colnames(scenario_meta_data) <- tolower(colnames(scenario_meta_data))
colnames(df) <- tolower(colnames(df))


# clean food_df
food_df <- food_data %>%
  # convert wide to long format by placing all years in one column
  pivot_longer(
    cols = starts_with("x"), names_to = "year",
    values_to = "value"
  ) %>%
  # clean up year column
  mutate(year = sub("^x", "", year)) %>%
  # clean up region column
  mutate(region = sub(".*\\|", "", region)) %>%
  # omit years with no data
  na.omit() %>%
  # rename variable
  mutate(variable = ifelse(variable == "Food Demand|Livestock",
    "Food_Demand_Livestock", variable
  )) %>%
  # drop all scenarios that only have one region for a indicator
  # reason:distributional justice is a comparative concept
  group_by(model, scenario, variable) %>%
  mutate(region_count = n_distinct(region)) %>%
  filter(region_count > 1) %>%
  select(-region_count) %>%
  # convert year to numeric
  mutate(year = as.numeric(year)) %>%
  # change per kcal per capita per day to kcal per region per day
  ## reason: to allow for a uniform application of later functions,
  ## which divide by population to get per capita values
  select(-unit) %>%
  group_by(model, scenario, region, year) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  na.omit() %>%
  mutate(
    Population = Population * 1e+06, # Unit individual people
    # Unit: kcal per capita per day * population = kcal per region per day
    Regional_Meat_Consumption = Food_Demand_Livestock * Population
  ) %>%
  # align food_df again with other pyam data structure
  # Unit: million people
  mutate(Population = Population / 1e+06) %>%
  select(-Food_Demand_Livestock) %>%
  pivot_longer(
    cols = c(Population, Regional_Meat_Consumption),
    names_to = "variable", values_to = "value"
  ) %>%
  mutate(unit = ifelse(variable == "Population", "million", "kcal/region/day"))


# clean scenario meta data
scenario_meta_df <- scenario_meta_data %>%
  rename("scen_category" = category,
         "ssp_version" = version) %>%
  select(model, scenario, scen_category, ssp_family, ssp_version, project_study)

# clean mapping_df
mapping_df <- mapping_df %>%
  rename("threshold" = sum_weighted_avg_dle_threshold_adjusted)

# splitting mapping_df into two dataframes depending on variable
mapping_df_transport_SSP2NPi <- mapping_df %>%
  filter(
      variable == "Final Energy|Transportation" &
      scenario == "SSP2-NPi")

mapping_df_transport_SSP21p5C <- mapping_df %>%
  filter(
    variable == "Final Energy|Transportation" &
      scenario == "SSP2-1p5C")

mapping_df_housing_SSP2NPi <- mapping_df %>%
  filter(
    variable == "Final Energy|Residential and Commercial" &
      scenario == "SSP2-NPi")

mapping_df_housing_SSP21p5C <- mapping_df %>%
  filter(
    variable == "Final Energy|Residential and Commercial" &
      scenario == "SSP2-1p5C")

# clean sdp_mapping_df
sdp_mapping_df <- sdp_mapping_df %>%
  rename("threshold" = weighted_avg_energy_per_capita_uncorrected)

# splitting mapping_df into two dataframes depending on variable
mapping_df_transport_SDP_EI <- sdp_mapping_df %>%
  filter(
    variable == "Final Energy|Transportation" &
      scenario == "SDP_EI-1p5C")

mapping_df_transport_SDP_RC <- sdp_mapping_df %>%
  filter(
    variable == "Final Energy|Transportation" &
      scenario == "SDP_RC-1p5C")

mapping_df_transport_SSP2NPi <- sdp_mapping_df %>%
  filter(
    variable == "Final Energy|Transportation" &
      scenario == "SSP2-NPi")

mapping_df_housing_SDP_EI <- sdp_mapping_df %>%
  filter(
    variable == "Final Energy|Residential and Commercial" &
      scenario == "SDP_EI-1p5C")

mapping_df_housing_SDP_RC <- sdp_mapping_df %>%
  filter(
    variable == "Final Energy|Residential and Commercial" &
      scenario == "SDP_RC-1p5C")

mapping_df_housing_SSP2NPi <- sdp_mapping_df %>%
  filter(
    variable == "Final Energy|Residential and Commercial" &
      scenario == "SSP2-NPi")


