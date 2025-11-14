# Load data
source("R/get_data.R")

# Get Functions
source("R/functions.R")

# Set parameters for justice diagnostics ----------------------------------------------
shared_params <- list(
  # Data related parameters
  start_year = 2020,
  end_year = 2050,
  conversion_rate_iam_variable = 1e09,
  conversion_rate_population_variable = 1e06,
  population_variable = "Population",
  
  #Justice diagnostics related parameters
  sufficientarian_threshold_food = 90,              #Unit: kcal/person/day
  limitarian_threshold_food = 210,                  #Unit: kcal/person/day
  limitarian_muliplier_transport = 1,               #using mapping to SSP2-NPi instead 
  limitarian_muliplier_housing = 1,                 #using mapping to SSP2-NPi instead
  
  # RELAXTER SDP_RC interpretation
  RELAX_prioritarian_share_of_lower_threshold = 0.2,  #Unit: share of lower 20% is considered
  RELAX_prioritarian_gamma = 1,                       #Unit: NA, parameter for Atkinsonfunction
  RELAX_egaliatarian_gini_threshold_transport = 0.35, #Unit: Gini index | Source: 0.1 larger than SDP_EI
  RELAX_egaliatarian_gini_threshold_housing = 0.33,   #Unit: Gini index | Source: 0.1 larger than SDP_EI (Similar to the GINI of SSP2-1p5C)
  RELAX_egaliatarian_gini_threshold_food = 0.3,       #Unit: Gini index | Source: 0.1 larger than SDP_EI
  
  # STRICTER SDP_EI interpretation
  STRICT_prioritarian_share_of_lower_threshold = 0.5, #Unit: share of lower 50% is considered
  STRICT_prioritarian_gamma = 3,                      #Unit: NA, parameter for Atkinsonfunction
  STRICT_egaliatarian_gini_threshold_transport = 0.25, #Unit: Gini index | Source: R10 average of SDP_EI-1p5C in 2050
  STRICT_egaliatarian_gini_threshold_housing = 0.23,    #Unit: Gini index | | Source: R10 average of SDP_EI-1p5C in 2050
  STRICT_egaliatarian_gini_threshold_food = 0.2      #Unit: Gini index | Source: none
  
# +++ SDP_EI-1p5C +++ ------------------------------------------------------------

# Final_Energy_Transportation---------------------------------------------------
# Get justice diagnostics for the variable Final_Energy_Transportation
results_final_energy_transport <- rbind(
  AggregateUtilitarian(
    dataframe = df, 
    iam_variable = "Final_Energy_Transportation", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    start_year = shared_params$start_year, 
    end_year = shared_params$end_year, 
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable
    #Unit: single person
  ),
  Prioritarian_new(
    dataframe = df,
    iam_variable = "Final_Energy_Transportation", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    start_year = shared_params$start_year,
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    gamma = shared_params$STRICT_prioritarian_gamma
  ),
  Egalitarian(
    dataframe = df,
    iam_variable = "Final_Energy_Transportation", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable
    #Unit: single
  ),
  Sufficientarian(
    dataframe = df,
    iam_variable = "Final_Energy_Transportation", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    #sufficientarian_threshold = shared_params$sufficientarian_threshold_feTransport,
    #Unit: MJ/person/year
    mapping_df = mapping_df_transport_SDP_EI
  ),
  Limitarian(
    dataframe = df,
    iam_variable = "Final_Energy_Transportation", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    #limitarian_threshold = shared_params$limitarian_threshold_feTransport,
    #Unit: MJ/person/year
    mapping_df = mapping_df_transport_SSP2NPi,
    limitarian_multiplier = shared_params$limitarian_muliplier_transport
  )
)

# Final_Energy_Residential_and_Commercial --------------------------------------------------------------
# Get justice diagnostics for the variable Final_Energy_Residential_and_Commercial
results_final_energy_housing <- rbind(
  AggregateUtilitarian(
    dataframe = df, 
    iam_variable = "Final_Energy_Residential_and_Commercial", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    start_year = shared_params$start_year, 
    end_year = shared_params$end_year, 
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable
    #Unit: single person
  ),
  Prioritarian_new(
    dataframe = df,
    iam_variable = "Final_Energy_Residential_and_Commercial", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    start_year = shared_params$start_year,
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    gamma = shared_params$STRICT_prioritarian_gamma
  ),
  Egalitarian(
    dataframe = df,
    iam_variable = "Final_Energy_Residential_and_Commercial", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable
    #Unit: single
  ),
  Sufficientarian(
    dataframe = df,
    iam_variable = "Final_Energy_Residential_and_Commercial", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    #sufficientarian_threshold = shared_params$sufficientarian_threshold_feHousing,
    #Unit: MJ/person/year
    mapping_df = mapping_df_housing_SDP_EI
  ),
  Limitarian(
    dataframe = df,
    iam_variable = "Final_Energy_Residential_and_Commercial", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    #limitarian_threshold = shared_params$limitarian_threshold_feHousing,
    #Unit: MJ/person/year
    mapping_df = mapping_df_housing_SSP2NPi,
    limitarian_multiplier = shared_params$limitarian_muliplier_housing
  )
)

# Regional_Meat_Consumption -------------------------------------------------------------- 
# Note: food data is read from different file, 
# and different units and conversation apply.
# Get justice diagnostics for the variable Regional_Meat_Consumption
results_regional_meat_consumption <- rbind(
  AggregateUtilitarian(
    dataframe = food_df, 
    iam_variable = "Regional_Meat_Consumption", #Unit: kcal/region/day
    population_variable = shared_params$population_variable, #Unit: million
    start_year = shared_params$start_year, 
    end_year = shared_params$end_year, 
    conversion_rate_iam_variable = 1,
    #Unit: kcal/region/day
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable
    #Unit: single person
  ),
  Prioritarian_new(
    dataframe = food_df,
    iam_variable = "Regional_Meat_Consumption", #Unit: kcal/region/day
    population_variable = shared_params$population_variable, #Unit: million
    start_year = shared_params$start_year,
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = 1,
    #Unit: kcal/region/day
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    gamma = shared_params$STRICT_prioritarian_gamma
  ),
  Egalitarian(
    dataframe = food_df,
    iam_variable = "Regional_Meat_Consumption", #Unit: kcal/region/day
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = 1,
    #Unit: kcal/region/day
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable
    #Unit: single
  ),
  Sufficientarian(
    dataframe = food_df,
    iam_variable = "Regional_Meat_Consumption", #Unit: kcal/region/day
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = 1,
    #Unit: kcal/region/day
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    sufficientarian_threshold = shared_params$sufficientarian_threshold_food
    #Unit: MJ/person/year
  ),
  Limitarian(
    dataframe = food_df,
    iam_variable = "Regional_Meat_Consumption", #Unit: kcal/region/day
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = 1,
    #Unit: kcal/region/day
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    limitarian_threshold = shared_params$limitarian_threshold_food
    #Unit: MJ/person/year
  )
)

# Create final output --------------------------------------------------------------
# Combine all results into one final df
final_justice_diagnostics_df_SDP_EI <- rbind(
  results_final_energy_transport,
  results_final_energy_housing,
  results_regional_meat_consumption
)%>%
  mutate(SDP = "EI")


# +++ SDP_RC-1p5C +++ ------------------------------------------------------------

# Final_Energy_Transportation---------------------------------------------------
# Get justice diagnostics for the variable Final_Energy_Transportation
results_final_energy_transport <- rbind(
  AggregateUtilitarian(
    dataframe = df, 
    iam_variable = "Final_Energy_Transportation", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    start_year = shared_params$start_year, 
    end_year = shared_params$end_year, 
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable
    #Unit: single person
  ),
  Prioritarian_new(
    dataframe = df,
    iam_variable = "Final_Energy_Transportation", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    start_year = shared_params$start_year,
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    gamma = shared_params$RELAX_prioritarian_gamma
  ),
  Egalitarian(
    dataframe = df,
    iam_variable = "Final_Energy_Transportation", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable
    #Unit: single
  ),
  Sufficientarian(
    dataframe = df,
    iam_variable = "Final_Energy_Transportation", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    #sufficientarian_threshold = shared_params$sufficientarian_threshold_feTransport,
    #Unit: MJ/person/year
    mapping_df = mapping_df_transport_SDP_RC
  ),
  Limitarian(
    dataframe = df,
    iam_variable = "Final_Energy_Transportation", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    #limitarian_threshold = shared_params$limitarian_threshold_feTransport,
    #Unit: MJ/person/year
    mapping_df = mapping_df_transport_SSP2NPi,
    limitarian_multiplier = shared_params$limitarian_muliplier_transport
  )
)

# Final_Energy_Residential_and_Commercial --------------------------------------------------------------
# Get justice diagnostics for the variable Final_Energy_Residential_and_Commercial
results_final_energy_housing <- rbind(
  AggregateUtilitarian(
    dataframe = df, 
    iam_variable = "Final_Energy_Residential_and_Commercial", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    start_year = shared_params$start_year, 
    end_year = shared_params$end_year, 
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable
    #Unit: single person
  ),
  Prioritarian_new(
    dataframe = df,
    iam_variable = "Final_Energy_Residential_and_Commercial", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    start_year = shared_params$start_year,
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    gamma = shared_params$RELAX_prioritarian_gamma
  ),
  Egalitarian(
    dataframe = df,
    iam_variable = "Final_Energy_Residential_and_Commercial", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable
    #Unit: single
  ),
  Sufficientarian(
    dataframe = df,
    iam_variable = "Final_Energy_Residential_and_Commercial", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    #sufficientarian_threshold = shared_params$sufficientarian_threshold_feHousing,
    #Unit: MJ/person/year
    mapping_df = mapping_df_housing_SDP_RC
  ),
  Limitarian(
    dataframe = df,
    iam_variable = "Final_Energy_Residential_and_Commercial", #Unit: EJ/yr
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = shared_params$conversion_rate_iam_variable,
    #Unit: MJ/yr
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    #limitarian_threshold = shared_params$limitarian_threshold_feHousing,
    #Unit: MJ/person/year
    mapping_df = mapping_df_housing_SSP2NPi,
    limitarian_multiplier = shared_params$limitarian_muliplier_housing
  )
)

# Regional_Meat_Consumption -------------------------------------------------------------- 
# Note: food data is read from different file, 
# and different units and conversation apply.
# Get justice diagnostics for the variable Regional_Meat_Consumption
results_regional_meat_consumption <- rbind(
  AggregateUtilitarian(
    dataframe = food_df, 
    iam_variable = "Regional_Meat_Consumption", #Unit: kcal/region/day
    population_variable = shared_params$population_variable, #Unit: million
    start_year = shared_params$start_year, 
    end_year = shared_params$end_year, 
    conversion_rate_iam_variable = 1,
    #Unit: kcal/region/day
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable
    #Unit: single person
  ),
  Prioritarian_new(
    dataframe = food_df,
    iam_variable = "Regional_Meat_Consumption", #Unit: kcal/region/day
    population_variable = shared_params$population_variable, #Unit: million
    start_year = shared_params$start_year,
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = 1,
    #Unit: kcal/region/day
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    gamma = shared_params$RELAX_prioritarian_gamma
  ),
  Egalitarian(
    dataframe = food_df,
    iam_variable = "Regional_Meat_Consumption", #Unit: kcal/region/day
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = 1,
    #Unit: kcal/region/day
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable
    #Unit: single
  ),
  Sufficientarian(
    dataframe = food_df,
    iam_variable = "Regional_Meat_Consumption", #Unit: kcal/region/day
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = 1,
    #Unit: kcal/region/day
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    sufficientarian_threshold = shared_params$sufficientarian_threshold_food
    #Unit: MJ/person/year
  ),
  Limitarian(
    dataframe = food_df,
    iam_variable = "Regional_Meat_Consumption", #Unit: kcal/region/day
    population_variable = shared_params$population_variable, #Unit: million
    end_year = shared_params$end_year,
    conversion_rate_iam_variable = 1,
    #Unit: kcal/region/day
    conversion_rate_population_variable = shared_params$conversion_rate_population_variable,
    #Unit: single
    limitarian_threshold = shared_params$limitarian_threshold_food
    #Unit: MJ/person/year
  )
)

# Create final output --------------------------------------------------------------
# Combine all results into one final df
final_justice_diagnostics_df_SDP_RC <- rbind(
  results_final_energy_transport,
  results_final_energy_housing,
  results_regional_meat_consumption
) %>%
  mutate(SDP = "RC")




# Combine both SDP results ---------------------------------
final_justice_diagnostics_df <- rbind(
  final_justice_diagnostics_df_SDP_RC,
  final_justice_diagnostics_df_SDP_EI
)


# Categorise scenarios based on justice diagnostics ----------------

# Create thresholds table for egalitarian
egalitarian_thresholds <- tibble::tibble(
  variable = c("Final_Energy_Transportation", "Final_Energy_Transportation",
               "Final_Energy_Residential_and_Commercial", "Final_Energy_Residential_and_Commercial",
               "Regional_Meat_Consumption", "Regional_Meat_Consumption"),
  SDP = c("EI", "RC", "EI", "RC", "EI", "RC"),
  #SDP = c("RC", "EI", "RC", "EI", "RC", "EI"),
  threshold = c(shared_params$STRICT_egaliatarian_gini_threshold_transport,
                shared_params$RELAX_egaliatarian_gini_threshold_transport,
                shared_params$STRICT_egaliatarian_gini_threshold_housing,
                shared_params$RELAX_egaliatarian_gini_threshold_housing,
                shared_params$STRICT_egaliatarian_gini_threshold_food,
                shared_params$RELAX_egaliatarian_gini_threshold_food)
)


final_justice_diagnostics_df <- final_justice_diagnostics_df %>%
  # Reshape dataframe
  pivot_wider(id_cols = c(model, scenario, variable, SDP),
    values_from = value, names_from = justice_diagnostic)%>%

  #drop na values -> scenarios that do not report certain IAM variables
  # NEEDS DOUBLE CHECKING/ TESTING
  na.omit()%>%
  
  # Join with egalitarian thresholds table
  left_join(egalitarian_thresholds, by = c("variable", "SDP")) %>%
  
  
  # Aggregate utilitarian
  ## core idea = overall consumption increase
  ## implementation: relative_change_global_consumption is positive
  ## NOTE: 5% buffer implemented to account for marginal increase
  mutate(AggUtilitarian = ifelse(relative_change_iam_variable_pc > 1.05, "yes", "no"))%>%
  
  # Prioritarian
  ## core idea = worst off are better off 
  ## implementation:delta_consumption_share_lower50 is positive 
  ## // this means share of lower 50% of population on total consumption increased between start and end year
  ## NOTE: 5% buffer implemented to account for marginal increase
  mutate(
    Prioritarian = ifelse(total_atkinson_utility_change > 0, "yes", "no"),
   
  #Egalitarian
  ##core idea = everyone has the same
  #Inequalities based 
  ##implementation: current global inequalities for housing and transport are between 0.45 and 0.5 according to Jarmo's calculations; 0.34 is SDP-RC GINI projection for Residential & Transport in 2040
    Egalitarian = case_when(
      !is.na(threshold) ~ if_else(global_gini <= threshold, "yes", "no"),
      TRUE ~ NA_character_),
  
  #Sufficientarian
  ##core idea = everyone is above certain threshold
  #implementation: global_avg_deprivation equal 0
  Sufficientarian = ifelse(global_avg_deprivation == 0, "yes", "no"),
  
  #Limitarian
  ##core idea = everyone is below certain threshold
  #implementation: global_avg_surplus equal 0
  Limitarian = ifelse(global_avg_surplus == 0, "yes", "no")) %>%
  # Drop threshold column
  select(-threshold)

  

# Save final results
 # write.csv(final_justice_diagnostics_df, "output/justice_diagnostics.csv",
 #           row.names = FALSE)
