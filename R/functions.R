library(stringr)
library(dplyr)
library(tidyr)
library(DescTools)


SplitRegions <- function(df, threshold, 
                          iam_variable, population_variable) {
  #' Split Regions into Lower and Higher Population Groups Based on a Threshold
  #'
  #' This function i) ranks all regions based on a the variable
  #' iam_variable, ii) then splits regions into two groups based on
  #' a population threshold. The splitting occurs iteratively: The region 
  #' that exceeds the threshold is split into two.
  #'
  #' @param df A dataframe containing at least two columns:
  #'           - `Population`: the population of each region.
  #'           - `iam_variable`: a per capita indicator used to sort regions.
  #' @param threshold A numeric value between 0 and 1, representing the percentage 
  #'                  of total population assigned to the lower group (e.g., 0.2 for the 
  #'                  lowest 20%).
  #' @return A dataframe with the same columns as the input, plus an additional `group` 
  #'         column. The `group` column indicates whether a row belongs to the lower 
  #'         or higher group. If a region’s population exceeds the threshold, it is split 
  #'         into two rows: one for the lower group and one for the higher group. The 
  #'         result may contain multiple rows for a single region.
  
  
  # Sort the dataframe by per capita indicator (ascending)
  df <- df %>% arrange(!!sym(iam_variable))
  
  # Compute total population for reference
  total_population <- sum(df %>% pull(!!sym(population_variable)), na.rm = TRUE)
  #df <- df %>% mutate(total_population = sum(pull(., !!sym(population_variable)), na.rm = TRUE))
  
  
  # Compute population threshold
  threshold_pop <- total_population * threshold  
  
  # Initialize cumulative population tracker
  sum_lower <- 0  
  
  # Initialize output dataframe
  result <- data.frame()
  
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    
    if (sum_lower + row[[population_variable]] <= threshold_pop) {
      # If the whole region fits in lower threshold, assign it to lower_XX
      row$group <- paste0("lower_", threshold * 100)
      sum_lower <- sum_lower + row[[population_variable]]
      result <- rbind(result, row)
    } else {
      # If adding the region exceeds threshold, split it
      split_lower <- threshold_pop - sum_lower
      split_higher <- row[[population_variable]] - split_lower
      
      # Create two new rows: one for lower threshold and one for the remaining
      row_lower <- row
      row_lower[[population_variable]] <- split_lower
      row_lower$group <- paste0("lower_", threshold * 100)
      
      row_higher <- row
      row_higher[[population_variable]] <- split_higher
      row_higher$group <- paste0("higher_", 100 - (threshold * 100))
      
      # Append both rows to result 
      result <- rbind(result, row_lower, row_higher)
      
      #Append all remaining rows to higher_, but only if there are any rows left
      if (i < nrow(df)) {
        result <- rbind(result, df[(i+1):nrow(df), ] %>% 
                          mutate(group = paste0("higher_", 
                                                100 - (threshold * 100))))
      }
      

      break #Stop iteration because threshold was reached
    }
  }
  # Include total_population in the result
  result <- result %>% mutate(total_population = total_population)
  
  
  return(result)
}


SplitDeciles <- function(df, iam_variable, population_variable) {
  #' Split Regions into Deciles Based on Population and Per Capita Indicator
  #'
  #' This function i) ranks all regions based on the variable
  #' global_indicator_PC, ii) then splits regions into 10 groups (deciles)
  #' based on a population thresholds in such a way, that each decile
  #' corresponds to 10% of the total population.
  #'
  #' @param df A dataframe containing at least two columns:
  #'           - `Population`: the population of each region.
  #'           - `global_indicator_PC`: a per capita indicator used to sort regions.
  #' @return A dataframe with the same columns as the input, plus an additional `group` 
  #'         column. The `group` column indicates which decile each region belongs to.
  #'         The decile is assigned based on the cumulative population in ascending order.
  #Takes a dataframe with global regions and splits the regional population into deciles
  #Ouputs a dataframe with one column per decile. Each column shows the 
  #population of each region in the respective decile
  
  # Calculate the population for each decile
    total_population <- sum(df %>% pull(!!sym(population_variable)),
                            na.rm = TRUE)
    people_per_decile <- 0.1 * total_population
    

  # Initialize new columns for each decile
  df <- df %>%
    #sort df by global_indicator_PC
    arrange(!!sym(iam_variable)) %>%
    #create 10 additional columns for deciles: decile_1 to decile_10
    mutate(decile_10 = NA,
           decile_9 = NA,
           decile_8 = NA,
           decile_7 = NA,
           decile_6 = NA,
           decile_5 = NA,
           decile_4 = NA,
           decile_3 = NA,
           decile_2 = NA,
           decile_1 = NA) %>%
    #at the beginning of the loop, the entire population is uncategorized
    mutate(uncategorized_population = !!sym(population_variable))
  
  deciles <- paste0("decile_", 10:1) 
  # TO DELETE: deciles <- c("decile_10", "decile_9", "decile_8", "decile_7", "decile_6", "decile_5", "decile_4", "decile_3", "decile_2", "decile_1")
  
  # Loop through each column ("decile")
  for (decile in deciles){
    sum_decile <- 0 #amount of people in decile
    #to start, select first row that has still uncategorised population
    i  <- which(df$uncategorized_population > 0)[1] 
    
    #Loop through each rows/regions until decile is filled
    ## i <= nrow(df) ensures that loop stops if all regions are categorised
    ## sum_decile < people_per_decile ensures that loop stops if decile is filled
    ## any(df$uncategorized_population > 0) ensures that loop stops if there are no more
    ## uncategorised regions (was needed to be able to run code across multiple scenarios)
    while(i <= nrow(df) && sum_decile < people_per_decile & any(df$uncategorized_population > 0)){
      # define loop variables
      #TO DELETE: row <- df[i, ] #current row = region
      #missing_people = the people missing to fill this decile
      missing_people <- people_per_decile - sum_decile
      #available_people = the people available in this region
      available_people <- df$uncategorized_population[i]
      
      
      if (available_people > missing_people){
        #if TRUE, this means that decile can be filled with this row alone
        #fill decile
        # TO DELETE: sum_decile <- sum_decile + missing_people
        df[i, decile] <- missing_people
        
        #adjust uncategorized_population
        #TO DELETE: df[i, ]$uncategorized_population <- row$uncategorized_population - missing_people
        df$uncategorized_population[i] <- available_people - missing_people
        
        #add people to decile
        #df[i, decile] <- missing_people
        
        break
        
      } else{ #if FALSE, this means that deciles can't be filled with this row alone
        #use all leftover people
        df[i, decile] <- available_people
        #sum_decile <- sum_decile + row$uncategorized_population
        #set uncategorized_population to zero
        df$uncategorized_population[i] <- 0
        
        #loop on next row
      }
      sum_decile <- sum_decile + available_people
      i <- i + 1
    }
    
  }
  
  df %>% 
    # Convert deciles to long format
    pivot_longer(cols = starts_with("decile_"),
                 names_to = "decile",
                 values_to = "decile_population")%>%
    #omit rows for all empty decile - region combinations
    na.omit()%>%
    #renaming all cells in decile column to numeric (e.g. instead of decile_1, 1)
    mutate(decile = as.numeric(str_extract(decile, "\\d+$")))%>%
  
  return(df)
}

AggregateUtilitarian <- function(dataframe, iam_variable, population_variable, 
                             start_year, end_year, 
                             conversion_rate_iam_variable,
                             conversion_rate_population_variable){
  #' Computes the total per capita change of an IAM variable over a given time period.
  #'
  #' @param dataframe A dataframe containing model data.
  #' @param iam_variable A string indicating the IAM variable of interest.
  #' @param population_variable A string indicating the population variable.
  #' @param start_year Numeric, the start year for analysis.
  #' @param end_year Numeric, the end year for analysis.
  #' @param conversion_rate Numeric, the conversion factor for IAM variable.
  #'
  #' @return A dataframe with the total per capita change of the IAM variable
  #' in the column "total_change_iam_variable_pc".

  df <- dataframe%>%
    #filter on relevant data
    filter(variable %in% c(iam_variable, population_variable) 
           & year %in% c(start_year, end_year)) %>% 
    select(-unit)%>%
    
    #scale the variables to the right unit
    mutate(value = ifelse(variable == iam_variable, 
                          value * conversion_rate_iam_variable,value))%>%
    #Unit: MJ per year OR kcal per region per day
    mutate(value = ifelse(variable == population_variable, 
                          value * conversion_rate_population_variable,
                          value))%>% #Unit: single person
    
    #get iam_variable and population_variable for each region
    group_by(model, scenario, region)%>% 
    pivot_wider(names_from = variable, values_from = value)%>%
    #drop data entries if either variable is missing
    na.omit() %>% 
    ungroup()%>%
    
    #get total sum of iam_variable and population_variable
    #per year for each scenario
    group_by(model, scenario, year) %>%
    summarise(
      total_iam_variable = sum(.data[[iam_variable]], na.rm = TRUE),
      #Unit:MJ/yr OR kcal/region/day
      total_population = sum(.data[[population_variable]], na.rm = TRUE)) %>% 
      #Unit:single person
    mutate(
      total_iam_variable_pc = total_iam_variable / total_population, 
      #Unit: MJ/cap/yr OR kcal/cap/day
      region = "Global")%>%
    ungroup()%>%
    
    #calculate the difference in total_iam_variable_pc between starting and 
    #end year for each scenario
    pivot_wider(id_cols = c(model, scenario, region),
                names_from = year, values_from = total_iam_variable_pc) %>%
    mutate(
      value = .data[[as.character(end_year)]] / .data[[as.character(start_year)]],
      #Unit: relative change of MJ/cap/yr OR kcal/cap/day
      justice_diagnostic = "relative_change_iam_variable_pc",
      year = as.numeric(NA), 
      evaluated_var = iam_variable)
 
  return(df %>% select(model, scenario, justice_diagnostic,
                       value, year, region)%>%
           mutate(variable = as.character(iam_variable)))
}


Prioritarian <- function(dataframe, iam_variable, population_variable, 
                                  start_year, end_year, 
                                  conversion_rate_iam_variable,
                                  conversion_rate_population_variable,
                                  share_of_lower_threshold){
  #' Calculate the Change in Total Variable Share for the Lower X% of the Population
  #'
  #' This function calculates how the consumption share of a given resource 
  #' (e.g., energy, food) has changed over time for the bottom X% of the population, 
  #' based on per capita consumption. It first splits regions into a lower and 
  #' higher consumption group, then computes the total and relative shares of 
  #' consumption for each group across two time periods.
  #'
  #' @param dataframe A data frame containing IAM and population data.
  #' @param iam_variable A string specifying the name of the resource variable (e.g., "energy").
  #' @param population_variable A string specifying the population variable (e.g., "population").
  #' @param start_year An integer representing the initial year of analysis.
  #' @param end_year An integer representing the final year of analysis.
  #' @param conversion_rate_iam_variable A numeric factor to convert IAM variable values to a standard unit.
  #' @param conversion_rate_population_variable A numeric factor to convert population values to a standard unit.
  #' @param share_of_lower_threshold A numeric value between 0 and 1, representing the share of the population 
  #'        classified as "lower" (e.g., 0.5 for the bottom 50%).
  #'
  #' @return A data frame containing the change in consumption share for the lower X% of the population.
  #'         The output includes columns: `model`, `scenario`, `variable`, `value`, `year`, and `region`.

  
  df <- dataframe %>%
    # Filter relevant data: select IAM and population variables, and focus on start & end years
    filter(variable %in% c(iam_variable, population_variable) 
           & year %in% c(start_year, end_year)) %>% 
    select(-unit)%>%
    
    # Convert IAM and population variables to the correct unit
    mutate(value = ifelse(variable == iam_variable, 
                          value * conversion_rate_iam_variable,value))%>%
    #Unit: MJ per year OR kcal per region per day
    mutate(value = ifelse(variable == population_variable, 
                          value * conversion_rate_population_variable,
                          value))%>% #Unit: single person
    # Reshape data: transform to wide format (separate IAM and population variables)
    pivot_wider(names_from = variable, values_from = value) %>%
    na.omit()%>%
    
    # Compute per capita values
    mutate(
      iam_variable_pc = .data[[iam_variable]] / .data[[population_variable]])%>%
    #Unit: MJ per person OR kcal per person per day 
    
    # Split the population into lower & upper groups based on per capita consumption
    #Applying SplitRegions()
    group_by(model, scenario, year)%>%
    group_modify(~ SplitRegions(.x, share_of_lower_threshold,
                                 iam_variable, population_variable)) %>%

  
    # Compute total iam variable per region
    group_by(model, scenario, year, group) %>%
    mutate(iam_variable_regional_total = .data[[population_variable]] * iam_variable_pc) %>% 
    #Unit: MJ or kcal per region per year
    
    # Aggregate total iam variable for each group
    summarise(iam_variable_group_total = sum(iam_variable_regional_total))%>% 
    #Unit: MJ or kcal per region per year
    
    # Compute total IAM variable per year
    group_by(model, scenario, year)%>%
    mutate(total_iam_variable = sum(iam_variable_group_total)) %>%
    ##Unit: MJ or kcal per region per year
    
    # Calculate the share of each group's consumption relative to the total
    mutate(group_share = iam_variable_group_total / total_iam_variable)%>% 
    #Unit: share of total iam_variable
    
    # Compare how group shares change over time
    pivot_wider(id_cols = c(model, scenario, group),
                names_from = year,
                values_from = group_share)%>%
    # Compute the change in the share of total consumption
    mutate(value = .data[[as.character(end_year)]] - .data[[as.character(start_year)]])%>% 
    #Unit: change of share of total consumption
    
    # Keep only the "lower X%" group (e.g., lower 50%)
    filter(group == paste0("lower_", round(share_of_lower_threshold * 100))) %>%
    
    # Final formatting: define output variable name, add metadata
    mutate(
      justice_diagnostic =  "delta_group_share_lower",
      year = as.numeric(end_year),
      region = "Global"
    )%>%
    ungroup()%>%
    select(model, scenario, justice_diagnostic, value, year, region)%>%
    mutate(variable = as.character(iam_variable))
  
  return(df)

}


Prioritarian_new <-function(dataframe, iam_variable, population_variable, 
                            start_year, end_year, 
                            conversion_rate_iam_variable,
                            conversion_rate_population_variable,
                            gamma = 1){
  #' Calculate the Change in Total Variable Share for the Lower X% of the Population
  #'
  #' This function calculates how the consumption share of a given resource 
  #' (e.g., energy, food) has changed over time for the bottom X% of the population, 
  #' based on per capita consumption. It first splits regions into a lower and 
  #' higher consumption group, then computes the total and relative shares of 
  #' consumption for each group across two time periods.
  #'
  #' @param dataframe A data frame containing IAM and population data.
  #' @param iam_variable A string specifying the name of the resource variable (e.g., "energy").
  #' @param population_variable A string specifying the population variable (e.g., "population").
  #' @param start_year An integer representing the initial year of analysis.
  #' @param end_year An integer representing the final year of analysis.
  #' @param conversion_rate_iam_variable A numeric factor to convert IAM variable values to a standard unit.
  #' @param conversion_rate_population_variable A numeric factor to convert population values to a standard unit.
  #' @param gamma A numeric parameter represeting the aversion of inqualities.The larger gamma, the larger inequality aversion.
  #'
  #' @return A data frame containing the change aggregated utility following the Atkinson function, weighted by regional population.
  #'         The output includes columns: `model`, `scenario`, `variable`, `value`, `year`, and `region`.
  
  
  df <- dataframe %>%
    # Filter relevant data: select IAM and population variables, and focus on start & end years
    filter(variable %in% c(iam_variable, population_variable) 
           & year %in% c(start_year, end_year)) %>% 
    select(-unit)%>%
    
    # Convert IAM and population variables to the correct unit
    mutate(value = ifelse(variable == iam_variable, 
                          value * conversion_rate_iam_variable,value))%>%
    #Unit: MJ per year OR kcal per region per day
    mutate(value = ifelse(variable == population_variable, 
                          value * conversion_rate_population_variable,
                          value))%>% #Unit: single person
    # Reshape data: transform to wide format (separate IAM and population variables)
    pivot_wider(names_from = variable, values_from = value) %>%
    na.omit()%>%
    
    # Compute per capita values
    mutate(
      iam_variable_pc = .data[[iam_variable]] / .data[[population_variable]])
    #Unit: MJ per person OR kcal per person per day 
    
    # Implement Atkinson formular. Utility is per capita values
    if (gamma == 1) {
      df <- df %>% mutate(atkinson_utility = log(iam_variable_pc))
    } else {
      df <- df %>% mutate(atkinson_utility = (iam_variable_pc^(1 - gamma)) / (1 - gamma))
    }
    
    df <- df %>%
    #Weight Atkinson Utility by regional population
    mutate(atkinson_uility_weighted = atkinson_utility * .data[[population_variable]]) %>% 
      
    # Compute aggregate Atkinson utilities
    group_by(model, scenario, year) %>%
    # Aggregate total iam variable for each group
    summarise(total_atkinson_utility = sum(atkinson_uility_weighted), .groups = "drop") %>%
    #Unit: MJ or kcal per region per year
    
    # Compare how Atkinson total changes between years
    pivot_wider(id_cols = c(model, scenario),
                names_from = year,
                values_from = total_atkinson_utility)%>%
    # Compute the change in total utility
    mutate(value = .data[[as.character(end_year)]] - .data[[as.character(start_year)]])%>% 
    #Unit: change of share of total consumption
    
    # Final formatting: define output variable name, add metadata
    mutate(
      justice_diagnostic =  "total_atkinson_utility_change",
      year = as.numeric(end_year),
      region = "Global"
    )%>%
    ungroup()%>%
    select(model, scenario, justice_diagnostic, value, year, region)%>%
    mutate(variable = as.character(iam_variable))
  
  return(df)
  
}

Egalitarian <- function(dataframe, iam_variable, population_variable, 
                         end_year, 
                         conversion_rate_iam_variable,
                         conversion_rate_population_variable){
  #' Calculate the Gini Coefficient of an IAM Varibable Across Population Deciles
  #'
  #' This function computes the Gini coefficient to measure the inequality of resource 
  #' consumption (e.g., energy, food) across population deciles in a given year. 
  #' It first ranks regions by per capita values of the iam variable, splits the population into 
  #' ten deciles, and calculates the total resource consumption for each decile. 
  #' Finally, it computes the Gini coefficient based on the distribution of decile 
  #' consumption.
  #'
  #' @param dataframe A data frame containing IAM model outputs and population data.
  #' @param iam_variable A string specifying the name of the resource consumption variable 
  #'        (e.g., `"energy"`).
  #' @param population_variable A string specifying the population variable (e.g., `"population"`).
  #' @param end_year An integer specifying the year of analysis.
  #' @param conversion_rate_iam_variable A numeric factor to convert IAM variable values 
  #'        to a standard unit.
  #' @param conversion_rate_population_variable A numeric factor to convert population values 
  #'        to a standard unit.
  #'
  #' @return A data frame containing the **Gini coefficient** for the given resource variable 
  #'         in the selected year. The output includes columns:  
  #'         - `model`: IAM model used.  
  #'         - `scenario`: Scenario name.  
  #'         - `variable`: `"global_gini"` (the computed Gini coefficient).  
  #'         - `value`: The Gini coefficient (ranging from **0** = perfect equality to **1** = maximum inequality).  
  #'         - `year`: The year of analysis (equal to `end_year`).  
  #'         - `region`: `"Global"`, as calculations are based on a global population split. 
  
  df <- dataframe %>%
    # Filter relevant data: select IAM and population variables, and focus on start & end years
    filter(variable %in% c(iam_variable, population_variable)
           & year %in% c(end_year)) %>% 
    select(-unit) %>%
    
    # Convert IAM and population variables to the correct unit
    mutate(value = ifelse(variable == iam_variable, 
                          value * conversion_rate_iam_variable,value))%>%
    #Unit: MJ per year OR kcal per region per day
    mutate(value = ifelse(variable == population_variable, 
                          value * conversion_rate_population_variable,
                          value))%>% #Unit: single person
    
    # Reshape data: transform to wide format (separate IAM and population variables)    
    group_by(model, scenario, region)%>% 
    pivot_wider(names_from = variable, values_from = value)%>%
    na.omit() %>% #drop data entries with only population or indicator
    ungroup()%>%
    
    # Compute per capita values
    mutate(
      iam_variable_pc = .data[[iam_variable]] / .data[[population_variable]])%>%
    #Unit: MJ per person OR kcal per person per day 
    
    # Calculate population deciles
    group_by(model, scenario)%>%
    #Applying SplitDeciles()
    group_modify(~ SplitDeciles(., iam_variable, population_variable))%>%
    
    # Calculate decile consumptions
    mutate(decile_consumption = decile_population * iam_variable_pc)%>% 
    #Unit: MJ or kcal per decile per year
    
    # Sum up regional decile consumptions
    group_by(model, scenario, year, decile)%>%
    summarise(decile_consumption = sum(decile_consumption))%>% 
    #Unit: MJ or kcal per decile per year
    
    # Calculate GINI coefficient of decile consumption
    ungroup()%>%
    group_by(model, scenario, year)%>%
    summarise(value = Gini(decile_consumption))%>%
    #Unit: GINI coefficient of iam_variable at end_year
    
    # Final formatting: define output variable name, add metadata
    mutate(region = "Global",
           justice_diagnostic = "global_gini")%>%
    select(model, scenario, justice_diagnostic, value, year, region)%>%
    mutate(variable = as.character(iam_variable))
 
  return(df)  
  
}

Sufficientarian <- function(dataframe, iam_variable, population_variable, 
                            end_year, 
                            conversion_rate_iam_variable,
                            conversion_rate_population_variable,
                            sufficientarian_threshold = NULL,
                            mapping_df = NULL){ #default to NULL
  
  
  ## calculate the average global deprivation for a given end_year
  df <- dataframe%>% 
    
    # Filter relevant data: select IAM and population variables, and focus on start & end years
    filter(variable %in% c(iam_variable, population_variable)
           & year %in% c(end_year)) %>% 
    select(-unit) %>%
  
    # Convert IAM and population variables to the correct unit
    mutate(value = ifelse(variable == iam_variable, 
                          value * conversion_rate_iam_variable,value))%>%
    #Unit: MJ per year OR kcal per region per day
    mutate(value = ifelse(variable == population_variable, 
                          value * conversion_rate_population_variable,
                          value))%>% #Unit: single person
    
    # Reshape data: transform to wide format (separate IAM and population variables)    
    group_by(model, scenario, region)%>% 
    pivot_wider(names_from = variable, values_from = value)%>%
    na.omit() %>% #drop data entries with only population or indicator
    ungroup()
    
    # If mapping_df is provided, join it and apply region-specific threshold
    if (!is.null(mapping_df)) {
      df <- df %>%
        left_join(mapping_df %>% select(region, threshold), by = 'region') %>%
        mutate(region_specific_threshold = threshold) %>%
        select(-threshold)  # Remove scaler column after use
    } else {
      df <- df %>%
        mutate(region_specific_threshold = sufficientarian_threshold)  # Use default threshold
      #warn if Sufficientarian threshold is NULL
      if (is.null(sufficientarian_threshold)) {
        warning("Warning: Neither mapping nor sufficientarian threshold provided!")
      }
    }
  
    
    # Compute per capita values
    df <- df %>%
      mutate(
      iam_variable_pc = .data[[iam_variable]] / .data[[population_variable]])%>%
    #Unit: MJ per person per year OR kcal per person per day 
    
    # Compute distance to sufficientarian threshold
    mutate(delta = iam_variable_pc - region_specific_threshold)%>% 
    #Unit: MJ per person per year OR kcal per person per day 
    
    # Only consider negative deltas; positives are set zero
    mutate(delta = ifelse(delta > 0, 0, delta)) %>%
    
    # Weight regional deltas by population
    mutate(delta_weighted = delta * .data[[population_variable]]) %>% 
    #Unit: Deprivation in MJ per region per year OR kcal per region per day 
    
    # Sum weighted deltas over all deprivated regions
    group_by(model, scenario) %>% 
    summarise(
      sum_delta_weighted = sum(delta_weighted), 
      #Unit: Deprivation in MJ per scenario per year OR kcal per scenario per day
      sum_deprived_population = sum(.data[[population_variable]][delta < 0]) 
      #Unit: people
    )%>%
    
    # Compute global average deprivation and set meta data; 
    # Return 0 if no one is deprived
    mutate(value = ifelse(sum_deprived_population == 0, 
                          0, 
                          sum_delta_weighted / sum_deprived_population), 
           #Unit: Average deprivation in MJ per person per year or kcal per person per day
           justice_diagnostic =  "global_avg_deprivation",
           year = end_year,
           region = "Global")%>%
    ungroup() %>%
    select(-sum_delta_weighted, -sum_deprived_population)%>%
    mutate(variable = as.character(iam_variable))

  return(df)

}


Limitarian <- function(dataframe, iam_variable, population_variable, 
                            end_year, 
                            conversion_rate_iam_variable,
                            conversion_rate_population_variable,
                            limitarian_threshold = NULL,
                            mapping_df = NULL, 
                            limitarian_multiplier = NULL){ #default to NULL
  
  
  
  df <- dataframe%>% 
    
    # Filter relevant data: select IAM and population variables, and focus on start & end years
    filter(variable %in% c(iam_variable, population_variable)
           & year %in% c(end_year)) %>% 
    select(-unit) %>%
    
    # Convert IAM and population variables to the correct unit
    mutate(value = ifelse(variable == iam_variable, 
                          value * conversion_rate_iam_variable,value))%>%
    #Unit: MJ per year OR kcal per region per day
    mutate(value = ifelse(variable == population_variable, 
                          value * conversion_rate_population_variable,
                          value))%>% #Unit: single person
    
    # Reshape data: transform to wide format (separate IAM and population variables)    
    group_by(model, scenario, region)%>% 
    pivot_wider(names_from = variable, values_from = value)%>%
    na.omit() %>% #drop data entries with only population or indicator
    ungroup()
    
    # If mapping_df is provided, join it and apply region-specific threshold
    if (!is.null(mapping_df)) {
      df <- df %>%
        left_join(mapping_df %>% select(region, threshold), by = 'region') %>%
        mutate(region_specific_threshold = threshold * limitarian_multiplier) %>%
        select(-threshold)  # Remove scaler column after use
    } else {
      df <- df %>%
        mutate(region_specific_threshold = limitarian_threshold)  # Use default threshold
      if (is.null(limitarian_threshold)) {
        warning("Warning: Neither mapping nor limitarian threshold provided!")
      }
    }
    
    df <- df  %>%
      # Compute per capita values
      mutate(
      iam_variable_pc = .data[[iam_variable]] / .data[[population_variable]])%>%
    #Unit: MJ per person per year OR kcal per person per day 
    
    # Compute distance to limitarian threshold
    mutate(delta = iam_variable_pc - region_specific_threshold)%>% 
    #Unit: MJ per person per year OR kcal per person per day 
    
    # Only consider positive deltas; negatives are set zero
    mutate(delta = ifelse(delta < 0, 0, delta)) %>%
    
    # Weight regional deltas by population
    mutate(delta_weighted = delta * .data[[population_variable]]) %>% 
    #Unit: Surplus in MJ per region per year OR kcal per region per day 
    
    # Sum weighted deltas over all surplus regions
    group_by(model, scenario) %>% 
    summarise(
      sum_delta_weighted = sum(delta_weighted), 
      #Unit: Surplus in MJ per scenario per year OR kcal per scenario per day
      sum_surplus_population = sum(.data[[population_variable]][delta > 0]) 
      #Unit: people
    )%>%
    
    # Compute global average surplus and set meta data; 
    # Return 0 if no one is in surplus
    mutate(value = ifelse(sum_surplus_population == 0, 
                          0, 
                          sum_delta_weighted / sum_surplus_population), 
           #Unit: Average deprivation in MJ per person per year or kcal per person per day
           justice_diagnostic =  "global_avg_surplus",
           year = end_year,
           region = "Global")%>%
  ungroup() %>%
  select(-sum_delta_weighted, -sum_surplus_population)%>%
  mutate(variable = as.character(iam_variable))

  return(df)
  
}
