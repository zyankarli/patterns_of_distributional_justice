source("R/get_data.R")

library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)
library(extrafont)


# Load justice diagnostics csv
justice_diagnostics_df <- read.csv("output/justice_diagnostics_newPrio.csv")
# Load mitigation strategies csv and pivot mitigation stratgies longer
mitigation_strategies_df <- read.csv("Data/mitigation_strategies.csv")
mitigation_strategies_df <- mitigation_strategies_df %>%
  pivot_longer(cols = c("Renewables", "CDR", "Demand", "Other"), 
               names_to = "mitigation_strategy", 
               values_to = "is_true")%>%
  filter(is_true == "True")

# Combine justice_diagnostics_df and mitigation_strategies_df
df_plot <- justice_diagnostics_df %>%
  left_join(scenario_meta_df,
            by = c("model", "scenario")) %>%
  {print(paste("Rows dropped due to NA in scen_category:", sum(is.na(.$scen_category)))) ; .} %>%
  filter(!is.na(scen_category)) %>%
  # drop 14 scenarios that do not have scenario meta data
  na.omit() %>% 
  left_join(mitigation_strategies_df%>% select(model, scenario, mitigation_strategy),
            by = c("model", "scenario"))%>%
  {print(paste("Rows dropped due to NA in mitigation_strategy:", sum(is.na(.$mitigation_strategy)))) ; .} %>%
  # drop 1359 scenarios that are not classified into mitigation strategies,
  # because they did not report the required clustering variables.
  na.omit()%>%
  rename(`Total Utilitarian` = AggUtilitarian)



# Define Theme --------------------------------------
theme_ks <- function() {
  
  font <- "Arial"
  
  theme_bw() %+replace%
    
    theme(
      
      # Grid elements
      
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_line(color="#636363",linewidth  = 0.7),          #strip axis ticks
      
      
      # Border lines
      panel.border = element_rect(color="#636363",fill=NA,linewidth = 0.7),
      panel.background = element_blank(),
      
      # Text elements
      
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 14,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 3,                #raise slightly
        color = 'black'),       #color
      
      plot.subtitle = element_text(
        family = font,
        size = 12,
        hjust = 0,
        vjust = 2,
        color = '#636363'),
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 0,                #right align
        color = '#bdbdbd'),       #color
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10,                #font size
        color = '#636363'),       #color
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 11,                 #font size
        color = 'black'),       #color
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      text = element_text(
        family = font,
        color = '#636363')
      
    )
  
}


# Create counts of justice patterns per mitigation strategy
df_counts <- df_plot %>%
  group_by(variable, SDP, scen_category, mitigation_strategy) %>%
  summarise(
    across(c(`Total Utilitarian`, Prioritarian, Egalitarian, Sufficientarian, Limitarian), 
           ~ sum(. == "yes")),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("Total Utilitarian", 
                           "Prioritarian", 
                           "Egalitarian", 
                           "Sufficientarian", 
                           "Limitarian"), 
               names_to = "justice_metric", 
               values_to = "count") %>%
  mutate(justice_metric = factor(justice_metric,
                                 levels = rev(c("Total Utilitarian", 
                                            "Prioritarian", 
                                            "Egalitarian", 
                                            "Sufficientarian", 
                                            "Limitarian")))) %>%
  #Rename Mitigation Strategies
  mutate(mitigation_strategy = case_when(
    mitigation_strategy == "Renewables" ~ "RENEWABLE",
    mitigation_strategy == "CDR" ~ "CDR",
    mitigation_strategy == "Demand" ~ "DEMAND",
    TRUE ~ "Other"
  )) %>%
  
  mutate(mitigation_strategy = factor(mitigation_strategy,
                            levels = c("DEMAND", "RENEWABLE", 
                                       "CDR", "Other")))%>%
  mutate(is_empty = ifelse(count == 0, "Unexplored", "Explored")) 

df_complete <- df_counts %>%
  complete(variable, SDP, scen_category, mitigation_strategy, justice_metric, 
           fill = list(count = 0, is_empty = "Unexplored"))


#BLANK ACROSS SCENARIO CATEGORY ------------------------

# Create a tibble with the justice metrics and calculate their positions around the circle
justice_positions <- tibble(
  justice_metric = c("Limitarian", "Sufficientarian", "Egalitarian", "Prioritarian", "Total Utilitarian"),
  #x_offset = c(0.0, 0.08, -0.08, -0.08, 0.08),
  #y_offset = c(0.3, 0.13, 0.13, -0.1, 0.3),
  x_offset = c(-.14,      -.1,       0.1,      0.14,      0.00),
  y_offset = c(0.06,      -.16,     -.16,      0.06,      0.2)
)

justice_colors = c("Limitarian" = "#E78AC3",
                   "Sufficientarian" = "#8DA0CB",
                   "Egalitarian" = "#66C2A5",
                   "Prioritarian" = "#FC8D62",
                   "Total Utilitarian" = "#A6D854")



df_test <- df_complete %>%
  #filter(is_empty == "Explored") %>% #CHANGE HERE
  left_join(justice_positions, by = "justice_metric")%>%
  #translate scen_category into description
  mutate(scen_category = ifelse(scen_category == "C1", "1.5°C (50%), no overshoot", 
                                ifelse(scen_category == "C2", "1.5°C (50%), overshoot", 
                                       ifelse(scen_category == "C3", "2°C (67%)", 
                                              ifelse(scen_category == "C4", "2°C (50%)", 
                                                     ifelse(scen_category == "C5", "2.5°C (50%)", 
                                                            ifelse(scen_category == "C6", "3°C (50%)", 
                                                                   ifelse(scen_category == "C7", "4°C (50%)", 
                                                                          ifelse(scen_category == "C8", "More than 4°C (50%)", 
                                                                                 "Failed Vetting"))))))))
  ) %>%
  #set factors
  mutate(scen_category = factor(scen_category,
                                levels = rev(c("1.5°C (50%), no overshoot", 
                                               "1.5°C (50%), overshoot", 
                                               "2°C (67%)", 
                                               "2°C (50%)", 
                                               "2.5°C (50%)", 
                                               "3°C (50%)", 
                                               "4°C (50%)", 
                                               "More than 4°C (50%)", 
                                               "Failed Vetting")))) %>%
  filter(#variable == "Regional_Meat_Consumption" & #Change here!
           #Final_Energy_Residential_and_Commercial, 
           #"Regional_Meat_Consumption, Final_Energy_Transportation
           !scen_category %in% c("3°C (50%)", "4°C (50%)", 
                                "More than 4°C (50%)", "Failed Vetting") &
           mitigation_strategy != 'Other')%>%
  mutate(justice_metric = factor(justice_metric, 
                                 levels = c("Total Utilitarian", "Prioritarian", "Egalitarian", 
                                            "Sufficientarian", "Limitarian")))

# 1. Define your plotting function
bubble_plot <- function(data, var_name, SDP_scenario) {
  df_var <- data %>% filter(variable == var_name & SDP == SDP_scenario)
  
  ggplot() +
    # Larger Square: Indicates whether any justice metric exists
    geom_point(data = df_var,
               aes(x = mitigation_strategy, y = scen_category),
               shape = 21, size = 9, color = "#525252",
               fill = NA, stroke = 0.1, show.legend = FALSE) +
    
    # Smaller Squares: Represent specific explored justice metrics
    geom_point(data = df_var,
               aes(x = mitigation_strategy, 
                   y = scen_category, 
                   fill = justice_metric, 
                   size = count),
               shape = 21, color = "black",
               position = position_nudge(x = df_var$x_offset, y = df_var$y_offset)) +
    
    scale_fill_manual(values = justice_colors) +
    #scale_size_continuous(range = c(2, 10)) + #limits = c(1, 40)
    scale_size(
      limits = c(1, 130),  # Set limits for size
      range = c(2, 10),  # Adjust bubble size range
      breaks = c(25, 50, 75, 100, 125) # Set breaks for size legend
                          ) +  # Adjust bubble size range
    facet_grid(~ mitigation_strategy, scales = "free") +
    theme_ks() +
    theme(
      axis.title = element_blank(),
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      strip.background = element_rect(fill = "white")
    ) +
    ggtitle(paste(display_vars[[var_name]]))+
    # to check whether SDP scenarios occur in correct column run
    # ggtitle(paste(display_vars[[var_name]], "-", SDP_scenario))+
    theme(plot.title = element_text(size = 11))
}


# 2. List of variables you want to plot
vars_to_plot <- c("Final_Energy_Residential_and_Commercial",
                  "Final_Energy_Transportation",
                  "Regional_Meat_Consumption")

# Define display names for your variables
display_vars <- c(
  "Final_Energy_Residential_and_Commercial" = "Energy for Housing",
  "Final_Energy_Transportation" = "Energy for Transportation",
  "Regional_Meat_Consumption" = "Meat Consumption"
)

# 3. Generate the individual plots
SDP_EI <- lapply(vars_to_plot, function(v) bubble_plot(df_test, v, "EI"))
SDP_RC <- lapply(vars_to_plot, function(v) bubble_plot(df_test, v, "RC")+
                         theme(axis.text.y = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.y = element_blank()))

# 4. Combine into a single patchwork grid
# Column headers as text-only plots
col_header_EI <- ggplot() + 
  annotate("text", x = 0.5, y = 0.5, label = "Strict Parameterization", size = 5, fontface = "bold") +
  theme_void()

col_header_RC <- ggplot() + 
  annotate("text", x = 0.5, y = 0.5, label = "Lenient Parameterization", size = 5, fontface = "bold") +
  theme_void()

# Wrap individual columns, otherwise patchwork won't work
column_EI <- wrap_plots(SDP_EI, ncol = 1)
column_RC <- wrap_plots(SDP_RC, ncol = 1)

# Stack headers and content into columns
final_plot <- wrap_plots(
  wrap_plots(col_header_EI, column_EI, ncol = 1, heights = c(0.05, 1)),
  wrap_plots(col_header_RC, column_RC, ncol = 1, heights = c(0.05, 1)),
  ncol = 2,
  guides = "collect"
) & 
  theme(legend.position = "bottom", legend.box = "vertical")

print(final_plot)

#ggsave("output/justice_metrics_bubble_plots.png", final_plot, width = 10, height = 15)


# BUBBLE PLOT COMBINED -----------

# Function to generate horizontal offsets
generate_horizontal_offsets <- function(n, total_width = 0.5) {
  # n = number of bubbles
  # total_width = total horizontal spread from leftmost to rightmost
  seq(-total_width/2, total_width/2, length.out = n)
}

# Example for your 5 justice metrics
justice_positions <- tibble(
  justice_metric = rev(c("Limitarian", "Sufficientarian", "Egalitarian", "Prioritarian", "Total Utilitarian")),
  x_offset = generate_horizontal_offsets(5, total_width = 0.5),  # increase total_width to reduce overlap
  y_offset = 0
)

# Join offsets to your data
df_doube_bubble <- df_test %>%
  select(-x_offset, -y_offset) %>%  # remove old offsets
  left_join(justice_positions, by = "justice_metric")

bubble_plot_combined_SDP <- function(data, var_name, layer1_sdp, layer2_sdp) {
  df_var <- data %>% 
    filter(variable == var_name)
  
  # Filter for each SDP scenario based on user input
  # Semi-transparent bubble
  df_layer1 <- df_var %>% filter(SDP == layer1_sdp, is_empty == "Explored")
  # Outline
  df_layer2 <- df_var %>% filter(SDP == layer2_sdp, is_empty == "Explored")
  # Crosses
  df_layer3 <- df_var %>% group_by(variable, scen_category, mitigation_strategy, justice_metric) %>%
    filter(all(SDP %in% c(layer1_sdp, layer2_sdp) & is_empty == "Unexplored")) %>%
    ungroup()
  
  
  ggplot() +
    # Layer 1: Filled, semi-transparent bubbles
    geom_point(data = df_layer1,
               aes(x = mitigation_strategy,
                   y = scen_category,
                   fill = justice_metric,
                   size = count
                   ),
               shape = 21,
               color = "NA",  # no border
               position = position_nudge(x = df_layer1$x_offset, y = df_layer1$y_offset),
               alpha = 0.6) +
    
    # Layer 2: Outline-only bubbles
    geom_point(data = df_layer2,
               aes(x = mitigation_strategy,
                   y = scen_category,
                   color = justice_metric,
                   size = count
                   ),
               shape = 21,
               fill = NA,  # no fill
               stroke = 1,
               position = position_nudge(x = df_layer2$x_offset, y = df_layer2$y_offset),
               alpha = 1) +
    geom_point(data = df_layer3,
               aes(x = mitigation_strategy,
                   y = scen_category
                   ),
              shape = 4, color = "grey", size = 3, stroke = 1,
              position = position_nudge(x = df_layer3$x_offset, y = df_layer3$y_offset)
    ) + 
    
    scale_fill_manual(values = justice_colors) +
    scale_color_manual(values = justice_colors) +
    scale_size(
      limits = c(1, 130),
      range = c(2, 10),
      breaks = c(25, 50, 75, 100, 125)
    ) +
    
    facet_grid(~ mitigation_strategy, scales = "free") +
    theme_ks() +
    theme(
      axis.title = element_blank(),
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      strip.background = element_rect(fill = "white")
    ) +
    ggtitle(paste(display_vars[[var_name]]))+
    theme(plot.title = element_text(size = 11))
}


# Generate plots with EI as filled layer, RC as outline
combined_SDP_plots <- lapply(vars_to_plot, function(v) {
  bubble_plot_combined_SDP(df_doube_bubble, v, 
                           layer1_sdp = "EI", #Bubble
                           layer2_sdp = "RC") #Outline
})

final_combined_plot <- wrap_plots(combined_SDP_plots, guides = "collect", ncol = 1) +
  plot_annotation(title = "Patterns of Justice in AR6 scenarios \nacross temperature targets and mitigation strategies",
                  subtitle = 'Fill: Level A parameterization | Outline: Level B parameterization') &
  theme(legend.position = "bottom", legend.box = "vertical")

print(final_combined_plot)

ggsave("output/justice_metrics_combined_bubbles.png", final_combined_plot, width = 10, height = 15)



# getting numbers-----

# Sample statistics

# Amount of total scenarios
df_plot %>%
  filter(scen_category %in% c("C1", "C2", "C3", "C4", "C5"))%>%
  distinct(model, scenario) %>%
  nrow()

# Amount of scenarios for each variable
df_plot %>%
  filter(scen_category %in% c("C1", "C2", "C3", "C4", "C5"))%>%
  group_by(variable, SDP)%>%
  summarise(n = n(), .groups='drop')

# N of scenarios for each mitigation strategy
df_plot %>%
  filter(scen_category %in% c("C1", "C2", "C3", "C4", "C5"))%>%
  distinct(model, scenario, mitigation_strategy) %>%
  group_by(mitigation_strategy)%>%
  summarise(n = n(), .groups='drop')


# N of scenarios for each climate category
df_plot %>%
  filter(scen_category %in% c("C1", "C2", "C3", "C4", "C5"))%>%
  distinct(model, scenario, scen_category, mitigation_strategy) %>%
  group_by(mitigation_strategy, scen_category)%>%
  summarise(n = n(), .groups='drop')

# N of scenarios for each variable and mitigation strategy
df_plot %>%
  filter(SDP == 'RC' & scen_category %in% c("C1", "C2", "C3", "C4", "C5")) %>%
  group_by(variable, mitigation_strategy) %>% distinct(model, scenario) %>%
  count()

df_counts %>%
  filter(scen_category %in% c("C1", "C2", "C3", "C4", "C5") &
           is_empty == 'Explored')%>%
  
  #group_by(justice_metric)%>% summarise(n = n())
  
  # Sample Description/ TABLE
  mutate(C1_C2 = ifelse(scen_category %in% c('C1', 'C2'), "Yes", "No")) %>%
  group_by(variable, mitigation_strategy, C1_C2)%>%
  summarise(n = sum(count), .groups='drop') %>%
  pivot_wider(names_from = C1_C2, values_from = n) %>%
  mutate(Yes = ifelse(is.na(Yes), 0, Yes)) %>%
  mutate(
    n = Yes + No,
    share_C1_C2 = Yes/No
    )%>%
  select(-No, -Yes) %>%
  # get total number of scenarios  
  # summarise(n=sum(n))  
  # get number of scenarios for each variable
  # group_by(variable)%>% summarise(n = sum(n), .groups='drop')
  # get number of scenarios for each mitigation strategy
   group_by(mitigation_strategy)%>% summarise(n = sum(n), .groups='drop')
  


# Additional analysis --------

## UpSet Plots
library(ComplexUpset)
library(stringr)

#TODOS
# Add solving type
# Add economic framework
# Get legend and colors in order

## Prepare data -----
# Select columns
df_upset <- df_plot %>%
  select(variable, model, scenario, SDP, scen_category, mitigation_strategy, ssp_family, project_study,
         `Total Utilitarian`, Prioritarian, Egalitarian, Sufficientarian, Limitarian)

# Define patterns
patterns <- rev(colnames(df_upset)[9:13])


# Add scenario information
df_upset <- df_upset %>%
  mutate(across(c(`Total Utilitarian`, Prioritarian, Egalitarian, Sufficientarian, Limitarian),
                ~ .x == "yes"))%>%
  # Add model info
  mutate(model_cat = case_when(
    startsWith(model, "MESSAGEix") ~ "MESSAGEix",
    startsWith(model, "REMIND") ~ "REMIND",
    startsWith(model, "WITCH") ~ "WITCH",
    startsWith(model, "GCAM") ~ "GCAM",
    startsWith(model, "IMAGE") ~ "IMAGE",
    startsWith(model, "GEM-E3") ~ "GEM-E3",
    TRUE ~ "other"
  )) %>%
  # Add SSP info
  mutate(ssp = case_when(
    ssp_family == 1 ~ "SSP1",
    ssp_family == 2 ~ "SSP2",
    ssp_family == 3 ~ "SSP3",
    ssp_family == 4 ~ "SSP4",
    ssp_family == 5 ~ "SSP5",
    TRUE ~ "other"
  )) %>%
  # Improve naming of variables
  mutate(variable_display = case_match(
    variable,
    "Final_Energy_Residential_and_Commercial" ~ "Energy for Housing",
    "Final_Energy_Transportation" ~ "Energy for Transportation",
    "Regional_Meat_Consumption" ~ "Meat Consumption",
    .default = variable
  )) %>%
  # Improve naming of SDPs
  mutate(parameterization = case_match(
    SDP, 
    "RC" ~ "Lenient",
    "EI" ~ "Strict"
    )
  ) %>%
  #Add meta data on whether scenario is classified
  mutate(
    classified = if_else(rowSums(select(., all_of(patterns))) > 0, TRUE, FALSE),
    classified = factor(classified, levels = c(TRUE, FALSE))  # enforce both levels
  ) %>%
  # # Add pathway info
  # mutate(
  #   pathway = case_when(
  #     str_detect(str_to_lower(scenario), "ndc")  ~ "ndc",
  #     str_detect(str_to_lower(scenario), "npi")  ~ "npi",
  #     str_detect(str_to_lower(scenario), "ssp1") ~ "ssp1",
  #     str_detect(str_to_lower(scenario), "ssp2") ~ "ssp2",
  #     str_detect(str_to_lower(scenario), "ssp3") ~ "ssp3",
  #     str_detect(str_to_lower(scenario), "ssp4") ~ "ssp4",
  #     str_detect(str_to_lower(scenario), "ssp5") ~ "ssp5",
  #     TRUE ~ "other"
  #   )
  # ) %>%
  # Set legend order via factors
  mutate(
    mitigation_strategy = factor(mitigation_strategy, 
                                 levels = c("Demand", "Renewables","CDR")),
    model_cat = factor(model_cat,
                       levels = c("MESSAGEix", "REMIND", "WITCH",  "GCAM", "GEM-E3", "IMAGE", "other")),
    ssp = factor(ssp, 
                 levels = c("SSP1","SSP2","SSP3","SSP4","SSP5","other")
                 ),
    parameterization = factor(parameterization,
                              levels=c('Strict', "Lenient"))
    )%>%
  filter(scen_category %in% c("C1", "C2", "C3", "C4", "C5"))
  




## Plot general classification results categorized vs uncategorised ------
# Prepare data
df_cat_vs_nocat <- df_plot %>%
  # only display climate mitigation scenarios C1 - C5
  filter(scen_category %in% c("C1", "C2", "C3", "C4", "C5")) %>%
  mutate(pattern_detected = if_any(
    c(`Total Utilitarian`, Prioritarian, Egalitarian, Sufficientarian, Limitarian),
    ~ .x == "yes"
  )) %>%
  group_by(variable, SDP)  %>%
  summarize(
    classified_count = sum(pattern_detected, na.rm = TRUE),
    unclassified_count = sum(!pattern_detected, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(classified_count, unclassified_count),
    names_to = 'classification',
    values_to = 'count'
  ) %>%
  group_by(variable, SDP) %>%
  mutate(
    total = sum(count),
    percentage = round(100 * count / total, 1)
  ) %>%
  mutate(parameterization = case_match(
    SDP, 
    "RC" ~ "Level B",
    "EI" ~ "Level A"
    )) %>%
  mutate(parameterization = factor(parameterization,
                            levels=c('Level A', "Level B"))) %>%
  mutate(variable_display = case_match(
    variable,
    "Final_Energy_Residential_and_Commercial" ~ "Energy for Housing",
    "Final_Energy_Transportation" ~ "Energy for Transportation",
    "Regional_Meat_Consumption" ~ "Meat Consumption",
    .default = variable
  )) 

# Make Plot
df_cat_vs_nocat %>%
  mutate(classified = ifelse(classification == 'classified_count',
                             "Scenarios featuring Patterns of Justice",
                             "Scenarios not featuring Patterns of Justice")) %>%
  ggplot(aes(x = classified, y = count, fill = classified)) +
  geom_col() +
  geom_text(
    aes(label = paste0(count, " (", round(percentage, 0), "%)")),
    vjust = -0.5,
    size = 4
  ) +
  facet_grid(parameterization ~ variable_display)+
  scale_fill_manual(
    name = " ",
    values = c("Scenarios not featuring Patterns of Justice" = "#999999",
               "Scenarios featuring Patterns of Justice" = "#1b9e77")) +
  ylim(c(0, 650)) +
  labs(
    x = NULL,
    y = "Number of Scenarios",
    title = "Patterns of Distributional Justice found in AR6 Scenarios",
    subtitle = "across variables and parameterizations"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),   
    axis.ticks.x = element_blank(),  
    axis.ticks.y = element_blank(),  
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

## UpSet Plot ----------
upset(df_upset, patterns, name='pattern', width_ratio=0.1, sort_sets=FALSE)

# build queries for each set
justice_queries <- lapply(names(justice_colors), function(p) {
  upset_query(
    set = p,
    fill  = justice_colors[p],
    color = justice_colors[p],
    only_components = c("intersections_matrix")
  )
})

# Set colors
colors_mitigation <- c(
  "CDR" = "#D98C8C",
  "Demand" = "#87AFC7",
  "Renewables" = "#8FBC8F"
)

colors_model <- c(
  "MESSAGEix" = "#8FBC8F",  # light green (dark sea green)
  "REMIND"   = "#87AFC7",  # soft sky blue
  "WITCH"    = "#D98C8C",  # muted light red (light coral)
  "GCAM"     = "#E6C39A",  # warm beige (tan/light gold)
  "GEM-E3"   = "#B9AEDC",  # soft lavender
  "IMAGE"    = "#F0E68C",  # pale yellow (khaki)
  "Other"    = "#CCCCCC"   # light grey
)


colors_pathway <- c(
  "SSP1" = "#8FBC8F",    # softer green, friendly and clear
  "SSP2" = "#87AFC7",    # muted purple, less intense than original
  "SSP3" = "#D98C8C",    # warm soft orange, easy on eyes
  "SSP4" = "#E6C39A",    # muted sienna brown, earthy tone
  "SSP5" = "#F48FB1",    # soft pink, gentle and distinct
  "other" = "#CCCCCC"     # neutral grey for others
)


#specify var
var = 'Final_Energy_Residential_and_Commercial'
# now run upset with custom colors
upset(
  df_upset %>% 
    filter(
      classified == TRUE,
      parameterization == "Strict"
           ),
  patterns,
  name = '',
  width_ratio = 0.1,
  set_sizes = 
    upset_set_size(geom=geom_bar(width=0.4))
  + theme(
    axis.text.x=element_text(angle = 90),
    axis.ticks.x=element_line(),
    panel.background = element_blank(),      # remove background
    panel.grid = element_blank()
    ),
  stripes = NA,
  queries = justice_queries,
  sort_sets = FALSE,
  #group_by = "degree",
  max_degree=1,
  sort_intersections_by = c('degree', 'cardinality'),
  sort_intersections = 'descending',
  base_annotations=list(
    # 'Interaction size\n(% of occurances in this intersection alone)'=intersection_size(
    #   text_mapping=aes(label=paste0(
    #     round(
    #       !!get_size_mode('exclusive_intersection')/!!get_size_mode('inclusive_union') * 100
    #     ),'%'))
    #   ,
    #     fill = 'white',         # no fill
    #     colour = "black",   # border color
    #     text = list(colour = "black", size = 3) 
    # ) + theme (
      'Intersection ratio\n(% of occurances in a certain intersection alone)' = intersection_ratio(
        text_mapping=aes(label=!!upset_text_percentage()))+
        theme(
      panel.grid = element_blank(),
      panel.background = element_blank() 
    )),
  #intersections='all',
  #   counts=FALSE,
  #   mapping=aes(fill=mitigation_strategy)
  # ) + scale_fill_manual(values=c(
  #     'CDR'='#E41A1C', 'Demand'='#377EB8',
  #     'Renewables'='#4DAF4A')),
  # annotations = list(
  #   # 'Mitigation Strategies'=(
  #   #   ggplot(mapping=aes(fill=mitigation_strategy))
  #   #   + geom_bar(stat='count', position='fill')
  #   #   + scale_y_continuous(labels=scales::percent_format())
  #   #   + scale_fill_manual(values=colors_mitigation)
  #   #   + ylab('Mitigation Strategy')
  #   # ), 
  #   
  #   "Model Category" = (
  #     ggplot(mapping=aes(fill=model_cat))
  #     + geom_bar(stat='count', position='fill')
  #     + scale_y_continuous(labels=scales::percent_format())
  #     + scale_fill_manual(values=colors_model) 
  #     + ylab('Model')
  #   ),
  #   
  #   "SSP" = (
  #     ggplot(mapping = aes(fill = ssp)) +
  #       geom_bar(stat = 'count', position = 'fill') +
  #       scale_y_continuous(labels = scales::percent_format()) +
  #       scale_fill_manual(values = colors_pathway)
  #   ) + ylab('SSP')
    # "Project Studies" =(
    #   ggplot(mapping=aes(fill=project_study))
    #     + geom_bar(stat='count', position='fill')
    #     + scale_y_continuous(labels=scales::percent_format())
    #   + scale_fill_viridis_d(option = "plasma")
    # )         + ylab('Project Study')
    # )
)+ 
  theme(
    axis.text.y = element_text(size = 12),                           # set labels
  panel.background = element_blank(),       # remove background
  panel.grid = element_blank() 
        )+
  labs(title='Interactions of Patterns of Justice in AR6 Scenarios ',
       subtitle = 'Level A parameterization, across all variables')
    

# home_made_data
library(scales)

intersection_data <- data.frame(
  pattern = c("Total Utilitarian", "Prioritarian", "Egalitarian", "Sufficientarian", "Limitarian"),
  occurrences_alone = c(0.06, 0.47, 0.06, 0, 0.87)) %>%
  mutate(occurrences_combination = 1 - occurrences_alone) %>%
  pivot_longer(cols = c("occurrences_alone", "occurrences_combination"),
               names_to = "type",
               values_to = "value")%>%
  mutate(
      pattern = factor(pattern, levels = rev(c("Total Utilitarian", "Prioritarian", "Egalitarian", "Sufficientarian", "Limitarian"))),
      type = factor(type, levels = rev(c("occurrences_alone", "occurrences_combination")))
    )

ggplot(intersection_data, aes(x = pattern, y = value, fill = pattern, alpha = type, color = type)) +
    geom_bar(stat = "identity", size = 1) +
  geom_text(data=subset(intersection_data, value > 0),
            aes(label = percent(value, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "black")+
    scale_fill_manual(values = justice_colors) +
    scale_alpha_manual(values = c("occurrences_alone" = 1, "occurrences_combination" = 0.5)) +
    scale_color_manual(values = c("occurrences_alone" = "black", "occurrences_combination" = "black"))+
  coord_flip()+
  theme_ks() +
  theme(
    axis.title = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "None",
    legend.text = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white")
  ) +
  labs(title='Intersection ratios of Patterns of Justice',
       subtitle = 'Level A parameterization, across all variables')
  
  
  
# Plot
ggplot(intersection_data, aes(x = patterns, y = value)) +
  geom_bar(aes(fill = fill_color), stat = "identity", width = 0.7, show.legend = FALSE) +
  geom_bar(aes(color = outline_color), stat = "identity", fill = NA, width = 0.7, linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values = justice_colors, na.value = NA) +
  scale_color_manual(values = justice_colors, na.value = NA) +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "Proportion") +
  theme(axis.text.y = element_text(size = 12))

# Wrap around multiple variables
plots <- lapply(unique(df_upset$variable), function(v) {
  df_subset <- df_upset[df_upset$variable == v, ]
  ComplexUpset::upset(
    df_upset %>% 
      filter(variable == 'Final_Energy_Transportation',
             scen_category %in% c("C1", 'C2'),
             classified == TRUE
      ),
    patterns,
    name = 'Distributional Justice Pattern',
    width_ratio = 0.1,
    set_sizes = 
      upset_set_size()
    + theme(
      axis.text.x=element_text(angle = 90),
      axis.ticks.x=element_line()),
    queries = justice_queries,
    sort_sets = FALSE,
    # sort_intersections = 'descending',
    group_by = "degree",
    max_degree=2,
    #sort_intersections_by = "degree",
    #sort_intersections = "ascending",
    base_annotations=list(
      #'Intersection ratio' = intersection_ratio(text_mapping=aes(label=!!upset_text_percentage())),
      'Intersection size'=intersection_size()
    ),
    annotations = list(
      "Model Category" = (
        ggplot(mapping=aes(fill=model_cat))
        + geom_bar(stat='count', position='fill')
        + scale_y_continuous(labels=scales::percent_format())
        + scale_fill_manual(values=colors_model) 
        + ylab('Model')
      ),
      
      "SSP" = (
        ggplot(mapping = aes(fill = ssp)) +
          geom_bar(stat = 'count', position = 'fill') +
          scale_y_continuous(labels = scales::percent_format()) +
          scale_fill_manual(values = colors_pathway)
      ) + ylab('SSP')
    )
  )+ theme(legend.spacing.x = unit(0.4, "cm"),
           legend.text = element_text(size = 9))
   + ggtitle(v)
})

wrap_plots(plots)


# HEATMAPS
get_matrix_data <-function(data, matrix_variable){
  df_matrix <- data %>%
    pivot_longer(
      cols= all_of(patterns), 
      names_to = 'pattern',
      values_to = 'classified'
    )  %>%
    #filter only on strict categorization
    filter(SDP == "EI", 
           scen_category %in% c("C1", "C2", "C3", "C4", "C5")) %>%
    mutate(classified = case_when(
      classified == 'yes' ~ TRUE,
      classified == 'no' ~ FALSE, 
      .default = NA)
    ) %>%
    group_by(variable, .data[[matrix_variable]], pattern) %>%
    summarize(
      classified_count = sum(classified, na.rm = TRUE),
      unclassified_count = sum(!classified, na.rm = TRUE)
    )%>%
    mutate(variable_display = case_match(
      variable,
      "Final_Energy_Residential_and_Commercial" ~ "Energy for Housing",
      "Final_Energy_Transportation" ~ "Energy for Transportation",
      "Regional_Meat_Consumption" ~ "Meat Consumption",
      .default = variable
    )) %>%
    #get factor levels
    mutate(
      pattern = factor(pattern, levels = rev(c("Total Utilitarian", "Prioritarian", "Egalitarian", "Sufficientarian", "Limitarian")))
    )
}

plot_heatmap <- function(data, heatmap_variable, heatmap_title){
  ggplot(data, aes(x = .data[[heatmap_variable]], y = pattern)) +
    geom_tile(aes(fill = if_else(classified_count == 0, 
                                 "zero",
                                 as.character(pattern))),
              color = "white") +
    geom_text(
      data = data%>%filter(classified_count > 0),
      aes(label = classified_count),
      color = "white", 
      size = 3
    ) +
    scale_fill_manual(values = c(zero = 'grey90', justice_colors), na.value = "white", guide = "none") +
    facet_wrap(~ variable_display, ncol = 1, scales = "free_x") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      strip.text = element_text(size = 12, face = "bold"),
      panel.grid = element_blank()
    ) +
    labs(
      x = "",
      y = "",
      title = paste0("Distribution of Justice Patterns in Climate Mitigation Scenarios \nAcross ", heatmap_title),
      subtitle = "Faceted by variable, Level A parameterization"
    )
  
}

test <- get_matrix_data(df_plot %>%
                          filter(project_study != ""), "project_study")
plot_heatmap(test, "project_study", "Project/ Study")

test <- get_matrix_data(df_plot %>%
                          mutate(ssp = case_when(
                            ssp_family == 1 ~ "SSP1",
                            ssp_family == 2 ~ "SSP2",
                            ssp_family == 3 ~ "SSP3",
                            ssp_family == 4 ~ "SSP4",
                            ssp_family == 5 ~ "SSP5",
                            TRUE ~ "other"
                          )),
                        "ssp")
plot_heatmap(test, "ssp", "SSP")


test <- get_matrix_data(df_plot %>%
                          mutate(model_cat = case_when(
                            startsWith(model, "MESSAGEix") ~ "MESSAGEix",
                            startsWith(model, "REMIND") ~ "REMIND",
                            startsWith(model, "WITCH") ~ "WITCH",
                            startsWith(model, "GCAM") ~ "GCAM",
                            startsWith(model, "IMAGE") ~ "IMAGE",
                            startsWith(model, "GEM-E3") ~ "GEM-E3",
                            TRUE ~ "other"
                          )) %>%
                          mutate(
                            model_cat = factor(model_cat,
                                               levels = c("MESSAGEix", "REMIND", "WITCH",  "GCAM", "GEM-E3", "IMAGE", "other"))
                          )
                          , "model_cat")


plot_heatmap(test, "model_cat", "Models")


# SANKEY DIAGRAM------
library(networkD3)
library(stringr)


patterns <- c("Total Utilitarian", "Prioritarian", "Egalitarian", "Sufficientarian", "Limitarian", "none")

#Identify scenarios that have Prioritarian only in Final_Energy_Transportation
transportation_prior_only <- df_plot %>%
  filter(variable == "Final_Energy_Transportation",
         Prioritarian == "yes",
         `Total Utilitarian` == "no",
         Egalitarian == "no",
         Sufficientarian == "no",
         Limitarian == "no") %>%
  filter(scen_category %in% c("C1", "C2", "C3", "C4", "C5")) %>%
  select(model, scenario)

# Prepare data
df_sankey <- df_plot %>% 
  filter(variable != "Regional_Meat_Consumption") %>%
  filter(scen_category %in% c("C1", "C2", "C3", "C4", "C5")) %>%
  #filter on scenarios with Prioriatrian only in Final_Energy_Housing
  semi_join(transportation_prior_only, by = c("model", "scenario")) %>%
  #add seperate pattern for scenarios without any pattern
  mutate(none = ifelse(
      Prioritarian == "no" &
      `Total Utilitarian` == "no" &
      Egalitarian == "no" & 
      Sufficientarian == "no" &
      Limitarian == "no", 
      'yes', 'no')) %>%
  pivot_longer(cols = c(`Total Utilitarian`:Limitarian, none), names_to = "classification", values_to = 'value')%>%
  filter(
    value == 'yes',
    SDP == "EI") %>%
  select(-value) %>%
  mutate(classification = ifelse(
    variable == "Final_Energy_Residential_and_Commercial", 
    paste0(classification, "_Housing"),
    paste0(classification, "_Transport")
  ))

df_housing <- df_sankey %>% filter(variable == 'Final_Energy_Residential_and_Commercial')
df_transport <- df_sankey %>% filter(variable == 'Final_Energy_Transportation')

# Cross joint to allow for all pattern combinations
df_sankey <- df_transport %>%
  inner_join(df_housing, by = c("model", "scenario"), suffix = c("_source", "_target"), 
             relationship = "many-to-many") %>%
  group_by(classification_source, classification_target) %>%
  summarise(value = n(), .groups = "drop") %>%
  mutate(group = sapply(classification_target, function(x) {
           patterns[str_detect(x, patterns)]
         })) %>%
  filter(classification_source != "none_Housing")
  

  

#following instructions https://r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html

#create nodes df
nodes <- data.frame(
  name=c(as.character(df_sankey$classification_source), as.character(df_sankey$classification_target)) %>% 
    unique()
)
#ensure nodes are colored
nodes$group <- sapply(nodes$name, function(x) {
  sub("_(Transport|Housing)$", "", x)  # remove suffix
})

#create IDs
df_sankey$IDsource <- match(df_sankey$classification_source, nodes$name)-1 
df_sankey$IDtarget <- match(df_sankey$classification_target, nodes$name)-1

#set color
my_color <- 'd3.scaleOrdinal()
.domain(["Total Utilitarian", "Prioritarian", "Egalitarian", "Sufficientarian", "Limitarian", "none"])
.range(["#A6D854", "#FC8D62", "#66C2A5", "#8DA0CB", "#E78AC3", "grey"])'

#make the network
p <- sankeyNetwork(Links = df_sankey, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale=my_color,
                   LinkGroup="group", NodeGroup="group", sinksRight=FALSE)

p

#Take-away: patterns can be different across variables!
#Enforcing a pattern in one variable does not mean other variables follow a patterns as well
#plot shows: Scenarios with only prioritarian pattern in housing and their patterns in transport