# 7 Covariate regressions
# 03/08/2023
# Anna Goulding

# packages ####
library(tidyverse)
library(openxlsx)
library(broom)
library(haven)

# set working directories ####
data <- "//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/wt1/wp3/037/working/data/"
results <- "//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/wt1/wp3/037/working/results/"

# open datasets ####
df_age_7 <- readRDS(paste0(data, "df_age_7.rds")) %>%  mutate(time_period = 7)
df_age_9 <- readRDS(paste0(data, "df_age_9.rds")) %>%  mutate(time_period = 9)
df_age_15 <- readRDS(paste0(data, "df_age_15.rds")) %>%  mutate(time_period = 15)
df_age_18 <- readRDS(paste0(data, "df_age_18.rds")) %>%  mutate(time_period = 18)
df_age_24 <- readRDS(paste0(data, "df_age_24.rds")) %>%  mutate(time_period = 24)

# functions ####
# Function to perform regression analysis for multiple exposures against multiple outcomes
perform_regression <- function(data, exposures, outcomes) {
  # Create a list to store the results of regression analysis
  results <- list()
  
  # Loop over each exposure
  for (exposure in exposures) {
    # Loop over each outcome
    for (outcome in outcomes) {
      # Prepare the formula for regression
      formula <- formula(paste(outcome, "~", exposure))
      
      # Perform regression analysis and store the result in the list
      result <- lm(formula, data = data)
      results[[paste(exposure, outcome, sep = ".")]] <- result
    }
  }
  
  return(results)
}

# Function to extract coefficients, p-values, and confidence intervals from regression models
extract_coefficients <- function(results, age) {
  # Create an empty list to store results
  result_list <- list()
  
  # Loop over each result (exposure.outcome combination)
  for (result_name in names(results)) {
    # Extract coefficients, p-values, and confidence intervals from the result
    result <- results[[result_name]]
    tidy_result <- tidy(result, conf.int = TRUE)
    
    # Filter out the intercept coefficient
    tidy_result <- filter(tidy_result, term != "(Intercept)")
    
    # Extract exposure and outcome names from the result name using full stop as the separator
    names <- strsplit(result_name, ".", fixed = TRUE)[[1]]
    exposure_name <- names[1]
    outcome_name <- names[2]
    
    # Add exposure and outcome columns to the tidy_result dataframe
    tidy_result <- mutate(tidy_result, exposure = exposure_name, outcome = outcome_name)
    
    # Append to the list
    result_list[[result_name]] <- tidy_result
  }
  
  # Bind all results together into a single dataframe
  result_df <- do.call(bind_rows, result_list)
  
  return(result_df)
}

# Function to apply regression and extract coefficients for multiple datasets
apply_regression_to_datasets <- function(dataset_list, common_exposures, varying_exposures_list, varying_outcomes_list, time_periods) {
  # Create an empty list to store results
  results_list <- list()
  
  # Loop over each dataset and time_period
  for (i in seq_along(dataset_list)) {
    dataset <- dataset_list[[i]]
    time_period <- time_periods[[i]]
    
    # Combine common exposures with varying exposures for this dataset
    all_exposures <- c(common_exposures, varying_exposures_list[[i]])
    
    # Combine varying outcomes for this dataset
    all_outcomes <- varying_outcomes_list[[i]]
    
    # Perform regression analysis for this dataset
    results <- perform_regression(dataset, all_exposures, all_outcomes)
    
    # Extract coefficients, p-values, and confidence intervals for this dataset
    extracted_results <- extract_coefficients(results)
    
    # Append time_period column to the extracted_results dataframe
    extracted_results <- mutate(extracted_results, time_period = time_period)
    
    # Append to the list
    results_list[[i]] <- extracted_results
  }
  
  # Bind all results together into a single dataframe
  result_df <- do.call(bind_rows, results_list)
  
  return(result_df)
}


#### Run regressions ####
# List of datasets
dataset_list <- list(df_age_7, df_age_9, df_age_15, df_age_18, df_age_24)

# List of common exposure variables (same names across all datasets)
common_exposures <- c("as_factor(kz021)", "as_factor(c645a)", "as_factor(social_class)")

# List of varying exposure variables for each dataset
varying_exposures_list <- list(
  c("f7003c_years", "as_factor(f7imd2000q5)"),
  c("f9003c_years", "as_factor(f9imd2000q5)"),
  c("fh0011a_years", "as_factor(tf3imd2000q5)"),
  c("FJ003b", "as_factor(tf4imd2000q5)"),
  c("FKAR0011", "as_factor(f24imd2000q5)")
)

# List of varying outcome variables for each dataset
varying_outcomes_list <- list(
  c("pm25_age7", "bc_age7", "no2_age7"),
  c("pm25_age9", "bc_age9", "no2_age9"),
  c("pm25_age15", "bc_age15", "no2_age15"),
  c("pm25_age18", "bc_age18", "no2_age18"),
  c("pm25_age24", "bc_age24", "no2_age24")
)

# List of time_period variable
time_periods <- c(7, 9, 15, 18, 24)

# Apply regression and extract coefficients for all datasets
result_df <- apply_regression_to_datasets(dataset_list, common_exposures, varying_exposures_list, varying_outcomes_list, time_periods) %>% 
  select(time_period, exposure, outcome, everything())


#### read out into Excel template ####
template <- openxlsx::loadWorkbook(file.path(paste0(results, "Templates/Covariate regression template.xlsx")))

writeData(template, "Covariate regressions", select(result_df, -c(time_period, exposure, outcome, term)), startCol = 5, startRow = 4, colNames = FALSE)

#### Save out workbook ####
saveWorkbook(template, (paste0(results, "Covariate_regressions_", Sys.Date(), ".xlsx")), overwrite =TRUE)
