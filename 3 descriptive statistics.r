# 3 Descriptive statistics
# 27/06/2023
# Anna Goulding

# packages ####
library(tidyverse)
library(openxlsx)

# functions ####
# summarise covariates using labels if available
covar_desc <- function(df, variable, variable_label, time_point){
  df %>%
    group_by({{variable}}) %>% 
    summarise(n_obs = n()) %>% 
    ungroup() %>% 
    mutate(var_levels = haven::as_factor({{variable}}, levels = "default")) %>% 
    mutate(percent = (n_obs/sum(n_obs))*100) %>% 
    mutate(age = time_point) %>% 
    select(age, var_levels, n_obs, percent) %>% 
    mutate(var_levels = (haven::zap_label(var_levels))) %>% 
    mutate(covariate = variable_label)

}

# summarise exposure variables (same for all time points)
exposures_desc <- function(df, pm25, no2, bc, time_point){
 df %>% 
  summarise(n_obs_pm25 = sum(!is.na({{pm25}})),
            median_pm25 = median({{pm25}}, na.rm = T),
            iqr_pm25 = IQR({{pm25}}, na.rm = T),
            min_pm25 = min({{pm25}}, na.rm = T),
            max_pm25 = max({{pm25}}, na.rm = T),
            n_obs_no2 = sum(!is.na({{no2}})),
            median_no2 = median({{no2}}, na.rm = T),
            iqr_no2 = IQR({{no2}}, na.rm = T),
            min_no2 = min({{no2}}, na.rm = T),
            max_no2 = max({{no2}}, na.rm = T),
            n_obs_bc = sum(!is.na({{bc}})),
            median_bc = median({{bc}}, na.rm = T),
            iqr_bc = IQR({{bc}}, na.rm = T),
            min_bc = min({{bc}}, na.rm = T),
            max_bc = max({{bc}}, na.rm = T)) %>% 
  mutate(age = time_point)
}

# summarise age variables
age_desc <- function(df, age_variable, time_point){
  df %>% 
    summarise(mean_age = mean({{age_variable}}, na.rm = T),
              sd_age = sd({{age_variable}}, na.rm = T),
              min_age = min({{age_variable}}, na.rm = T),
              max_age = max({{age_variable}}, na.rm = T)) %>% 
    mutate(age = time_point)
}

# set working directories ####
data <- "//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/wt1/wp3/037/working/data/"
results <- "//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/wt1/wp3/037/working/results/"

# open datasets ####
df_age_7 <- readRDS(paste0(data, "df_age_7.rds"))
df_age_9 <- readRDS(paste0(data, "df_age_9.rds"))
df_age_15 <- readRDS(paste0(data, "df_age_15.rds"))
df_age_18 <- readRDS(paste0(data, "df_age_18.rds"))
df_age_24 <- readRDS(paste0(data, "df_age_24.rds"))


# Age 7 ####
# Outcomes
age_7_outcomes <- df_age_7 %>% 
  summarise(n_obs_glyca = sum(!is.na(Gp_F7)),
            median_glyca = median(Gp_F7),
            iqr_glyca = IQR(Gp_F7),
            min_glyca = min(Gp_F7),
            max_glyca = max(Gp_F7)) %>% 
  mutate(age = 7)

# Exposures
age_7_exposures <- exposures_desc(df_age_7, pm25_age7, no2_age7, bc_age7, 7) 
age_7_exposures_std <- exposures_desc(df_age_7, pm25_age7_std, no2_age7_std, bc_age7_std, 7) 

# Covariates
age_7_sex <- covar_desc(df_age_7, kz021, "Sex", 7) 
age_7_age <- age_desc(df_age_7, f7003c_years, 7)
age_7_class <- covar_desc(df_age_7, social_class, "Social class", 7) 
age_7_imd <- covar_desc(df_age_7, f7imd2000q5, "Area deprivation", 7)
age_7_mated <- covar_desc(df_age_7, c645a, "Maternal education", 7)

age_7_covariates <- age_7_sex %>% 
  bind_rows(age_7_class) %>% 
  bind_rows(age_7_imd) %>% 
  bind_rows(age_7_mated)


# Age 9 ####
# Outcomes
age_9_outcomes <- df_age_9 %>% 
  summarise(n_obs_crp = sum(!is.na(CRP_f9)),
            median_crp = median(CRP_f9, na.rm = T),
            iqr_crp = IQR(CRP_f9, na.rm = T),
            min_crp = min(CRP_f9, na.rm = T),
            max_crp = max(CRP_f9, na.rm = T),
            n_obs_il6 = sum(!is.na(IL6_pgml_f9)),
            median_il6 = median(IL6_pgml_f9, na.rm = T),
            iqr_il6 = IQR(IL6_pgml_f9, na.rm = T),
            min_il6 = min(IL6_pgml_f9, na.rm = T),
            max_il6 = max(IL6_pgml_f9, na.rm = T)) %>% 
  mutate(age = 9)

# Exposures
age_9_exposures <- exposures_desc(df_age_9, pm25_age9, no2_age9, bc_age9, 9) 
age_9_exposures_std <- exposures_desc(df_age_9, pm25_age9_std, no2_age9_std, bc_age9_std, 9) 

# Covariates
age_9_sex <- covar_desc(df_age_9, kz021, "Sex", 9) 
age_9_age <- age_desc(df_age_9, f9003c_years, 9)
age_9_class <- covar_desc(df_age_9, social_class, "Social class", 9) 
age_9_imd <- covar_desc(df_age_9, f9imd2000q5, "Area deprivation", 9)
age_9_mated <- covar_desc(df_age_9, c645a, "Maternal education", 9)

age_9_covariates <- age_9_sex %>% 
  bind_rows(age_9_class) %>% 
  bind_rows(age_9_imd) %>% 
  bind_rows(age_9_mated)


# Age 15 ####
# Outcomes
age_15_outcomes <- df_age_15 %>% 
  summarise(n_obs_crp = sum(!is.na(crp_TF3)),
            median_crp = median(crp_TF3, na.rm = T),
            iqr_crp = IQR(crp_TF3, na.rm = T),
            min_crp = min(crp_TF3, na.rm = T),
            max_crp = max(crp_TF3, na.rm = T),
            n_obs_glyca = sum(!is.na(Gp_TF3)),
            median_glyca = median(Gp_TF3, na.rm = T),
            iqr_glyca = IQR(Gp_TF3, na.rm = T),
            min_glyca = min(Gp_TF3, na.rm = T),
            max_glyca = max(Gp_TF3, na.rm = T)) %>% 
  mutate(age = 15)

# Exposures
age_15_exposures <- exposures_desc(df_age_15, pm25_age15, no2_age15, bc_age15, 15) 
age_15_exposures_std <- exposures_desc(df_age_15, pm25_age15_std, no2_age15_std, bc_age15_std, 15) 

# Covariates
age_15_sex <- covar_desc(df_age_15, kz021, "Sex", 15) 
age_15_age <- age_desc(df_age_15, fh0011a_years, 15)
age_15_class <- covar_desc(df_age_15, social_class, "Social class", 15) 
age_15_imd <- covar_desc(df_age_15, tf3imd2000q5, "Area deprivation", 15)
age_15_mated <- covar_desc(df_age_15, c645a, "Maternal education", 15)

age_15_covariates <- age_15_sex %>% 
  bind_rows(age_15_class) %>% 
  bind_rows(age_15_imd) %>% 
  bind_rows(age_15_mated)


# Age 18 ####
# Outcomes
age_18_outcomes <- df_age_18 %>% 
  summarise(n_obs_crp = sum(!is.na(CRP_TF4)),
            median_crp = median(CRP_TF4, na.rm = T),
            iqr_crp = IQR(CRP_TF4, na.rm = T),
            min_crp = min(CRP_TF4, na.rm = T),
            max_crp = max(CRP_TF4, na.rm = T),
            n_obs_glyca = sum(!is.na(Gp_TF4)),
            median_glyca = median(Gp_TF4, na.rm = T),
            iqr_glyca = IQR(Gp_TF4, na.rm = T),
            min_glyca = min(Gp_TF4, na.rm = T),
            max_glyca = max(Gp_TF4, na.rm = T)) %>% 
  mutate(age = 18)

# Exposures
age_18_exposures <- exposures_desc(df_age_18, pm25_age18, no2_age18, bc_age18, 18)
age_18_exposures_std <- exposures_desc(df_age_18, pm25_age18_std, no2_age18_std, bc_age18_std, 18) 

# Covariates
age_18_sex <- covar_desc(df_age_18, kz021, "Sex", 18) 
age_18_age <- age_desc(df_age_18, FJ003b, 18)
age_18_class <- covar_desc(df_age_18, social_class, "Social class", 18) 
age_18_imd <- covar_desc(df_age_18, tf4imd2000q5, "Area deprivation", 18)
age_18_mated <- covar_desc(df_age_18, c645a, "Maternal education", 18)

age_18_covariates <- age_18_sex %>% 
  bind_rows(age_18_class) %>% 
  bind_rows(age_18_imd) %>% 
  bind_rows(age_18_mated)


# Age 24 ####
# Outcomes
age_24_outcomes <- df_age_24 %>% 
  summarise(n_obs_crp = sum(!is.na(CRP_F24)),
            median_crp = median(CRP_F24, na.rm = T),
            iqr_crp = IQR(CRP_F24, na.rm = T),
            min_crp = min(CRP_F24, na.rm = T),
            max_crp = max(CRP_F24, na.rm = T),
            #n_obs_il6 = sum(!is.na(IL6_F24)),
            #median_il6 = median(IL6_F24, na.rm = T),
            #iqr_il6 = IQR(IL6_F24, na.rm = T),
            #min_il6 = min(IL6_F24, na.rm = T),
            #max_il6 = max(IL6_F24, na.rm = T),
            n_obs_glyca = sum(!is.na(Gp_F24)),
            median_glyca = median(Gp_F24, na.rm = T),
            iqr_glyca = IQR(Gp_F24, na.rm = T),
            min_glyca = min(Gp_F24, na.rm = T),
            max_glyca = max(Gp_F24, na.rm = T)) %>% 
  mutate(age = 24)

# Exposures
age_24_exposures <- exposures_desc(df_age_24, pm25_age24, no2_age24, bc_age24, 24) 
age_24_exposures_std <- exposures_desc(df_age_24, pm25_age24_std, no2_age24_std, bc_age24_std, 24) 

# Covariates
age_24_sex <- covar_desc(df_age_24, kz021, "Sex", 24) 
age_24_age <- age_desc(df_age_24, FKAR0011, 24)
age_24_class <- covar_desc(df_age_24, social_class, "Social class", 24) 
age_24_imd <- covar_desc(df_age_24, f24imd2000q5, "Area deprivation", 24)
age_24_mated <- covar_desc(df_age_24, c645a, "Maternal education", 24)

age_24_covariates <- age_24_sex %>% 
  bind_rows(age_24_class) %>% 
  bind_rows(age_24_imd) %>% 
  bind_rows(age_24_mated)


# Create unstandardised exposures table ####

desc_stats_exp <- age_7_exposures %>% 
  bind_rows(age_9_exposures) %>% 
  bind_rows(age_15_exposures) %>% 
  bind_rows(age_18_exposures) %>% 
  bind_rows(age_24_exposures) %>% 
  pivot_longer(cols = !age, names_to = "measure", values_to = "value") %>% 
  pivot_wider(names_from = age, values_from = value, names_prefix = "age_")

# Create standardised exposures table ####

desc_stats_exp_std <- age_7_exposures_std %>% 
  bind_rows(age_9_exposures_std) %>% 
  bind_rows(age_15_exposures_std) %>% 
  bind_rows(age_18_exposures_std) %>% 
  bind_rows(age_24_exposures_std) %>% 
  pivot_longer(cols = !age, names_to = "measure", values_to = "value") %>% 
  pivot_wider(names_from = age, values_from = value, names_prefix = "age_")


# Create outcomes table ####

desc_stats_out <- age_7_outcomes %>% 
  bind_rows(age_9_outcomes) %>% 
  bind_rows(age_15_outcomes) %>% 
  bind_rows(age_18_outcomes) %>% 
  bind_rows(age_24_outcomes) %>% 
  pivot_longer(cols = !age, names_to = "measure", values_to = "value") %>% 
  pivot_wider(names_from = age, values_from = value, names_prefix = "age_")


# Create covariates tables ####

desc_stats_covar <- age_7_covariates %>% 
  bind_rows(age_9_covariates) %>% 
  bind_rows(age_15_covariates) %>% 
  bind_rows(age_18_covariates) %>% 
  bind_rows(age_24_covariates) %>% 
  pivot_wider(id_cols = c(covariate, var_levels), names_from = age, 
              values_from = c(n_obs, percent), names_prefix = "age_",
              names_vary = "slowest") %>% 
  arrange(covariate)

desc_stats_age <- age_7_age %>% 
  bind_rows(age_9_age) %>% 
  bind_rows(age_15_age) %>% 
  bind_rows(age_18_age) %>% 
  bind_rows(age_24_age) %>% 
  select(age, everything())

#### read out into Excel template ####
template <- openxlsx::loadWorkbook(file.path(paste0(results, "Templates/Descriptive statistics template.xlsx")))

writeData(template, "Outcomes", select(desc_stats_out, -c(measure)), startCol = 3, startRow = 4, colNames = FALSE)
writeData(template, "Exposures", select(desc_stats_exp, -c(measure)), startCol = 3, startRow = 4, colNames = FALSE)
writeData(template, "Covariates", select(desc_stats_covar, -c(covariate)), startCol = 2, startRow = 4, colNames = FALSE)
writeData(template, "Covariates", desc_stats_age, startCol = 1, startRow = 28, colNames = FALSE)


#### Save out workbook ####
saveWorkbook(template, (paste0(results, "Descriptive_statistics_", Sys.Date(), ".xlsx")), overwrite =TRUE)

