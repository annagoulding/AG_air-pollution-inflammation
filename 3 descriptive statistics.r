# 3 Descriptive statistics
# 27/06/2023
# Anna Goulding

# packages ####
library(tidyverse)

# functions ####
# summarise covariates using labels if available
covar_desc <- function(df, variable, time_point){
  df %>%
    group_by({{variable}}) %>% 
    summarise(n_obs = n()) %>% 
    ungroup() %>% 
    mutate(covariate = haven::as_factor({{variable}}, levels = "default")) %>% 
    mutate(percent = (n_obs/sum(n_obs))*100) %>% 
    mutate(age = time_point) %>% 
    select(age, covariate, n_obs, percent)

}

# set data working directory ####
data <- "//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/wt1/wp3/037/working/data/"

# open datasets ####
df_age_7 <- readRDS(paste0(data, "df_age_7.rds"))
df_age_9 <- readRDS(paste0(data, "df_age_9.rds"))
df_age_15 <- readRDS(paste0(data, "df_age_15.rds"))
df_age_18 <- readRDS(paste0(data, "df_age_18.rds"))
df_age_24 <- readRDS(paste0(data, "df_age_24.rds"))

# Age 7 ####
# Outcomes
age_7_outcomes <- df_age_7 %>% 
  summarise(n_obs = n(),
            median_glyca = median(Gp_F7_log),
            iqr_glyca = IQR(Gp_F7_log),
            min_glyca = min(Gp_F7_log),
            max_glyca = max(Gp_F7_log)) %>% 
  mutate(age = 7)

# Exposures
age_7_exposures <- df_age_7 %>% 
  summarise(n_obs = n(),
            median_pm25 = median(pm25_age7_std, na.rm = T),
            iqr_pm25 = IQR(pm25_age7_std, na.rm = T),
            min_pm25 = min(pm25_age7_std, na.rm = T),
            max_pm25 = max(pm25_age7_std, na.rm = T),
            median_no2 = median(no2_age7_std, na.rm = T),
            iqr_no2 = IQR(no2_age7_std, na.rm = T),
            min_no2 = min(no2_age7_std, na.rm = T),
            max_no2 = max(no2_age7_std, na.rm = T),
            median_bc = median(bc_age7_std, na.rm = T),
            iqr_bc = IQR(bc_age7_std, na.rm = T),
            min_bc = min(bc_age7_std, na.rm = T),
            max_bc = max(bc_age7_std, na.rm = T)) %>% 
  mutate(age = 7)

# Covariates
age_7_sex <- covar_desc(df_age_7, kz021, 7) 
age_7_class <- covar_desc(df_age_7, social_class, 7) 
age_7_imd <- covar_desc(df_age_7, f7imd2000q5, 7)
age_7_mated <- covar_desc(df_age_7, c645a, 7)

# Age 7 ####
# Outcomes
age_7_outcomes <- df_age_7 %>% 
  summarise(n_obs = n(),
            median_glyca = median(Gp_F7_log),
            iqr_glyca = IQR(Gp_F7_log),
            min_glyca = min(Gp_F7_log),
            max_glyca = max(Gp_F7_log)) %>% 
  mutate(age = 7)

# Exposures
age_7_exposures <- df_age_7 %>% 
  summarise(n_obs = n(),
            median_pm25 = median(pm25_age7_std, na.rm = T),
            iqr_pm25 = IQR(pm25_age7_std, na.rm = T),
            min_pm25 = min(pm25_age7_std, na.rm = T),
            max_pm25 = max(pm25_age7_std, na.rm = T),
            median_no2 = median(no2_age7_std, na.rm = T),
            iqr_no2 = IQR(no2_age7_std, na.rm = T),
            min_no2 = min(no2_age7_std, na.rm = T),
            max_no2 = max(no2_age7_std, na.rm = T),
            median_bc = median(bc_age7_std, na.rm = T),
            iqr_bc = IQR(bc_age7_std, na.rm = T),
            min_bc = min(bc_age7_std, na.rm = T),
            max_bc = max(bc_age7_std, na.rm = T)) %>% 
  mutate(age = 7)

# Covariates
age_7_sex <- covar_desc(df_age_7, kz021, 7) 
#age_7_age <- mean(df_age_7$f7003c_years, na.rm = T)
age_7_class <- covar_desc(df_age_7, social_class, 7) 
age_7_imd <- covar_desc(df_age_7, f7imd2000q5, 7)
age_7_mated <- covar_desc(df_age_7, c645a, 7)
