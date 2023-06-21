# Initial data exploration
# 15/06/2023
# Anna Goulding

# packages ####
library(tidyverse)
library(haven)
library(janitor)

# functions ####
rob_stand <- function(exposure){
  (exposure - median(exposure, na.rm=T)) / (IQR(exposure, na.rm=T))
}

# set data working directory ####
data <- "//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/wt1/wp3/037/working/data/"

# open dataset ####
initial_dataset <- read_dta(paste0(data, "pollution_inflammation.dta"))

# explore data ####
initial_dataset %>% 
  tabyl(c755)
initial_dataset %>% 
  tabyl(c765)
initial_dataset %>% 
  tabyl(kz021)
initial_dataset %>% 
  tabyl(c645a)


# create single parental social class variable ####
df2 <- initial_dataset %>%
  mutate(social_class = if_else(!is.na(c755), c755, c765))

df2 %>% 
  tabyl(social_class)
df2 %>% 
  tabyl(social_class, c755)
df2 %>% 
  tabyl(social_class, c765)

# standardisation of exposure variables ####
df3 <- df2 %>% 
  mutate_at(vars(matches("pm25_age") | matches("bc_age") | matches("no2_age")), list(std = ~rob_stand(.)), NA)

# delete unused variables
df3_limited <- df3 %>% 
  select(cidB4317, kz021, f7003c, f9003c, fh0011a, FJ003b, FKAR0011, matches("ms026a"), fh3019, FJMR022a, FKMS1040, matches("imd2000"), 
         c645a, social_class, matches("CRP_"), matches("crp_"), 
         matches("IL6_"), matches("Gp_"), matches("pm25_age"), matches("bc_age"), matches("no2_age"))

# drop rows with insufficient data and record how many are dropped for each reason ####
# overall dataset
# no exposure variables
num_exposure <- df3_limited %>% 
  filter_at(vars(ends_with("_std")), any_vars(!is.na(.)))

num_no_exposure <- nrow(df3_limited) - nrow(num_exposure)

# no outcome variables
num_outcome <- num_exposure %>% 
  filter_at(vars(matches("CRP_"), matches("crp_"), 
                 matches("IL6_"), matches("Gp_")), any_vars(!is.na(.)))

num_no_outcome <- nrow(num_exposure) - nrow(num_outcome)

### age 7
num_ex_out_7 <- num_outcome %>% 
  filter_at(vars(contains("age7")), any_vars(!is.na(.))) %>% 
  filter(!is.na(Gp_F7))
num_7 <- nrow(num_outcome) - nrow(num_ex_out_7)

# no confounder variables
df_age_7 <- num_ex_out_7 %>% 
  drop_na(kz021, f7003c, f7imd2000q5, c645a, social_class)
num_conf_7 <- nrow(num_ex_out_7) - nrow(df_age_7)

# save out age 7 file
saveRDS(df_age_7, file = paste0(data, "df_age_7.rds"))


### age 9
num_ex_out_9 <- num_outcome %>% 
  filter_at(vars(contains("age9")), any_vars(!is.na(.))) %>% 
  filter_at(vars(CRP_f9, IL6_pgml_f9, IL6_F9), any_vars(!is.na(.)))
num_9 <- nrow(num_outcome) - nrow(num_ex_out_9)

# no confounder variables
df_age_9 <- num_ex_out_9 %>% 
  drop_na(kz021, f9003c, f9imd2000q5, social_class)
num_conf_9 <- nrow(num_ex_out_9) - nrow(df_age_9)

# save out age 9 file
saveRDS(df_age_9, file = paste0(data, "df_age_9.rds"))


### age 15
num_ex_out_15 <- num_outcome %>% 
  filter_at(vars(contains("age15")), any_vars(!is.na(.))) %>% 
  filter_at(vars(crp_TF3, Gp_TF3), any_vars(!is.na(.)))
num_15 <- nrow(num_outcome) - nrow(num_ex_out_15)

# no confounder variables
df_age_15 <- num_ex_out_15 %>% 
  drop_na(kz021, fh0011a, tf3imd2000q5, social_class)
num_conf_15 <- nrow(num_ex_out_15) - nrow(df_age_15)

# save out age 15 file
saveRDS(df_age_15, file = paste0(data, "df_age_15.rds"))


### age 18
num_ex_out_18 <- num_outcome %>% 
  filter_at(vars(contains("age18")), any_vars(!is.na(.))) %>% 
  filter_at(vars(CRP_TF4, Gp_TF4), any_vars(!is.na(.)))
num_18 <- nrow(num_outcome) - nrow(num_ex_out_18)

# no confounder variables
df_age_18 <- num_ex_out_18 %>% 
  drop_na(kz021, FJ003b, tf4imd2000q5, social_class)
num_conf_18 <- nrow(num_ex_out_18) - nrow(df_age_18)

# save out age 18 file
saveRDS(df_age_18, file = paste0(data, "df_age_18.rds"))


### age 24
num_ex_out_24 <- num_outcome %>% 
  filter_at(vars(contains("age24")), any_vars(!is.na(.))) %>% 
  filter_at(vars(CRP_F24, IL6_F24, Gp_F24), any_vars(!is.na(.)))
num_24 <- nrow(num_outcome) - nrow(num_ex_out_24)

# no confounder variables
df_age_24 <- num_ex_out_24 %>% 
  drop_na(kz021, FKAR0011, f24imd2000q5, social_class)
num_conf_24 <- nrow(num_ex_out_24) - nrow(df_age_24)

# save out age 24 file
saveRDS(df_age_24, file = paste0(data, "df_age_24.rds"))

