# 5 Processing regression output
# 18/07/2023
# Anna Goulding

# packages ####
library(tidyverse)
library(openxlsx)

# set working directories ####
data <- "//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/wt1/wp3/037/working/data/"
results <- "//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/wt1/wp3/037/working/results/"

# open datasets ####
df_age_7 <- readRDS(paste0(data, "df_age_7.rds"))
df_age_9 <- readRDS(paste0(data, "df_age_9.rds"))
df_age_15 <- readRDS(paste0(data, "df_age_15.rds"))
df_age_18 <- readRDS(paste0(data, "df_age_18.rds"))
df_age_24 <- readRDS(paste0(data, "df_age_24.rds"))
all_models <- readRDS(paste0(results, "all_models.rds"))

# extract and pivot p-values
p_value <- all_models %>% 
  select(!estimate) %>% 
  mutate(Outcome = str_extract(Outcome, "^[A-Za-z0-9]+")) %>% 
  mutate(Outcome = tolower(Outcome)) %>% 
  pivot_wider(id_cols = c(term, time_point), names_from = Outcome, values_from = p.value) %>% 
  select(term, time_point, everything())

# back-transform outcome values
back_trans <- all_models %>% 
  mutate(estimate_bt = exp(estimate)) %>% 
  select(!c(p.value, estimate)) %>% 
  mutate(Outcome = str_extract(Outcome, "^[A-Za-z0-9]+")) %>% 
  mutate(Outcome = tolower(Outcome)) %>% 
  pivot_wider(id_cols = c(time_point, term), names_from = Outcome, values_from = estimate_bt)



#### read out into Excel template ####
template <- openxlsx::loadWorkbook(file.path(paste0(results, "Templates/Regression output template.xlsx")))

writeData(template, "p values", p_value, startCol = 1, startRow = 1, colNames = T)
writeData(template, "Estimates", back_trans, startCol = 1, startRow = 1, colNames = T)

#### Save out workbook ####
saveWorkbook(template, (paste0(results, "Regression_output_", Sys.Date(), ".xlsx")), overwrite =TRUE)
