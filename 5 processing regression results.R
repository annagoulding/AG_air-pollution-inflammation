# 5 Processing regression output
# 18/07/2023
# Anna Goulding

# packages ####
library(tidyverse)
library(openxlsx)
library(ggplot2)

# set working directories ####
data <- "//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/wt1/wp3/037/working/data/"
results <- "//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/wt1/wp3/037/working/results/"

# open datasets ####
df_age_7 <- readRDS(paste0(data, "df_age_7.rds"))
df_age_9 <- readRDS(paste0(data, "df_age_9.rds"))
df_age_15 <- readRDS(paste0(data, "df_age_15.rds"))
df_age_18 <- readRDS(paste0(data, "df_age_18.rds"))
df_age_24 <- readRDS(paste0(data, "df_age_24.rds"))
all_models_nu <- readRDS(paste0(results, "all_models_natural_units.rds"))
all_models_standardised <- readRDS(paste0(results, "all_models_standardised.rds"))


# Simplify exposure and outcome variables
all_models_nu_edited <- all_models_nu %>%
  mutate(term = str_split(term, "_", simplify = TRUE)[, 1],
         Outcome = str_split(Outcome, "_", simplify = TRUE)[, 1],
         term = toupper(term),
         Outcome = toupper(Outcome),
         time_point = as.factor(time_point)) %>% 
  mutate(Outcome = case_when(Outcome == "GP" ~ "GlycA",
                             Outcome== "IL6" ~ "IL-6",
                             T ~ Outcome))

all_models_standardised_edited <- all_models_standardised %>%
  mutate(term = str_split(term, "_", simplify = TRUE)[, 1],
         Outcome = str_split(Outcome, "_", simplify = TRUE)[, 1],
         term = toupper(term),
         Outcome = toupper(Outcome),
         time_point = as.factor(time_point)) %>% 
  mutate(Outcome = case_when(Outcome == "GP" ~ "GlycA",
                             Outcome== "IL6" ~ "IL-6",
                             T ~ Outcome))


# Create tables of regression results ####
combined_estimates_nu <- all_models_nu_edited %>% 
  mutate(pres_est = paste0(format(round(estimate_bt, digits = 2), nsmall = 2), " (", format(round(conf.low_bt, digits = 2), nsmall = 2), 
                           " - ", format(round(conf.high_bt, digits = 2), nsmall = 2), ")")) %>% 
  select(c(time_point, Outcome, term, pres_est, p.value)) %>% 
  pivot_wider(id_cols = c(time_point, term), names_from = Outcome, values_from = c(pres_est, p.value),
              names_vary= "slowest")

combined_estimates_std <- all_models_standardised_edited %>% 
  mutate(pres_est = paste0(format(round(estimate_bt, digits = 2), nsmall = 2), " (", format(round(conf.low_bt, digits = 2), nsmall = 2), 
                           " - ", format(round(conf.high_bt, digits = 2), nsmall = 2), ")")) %>% 
  select(c(time_point, Outcome, term, pres_est, p.value)) %>% 
  pivot_wider(id_cols = c(time_point, term), names_from = Outcome, values_from = c(pres_est, p.value),
              names_vary= "slowest")



#### read out into Excel template ####
template <- openxlsx::loadWorkbook(file.path(paste0(results, "Templates/Regression output template.xlsx")))

writeData(template, "Natural units", combined_estimates_nu, startCol = 1, startRow = 2, colNames = T)
writeData(template, "Standardised", combined_estimates_std, startCol = 1, startRow = 2, colNames = T)

#### Save out workbook ####
saveWorkbook(template, (paste0(results, "Regression_output_", Sys.Date(), ".xlsx")), overwrite =TRUE)



# Create plots ####
# Function to create and save forest plots as images
create_and_save_forest_plot <- function(df, time_period) {
  df_filtered <- df %>% 
    filter(time_point == time_period) 
  
  lab1 <- c(expression("Black carbon"),
            expression(NO['2']), 
            expression(PM["2.5"]))
  
  
  plot <- ggplot(df_filtered, aes(x = estimate_bt, xmin = conf.low_bt, xmax = conf.high_bt, y = term, color = Outcome)) +
    geom_pointrange(position = position_dodge2(width = 0.5), size = 1.2, fatten = 1.5) +  
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.5) +
    labs(title = paste("Estimates and 95% CIs", time_period),
         subtitle = "Reverse Log-Transformed Estimates and Confidence Intervals",
         x = "Estimate (95% CI)",
         y = "Air pollutant exposure",
         color = "Outcome") +
    scale_color_manual(values = setNames(palette$colours, palette$outcomes)) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.y = element_blank()) +  # Remove horizontal gridlines
    scale_y_discrete(labels = lab1) +
    scale_x_continuous(limits = c(0.7, 1.8), breaks = c(0.75, 1, 1.25, 1.5, 1.75)) + # Set x-axis scale limits and desired tick marks
    guides(color = guide_legend(reverse = TRUE))  # Reverse the order of legend items
  
  # Save the plot as an image file
  filename <- paste0(time_period, "_forest_plot.png")
  ggsave(paste0(results, filename), plot, width = 8, height = 5, dpi = 300)
  
  return(filename)
}

# Recode time points
all_models_edited <- all_models_nu_edited %>%
  mutate(time_point = case_when(
    time_point %in% c("7", "9") ~ "in Childhood",
    time_point == "15" ~ "in Adolescence",
    time_point == "18" ~ "at 18 Years Old",
    time_point == "24" ~ "at 24 Years Old"
  ))

# List of time periods
time_periods <- c("in Childhood", "in Adolescence", "at 18 Years Old", "at 24 Years Old")

# Create palette
colours <- c('#00bf7d', '#0073e6', '#5928ed')
outcomes <- unique(all_models_edited$Outcome)
palette <- data.frame(outcomes, colours)

# Create and save separate forest plots for each time period
plot_filenames <- lapply(time_periods, function(time_period) {
  create_and_save_forest_plot(all_models_edited, time_period)
})

