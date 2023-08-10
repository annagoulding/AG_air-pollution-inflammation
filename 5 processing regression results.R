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

# functions ####
# Function to create and save forest plots as images
create_and_save_forest_plot <- function(df, outcome) {
  df_filtered <- df %>% 
    filter(Outcome == outcome) 
  
  lab1 <- c(expression("Black carbon"),
            expression(NO['2']), 
            expression(PM["2.5"]))
  
  colours <- c('#00bf7d', '#00b4c5', '#0073e6', '#2546f0', '#5928ed')
  ages <- c(7, 9, 15, 18, 24)
  palette <- data.frame(ages, colours)
  
  df_filtered$time_point <- factor(df_filtered$time_point, levels = rev(levels(df_filtered$time_point)))  # Reverse order of time_point levels
  
  plot <- ggplot(df_filtered, aes(x = estimate_bt, xmin = conf.low_bt, xmax = conf.high_bt, y = term, color = time_point)) +
    geom_pointrange(position = position_dodge2(width = 0.5), size = 1.2, fatten = 1.5) +  
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.5) +
    labs(title = paste("Estimates and 95% CIs for", outcome),
         subtitle = "Reverse Log-Transformed Estimates and Confidence Intervals",
         x = "Estimate (95% CI)",
         y = "Air pollutant exposure",
         color = "Time Point") +
    scale_color_manual(values = setNames(palette$colours, palette$ages)) +
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
    scale_x_continuous(limits = c(0.7, 1.8), breaks = c(0.75, 1, 1.25, 1.5, 1.75)) +  # Set x-axis scale limits and desired tick marks
  guides(color = guide_legend(reverse = TRUE))  # Reverse the order of legend items
  
  # Save the plot as an image file
  filename <- paste0(outcome, "_forest_plot.png")
  ggsave(paste0(results, filename), plot, width = 8, height = 5, dpi = 300)
  
  return(filename)
}
  

# open datasets ####
df_age_7 <- readRDS(paste0(data, "df_age_7.rds"))
df_age_9 <- readRDS(paste0(data, "df_age_9.rds"))
df_age_15 <- readRDS(paste0(data, "df_age_15.rds"))
df_age_18 <- readRDS(paste0(data, "df_age_18.rds"))
df_age_24 <- readRDS(paste0(data, "df_age_24.rds"))
all_models <- readRDS(paste0(results, "all_models_natural_units.rds"))


# Create plots ####
# Edit variable contents
all_models_edited <- all_models %>%
  mutate(term = str_split(term, "_", simplify = TRUE)[, 1],
         Outcome = str_split(Outcome, "_", simplify = TRUE)[, 1],
         term = toupper(term),
         Outcome = toupper(Outcome),
         time_point = as.factor(time_point)) %>% 
  mutate(Outcome = case_when(Outcome == "GP" ~ "GlycA",
                             Outcome== "IL6" ~ "IL-6",
                             T ~ Outcome))

# List of outcomes
outcomes <- unique(all_models_edited$Outcome)

# Create and save separate forest plots for each outcome
plot_filenames <- lapply(outcomes, function(outcome) {
  create_and_save_forest_plot(all_models_edited, outcome)
})



# Create tables of regression results ####
combined_estimates <- all_models_edited %>% 
  mutate(pres_est = paste0(format(round(estimate_bt, digits = 2), nsmall = 2), " (", format(round(conf.low_bt, digits = 2), nsmall = 2), 
                           " - ", format(round(conf.high_bt, digits = 2), nsmall = 2), ")")) %>% 
  select(c(time_point, Outcome, term, pres_est, p.value)) %>% 
  pivot_wider(id_cols = c(time_point, Outcome), names_from = term, values_from = c(pres_est, p.value),
              names_vary= "slowest")


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




