# Load packages
library(tidyverse)
library(car)
library(ggfortify)
library(olsrr)
library(dplyr)
library(tidyr)
library(janitor)
library(pecan)
library(nlme)
library(corrplot)
library(broom)
library(sjPlot)
library(patchwork)
library(sjmisc)
library(sjlabelled)
library(lm.beta)
library(psych)

# Read data
all_edt_wide <- read.csv('data/wide_hc_edt.csv')
FLSA_merge <- read_csv("RMD scripts and files from Cavan/FLSA_mergeScoredVariables.csv")

# Demographics analysis and preparation
all_edt_wide <- all_edt_wide %>%
  mutate(black_dc = case_when(race == 1 ~ 0, race == 2 ~ 1, TRUE ~ NA_real_))

FLSA_merge <- FLSA_merge %>%
  mutate(black_dc = ifelse(race == 1, 0, ifelse(race == 2, 1, NA)))

# Function to calculate standardized coefficients and their confidence intervals
standardized_regression <- function(formula, data) {
  model <- lm(formula, data = data)
  std_coeffs_list <- lm.beta(model)
  
  # Extract standardized coefficients from the list
  std_coeffs <- as.numeric(std_coeffs_list$standardized.coefficients[-1])
  
  # Extract unstandardized coefficients and standard errors
  unstd_coeffs <- coef(model)[-1]  # Excluding intercept
  se <- summary(model)$coefficients[-1, "Std. Error"]  # Assuming intercept is first
  
  # Handle NA values and ensure numeric types
  valid_indices <- !is.na(std_coeffs) & !is.na(unstd_coeffs) & !is.na(se)
  std_coeffs <- std_coeffs[valid_indices]
  unstd_coeffs <- unstd_coeffs[valid_indices]
  se <- se[valid_indices]
  
  if(length(std_coeffs) != length(unstd_coeffs) || length(se) != length(std_coeffs)) {
    stop("Length mismatch in coefficients or standard errors.")
  }
  
  # Standardized errors calculation
  std_errors <- se * abs(std_coeffs / unstd_coeffs)
  
  # 95% Confidence intervals using normal distribution
  z_value <- qnorm(0.975)
  lower_ci <- std_coeffs - z_value * std_errors
  upper_ci <- std_coeffs + z_value * std_errors
  
  # Create a dataframe for output
  results <- data.frame(
    Variable = names(std_coeffs_list$standardized.coefficients[-1]),
    Std_Coefficient = std_coeffs,
    Lower_CI = lower_ci,
    Upper_CI = upper_ci
  )
  
  list(model_summary = summary(model), std_coeffs = results)
}

# Regression Analyses ====

# Demographics
demographics_results_new <- standardized_regression(hcscore ~ gender + black_dc + income_t5 + education, FLSA_merge)
print(demographics_results_new$model_summary)
print(demographics_results_new$std_coeffs)

# Big Five
bfin1_results_new <- standardized_regression(hcscore ~ black_dc + income_t5 + education + gender + ext_m_t5, FLSA_merge)
print(bfin1_results_new$model_summary)
print(bfin1_results_new$std_coeffs)

# Openness
bfin2_results_new <- standardized_regression(hcscore ~ black_dc + income_t5 + education + gender + open_m_t5, FLSA_merge)
print(bfin2_results_new$model_summary)
print(bfin2_results_new$std_coeffs)

# Neuroticism
neur_results_new <- standardized_regression(hcscore ~ black_dc + income_t5 + education + gender + neuro_m_t5, FLSA_merge)
print(neur_results_new$model_summary)
print(neur_results_new$std_coeffs)

# Conscientiousness
con_results_new <- standardized_regression(hcscore ~ black_dc + income_t5 + education + gender + con_m_t5, FLSA_merge)
print(con_results_new$model_summary)
print(con_results_new$std_coeffs)

# Agreeableness
agree_results_new <- standardized_regression(hcscore ~ black_dc + income_t5 + education + gender + ag_m_t5, FLSA_merge)
print(agree_results_new$model_summary)
print(agree_results_new$std_coeffs)

# Big Five combined
bfm2_results <- standardized_regression(hcscore ~ black_dc + income_t5 + extra + open + neur + consc + agree + education + gender, all_edt_wide)
print(bfm2_results$model_summary)
print(bfm2_results$std_coeffs)

bfm2_results_new <- standardized_regression(hcscore ~ black_dc + income_t5 + ext_m_t5 + open_m_t5 + neuro_m_t5 + ag_m_t5 + education + gender + t5sie07x, FLSA_merge)
print(bfm2_results_new$model_summary)
print(bfm2_results_new$std_coeffs)

# Humanitarianism (only Time 6 available)

amr_results_new <- standardized_regression(hcscore ~ gender + black_dc + income_t5 + education + t6iwahamer, FLSA_merge)
print(amr_results_new$model_summary)
print(amr_results_new$std_coeffs)

cm_results_new <- standardized_regression(hcscore ~ gender + black_dc + income_t5 + education + t6iwahcomm, FLSA_merge)
print(cm_results_new$model_summary)
print(cm_results_new$std_coeffs)

w_results_new <- standardized_regression(hcscore ~ gender + black_dc + income_t5 + education + t6iwahworld, FLSA_merge)
print(w_results_new$model_summary)
print(w_results_new$std_coeffs)


# Generativity as outcome variable
generativity_outcome_results_new <- standardized_regression(lgs_m_t5 ~ gender + black_dc + income_t5 + education + hcscore, FLSA_merge)
print(generativity_outcome_results_new$model_summary)
print(generativity_outcome_results_new$std_coeffs)

# Wellbeing as outcome variable

wellbeing_outcome_results_new <- standardized_regression(pwb_pmean ~ gender + black_dc + income_t5 + education + hcscore, FLSA_merge)
print(wellbeing_outcome_results_new$model_summary)
print(wellbeing_outcome_results_new$std_coeffs)

# Political Engagement as outcome variable
pol_engage_results_new <- standardized_regression(t5.PolEngage.Comp ~ gender + black_dc + income_t5 + education + hcscore, FLSA_merge)
print(pol_engage_results_new$model_summary)
print(pol_engage_results_new$std_coeffs)

# Civic Engagement as outcome variable
civ_engage_results_new <- standardized_regression(t5.CivEngage.Comp ~ gender + black_dc + income_t5 + education + hcscore, FLSA_merge)
print(civ_engage_results_new$model_summary)
print(civ_engage_results_new$std_coeffs)

# Load necessary libraries
library(tidyverse)
library(psych)
library(sjPlot)

# Define variables of interest
variables_of_interest <- c("hcscore", "ext_m_t5", "open_m_t5", "neuro_m_t5","con_m_t5", "ag_m_t5", 
                           "t5.PolEngage.Comp", "t5.CivEngage.Comp", "lgs_m_t5", "pwb_pmean", 
                            "t6iwahamer", "t6iwahcomm", "t6iwahworld")

# Calculate correlations for FLSA_merge dataset
corr_results <- corr.test(select(FLSA_merge, all_of(variables_of_interest)), method = "pearson")

# Display correlations and confidence intervals
print(corr_results, short = FALSE)

# Create a dataframe for tab_corr function
corr_data <- FLSA_merge %>%
  select(all_of(variables_of_interest))

# Display the correlation table using sjPlot
tab_corr(corr_data, p.numeric = TRUE, triangle = "lower")

# For displaying specific correlations like the HC - Big Five
corr_hc_bigfive <- FLSA_merge %>% 
  select(hcscore, ext_m_t5, open_m_t5, neuro_m_t5, con_m_t5, ag_m_t5)

# Display the correlation table for HC and Big Five
tab_corr(corr_hc_bigfive, p.numeric = TRUE, triangle = "lower")

# Additional example: HC and identification with humanity
corr_hc_iwah <- FLSA_merge %>% 
  select(hcscore, t6iwahamer, t6iwahcomm, t6iwahworld)

# Display the correlation table for HC and Identification with Humanity
tab_corr(corr_hc_iwah, p.numeric = TRUE, triangle = "lower")


# Additional example: HC and LGS and PWB
corr_hc_gen <- FLSA_merge %>% 
  select(hcscore, lgs_m_t5, pwb_pmean)

# Display the correlation table for HC and Identification with Humanity
tab_corr(corr_hc_gen, p.numeric = TRUE, triangle = "lower")


# Additional example: HC and Polit and Civic Engagement
corr_hc_civ <- FLSA_merge %>% 
  select(hcscore,t5.PolEngage.Comp,t5.CivEngage.Comp)

# Display the correlation table for HC and Identification with Humanity
tab_corr(corr_hc_civ, p.numeric = TRUE, triangle = "lower")











# Load necessary libraries
library(tidyverse)
library(psych)
library(sjPlot)
library(gt)

# Define variables of interest
variables_of_interest <- c("hcscore", "ext_m_t5", "open_m_t5", "neuro_m_t5", "con_m_t5", "ag_m_t5", 
                           "t5.PolEngage.Comp", "t5.CivEngage.Comp", "lgs_m_t5", "pwb_pmean", 
                           "t6iwahamer", "t6iwahcomm", "t6iwahworld")

# Calculate correlations and confidence intervals manually
calculate_correlations_ci <- function(data, variables) {
  results <- data.frame(Variable1 = character(), Variable2 = character(), 
                        Correlation = numeric(), Lower_CI = numeric(), Upper_CI = numeric(), 
                        P_Value = numeric(), stringsAsFactors = FALSE)
  
  for (var1 in variables) {
    for (var2 in variables) {
      if (var1 != var2) {
        corr_test <- cor.test(data[[var1]], data[[var2]], method = "pearson")
        corr_value <- corr_test$estimate
        p_value <- corr_test$p.value
        ci <- corr_test$conf.int
        results <- rbind(results, data.frame(Variable1 = var1, Variable2 = var2, 
                                             Correlation = corr_value, Lower_CI = ci[1], Upper_CI = ci[2], 
                                             P_Value = p_value, stringsAsFactors = FALSE))
      }
    }
  }
  return(results)
}

# Calculate correlations for FLSA_merge dataset
correlations_ci <- calculate_correlations_ci(FLSA_merge, variables_of_interest)

# Function to format the combined dataframe for display in APA style
format_corr_table <- function(df) {
  df %>%
    mutate(
      Correlation = sprintf("%.2f", Correlation),
      CI = paste0("[", sprintf("%.2f", Lower_CI), ", ", sprintf("%.2f", Upper_CI), "]"),
      P_Value = ifelse(P_Value < .001, "< .001", sprintf("%.3f", P_Value))
    ) %>%
    select(Variable1, Variable2, Correlation, CI, P_Value)
}

# Format the combined dataframe for display
formatted_corr_table <- format_corr_table(correlations_ci)

# Function to create APA style table using gt
create_apa_table <- function(correlation_data) {
  correlation_data %>%
    gt() %>%
    tab_header(
      title = md("**Correlations and Confidence Intervals**"),
      subtitle = "Pearson Correlations with 95% Confidence Intervals and p-values"
    ) %>%
    cols_label(
      Variable1 = "Variable 1",
      Variable2 = "Variable 2",
      Correlation = "r",
      CI = "95% CI",
      P_Value = "p-value"
    ) %>%
    fmt_number(
      columns = vars(Correlation, P_Value),
      decimals = 2
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = vars(Correlation))
    )
}

# Display APA style table for combined correlations
create_apa_table(formatted_corr_table)


# Install necessary libraries
install.packages("readxl")
install.packages("irr")
library(readxl)
library(irr)

# Read the data from the Excel file
data <- read_excel("data/ICC.xlsx")

# Convert ratings to numeric, handling missing values
data$sebhc <- as.numeric(data$sebhc)
data$maryhc <- as.numeric(data$maryhc)

# Drop rows with NA values in either rating
cleaned_data <- na.omit(data[, c("sebhc", "maryhc")])

# Prepare the data for ICC calculation
ratings_matrix <- cleaned_data

# Compute ICC using a two-way random effects model
icc_result <- icc(ratings_matrix, model = "twoway", type = "agreement", unit = "single")

# Print the results
print(icc_result)


