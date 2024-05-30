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
require(psych)

## Read data
all_edt_wide <- read.csv('data/wide_hc_edt.csv')

# Demographics ====

## Tabulate demographics
tabyl(all_edt_wide$gender)
tabyl(all_edt_wide$education)
tabyl(all_edt_wide$race)

## Race and gender differences
race_data2 <- all_edt_wide %>% 
  filter(race < 2) 

tabyl(race_data2$gender)

describeBy(select(all_edt_wide, gender, hcscore), group=all_edt_wide$gender)
t.test(hcscore~gender, data=all_edt_wide)

describeBy(select(race_data2, race, gender, hcscore), 
           group=race_data2$race)

black_scores <- all_edt_wide$hcscore[all_edt_wide$race == "2"]
white_scores <- all_edt_wide$hcscore[all_edt_wide$race == "1"]

rtest <- t.test(hcscore~race, data=filter(all_edt_wide, race %in% c(1, 2)))
print(rtest)

t_test_result <- t.test(black_scores, white_scores, var.equal = FALSE)
print(t_test_result)


library(ggplot2)
library(dplyr)
library(ggplot2)
library(dplyr)

# Create summary dataset
summary_data <- all_edt_wide %>%
  filter(race %in% c(1, 2)) %>%
  group_by(race) %>%
  summarize(
    Mean = mean(hcscore, na.rm = TRUE),
    SD = sd(hcscore, na.rm = TRUE),
    SE = SD / sqrt(n()),
    n = n()
  ) %>%
  mutate(race = ifelse(race == 1, "White", "Black")) # Convert numeric race to factor with labels
print(summary_data)
# Find the position for the significance line
y_max <- max(summary_data$Mean + summary_data$SE)
y_pos_signif <- y_max + 0.1 * y_max # Adjust this factor to ensure the line is above the error bars

# Plot
p <- ggplot(summary_data, aes(x = race, y = Mean)) +
  geom_bar(stat = "identity", position = position_dodge(), aes(fill = race)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2, position = position_dodge(.9)) +
  geom_segment(aes(x = 0.75, xend = 1.25, y = y_pos_signif, yend = y_pos_signif), color = "black") + # Horizontal line for White
  geom_segment(aes(x = 1.75, xend = 2.25, y = y_pos_signif, yend = y_pos_signif), color = "black") + # Horizontal line for Black
  geom_segment(aes(x = 1.25, xend = 1.75, y = y_pos_signif, yend = y_pos_signif), color = "black") + # Significance line between groups
  geom_text(aes(x = 1.5, y = y_pos_signif), label = "*", vjust = 0, color = "black", size = 6) + # Asterisk for significance
  scale_fill_manual(values = c("White" = "skyblue", "Black" = "gold")) +
  labs(x = "Race", y = "Historical Consciousness Score") +
  theme_minimal() +
  theme(legend.position = "none") # Hide the legend

# Display the plot
print(p)

# Save the plot to a file
ggsave("hcscore_race_difference.png", plot = p, width = 8, height = 6, dpi = 300)





# Descriptives ====

## Description of variables ----
# hcscore - historical consciousness (narrative score)
# neur:open - the big five (mean scores)
# iwah_a - identification with all of humanity scale (american identification)
# iwah_c - identification with one's community
# iwah_w - identification with all humanity/ world
# lgs_m_t5 - generativity measured at Time 5
# income_t5 - income at Time 5 
# psm_m - public service motivation, mean score (only measured at Time 1 and 6)
# pwb_pmean - psychological wellbeing (mean scores)

describe(select(all_edt_wide, hcscore,neur:open, iwah_a:iwah_w, lgs_m_t5, income_t5, psm_m,pwb_pmean))


library(ggplot2)

descr(all_edt_wide$hcscore)

# Summarize data
summary_stats <- all_edt_wide %>%
  summarise(Mean = mean(hcscore, na.rm = TRUE),
            Median = median(hcscore, na.rm = TRUE),
            SD = sd(hcscore, na.rm = TRUE),
            IQR = IQR(hcscore, na.rm = TRUE))

print(summary_stats)

# Create a combined plot with density and boxplot
ggplot(all_edt_wide, aes(x = hcscore)) +
  geom_density(fill = "skyblue", alpha = 0.5) +  # Density plot
  geom_boxplot(width = 0.2, fill = "lightgray", position = position_nudge(x = 0.2)) + # Boxplot slightly offset
  labs(
       x = "Historical Consciousness Score",
       y = "Density") +
  theme_minimal()



## Shapiro-Wilk normality test for hcscore ----
shapiro.test(all_edt_wide$hcscore)

## All correlations ----
corr.test(select(all_edt_wide, hcscore,neur:open, iwah_a:iwah_w, lgs_m_t5, income_t5, psm_m,pwb_pmean))
corr.test(select(all_edt_wide, hcscore, income_t5, education))
print(corr.test(select(all_edt_wide, hcscore, neur:open)), short = FALSE)

### Table: HC - Big five correlations  ----
corr5 <- all_edt_wide %>% 
  dplyr::select(hcscore, extra, neur, consc, agree, open) 

tab_corr(corr5, p.numeric = TRUE, triangle = "lower")

#fdr correction?
#raffi <- corr.test(select(all_edt_wide, hcscore, extra, neur, consc, agree, open), adjust = "fdr")
#round(raffi$r, 2)
#round(raffi$p, 3)


# Regression Analyses ====
## Demographics ----
all_edt_wide %>%
  mutate(black_dc = case_when(race == 1 ~ 0, race == 2 ~ 1, TRUE ~ NA_real_)) -> all_edt_wide

dmodel <- lm(hcscore ~ gender + black_dc + income_t5 + education, data = all_edt_wide)
summary(dmodel)
confint(dmodel)
tab_model(dmodel, auto.label = TRUE, show.se = TRUE, string.est = "B", string.ci = "95% CI", string.se = "SE")

## Big Five ----
### Extra + Demo ----
bfin1 <- lm(hcscore ~ black_dc + income_t5 + education + polit+ gender + extra, data = all_edt_wide)
summary(bfin1)
confint(bfin1)
tab_model(bfin1, auto.label = TRUE, show.se = TRUE, string.est = "B", string.ci = "95% CI", string.se = "SE")

#tidy(lm.beta(bfin1), conf.int = T)

### Open + Demo ----
bfin2 <- lm(hcscore ~ black_dc + income_t5 + education + gender + open, data = all_edt_wide)
summary(bfin2)
confint(bfin2)
tab_model(bfin2, auto.label = TRUE, show.se = TRUE, string.est = "B", string.ci = "95% CI", string.se = "SE")
lm.beta(bfin2)

### Neur + Demo ----
neur <- lm(hcscore ~ black_dc + income_t5 + education + gender + neur, data = all_edt_wide)
summary(neur)
confint(neur)
tab_model(neur, auto.label = TRUE, show.se = TRUE, string.est = "B", string.ci = "95% CI", string.se = "SE")

# NEW

# Calculate standardized coefficients
std_coeffs <- lm.beta(agree)

# Extract standard errors and compute for standardized
se <- summary(agree)$coefficients[-1, "Std. Error"]  # Assuming intercept is first
unstd_coeffs <- coef(agree)[-1]  # Excluding intercept

# Standardized errors calculation
std_errors <- se * abs(std_coeffs / unstd_coeffs)

# 95% Confidence intervals using normal distribution
z_value <- qnorm(0.975)
lower_ci <- std_coeffs - z_value * std_errors
upper_ci <- std_coeffs + z_value * std_errors
se
# Create a dataframe for output
results <- data.frame(
  Variable = names(std_coeffs),
  Std_Coefficient = std_coeffs,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci
)

# Display the results
print(results)


### Consc + Demo ----
consc <- lm(hcscore ~ black_dc + income_t5 + education + gender + consc, data = all_edt_wide)
summary(consc)
confint(consc)
tab_model(consc, auto.label = TRUE, show.se = TRUE, string.est = "B", string.ci = "95% CI", string.se = "SE")




### Agree + Demo ----
agree <- lm(hcscore ~ black_dc + income_t5 + education + gender + agree, data = all_edt_wide)
summary(agree)
confint(agree)
tab_model(agree, auto.label = TRUE, show.se = TRUE, string.est = "B", string.ci = "95% CI", string.se = "SE")

### BFI + Demo ----
bfm2 <- lm(hcscore ~ black_dc + income_t5 + extra + open + neur + consc + agree + education + gender, data = all_edt_wide)
summary(bfm2)


## Humanitarianism ----

print(corr.test(select(all_edt_wide, hcscore, iwah_a, iwah_c, iwah_w)), short = FALSE)
corrhum <- all_edt_wide %>% 
  dplyr::select(hcscore, iwah_a, iwah_c, iwah_w)
tab_corr(corrhum, p.numeric = TRUE, triangle = "lower")

summary(amr <- lm(hcscore ~ gender + black_dc + income_t5 + education + iwah_a, data = all_edt_wide))
summary(cm <- lm(hcscore ~ gender + black_dc + income_t5 + education + iwah_c, data = all_edt_wide))
summary(w <- lm(hcscore ~ gender + black_dc + income_t5 + education + iwah_w, data = all_edt_wide))
tab_model(amr, cm, w, auto.label = TRUE, show.se = TRUE, string.est = "B", string.ci = "95% CI", string.se = "SE")


# NEW

# Fit the linear model
amr <- lm(hcscore ~ gender + black_dc + income_t5 + education + iwah_a, data = all_edt_wide)
# Calculate standardized coefficients
std_coeffs <- lm.beta(amr)

# Extract standard errors and compute for standardized
se <- summary(amr)$coefficients[-1, "Std. Error"]  # Assuming intercept is first
unstd_coeffs <- coef(amr)[-1]  # Excluding intercept

# Standardized errors calculation
std_errors <- se * abs(std_coeffs / unstd_coeffs)

# 95% Confidence intervals using normal distribution
z_value <- qnorm(0.975)
lower_ci <- std_coeffs - z_value * std_errors
upper_ci <- std_coeffs + z_value * std_errors
se
# Create a dataframe for output
results <- data.frame(
  Variable = names(std_coeffs),
  Std_Coefficient = std_coeffs,
  Lower_CI = lower_ci,
  Upper_CI = upper_cise
)

# Display the results
print(results)



## Generativity ----
summary(lm(lgs_m_t5 ~ hcscore + gender + black_dc + income_t5 + education, data = all_edt_wide))
confint(lm(lgs_m_t5 ~ hcscore + gender + black_dc + income_t5 + education, data = all_edt_wide))
generativity <- lm(hcscore ~ gender + black_dc + income_t5 + education + lgs_m_t5, data = all_edt_wide)
tab_model(generativity, auto.label = TRUE, show.se = TRUE, string.est = "B", string.ci = "95% CI", string.se = "SE")

# NEW

gen <- lm(lgs_m_t5 ~ hcscore + gender + black_dc + income_t5 + education, data = all_edt_wide)
summary(gen)

genX <- lm(lgs_m_t5 ~ hcscore + gender + black_dc + income_t5 + education+ t5sie07x , data = FLSA_merge)
summary(genX)

# Calculate standardized coefficients
std_coeffs <- lm.beta(pwbmod)

# Extract standard errors and compute for standardized
se <- summary(pwbmod)$coefficients[-1, "Std. Error"]  # Assuming intercept is first
unstd_coeffs <- coef(pwbmod)[-1]  # Excluding intercept

# Standardized errors calculation
std_errors <- se * abs(std_coeffs / unstd_coeffs)

# 95% Confidence intervals using normal distribution
z_value <- qnorm(0.975)
lower_ci <- std_coeffs - z_value * std_errors
upper_ci <- std_coeffs + z_value * std_errors
se
# Create a dataframe for output
results <- data.frame(
  Variable = names(std_coeffs),
  Std_Coefficient = std_coeffs,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci
)

# Display the results
print(results)


## PSM ----
psmmod <- lm(psm_m ~ gender + black_dc + income_t5 + education + hcscore, data = all_edt_wide)
summary(psmmod)
confint(psmmod)

# psm, generativity and hcscore
summary(lm(psm_m ~ lgs_m_t5 + pwb_pmean + hcscore + gender + black_dc + income_t5 + education, data = all_edt_wide))


## Wellbeing ----
pwbmod <- lm(pwb_pmean ~ gender + black_dc + income_t5 + education + hcscore, data = all_edt_wide)
summary(pwbmod)
confint(pwbmod)

View(all_edt_wide)



# Read in .CSV with scored variables ----

FLSA_merge <- read_csv("RMD scripts and files from Cavan/FLSA_mergeScoredVariables.csv")

View(FLSA_merge)

# Intercorrelations of Behavior Variables, Attitudes, Generativity, Etc

BehaviorCorMat <- FLSA_merge %>% select(hcscore,
                                        t5sie07x,
                                        t5.PolEngage.Comp,
                                        t5.CivEngage.Comp,
                                        t5lgs_m,
                                        tcomp.sdo.composite,
                                        t2and8_rwa_composite,
                                        iwah_a, iwah_c, iwah_w)

StudyDescriptives <- psych::describe(BehaviorCorMat)
StudyDescriptives

# write_csv(StudyDescriptives, "~/Documents/GitHub/FLSA_Ideology/StudyDescriptives.csv")

BehaviorCorMatCorr <- cor(BehaviorCorMat, use="pairwise.complete.obs")
BehaviorCorMatCorr

library(apaTables)
# apa.cor.table(BehaviorCorMat,              
# filename = "~/Documents/GitHub/FLSA_Ideology/FLSA.HC.StudyVarCorrs.doc",              
# table.number = 1,              
# show.conf.interval = TRUE,              
# show.sig.stars = TRUE,              
# landscape = TRUE)


# Corr Matrix for the  Variables

t1CorMat <- FLSA_merge %>% select(hcscore, 
                                  iwah_a, iwah_c,iwah_w,
                                  t5sie07x,
                                  lgs_m_t5,
                                  pwb_pmean,
                                  t5.PolEngage.Comp,
                                  t5.CivEngage.Comp)
t1CorMatCorr <- cor(t1CorMat, use="pairwise.complete.obs")
t1CorMatCorr
apa.cor.table(t1CorMat,              
filename = "FLSA.HC.ideology.corrs.doc",              
table.number = 1,              
show.conf.interval = TRUE,              
show.sig.stars = TRUE,              
landscape = TRUE)

print(corr.test(select(FLSA_merge, hcscore,t5.PolEngage.Comp)), short = FALSE)




## ideology ----

# Assuming FLSA_merge needs to keep the mutated column for later use
FLSA_merge <- FLSA_merge %>%
  mutate(black_dc = ifelse(race == 1, 0, ifelse(race == 2, 1, NA)))

# Function to fit a model, display the summary, confidence intervals, and tabular model
fit_and_summarize <- function(formula, data) {
  model <- lm(formula, data = data)
  print(summary(model))
  print(confint(model))
  tab_model(model, auto.label = TRUE, show.se = TRUE, string.est = "B", string.ci = "95% CI", string.se = "SE")
}

# Apply the function for each model
fit_and_summarize(hcscore ~ black_dc + income_t5 + education + gender + t5sie07x, FLSA_merge)
fit_and_summarize(t5.PolEngage.Comp ~ black_dc + income_t5 + education + gender + hcscore, FLSA_merge)
fit_and_summarize(t5.CivEngage.Comp ~ black_dc + income_t5 + education + gender + hcscore, FLSA_merge)

fit_and_summarize(t5.PolEngage.Comp ~ hcscore + t5sie07x, FLSA_merge)
fit_and_summarize(t5.CivEngage.Comp ~ hcscore+ t5sie07x, FLSA_merge)

deneme1 <- lm(t5.PolEngage.Comp ~ black_dc + income_t5 + education + gender + hcscore, FLSA_merge)
summary(deneme1)

deneme2 <- lm(t5.CivEngage.Comp ~ black_dc + income_t5 + education + gender + hcscore, FLSA_merge)

summary(deneme2)

lm.beta(deneme1)


# Calculate standardized coefficients
std_coeffs <- lm.beta(deneme2)

# Extract standard errors and compute for standardized
se <- summary(deneme2)$coefficients[-1, "Std. Error"]  # Assuming intercept is first
unstd_coeffs <- coef(deneme2)[-1]  # Excluding intercept

# Standardized errors calculation
std_errors <- se * abs(std_coeffs / unstd_coeffs)

# 95% Confidence intervals using normal distribution
z_value <- qnorm(0.975)
lower_ci <- std_coeffs - z_value * std_errors
upper_ci <- std_coeffs + z_value * std_errors
se
# Create a dataframe for output
results <- data.frame(
  Variable = names(std_coeffs),
  Std_Coefficient = std_coeffs,
  Lower_CI = lower_ci,
  Upper_CI = upper_ci
)

# Display the results
print(results)



library(broom)
tidy(lm.beta(deneme1), conf.int=T)



# Mediation tests ----

library(mediation)


## iwah_w--> liberalism --> hc ----
# Define the variables of interest for the first test
vars_test_1 <- c("t5sie07x", "iwah_w", "hcscore")

# Fit the mediator model: t5sie07x predicted by iwah_w, only using complete cases for this specific test
mediator.model.1 <- lm(t5sie07x ~ iwah_w, data = FLSA_merge, subset = complete.cases(FLSA_merge[vars_test_1]))

# Fit the outcome model: historicalConsciousness predicted by both iwah_w and t5sie07x
outcome.model.1 <- lm(hcscore ~ iwah_w + t5sie07x, data = FLSA_merge, subset = complete.cases(FLSA_merge[vars_test_1]))

# Conduct the mediation analysis
med.analysis.1 <- mediate(mediator.model.1, outcome.model.1, treat = "iwah_w", mediator = "t5sie07x")
summary(med.analysis.1)

## hc --> generativity --> pwb ----
# Define the variables of interest for the second test

vars_test_2 <- c("lgs_m_t5", "pwb_pmean", "hcscore")

# Fit the mediator model: lgs predicted by hcscore, only using complete cases for this specific test
mediator.model.2 <- lm(lgs_m_t5 ~ hcscore, data = FLSA_merge, subset = complete.cases(FLSA_merge[vars_test_2]))

# Fit the outcome model: pwb predicted by both hc and lgs
outcome.model.2 <- lm(pwb_pmean ~ hcscore + lgs_m_t5, data = FLSA_merge, subset = complete.cases(FLSA_merge[vars_test_2]))

# Conduct the mediation analysis
med.analysis.2 <- mediate(mediator.model.2, outcome.model.2, treat = "hcscore", mediator = "lgs_m_t5")
summary(med.analysis.2)
#######
# Fit the mediator model: hcscore predicted by lgs, only using complete cases for this specific test
mediator.model.3 <- lm(hcscore ~ lgs_m_t5, data = FLSA_merge, subset = complete.cases(FLSA_merge[vars_test_2]))

# Fit the outcome model: pwb predicted by both hc and lgs
outcome.model.3 <- lm(pwb_pmean ~ hcscore + lgs_m_t5, data = FLSA_merge, subset = complete.cases(FLSA_merge[vars_test_2]))

# Conduct the mediation analysis
med.analysis.3 <- mediate(mediator.model.3, outcome.model.3, treat = "lgs_m_t5", mediator = "hcscore")
summary(med.analysis.3)


