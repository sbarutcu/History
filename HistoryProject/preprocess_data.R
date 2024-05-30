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
library("corrplot")
library(sjPlot)
library(patchwork)
library(sjmisc)
library(sjlabelled)
library(lm.beta)
require(psych)


# Data processing

## Load and prepare data
#| label: load-data
mydata <- read.csv('data/big_hc_clean.csv', na.strings = "NA") %>% 
  arrange(ID, year)

narscore <- read.csv('data/score_hc_clean.csv', na.strings = "NA") %>% 
  select(., -c(X:X.3))

#| label: convert hcscores from character to number
narscore <- narscore %>% 
  mutate(hcscore = as.numeric(as.character(hcscore)))

## Join datasets
#| label: join-data
all_edt <- narscore %>%
  full_join(mydata, by = c("ID")) # join with self-report data

# Check IDs
tabyl(all_edt$ID)

## Save the long dataset with narrative variables
#| label: save-new-data
write_csv(all_edt, "data/long_hc_edt.csv")

## Convert long to wide format
all_edt %>% 
  mutate(year = paste("t", year, sep = "")) -> all_edt

pivot_wider(all_edt,
            id_cols = ID,
            names_from = year,
            values_from = c(lgs_m, lgs._t, income, ag_m:open_m)) -> widetimevarying

# Keep only columns with less than nrow(widetimevarying) NA values
widetimevarying <- widetimevarying[, colSums(is.na(widetimevarying)) < nrow(widetimevarying)]

# Select and filter specific year data
all_edt %>%
  select(., ID, year, hcscore, psm_m, gender:open_z) %>%
  filter(year == "t1") %>%
  select(-year) -> wideremaining

all_edt %>%
  select(., ID, year, iwah_a:iwah_m) %>%
  filter(year == 't6') %>%
  select(-year) -> wideiwah

# View intermediate datasets (Commented out for script-running purposes)
# View(wideremaining)
# View(wideiwah)

## Merge the processed datasets
mergethem(wideremaining, wideiwah, widetimevarying) -> all_edt_wide

# View the merged wide dataset (Commented out for script-running purposes)
# View(all_edt_wide)

## Save the final wide dataset
write.csv(all_edt_wide, 'data/wide_hc_edt.csv')

