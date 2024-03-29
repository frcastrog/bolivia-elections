#------------------------------Data Preparation--------------------------------#
#-Author: Francisca Castro ----------------------- Created: September 18, 2023-#
#-R Version: 4.3.1 ----------------------------------- Revised: March 26, 2024-#

# 1) Load packages

pacman::p_load(readxl,tidyverse,stargazer,broom,scales,modelsummary,fixest,
               ggh4x,plm,did,Hmisc,writexl,GGally,labelled,aod, ggeffects, MASS,
               gtsummary,magrittr,lfe,ggpubr,janitor,xtable,lme4)


options(scipen = 999)

# 2) Prepare databases

protests <- read_excel("01-data/protest-data.xlsx")
elections <- read_excel("01-data/electoral-data.xlsx")

# Merge databases
# First, create variables number_protest, number_protest_acled, diff_acled and protest_binary in a new database
data <- elections
data$post <- 0
data %<>% mutate(post = case_when(country == "bolivia" & year > 2019 ~ 1,
                                  country == "chile" & year > 2019 ~ 1,
                                  TRUE ~ 0))

# Then, include treatment information
data %<>% 
  left_join(protests)

data$treated <- 0
data %<>% mutate(treated = case_when(post == 1 & protest_binary == 1 ~ 1,
                                     TRUE ~ 0))


# Create a new variable that establishes treatment but only at the province level
data %<>%
  group_by(province) %>%
  mutate(treated_prov = sum(protest_binary)) %>%
  mutate(treated_prov = case_when(post == 1 & treated_prov >= 1 ~ 1,
                                  TRUE ~ 0))

# Filter for Bolivia after 2009 and Chile after 2012
data_filtered <- data %>%
  filter(year >= 2009 & year <= 2020) %>% #not include 2021 chile
  filter(!country %in% c("chile") | !year %in% c("2009"))

# Construct valid_votes_total so it doesn't count the rollout
data_filtered$valid_votes_total <- data_filtered$valid_votes/data_filtered$total_votes

#chile after 2021
chile_db <- data_filtered %>%
  filter(country == "chile")

# bolivia after 2009
bolivia_db <- data_filtered %>%
  filter(country == "bolivia")