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
# - First, create variables number_protest, number_protest_acled, 
# - diff_acled and protest_binary in a new database
data <- elections
data$post <- 0
data %<>% mutate(post = case_when(country == "bolivia" & year > 2019 ~ 1,
                                  country == "chile" & year > 2019 ~ 1,
                                  TRUE ~ 0))

# - Then, include treatment information
data %<>% 
  left_join(protests)

data$treated <- 0
data %<>% mutate(treated = case_when(post == 1 & protest_binary == 1 ~ 1,
                                     TRUE ~ 0))


# - Create a new variable that establishes treatment but only at the province level
data %<>%
  group_by(province) %>%
  mutate(treated_prov = sum(protest_binary)) %>%
  mutate(treated_prov = case_when(post == 1 & treated_prov >= 1 ~ 1,
                                  TRUE ~ 0))

# - Filter for Bolivia after 2009 and Chile after 2012
data_filtered <- data %>%
  filter(year >= 2009 & year <= 2020) %>% #not include 2021 chile
  filter(!country %in% c("chile") | !year %in% c("2009"))

# - Construct valid_votes_total so it doesn't count the rollout
data_filtered$valid_votes_total <- data_filtered$valid_votes/data_filtered$total_votes

# - chile after 2021
chile_db <- data_filtered %>%
  filter(country == "chile")

# - bolivia after 2009
bolivia_db <- data_filtered %>%
  filter(country == "bolivia")

# Distribution of type of protests in Bolivia

protest_type_bolivia <- read_excel("01-data/protest-data.xlsx", sheet = "bolivia_type")

# - Add departamento
bolivia_provinces <- bolivia_db %>%
  filter(year == 2020) %>%
  select(region, mun_code) 

protest_type_bolivia %<>%
  left_join(., bolivia_provinces, by = c("mun_code")) 

# - Double check that protest_binary is correct when considereng treatment+post

table(data_filtered$protest_binary)

table(data_filtered$protest_binary, data_filtered$post)

table(data_filtered$treated, data_filtered$post)

# 3) ELSOC database

load("01-data/ELSOC.RData")
elsoc <- elsoc_long_2016_2021
rm(elsoc_long_2016_2021)

## 3.1) Preparing database 

# Removing cases without municipalities 
elsoc$comuna_cod[elsoc$comuna_cod == "14501"] <- NA
elsoc$comuna_cod[elsoc$comuna_cod == "16403"] <- NA
elsoc$comuna_cod[elsoc$comuna_cod == "14605"] <- NA

elsoc %<>% drop_na(comuna_cod)

# Cleaning variables of interest
elsoc %<>% 
  mutate(c21_04 = replace(c21_04, c21_04 <= 0, NA), # esperanzado futuro movimiento
         c21_05 = replace(c21_05, c21_05 <= 0, NA), # acciones mov generan cambio
         c11 = replace(c11, c11 <= 0, NA),# votó en últimas elecciones 2013-17
         c43 = replace(c43, c43 <= 0, NA), # votó plebiscito
         c14_02 = replace(c14_02, c14_02 <= 0, NA), #informacion medios comunicacion
         c41_01 = replace(c41_01, c41_01 <= 0, NA), # rabia niveles desigualdad
         c20 = replace(c20, c20 <= 0, NA), #movimiento que más valora
         m01 = replace(m01, m01 <= 0, NA), # educational level
         m14 = replace(m14, m14 <= 0, NA),
         c21_11 = replace(c21_11, c21_11 <= 0, NA), #las políticas son injustas
         c05_01 = replace(c05_01, c05_01 <= 0, NA),#confianza gobierno 
         c15 = replace(c15, c15 < 0, NA), #izq derecha
         c18_04 = replace(c18_04, c18_04 <= 0, NA), #gob firme
         c01 = replace(c01, c01 <= 0, NA), #satisfaccion democracia
         m13 = ifelse(m13 <= 0 | m13 == 999999999, NA, m13)) # income


# Recode izq-derecha
elsoc$pospol <- recode(elsoc$c15,
                       "0" = 1,
                       "1" = 1,
                       "2" = 1,
                       "3" = 1,
                       "4" = 2,
                       "5" = 2,
                       "6" = 2,
                       "7" = 3,
                       "8" = 3,
                       "9" = 3,
                       "10" = 3,
                       "11" = 4,
                       "12" = 4)

elsoc$year <- elsoc$annio_entr

table(elsoc$pospol)

elsoc %<>%
  rename(mun_code = comuna_cod)

### Create treatment variables municipal level
# - Treatment status of all municipalities
treated_chile <- chile_db %>%
  filter(year == 2020) %>%
  dplyr::select(municipality, treated, mun_code) %>%
  rename(protest_binary = treated)

elsoc_merged <- left_join(elsoc, treated_chile, by = c("mun_code"))
table(elsoc_merged$protest_binary, elsoc_merged$year) 

# - Create variable treated = protest_binary post treatment
elsoc_merged$treated <- 0
elsoc_merged %<>% mutate(treated = case_when(year >= 2019 & protest_binary == 1 ~ 1,
                                             TRUE ~ 0))
table(elsoc_merged$treated, elsoc_merged$year) 

# -  Create variable post treatment
elsoc_merged$post <- 0
elsoc_merged %<>% mutate(post = case_when(year >= 2019 ~ 1, TRUE ~ 0))
table(elsoc_merged$post, elsoc_merged$year)

# 3.2) Creating variables

# Political efficacy - Individual mechanism 

# - Fist variable: political efficacy

elsoc_merged$esperanza_mov <- recode(elsoc_merged$c21_04,
                                     "1" = 1,
                                     "2" = 1,
                                     "3" = 2,
                                     "4" = 3,
                                     "5" = 3) #only 3 categories
table(elsoc_merged$annio_entr, elsoc_merged$esperanza_mov)

# Second variable: retrospective vote

# - c11: Participacion electoral retrospectiva\
# - Respecto de su participación en elecciones: ¿Votó usted en las últimas 
# - elecciones presidenciales de [Noviembre de 2013/Noviembre de 2017]?.
table(elsoc_merged$year, elsoc_merged$c11) #just 2016 y 2018

# - c43: Participacion electoral retrospectiva Plebiscito Nueva Constitucion
table(elsoc_merged$year, elsoc_merged$c43)

# Create variable merging the two retrospective votes

elsoc_merged <- elsoc_merged %>%
  mutate(participacion = rowSums(select(., c11, c43), na.rm = TRUE))


elsoc_merged$participacion[elsoc_merged$participacion == 0] <- NA
table(elsoc_merged$annio_entr, elsoc_merged$participacion) # 1 yes 2 no 3 i was not old enough

# - work only with 1 and 2
elsoc_merged$participacion[elsoc_merged$participacion == 3] <- NA
elsoc_merged$participacion[elsoc_merged$participacion == 1] <- 1 # yes
elsoc_merged$participacion[elsoc_merged$participacion == 2] <- 0 # no
table(elsoc_merged$year, elsoc_merged$participacion)

# - Third variable: my vote influences the results

table(elsoc_merged$year, elsoc_merged$c10_02)

elsoc_merged %<>% 
  mutate(c10_02 = replace(c10_02, c10_02 <= 0, NA))


## Save 

save(bolivia_db, file = "01-data/bolivia_db.RData")
