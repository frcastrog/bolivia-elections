#-----------------------------Replication Script-------------------------------#
#-Author: Francisca Castro ----------------------- Created: September 18, 2023-#
#-R Version: 4.3.1 ----------------------------------- Revised: March 26, 2024-#

# 0) Load packages

pacman::p_load(readxl,tidyverse,stargazer,broom,scales,modelsummary,fixest,
               ggh4x,plm,did,Hmisc,writexl,GGally,labelled,aod, ggeffects, MASS,
               gtsummary,magrittr,lfe,ggpubr,janitor,xtable,lme4)

options(scipen = 999)

# 1) Summary statistics

table_summary <- data_filtered[, c("country", "population", "treated",
                                   "protest_binary",
                                   "electoral_roll_municipality",
                                   "valid_votes_total", "perc_participation",
                                   "incumbent_vote_share")]
table_summary %<>% drop_na()

# - Information to generate Table 2
table_summary %>% 
  filter(country == "bolivia") %>%
  tbl_summary(by = protest_binary, statistic = list(all_continuous() ~
                                                      "{mean} ({sd}) ; {min} - {max}"))

table_summary_2 <- data_filtered[, c("country", "number_protest",
                                     "number_protest_acled")]

# - Remove NAs pre-treatment for table_summary_2
table_summary_2 %<>% drop_na()

table_summary_2 %>% tbl_summary(by = country,
                                statistic = list(all_continuous() ~ 
                                                   "{mean} ({sd}) ; {min} - {max}"))

# - But maybe is better to just have the number of treated municipalities so...
table(data_filtered$post, data_filtered$country, data_filtered$treated)

# Distribution protests Bolivia

protest_type_by_mun <- protest_type_bolivia %>%
  filter(Duplicates == 0) %>%
  dplyr::count(mun_code, Category) %>%
  tidyr::spread(key = mun_code, value = n) %>%
  tidyr::gather(key = mun_code, value = "n", 
                -Category, na.rm = TRUE) %>%
  dplyr::select(mun_code, Category, n)

protest_type_by_region <- protest_type_bolivia %>%
  filter(Duplicates == 0) %>%
  dplyr::count(region, Category) %>%
  tidyr::spread(key = region, value = n) %>%
  tidyr::gather(key = region, value = "n", 
                -Category, na.rm = TRUE) %>%
  dplyr::select(region, Category, n)

protest_type_by_region_long <- protest_type_by_region %>%
  spread(Category, n) %>% 
  replace(is.na(.), 0) %>%
  adorn_totals(name = 'Total') %>%
  adorn_totals(name = 'Total', where = 'col')

print(xtable(protest_type_by_region_long, type = "html"))

# 2) Analysis

## 2.1) Voter turnout

table(chile_db$treated, chile_db$year)

table(bolivia_db$treated, bolivia_db$year)

# Just DID with OLS
turnout_1 <- lm(perc_participation ~ treated, data = chile_db)

# + FE years
turnout_2 <- feols(perc_participation ~ treated |
                     year, data = chile_db,
                   panel.id = c("mun_code", "year"))

# + FE municipality
turnout_3 <- feols(perc_participation ~ treated |
                     mun_code + year, data = chile_db, 
                   panel.id = c("mun_code", "year"))

# + Controls
turnout_4 <- feols(perc_participation ~ treated +
                     log(population) | mun_code + year, data = chile_db,
                   panel.id = c("mun_code", "year"))

# Table 1 appendix
modelsummary(list(turnout_1, turnout_2, turnout_3, turnout_4), 
             stars = c('*' = .1, '**' = .05, '***' = .01),
             output = "markdown")

## Plots
# - Figure 2
# - Reduced Model

turnout_simple_tidy <- tidy(turnout_3)

fit_cis_95 <- confint(turnout_3, level = 0.95) %>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..", "conf.high_95" = "X97.5..")

fit_cis_90 <- confint(turnout_3, level = 0.90) %>% 
  data.frame() %>%
  rename("conf.low_90" = "X5..", "conf.high_90" = "X95..")

turnout_simple_tidy <- bind_cols(turnout_simple_tidy, fit_cis_95, fit_cis_90) %>%
  rename(Variable = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Variable != "(Intercept)")

turnout_simple_tidy$Variable[turnout_simple_tidy$Variable == 
                               "treated"] <- "ATT (Reduced Model)"
turnout_simple_tidy$model <- "Reduced Model"

# - Extended Model
turnout_extended_tidy <- tidy(turnout_4)

fit_cis_95 <- confint(turnout_4, level = 0.95) %>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..", "conf.high_95" = "X97.5..")

fit_cis_90 <- confint(turnout_4, level = 0.90) %>% 
  data.frame() %>%
  rename("conf.low_90" = "X5..", "conf.high_90" = "X95..")

turnout_extended_tidy <- bind_cols(turnout_extended_tidy, fit_cis_95, fit_cis_90) %>%
  rename(Variable = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Variable != "(Intercept)")

turnout_extended_tidy$Variable[turnout_extended_tidy$Variable == 
                                 "treated"] <- "ATT (Extended Model)"
turnout_extended_tidy$model <- "Extended Model"

# - Join both models
df_turnout <- rbind(turnout_simple_tidy, turnout_extended_tidy)

df_turnout_coef_only <- df_turnout %>%
  filter(Variable == "ATT (Reduced Model)" | Variable == "ATT (Extended Model)")

# - Plot
plot_turnout <- ggplot(df_turnout_coef_only, aes(x = Variable, y = Coefficient)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = Variable, 
                 y = Coefficient)) + 
  geom_linerange(aes(x = Variable, 
                     ymin = conf.low_90,
                     ymax = conf.high_90),
                 lwd = 1) +
  geom_linerange(aes(x = Variable, 
                     ymin = conf.low_95,
                     ymax = conf.high_95),
                 lwd = 1/2) + 
  theme_bw() + 
  coord_flip() +
  theme(axis.title.y=element_blank()) +
  theme(aspect.ratio = 3/4) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))


plot_turnout

ggsave("03-outputs/plot_turnout.png", 
       width = 11, height = 7, units = "cm", dpi = 600)

## 2.2) Incumbent vote

table(bolivia_db$treated, bolivia_db$year)

# Just DID with OLS
incumbent_1 <- lm(incumbent_vote_share ~ treated, data = bolivia_db)

# + FE years
incumbent_2 <- feols(incumbent_vote_share ~ treated | year, data = bolivia_db,
                     panel.id = c("mun_code", "year"))

# + FE municipality
incumbent_3 <- feols(incumbent_vote_share ~ treated | 
                       mun_code + year, data = bolivia_db,
                     panel.id = c("mun_code", "year"))

# + Controls
incumbent_4 <- feols(incumbent_vote_share ~ treated + log(population) | 
                       mun_code + year, data = bolivia_db,
                     panel.id = c("mun_code", "year"))


# Table B2 appendix
msummary(list(incumbent_1, incumbent_2, incumbent_3, incumbent_4), 
         stars = c('*' = .1, '**' = .05, '***' = .01),
         output = "markdown") 

## Plot

# - Figure 3
# - Simple Model
incumbent_simple_tidy <- tidy(incumbent_3)

fit_cis_95 <- confint(incumbent_3, level = 0.95) %>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..", "conf.high_95" = "X97.5..")

fit_cis_90 <- confint(incumbent_3, level = 0.90) %>% 
  data.frame() %>%
  rename("conf.low_90" = "X5..", "conf.high_90" = "X95..")

incumbent_simple_tidy <- bind_cols(incumbent_simple_tidy, fit_cis_95, fit_cis_90) %>%
  rename(Variable = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Variable != "(Intercept)")

incumbent_simple_tidy$Variable[incumbent_simple_tidy$Variable == 
                                 "treated"] <- "ATT (Reduced Model)"
incumbent_simple_tidy$model <- "Reduced Model"

# - Extended Model
incumbent_extended_tidy <- tidy(incumbent_4)

fit_cis_95 <- confint(incumbent_4, level = 0.95) %>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..", "conf.high_95" = "X97.5..")

fit_cis_90 <- confint(incumbent_4, level = 0.90) %>% 
  data.frame() %>%
  rename("conf.low_90" = "X5..", "conf.high_90" = "X95..")

incumbent_extended_tidy <- bind_cols(incumbent_extended_tidy, fit_cis_95, fit_cis_90) %>%
  rename(Variable = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Variable != "(Intercept)")

incumbent_extended_tidy$Variable[incumbent_extended_tidy$Variable == 
                                   "treated"] <- "ATT (Extended Model)"
incumbent_extended_tidy$model <- "Extended Model"

# - Join both models
df_incumbent <- rbind(incumbent_simple_tidy, incumbent_extended_tidy)

df_incumbent_coef_only <- df_incumbent %>%
  filter(Variable == "ATT (Reduced Model)" | Variable == "ATT (Extended Model)")

# - Plot
plot_incumbent <- ggplot(df_incumbent_coef_only, aes(x = Variable, y = Coefficient)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = Variable, 
                 y = Coefficient)) + 
  geom_linerange(aes(x = Variable, 
                     ymin = conf.low_90,
                     ymax = conf.high_90),
                 lwd = 1) +
  geom_linerange(aes(x = Variable, 
                     ymin = conf.low_95,
                     ymax = conf.high_95),
                 lwd = 1/2) + 
  theme_bw() + 
  coord_flip() +
  theme(axis.title.y=element_blank()) +
  theme(aspect.ratio = 3/4) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))


plot_incumbent

ggsave("03-outputs/plot_incumbent.png", 
       width = 11, height = 7, units = "cm", dpi = 600)

# 2.3) Individual Mechanism ELSOC data

# 2.3.1) Descriptive statistics

# - Relevant Variables:
# - m0_sexo - Gender ( 1 male 2 female) 

# - m0_edad - Age 

# - m01 - Educational level (1 No studies 2 Incomplete primary education 
# - 3 Complete primary education 4 Incomplete secondary education 
# - 5 Complete secondary education 6 Incomplete technical education 
# - 7 Complete technical education 8 Incomplete university education 
# - 9 Complete university education 10 Postgraduate studies (master or doctorate)) 

# - pospol - Left-right self-placement scale (1 to 3, 4 for independents/don't know)

# - m13 - income 

# - c43 - retrospective vote Plebiscite (1 Yes 2No 3 I was not old enough)

summary_stats <- elsoc_merged %>%
  filter(year >= 2019) %>%
  group_by(treated, year) %>%
  summarise(
    # Sample size
    N = n(),
    # Categorical variables
    Count_Sexo = sum(!is.na(m0_sexo)),
    Percent_Male = sum(m0_sexo == 1, na.rm = TRUE) / Count_Sexo * 100,
    Percent_Female = sum(m0_sexo == 2, na.rm = TRUE) / Count_Sexo * 100,
    Count_Education = sum(!is.na(m01)),
    Percent_No_Education = sum(m01 == 1, na.rm = TRUE) / Count_Education * 100,
    Percent_Secondary_Or_Less = sum(m01 %in% 2:5, na.rm = TRUE) / Count_Education * 100,
    Percent_Universitary_Or_More = sum(m01 %in% 6:10, na.rm = TRUE) / Count_Education * 100,
    Count_Pospol = sum(!is.na(pospol)),
    Percent_Left = sum(pospol == 1, na.rm = TRUE) / Count_Pospol * 100,
    Percent_Center = sum(pospol == 2, na.rm = TRUE) / Count_Pospol * 100,
    Percent_Right = sum(pospol == 3, na.rm = TRUE) / Count_Pospol * 100,
    Percent_Independent_DontKnow = sum(pospol == 4, na.rm = TRUE) / Count_Pospol * 100,
    Count_Retrospective_Vote = sum(!is.na(c43)),
    Percent_Vote_Yes = sum(c43 == 1, na.rm = TRUE) / Count_Retrospective_Vote * 100,
    Percent_Vote_No = sum(c43 == 2, na.rm = TRUE) / Count_Retrospective_Vote * 100,
    Percent_Not_Old_Enough = sum(c43 == 3, na.rm = TRUE) / Count_Retrospective_Vote * 100,
    Mean_Age = mean(m0_edad, na.rm = TRUE),
    SD_Age = sd(m0_edad, na.rm = TRUE),
    Mean_Income = mean(m13, na.rm = TRUE),
    SD_Income = sd(m13, na.rm = TRUE)
  ) %>%
  ungroup()  # Ensure the resulting dataframe is ungrouped

long_summary_stats <- summary_stats %>%
  pivot_longer(
    cols = -c(treated, year, N), # Exclude the grouping and count columns
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Value = round(Value, 2), # Round the value to two decimal places
    Year_Treatment = paste(year, ifelse(treated == 1, "Treated", "Untreated"), sep = " "))

long_summary_stats <- long_summary_stats %>%
  select(-treated, -year, -N)

# Reshape the data to wide format
wide_summary_stats <- long_summary_stats %>%
  pivot_wider(
    names_from = Year_Treatment,
    values_from = Value)

summary_elsoc <- xtable(wide_summary_stats, booktabs = TRUE)
print(summary_elsoc, type = "latex", include.rownames = FALSE, floating = FALSE)

mean_income_2019 <- elsoc_merged %>%
  filter(year == 2019)

### Analyze which differences are statistically significant
summary(model1 <- lm(m0_sexo ~ treated, data = elsoc_merged[elsoc_merged$year >= 2019, ])) # not sig

summary(model1 <- lm(m01 ~ treated, data = elsoc_merged[elsoc_merged$year >= 2019, ])) # education ***

summary(model1 <- lm(pospol ~ treated, data = elsoc_merged[elsoc_merged$year >= 2019, ])) # not sig

summary(model1 <- lm(c43 ~ treated, data = elsoc_merged[elsoc_merged$year >= 2019, ])) #plebistice vote not sig

summary(model1 <- lm(m0_edad ~ treated, data = elsoc_merged[elsoc_merged$year >= 2019, ])) #*

summary(model1 <- lm(m13 ~ treated, data = elsoc_merged[elsoc_merged$year >= 2019, ])) # income *

### Political efficacy - Individual mechanism

#Q1: The actions of the movement generate social change
actions_mov_covariates <- feols(c21_05 ~ treated + m01 | idencuesta, 
                                data = elsoc_merged, 
                                panel.id = c("idencuesta", "year"),
                                cluster = ~municipality,
                                weights = ~ponderador02)

#Q2: My vote influences the outcome of the election
vote_influence_covariates <- feols(c10_02 ~ treated + m01 | idencuesta, 
                                   data = elsoc_merged, 
                                   panel.id = c("idencuesta", "year"),
                                   cluster = ~municipality,
                                   weights = ~ponderador02)

#Q3 level of information in media
informacion_model_covariates <- feols(c14_02 ~ treated + m01 | idencuesta,
                                      data = elsoc_merged, 
                                      panel.id = c("idencuesta", "year"),
                                      cluster = ~municipality,
                                      weights = ~ponderador02)

msummary(list(actions_mov_covariates,
              vote_influence_covariates,
              informacion_model_covariates),
         stars = c('*' = .1, '**' = .05, '***' = .01),
         output = "markdown")

 ## Plot

table(elsoc_merged$c21_05, elsoc_merged$year)

table(elsoc_merged$c10_02, elsoc_merged$year)

table(elsoc_merged$c14_02, elsoc_merged$year)

#Q1: The actions of the movement generate social change
actions2 <- tidy(actions_mov_covariates)

fit_cis_95 <- confint(actions_mov_covariates, level = 0.95) %>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..", "conf.high_95" = "X97.5..")

fit_cis_90 <- confint(actions_mov_covariates, level = 0.90) %>% 
  data.frame() %>%
  rename("conf.low_90" = "X5..", "conf.high_90" = "X95..")

actions2 <- bind_cols(actions2, fit_cis_95, fit_cis_90) %>%
  rename(Variable = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Variable != "(Intercept)")

actions2$Variable[actions2$Variable == 
                    "treated"] <- "ATT"

actions2$model <- "Extended Model"

df_actions_coef_only <- actions2 %>%
  filter(Variable == "ATT")

#Q2: My vote influences the outcome of the election
influence2 <- tidy(vote_influence_covariates)

fit_cis_95 <- confint(vote_influence_covariates, level = 0.95) %>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..", "conf.high_95" = "X97.5..")

fit_cis_90 <- confint(vote_influence_covariates, level = 0.90) %>% 
  data.frame() %>%
  rename("conf.low_90" = "X5..", "conf.high_90" = "X95..")

influence2 <- bind_cols(influence2, fit_cis_95, fit_cis_90) %>%
  rename(Variable = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Variable != "(Intercept)")

influence2$Variable[influence2$Variable == 
                      "treated"] <- "ATT"

influence2$model <- "Extended Model"

df_influence_coef_only <- influence2 %>%
  filter(Variable == "ATT")

#Q3: Political information
information2 <- tidy(informacion_model_covariates)

fit_cis_95 <- confint(informacion_model_covariates, level = 0.95) %>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..", "conf.high_95" = "X97.5..")

fit_cis_90 <- confint(informacion_model_covariates, level = 0.90) %>% 
  data.frame() %>%
  rename("conf.low_90" = "X5..", "conf.high_90" = "X95..")

information2 <- bind_cols(information2, fit_cis_95, fit_cis_90) %>%
  rename(Variable = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Variable != "(Intercept)")

information2$Variable[information2$Variable == 
                        "treated"] <- "ATT"

information2$model <- "Extended Model"

df_information_coef_only <- information2 %>%
  filter(Variable == "ATT")

df_actions_coef_only$dv <- "Actions Movement"
df_influence_coef_only$dv <- "Influence One's Own Vote"
df_information_coef_only$dv <- "Political Information"

individual_plots <- rbind(df_actions_coef_only,
                          df_influence_coef_only,df_information_coef_only)

individual_plots_effect <- ggplot(individual_plots, aes(x = Variable, y = Coefficient)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = Variable, y = Coefficient)) + 
  geom_linerange(aes(x = Variable, ymin = conf.low_90, ymax = conf.high_90),
                 lwd = 1) +
  geom_linerange(aes(x = Variable, ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2) + 
  theme_bw() + 
  coord_flip() +
  facet_grid(~dv) +
  theme(axis.title.y=element_blank()) +
  theme(aspect.ratio = 1) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

individual_plots_effect

ggsave("03-outputs/individual_plots_effect.png",
       height = 7, units = "cm", dpi = 600)


## Yearly individual efficacy

count_data_efficacy <- elsoc_merged %>%
  tidyr::pivot_longer(cols = c(c21_05, c10_02, c14_02), names_to = "variable", 
                      values_to = "category") %>%
  dplyr::filter(!is.na(category)) %>%
  dplyr::group_by(year, variable, protest_binary) %>%
  dplyr::summarize(mean_value = mean(category, na.rm = TRUE),
                   ci_lower = mean_value - qt(0.975, 
                                              sum(!is.na(category))) * sd(category, 
                                                                          na.rm = TRUE) /
                     sqrt(sum(!is.na(category))),
                   ci_upper = mean_value + qt(0.975, 
                                              sum(!is.na(category))) * sd(category, 
                                                                          na.rm = TRUE) /
                     sqrt(sum(!is.na(category))))


custom_labels <- c("c21_05" = "Actions Movement", "c10_02" = "Influence Own's vote",
                   "c14_02" = "Political Information")

efficacy_trends <- ggplot(count_data_efficacy, aes(x = year, y = mean_value, 
                                                   color = as.factor(protest_binary))) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(width = 0.2), width = 0.2) +
  facet_grid(custom_labels[variable] ~ ., scales = "free_y") +  # Use custom labels for variable names
  theme_minimal() +
  labs(color = "Treated", x = "Year", y = "Mean") +
  scale_color_manual(values = c("0" = "#A6ACAF", "1" = "black"),
                     labels = c("0" = "Non-treated", "1" = "Treated")) +
  ylim(2, 4.5) +  # Set the y-axis limits to 1 to 5
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.3))

efficacy_trends

ggsave("03-outputs/efficacy_trends.png", 
       width = 16, height = 17, units = "cm", dpi = 600)


# 3) Robustness checks

# 3.1) Parallel trends

### Turnout

parallel_trends <- data_filtered %>%
  mutate(Country = factor(country, levels = c("bolivia", "chile"), 
                          labels = c("Bolivia", "Chile")),
         Protest = factor(protest_binary, levels = c("0", "1"), 
                          labels = c("Non Treated", "Treated"))) %>%
  group_by(Country, Protest, year) %>%
  summarise(mean_participation = mean(perc_participation, na.rm = TRUE),
            se_participation = sd(perc_participation) / sqrt(n()),
            upper = mean_participation + (1.96 * se_participation),
            lower = mean_participation - (1.96 * se_participation))

# Figure 3 manuscript
parallel_trends_plot <- ggplot(parallel_trends, aes(x = year, 
                                                    y = mean_participation, 
                                                    color = Protest)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 0.5) +
  geom_line(aes(group = Protest)) + 
  scale_color_manual(values=c("#A6ACAF", "#17202A")) +
  theme_bw() +
  facet_wrap(~ Country) +
  labs(x = "Election Year", y = "Mean Turnout") +
  geom_vline(xintercept = 2019, linetype = "dotted") +
  geom_text(aes(x=2019, label="Treatment", y= 0.6), 
            color = "#34495E", angle=90, vjust = 1.2, 
            text=element_text(size=11)) +
  theme(aspect.ratio = 4/3) 

parallel_trends_plot

ggsave("03-outputs/parallel_trends_turnout.png", 
       width = 15, height = 12, units = "cm", dpi = 600)

### Turnout only for Chile

parallel_trends_chile <- parallel_trends %>%
  filter(Country == "Chile")

# Figure 3 Manuscript
parallel_trends_plot_chile <- ggplot(parallel_trends_chile, 
                                     aes(x = year, y = mean_participation, 
                                         color = Protest)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 0.5) +
  geom_line(aes(group = Protest)) + 
  scale_color_manual(values=c("#A6ACAF", "#17202A")) +
  theme_bw() +
  labs(x = "Election Year", y = "Mean Turnout") +
  geom_vline(xintercept = 2019, linetype = "dotted") +
  geom_text(aes(x=2019, label="Treatment", y= 0.4), 
            color = "#34495E", angle=90, vjust = 1.2, text=element_text(size=11)) +
  scale_x_continuous(breaks=seq(2013, 2020, 1)) +
  labs(title = "Voter turnout in Chile")

parallel_trends_plot_chile #maybe add both graphs in the same graph

ggsave("03-outputs/parallel_trends_plot_chile.png", 
       height = 9, units = "cm", dpi = 600)

### Incumbent vote share Bolivia

parallel_incumbent <- data_filtered %>%
  mutate(Country = factor(country, levels = c("bolivia", "chile"), 
                          labels = c("Bolivia", "Chile")),
         Protest = factor(protest_binary, levels = c("0", "1"), 
                          labels = c("Non Treated", "Treated"))) %>%
  group_by(Country, Protest, year) %>%
  summarise(mean_incumbent = mean(incumbent_vote_share, na.rm = TRUE),
            se_incumbent = sd(incumbent_vote_share) / sqrt(n()),
            upper = mean_incumbent + (1.96 * se_incumbent),
            lower = mean_incumbent - (1.96 * se_incumbent))

parallel_trends_bolivia <- parallel_incumbent %>%
  filter(Country == "Bolivia")

# Figure 4 Manuscript
parallel_incumbent_plot_bolivia <- ggplot(parallel_trends_bolivia, aes(x = year, 
                                                                       y = mean_incumbent, 
                                                                       color = Protest)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 0.5) +
  geom_line(aes(group = Protest)) + 
  scale_color_manual(values=c("#A6ACAF", "#17202A")) +
  theme_bw() +
  labs(x = "Election Year", y = "Mean Incumbent Vote Share") +
  geom_vline(xintercept = 2019.2, linetype = "dotted") +
  geom_text(aes(x=2019.2, label="Treatment", y= 0.80), 
            color = "#34495E", angle=90, vjust = 1.2, text=element_text(size=11)) +
  scale_x_continuous(breaks=seq(2009, 2020, 1)) +
  ylim(0.30, 0.90)  +
  labs(title = "Incumbent Vote Share in Bolivia")

parallel_incumbent_plot_bolivia

ggsave("03-outputs/parallel_incumbent_plot_bolivia.png", 
       height = 11, units = "cm", dpi = 600)

### Both plots together

parallel_plots_together <- ggarrange(
  parallel_trends_plot_chile, parallel_incumbent_plot_bolivia,
  ncol = 1, nrow = 2,
  common.legend = TRUE, legend = "bottom") 

parallel_plots_together

ggsave("outputs/figures/parallel_plots_together.png", 
       width = 20, height = 15, units = "cm", dpi = 600)

### Plot trends but with `data`

trends_turnout_all <- data %>%
  select(country, year, province, municipality, electoral_roll_municipality, total_votes) %>%
  group_by(country, year) %>%
  summarise(electoral_roll = sum(electoral_roll_municipality),
            votes = sum(total_votes)) %>%
  mutate(turnout = votes/electoral_roll) %>%
  rename(Country = country, Year = year, Turnout = turnout) %>%
  mutate(Country = recode(Country, bolivia = "Bolivia", chile = "Chile"))

trends_turnout_all_plot <- 
  ggplot(trends_turnout_all, aes(x = Year, y = Turnout)) +
  geom_point() +
  geom_line(aes(group = Country)) +
  theme_bw() +
  facet_wrap(~Country, ncol = 1) +
  scale_x_continuous(breaks=seq(1997, 2021, 1), guide = guide_axis(angle = 90)) +
  ylim(0.00, 1.00)

trends_turnout_all_plot

ggsave("03-outputs/trends_turnout_all_plot.png", 
       width = 17, height = 15, units = "cm", dpi = 600)

trends_turnout_all_chile <- trends_turnout_all %>%
  filter(Country == "Chile")

trends_turnout_all_bolivia <- trends_turnout_all %>%
  filter(Country == "Bolivia")

trends_turnout_all_plot_chile <- 
  ggplot(trends_turnout_all_chile, aes(x = Year, y = Turnout)) +
  geom_point() +
  geom_line(aes(group = Country)) +
  theme_bw() 

trends_turnout_all_plot_chile

trends_turnout_all_plot_bolivia <- 
  ggplot(trends_turnout_all_bolivia, aes(x = Year, y = Turnout)) +
  geom_point() +
  geom_line(aes(group = Country)) +
  theme_bw() 

trends_turnout_all_plot_bolivia

# 3.2) Incumbent vote share but with null/blank votes

# - Create variable that summarizes null and white votes
bolivia_db %<>%
  rowwise() %>%
  mutate(invalid_votes = sum(c_across(white_votes|null_votes)))

# - Transform to proportion
bolivia_db$invalid_votes_prop <- (bolivia_db$invalid_votes/bolivia_db$electoral_roll_municipality)

# Just DID OLS
invalid_votes1 <- lm(invalid_votes_prop ~ treated, data = bolivia_db)

# + FE years
invalid_votes2 <- feols(invalid_votes_prop ~ treated | year, data = bolivia_db,
                        panel.id = c("mun_code", "year"))

# + FE municipality
invalid_votes3 <- feols(invalid_votes_prop ~ treated | 
                          mun_code + year, data = bolivia_db,
                        panel.id = c("mun_code", "year"))

# + Controls 
invalid_votes4 <- feols(invalid_votes_prop ~ treated +
                          log(population) | 
                          mun_code + year, data = bolivia_db,
                        panel.id = c("mun_code", "year"))

# Table 3 appendix
msummary(list(invalid_votes1, invalid_votes2, invalid_votes3, invalid_votes4), 
         stars = c('*' = .1, '**' = .05, '***' = .01), 
         output = "markdown") 

# 3.3) Spillover effect province level

### Turnout
# Just DID OLS
spillover_turnout1 <- lm(perc_participation ~ treated_prov, 
                         data = chile_db[chile_db$treated == 0, ])

# + FE year
spillover_turnout2 <- feols(perc_participation ~ treated_prov | year, 
                            data = chile_db[chile_db$treated == 0, ], 
                            panel.id = c("province", "year"))

# + FE municipality
spillover_turnout3 <- feols(perc_participation ~ treated_prov | 
                              province + year, data = chile_db[chile_db$treated == 0, ], 
                            panel.id = c("province", "year"))
# + Controls
spillover_turnout4 <- feols(perc_participation ~ treated_prov  + 
                              log(population) | province + year, 
                            data = chile_db[chile_db$treated == 0, ], 
                            panel.id = c("province", "year"))

# Table C1 appendix
modelsummary(list(spillover_turnout1, spillover_turnout2,
                  spillover_turnout3, spillover_turnout4),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             output = "markdown")

### Incumbent vote share
# Just DID OLS
spillover_incumbent1 <- lm(incumbent_vote_share ~ treated_prov,
                           data = bolivia_db[bolivia_db$treated == 0, ])

# + FE year
spillover_incumbent2 <- feols(incumbent_vote_share ~ treated_prov | 
                              year, data = bolivia_db[bolivia_db$treated == 0, ], 
                              panel.id = c("province", "year"))

# + FE municipality
spillover_incumbent3 <- feols(incumbent_vote_share ~ treated_prov | province + 
                              year, data = bolivia_db[bolivia_db$treated == 0, ],
                              panel.id = c("province", "year"))

# + Controls
spillover_incumbent4 <- feols(incumbent_vote_share ~ treated_prov + 
                              log(population) | province + year, 
                              data = bolivia_db[bolivia_db$treated == 0, ], 
                              panel.id = c("province", "year"))

# Table C2 appendix
modelsummary(list(spillover_incumbent1, spillover_incumbent2,
                  spillover_incumbent3, spillover_incumbent4),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             output = "markdown")

# 3.4) ## November 2021 election for Turnout

#chile_2021
chile_db_2021 <- data %>%
  filter(country == "chile" & year >= 2012) %>%
  filter(year != 2020)

table(chile_db_2021$year)

# Just DID OLS
turnout_2021_1 <- lm(perc_participation ~ treated, data = chile_db_2021)

# + FE year
turnout_2021_2 <- feols(perc_participation ~ treated | year, 
                        data = chile_db_2021, 
                        panel.id = c("mun_code", "year"))

# + FE municipality
turnout_2021_3 <- feols(perc_participation ~ treated | 
                          mun_code + year, data = chile_db_2021, 
                        panel.id = c("mun_code", "year"))
# + Controls
turnout_2021_4 <- feols(perc_participation ~ treated + log(population) | 
                          mun_code + year, data = chile_db_2021, 
                        panel.id = c("mun_code", "year"))

# Table C3 Appendix
msummary(list(turnout_2021_1, turnout_2021_2,
              turnout_2021_3, turnout_2021_4), 
         stars = c('*' = .1, '**' = .05, '***' = .01), 
         output = "markdown")

# 3.5) Type of protest in Bolivia

# - For `bolivia_db`, estimate additional models excluding the biggest 
# - department that had anti-government protests (Cochamba), and the department
# - that had most pro-government protests (La Paz).

# - First, create separate databases
bolivia_db_no_cochamba <- bolivia_db %>%
  filter(region_cod != 1)

bolivia_db_no_lapaz <- bolivia_db %>%
  filter(region_cod != 2)

# No Cochamba (no anti-MAS)
incumbent_no_cochamba <- feols(incumbent_vote_share ~ treated +
                                 log(population) | mun_code + year, 
                               data = bolivia_db_no_cochamba,
                               panel.id = c("mun_code", "year"))
# No La Paz (no pro-MAS)
incumbent_no_lapaz <- feols(incumbent_vote_share ~ treated +
                              log(population) | mun_code + year, 
                            data = bolivia_db_no_lapaz,
                            panel.id = c("mun_code", "year"))

# + Controls
incumbent_4 <- feols(incumbent_vote_share ~ treated + log(population) | 
                     mun_code + year, data = bolivia_db,
                     panel.id = c("mun_code", "year"))


# Table C4 appendix
msummary(list(incumbent_no_cochamba, incumbent_no_lapaz, incumbent_4), 
         stars = c('*' = .1, '**' = .05, '***' = .01),
         output = "markdown") 

# 3.6) Continuous specification of the treatment variable

## Voter turnout, create quartile variable

protests %>%
  filter(country == "chile") %>%
  plyr::summarise(Mean = mean(number_protest, na.rm=TRUE)) #12.4422

protests %>%
  filter(country == "chile") %>%
  plyr::summarise(Mean = mean(number_protest_acled, na.rm=TRUE)) #3.352601

chile_db %<>%
  mutate(quartile_prot = ntile(number_protest, 4),
         quartile_acled = ntile(number_protest_acled, 4),
         above_mean_prot = case_when(number_protest < 12 ~ "0",
                                     number_protest >= 12 ~ "1"),
         above_mean_acled = case_when(number_protest < 3 ~ "0",
                                      number_protest >= 3 ~ "1"))

# + Controls
turnout_number_prot <- feols(perc_participation ~ factor(post)*number_protest +
                             log(population) | mun_code + year, 
                             data = chile_db, panel.id = c("mun_code", "year"))

turnout_number_prot2 <- feols(perc_participation ~ factor(post)*number_protest_acled +
                              log(population) | mun_code + year, data = chile_db,
                              panel.id = c("mun_code", "year"))

turnout_mean_prot <- feols(perc_participation ~ factor(post)*factor(above_mean_prot) +
                           log(population) | mun_code + year, data = chile_db,
                           panel.id = c("mun_code", "year"))

turnout_mean_acled <- feols(perc_participation ~ factor(post)*factor(above_mean_acled) +
                            log(population) | mun_code + year, data = chile_db,
                            panel.id = c("mun_code", "year"))
# Table 1 appendix
modelsummary(list(turnout_number_prot, turnout_number_prot2,
                  turnout_mean_prot,turnout_mean_acled), 
             stars = c('*' = .1, '**' = .05, '***' = .01),
             output = "markdown",
             title = "Effect of protests (continuous and above mean specification) on voter turnout")

## Incumbent vote share

protests %>%
  filter(country == "bolivia") %>%
  plyr::summarise(Mean = mean(number_protest, na.rm=TRUE)) #0.6920821

protests %>%
  filter(country == "bolivia") %>%
  plyr::summarise(Mean = mean(number_protest_acled, na.rm=TRUE)) #0.7536657

bolivia_db %<>%
  mutate(quartile_prot = ntile(number_protest, 4),
         quartile_acled = ntile(number_protest_acled, 4),
         above_mean_prot = case_when(number_protest < 1 ~ "0",
                                     number_protest >= 1 ~ "1"),
         above_mean_acled = case_when(number_protest < 1 ~ "0",
                                      number_protest >= 1 ~ "1"))

# + Controls
incumbent_number_prot <- feols(incumbent_vote_share ~ factor(post)*number_protest +
                               log(population) | mun_code + year, data = bolivia_db,
                               panel.id = c("mun_code", "year"))

incumbent_number_prot2 <- feols(incumbent_vote_share ~ factor(post)*number_protest_acled +
                                log(population) | mun_code + year, data = bolivia_db,
                                panel.id = c("mun_code", "year"))

incumbent_mean_prot <- feols(incumbent_vote_share ~ factor(post)*factor(above_mean_prot) +
                             log(population) | mun_code + year, data = bolivia_db,
                             panel.id = c("mun_code", "year"))

incumbent_mean_acled <- feols(incumbent_vote_share ~ 
                              factor(post)*factor(above_mean_acled) +
                              log(population) | mun_code + year, data = bolivia_db,
                              panel.id = c("mun_code", "year"))

# Table 1 appendix
modelsummary(list(incumbent_number_prot, incumbent_number_prot2,
                  incumbent_mean_prot,incumbent_mean_acled), 
             stars = c('*' = .1, '**' = .05, '***' = .01),
             output = "markdown",
             title = "Effect of protests (continuous and above mean specification) on incumbent vote share")

# 3.7) Effect of treatment in retrospective vote

# - Variable `participacion` merged variable 
# - `c43` (Retrospective turnout Plebiscite New Constitution) and variable  
# - `c11` (Retrospective electoral participation) but only year 2021 will be 
# - considered so it will only measure retrospective vote in the plebiscite.

# - `participacion` is a binary variable so the models need to be adjusted accordingly

# Create a pdata.frame (panel data frame) from the data
panel_data <- pdata.frame(elsoc_merged, index = c("idencuesta", "year"))

# Estimate the fixed-effects logistic regression model
fe_logit_model <- plm(participacion ~ treated + m01, 
                      data = panel_data, model = "within", 
                      effect = "individual", 
                      index = c("idencuesta", "year"), 
                      family = binomial(link = "logit"),
                      weights = panel_data$ponderador02)

msummary(list(fe_logit_model),
         stars = c('*' = .1, '**' = .05, '***' = .01),
         output = "markdown")



