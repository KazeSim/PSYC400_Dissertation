# R Script for PSYC400 Dissertation

####### Prepare RStudio #######

# Clear RStudio

rm(list=ls())

# Set working directory

setwd("S:/RStudio/PSYC400_Dissertation")

options(scipen=999)

# Load relevant libraries

library(tidyverse)

library(psych)

library(moments)

library(ggExtra)

library(gridExtra)

library(lavaan)

library(lavaanPlot)

library(DiagrammeRsvg)

library(rsvg)

library(car)

library(olsrr)

library(rstatix)

library(lsr)

library(broom)

library(stats)

library(effectsize)

####### Data Wrangling #######

# Create datasets from Qualtrics surveys

qualtrics_old <- read_csv("qualtrics_old.csv")

qualtrics_new <- read_csv("qualtrics_new.csv")

# Join Qualtrics surveys

dataset <- qualtrics_old %>%
  full_join(qualtrics_new)

# Remove first and second row from 'dataset'

dataset <- dataset[-c(1, 2), ]

# Select required variables

dataset_select <- dataset %>%
  select(participant_number, age, sex, english, rct_01:rct_32, spq_01_cp_ir_1:spq_32_cp_up_1)

# Change variables to appropriate class

dataset_select <- dataset_select %>%
  mutate(across(c(rct_01:rct_32, spq_01_cp_ir_1:spq_32_cp_up_1, age), as.numeric)) %>%
  mutate(across(c(participant_number, sex, english), as.factor))

# Replace na with 0 for reading comprehension task

dataset_select <- dataset_select %>%
  replace(is.na(.), 0)

# Recode 'sex' variable from numeric code to corresponding category

dataset_select <- dataset_select %>%
  mutate(sex = dplyr::recode(sex, `1` = "male", `2` = "female", `3` = "prefer not to say"))

# Recode 'english' variable from numeric code to corresponding category

dataset_select <- dataset_select %>%
  mutate(english = dplyr::recode(english, '1' = "english is not native language", `2` = "english is native language",
                          `3` = "prefer not to say"))

# Show summary of 'dataset_select' dataset

summary(dataset_select)

describe(dataset_select)

# Create long datasets

dataset_select_long_rct <- dataset_select %>%
  pivot_longer(rct_01:rct_32, names_to = "rct_question", values_to = "rct_response") %>%
  select(participant_number, age, sex, english, rct_question, rct_response)

dataset_select_long_spq <- dataset_select %>%
  pivot_longer(spq_01_cp_ir_1:spq_32_cp_up_1, names_to = "spq_question", values_to = "spq_response") %>%
  select(participant_number, age, sex, english, spq_question, spq_response)

# Draw plots for long datasets

plot_long_rct <- dataset_select_long_rct %>%
  ggplot(aes(x = rct_response)) +
  geom_histogram(bins = 2) +
  theme_bw() +
  facet_wrap(~rct_question)

plot_long_rct

ggsave("plot_long_rct.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw', plot_long_rct)

plot_long_spq <- dataset_select_long_spq %>%
  ggplot(aes(x = spq_response)) +
  geom_histogram(bins = 5) +
  theme_bw() +
  facet_wrap(~spq_question)

plot_long_spq

ggsave("plot_long_spq.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw', plot_long_spq)

# Calculate reading comprehension task score and assign to variable 'rct_score'

dataset_select <- dataset_select %>%
  mutate(rct_score = rowSums(across(c(rct_01:rct_32))))

# Calculate total schizotypal personality questionnaire score and assign to variable "spq_total"

dataset_select <- dataset_select %>%
  mutate(spq_total = rowSums(across(c(spq_01_cp_ir_1:spq_32_cp_up_1))))

# Calculate the schizotypal personality questionnaire interpersonal sub dimension score and assign to variable "spq_interpersonal"

dataset_select <- dataset_select %>%
  mutate(spq_interpersonal = (spq_07_ip_cf_1 + spq_08_ip_cf_1 + spq_09_ip_cf_1 + spq_10_ip_ca_1 + spq_11_ip_ca_1 + spq_12_ip_ca_1))

# Calculate the schizotypal personality questionnaire cognitive perceptual sub dimension score and assign to variable
# "spq_cognitive_perceptual"

dataset_select <- dataset_select %>%
  mutate(spq_cognitive_perceptual = (spq_01_cp_ir_1 + spq_02_cp_ir_1 + spq_03_cp_ir_1 + spq_04_cp_su_1 + spq_05_cp_su_1 +
                                       spq_06_cp_su_1 + spq_21_cp_mt_1 + spq_22_cp_mt_1 + spq_23_cp_mt_1 + spq_24_cp_mt_1 +
                                       spq_29_cp_up_1 + spq_30_cp_up_1 + spq_31_cp_up_1 + spq_32_cp_up_1))

# Calculate the schizotypal personality questionnaire disorganised sub dimension score and assign to variable "spq_disorganised"

dataset_select <- dataset_select %>%
  mutate(spq_disorganised = (spq_13_do_eb_1 + spq_14_do_eb_1 + spq_15_do_eb_1 + spq_16_do_eb_1 + spq_25_do_os_1 + spq_26_do_os_1 +
                               spq_27_do_os_1 + spq_28_do_os_1))

# Calculate the schizotypal personality questionnaire social anxiety sub dimension score and assign to variable "spq_social_anxiety"

dataset_select <- dataset_select %>%
  mutate(spq_social_anxiety = (spq_17_ip_sa_1 + spq_18_ip_sa_1 + spq_19_ip_sa_1 + spq_20_ip_sa_1))

# Select required variables

dataset_final <- dataset_select  %>%
  select(participant_number, age, sex, english, rct_score, spq_total, spq_interpersonal, spq_cognitive_perceptual,
         spq_disorganised, spq_social_anxiety)

# Show summary of 'dataset_final'

summary(dataset_final)

describe(dataset_final)

# Assign schizotypal groups with norms from study that developed the schizotypal personality questionnaire

dataset_final_male_sg_sn <- dataset_final %>%
  filter(sex == "male")

dataset_final_female_sg_sn <- dataset_final %>%
  filter(sex == "female")

dataset_final_male_sg_sn <- dataset_final_male_sg_sn %>%
  mutate(spq_interpersonal_group = case_when(spq_interpersonal < (9.72) ~ "low", spq_interpersonal >= (9.72)
                                             & spq_interpersonal <= (19.6) ~ "medium", spq_interpersonal > (19.6) ~ "high"))

dataset_final_female_sg_sn <- dataset_final_female_sg_sn %>%
  mutate(spq_interpersonal_group = case_when(spq_interpersonal < (8.86) ~ "low", spq_interpersonal >= (8.86)
                                             & spq_interpersonal <= (18.42) ~ "medium", spq_interpersonal > (18.42) ~ "high"))

dataset_final_male_sg_sn <- dataset_final_male_sg_sn %>%
  mutate(spq_cognitive_perceptual_group = case_when(spq_cognitive_perceptual < (22.52) ~ "low", spq_cognitive_perceptual >= (22.52)
                                                    & spq_cognitive_perceptual <= (38) ~ "medium", spq_cognitive_perceptual > (38) ~ "high"))

dataset_final_female_sg_sn <- dataset_final_female_sg_sn %>%
  mutate(spq_cognitive_perceptual_group = case_when(spq_cognitive_perceptual < (23.4) ~ "low", spq_cognitive_perceptual >= (23.4)
                                                    & spq_cognitive_perceptual <= (38.42) ~ "medium",
                                                    spq_cognitive_perceptual > (38.42) ~ "high"))

dataset_final_male_sg_sn <- dataset_final_male_sg_sn %>%
  mutate(spq_disorganised_group = case_when(spq_disorganised < (16.71) ~ "low", spq_disorganised >= (16.71)
                                            & spq_disorganised <= (29.05) ~ "medium", spq_disorganised > (29.05) ~ "high"))

dataset_final_female_sg_sn <- dataset_final_female_sg_sn %>%
  mutate(spq_disorganised_group = case_when(spq_disorganised < (17.41) ~ "low", spq_disorganised >= (17.41)
                                            & spq_disorganised <= (29.87) ~ "medium", spq_disorganised > (29.87) ~ "high"))

dataset_final_male_sg_sn <- dataset_final_male_sg_sn %>%
  mutate(spq_social_anxiety_group = case_when(spq_social_anxiety < (7.85) ~ "low", spq_social_anxiety >= (7.85)
                                              & spq_social_anxiety <= (15.95) ~ "medium", spq_social_anxiety > (15.95) ~ "high"))

dataset_final_female_sg_sn <- dataset_final_female_sg_sn %>%
  mutate(spq_social_anxiety_group = case_when(spq_social_anxiety < (7.69) ~ "low", spq_social_anxiety >= (7.69)
                                              & spq_social_anxiety <= (15.49) ~ "medium", spq_social_anxiety > (15.49) ~ "high"))

dataset_final_sg_sn <- dataset_final_male_sg_sn %>%
  full_join(dataset_final_female_sg_sn)

dataset_final_sg_sn <- dataset_final_sg_sn %>%
  mutate(across(c(spq_interpersonal_group, spq_cognitive_perceptual_group, spq_disorganised_group, spq_social_anxiety_group), as.factor))

dataset_final <- dataset_final_sg_sn

summary(dataset_final)

describe(dataset_final)

# Draw plot of schiztopyal sub dimeansion distributions

dataset_final$spq_interpersonal_group <- factor(dataset_final$spq_interpersonal_group,
                                                levels=c("low", "medium", "high"))

plot_spq_int <- dataset_final %>%
  ggplot(aes(x = spq_interpersonal_group, y = (..count..))) +
  geom_bar(position = 'dodge', fill = "grey", colour = "black") +
  ylim(0, 25) +
  theme_bw() +
  labs(x = "Interpersonal Groups", y = "Participants") +
  theme(text = element_text(size = 20))

plot_spq_int

dataset_final$spq_cognitive_perceptual_group <- factor(dataset_final$spq_cognitive_perceptual_group,
                                                       levels=c("low", "medium", "high"))

plot_spq_cog <- dataset_final %>%
  ggplot(aes(x = spq_cognitive_perceptual_group, y = (..count..))) +
  geom_bar(position = 'dodge', fill = "grey", colour = "black") +
  ylim(0, 25) +
  theme_bw() +
  labs(x = "Cognitive Perceptual Groups", y = "Participants") +
  theme(text = element_text(size = 20))

plot_spq_cog

dataset_final$spq_disorganised_group <- factor(dataset_final$spq_disorganised_group,
                                                       levels=c("low", "medium", "high"))

plot_spq_dis <- dataset_final %>%
  ggplot(aes(x = spq_disorganised_group, y = (..count..))) +
  geom_bar(position = 'dodge', fill = "grey", colour = "black") +
  ylim(0, 25) +
  theme_bw() +
  labs(x = "Disorganised  Groups", y = "Participants") +
  theme(text = element_text(size = 20))

plot_spq_dis

dataset_final$spq_social_anxiety_group <- factor(dataset_final$spq_social_anxiety_group,
                                                         levels=c("low", "medium", "high"))

plot_spq_soc <- dataset_final %>%
  ggplot(aes(x = spq_social_anxiety_group, y = (..count..))) +
  geom_bar(position = 'dodge', fill = "grey", colour = "black") +
  ylim(0, 25) +
  theme_bw() +
  labs(x = "Social Anxiety Groups", y = "Participants") +
  theme(text = element_text(size = 20))

plot_spq_soc

ga_plot_spq_group_distrib <- grid.arrange(plot_spq_int, plot_spq_cog, plot_spq_dis, plot_spq_soc, ncol = 2)

ggsave("ga_plot_spq_group_distrib.tiff", units="in", width=13, height=10, dpi=300, compression = 'lzw', ga_plot_spq_group_distrib)

# Create dataset from verbal fluency tasks data

dataset_vft <- read_csv("Verbal_Fluency_Tasks.csv")

# Change variables to appropriate class

dataset_vft <- dataset_vft %>%
  mutate(across(c(participant_number, semantic_fruit, semantic_animal, letter_m, letter_s), as.factor))

summary(dataset_vft)

describe(dataset_vft)

# Create dataset verbal fluency datasets

dataset_vft_sfv <- dataset_vft %>%
  select(participant_number, semantic_fruit, semantic_fruit_valid) %>%
  filter(semantic_fruit_valid == 1) %>%
  select(participant_number, semantic_fruit) %>%
  drop_na(semantic_fruit)

dataset_vft_sav <- dataset_vft %>%
  select(participant_number, semantic_animal, semantic_animal_valid) %>%
  filter(semantic_animal_valid == 1) %>%
  select(participant_number, semantic_animal) %>%
  drop_na(semantic_animal)

dataset_vft_lmv <- dataset_vft %>%
  select(participant_number, letter_m, letter_m_valid) %>%
  filter(letter_m_valid == 1) %>%
  select(participant_number, letter_m) %>%
  drop_na(letter_m)

dataset_vft_lsv <- dataset_vft %>%
  select(participant_number, letter_s, letter_s_valid) %>%
  filter(letter_s_valid == 1) %>%
  select(participant_number, letter_s) %>%
  drop_na(letter_s)

dataset_vft_sfp <- dataset_vft %>%
  select(participant_number, semantic_fruit, semantic_fruit_valid) %>%
  filter(semantic_fruit_valid == 1 | semantic_fruit_valid == 0) %>%
  select(participant_number, semantic_fruit) %>%
  drop_na(semantic_fruit)

dataset_vft_sap <- dataset_vft %>%
  select(participant_number, semantic_animal, semantic_animal_valid) %>%
  filter(semantic_animal_valid == 1 | semantic_animal_valid == 0) %>%
  select(participant_number, semantic_animal) %>%
  drop_na(semantic_animal)

dataset_vft_lmp <- dataset_vft %>%
  select(participant_number, letter_m, letter_m_valid) %>%
  filter(letter_m_valid == 1 | letter_m_valid == 0) %>%
  select(participant_number, letter_m) %>%
  drop_na(letter_m)

dataset_vft_lsp <- dataset_vft %>%
  select(participant_number, letter_s, letter_s_valid) %>%
  filter(letter_s_valid == 1 | letter_s_valid == 0) %>%
  select(participant_number, letter_s) %>%
  drop_na(letter_s)

# Remove duplicated words for each participant

dataset_vft_sfv <- dataset_vft_sfv %>%
  group_by(participant_number) %>%
  distinct(semantic_fruit) %>%
  ungroup()

dataset_vft_sav <- dataset_vft_sav %>%
  group_by(participant_number) %>%
  distinct(semantic_animal) %>%
  ungroup()

dataset_vft_lmv <- dataset_vft_lmv %>%
  group_by(participant_number) %>%
  distinct(letter_m) %>%
  ungroup()

dataset_vft_lsv <- dataset_vft_lsv %>%
  group_by(participant_number) %>%
  distinct(letter_s) %>%
  ungroup()

dataset_vft_sfp <- dataset_vft_sfp %>%
  group_by(participant_number) %>%
  distinct(semantic_fruit) %>%
  ungroup()

dataset_vft_sap <- dataset_vft_sap %>%
  group_by(participant_number) %>%
  distinct(semantic_animal) %>%
  ungroup()

dataset_vft_lmp <- dataset_vft_lmp %>%
  group_by(participant_number) %>%
  distinct(letter_m) %>%
  ungroup()

dataset_vft_lsp <- dataset_vft_lsp %>%
  group_by(participant_number) %>%
  distinct(letter_s) %>%
  ungroup()

# Count unique responses

dataset_fruit_valid_tot <- dataset_vft_sfv %>%
  select(semantic_fruit)

dataset_animal_valid_tot <- dataset_vft_sav %>%
  select(semantic_animal)

dataset_fruit_productivity_tot <- dataset_vft_sfp %>%
  select(semantic_fruit)

dataset_animal_productivity_tot <- dataset_vft_sap %>%
  select(semantic_animal)

dataset_m_valid_tot <- dataset_vft_lmv %>%
  select(letter_m)

dataset_s_valid_tot <- dataset_vft_lsv %>%
  select(letter_s)

dataset_m_productivity_tot <- dataset_vft_lmp %>%
  select(letter_m)

dataset_s_productivity_tot <- dataset_vft_lsp %>%
  select(letter_s)

dataset_fruit_valid_tot_d <- dataset_vft_sfv %>%
  distinct(semantic_fruit)

dataset_animal_valid_tot_d <- dataset_vft_sav %>%
  distinct(semantic_animal)

dataset_fruit_productivity_tot_d <- dataset_vft_sfp %>%
  distinct(semantic_fruit)

dataset_animal_productivity_tot_d <- dataset_vft_sap %>%
  distinct(semantic_animal)

dataset_m_valid_tot_d <- dataset_vft_lmv %>%
  distinct(letter_m)

dataset_s_valid_tot_d <- dataset_vft_lsv %>%
  distinct(letter_s)

# Count unique responses for each participant

dataset_vft_sfv_c <- dataset_vft_sfv %>%
  group_by(participant_number) %>%
  summarise(sfv_n = n_distinct(semantic_fruit)) %>%
  ungroup()

dataset_vft_sav_c <- dataset_vft_sav %>%
  group_by(participant_number) %>%
  summarise(sav_n = n_distinct(semantic_animal)) %>%
  ungroup()

dataset_vft_lmv_c <- dataset_vft_lmv %>%
  group_by(participant_number) %>%
  summarise(lmv_n = n_distinct(letter_m)) %>%
  ungroup()

dataset_vft_lsv_c <- dataset_vft_lsv %>%
  group_by(participant_number) %>%
  summarise(lsv_n = n_distinct(letter_s)) %>%
  ungroup()

dataset_vft_sfp_c <- dataset_vft_sfp %>%
  group_by(participant_number) %>%
  summarise(sfp_n = n_distinct(semantic_fruit)) %>%
  ungroup()

dataset_vft_sap_c <- dataset_vft_sap %>%
  group_by(participant_number) %>%
  summarise(sap_n = n_distinct(semantic_animal)) %>%
  ungroup()

# Calculate atypicality score

dataset_vft_sfv_uc <- dataset_vft_sfv %>%
  ungroup() %>%
  group_by(semantic_fruit) %>%
  select(semantic_fruit) %>%
  summarise(sfv_uc = length(semantic_fruit)) %>%
  ungroup() %>%
  mutate(across(sfv_uc, as.numeric)) %>%
  arrange(desc(sfv_uc)) %>%
  mutate(atypicality_fv = dense_rank(desc(sfv_uc))) %>%
  ungroup()

dataset_vft_sfv <- dataset_vft_sfv %>%
  full_join(dataset_vft_sfv_uc)

dataset_vft_sfv_afvt <-  dataset_vft_sfv %>%
  group_by(participant_number) %>%
  summarise(afvt = sum(atypicality_fv)) %>%
  select(participant_number, afvt) %>%
  ungroup()

dataset_vft_sfv_afvt_c <- dataset_vft_sfv_afvt %>%
  full_join(dataset_vft_sfv_c)

dataset_vft_sfv_afvt_c <- dataset_vft_sfv_afvt_c %>%
  mutate(afvs = (afvt / sfv_n))

####

dataset_vft_sav_uc <- dataset_vft_sav %>%
  ungroup() %>%
  group_by(semantic_animal) %>%
  select(semantic_animal) %>%
  summarise(sav_uc = length(semantic_animal)) %>%
  ungroup() %>%
  mutate(across(sav_uc, as.numeric)) %>%
  arrange(desc(sav_uc)) %>%
  mutate(atypicality_av = dense_rank(desc(sav_uc))) %>%
  ungroup()

dataset_vft_sav <- dataset_vft_sav %>%
  full_join(dataset_vft_sav_uc)

dataset_vft_sav_aavt <-  dataset_vft_sav %>%
  group_by(participant_number) %>%
  summarise(aavt = sum(atypicality_av)) %>%
  select(participant_number, aavt) %>%
  ungroup()

dataset_vft_sav_aavt_c <- dataset_vft_sav_aavt %>%
  full_join(dataset_vft_sav_c)

dataset_vft_sav_aavt_c <- dataset_vft_sav_aavt_c %>%
  mutate(aavs = (aavt / sav_n))

####

dataset_vft_lmv_uc <- dataset_vft_lmv %>%
  ungroup() %>%
  group_by(letter_m) %>%
  select(letter_m) %>%
  summarise(lmv_uc = length(letter_m)) %>%
  ungroup() %>%
  mutate(across(lmv_uc, as.numeric)) %>%
  arrange(desc(lmv_uc)) %>%
  mutate(atypicality_mv = dense_rank(desc(lmv_uc))) %>%
  ungroup()

dataset_vft_lmv <- dataset_vft_lmv %>%
  full_join(dataset_vft_lmv_uc)

dataset_vft_lmv_amvt <-  dataset_vft_lmv %>%
  group_by(participant_number) %>%
  summarise(amvt = sum(atypicality_mv)) %>%
  select(participant_number, amvt) %>%
  ungroup()

dataset_vft_lmv_amvt_c <- dataset_vft_lmv_amvt %>%
  full_join(dataset_vft_lmv_c)

dataset_vft_lmv_amvt_c <- dataset_vft_lmv_amvt_c %>%
  mutate(amvs = (amvt / lmv_n))

####

dataset_vft_lsv_uc <- dataset_vft_lsv %>%
  ungroup() %>%
  group_by(letter_s) %>%
  select(letter_s) %>%
  summarise(lsv_uc = length(letter_s)) %>%
  ungroup() %>%
  mutate(across(lsv_uc, as.numeric)) %>%
  arrange(desc(lsv_uc)) %>%
  mutate(atypicality_sv = dense_rank(desc(lsv_uc))) %>%
  ungroup()

dataset_vft_lsv <- dataset_vft_lsv %>%
  full_join(dataset_vft_lsv_uc)

dataset_vft_lsv_asvt <-  dataset_vft_lsv %>%
  group_by(participant_number) %>%
  summarise(asvt = sum(atypicality_sv)) %>%
  select(participant_number, asvt) %>%
  ungroup()

dataset_vft_lsv_asvt_c <- dataset_vft_lsv_asvt %>%
  full_join(dataset_vft_lsv_c)

dataset_vft_lsv_asvt_c <- dataset_vft_lsv_asvt_c %>%
  mutate(asvs = (asvt / lsv_n))

# Join verbal fluency tasks datasets

dataset_vft_final <- dataset_vft_lmv_amvt_c %>%
  full_join(dataset_vft_sav_aavt_c)

dataset_vft_final <- dataset_vft_final %>%
  full_join(dataset_vft_lmv_amvt_c)

dataset_vft_final <- dataset_vft_final %>%
  full_join(dataset_vft_lsv_asvt_c)

dataset_vft_final <- dataset_vft_final %>%
  full_join(dataset_vft_sfp_c)

dataset_vft_final <- dataset_vft_final %>%
  full_join(dataset_vft_sap_c)

dataset_vft_final <- dataset_vft_final %>%
  full_join(dataset_vft_sfv_afvt_c)

dataset_vft_final <- dataset_vft_final %>%
  select(participant_number, sfv_n, afvs, sfp_n, sav_n, aavs, sap_n, lmv_n, amvs,lsv_n, asvs)

# Join main datasets

dataset_master <- dataset_final %>%
  full_join(dataset_vft_final)

dataset_master <- dataset_master %>%
  rename(fruit_valid_score = sfv_n) %>%
  rename(fruit_atypicality_score = afvs) %>%
  rename(fruit_productivity_score = sfp_n) %>%
  rename(animal_valid_score = sav_n) %>%
  rename(animal_atypicality_score = aavs) %>%
  rename(animal_productivity_score = sap_n) %>%
  rename(m_valid_score = lmv_n) %>%
  rename(m_atypicality_score = amvs) %>%
  rename(s_valid_score = lsv_n) %>%
  rename(s_atypicality_score = asvs)

# Change to appropriate class

dataset_master <- dataset_master %>%
  mutate(across(c(fruit_valid_score, fruit_productivity_score, animal_valid_score, animal_productivity_score, m_valid_score,
                  s_valid_score), as.numeric))

# Create z score variables

dataset_master <- dataset_master %>%
  mutate(fruit_valid_score_z = scale(fruit_valid_score), fruit_atypicality_score_z = scale(fruit_atypicality_score),
         fruit_productivity_score_z = scale(fruit_productivity_score), animal_valid_score_z = scale(animal_valid_score),
         animal_atypicality_score_z = scale(animal_atypicality_score), animal_productivity_score_z = scale(animal_productivity_score),
         m_valid_score_z = scale(m_valid_score), m_atypicality_score_z = scale(m_atypicality_score),
         s_valid_score_z = scale(s_valid_score), s_atypicality_score_z = scale(s_atypicality_score))

dataset_master <- dataset_master %>%
  mutate(across(c(fruit_valid_score_z, fruit_atypicality_score_z, fruit_productivity_score_z, animal_valid_score_z,
                  animal_atypicality_score_z, animal_productivity_score_z, m_valid_score_z, m_atypicality_score_z, s_valid_score_z,
                  s_atypicality_score_z), as.numeric))

# Calculate average verbal fluency task scores

dataset_master <- dataset_master %>%
  mutate(semantic_valid_score_z = ((fruit_valid_score_z + animal_valid_score_z) / 2)) %>%
  mutate(semantic_atypicality_score_z = ((fruit_atypicality_score_z + animal_atypicality_score_z) / 2)) %>%
  mutate(semantic_productivity_score_z = ((fruit_productivity_score_z + animal_productivity_score_z) / 2)) %>%
  mutate(letter_valid_score_z = ((m_valid_score_z + s_valid_score_z) / 2)) %>%
  mutate(letter_atypicality_score_z = ((m_atypicality_score_z + s_atypicality_score_z) / 2))

# Convert z score to numeric

dataset_master <- dataset_master %>%
  mutate(across(c(semantic_valid_score_z, semantic_atypicality_score_z, semantic_productivity_score_z, letter_valid_score_z,
                  letter_atypicality_score_z), as.numeric))

# Prepare correlation datasets

dataset_master <- dataset_master %>%
  mutate(sex_b = sex)

dataset_master <- dataset_master %>%
  mutate(sex_b = dplyr::recode(sex_b, "male" = 0, "female" = 1))

dataset_master <- dataset_master %>%
  mutate(english_b = english)

dataset_master <- dataset_master %>%
  mutate(english_b = dplyr::recode(english_b, "english is native language" = 0, "english is not native language" = 1))

# Transform reading comprehension task score

dataset_master %>%
  shapiro_test(rct_score)

dataset_master <- dataset_master %>%
  mutate(rct_score_t = rct_score ** 2)

dataset_master %>%
  shapiro_test(rct_score_t)

# Create dataset for stated whether English was native language

dataset_master_english <- dataset_master %>%
  filter(english == "english is not native language" | english == "english is native language")

# Create dummy column for english prefer not to say

dataset_master <- dataset_master %>%
  mutate(english_prefer_not_to_say = case_when(english == "english is native language" ~ "0", english == "english is not native language" ~ "0",
                                               english == "prefer not to say" ~ "1"))

dataset_master <- dataset_master %>%
  mutate(english_is = case_when(english == "english is native language" ~ "1", english == "english is not native language" ~ "0",
                                english == "prefer not to say" ~ "0"))

####### Dessciptives #######

summary(dataset_master)

describe(dataset_master)

# Descriptives for schizotypal questionnaire, female

dataset_master_female <- dataset_master %>%
  filter(sex == "female")

summary(dataset_master_female)

describe(dataset_master_female)

# Descriptives for schizotypal questionnaire, male

dataset_master_male <- dataset_master %>%
  filter(sex == "male")

summary(dataset_master_male)

describe(dataset_master_male)

# Descriptives for schizotypal questionnaire, all participants

summary(dataset_master)

describe(dataset_master)

# Means, standard deviations, and ranges

summary(dataset_master)

describe(dataset_master)

dataset_master %>%
  filter(spq_interpersonal_group == "low") %>%
  describe()

dataset_master %>%
  filter(spq_interpersonal_group == "medium") %>%
  describe()

dataset_master %>%
  filter(spq_interpersonal_group == "high") %>%
  describe()

dataset_master %>%
  filter(spq_cognitive_perceptual_group == "low") %>%
  describe()

dataset_master %>%
  filter(spq_cognitive_perceptual_group == "medium") %>%
  describe()

dataset_master %>%
  filter(spq_cognitive_perceptual_group == "high") %>%
  describe()

dataset_master %>%
  filter(spq_disorganised_group == "low") %>%
  describe()

dataset_master %>%
  filter(spq_disorganised_group == "medium") %>%
  describe()

dataset_master %>%
  filter(spq_disorganised_group == "high") %>%
  describe()

dataset_master %>%
  filter(spq_social_anxiety_group == "low") %>%
  describe()

dataset_master %>%
  filter(spq_social_anxiety_group == "medium") %>%
  describe()

dataset_master %>%
  filter(spq_social_anxiety_group == "high") %>%
  describe()

dataset_master_sex <- dataset_master %>%
  group_by(sex) %>%
  summarise(mean_age = mean(age), sd_age = sd(age), range__min_age = min(range(age)), range_max_age = max(range(age))) %>%
  ungroup()

dataset_master %>%
  group_by(sex) %>%
  summarise(mean(rct_score)) %>%
  mutate_if(is.numeric, format, 1) %>%
  ungroup()

# Check for outliers

dataset_master %>%
  select(age, sex, english, rct_score, spq_total, spq_interpersonal, spq_cognitive_perceptual, spq_disorganised, spq_social_anxiety) %>%
  boxplot()

dataset_master %>%
  select(semantic_valid_score_z, semantic_atypicality_score_z, semantic_productivity_score_z, letter_valid_score_z,
         letter_atypicality_score_z)  %>%
  boxplot()

# Check normality assumptions

# Schizotypal personality sub-dimensions

plot_interpersonal <- dataset_master %>%
  ggplot(aes(x = spq_interpersonal)) +
  geom_histogram(binwidth = 1, colour = "black") +
  theme_bw() +
  labs(x = "Interpersonal", y = "Frequency")

plot_interpersonal

plot_cognitive_perceptual <- dataset_master %>%
  ggplot(aes(x = spq_cognitive_perceptual)) +
  geom_histogram(binwidth = 1, colour = "black") +
  theme_bw() +
  labs(x = "Cognitive Perceptual", y = "Frequency")

plot_cognitive_perceptual

plot_disorganised <- dataset_master %>%
  ggplot(aes(x = spq_disorganised)) +
  geom_histogram(binwidth = 1, colour = "black") +
  theme_bw() +
  labs(x = "Disorganised", y = "Frequency")

plot_disorganised

plot_social_anxiety <- dataset_master %>%
  ggplot(aes(x = spq_social_anxiety)) +
  geom_histogram(binwidth = 1, colour = "black") +
  theme_bw() +
  labs(x = "Social Anxiety", y = "Frequency")

plot_social_anxiety

ga_plot_spq_distrib <- grid.arrange(plot_interpersonal, plot_cognitive_perceptual, plot_disorganised,
                                        plot_social_anxiety, ncol = 2)

ggsave("ga_plot_spq_distrib.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw', ga_plot_spq_distrib)

ggsave("plot_social_anxiety.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw', plot_social_anxiety)

dataset_master %>%
  shapiro_test(spq_interpersonal)

dataset_master %>%
  shapiro_test(spq_cognitive_perceptual)

dataset_master %>%
  shapiro_test(spq_disorganised)

dataset_master %>%
  shapiro_test(spq_social_anxiety)

# Semantic fluency task valid

plot_semantic_valid_score_z <- dataset_master %>%
  ggplot(aes(x = semantic_valid_score_z)) +
  geom_histogram(binwidth = 0.1, colour = "black") +
  theme_bw() +
  labs(x = "Semantic Task Valid Z Score", y = "Frequency")

plot_semantic_valid_score_z

ggsave("plot_semantic_valid_score_z.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw', plot_semantic_valid_score_z)

dataset_master %>%
  shapiro_test(semantic_valid_score_z)

# Semantic fluency atypicality

plot_semantic_atypicality_score_z <- dataset_master %>%
  ggplot(aes(x = semantic_atypicality_score_z)) +
  geom_histogram(binwidth = 0.1, colour = "black") +
  theme_bw() +
  labs(x = "Semantic Task Atypicality Z Score", y = "Frequency")

plot_semantic_atypicality_score_z

ggsave("plot_semantic_atypicality_score_z.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw',
       plot_semantic_atypicality_score_z)

dataset_master %>%
  shapiro_test(semantic_atypicality_score_z)

# Semantic fluency productivity

plot_semantic_productivity_score_z <- dataset_master %>%
  ggplot(aes(x = semantic_productivity_score_z)) +
  geom_histogram(binwidth = 0.1, colour = "black") +
  theme_bw() +
  labs(x = "Semantic Task Productivity Z Score", y = "Frequency")

plot_semantic_productivity_score_z

ggsave("plot_semantic_productivity_score_z.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw',
       plot_semantic_productivity_score_z)

dataset_master %>%
  shapiro_test(semantic_productivity_score_z)

# Letter fluency task valid

plot_letter_score_z <- dataset_master %>%
  ggplot(aes(x = letter_valid_score_z)) +
  geom_histogram(binwidth = 0.1, colour = "black") +
  theme_bw() +
  labs(x = "Letter Task Valid Z Score", y = "Frequency")

plot_letter_score_z

ggsave("plot_letter_score_z.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw', plot_letter_score_z)

dataset_master %>%
  shapiro_test(letter_valid_score_z)

# Letter fluency atypicality

plot_letter_atypicality_score_z <- dataset_master %>%
  ggplot(aes(x = letter_atypicality_score_z)) +
  geom_histogram(binwidth = 0.1, colour = "black") +
  theme_bw() +
  labs(x = "Letter Task Atypicality Z Score", y = "Frequency")

plot_letter_atypicality_score_z

ggsave("plot_letter_atypicality_score_z.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw',
       plot_letter_atypicality_score_z)

dataset_master %>%
  shapiro_test(letter_atypicality_score_z)

# Reading comprehension task

plot_rct_score_distrib <- dataset_master %>%
  ggplot(aes(x = rct_score)) +
  geom_histogram(binwidth = 1, colour = "black") +
  theme_bw() +
  labs(x = "Deep Cloze Test Score", y = "Frequency")

plot_rct_score_distrib

ggsave("plot_rct_score_distrib.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw', plot_rct_score_distrib)

dataset_master %>%
  shapiro_test(rct_score)

# Reading comprehension task squared transformation

plot_rct_score_t_distrib <- dataset_master %>%
  ggplot(aes(x = rct_score_t)) +
  geom_histogram(binwidth = 25, colour = "black") +
  theme_bw() +
  labs(x = "Deep Cloze Test Score Squared Transformation", y = "Frequency")

plot_rct_score_t_distrib

ggsave("plot_rct_score_t_distrib.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw', plot_rct_score_t_distrib)

dataset_master %>%
  shapiro_test(rct_score_t)

# Age

plot_age_distrib <- dataset_master %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 1, colour = "black") +
  theme_bw() +
  labs(x = "Age", y = "Frequency")

plot_age_distrib

ggsave("plot_age_distrib.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw', plot_age_distrib)

dataset_master %>%
  shapiro_test(age)

# English

plot_english_distrib <- dataset_master %>%
  ggplot(aes(x = english)) +
  geom_bar(colour = "black") +
  theme_bw() +
  labs(x = "English as Native Language", y = "Frequency")

plot_english_distrib

ggsave("plot_english_distrib.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw', plot_english_distrib)

# Sex

plot_sex_distrib <- dataset_master %>%
  ggplot(aes(x = sex)) +
  geom_bar(colour = "black") +
  theme_bw() +
  labs(x = "Biological Sex", y = "Frequency")

plot_sex_distrib

ggsave("plot_sex_distrib.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw', plot_sex_distrib)

# Shapiro Wilk Tests

dataset_master %>%
  shapiro_test(fruit_valid_score)

dataset_master %>%
  shapiro_test(animal_valid_score)

dataset_master %>%
  shapiro_test(m_valid_score)

dataset_master %>%
  shapiro_test(s_valid_score)

dataset_master %>%
  shapiro_test(fruit_atypicality_score)

dataset_master %>%
  shapiro_test(animal_atypicality_score)

dataset_master %>%
  shapiro_test(m_atypicality_score)

# Semantic fluency task valid

plot_m_atypicality_score <- dataset_master %>%
  ggplot(aes(x = m_atypicality_score)) +
  geom_histogram(binwidth = 0.1, colour = "black") +
  theme_bw() +
  labs(x = "Letter M Atypicality Score", y = "Frequency")

plot_m_atypicality_score

ggsave("plot_m_atypicality_score.tiff", units="in", width=13, height=9, dpi=300, compression = 'lzw', plot_m_atypicality_score)

dataset_master %>%
  shapiro_test(s_atypicality_score)

dataset_master %>%
  shapiro_test(fruit_productivity_score)

dataset_master %>%
  shapiro_test(animal_productivity_score)

# Calculate Cronbachs alpha for the reading comprehension task, and the schizotypal personality questionnaire

dataset_select_rct_validity <- dataset_select %>%
  select(rct_01:rct_32)

alpha(dataset_select_rct_validity)

dataset_select_spq_validity <- dataset_select %>%
  select(spq_01_cp_ir_1:spq_32_cp_up_1)

alpha(dataset_select_spq_validity)

dataset_select_interpersonal_validity <- dataset_select %>%
  select(spq_07_ip_cf_1, spq_08_ip_cf_1, spq_09_ip_cf_1, spq_10_ip_ca_1, spq_11_ip_ca_1,spq_12_ip_ca_1)

alpha(dataset_select_interpersonal_validity)

dataset_select_cognitive_perceptual_validity <- dataset_select %>%
  select(spq_01_cp_ir_1, spq_02_cp_ir_1, spq_03_cp_ir_1,  spq_04_cp_su_1, spq_05_cp_su_1, spq_06_cp_su_1, spq_21_cp_mt_1,
         spq_22_cp_mt_1, spq_23_cp_mt_1, spq_24_cp_mt_1, spq_29_cp_up_1, spq_30_cp_up_1, spq_31_cp_up_1, spq_32_cp_up_1)

alpha(dataset_select_cognitive_perceptual_validity)

dataset_select_disorganised_validity <- dataset_select %>%
  select(spq_13_do_eb_1, spq_14_do_eb_1, spq_15_do_eb_1, spq_16_do_eb_1, spq_25_do_os_1, spq_26_do_os_1, spq_27_do_os_1,
         spq_28_do_os_1)

alpha(dataset_select_disorganised_validity)

dataset_select_social_anxiety_validity <- dataset_select %>%
  select(spq_17_ip_sa_1, spq_18_ip_sa_1, spq_19_ip_sa_1, spq_20_ip_sa_1)

alpha(dataset_select_social_anxiety_validity)

# Conduct a confirmatory factor analysis to check the schizotypal personality sub dimensions exist for the dataset created by the
# present study

# Describe the schizotypal personality questionnaire items

dataset_select_spq <- dataset_select %>%
  select(spq_01_cp_ir_1:spq_32_cp_up_1)

describe(dataset_select_spq)

plot_long_spq

mardia(dataset_select_spq)

# Specify confirmatory factor analysis model

schizotypal_cfa_model <- "interpersonal =~ spq_07_ip_cf_1 + spq_08_ip_cf_1 + spq_09_ip_cf_1 + spq_10_ip_ca_1 + spq_11_ip_ca_1 +
spq_12_ip_ca_1
cognitive_perceptual =~ spq_01_cp_ir_1 + spq_02_cp_ir_1 + spq_03_cp_ir_1 + spq_04_cp_su_1 + spq_05_cp_su_1 + spq_06_cp_su_1 +
spq_21_cp_mt_1 + spq_22_cp_mt_1 + spq_23_cp_mt_1 + spq_24_cp_mt_1 + spq_29_cp_up_1 + spq_30_cp_up_1 + spq_31_cp_up_1 +
spq_32_cp_up_1
disorganised =~ spq_13_do_eb_1 + spq_14_do_eb_1 + spq_15_do_eb_1 + spq_16_do_eb_1 + spq_25_do_os_1 + spq_26_do_os_1 +
spq_27_do_os_1 + spq_28_do_os_1
social_anxiety =~ spq_17_ip_sa_1 + spq_18_ip_sa_1 + spq_19_ip_sa_1 + spq_20_ip_sa_1"

# Run the confirmatory factor analysis

cfa_result_schizotypal <- cfa(schizotypal_cfa_model, estimator = "MLR", data = dataset_select)

fitMeasures(cfa_result_schizotypal, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust", "tli.robust", "rmsea.robust"))

summary(cfa_result_schizotypal, standardized = TRUE, ci = TRUE, fit.measures = TRUE)

modindices(cfa_result_schizotypal) %>%
  arrange(-mi)

semTools::reliability(cfa_result_schizotypal)

# Draw a plot showing loadings

cft_spq_plot <- lavaanPlot(model = cfa_result_schizotypal, node_options = list(shape = "box", fontname = "Arial"),
                           edge_options = list(colour = "grey"), coefs = T, stand = T)

cft_spq_plot

save_png(cft_spq_plot, "cft_spq_plt.png", width = 7000)

# Score differences

# Semantic valid score

dataset_fruit_animal_valid <- dataset_master %>%
  select(fruit_valid_score, animal_valid_score) %>%
  pivot_longer(fruit_valid_score:animal_valid_score, names_to = "category", values_to = "score")

dataset_fruit_animal_valid <- dataset_fruit_animal_valid %>%
  mutate(across(c(category), as.factor)) %>%
  mutate(category = dplyr::recode(category, "fruit_valid_score" = "fruit", "animal_valid_score" = "animal"))

dataset_fruit_animal_valid$category <- factor(dataset_fruit_animal_valid$category,
                                                levels=c("fruit", "animal"))

fruit_animal_valid_difference_plot <- dataset_fruit_animal_valid %>%
  ggplot(aes(x = category, y = score)) +
  geom_point(colour = "black", position = position_jitter(width = 0.1), alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "black", width = 0.2, alpha = 1, size = 1) +
  stat_summary(fun = mean, geom = "point", colour = "black", alpha = 1, size = 3) +
  theme_bw() +
  labs(y = "Semantic Valid Score", x = "Category", title="(A)") +
  theme(legend.position = "none",
        axis.text = element_text(size = rel(0.7)),
        axis.title = element_text(size = rel(0.7)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

fruit_animal_valid_difference_plot

t.test(score ~ category, data = dataset_fruit_animal_valid)

# Letter valid score

dataset_m_s_valid <- dataset_master %>%
  select(m_valid_score, s_valid_score) %>%
  pivot_longer(m_valid_score:s_valid_score, names_to = "letter", values_to = "score")

dataset_m_s_valid <- dataset_m_s_valid %>%
  mutate(across(c(letter), as.factor)) %>%
  mutate(letter = dplyr::recode(letter, "m_valid_score" = "m", "s_valid_score" = "s"))

dataset_m_s_valid$letter <- factor(dataset_m_s_valid$letter,
                                        levels=c("m", "s"))

m_s_valid_difference_plot <- dataset_m_s_valid %>%
  ggplot(aes(x = letter, y = score)) +
  geom_point(colour = "black", position = position_jitter(width = 0.1), alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "black", width = 0.2, alpha = 1, size = 1) +
  stat_summary(fun = mean, geom = "point", colour = "black", alpha = 1, size = 3) +
  theme_bw() +
  labs(y = "Phonological Valid Score", x = "Letter", title = "(B)") +
  theme(legend.position = "none",
        axis.text = element_text(size = rel(0.7)),
        axis.title = element_text(size = rel(0.7)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

m_s_valid_difference_plot

t.test(score ~ letter, data = dataset_m_s_valid)

# Semantic atypicality score

dataset_fruit_animal_atypicality <- dataset_master %>%
  select(fruit_atypicality_score, animal_atypicality_score) %>%
  pivot_longer(fruit_atypicality_score:animal_atypicality_score, names_to = "category", values_to = "score")

dataset_fruit_animal_atypicality <- dataset_fruit_animal_atypicality %>%
  mutate(across(c(category), as.factor)) %>%
  mutate(category = dplyr::recode(category, "fruit_atypicality_score" = "fruit", "animal_atypicality_score" = "animal"))

dataset_fruit_animal_atypicality$category <- factor(dataset_fruit_animal_atypicality$category,
                                   levels=c("fruit", "animal"))

fruit_animal_atypicality_difference_plot <- dataset_fruit_animal_atypicality %>%
  ggplot(aes(x = category, y = score)) +
  geom_point(colour = "black", position = position_jitter(width = 0.1), alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "black", width = 0.2, alpha = 1, size = 1) +
  stat_summary(fun = mean, geom = "point", colour = "black", alpha = 1, size = 3) +
  theme_bw() +
  labs(y = "Semantic Atypicality Score", x = "Category", title = "(C)") +
  theme(legend.position = "none",
        axis.text = element_text(size = rel(0.7)),
        axis.title = element_text(size = rel(0.7)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

fruit_animal_atypicality_difference_plot

t.test(score ~ category, data = dataset_fruit_animal_atypicality)

# Letter atypicality score

dataset_m_s_atypicality <- dataset_master %>%
  select(m_atypicality_score, s_atypicality_score) %>%
  pivot_longer(m_atypicality_score:s_atypicality_score, names_to = "letter", values_to = "score")

dataset_m_s_atypicality <- dataset_m_s_atypicality %>%
  mutate(across(c(letter), as.factor)) %>%
  mutate(letter = dplyr::recode(letter, "m_atypicality_score" = "m", "s_atypicality_score" = "s"))

dataset_m_s_atypicality$letter <- factor(dataset_m_s_atypicality$letter,
                                                    levels=c("m", "s"))

m_s_atypicality_difference_plot <- dataset_m_s_atypicality %>%
  ggplot(aes(x = letter, y = score)) +
  geom_point(colour = "black", position = position_jitter(width = 0.1), alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "black", width = 0.2, alpha = 1, size = 1) +
  stat_summary(fun = mean, geom = "point", colour = "black", alpha = 1, size = 3) +
  theme_bw() +
  labs(y = "Phonological Atypicality Score", x = "Letter", title = "(D)") +
  theme(legend.position = "none",
        axis.text = element_text(size = rel(0.7)),
        axis.title = element_text(size = rel(0.7)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

m_s_atypicality_difference_plot

t.test(score ~ letter, data = dataset_m_s_atypicality)

# Semantic productivity score

dataset_fruit_aniaml_productivity <- dataset_master %>%
  select(fruit_productivity_score, animal_productivity_score) %>%
  pivot_longer(fruit_productivity_score:animal_productivity_score, names_to = "category", values_to = "score")

dataset_fruit_aniaml_productivity <- dataset_fruit_aniaml_productivity %>%
  mutate(across(c(category), as.factor)) %>%
  mutate(category = dplyr::recode(category, "fruit_productivity_score" = "fruit", "animal_productivity_score" = "animal"))

dataset_fruit_aniaml_productivity$category <- factor(dataset_fruit_aniaml_productivity$category,
                                         levels=c("fruit", "animal"))

fruit_aniaml_productivity_difference_plot <- dataset_fruit_aniaml_productivity %>%
  ggplot(aes(x = category, y = score)) +
  geom_point(colour = "black", position = position_jitter(width = 0.1), alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "black", width = 0.2, alpha = 1, size = 1) +
  stat_summary(fun = mean, geom = "point", colour = "black", alpha = 1, size = 3) +
  theme_bw() +
  labs(y = "Semantic Productivity Score", x = "Category", title = "(E)") +
  theme(legend.position = "none",
        axis.text = element_text(size = rel(0.7)),
        axis.title = element_text(size = rel(0.7)),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

fruit_aniaml_productivity_difference_plot

t.test(score ~ category, data = dataset_fruit_aniaml_productivity)

ga_plot_task_score_differences <- grid.arrange(fruit_animal_valid_difference_plot, m_s_valid_difference_plot,
                                               fruit_animal_atypicality_difference_plot, m_s_atypicality_difference_plot,
                                               fruit_aniaml_productivity_difference_plot, ncol = 2)

ggsave("ga_plot_task_score_differences.tiff", units="in", width=10, height=9.5, dpi=300, compression = 'lzw',
       ga_plot_task_score_differences)

# Conduct correlations

dataset_master %>%
  cor_test(spq_interpersonal, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(spq_cognitive_perceptual, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(spq_disorganised, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(spq_social_anxiety, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(semantic_valid_score_z, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(semantic_atypicality_score_z, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(semantic_productivity_score_z, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(letter_valid_score_z, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(letter_atypicality_score_z, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(rct_score_t, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(english_b, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(age, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(sex_b, method = "pearson") %>%
  print(n = nrow(.))

dataset_master %>%
  cor_test(spq_total, method = "pearson") %>%
  print(n = nrow(.))

# Plot meaningful correlations

plot_sem_atypicality_disorganised_cor <- dataset_master %>%
  ggplot(aes(x = spq_disorganised, y = semantic_atypicality_score_z)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth(size = 1.5, colour = "red", method = "lm", se = TRUE) +
  geom_label(label="r = 0.26", label.size = 0, colour = "red", x = 37, y = -1.5) +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(0.7)),
    axis.title = element_text(size = rel(0.7)),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  ) +
  labs(x = 'Disorganised Schizotypal Trait', y = 'Semantic Atypicality Score', title = "(A)")

plot_sem_atypicality_disorganised_cor_md <- ggMarginal(plot_sem_atypicality_disorganised_cor, type = "histogram", colour = "lightgrey",
                                                       fill = "lightgrey")

plot_sem_atypicality_disorganised_cor_md

plot_letter_valid_interpersonal_cor <- dataset_master %>%
  ggplot(aes(x = spq_interpersonal, y = letter_valid_score_z)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth(size = 1.5, colour = "red", method = "lm", se = TRUE) +
  geom_label(label="r = -0.28", label.size = 0, colour = "red", x = 25, y = 2) +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(0.7)),
    axis.title = element_text(size = rel(0.7)),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  ) +
  labs(x = 'Interpersonal Scizotypal Trait', y = 'Phonological Valid Score', title = "(B)")

plot_letter_valid_interpersonal_cor_md <- ggMarginal(plot_letter_valid_interpersonal_cor, type = "histogram", colour = "lightgrey",
                                                     fill = "lightgrey")

plot_letter_valid_interpersonal_cor_md

plot_letter_valid_disorganised_cor <- dataset_master %>%
  ggplot(aes(x = spq_disorganised, y = letter_valid_score_z)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth(size = 1.5, colour = "red", method = "lm", se = TRUE) +
  geom_label(label="r = 0.26", label.size = 0, colour = "red", x = 35, y = 2) +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(0.7)),
    axis.title = element_text(size = rel(0.7)),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  ) +
  labs(x = 'Disorganised Schizotypal Trait', y = 'Phonological Valid Score', title = "(C)")

plot_letter_valid_disorganised_cor_md <- ggMarginal(plot_letter_valid_disorganised_cor, type = "histogram", colour = "lightgrey",
                                              fill = "lightgrey")

plot_letter_valid_disorganised_cor_md

plot_letter_valid_social_anxiety_cor <- dataset_master %>%
  ggplot(aes(x = spq_social_anxiety, y = letter_valid_score_z)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth(size = 1.5, colour = "red", method = "lm", se = TRUE) +
  geom_label(label="r = -0.35", label.size = 0, colour = "red", x = 17, y = 2) +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(0.7)),
    axis.title = element_text(size = rel(0.7)),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  ) +
  labs(x = 'Social Anxiety Schizotypal Trait', y = 'Phonological Valid Score', title = "(D)")

plot_letter_valid_social_anxiety_cor_md <- ggMarginal(plot_letter_valid_social_anxiety_cor, type = "histogram", colour = "lightgrey",
                                                      fill = "lightgrey")

plot_letter_valid_social_anxiety_cor_md

plot_letter_atypicality_disorganised_cor <- dataset_master %>%
  ggplot(aes(x = spq_disorganised, y = letter_atypicality_score_z)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth(size = 1.5, colour = "red", method = "lm", se = TRUE) +
  geom_label(label="r = 0.22", label.size = 0, colour = "red", x = 35, y = -1.9) +
  theme_bw() +
  theme(
    axis.text = element_text(size = rel(0.7)),
    axis.title = element_text(size = rel(0.7)),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  ) +
  labs(x = 'Disorganised Schizotypal Trait', y = 'Phonological Atypicality Score', title = "(E)")

plot_letter_atypicality_disorganised_cor_md <- ggMarginal(plot_letter_atypicality_disorganised_cor, type = "histogram", colour = "lightgrey",
                                                    fill = "lightgrey")

plot_letter_atypicality_disorganised_cor_md

ga_practically_significant_correlations <- grid.arrange(plot_sem_atypicality_disorganised_cor_md, plot_letter_valid_interpersonal_cor_md,
                                                        plot_letter_valid_disorganised_cor_md, plot_letter_valid_social_anxiety_cor_md,
                                                        plot_letter_atypicality_disorganised_cor_md, ncol = 2)

ggsave("ga_practically_significant_correlations.tiff", units="in", width=10, height=10, dpi=300, compression = 'lzw',
       ga_practically_significant_correlations)

# Run a linear regression with semantic_atypicality_score_z as the outcome, and sex, age, and english as native language as the covariates,
# and spq_disorganised as the predictor

lm_semantic_atypiciality_disorganised_english <- lm(semantic_atypicality_score_z ~ sex + age + english + spq_disorganised,
                                                    data = dataset_master_english)

summary(lm_semantic_atypiciality_disorganised_english)

ols_plot_resid_qq(lm_semantic_atypiciality_disorganised_english)

shapiro_test(lm_semantic_atypiciality_disorganised_english$residuals)

residualPlot(lm_semantic_atypiciality_disorganised_english)

vif(lm_semantic_atypiciality_disorganised_english)

# Compare linear models with letter_valid_score as the outcome, and sex, age, and english as native language as the covariates, and combinations
# of spq_interpersonal, spq_disorganised, and spq_social_anxiety

lm_letter_valid_english <- lm(letter_valid_score_z ~ sex + age + english, data = dataset_master_english)

summary(lm_letter_valid_english)

glance(lm_letter_valid_english)

ols_plot_resid_qq(lm_letter_valid_english)

shapiro_test(lm_letter_valid_english$residuals)

residualPlot(lm_letter_valid_english)

vif(lm_letter_valid_english)

####

lm_letter_valid_interpersonal_english <- lm(letter_valid_score_z ~ sex + age + english + spq_interpersonal, data = dataset_master_english)

summary(lm_letter_valid_interpersonal_english)

glance(lm_letter_valid_interpersonal_english)

ols_plot_resid_qq(lm_letter_valid_interpersonal_english)

shapiro_test(lm_letter_valid_interpersonal_english$residuals)

residualPlot(lm_letter_valid_interpersonal_english)

vif(lm_letter_valid_interpersonal_english)

####

lm_letter_valid_disorganised_english <- lm(letter_valid_score_z ~ sex + age + english +spq_disorganised, data = dataset_master_english)

summary(lm_letter_valid_disorganised_english)

glance(lm_letter_valid_disorganised_english)

ols_plot_resid_qq(lm_letter_valid_disorganised_english)

shapiro_test(lm_letter_valid_disorganised_english$residuals)

residualPlot(lm_letter_valid_disorganised_english)

vif(lm_letter_valid_disorganised_english)

####

lm_letter_valid_social_anxiety_english <- lm(letter_valid_score_z ~ sex + age + english + spq_social_anxiety, data = dataset_master_english)

summary(lm_letter_valid_social_anxiety_english)

glance(lm_letter_valid_social_anxiety_english)

ols_plot_resid_qq(lm_letter_valid_social_anxiety_english)

shapiro_test(lm_letter_valid_social_anxiety_english$residuals)

residualPlot(lm_letter_valid_social_anxiety_english)

vif(lm_letter_valid_social_anxiety_english)

####

lm_letter_valid_interpersonal_disorganised_english <- lm(letter_valid_score_z ~ sex + age + english + spq_interpersonal + spq_disorganised,
                                                         data = dataset_master_english)

summary(lm_letter_valid_interpersonal_disorganised_english)

glance(lm_letter_valid_interpersonal_disorganised_english)

ols_plot_resid_qq(lm_letter_valid_interpersonal_disorganised_english)

shapiro_test(lm_letter_valid_interpersonal_disorganised_english$residuals)

residualPlot(lm_letter_valid_interpersonal_disorganised_english)

vif(lm_letter_valid_interpersonal_disorganised_english)

####

lm_letter_valid_interpersonal_social_anxiety_english <- lm(letter_valid_score_z ~ sex + age + english + spq_interpersonal + spq_social_anxiety,
                                                   data = dataset_master_english)

summary(lm_letter_valid_interpersonal_social_anxiety_english)

glance(lm_letter_valid_interpersonal_social_anxiety_english)

ols_plot_resid_qq(lm_letter_valid_interpersonal_social_anxiety_english)

shapiro_test(lm_letter_valid_interpersonal_social_anxiety_english$residuals)

residualPlot(lm_letter_valid_interpersonal_social_anxiety_english)

vif(lm_letter_valid_interpersonal_social_anxiety_english)

####

lm_letter_valid_disorganised_social_anxiety_english <- lm(letter_valid_score_z ~ sex + age + english + spq_disorganised + spq_social_anxiety,
                                                  data = dataset_master)

summary(lm_letter_valid_disorganised_social_anxiety_english)

glance(lm_letter_valid_disorganised_social_anxiety_english)

ols_plot_resid_qq(lm_letter_valid_disorganised_social_anxiety_english)

shapiro_test(lm_letter_valid_disorganised_social_anxiety_english$residuals)

residualPlot(lm_letter_valid_disorganised_social_anxiety_english)

vif(lm_letter_valid_disorganised_social_anxiety_english)

####

lm_letter_valid_disorganised_interpersonal_social_anxiety_english <- lm(letter_valid_score_z ~ sex + age + english + spq_interpersonal + 
                                                                          spq_disorganised + spq_social_anxiety, data = dataset_master_english)

summary(lm_letter_valid_disorganised_interpersonal_social_anxiety_english)

glance(lm_letter_valid_disorganised_interpersonal_social_anxiety_english)

ols_plot_resid_qq(lm_letter_valid_disorganised_interpersonal_social_anxiety_english)

shapiro_test(lm_letter_valid_disorganised_interpersonal_social_anxiety_english$residuals)

residualPlot(lm_letter_valid_disorganised_interpersonal_social_anxiety_english)

vif(lm_letter_valid_disorganised_interpersonal_social_anxiety_english)

# Run a linear regression with letter_atypicality_score_z as the outcome, and sex, age, and english as native language as the covariates,
# and spq_disorganised as the predictor

lm_letter_atypicality_disorganised_english <- lm(letter_atypicality_score_z ~ sex + age + english + spq_disorganised,
                                                 data = dataset_master_english)

summary(lm_letter_atypicality_disorganised_english)

ols_plot_resid_qq(lm_letter_atypicality_disorganised_english)

shapiro_test(lm_letter_atypicality_disorganised_english$residuals)

residualPlot(lm_letter_atypicality_disorganised_english)

vif(lm_letter_atypicality_disorganised_english)

# Run a linear regression with semantic_atypicality_score_z as the outcome, and sex, age, and as the covariates, and spq_disorganised
# as the predictor

lm_semantic_atypiciality_disorganised <- lm(semantic_atypicality_score_z ~ sex + age + spq_disorganised, data = dataset_master)

summary(lm_semantic_atypiciality_disorganised)

ols_plot_resid_qq(lm_semantic_atypiciality_disorganised)

shapiro_test(lm_semantic_atypiciality_disorganised$residuals)

residualPlot(lm_semantic_atypiciality_disorganised)

vif(lm_semantic_atypiciality_disorganised)

# Compare linear models with letter_valid_score as the outcome, and combinations of spq_interpersonal, spq_disorganised, and spq_social_anxiety

lm_letter_valid_interpersonal <- lm(letter_valid_score_z ~ sex + age, data = dataset_master)

summary(lm_letter_valid_interpersonal)

glance(lm_letter_valid_interpersonal)

ols_plot_resid_qq(lm_letter_valid_interpersonal)

shapiro_test(lm_letter_valid_interpersonal$residuals)

residualPlot(lm_letter_valid_interpersonal)

vif(lm_letter_valid_interpersonal)

####

lm_letter_valid_interpersonal <- lm(letter_valid_score_z ~ sex + age + spq_interpersonal, data = dataset_master)

summary(lm_letter_valid_interpersonal)

glance(lm_letter_valid_interpersonal)

ols_plot_resid_qq(lm_letter_valid_interpersonal)

shapiro_test(lm_letter_valid_interpersonal$residuals)

residualPlot(lm_letter_valid_interpersonal)

vif(lm_letter_valid_interpersonal)

####

lm_letter_valid_disorganised <- lm(letter_valid_score_z ~ sex + age + spq_disorganised, data = dataset_master)

summary(lm_letter_valid_disorganised)

glance(lm_letter_valid_disorganised)

ols_plot_resid_qq(lm_letter_valid_disorganised)

shapiro_test(lm_letter_valid_disorganised$residuals)

residualPlot(lm_letter_valid_disorganised)

vif(lm_letter_valid_disorganised)

####

lm_letter_valid_social_anxiety <- lm(letter_valid_score_z ~ sex + age + spq_social_anxiety, data = dataset_master)

summary(lm_letter_valid_social_anxiety)

glance(lm_letter_valid_social_anxiety)

ols_plot_resid_qq(lm_letter_valid_social_anxiety)

shapiro_test(lm_letter_valid_social_anxiety$residuals)

residualPlot(lm_letter_valid_social_anxiety)

vif(lm_letter_valid_social_anxiety)

####

lm_letter_valid_interpersonal_disorganised <- lm(letter_valid_score_z ~ sex + age + spq_interpersonal + spq_disorganised, data = dataset_master)

summary(lm_letter_valid_interpersonal_disorganised)

glance(lm_letter_valid_interpersonal_disorganised)

ols_plot_resid_qq(lm_letter_valid_interpersonal_disorganised)

shapiro_test(lm_letter_valid_interpersonal_disorganised$residuals)

residualPlot(lm_letter_valid_interpersonal_disorganised)

vif(lm_letter_valid_interpersonal_disorganised)

####

lm_letter_valid_interpersonal_social_anxiety <- lm(letter_valid_score_z ~ sex + age + spq_interpersonal + spq_social_anxiety,
                                                   data = dataset_master)

summary(lm_letter_valid_interpersonal_social_anxiety)

glance(lm_letter_valid_interpersonal_social_anxiety)

ols_plot_resid_qq(lm_letter_valid_interpersonal_social_anxiety)

shapiro_test(lm_letter_valid_interpersonal_social_anxiety$residuals)

residualPlot(lm_letter_valid_interpersonal_social_anxiety)

vif(lm_letter_valid_interpersonal_social_anxiety)

####

lm_letter_valid_disorganised_social_anxiety <- lm(letter_valid_score_z ~ sex + age + spq_disorganised + spq_social_anxiety,
                                                  data = dataset_master)

summary(lm_letter_valid_disorganised_social_anxiety)

glance(lm_letter_valid_disorganised_social_anxiety)

ols_plot_resid_qq(lm_letter_valid_disorganised_social_anxiety)

shapiro_test(lm_letter_valid_disorganised_social_anxiety$residuals)

residualPlot(lm_letter_valid_disorganised_social_anxiety)

vif(lm_letter_valid_disorganised_social_anxiety)

####

lm_letter_valid_disorganised_interpersonal_social_anxiety <- lm(letter_valid_score_z ~ sex + age + spq_interpersonal + spq_disorganised +
                                                                  spq_social_anxiety, data = dataset_master)

summary(lm_letter_valid_disorganised_interpersonal_social_anxiety)

glance(lm_letter_valid_disorganised_interpersonal_social_anxiety)

ols_plot_resid_qq(lm_letter_valid_disorganised_interpersonal_social_anxiety)

shapiro_test(lm_letter_valid_disorganised_interpersonal_social_anxiety$residuals)

residualPlot(lm_letter_valid_disorganised_interpersonal_social_anxiety)

vif(lm_letter_valid_disorganised_interpersonal_social_anxiety)

# Run a linear regression with letter_atypicality_score_z as the outcome, sex, age, and rct_score as the covariates, and spq_disorganised as
# the predictor

lm_letter_atypicality_disorganised <- lm(letter_atypicality_score_z ~ sex + age + spq_disorganised, data = dataset_master)

summary(lm_letter_atypicality_disorganised)

ols_plot_resid_qq(lm_letter_atypicality_disorganised)

shapiro_test(lm_letter_atypicality_disorganised$residuals)

residualPlot(lm_letter_atypicality_disorganised)

vif(lm_letter_atypicality_disorganised)

# Conduct ANCOVAs

# Semantic valid score versus interpersonal

ancova_semantic_valid_interpersonal <- aov(semantic_valid_score_z ~ age + sex + spq_interpersonal_group, data = dataset_master)

Anova(ancova_semantic_valid_interpersonal, type = "III")

eta_squared(ancova_semantic_valid_interpersonal, partial = TRUE)

levene_test(ancova_semantic_valid_interpersonal$residuals ~ spq_interpersonal_group, data = dataset_master)

shapiro_test(ancova_semantic_valid_interpersonal$residuals)

# Semantic atypicality score versus interpersonal

ancova_semantic_atypicality_interpersonal <- aov(semantic_atypicality_score_z ~ age + sex + spq_interpersonal_group,
                                                 data = dataset_master)

Anova(ancova_semantic_atypicality_interpersonal, type = "III")

eta_squared(ancova_semantic_atypicality_interpersonal, partial = TRUE)

levene_test(ancova_semantic_atypicality_interpersonal$residuals ~ spq_interpersonal_group, data = dataset_master)

shapiro_test(ancova_semantic_atypicality_interpersonal$residuals)

# Semantic productivity score versus interpersonal

ancova_semantic_productivity_interpersonal <- aov(semantic_productivity_score_z ~ age + sex + spq_interpersonal_group,
                                                  data = dataset_master)

Anova(ancova_semantic_productivity_interpersonal, type = "III")

eta_squared(ancova_semantic_productivity_interpersonal, partial = TRUE)

levene_test(ancova_semantic_productivity_interpersonal$residuals ~ spq_interpersonal_group, data = dataset_master)

shapiro_test(ancova_semantic_productivity_interpersonal$residuals)

# Letter valid score versus interpersonal

ancova_letter_valid_interpersonal <- aov(letter_valid_score_z ~ age + sex + spq_interpersonal_group,
                                         data = dataset_master)

Anova(ancova_letter_valid_interpersonal, type = "III")

eta_squared(ancova_letter_valid_interpersonal, partial = TRUE)

levene_test(ancova_letter_valid_interpersonal$residuals ~ spq_interpersonal_group, data = dataset_master)

shapiro_test(ancova_letter_valid_interpersonal$residuals)

# Letter atypicality score versus interpersonal

ancova_letter_atypicality_interpersonal <- aov(letter_atypicality_score_z ~ age + sex + spq_interpersonal_group,
                                               data = dataset_master)

Anova(ancova_letter_atypicality_interpersonal, type = "III")

eta_squared(ancova_letter_atypicality_interpersonal, partial = TRUE)

levene_test(ancova_letter_atypicality_interpersonal$residuals ~ spq_interpersonal_group, data = dataset_master)

shapiro_test(ancova_letter_atypicality_interpersonal$residuals)

# rct score versus interpersonal

ancova_rct_interpersonal <- aov(rct_score ~ age + sex + spq_interpersonal_group,
                                data = dataset_master)

Anova(ancova_rct_interpersonal, type = "III")

eta_squared(ancova_rct_interpersonal, partial = TRUE)

levene_test(ancova_rct_interpersonal$residuals ~ spq_interpersonal_group, data = dataset_master)

shapiro_test(ancova_rct_interpersonal$residuals)

# Semantic valid score versus cognitive perceptual

ancova_semantic_valid_cognitive_perceptual <- aov(semantic_valid_score_z ~ age + sex + spq_cognitive_perceptual_group,
                                                  data = dataset_master)

Anova(ancova_semantic_valid_cognitive_perceptual, type = "III")

eta_squared(ancova_semantic_valid_cognitive_perceptual, partial = TRUE)

levene_test(ancova_semantic_valid_cognitive_perceptual$residuals ~ spq_interpersonal_group, data = dataset_master)

shapiro_test(ancova_semantic_valid_cognitive_perceptual$residuals)

# Semantic atypicality score versus cognitive perceptual

ancova_semantic_atypicality_cognitive_perceptual <- aov(semantic_atypicality_score_z ~ age + sex + spq_cognitive_perceptual_group,
                                                        data = dataset_master)

Anova(ancova_semantic_atypicality_cognitive_perceptual, type = "III")

eta_squared(ancova_semantic_atypicality_cognitive_perceptual, partial = TRUE)

levene_test(ancova_semantic_atypicality_cognitive_perceptual$residuals ~ spq_cognitive_perceptual_group, data = dataset_master)

shapiro_test(ancova_semantic_atypicality_cognitive_perceptual$residuals)

# Semantic productivity score versus cognitive perceptual

ancova_semantic_productivity_cognitive_perceptual <- aov(semantic_productivity_score_z ~ age + sex + spq_cognitive_perceptual_group,
                                                         data = dataset_master)

Anova(ancova_semantic_productivity_cognitive_perceptual, type = "III")

eta_squared(ancova_semantic_productivity_cognitive_perceptual, partial = TRUE)

levene_test(ancova_semantic_productivity_cognitive_perceptual$residuals ~ spq_cognitive_perceptual_group, data = dataset_master)

shapiro_test(ancova_semantic_productivity_cognitive_perceptual$residuals)

# Letter valid score versus cognitive perceptual

ancova_letter_valid_cognitive_perceptual <- aov(letter_valid_score_z ~ age + sex + spq_cognitive_perceptual_group,
                                                data = dataset_master)

Anova(ancova_letter_valid_cognitive_perceptual, type = "III")

eta_squared(ancova_letter_valid_cognitive_perceptual, partial = TRUE)

levene_test(ancova_letter_valid_cognitive_perceptual$residuals ~ spq_cognitive_perceptual_group, data = dataset_master)

shapiro_test(ancova_letter_valid_cognitive_perceptual$residuals)

# Letter atypicality score versus cognitive perceptual

ancova_letter_atypicality_cognitive_perceptual <- aov(letter_atypicality_score_z ~ age + sex + spq_cognitive_perceptual_group,
                                                      data = dataset_master)

Anova(ancova_letter_atypicality_cognitive_perceptual, type = "III")

eta_squared(ancova_letter_atypicality_cognitive_perceptual, partial = TRUE)

levene_test(ancova_letter_atypicality_cognitive_perceptual$residuals ~ spq_cognitive_perceptual_group, data = dataset_master)

shapiro_test(ancova_letter_atypicality_cognitive_perceptual$residuals)

# rct score versus cognitive perceptual

ancova_rct_cognitive_perceptual <- aov(rct_score ~ age + sex + spq_cognitive_perceptual_group,
                                       data = dataset_master)

Anova(ancova_rct_cognitive_perceptual, type = "III")

eta_squared(ancova_rct_cognitive_perceptual, partial = TRUE)

levene_test(ancova_rct_cognitive_perceptual$residuals ~ spq_cognitive_perceptual_group, data = dataset_master)

shapiro_test(ancova_rct_cognitive_perceptual$residuals)

# Semantic valid score versus disorganised

ancova_semantic_valid_disorganised <- aov(semantic_valid_score_z ~ age + sex + spq_disorganised_group,
                                          data = dataset_master)

Anova(ancova_semantic_valid_disorganised, type = "III")

eta_squared(ancova_semantic_valid_disorganised, partial = TRUE)

levene_test(ancova_semantic_valid_disorganised$residuals ~ spq_disorganised_group, data = dataset_master)

shapiro_test(ancova_semantic_valid_disorganised$residuals)

# Semantic atypicality score versus disorganised

ancova_semantic_atypicality_disorganised <- aov(semantic_atypicality_score_z ~ age + sex + spq_disorganised_group,
                                                data = dataset_master)

Anova(ancova_semantic_atypicality_disorganised, type = "III")

eta_squared(ancova_semantic_atypicality_disorganised, partial = TRUE)

levene_test(ancova_semantic_atypicality_disorganised$residuals ~ spq_disorganised_group, data = dataset_master)

shapiro_test(ancova_semantic_atypicality_disorganised$residuals)

# Semantic productivity score versus disorganised

ancova_semantic_productivity_disorganised <- aov(semantic_productivity_score_z ~ age + sex + spq_disorganised_group,
                                                 data = dataset_master)

Anova(ancova_semantic_productivity_disorganised, type = "III")

eta_squared(ancova_semantic_productivity_disorganised, partial = TRUE)

levene_test(ancova_semantic_productivity_disorganised$residuals ~ spq_disorganised_group, data = dataset_master)

shapiro_test(ancova_semantic_productivity_disorganised$residuals)

# Letter valid score versus disorganised

ancova_letter_valid_disorganised <- aov(letter_valid_score_z ~ age + sex + spq_disorganised_group,
                                        data = dataset_master)

Anova(ancova_letter_valid_disorganised, type = "III")

eta_squared(ancova_letter_valid_disorganised, partial = TRUE)

levene_test(ancova_letter_valid_disorganised$residuals ~ spq_disorganised_group, data = dataset_master)

shapiro_test(ancova_letter_valid_disorganised$residuals)

# Letter atypicality score versus disorganised

ancova_letter_atypicality_disorganised <- aov(letter_atypicality_score_z ~ age + sex + spq_disorganised_group,
                                              data = dataset_master)

Anova(ancova_letter_atypicality_disorganised, type = "III")

eta_squared(ancova_letter_atypicality_disorganised, partial = TRUE)

levene_test(ancova_letter_atypicality_disorganised$residuals ~ spq_disorganised_group, data = dataset_master)

shapiro_test(ancova_letter_atypicality_disorganised$residuals)

# rct score versus disorganised

ancova_rct_disorganised <- aov(rct_score ~ age + sex + spq_disorganised_group,
                               data = dataset_master)

Anova(ancova_rct_disorganised, type = "III")

eta_squared(ancova_rct_disorganised, partial = TRUE)

levene_test(ancova_rct_disorganised$residuals ~ spq_disorganised_group, data = dataset_master)

shapiro_test(ancova_rct_disorganised$residuals)

dataset_master$spq_disorganised_group <- factor(dataset_master$spq_disorganised_group,
                                                  levels=c("low", "medium", "high"))

plot_rct_disorganised_groups <- dataset_master %>%
  ggplot(aes(x = spq_disorganised_group, y = rct_score, fill = spq_disorganised_group)) +
  geom_boxplot(alpha = 0.5) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "BuPu") +
  labs(x = "Schizotypal Personaliy, Disorganised Group", y = "Deep Cloze Test")

plot_rct_disorganised_groups

ggsave("plot_rct_disorganised_groups.tiff", units="in", width=10, height=7, dpi=300, compression = 'lzw',
       plot_rct_disorganised_groups)

# Semantic valid score versus social anxiety

ancova_semantic_valid_social_anxiety <- aov(semantic_valid_score_z ~ age + sex + spq_social_anxiety_group,
                                            data = dataset_master)

Anova(ancova_semantic_valid_social_anxiety, type = "III")

eta_squared(ancova_semantic_valid_social_anxiety, partial = TRUE)

levene_test(ancova_semantic_valid_social_anxiety$residuals ~ spq_social_anxiety_group, data = dataset_master)

shapiro_test(ancova_semantic_valid_social_anxiety$residuals)

# Semantic atypicality score versus social anxiety

ancova_semantic_atypicality_social_anxiety <- aov(semantic_atypicality_score_z ~ age + sex + spq_social_anxiety_group,
                                                  data = dataset_master)

Anova(ancova_semantic_atypicality_social_anxiety, type = "III")

eta_squared(ancova_semantic_atypicality_social_anxiety, partial = TRUE)

levene_test(ancova_semantic_atypicality_social_anxiety$residuals ~ spq_social_anxiety_group, data = dataset_master)

shapiro_test(ancova_semantic_atypicality_social_anxiety$residuals)

# Semantic productivity score versus social anxiety

ancova_semantic_productivity_social_anxiety <- aov(semantic_productivity_score_z ~ age + sex + spq_social_anxiety_group,
                                                   data = dataset_master)

Anova(ancova_semantic_productivity_social_anxiety, type = "III")

eta_squared(ancova_semantic_productivity_social_anxiety, partial = TRUE)

levene_test(ancova_semantic_productivity_social_anxiety$residuals ~ spq_social_anxiety_group, data = dataset_master)

shapiro_test(ancova_semantic_productivity_social_anxiety$residuals)

# Letter valid score versus social anxiety

ancova_letter_valid_social_anxiety <- aov(letter_valid_score_z ~ age + sex + spq_social_anxiety_group,
                                          data = dataset_master)

Anova(ancova_letter_valid_social_anxiety, type = "III")

eta_squared(ancova_letter_valid_social_anxiety, partial = TRUE)

levene_test(ancova_letter_valid_social_anxiety$residuals ~ spq_social_anxiety_group, data = dataset_master)

shapiro_test(ancova_letter_valid_social_anxiety$residuals)

TukeyHSD(ancova_letter_valid_social_anxiety, which = "spq_social_anxiety_group")

dataset_master$spq_social_anxiety_group <- factor(dataset_master$spq_social_anxiety_group,
                                                 levels=c("low", "medium", "high"))

plot_letter_valid_social_anxiety_groups <- dataset_master %>%
  ggplot(aes(x = spq_social_anxiety_group, y = letter_valid_score_z, fill = spq_social_anxiety_group)) +
  geom_boxplot(alpha = 0.5) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "BuPu") +
  labs(x = "Schizotypal Personaliy, Social Anxiety Group", y = "Letter Fluency Valid Score")

plot_letter_valid_social_anxiety_groups

ggsave("plot_letter_valid_social_anxiety_groups.tiff", units="in", width=10, height=7, dpi=300, compression = 'lzw',
       plot_letter_valid_social_anxiety_groups)

# Letter atypicality score versus social anxiety

ancova_letter_atypicality_social_anxiety <- aov(letter_atypicality_score_z ~ age + sex + spq_social_anxiety_group,
                                                data = dataset_master)

Anova(ancova_letter_atypicality_social_anxiety, type = "III")

eta_squared(ancova_letter_atypicality_social_anxiety, partial = TRUE)

levene_test(ancova_letter_atypicality_social_anxiety$residuals ~ spq_social_anxiety_group, data = dataset_master)

shapiro_test(ancova_letter_atypicality_social_anxiety$residuals)

# rct score versus social anxiety

ancova_rct_social_anxiety <- aov(rct_score ~ age + sex + spq_social_anxiety_group,
                               data = dataset_master)

Anova(ancova_rct_social_anxiety, type = "III")

eta_squared(ancova_rct_social_anxiety, partial = TRUE)

levene_test(ancova_rct_social_anxiety$residuals ~ spq_social_anxiety_group, data = dataset_master)

shapiro_test(ancova_rct_social_anxiety$residuals)
