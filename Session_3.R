library(impactR4PHU)
library(tidyverse)

my_data <- impactR4PHU::impactR4PHU_data_template |>  
  filter(respondent_consent != "no") 

my_data_with_indicators <- my_data %>% add_fcs(
  cutoffs = "normal"
) %>%
  add_hhs()

## return just the fcs data
my_data_with_fcs %>%
  select(contains('fcs_')) %>%
  head()

##

review_df <- my_data %>%
  add_fcs(
    cutoffs = "normal",
  ) %>%
  select(uuid, fsl_fcs_score, fsl_fcs_cat)

binded_df <- my_data_with_indicators %>%
  full_join(review_df, by = "uuid")

## not really sure what the point of this join is ---- arent they the same dataset?

library(addindicators)

review_one_var <- review_variables(binded_df,
                                   columns_to_review = "fsl_fcs_cat.x",
                                   columns_to_compare_with = "fsl_fcs_cat.y")


review_one_var %>% 
  names()

review_one_var$review_table %>%
  group_by(review_check, review_comment) %>%
  tally()



### Practice exercise


exercise_data <- addindicators::addindicators_MSNA_template_data %>%
  add_fcs(
    cutoffs = "normal",
    fsl_fcs_cereal = "fs_fcs_cereals_grains_roots_tubers",
    fsl_fcs_legumes = "fs_fcs_beans_nuts",
    fsl_fcs_veg = "fs_fcs_vegetables_leaves",
    fsl_fcs_fruit = "fs_fcs_fruit",
    fsl_fcs_meat = "fs_fcs_meat_fish_eggs",
    fsl_fcs_dairy = "fs_fcs_dairy",
    fsl_fcs_sugar = "fs_fcs_sugar",
    fsl_fcs_oil = "fs_fcs_oil_fat_butter"
  ) %>%
  add_hhs(
    fsl_hhs_nofoodhh = "fs_hhs_nofood_yn",
    fsl_hhs_nofoodhh_freq = "fs_hhs_nofood_freq",
    fsl_hhs_sleephungry = "fs_hhs_sleephungry_yn",
    fsl_hhs_sleephungry_freq = "fs_hhs_sleephungry_freq",
    fsl_hhs_alldaynight = "fs_hhs_daynoteating_yn",
    fsl_hhs_alldaynight_freq = "fs_hhs_daynoteating_freq",
    yes_answer = "yes",
    no_answer = "no",
    rarely_answer = "rarely_1_2",
    sometimes_answer = "sometimes_3_10",
    often_answer = "often_10_times"
  ) %>%
  add_rcsi(
    fsl_rcsi_lessquality = "rCSILessQlty",
    fsl_rcsi_borrow = "rCSIBorrow",
    fsl_rcsi_mealsize = "rCSIMealSize",
    fsl_rcsi_mealadult = "rCSIMealAdult",
    fsl_rcsi_mealnb = "rCSIMealNb"
  ) %>%
  add_fcm_phase(
  )




### Practice 2

dataset_to_review <- read.csv("06 - exercise - dataset_to_review.csv")

dataset_without_indicators <- addindicators::addindicators_MSNA_template_data
exercise_data <- addindicators::addindicators_MSNA_template_data %>%
  add_fcs(
    cutoffs = "normal",
    fsl_fcs_cereal = "fs_fcs_cereals_grains_roots_tubers",
    fsl_fcs_legumes = "fs_fcs_beans_nuts",
    fsl_fcs_veg = "fs_fcs_vegetables_leaves",
    fsl_fcs_fruit = "fs_fcs_fruit",
    fsl_fcs_meat = "fs_fcs_meat_fish_eggs",
    fsl_fcs_dairy = "fs_fcs_dairy",
    fsl_fcs_sugar = "fs_fcs_sugar",
    fsl_fcs_oil = "fs_fcs_oil_fat_butter"
  ) %>%
  add_hhs(
    fsl_hhs_nofoodhh = "fs_hhs_nofood_yn",
    fsl_hhs_nofoodhh_freq = "fs_hhs_nofood_freq",
    fsl_hhs_sleephungry = "fs_hhs_sleephungry_yn",
    fsl_hhs_sleephungry_freq = "fs_hhs_sleephungry_freq",
    fsl_hhs_alldaynight = "fs_hhs_daynoteating_yn",
    fsl_hhs_alldaynight_freq = "fs_hhs_daynoteating_freq",
    yes_answer = "yes",
    no_answer = "no",
    rarely_answer = "rarely_1_2",
    sometimes_answer = "sometimes_3_10",
    often_answer = "often_10_times"
  ) 

binded_df <- dataset_to_review %>%
  full_join(exercise_data, by = join_by("uuid"))

review_fsl_cat <- review_variables(binded_df,
                                   columns_to_review = "fsl_fcs_cat.x",
                                   columns_to_compare_with = "fsl_fcs_cat.y")

review_table_fsl <- review_fsl_cat %>%
  pluck("review_table")

fsl_tally <- review_table_fsl %>% 
  group_by(variable, review_check, review_comment) %>%
  tally()

review_hhs <- review_variables(binded_df,
                                     columns_to_review = "hhs_score",
                                     columns_to_compare_with = "hhs_cat")

review_hhs_table <- review_hhs %>%
  pluck("review_table")

hhs_tally <- review_hhs_table %>%
  group_by(variable, review_check, review_comment) %>%
  tally()

rbind(hhs_tally, fsl_tally)

review_fsl_cat %>%
  pluck("dataset") %>%
  filter(!review_check_fsl_fcs_cat.x) %>% 
  select(uuid, review_comment_fsl_fcs_cat.x, fsl_fcs_score.y, fsl_fcs_score.x, fsl_fcs_cat.x, fsl_fcs_cat.y) %>%
  head()

review_fsl_cat %>%
  pluck("dataset") %>% 
  ggplot(., aes(x = x)) +
  geom_histogram(aes(fsl_fcs_score.y, y = ..density.., fill = fsl_fcs_cat.y)) +
  geom_histogram(aes(fsl_fcs_score.x, y = -..density..,fill = fsl_fcs_cat.x)) +
  xlab("FCS Score") +
  theme_minimal() 

## beeswarm

review <- binded_df %>% 
  review_variables(columns_to_review = c("fsl_fcs_cat.x", "fsl_fcs_score.x", "hhs_cat", "hhs_score"),
                   columns_to_compare_with = c("fsl_fcs_cat.y", "fsl_fcs_score.y", "fsl_hhs_cat", "fsl_hhs_score"))


library(ggbeeswarm)

dataset_1 <- review_fsl_cat %>% 
  pluck("dataset") %>%
  select(uuid, fsl_fcs_score = fsl_fcs_score.x, fsl_fcs_cat = fsl_fcs_cat.x) %>%
  mutate(dataset = 'One')

dataset_2 <- review_fsl_cat %>% 
  pluck("dataset") %>%
  select(uuid, fsl_fcs_score = fsl_fcs_score.y, fsl_fcs_cat = fsl_fcs_cat.y) %>%
  mutate(dataset = 'Two')

dataset_joined <- rbind(dataset_1, dataset_2)

dataset_joined %>%
  ggplot(., aes(x = dataset, y = fsl_fcs_score, color = fsl_fcs_cat)) +
  geom_beeswarm()
 
