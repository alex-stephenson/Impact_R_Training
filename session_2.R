##session_2

library(cleaningtools)
library(tidyverse)

my_raw_dataset <- cleaningtools::cleaningtools_raw_data
my_kobo_survey <- cleaningtools::cleaningtools_survey
my_kobo_choice <- cleaningtools::cleaningtools_choices



more_logs <- my_raw_dataset %>% 
  check_duplicate(uuid_column = "X_uuid") %>% 
  check_soft_duplicates(uuid_column = "X_uuid", kobo_survey = my_kobo_survey, sm_separator = ".") %>%
  check_outliers(uuid_column = "X_uuid") %>%
  check_value(uuid_column = "X_uuid") 

more_logs$checked_dataset <- more_logs$checked_dataset %>% 
  add_duration(uuid_column = "X_uuid", start_column = "X.U.FEFF.start", end_column = "end")

more_logs <- more_logs %>% 
  check_duration(column_to_check = "duration", uuid_column = "X_uuid")

other_columns_to_check <- my_kobo_survey %>% 
  filter(type == "text") %>% 
  filter(name %in% names(my_raw_dataset)) %>%
  pull(name) 

more_logs <- more_logs %>% 
  check_others(uuid_column = "X_uuid", columns_to_check = other_columns_to_check) 

logical_check_list <- readxl::read_excel("01 - example - check_list.xlsx")

more_logs <- more_logs %>% 
  check_logical_with_list(uuid_column = "X_uuid",
                          list_of_check = logical_check_list,
                          check_id_column = "check_id",
                          check_to_perform_column = "check_to_perform",
                          columns_to_clean_column = "columns_to_clean",
                          description_column = "description")

my_combined_log <- create_combined_log(more_logs)

my_combined_log <- my_combined_log %>% 
  add_info_to_cleaning_log(dataset_uuid_column = "X_uuid", 
                           information_to_add = "enumerator_num")

create_xlsx_cleaning_log(my_combined_log,
                         sm_dropdown_type = "logical",
                         output_path =  "../outputs/01 - example - cleaning-log-no-kobo.xlsx")

##2 


my_filled_log <- readxl::read_excel("02 - example - cleaning-log-with-kobo - filled.xlsx", sheet = 2)

check_log_results <- review_cleaning_log(raw_dataset = my_raw_dataset,
                                         raw_data_uuid_column = "X_uuid",
                                         cleaning_log = my_filled_log, 
                                         cleaning_log_uuid_column = "uuid",
                                         cleaning_log_question_column = "question",
                                         cleaning_log_new_value_column = "new_value",
                                         cleaning_log_change_type_column = "change_type",
                                         change_response_value = "change_response")
check_log_results


my_clean_data <- create_clean_data(raw_dataset = my_raw_dataset,
                                   raw_data_uuid_column = "X_uuid",
                                   cleaning_log = my_filled_log, 
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_change_type_column = "change_type")

my_clean_data2 <- recreate_parent_column(dataset = my_clean_data,
                                         uuid_column = "X_uuid",
                                         kobo_survey = my_kobo_survey,
                                         kobo_choices = my_kobo_choice,
                                         sm_separator = ".", 
                                         cleaning_log_to_append = my_filled_log)

review_other_log <- review_others(dataset = my_clean_data2$data_with_fix_concat,
                                  uuid_column = "X_uuid", 
                                  kobo_survey = my_kobo_survey, 
                                  columns_not_to_check = "consent_telephone_number")


my_deletion_log <- my_clean_data2$cleaning_log %>% 
  filter(change_type == "remove_survey")

my_filled_log_no_deletion <- my_clean_data2$cleaning_log %>% 
  filter(change_type != "remove_survey") %>% 
  filter(!uuid %in% my_deletion_log$uuid)

review_of_cleaning <- review_cleaning(raw_dataset = my_raw_dataset,
                                      raw_dataset_uuid_column = "X_uuid", 
                                      clean_dataset = my_clean_data2$data_with_fix_concat,
                                      clean_dataset_uuid_column = "X_uuid",
                                      cleaning_log = my_filled_log_no_deletion, 
                                      cleaning_log_uuid_column = "uuid",
                                      cleaning_log_question_column = "question",
                                      cleaning_log_new_value_column = "new_value",
                                      cleaning_log_change_type_column = "change_type", 
                                      cleaning_log_old_value_column = "old_value", 
                                      deletion_log = my_deletion_log, 
                                      deletion_log_uuid_column = "uuid"
)

## practice 

## 1 

previous_exercise_log <- readRDS("03 - exercise - previous_log.RDS")

previous_exercise_log %>% names()

combined_log <- previous_exercise_log %>%
  create_combined_log()


create_xlsx_cleaning_log(combined_log,
                         kobo_survey = my_kobo_survey,
                         kobo_choices = my_kobo_choice,
                         sm_dropdown_type = "logical",
                         output_path =  "outputs/ex_2_1_clog.xlsx")


exercise_filled_log <- readxl::read_excel("04 - exercise - cleaning_log - filled.xlsx", sheet = "cleaning_log")
exercise_checked_data <- combined_log$checked_dataset



my_clean_data <- create_clean_data(raw_dataset = exercise_checked_data,
                                   raw_data_uuid_column = "X_uuid",
                                   cleaning_log = exercise_filled_log, 
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_change_type_column = "change_type")

exercise_clean_dataset2 <- recreate_parent_column(my_clean_data,
                                                  uuid_column = "X_uuid", 
                                                  kobo_survey = my_kobo_survey,
                                                  kobo_choices = my_kobo_choice,
                                                  cleaning_log_to_append = exercise_filled_log)


exercise3_clean_dataset <- readxl::read_excel("05 - exercise - clean dataset for review.xlsx")

exercise3_cleaning_log <- readxl::read_excel("05 - exercise - clean dataset for review.xlsx", sheet = 2)

exercise3_deletion_log <- exercise3_cleaning_log %>% 
  filter(change_type == "remove_survey")

exercise3_log_no_deletion <- exercise3_cleaning_log %>% 
  filter(change_type != "remove_survey") %>% 
  filter(!uuid %in% exercise3_deletion_log$uuid)

review_of_cleaning <- review_cleaning(raw_dataset = exercise_checked_data,
                                      raw_dataset_uuid_column = "X_uuid", 
                                      clean_dataset = exercise3_clean_dataset,
                                      clean_dataset_uuid_column = "X_uuid",
                                      cleaning_log = exercise3_log_no_deletion, 
                                      cleaning_log_uuid_column = "uuid",
                                      cleaning_log_question_column = "question",
                                      cleaning_log_new_value_column = "new_value",
                                      cleaning_log_change_type_column = "change_type", 
                                      cleaning_log_old_value_column = "old_value", 
                                      deletion_log = exercise3_deletion_log, 
                                      deletion_log_uuid_column = "uuid"
)

