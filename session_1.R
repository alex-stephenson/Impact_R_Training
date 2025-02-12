library(cleaningtools)
library(dplyr)
library(readxl)
my_raw_dataset <- cleaningtools::cleaningtools_raw_data
my_kobo_survey <- cleaningtools::cleaningtools_survey
my_kobo_choice <- cleaningtools::cleaningtools_choices


my_raw_dataset  %>%
  check_pii(uuid = "X_uuid")


my_raw_dataset %>%
  add_percentage_missing() %>%
  check_percentage_missing(uuid = "X_uuid")

logical_check_list <- read_excel("01 - example - check_list.xlsx")


my_raw_dataset %>% 
  check_logical_with_list(uuid_column = "X_uuid",
                          list_of_check = logical_check_list,
                          check_id_column = "check_id",
                          check_to_perform_column = "check_to_perform",
                          columns_to_clean_column = "columns_to_clean",
                          description_column = "description") %>%
  pluck(2) 
