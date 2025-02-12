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