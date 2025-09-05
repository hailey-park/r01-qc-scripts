rm(list=ls())

#Read in libraries
library(tidyverse)

#Set working directory to folder where you have raw datasets (TO DO: Update folder name)
setwd("~/Stanford Research/r01-qc-scripts/data-qc-090225")

#Read in participant data (TO DO: update names of data files)
data <- read_csv("HOTSPOTParticipantle_DATA_2025-09-02_1443.csv", guess_max = 3000)
data_labels <- read_csv("HOTSPOTParticipantle_DATA_LABELS_2025-09-02_1444.csv", guess_max = 3000)

############################################################################################################################################
#PART 1: Create .csv file of records with PID of incorrect number of digits (not following the XX-XXX-X format)

#### check for records with incorrectly entered participant IDs (correct digits)
correct_digits_no_leading_zero <-  data %>% filter(grepl("^[1-9][0-9]-\\d{3}-\\d{1}", participant_id_format)) %>% select(record_id, participant_id_format)
correct_digits_leading_zero <-  data %>% filter(grepl("^0[1-9]{1}-\\d{3}-\\d{1}", participant_id_format)) %>% select(record_id, participant_id_format)

wrong_digits <- data %>% filter(!record_id %in% c(correct_digits_no_leading_zero$record_id, correct_digits_leading_zero$record_id)) %>% select(record_id, participant_id_format, village_code)

#### write to csv 
write.csv(wrong_digits, "PID-incorrect-digits.csv")

############################################################################################################################################
#PART 2: Create .csv file of records with the incorrect village ID

#### check for records with incorrectly entered participant IDs (correct village ID)
village_ID_check <- data_labels %>% unite(sub_district, starts_with('Aire de Santé'), na.rm = TRUE, remove = FALSE, sep = ' , ') %>%
  rowwise() %>%
  mutate(sub_district = toupper(if_else(sub_district == "AUTRE", `Préciser 'Autre' :`, sub_district))) %>%
  unite(village_name, starts_with('Nom du Village'), na.rm = TRUE, remove = FALSE, sep = ' , ') %>%
  mutate(village_name = if_else(village_name == "", `Préciser 'Autre' Village:`, village_name)) %>%
  select(`ID d'enregistrement`, District, sub_district, village_name, `Code du Village (2 chiffres) Exemple :    Village 2   = 02   Village 12 = 12`,
         `ID du Participant (PID) Format ID du Participant (PID)  = [ménage]- code du participant], où le ménage 10-025 a trois (3) participants:     Membre du ménage 1 PID = 10-025-1     Membre du ménage 2 PID = 10-025-2     Membre du ménage 3 PID = 10-025-3`) %>%
  rename(record_id = `ID d'enregistrement`, district = District, village_code = `Code du Village (2 chiffres) Exemple :    Village 2   = 02   Village 12 = 12`,
         participant_id_format = `ID du Participant (PID) Format ID du Participant (PID)  = [ménage]- code du participant], où le ménage 10-025 a trois (3) participants:     Membre du ménage 1 PID = 10-025-1     Membre du ménage 2 PID = 10-025-2     Membre du ménage 3 PID = 10-025-3`) 
  
incorrect_village_ID <- village_ID_check %>%
  group_by(district, sub_district, village_name, village_code) %>%
  summarise(count = n()) %>% filter(count < 100)

all_incorrect_village_ID_records <- semi_join(village_ID_check, incorrect_village_ID, by = c("district", "sub_district", "village_name", "village_code"))

#### write to csv 
write.csv(all_incorrect_village_ID_records, "PID-incorrect-villageID.csv")

############################################################################################################################################
#PART 3: Manually review the PID inconsistencies (wrong digits, incorrect village ID) and update the dataset.
#        After reviewing, save the .csv files as "[file-name]-reviewed.csv".
############################################################################################################################################
#PART 4: After reviewing PID inconsistencies (wrong digits, incorrect village ID), correct the original dataset

#Read in corrected records
corrected_PID_digits <- read_csv("PID-incorrect-digits-reviewed.csv", col_types = cols(
  record_id = col_integer(),
  `Corrected village code` = col_character(),
  `Corrected PID` = col_character(),
  .default = col_skip())) %>%
  select(record_id, `Corrected village code`, `Corrected PID`)
corrected_PID_villageID <- read_csv("PID-incorrect-villageID-reviewed.csv", col_types = cols(
  record_id = col_integer(),
  `Corrected village code` = col_character(),
  `Corrected PID` = col_character(),
  .default = col_skip())) %>%
  select(record_id, `Corrected village code`, `Corrected PID`)


#Merge corrected PID files with original database (only correcting village code and PID, not other fields)
corrected_data <- left_join(left_join(data, corrected_PID_digits, by = "record_id"),
                            corrected_PID_villageID, by = "record_id") %>%
  rowwise() %>%
  mutate(village_code =  dplyr::case_when(!is.na(`Corrected village code.x`) ~ `Corrected village code.x`,
                                   !is.na(`Corrected village code.y`) ~ `Corrected village code.y`,
                                   TRUE ~ village_code),
         participant_id_format = dplyr::case_when(!is.na(`Corrected PID.x`) ~ `Corrected PID.x`,
                                               !is.na(`Corrected PID.y`) ~ `Corrected PID.y`,
                                               TRUE ~ participant_id_format))

############################################################################################################################################
#PART 5: After correcting the original database, check for duplicate records.

#### check for duplicate records (duplicate participant IDs)
duplicates <- corrected_data %>% group_by(participant_id_format) %>% filter(n()>1) %>% 
  mutate(has_complete_form = if_else(!is.na(consent_pic) & !is.na(cert_pic), 1, 0)) %>%
  select(record_id, participant_id_format, has_complete_form)

  #Normal duplicate is when there is one record with forms, and other records with duplicate PID but no forms
normal_duplicate <- duplicates %>% group_by(participant_id_format, has_complete_form) %>% distinct(has_complete_form, .keep_all = TRUE) %>% 
  group_by(participant_id_format) %>% filter(n() == 2) 

  #This is for duplicates where all duplicates records have none with ethics forms
missing_record_complete_form <- duplicates %>% group_by(participant_id_format, has_complete_form) %>% distinct(has_complete_form, .keep_all = TRUE) %>% 
  group_by(participant_id_format) %>% filter(n() < 2) %>% ungroup() %>% filter(has_complete_form == 0)

  #This is for duplicates where there are more than 1 duplicate record with ethics forms
multiple_records_complete_form <- duplicates %>% group_by(participant_id_format, has_complete_form) %>% distinct(has_complete_form, .keep_all = TRUE) %>% 
  group_by(participant_id_format) %>% filter(n() < 2) %>% ungroup() %>% filter(has_complete_form == 1)


all_missing_record_complete_form <- semi_join(duplicates, missing_record_complete_form, by = c("participant_id_format", "has_complete_form")) %>%
  mutate(notes = "These are duplicates where there is no record with ethics forms under the same PID.")
all_multiple_records_complete_form <- semi_join(duplicates, multiple_records_complete_form, by = c("participant_id_format", "has_complete_form")) %>%
  mutate(notes = "These are duplicates where multiple records have ethics forms under the same PID.")
all_normal_duplicates <- semi_join(duplicates, normal_duplicate, by = c("participant_id_format", "has_complete_form")) %>%
  mutate(notes = "These are duplicates where only 1 record has ethics forms, and other records under the same PID have no forms.")

combined_duplicate_records <- rbind(all_missing_record_complete_form, all_multiple_records_complete_form, all_normal_duplicates)

#### write to csv
write.csv(combined_duplicate_records, "PID-all-duplicates.csv")

############################################################################################################################################
#PART 6: Remove most duplicate records and create a cleaned database to run the participant-level QC and sample size QC reports

  #remove NULL records from incorrect digits and village ID files
records_NULL <- (corrected_PID_villageID %>% filter(is.na(`Corrected PID`)))$record_id

  #remove records where all duplicates have no forms (except one set. we keep the set that is more "completed")
records_duplicates_all_missing <- setdiff(all_missing_record_complete_form$record_id, 
                                          (corrected_data %>% 
                                             filter(participant_id_format %in% unique(all_missing_record_complete_form$participant_id_format)) %>%
                                             mutate(completeness = participant_enrollment_complete + sample_collection_record_complete + individual_test_results_complete) %>% 
                                             group_by(participant_id_format) %>%
                                             filter(completeness == max(completeness)) %>%
                                             distinct(participant_id_format, .keep_all = TRUE))$record_id)


  #remove records where all duplicates have forms (except one set. we keep the set that is more "completed")
records_duplicates_all_multiple <- setdiff(all_multiple_records_complete_form$record_id, 
                                           (corrected_data %>% 
                                              filter(participant_id_format %in% unique(all_multiple_records_complete_form$participant_id_format)) %>%
                                              mutate(completeness = participant_enrollment_complete + sample_collection_record_complete + individual_test_results_complete) %>% 
                                              group_by(participant_id_format) %>%
                                              filter(completeness == max(completeness)) %>%
                                              distinct(participant_id_format, .keep_all = TRUE))$record_id)


  #remove duplicate records (keep duplicate with forms)
records_duplicates_all_normal <- (all_normal_duplicates %>% filter(has_complete_form == 0))$record_id


  #create combined vector of all records to remove 
all_records_to_remove <- c(records_NULL, records_duplicates_all_missing, records_duplicates_all_multiple, records_duplicates_all_normal)

  #Create csv for clean data
clean_data <- corrected_data %>% filter(!record_id %in% all_records_to_remove)

  #Create csv for all duplicates that will be deleted after review
duplicates_to_delete_data <- corrected_data %>% filter(record_id %in% intersect(combined_duplicate_records$record_id, all_records_to_remove)) %>% select(record_id, participant_id_format)

#### write to csv
write.csv(clean_data, "clean-data.csv")
write.csv(duplicates_to_delete_data, "PID-duplicates-to-delete.csv")















