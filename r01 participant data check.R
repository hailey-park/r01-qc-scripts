rm(list=ls())

#Read in libraries
library(rtf)
library(tidyverse)

#Set working directory to folder where you have cleaned participant-level data (TO DO: Update folder name with correct date)
setwd("~/Stanford Research/r01-qc-scripts/data-qc-090225")

#Read in cleaned participant data (this cleaned data is from the `PID cleaning script.R`)
cleaned_data <- read_csv("clean-data.csv")[,-1] 

#Create folder to store village QC reports and set the folder as the working directory
dir.create("village-qc-reports")
setwd("village-qc-reports")

############################################################################################################################################
#This automated script creates a participant-level QC report doc for a specified village. After specifying the village below,
#you can run the entire bottom portion of this script (line 23 and below) to output the report file. The report docs should
#populate in the `village-qc-reports` folder.

#Fill in village name, village code (make sure it's 2 digits), and your name (TO DO: fill out!)
village_name <- "SEGUIE"
village_code_input <- "17"
your_name_stanford_researcher <- "Hailey Park"

#Filter specific village data
data <- cleaned_data %>%
  filter(village_code == village_code_input)


#Create word doc to print qc output
qc_doc <- RTF(paste0("QC-report-", village_code_input, "-", format(Sys.Date(), "%m%d%y"), ".doc"))  # this can be an .rtf or a .doc
addHeader(qc_doc, paste0("HOTSPOTS Automated Data QC Report"), font.size = 14)
addText(qc_doc, paste0("Village Name: ", village_name, "\nVillage ID: ", village_code_input, "\nDate: ", Sys.Date(), "\nStanford Researcher: ", your_name_stanford_researcher))

#Print total enrollment numbers
unduplicated_data <- data %>% distinct(record_id, .keep_all = TRUE) %>% filter(!participant_code %>% is.na())
addText(qc_doc, "\n\nEnrollment numbers", italic = TRUE)
addText(qc_doc, paste0("\nTotal: ", nrow(unduplicated_data)))
addText(qc_doc, paste0("\nPre-school children (2-4 years): ", nrow(unduplicated_data %>% filter(as.numeric(age) %in% c(2:4)))))
addText(qc_doc, paste0("\nSchool children (5-17 years): ", nrow(unduplicated_data %>% filter(as.numeric(age) %in% c(5:17)))))
addText(qc_doc, paste0("\nAdults (18+ years): ", nrow(unduplicated_data %>% filter(as.numeric(age) > 17))))
addText(qc_doc, paste0("\nNA: ", nrow(unduplicated_data %>% filter(as.numeric(age) %>% is.na()))))

#Create section for reporting data issues
addText(qc_doc, "\n\nPart 1: Ethics Forms:")
#Check each record (row-by-row)
for(i in c(1:nrow(unduplicated_data))){
  
  record <- unduplicated_data[i, ] 
  pid <- record$participant_id_format
  
  #### check consent/assent/non-opposition for enrollment
  if((!record$minor_q %>% is.na() & record$minor_q == 0) & (record$consent_adult!= 1 | record$consent_adult%>%is.na())){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Consent form (adult) is not marked as 'yes'."))}
  if((!record$minor_q %>% is.na() & record$minor_q == 0) & (record$certificate!= 1 | record$certificate%>%is.na())){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Certificate of non-opposition form (adult) is not marked as 'yes'."))}
  if((!record$minor_q %>% is.na() & record$minor_q == 1) & (record$consent_minor!= 1 | record$consent_minor%>%is.na())){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Consent form (minor) is not marked as 'yes'."))}
  if((!record$minor_q %>% is.na() & record$minor_q == 1) & (record$minor_assent_q!= 1 | record$minor_assent_q%>%is.na())){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Assent form (minor) is not marked as 'yes'."))}
  if((!record$minor_q %>% is.na() & record$minor_q == 1) & (record$certificate_minor!= 1 | record$certificate_minor%>%is.na())){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Certificate of non-opposition form (minor) is not marked as 'yes'."))}
  if((!record$minor_q %>% is.na() & record$minor_q == 0) & (record$consent_sc!= 1 | record$consent_sc%>%is.na())){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - Consent/Assent confirmation question (adult) is not marked as 'yes'."))}
  if((!record$minor_q %>% is.na() & record$minor_q == 1) & (record$consent_sc!= 1 | record$consent_sc%>%is.na())){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - Consent/Assent confirmation question (minor) is not marked as 'yes'."))}
  
  #### check for missing pictures
  if(record$consent_pic %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Consent form picture - Missing (required)."))}
  if((!record$minor_q %>% is.na() & record$minor_q == 1) & record$assent_pic %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Assent form picture - Missing (required)."))}
  if(record$cert_pic %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Certificate of non-opposition form picture - Missing (required)."))}
  
}

#Create section for reporting data issues
addText(qc_doc, "\n\nPart 2: Return of Test Results and Treatment: ")

#Check each record (row-by-row)
for(i in c(1:nrow(unduplicated_data))){

  record <- unduplicated_data[i, ]
  pid <- record$participant_id_format

  # #### check that all tests are marked as returned (THIS IS INTENTIONALLY COMMENTED OUT)
  # if(record$test_return %>% is.na() | record$test_return != 1){addText(qc_doc, paste0("\n - ", pid, " - Test not marked as returned."))}

  #### check S.mansoni and S.haemotobium positive cases were offered praziquantel
  if(any((!record$a_mansoni_1 %>% is.na() & record$a_mansoni_1 > 0), 
         (!record$a_mansoni_2 %>% is.na() & record$a_mansoni_2 > 0),
         (!record$b_mansoni_1 %>% is.na() & record$b_mansoni_1 > 0),
         (!record$b_mansoni_2 %>% is.na() & record$b_mansoni_2 > 0),
         (!record$haemat_urine_num %>% is.na() & record$haemat_urine_num > 0)) &
     !record$praz_offer %>% is.na() & record$praz_offer == 0){addText(qc_doc, paste0("\n - ", pid, " - Had a positive S.mansoni or S.haematobium infection but praziquantel was not marked as offered."))}

  #### check STH infections (Ascaris, hookworm, Trichuris) positive cases were offered albendazole.
  if(any((!record$a_hookworm_1 %>% is.na() & record$a_hookworm_1 > 0),
         (!record$a_hookworm_2 %>% is.na() & record$a_hookworm_2 > 0),
         (!record$b_hookworm_1 %>% is.na() & record$b_hookworm_1 > 0),
         (!record$b_hookworm_2 %>% is.na() & record$b_hookworm_2 > 0),
         (!record$a_lumbri_1 %>% is.na() & record$a_lumbri_1 > 0),
         (!record$a_lumbri_2 %>% is.na() & record$a_lumbri_2 > 0),
         (!record$b_lumbri_1 %>% is.na() & record$b_lumbri_1 > 0),
         (!record$b_lumbri_2 %>% is.na() & record$b_lumbri_2 > 0),
         (!record$a_trichi_1 %>% is.na() & record$a_trichi_1 > 0),
         (!record$a_trichi_2 %>% is.na() & record$a_trichi_2 > 0),
         (!record$b_trichi_1 %>% is.na() & record$b_trichi_1 > 0),
         (!record$b_trichi_2 %>% is.na() & record$b_trichi_2 > 0)) &
     !record$alb_offer %>% is.na() & record$alb_offer == 0){addText(qc_doc, paste0("\n - ", pid, " - Had a positive STH infections (Ascaris, hookworm, Trichuris) but albendazole was not marked as offered."))}

  #### check if you were offered treatment but had no infections 
  if(all(record %>% select(starts_with("a_"), starts_with("b_"), haemat_urine_num, poc_reader_urine, poc_gscore_urine, poc_auto_urine) %>% is.na() |
     (c(record %>% select(starts_with("a_"), starts_with("b_"), haemat_urine_num, poc_auto_urine) == 0, record %>% select(poc_reader_urine, poc_gscore_urine) == 1))) &
     any((!record$praz_offer %>% is.na() & record$praz_offer == 1), (!record$alb_offer %>% is.na() & record$alb_offer == 1))){addText(qc_doc, paste0("\n - ", pid, " - Had no infections but treatment (praziquantel, albendazole) were marked as offered."))}

  #### Flag if "Other" infections are reported
  if((!record$a_other_1 %>% is.na() & (record$a_other_1) > 0) |
     (!record$a_other_2 %>% is.na() & (record$a_other_2) > 0) |
     (!record$b_other_1 %>% is.na() & (record$b_other_1) > 0) |
     (!record$b_other_2 %>% is.na() & (record$b_other_2) > 0)){addText(qc_doc, paste0("\n - ", pid, " - Had 'Other' infections reported. Described as:\n            Slide 1A: ", record$a_other_1, "\n            Slide 2A: ", record$a_other_2, "\n            Slide 1B: ", record$b_other_1,
     "\n            Slide 2B: ", record$b_other_2, "\n          Can you verify this was reviewed and treated appropriately?."))}

  #### Flag if medication is offered but not accepted
  if((!record$praz_offer %>% is.na() & record$praz_offer == 1 & !record$praz_offer %>% is.na() & record$praz_accept == 0) | (!record$alb_offer %>% is.na() & record$alb_offer == 1 & !record$alb_accept %>% is.na() & record$alb_accept == 0)){addText(qc_doc, paste0("\n - ", pid, " - Medication was offered but not accepted. Is this correct?."))}

}


#Create section for reporting data issues
addText(qc_doc, "\n\nPart 3: Data Clarifications:")

#### check for duplicate records (duplicate participant IDs) and correct digits
duplicates <- data %>% group_by(participant_id_format) %>% filter(n()>1) %>% distinct(participant_id_format, .keep_all = TRUE)
wrong_digits <- unduplicated_data %>% filter(!grepl("[0-9]{2}-[0-9]{3}-[0-9]", participant_id_format)) %>% select(record_id, participant_id_format)
printDuplicateFlag <- function(pid) addText(qc_doc, paste0("\n - ", pid, " - Duplicate participant ID found. Please review."))
printDigitFlag <- function(pid) addText(qc_doc, paste0("\n - ", pid, " - Wrong number of digits used for participant ID. Please review."))
if(nrow(duplicates) > 0) {apply(duplicates[,c('participant_id_format')], 1, function(x) printDuplicateFlag(x))}
if(nrow(wrong_digits) > 0) {apply(wrong_digits[,c('participant_id_format')], 1, function(x) printDigitFlag(x))}

#### check for child records with reported age less than 2 years old
young_age <- unduplicated_data %>% filter(!age %>% is.na() & as.numeric(age) < 2)
printAgeFlag <- function(pid) addText(qc_doc, paste0("\n - ", pid, " - Age is less than 2 years old. Please review."))
if(nrow(young_age) > 0) {apply(young_age[,c('participant_id_format')], 1, function(x) printAgeFlag(x))}

#### check enrollment (4 per household – 2 school children, 1 pre-school child, 1 adult) (THIS IS INTENTIONALLY COMMENTED OUT)
# age_group <- unduplicated_data %>% mutate(age_group = case_when(as.numeric(age) %in% c(2:4) ~ "Pre-School Child",
#                                                                 as.numeric(age) %in% c(5:17) ~ "School-Aged Child",
#                                                                 as.numeric(age) > 17 ~ "Adult",
#                                                                 TRUE ~ NA)) %>%
#   group_by(household_code, age_group) %>% summarise(count = n()) %>% ungroup() %>%
#   complete(age_group, nesting(household_code)) %>%
#   mutate(count = if_else(is.na(count), 0, count)) %>% filter(case_when(age_group == "Pre-School Child" ~ count != 1,
#                                                                                       age_group == "School-Aged Child" ~ count != 2,
#                                                                                       age_group == "Adult" ~ count != 1))
# 
# printHouseholdPreSchoolChildFlag <- function(household, count) addText(qc_doc, paste0("\n - This household (Code: ", household, ") has ", count, " total pre-school-aged children enrolled. Should be 1.Please review."))
# printHouseholdSchoolChildFlag <- function(household, count) addText(qc_doc, paste0("\n - This household (Code: ", household, ") has ", count, " total school-aged children enrolled. Should be 2. Please review."))
# printHouseholdAdultFlag <- function(household, count) addText(qc_doc, paste0("\n - This household (Code: ", household, ") has ", count, " total adults enrolled. Should be 1. Please review."))
# if(nrow(age_group %>% filter(age_group == "Pre-School Child")) > 0) {apply((age_group %>% filter(age_group == "Pre-School Child"))[,c('household_code', 'count')], 1, function(x) printHouseholdPreSchoolChildFlag(x[1], x[2]))}
# if(nrow(age_group %>% filter(age_group == "School-Aged Child")) > 0) {apply((age_group %>% filter(age_group == "School-Aged Child"))[,c('household_code', 'count')], 1, function(x) printHouseholdSchoolChildFlag(x[1], x[2]))}
# if(nrow(age_group %>% filter(age_group == "Adult")) > 0) {apply((age_group %>% filter(age_group == "Adult"))[,c('household_code', 'count')], 1, function(x) printHouseholdAdultFlag(x[1], x[2]))}

#Check each record (row-by-row)
for(i in c(1:nrow(unduplicated_data))){
  
  record <- unduplicated_data[i, ]
  pid <- record$participant_id_format
  
  print(record$record_id)
  
  #### check completeness of survey (based on complete question)
  if(record$participant_enrollment_complete%>%is.na()|record$participant_enrollment_complete==0){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Survey form is not marked as complete."))}
  if(record$sample_collection_record_complete%>%is.na()|record$sample_collection_record_complete==0){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - Survey form is not marked as complete."))}
  if(record$individual_test_results_complete%>%is.na()|record$individual_test_results_complete==0){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Survey form is not marked as complete."))}
  # if(record$return_of_test_results_and_treatment_complete==0|record$return_of_test_results_and_treatment_complete%>%is.na()){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - Survey form is not marked as complete."))} (THIS IS INTENTIONALLY COMMENTED OUT)
  
  #### check missing data survey (participant enrollment)
  if(record$investigator_participant %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Investigator selection question - Missing (required)."))}
  if(record$investigator_participant_oth %>% is.na() & !record$investigator_participant %>% is.na() & record$investigator_participant == 99){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Investigator selection (Other) question - Missing (required)."))}
  if(record$district %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - District question - Missing (required)."))}
  if(all(record %>% select(starts_with("subdist_")) %>% is.na())){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Sub-district question - Missing (required)."))}
  if(all(record %>% select(starts_with("vil_")) %>% is.na())){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Village name question - Missing (required)."))}
  if(record$village_code %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Village code question - Missing (required)."))}
  if(record$household_code %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Household code question - Missing (required)."))}
  if(record$participant_code %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Participant code question - Missing (required)."))}
  if(record$first_name %>% is.na() | record$last_name %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Participant name question - Missing (required)."))}
  if(record$head_house %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Head of household question - Missing (required)."))}
  if(record$minor_q %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Are you a minor? question - Missing (required)."))}
  if((!record$minor_q %>% is.na() & record$minor_q == 1) & (record$first_name_parent %>% is.na() | record$last_name_parent %>% is.na())){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Participant's guardian name question - Missing (required)."))}
  if((!record$minor_q %>% is.na() & record$minor_q == 1) & record$who_ans %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Which guardian? question - Missing (required)."))}
  if(record$dob_known %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Date of birth known? question - Missing (required)."))}
  if((!record$dob_known %>% is.na() & record$dob_known == 1) & record$dob %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Date of birth question - Missing (required)."))}
  if(record$age %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Age question - Missing (required)."))}
  if(record$sex %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Sex question - Missing (required)."))}
  if(record$hist_treat %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - History of treatment question - Missing (required)."))}
  if(all(record %>% select(starts_with("outside_many")) %>% is.na())){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Travel question - Missing (required)."))}
  if((!any(record %>% select(starts_with("outside_many"))) %>% is.na() & any(record %>% select(starts_with("outside_many"))) != 1) & all(record %>% select(starts_with("outside_much")) %>% is.na())){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Travel question (How much?) - Missing (required)."))}
  if((!any(record %>% select(starts_with("outside_many"))) %>% is.na() & any(record %>% select(starts_with("outside_many"))) != 1) & all(record %>% select(starts_with("outside_where")) %>% is.na())){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Travel question (Where?) - Missing (required)."))}
  if(record$diarrhea %>% is.na() & record$age %in% c(2:5)){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Diahhrea question - Missing (required)."))}
  if(record$diarrhea_num %>% is.na() & record$diarrhea == 1 & record$age %in% c(2:5)){addText(qc_doc, paste0("\n - ", pid, " - Participant Enrollment survey - Diahhrea (how many?) question - Missing (required)."))}

  #### check missing data survey (sample collection)
  if(record$investigator_sample %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - Investigator selection question - Missing (required)."))}
  if(as.character(record$investigator_sample_oth) %>% is.na() & !record$investigator_sample %>% is.na() & record$investigator_sample == 99){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - Investigator selection (Other) question - Missing (required)."))}
  if(record$participant_id_sc %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - PID confirmation question - Missing (required)."))}
  if((record$stool_1 %>% is.na() | record$stool_1 == 1) & (record$stool_2 %>% is.na() | record$stool_2 == 1) & ((!record$sample_stool1 %>% is.na() & record$sample_stool1 == 1) | (!record$sample_stool2 %>% is.na() & record$sample_stool2 == 1))){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - Stool sample collection is marked as no, but test results for stool sample are available, please clarify."))}
  if((record$stool_1 %>% is.na() | record$stool_1 == 1) & (record$stool_2 %>% is.na() | record$stool_2 == 1) & ((record$sample_stool1 %>% is.na() | record$sample_stool1 == 0) | (record$sample_stool2 %>% is.na() | record$sample_stool2 == 0))){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - There is no stool sample collected and no stool testing results, is this correct?"))}
  if((record$urine %>% is.na() | record$urine == 1) & ((!record$sample_urine_micro %>% is.na() & record$sample_urine_micro == 1) | (!record$sample_urine_poc %>% is.na() & record$sample_urine_poc == 1))){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - Urine sample collection is marked as no, but test results for urine sample are available, please clarify."))}
  if((record$urine %>% is.na() | record$urine == 1) & ((record$sample_urine_micro %>% is.na() | record$sample_urine_micro == 0) | (record$sample_urine_poc %>% is.na() | record$sample_urine_poc == 0))){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - There is no urine sample collected and no urine testing results, is this correct?"))}
  if(record$blood %>% is.na() | record$blood == 1){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - DBS sample question - Missing (required)."))}
  if(record$date_collect_stool1 %>% is.na() & !record$stool_1 %>% is.na() & record$stool_1 == 0){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - Stool sample (#1), Date question - Missing (required)."))}
  if(record$date_collect_stool2 %>% is.na() & !record$stool_2 %>% is.na() & record$stool_2 == 0){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - Stool sample (#2), Date question - Missing (required)."))}
  if(record$date_collect_urine %>% is.na() & !record$urine %>% is.na() & record$urine == 0){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - Urine sample, Date question - Missing (required)."))}
  if(record$date_collect_dbs %>% is.na() & !record$blood %>% is.na() & record$blood == 0){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - DBS sample, Date question - Missing (required)."))}
  if(record$dbs_filled %>% is.na() & (!record$blood %>% is.na() & record$blood == 0)){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - DBS sample, Circles filled question - Missing (required)."))}
  if((!record$blood %>% is.na() & record$blood == 0) & all(record$dbs_id %>% is.na(), record$dbs_filled %>% is.na(), record$dbs_pic %>% is.na())){addText(qc_doc, paste0("\n - ", pid, " - Sample Collection survey - DBS sample, Any identification (barcode, manual ID, picture) - Missing (required)."))}
  
  #### check missing data survey (individual test results)
  if(record$investigator_test %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Investigator selection question - Missing (required)."))}
  if(record$investigator_test_oth %>% is.na() & !record$investigator_test %>% is.na() & record$investigator_test == 99){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Investigator selection (Other) question - Missing (required)."))}
  if(record$participant_id_test %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - PID confirmation question - Missing (required)."))}
  if(record$sample_stool1 %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Stool sample (#1) results not reported - Missing (required)."))}
  if(!record$sample_stool1 %>% is.na() & record$sample_stool1 == 1 & any(record %>% select((starts_with("a_") & ends_with("_1")), (starts_with("b_") & ends_with("_1")), -"a_other_1", -"b_other_1") %>% is.na())){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Stool sample (#1), Not all egg counts reported - Missing (required)."))}
  if(!record$stool_2 %>% is.na() & record$stool_2 == 0 & record$sample_stool2 %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Stool sample (#2) results not reported - Missing (required)."))}
  if(!record$sample_stool2 %>% is.na() & record$sample_stool2 == 1 & any(record %>% select((starts_with("a_") & ends_with("_2")), (starts_with("b_") & ends_with("_2")), -"a_other_2", -"b_other_2") %>% is.na())){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Stool sample (#2), Not all egg counts reported - Missing (required)."))}
  if(record$sample_urine_micro %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Urine microscopy results not reported - Missing (required)."))}
  if(!record$sample_urine_micro %>% is.na() & record$sample_urine_micro == 1 & record$haemat_urine_num %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Urine microscopy, No egg counts reported - Missing (required)."))}
  if(record$sample_urine_poc %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Urine POC-CCA results not reported - Missing (required)."))}
  if(!record$sample_urine_poc %>% is.na() & record$sample_urine_poc == 1 & record$poc_reader_urine %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Urine POC-CCA results, No score reported - Missing (required)."))}

  # #### check missing data survey (return of tests/treatment) (THIS IS INTENTIONALLY COMMENTED OUT FOR NOW)
  # if(record$investigator_return %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - Investigator selection question - Missing (required)."))}
  # if(record$investigator_return_oth %>% is.na() & !record$investigator_return %>% is.na() & record$investigator_return == 99){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - Investigator selection (Other) question - Missing (required)."))}
  # if(record$participant_id_return %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - PID confirmation question - Missing (required)."))}
  # if(record$consent_confirm %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - Consent question - Missing (required)."))}
  # if(record$test_return %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - Return tests question - Missing (required)."))}
  # if(record$praz_offer %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - Treatment offered (praziquantel) question - Missing (required)."))}
  # if(!record$praz_offer %>% is.na() & record$praz_offer == 1 & record$praz_accept %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - Treatment accepted (praziquantel) question - Missing (required)."))}
  # if(((!record$praz_accept %>% is.na() & record$praz_accept == 1)) & record$praz_accept_num %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - Treatment accepted (praziquantel), quantity question - Missing (required)."))}
  # if(!record$praz_accept %>% is.na() & record$praz_accept == 0 & record$praz_refuse_reason %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - Treatment refused (praziquantel) - can you confirm this is correct?"))}
  # if(record$alb_offer %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - Treatment offered (albendazole) question - Missing (required)."))}
  # if(!record$alb_offer %>% is.na() & record$alb_offer == 1 & record$alb_accept %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - Treatment accepted (albendazole) question - Missing (required)."))}
  # if(!record$alb_accept %>% is.na() & record$alb_accept == 0 & record$alb_refuse_reason %>% is.na()){addText(qc_doc, paste0("\n - ", pid, " - Return of Test Results and Treatment survey - Treatment refused (albendazole) - can you confirm this is correct?"))}

  #### check if all collected samples have reported test results
  if(!record$stool_1 %>% is.na() & record$stool_1 == 0 & (record$sample_stool1 == 0 | record$sample_stool1 %>% is.na())){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Stool sample (#1) was collected but test results not reported."))}
  if(!record$stool_2 %>% is.na() & record$stool_2 == 0 & (record$sample_stool2 == 0 | record$sample_stool2 %>% is.na())){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Stool sample (#2) was collected but test results not reported."))}
  if(!record$urine %>% is.na() & record$urine == 0 & (record$sample_urine_micro == 0 | record$sample_urine_micro %>% is.na() | record$sample_urine_poc == 0 | record$sample_urine_poc %>% is.na())){addText(qc_doc, paste0("\n - ", pid, " - Individual Test Results survey - Urine sample was collected but test results not reported."))}

}
rtf::done(qc_doc)

