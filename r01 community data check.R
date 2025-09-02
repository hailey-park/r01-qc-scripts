rm(list=ls())

#Read in libraries
library(rtf)
library(tidyverse)

#Read in participant data (TO DO: update name of data file)
data <- read_csv("~/Stanford Research/r01-qc-scripts/HOTSPOTCommunitySurv_DATA_2025-08-28_1921.csv")

############################################################################################################################################
#PART 1: Generate QC report for community-level database to check for anomaly records (e.g., duplicates or null)
#        and check for completeness of forms

#Your name (TO DO: write your name)
your_name_stanford_researcher <- "Hailey Park"

#Create word doc to print qc output (TO DO: specify name of doc file)
qc_doc <- RTF("QC-report-community-082825.doc")  # this can be an .rtf or a .doc
addHeader(qc_doc, paste0("HOTSPOTS Community Survey Automated Data QC Report"), font.size = 14)
addText(qc_doc, paste0("\nDate: ", Sys.Date(), "\nStanford Researcher: ", your_name_stanford_researcher))

#Print null or duplicated community records
null_records <- data %>% filter(vil_code %>% is.na())
duplicated_records <- data %>% filter(!vil_code %>% is.na()) %>% group_by(vil_code) %>% filter(n() > 1) %>% 
  distinct(vil_code, vil_name)

addText(qc_doc, "\n\nPart 1: Village Record Discrepancies", italic = TRUE)
addText(qc_doc, paste0("\n - There are ", nrow(null_records), " null records."))
for(i in c(1:nrow(duplicated_records))){
  addText(qc_doc, paste0("\n - Village ", duplicated_records[i, "vil_code"], " (", duplicated_records[i, "vil_name"], ") has a duplicated record."))
}

#Remove null records from dataset
clean_data <- data %>% filter(!vil_code %>% is.na())

#Create section for reporting form completeness
addText(qc_doc, "\n\nPart 2: Setting and environmental risk forms marked as 'complete':", italic = TRUE)
#Check each record (row-by-row)
for(i in c(1:nrow(clean_data))){
  
  record <- clean_data[i, ] 
  village_id <- record$vil_code
  
  #### check completeness of survey (based on complete question)
  if(record$setting_information_complete%>%is.na()|record$setting_information_complete==0){addText(qc_doc, paste0("\n - Village ", village_id, " (", record$vil_name, ") - Setting survey - Survey form is not marked as complete."))}
  if(record$environmental_risk_survey_complete==0|record$environmental_risk_survey_complete%>%is.na()){addText(qc_doc, paste0("\n - Village ", village_id, " (", record$vil_name, ") - Environmental Risk survey - Survey form is not marked as complete."))}
  # if(record$pooling_test_results_complete%>%is.na()|record$pooling_test_results_complete==0){addText(qc_doc, paste0("\n - Village ", village_id, " (", record$vil_name, ") - Pooling Test survey - Survey form is not marked as complete."))}

}

#Create section for reporting data issues with pooling test survey
addText(qc_doc, "\n\nPart 3: Pooling test survey: ", italic = TRUE)

data_with_pooling <- clean_data %>% filter(pooling_test_results_complete == 2)
addText(qc_doc, paste0("\n - There are ", nrow(data_with_pooling), " village records with pooling test data reported."))

#Check each record (row-by-row)
for(i in c(1:nrow(data_with_pooling))){
  
  record <- data_with_pooling[i, ] 
  village_id <- record$vil_code

  #### check for completeness of pooling test survey
  if(any(record %>% select(starts_with("pu_")) %>% is.na())){addText(qc_doc, paste0("\n - Village ", village_id, " (", record$vil_name, ") - Pooling Test survey - Missing some data."))}

}
rtf::done(qc_doc)

############################################################################################################################################
#PART 2: Create .csv file for miracidia report

  #Use only records with miracidia data reported
data_with_miracidia <- clean_data %>% filter(fiche_de_report_des_miracidium_clos_complete == 2)


#Create report
miracidia_report <- data_with_miracidia %>% select(vil_code, vil_name) %>% 
  rename(village_code = vil_code, village_name = vil_name) %>% 
  mutate(total_miracidia = NA,
         total_people = NA,
         total_miracidia_high = NA,
         total_people_high = NA,
         total_miracidia_moderate = NA,
         total_people_moderate = NA,
         total_miracidia_low = NA,
         total_people_low = NA,
         total_miracidia_negative = NA,
         total_people_negative = NA)

for(i in c(1:nrow(data_with_miracidia))){
  
  record <- data_with_miracidia[i, ] 
  
  total_people_high <- sum((!record %>% select(starts_with("eleve_pid_")) %>% is.na()) + (!record %>% select(starts_with("eleve_nbre_")) %>% is.na() & record %>% select(starts_with("eleve_nbre_")) > 0) == 2)
  total_miracidia_high <- sum(record %>% select(starts_with("eleve_nbre_")), na.rm = TRUE)
  
  total_people_moderate <- sum((!record %>% select(starts_with("modere_pid_")) %>% is.na()) + (!record %>% select(starts_with("modere_nbre_")) %>% is.na() & record %>% select(starts_with("modere_nbre_")) > 0) == 2)
  total_miracidia_moderate <- sum(record %>% select(starts_with("modere_nbre_")), na.rm = TRUE)
  
  total_people_low <- sum((!record %>% select(starts_with("faible_pid_")) %>% is.na()) + (!record %>% select(starts_with("faible_nbre_")) %>% is.na() & record %>% select(starts_with("faible_nbre_")) > 0) == 2)
  total_miracidia_low <- sum(record %>% select(starts_with("faible_nbre_")), na.rm = TRUE)
  
  total_people_negative <- sum((!record %>% select(starts_with("negatif_pid_")) %>% is.na()) + (!record %>% select(starts_with("negatif_nbre_")) %>% is.na() & record %>% select(starts_with("negatif_nbre_")) > 0) == 2)
  total_miracidia_negative <- sum(record %>% select(starts_with("negatif_nbre_")), na.rm = TRUE)
  
  total_people <- total_people_high + total_people_moderate + total_people_low
  total_miracidia <- total_miracidia_high + total_miracidia_moderate + total_miracidia_low
  
  miracidia_report[i, 3:12] <- list(total_miracidia, total_people, total_miracidia_high, total_people_high, total_miracidia_moderate, total_people_moderate,
                                 total_miracidia_low, total_people_low, total_miracidia_negative, total_people_negative)
}

#Write to csv (TO DO: update date of csv file)
write.csv(miracidia_report, "QC-report-miracidia-082825.csv")


