rm(list=ls())

#Read in libraries
library(rtf)
library(tidyverse)

#Set working directory to folder where you have raw datasets (TO DO: Update folder name)
setwd("~/Stanford Research/r01-qc-scripts/data-qc-090225")

#Read in community data (TO DO: update name of data file)
data <- read_csv("HOTSPOTCommunitySurv_DATA_2025-09-02_1445.csv")

#Create dataframe of all villages and village codes to identify any villages without record (TO DO: Change village reference if running QC for a different district)
village_reference <- data.frame(village_name = c("Bonikro", "N'Chompo", "Erymakouguié 1", "Banguié 1", "Ehouegue", "Ouanguié", "Grand Moutcho", 
                                                 "Laoguié", "Yapo-Gare", "Yapo-kpa", "Guessiguié 1", "Guessiguié 2", "Gbesse", "Offoriguié", 
                                                 "Aboude Mandeke", "Aboudé Kouassikro", "Seguié", "Boguié", "Oulézué", "Adomokro", "Kotchimpo",
                                                 "Broumpo + Kédjempo", "Ananguié", "Aké Douanier", "Céchi", "Trénou", "Elevi", "Aké Béfia"),
                                village_code = sprintf("%02d", seq(1, 28)))

#Function to replace accent "é" with "e" (so it is readable in the doc file)
accentCorrection <- function(name) {
  gsub("é", "e", name)
}
############################################################################################################################################
#PART 1: Generate QC report for community-level database to check for anomaly records (e.g., duplicates or null)
#        and check for completeness of forms

#Your name (TO DO: write your name)
your_name_stanford_researcher <- "Hailey Park"

#Create word doc to print qc output 
qc_doc <- RTF(paste0("QC-report-community-", format(Sys.Date(), "%m%d%y"), ".doc"))  # this can be an .rtf or a .doc
addHeader(qc_doc, paste0("HOTSPOTS Community Survey Automated Data QC Report"), font.size = 14)
addText(qc_doc, paste0("\nDate: ", Sys.Date(), "\nStanford Researcher: ", your_name_stanford_researcher))

#Print null or duplicated community records
null_records <- data %>% filter(vil_code %>% is.na())
duplicated_records <- data %>% filter(!vil_code %>% is.na()) %>% group_by(vil_code) %>% filter(n() > 1) %>% 
  distinct(vil_code, vil_name)

#Print missing community records
missing_records <- village_reference %>% filter(!village_code %in% data$vil_code)

#Print community records with name discrepancies
name_discrepancies_records <- village_reference %>% filter(!toupper(village_name) %in% toupper(data$vil_name),
                                                           !village_code %in% missing_records$village_code)

addText(qc_doc, "\n\nPart 1: Village Record Discrepancies", italic = TRUE)
addText(qc_doc, paste0("\n - There are ", nrow(null_records), " null records."))
for(i in c(1:nrow(duplicated_records))){
  addText(qc_doc, paste0("\n - Village ", duplicated_records[i, "vil_code"], " (", accentCorrection(duplicated_records[i, "vil_name"]), ") has a duplicated record."))
}
for(i in c(1:nrow(missing_records))){
  addText(qc_doc, paste0("\n - Village ", missing_records[i, "village_code"], " (", accentCorrection(missing_records[i, "village_name"]), ") has a missing record."))
}
for(i in c(1:nrow(name_discrepancies_records))){
  addText(qc_doc, paste0("\n - Village ", name_discrepancies_records[i, "village_code"], " (", accentCorrection(name_discrepancies_records[i, "village_name"]), ") has a name discrepancy in the record. Was written as ", accentCorrection((data %>% filter(vil_code == name_discrepancies_records[i, "village_code"]))$vil_name)))
}

#Remove null records from dataset
clean_data <- data %>% filter(!vil_code %>% is.na())

#Remove duplicate records (keep the more "complete" set) from dataset
records_duplicates_to_remove <- setdiff((clean_data %>% filter(vil_code %in% duplicated_records$vil_code))$record_id, 
                                          (clean_data %>% 
                                             filter(vil_code %in% unique(duplicated_records$vil_code)) %>%
                                             mutate(completeness = setting_information_complete + environmental_risk_survey_complete + pooling_test_results_complete) %>% 
                                             group_by(vil_code) %>%
                                             filter(completeness == max(completeness)) %>%
                                             distinct(vil_code, .keep_all = TRUE))$record_id)

clean_data <- clean_data %>% filter(!record_id %in% records_duplicates_to_remove)


#Create section for reporting form completeness
addText(qc_doc, "\n\nPart 2: Setting and environmental risk forms marked as 'complete':", italic = TRUE)

  #Check each record in clean dataset (row-by-row)
for(i in c(1:nrow(clean_data))){
  
  record <- clean_data[i, ] 
  village_id <- record$vil_code
  
  #### check completeness of survey (based on complete question)
  if(record$setting_information_complete%>%is.na()|record$setting_information_complete==0){addText(qc_doc, paste0("\n - Village ", village_id, " (", accentCorrection(record$vil_name), ") - Setting survey - Survey form is not marked as complete."))}
  if(record$environmental_risk_survey_complete==0|record$environmental_risk_survey_complete%>%is.na()){addText(qc_doc, paste0("\n - Village ", village_id, " (", accentCorrection(record$vil_name), ") - Environmental Risk survey - Survey form is not marked as complete."))}
  # if(record$pooling_test_results_complete%>%is.na()|record$pooling_test_results_complete==0){addText(qc_doc, paste0("\n - Village ", village_id, " (", record$vil_name, ") - Pooling Test survey - Survey form is not marked as complete."))} (THIS IS INTENTIONALLY COMMENTED OUT)

}

  #Print flag for each record in missing records df (row-by-row) that it is missing these forms
for(i in c(1:nrow(missing_records))){
  addText(qc_doc, paste0("\n - Village ", missing_records[i, "village_code"], " (", accentCorrection(missing_records[i, "village_name"]), ") is missing ALL forms."))
}

#Create section for reporting data issues with pooling test survey
addText(qc_doc, "\n\nPart 3: Pooling test survey: ", italic = TRUE)

data_with_pooling <- clean_data %>% filter(pooling_test_results_complete == 2)
addText(qc_doc, paste0("\n - There are ", nrow(data_with_pooling), " village records with pooling test data reported."))

data_without_pooling <- clean_data %>% filter(pooling_test_results_complete != 2)

addText(qc_doc, paste0("\n - There are ", nrow(data_without_pooling), " village records with no pooling test data reported."))

addText(qc_doc, paste0("\n - There are ", nrow(missing_records), " villages that are missing record in the database, and also have no pooling test data reported."))

addNewLine(qc_doc)


  #Check each record (row-by-row)
for(i in c(1:nrow(data_with_pooling))){
  
  record <- data_with_pooling[i, ] 
  village_id <- record$vil_code

  #### check for completeness of pooling test survey
  if(any(record %>% select(starts_with("pu_")) %>% is.na())){addText(qc_doc, paste0("\n - Village ", village_id, " (", accentCorrection(record$vil_name), ") - Pooling Test survey - Missing some data."))}

}

#Print flag for each record in that has no pooling data (row-by-row)
for(i in c(1:nrow(data_without_pooling))){
  
  record <- data_without_pooling[i, ] 
  village_id <- record$vil_code
  
  addText(qc_doc, paste0("\n - Village ", village_id, " (", accentCorrection(record$vil_name), ") - Pooling Test survey - Missing ENTIRE form."))
}

  #Print flag for each record that is missing entirely (row-by-row)
for(i in c(1:nrow(missing_records))){
  addText(qc_doc, paste0("\n - Village ", missing_records[i, "village_code"], " (", accentCorrection(missing_records[i, "village_name"]), ") is a missing record."))
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
write.csv(miracidia_report, paste0("QC-report-miracidia-", format(Sys.Date(), "%m%d%y"), ".csv"))


