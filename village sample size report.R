rm(list=ls())

#Read in libraries
library(tidyverse)

#Set working directory to folder where you have cleaned participant-level data (TO DO: Update folder name with correct date)
setwd("~/Stanford Research/r01-qc-scripts/data-qc-091225")

#Read in cleaned participant data (this cleaned data is from the `PID cleaning script.R`)
data <- read_csv("clean-data.csv")[,-1]


#add age groupings
age_groups_added <- data %>% mutate(age_group = case_when(as.numeric(age) %in% c(2:4) ~ "Pre-school children (2-4 years)",
                                                          as.numeric(age) %in% c(5:17) ~ "School children (5-17 years)",
                                                          as.numeric(age) > 17 ~ "Adults (18+ years)",
                                                          as.numeric(age) < 2 ~ "0-1 years",
                                                          TRUE ~ NA))

#Get sample size by village
sample_size <- age_groups_added %>% group_by(village_code, age_group) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = age_group, values_from = total) %>%
  #rename(`Missing Age Data` = `NA`) %>% #if there are NAs
  mutate(`Missing Age Data` = 0) %>% #if there are no NAs
  replace(is.na(.), 0) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

#If `0-1 years` column is missing from the sample size dataframe (which only happens if there is no individuals 
#0-1 years old), add the column to the dataframe
if (!"0-1 years" %in% names(sample_size)) {sample_size$`0-1 years` <- 0}

#Write to .csv
write.csv(sample_size[,c(1,6,2,3,4,7,5)], "village-sample-sizes-report.csv")

