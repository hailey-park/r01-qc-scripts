# Automated REDCap Quality Check (QC) Scripts for R01 Study 

This repository contains code to run the automated QC scripts for the REDCap HOTSPOTS databases (both participant-level and community-level). 


## Data Access
These scripts use data directly exported from the REDCap databases. Please talk with Nathan first to confirm IRB status and REDCap database access privileges. For the participant-level database, you need to export both the raw data (e.g., "HOTSPOTParticipantle_DATA_[DATE & TIME OF EXPORT].csv") and the associated labels (e.g., "HOTSPOTParticipantle_DATA_LABELS_[DATE & TIME OF EXPORT].csv"). For the community-level database, you only need to export the raw data (e.g., "HOTSPOTCommunitySurv_DATA_[DATE & TIME OF EXPORT].csv"). 

NOTE: Since we split the participant-level database for the Agboville district into 2 databases, you will also need to pull the raw data and associated label data from the smaller database too (for villages 4, 11, 13, 14).


## Set Up and Structure
After downloading the datasets from REDCap and the R scripts in this repo, put everything in one folder (naming convention I use is `r01-qc-scripts`. I would recommend a folder organization structure shown below (this is how my file paths are set up, but of course you can change this):
* `r01-qc-scripts`: general folder name for QC work
  
  * `PID cleaning script.R`: This script generates a "cleaned" participant-level database. It addresses issues of incorrect PID (wrong digits, wrong village code) and duplicates. The script is split into 6 sections. In sections 1 and 2, it outputs two .csv files, one for all the records with incorrect digits in the PID (`PID-incorrect-digits.csv`), and the other for all the records with discrepancies with the village code (`PID-incorrect-villageID.csv`). Section 3 requires manual inspection of both .csv files to review and correct the PIDs, which should be saved as separate .csv files (`PID-incorrect-digits-reviewed.csv`, `PID-incorrect-villageID-reviewed.csv`). Section 4 is merging the corrected PIDs back with the original database. Section 5 and 6 identifies duplicate records (saved as output .csv file `PID-all-duplicates.csv`) and removes duplicate records from the database (saved as output .csv file `PID-duplicates-to-delete.csv`). Finally, it outputs a .csv file called `clean-data.csv` which is the cleaned database. 

  * `village sample size report.R`: This script generates a sample size report of each age group for across all the villages. It outputs a .csv file called `village-sample-sizes-report.csv`. The `PID cleaning script.R` script must be run first to generate the cleaned participant-level database used for this village sample size report.

  * `r01 participant data check.R`: This script creates a participant-level QC report doc for a specified village. You must manually specify the village that you want to run the QC report on. All QC report docs are populated in the `village-qc-reports` folder, which is inside the `data-qc-[DATE]` folder.

  * `r01 community data check.R`: This script is split into 2 sections. The first section generates a QC report doc (`QC-report-community-[DATE].doc`) for the community-level database to check for anomaly records (e.g., duplicates, null, mispelled, missing) and check for completeness of forms. The second section generates a miracidia report (`QC-report-miracidia-[DATE].csv`)
    
  * `data-qc-[DATE]`: this folder will hold the raw REDCap datasets and QC output files for the qc on this date. 
    * `HOTSPOTParticipantle_DATA_[DATE & TIME OF EXPORT].csv`
    * `HOTSPOTParticipantle_DATA_LABELS_[DATE & TIME OF EXPORT].csv`
    * `HOTSPOTCommunitySurv_DATA_[DATE & TIME OF EXPORT].csv`
    * `village-qc-reports`: This folder will be populated with the village-specific participant-level data QC reports. The code to create this folder is already included in the automated script.
      
  * `data-qc-[FUTURE DATE]`: you can keep making new folders for future QCs. 


Most of these scripts are automated, but I have commented in the scripts "TO DO: ____" for any part that requires manual changes. This includes updating file names, file paths, village specificiations, etc.

## Contact 
Please direct any questions to the study PI:

Nathan Lo, Stanford University, contact: Nathan.Lo@stanford.edu
