## Script name: Munge script 
##
## Purpose of script: Import and wrangle data
##
## Date Created: 2023-01-05
##
## Notes:
##   

## Load packages: ---------------------------

library(tidyverse)
library(here)
library(janitor)
library(magrittr)
library(randomForest)
library(naniar)
library(MatchIt)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("recode", "dplyr")


# Clear workspace
rm(list = ls())


# Querying add'l data ---------------------------

input_file <- here("data/original_INQuery_Output_2022.07.12_09.59_.xlsx")

# Get list of unique INDDIDs from initial query
original_query <- readxl::read_excel(input_file)

# Count how many total 
n_total <- 
  original_query %>% 
  select(INDDID) %>% 
  distinct() %>% 
  nrow() %T>%
  print()

# Count how many controls:
n_controls<-
  original_query %>% 
  filter(ClinicalPhenotype1=="Normal") %>% 
  select(INDDID) %>% 
  distinct() %>% 
  nrow() %T>%
  print()

# Count how many cases 
# I think those enrolled through UDALL had "ClinicalPhenotype1" ==NA

# Non-UDALL cases 
n_cases_adrc <-
  original_query %>% 
  filter(ClinicalPhenotype1!="Normal") %>% 
  select(INDDID) %>% 
  distinct() %>% 
  nrow() %T>%
  print()

cases_adrc <-
  original_query %>% 
  filter(ClinicalPhenotype1!="Normal") %>% 
  select(INDDID)

# UDALL cases 
n_cases_udall <-
  original_query %>% 
  filter(is.na(ClinicalPhenotype1)==T) %>% 
  select(INDDID) %>% 
  distinct() %>% 
  nrow() %T>%
  print()

cases_udall <-
  original_query %>% 
  filter(is.na(ClinicalPhenotype1)==T) %>% 
  select(INDDID)

# Total cases:
n_cases_all <- sum(n_cases_adrc, n_cases_udall) %T>%
  print()

# This should equal 0
n_total - sum(n_controls,  n_cases_all)


# Save csv of unique inddids ----------------
# All subjects
output_file <- here("reports", "inddids_all.csv")

inddids_all <- 
  original_query %>% 
  select(INDDID) %>% 
  distinct()

write_csv(inddids_all, file = output_file)

# Controls
output_file_2 <- here("reports", "inddids_controls.csv")

inddids_controls <- 
  original_query %>% 
  filter(ClinicalPhenotype1=="Normal") %>% 
  select(INDDID, diagnosis = ClinicalPhenotype1) %>% 
  distinct()

write_csv(inddids_controls, file = output_file_2)

# Cases
output_file_3 <- here("reports", "inddids_cases.csv")

inddids_cases <- 
  original_query %>% 
  filter(ClinicalPhenotype1!="Normal" | is.na(ClinicalPhenotype1)==T) %>%
  select(INDDID) %>% 
  distinct()

write_csv(inddids_cases, file = output_file_3)

n_cases <- inddids_cases %>% nrow() 

# #clear workspace
# rm(list = ls())

# Collapse diagnoses ---------------------------

input_file <- here("data/original_INQuery_Output_2022.07.12_09.59_.xlsx")

# Rename INQuery output

clinical <- readxl::read_excel(input_file) %>% 
  mutate(INDDID = as.character(INDDID))

# Assign "MotorDx" and "CognitiveDx" to ClinicalPhenotype_Sum for
# those without "ClinicalPhenotype1"

clinical2 <- 
  clinical %>%
  replace_na(list(MotorDx1 = "NA", CognitiveDx = "NA")) %>% #to be able to concatenate them later
  mutate(ClinicalPhenotype_sum = 
           ifelse(
             is.na(ClinicalPhenotype1) == T, 
             str_c(MotorDx1, CognitiveDx, sep = " "),
             ClinicalPhenotype1)) %>% 
  mutate(ClinicalPhenotype_sum = recode(ClinicalPhenotype_sum, 
                                        `Parkinson Disease Normal` = "PD",
                                        `Parkinson Disease MCI` = "PD-MCI",
                                        `Parkinson Disease NA` = "PD",
                                        `DLB MCI` = "DLB-MCI",
                                        `DLB NA` = "DLB")) %>% 
  select(-ClinicalPhenotype1, MotorDx1, CognitiveDx)

# Recode to condense groups
clinical3 <- clinical2 %>% 
  select(INDDID,
         ClinicalPhenotype_sum,
         everything()) %>% 
  mutate(ClinicalPhenotype_sum = recode(ClinicalPhenotype_sum,   # condense dx groups 
                                        `Parkinson Disease` = "PD",
                                        `Parkinson Disease Dementia` = "PDD",
                                        `DLB-MCI` = "DLB", `PD-MCI` = "PD"),
         INDDID = as.character(INDDID)) %>% 
  select(INDDID, 
         diagnosis = ClinicalPhenotype_sum) %>% 
  distinct() #keep only unique entries

# Check if cases or controls lost with this step
### TO DO for each step of munge file

# Save as "id_diagnosis"
output_file <- here("objects/id_diagnosis.RDS")

saveRDS(clinical3, file = output_file)

# N by diagnosis
n_pd <- clinical3 %>% filter(diagnosis=="PD") %>% nrow()

n_pdd <- clinical3 %>% filter(diagnosis=="PDD") %>% nrow()

n_dlb <- clinical3 %>% filter(diagnosis=="DLB") %>% nrow()

# Import demographics -----------
# Demographics
input_file <- here("data/demographics_all_(2023.02.28 15.24).xlsx")

demographics <- readxl::read_excel(input_file) %>% 
  mutate(INDDID = as.character(INDDID),
         Education = na_if(Education, 999)) %>% 
  select(INDDID, YOB, Race, Sex, 
           Education)

# Save output 
output_file <- here("objects/demographics.RDS")
saveRDS(demographics, file = output_file)

# Cases autopsy, PET and CSF ---------
# Autopsy 
input_file <- here("data/autopsy_cases_(2023.02.28 15.59).xlsx")

autopsy_cases <- readxl::read_excel(input_file) %>% 
  mutate(INDDID = as.character(INDDID)) %>%
  select(INDDID, AutopsyDate,
         NPDx1, NPDx2, Braak03, 
         ABeta, Braak06, CERAD) %>% 
  filter(is.na(NPDx1)==F)

# Save output 
output_file <- here("objects/autopsy_cases.RDS")
saveRDS(autopsy_cases, file = output_file)

# PET 
input_file <- here("data/pet_cases_(2023.02.28 16.01).xlsx")

pet_cases <- readxl::read_excel(input_file) %>% 
  mutate(INDDID = as.character(INDDID), 
         PET_read = Clinical_Read) %>% 
  select(INDDID, PETDate, 
         PETTracer, PET_read) %>% 
  filter((PETTracer %in% c("Florbetaben (amyloid)", "Florbetapir (amyloid)") & 
            is.na(PET_read) == F)) 

# Save output 
output_file <- here("objects/pet_cases.RDS")
saveRDS(pet_cases, file = output_file)

# CSF
# original import
input_file <- here("data/csf_cases_(2023.02.28 16.00).xlsx")

csf_cases <- readxl::read_excel(input_file) %>% 
  mutate(INDDID = as.character(INDDID),
         ttau = LuminexTTau,
         abeta42 = LuminexAbeta42,
         tau_abeta42_ratio = ttau / abeta42) %>% 
  select(INDDID, CSFDate, ttau, abeta42, tau_abeta42_ratio) %>% 
  filter(is.na(CSFDate) == FALSE)

# Save output 
output_file <- here("objects/csf_cases.RDS")
saveRDS(csf_cases, file = output_file)


# Controls autopsy, PET and CSF ---------
# PET 
input_file <- here("data/pet_controls_(2023.02.28 16.15).xlsx")

pet_controls <- readxl::read_excel(input_file) %>% 
  mutate(INDDID = as.character(INDDID), 
         PET_read = Clinical_Read) %>% 
  select(INDDID, PETDate, 
         PETTracer, PET_read) %>% 
  filter((PETTracer %in% c("Florbetaben (amyloid)", "Florbetapir (amyloid)") & 
            is.na(PET_read) == F)) 

# Controls with amyloid PET completed:
pet_controls %>% 
  filter(PETTracer %in% c("Florbetaben (amyloid)", "Florbetapir (amyloid)")) %>% 
  select(INDDID) %>% distinct() %>% nrow()

n_controls_w_pet_complete <- n_distinct(pet_controls)

# Controls with amyloid PET completed and read:
pet_controls %>% filter((PETTracer %in% c("Florbetaben (amyloid)", "Florbetapir (amyloid)") & 
                        is.na(PET_read) == F)) %>% 
  select(INDDID) %>% distinct() %>% nrow()

# Save output 
output_file <- here("objects/pet_controls.RDS")
saveRDS(pet_controls, file = output_file)

# input_file <- here("data/demographic_biomarker_autopsy_(2022.12.01 21.51).xlsx")
# 
# dem_biomarkers <- readxl::read_excel(input_file) %>% 
#   mutate(INDDID = as.character(INDDID),
#          Education = na_if(Education, 999),
#          PET_read = Clinical_Read,
#          tau_abeta42_ratio = LuminexTTauAbetaRatio)
# 
# 
# dem_biomarkers2 <- dem_biomarkers %>% 
#   select(c(INDDID, YOB, Race, Sex, 
#            Education, AutopsyDate,
#            NPDx1, Braak03, 
#            ABeta, Braak06, CERAD, 
#            CSFDate, tau_abeta42_ratio, 
#            PETDate, 
#            PETTracer, PET_read = Clinical_Read
#   ))
# 
# # Save output 
# output_file <- here("objects/dem_biomarkers2.RDS")
# 
# saveRDS(dem_biomarkers2, file = output_file)

# Import MRI metadata ------------------------
input_file <- here("data/mri_bids_(2022.12.02 11.21).xlsx")

mri_bids_data <- readxl::read_excel(input_file, col_types = "text" ) %>% 
  mutate(INDDID = as.character(INDDID))

sessions_ashs <- mri_bids_data %>% select(INDDID, FlywheelSessionLabel) %>% distinct()

# write_csv(sessions_ashs, file= "sessions_info_lbd.csv")

mri_bids_data_2 <- 
  mri_bids_data %>% 
  select(INDDID, FlywheelSessionLabel, FlywheelProjectLabel, 
         FlywheelAcquisitionMeasurement, FlywheelAcquisitionLabel, 
         FlywheelAcquisitionFeatures, FlywheelAcquisitionInternalID,
         DicomInstitutionName, DicomStationName,
         DicomStudyInstanceUID,
         DicomSeriesInstanceUID,
         DicomSliceThickness, DicomPixelSpacingX, DicomPixelSpacingY,
         DicomSequenceName,
         DicomMagneticFieldStrength, 
         DicomRepetitionTime, 
         DicomEchoTime, 
         DicomSpacingBetweenSlices, 
         FileName,
         BIDSFlywheelAcquisitionInternalID,
         BidsFilename,
         BidsPath,
         BidsFolder,
         Expr2)

# T1
output_file1 <- here("objects/mri_bids_data_t1.RDS")

mri_bids_data_t1 <- mri_bids_data_2 %>% 
  filter(FlywheelAcquisitionMeasurement == 'T1')

saveRDS(mri_bids_data_t1, output_file1)

# T2

output_file2 <- here("objects/mri_bids_data_t2.RDS")

mri_bids_data_t2 <- mri_bids_data_2 %>% 
  filter(FlywheelAcquisitionMeasurement == 'T2')

saveRDS(mri_bids_data_t2, output_file2)

# Clinical testing -----------------------
# MMSE
mmse <- readxl::read_excel(here("data/mmse_(2022.10.26 15.44).xlsx")) %>% 
  mutate(INDDID = as.character(INDDID))

saveRDS(mmse, file = here("objects/clinical_testing/mmse.RDS"))

# MOCA
moca <- readxl::read_excel(here("data/moca_(2022.10.26 16.08).xlsx")) %>% 
  mutate(INDDID = as.character(INDDID))

saveRDS(moca, file = here("objects/clinical_testing/moca.RDS"))

# Verbal learning testing
vlt <- readxl::read_excel(here("data/vlt_(2022.11.08 09.42).xlsx")) %>% 
  mutate(INDDID = as.character(INDDID))

saveRDS(vlt, file = here("objects/clinical_testing/vlt_raw.RDS"))

# Epworth
epworth <- readxl::read_excel(here("data/epworth_(2022.10.04 17.59).xlsx")) %>% 
  mutate(INDDID = as.character(INDDID))

saveRDS(epworth, file = here("objects/clinical_testing/ess.RDS"))

# Rey figure
rey_figure <- readxl::read_excel("data/rey_figure_(2023.01.24 13.51).xlsx") %>% 
  mutate(INDDID = as.character(INDDID))

saveRDS(rey_figure, file = here("objects/clinical_testing/rey_figure.RDS"))

# #clear workspace
# rm(list = ls())

# ASHS import --------------------
input_file <- here("data/ashs_all_output.csv")
  
ashs <- read_csv(input_file) %>% 
  mutate(INDDID = as.character(INDDID)) %>% 
  filter(Version != "0.1.1") %>% #remove output from ASHS 0.1.1
  rename(FlywheelSessionLabel = MRISession)

# Split ASHS into T1 and T2 
ashs_t1 <- ashs %>% 
  filter(Version == '0.2.0_t1') %>% 
  mutate(ASHS_import_batch = "1") %>% 
  distinct()

# Import additional ASHS T1 data from Jesse's run in 3/2023
input_file2 <- here("data/ashs_t1_output2_jsc_20230321.csv")

col_names <- c("Input", "Hemisphere", "Region", "Label", "Volume")
ashs_t1_extras <- read_csv(input_file2, col_names = col_names)

ashs_t1_extras %<>% mutate(INDDID = str_extract(Input, "[0-9]+"),
                          FlywheelSessionLabel = str_extract(Input, "[0-9]+x[0-9]+"),
                          Version = "0.2.0_t1",
                          ASHS_import_batch = "2") %>% 
  select(INDDID, FlywheelSessionLabel, Version, everything()) %>% 
  distinct()

# Look how many duplicates in first and second batch of T1 imports:
nrow(ashs_t1) + nrow(ashs_t1_extras)

# rbind second T1 ASHS import to the first:
ashs_t1 <- rbind(ashs_t1, ashs_t1_extras)

# Check to see if any duplicates by seeing if lose any rows with distinct()
ashs_t1 %>% distinct() %>% nrow()

# Save T1 
output_file1 <- here("objects/ashs_t1.RDS")

saveRDS(ashs_t1, file = output_file1)

# Save T2 
output_file2 <- here("objects/ashs_t2.RDS")

ashs_t2 <- ashs %>% 
  filter(Version == '0.2.0') %>% 
  distinct()

saveRDS(ashs_t2, file = output_file2)

# #clear workspace
# rm(list = ls())

# Classify all subjects as ad_present or absent: -----------

# Number of cases/controls from original query
diagnosis <- readRDS(here("objects/id_diagnosis.RDS"))

diagnosis %>% select(diagnosis) %>% table()

cases <- 
  diagnosis %>% 
  filter(diagnosis != "Normal")

cases %>% 
  select(diagnosis) %>% table()

# input
# ashs_wide <- readRDS(here("objects/ashs_wide.RDS"))
# dem_biomarkers2 <- readRDS("objects/dem_biomarkers2.RDS")

# Start with autopsy ------
autopsy <- readRDS(here("objects/autopsy_cases.RDS"))

# Number of cases with autopsy available:
autopsy %>% 
  filter(is.na(NPDx1) == FALSE) %>% 
  select(INDDID) %>% distinct() %>% nrow()

ad_classification_autopsy <- 
  cases %>% 
  left_join(autopsy) %>% 
  distinct()

# Convert Braak03 to Braak06
ad_classification_autopsy$Braak06[is.na(ad_classification_autopsy$Braak06) == TRUE & ad_classification_autopsy$Braak03 == "0"] <- "0"
ad_classification_autopsy$Braak06[is.na(ad_classification_autopsy$Braak06) == TRUE & ad_classification_autopsy$Braak03 == "1"] <- "2"
ad_classification_autopsy$Braak06[is.na(ad_classification_autopsy$Braak06) == TRUE & ad_classification_autopsy$Braak03 == "2"] <- "4"
ad_classification_autopsy$Braak06[is.na(ad_classification_autopsy$Braak06) == TRUE & ad_classification_autopsy$Braak03 == "3"] <- "6"

# Add column with ABC level
ad_classification_autopsy$abc[ad_classification_autopsy$ABeta == "0" & ad_classification_autopsy$CERAD == "0"] <- "Not"
ad_classification_autopsy$abc[ad_classification_autopsy$ABeta %in% c("1","2") & ad_classification_autopsy$CERAD %in% c("0","1")] <- "Low"
ad_classification_autopsy$abc[ad_classification_autopsy$ABeta %in% c("1","2") & ad_classification_autopsy$CERAD %in% c("2","3") & ad_classification_autopsy$Braak06 %in% c("0","1", "2")] <- "Low"
ad_classification_autopsy$abc[ad_classification_autopsy$ABeta %in% c("1","2")  & ad_classification_autopsy$CERAD %in% c("2","3") & ad_classification_autopsy$Braak06 %in% c("3","4","5","6")] <- "Intermediate"
ad_classification_autopsy$abc[ad_classification_autopsy$ABeta == "3" & ad_classification_autopsy$Braak06 %in% c("0","1", "2")] <- "Low"
ad_classification_autopsy$abc[ad_classification_autopsy$ABeta == "3" & ad_classification_autopsy$Braak06 %in% c("3","4","5","6")] <- "Intermediate"
ad_classification_autopsy$abc[ad_classification_autopsy$ABeta %in% c("4","5") & ad_classification_autopsy$CERAD %in% c("0","1") & ad_classification_autopsy$Braak06 %in% c("0","1", "2")] <- "Low"
ad_classification_autopsy$abc[ad_classification_autopsy$ABeta %in% c("4","5") & ad_classification_autopsy$CERAD %in% c("0","1") & ad_classification_autopsy$Braak06 %in% c("3","4","5","6")] <- "Intermediate"
ad_classification_autopsy$abc[ad_classification_autopsy$ABeta %in% c("4","5") & ad_classification_autopsy$CERAD %in% c("2","3") & ad_classification_autopsy$Braak06 %in% c("0","1", "2")] <- "Low"
ad_classification_autopsy$abc[ad_classification_autopsy$ABeta %in% c("4","5") & ad_classification_autopsy$CERAD %in% c("2","3") & ad_classification_autopsy$Braak06 %in% c("3","4")] <- "Intermediate"
ad_classification_autopsy$abc[ad_classification_autopsy$ABeta %in% c("4","5") & ad_classification_autopsy$CERAD %in% c("2","3") & ad_classification_autopsy$Braak06 %in% c("5","6")] <- "High"

# Classify as ad_present or not
# Autopsy: 
ad_classification_autopsy$ad_present_autopsy[ad_classification_autopsy$abc %in% c("Intermediate", "High")] <- TRUE
ad_classification_autopsy$ad_present_autopsy[ad_classification_autopsy$abc %in% c("Not", "Low")] <- FALSE

# How many subjects are classified with autopsy? 
# Have autopsy 
ad_classification_autopsy %>% filter(is.na(ad_present_autopsy) == FALSE)

# Cases to exclude due to alternative neuropath dx
non_lbd_autopsy <-
  ad_classification_autopsy %>% filter(is.na(NPDx1) == FALSE &
                                     ((NPDx1 != "Lewy body disease" &
                                         is.na(NPDx2 == TRUE)) | 
                                        (NPDx1 != "Lewy body disease" & 
                                           NPDx2 != "Lewy body disease"))) %T>% 
  print() 

n_exclude_autopsy <- non_lbd_autopsy %>% nrow()

npdx1_exclude_autopsy <- non_lbd_autopsy %>% select(NPDx1) %>% table()


# Create list of cases to remove:

cases_to_remove <- non_lbd_autopsy$INDDID

# Cases classified:
classified_autopsy <-
  ad_classification_autopsy %>% 
  filter(is.na(NPDx1) == FALSE & 
           !(INDDID %in% non_lbd_autopsy$INDDID)) 

n_classified_autopsy <- 
  classified_autopsy %>% 
  select(INDDID) %>% 
  distinct() %>% 
  nrow()

# Then for PET -------
pet <- readRDS(here("objects/pet_cases.RDS"))

ad_classification_pet <- 
  cases %>% 
  filter(!INDDID %in% cases_to_remove &
           !INDDID %in% classified_autopsy$INDDID) %>% 
  left_join(pet) %>% 
  distinct()

# PET
ad_classification_pet$ad_present_pet[ad_classification_pet$PET_read == "Positive"] <- TRUE
ad_classification_pet$ad_present_pet[ad_classification_pet$PET_read == "Negative" ] <- FALSE

# Cases with amyloid PET completed:
n_cases_with_pet <-
  pet %>% filter(PETTracer %in% c("Florbetaben (amyloid)", "Florbetapir (amyloid)")) %>% 
  select(INDDID) %>% distinct() %T>%
  print() %>% 
  nrow()

# Cases with amyloid PET completed and read:
n_classified_pet <-
  pet %>% filter((PETTracer %in% c("Florbetaben (amyloid)", "Florbetapir (amyloid)") & 
                        is.na(PET_read) == F)) %>% 
  select(INDDID) %>% distinct() %T>%
  print() %>%
  nrow()

# Cases classified:
ad_classification_pet %>% 
  filter(is.na(PET_read)) %T>% 
  print() %>% 
  nrow()

# And for CSF -------
csf <- readRDS(here("objects/csf_cases.RDS"))

ad_classification_csf <- 
  cases %>% 
  left_join(csf) %>% 
  filter(!INDDID %in% cases_to_remove &
           !INDDID %in% classified_autopsy$INDDID) %>% 
  filter(is.na(ttau)==F) %>% 
  distinct()

# CSF
ad_classification_csf$ad_present_csf[ad_classification_csf$tau_abeta42_ratio > 0.3] <- TRUE
ad_classification_csf$ad_present_csf[ad_classification_csf$tau_abeta42_ratio <= 0.3] <- FALSE

# Cases classifiable with CSF:
csf_classifiable <-
  ad_classification_csf %>% 
  filter(is.na(tau_abeta42_ratio) == FALSE) 

n_cases_with_csf <- 
  csf_classifiable %>% 
  select(INDDID) %>% distinct() %T>% 
  print() %>% 
  nrow()
  
# Among subjects with multiple CSF measurements, were they incongruous?
multiple_csf <- 
  csf_classifiable%>% 
  count(INDDID) %>% 
  filter(n>1) %>% 
  select(INDDID) %>% 
  distinct()

n_multiple_csf <- nrow(multiple_csf)

conflicting_csf <-
  csf_classifiable %>% 
  filter(INDDID %in% multiple_csf$INDDID) %>% 
  group_by(INDDID) %>% 
  mutate(ad_congruous = mean(ad_present_csf)) %>% 
  filter(ad_congruous > 0 & ad_congruous < 1) %>%
  arrange()

conflicting_csf_ids <- 
  conflicting_csf %>% 
  select(INDDID) %>% 
  distinct()

n_conflicting_csf <- conflicting_csf_ids %>% nrow()

cases_to_remove <- union(cases_to_remove, conflicting_csf$INDDID)

n_classified_csf <- 
  csf_classifiable %>% 
  filter(!INDDID %in% conflicting_csf$INDDID) %>% 
  select(INDDID) %>% 
  distinct() %>% 
  nrow() %T>%
  print()

# Merge the three AD markers tables: ------
ad_classification <- 
  cases %>% 
  left_join(ad_classification_autopsy) %>% 
  left_join(ad_classification_pet) %>% 
  left_join(ad_classification_csf) %>% 
  filter(!INDDID %in% cases_to_remove) %>% 
  select(1:2, contains("ad_present"))

# Group by INDDID and mark as ad_present if any marker is positive
ad_classification2 <-
  ad_classification %>%
  group_by(INDDID) %>% 
  mutate(
    ad_present = 
           mean(
             c(ad_present_autopsy, ad_present_pet, ad_present_csf), na.rm = T)) %>% 
  ungroup() %>% 
  filter(is.na(ad_present)==F) %>%
  distinct() 

# Identify duplicates and make sure ad_present does not conflict:
ad_classification2 %>% 
  group_by(INDDID) %>% 
  filter(n()>1) %>% 
  ungroup()

# Then use slice to remove duplicate:
ad_classification2 %<>% 
  group_by(INDDID) %>% 
  slice_head(n=1) %>% 
  ungroup()

# Any subjects with conflicting AD markers? 
# (Not anymore since I didn't look at the CSF of those that were already 
# classified with autopsy)

conflicting_autopsy_csf <-
  ad_classification2 %>% 
  filter(ad_present != 1 & ad_present != 0)
  
autopsy_cases$INDDID <-as.character(autopsy_cases$INDDID)

# Look at CSF dates 
conflicting_autopsy_csf %>% 
  left_join(csf_cases) %>% 
  left_join(autopsy_cases) %>% 
  mutate(autopsy_csf_intvl = AutopsyDate - CSFDate) %>% 
  select(1:3, 5:6, tau_abeta42_ratio, autopsy_csf_intvl) %>% 
  mutate(autopsy_csf_intvl_yrs = as.numeric(autopsy_csf_intvl)/365)

# For those with conflicting autopsy and csf, use the autopsy diagnosis for ad_present
# First check how many ad_present
ad_classification2 %>% 
  select(ad_present) %>% 
  table()

# For those with conflicting ad_present (i.e. = 0.5), use autopsy diagnosis
ad_classification3 <- 
  ad_classification2 %>% 
  mutate(ad_present = case_when(
    ad_present == 0 ~ FALSE,
    ad_present == 1 ~ TRUE,
    ad_present == 0.5 ~ ad_present_autopsy
    )
  )
    
# Check result for those with conflicting autopsy and csf:
conflicting_autopsy_csf %>% 
  select(INDDID, ad_present) %>% 
  left_join(ad_classification3, by = "INDDID") %>% 
  select(1,3,2,ad_present.y, everything(), -ad_present_pet)

# Update list of cases to remove
cases_to_remove <- union(cases_to_remove, conflicting_autopsy_csf$INDDID)

# Who had no ad_classification markers? -------
any_ad_marker <- ad_classification %>% 
  group_by(INDDID) %>% 
  summarise(has_marker = 
              !all_na(
                c(ad_present_autopsy, ad_present_pet, ad_present_csf)
              )
  ) 

any_ad_marker %>% select(has_marker) %>% table()

no_markers <- any_ad_marker %>% filter(has_marker==F) %>% 
  select(INDDID) %>% distinct()

n_no_markers <- no_markers %>% nrow()

write_csv(no_markers, file = here("reports/no_markers_ids.csv"))

#Add to list of cases to remove:
cases_to_remove <- union(cases_to_remove, no_markers$INDDID)

# When I looked them up in INDD, it looks like it was a combination of UDALL
# patients with no CSF and ADC patients with CSF collected in the last 2-3 years

# Phenotype of those excluded
no_markers %>% 
  left_join(cases) %>% 
  select(diagnosis) %>% 
  table()

# How many of them have unprocessed CSF?
no_markers %>% 
  left_join(csf) %>% 
  filter(is.na(CSFDate)==F) %>% 
  select(INDDID) %>% 
  distinct() %T>% 
  nrow()

# Number of cases to remove based on ad markers (or lack thereof)
n_cases_remove_due_to_markers <- cases_to_remove %>% n_distinct()

# Final table to use for merging with ASHS data and all subsequent analysis:
ad_classification3 %<>% 
  filter(!INDDID %in% cases_to_remove) 

write_rds(ad_classification3, file = here("objects/ad_classification_final.RDS"))

# Calculate interval between MRI (ASHS output) and AD biomarker/autopsy ---------

# First pivot ASHS output to wide
input_file <- here("objects/ashs_t1.RDS") 

ashs_t1 <- readRDS(input_file)

ashs_wide <- ashs_t1 %>%
  unite(col = region, Hemisphere, Region) %>% 
  select(-Label, -Version) %>%
  distinct() %>% 
  filter(!(Input == "sub-100066_ses-20070926x1018_BrainSegmentation0N4" &
             ASHS_import_batch == "2")) %>% # remove duplicate
  pivot_wider(id_cols = c(INDDID, FlywheelSessionLabel), 
              names_from = region, values_from = Volume)

# For some reason there was one duplicate ASHS T1 session output with slightly
# different volumes btwn the first and second run

# ashs_t1 %>%  unite(col = region, Hemisphere, Region) %>% 
#   select(-Label, -Version) %>%
#   distinct() %>% 
#   dplyr::group_by(INDDID, FlywheelSessionLabel, region) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L) 

duplicate_ashs_output <- ashs_t1 %>% filter(INDDID=="100066")

# Send Jeff the duplicate ashs output to see how this happened since it looks like
# slightly different volumes from the same input file?
write_csv(duplicate_ashs_output, file = "reports/duplicate_ashs_ouput.csv")

saveRDS(ashs_wide, here("objects/ashs_wide.RDS"))

# Create MRI session date variable 
ashs_date <- ashs_wide %>%   
  select(INDDID, FlywheelSessionLabel) %>%
  mutate(session_date = str_sub(FlywheelSessionLabel, 0, 8)) %>% 
  mutate(session_date = as.Date(session_date, format = '%Y%m%d')) 

# Import finalized ad_classification:
input_file <- here("objects/ad_classification_final.RDS")

ad_classification <- read_rds(input_file)

# Merge ASHS with ad_classification df
ad_classified_ashs <- 
  ad_classification %>% 
  left_join(ashs_date)

# Add relevant columns to calculate intervals between biomarkers and MRI

# First pull dates from original dataframes 
autopsy_dates <- autopsy_cases %>% select(INDDID, AutopsyDate) 

pet_dates <- pet_cases %>% select(INDDID, PETDate)

csf_dates <- csf_cases %>% select(INDDID, CSFDate) %>% 
  filter(!INDDID %in% classified_autopsy$INDDID)

# Merge dates 
ad_ashs_dates <- ad_classified_ashs %>% 
  left_join(autopsy_dates) %>% 
  left_join(pet_dates) %>% 
  left_join(csf_dates)

# Calculate time between MRI and biomarker
ad_ashs_dates2 <-
  ad_ashs_dates %>% 
  mutate(intvl = case_when(
    is.na(ad_present_autopsy)==F ~
      as.numeric(as.Date(AutopsyDate) - as.Date(session_date))/365,
    is.na(ad_present_pet)==F ~
      as.numeric(as.Date(PETDate) - as.Date(session_date))/365,
    is.na(ad_present_csf)==F ~
      as.numeric(as.Date(CSFDate) - as.Date(session_date))/365)
  )

ad_ashs_dates3 <- ad_ashs_dates2 %>% filter(is.na(intvl)==F) 

# Select MRIs cases ------

# First identify those with only one MRI to choose
ad_ashs_dates3 %>% group_by(INDDID) %>% filter(n()==1) 

# For those with an autopsy ---------
autopsy_mris <- ad_ashs_dates3 %>% 
  mutate(ad_marker = "autopsy") %>% 
  group_by(INDDID) %>%
  filter(intvl < 5,
         is.na(ad_present_autopsy) == F) %>% 
  filter(intvl == max(intvl)) %>% #take earliest MRI within 5 years of autopsy 
  distinct() %>% 
  ungroup()

# For flow chart:
# Subjects with autopsy available:
ad_ashs_dates3 %>% filter(is.na(ad_present_autopsy)==F) %>% select(INDDID) %>% distinct() %>% nrow()

# Subjects whose autopsy was >5 years from MRI 
autopsy_intvl_5y_or_more <- 
  ad_ashs_dates3 %>% 
  filter(is.na(ad_present_autopsy)==F) %>%
  filter(intvl > 5) %>% 
  select(-ad_present_pet, -ad_present_csf, PETDate, -CSFDate, -intvl)

# See if they have PET available:
any(autopsy_intvl_5y_or_more$INDDID %in% pet$INDDID)

# See if they have CSF available:
any(autopsy_intvl_5y_or_more$INDDID %in% csf$INDDID)

# Join with csf df and recalculate ad dx on basis of csf
autopsy_intvl_5y_or_more %<>% 
  left_join(csf) %>% 
  filter(is.na(CSFDate)==FALSE) %>% 
  mutate(ad_present_csf = ifelse(tau_abeta42_ratio > 0.3, TRUE,
                                 ifelse(tau_abeta42_ratio <= 0.3, FALSE,
                                        NA)
                                 )
  )

# Confirm no discrepancies between autopsy and csf:
conflicting_csf_and_autopsy_5y_or_more <- 
  autopsy_intvl_5y_or_more %>% 
  filter(ad_present_autopsy != ad_present_csf) %>% 
  select(1:3, 13, 7, 9:12)

n_conflicting_csf_and_autopsy_5y_or_more <- conflicting_csf_and_autopsy_5y_or_more %>% select(INDDID) %>% distinct() %>% nrow()

# Add these to cases to remove
n_distinct(cases_to_remove)

cases_to_remove <- union(conflicting_csf_and_autopsy_5y_or_more$INDDID, cases_to_remove)

n_distinct(cases_to_remove)

n_distinct(autopsy_intvl_5y_or_more$INDDID)

autopsy_csf_mris <- autopsy_intvl_5y_or_more %>% 
  mutate(intvl = as.numeric(as.Date(CSFDate) - as.Date(session_date))/365) %>% 
  filter(!INDDID %in% cases_to_remove) %>% 
  group_by(INDDID) %>% 
  mutate(intvl = abs(intvl)) %>% 
  filter(intvl < 5) %>%
  filter(intvl == min(intvl)) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(ad_present_pet = NA, ad_marker = "autopsy_and_csf") %>% 
  select(INDDID, diagnosis, ad_present_autopsy, ad_present_pet, ad_present_csf, ad_present, FlywheelSessionLabel,
         session_date, AutopsyDate, PETDate, CSFDate, intvl, ad_marker)

# Join with rest of autopsy_mris 
autopsy_mris <- rbind(autopsy_mris, autopsy_csf_mris)

n_distinct(autopsy_mris)

# For PET ---------
pet_mris <- ad_ashs_dates3 %>%
  mutate(ad_marker = "pet") %>% 
  filter(INDDID %in% autopsy_mris$INDDID == F,
         is.na(PETDate)==F) %>% 
  group_by(INDDID) %>% 
  mutate(intvl = abs(intvl)) %>% 
  filter(intvl < 5) %>%
  filter(intvl == min(intvl)) %>%
  mutate(CSFDate=NA) %>% 
  distinct() %>% 
  ungroup()

# See if any PET >5 years from MRI 
pet_5y_from_mri <- ad_ashs_dates3 %>%
  filter(INDDID %in% autopsy_mris$INDDID == F &
         is.na(PETDate)==F) %>% 
  group_by(INDDID) %>% 
  mutate(intvl = abs(intvl)) %>% 
  filter(intvl > 5) 

# Do they have PET that is earlier than that? 
all(pet_5y_from_mri$INDDID %in% pet_mris$INDDID)

# For CSF -----------
csf_mris <- ad_ashs_dates3 %>%
  mutate(ad_marker = "csf") %>% 
  filter(INDDID %in% autopsy_mris$INDDID == F,
           INDDID %in% pet_mris$INDDID == F,
         is.na(CSFDate)==F) %>% 
  group_by(INDDID) %>% 
  mutate(intvl = abs(intvl)) %>% 
  filter(intvl < 5) %>%
  filter(intvl == min(intvl)) %>% 
  distinct() %>% 
  ungroup()

# !!!MAKE SURE ONLY EXCLUDING CSF SAMPLES NOT INDIVIDUALS !!!--------
# See if any csf >5 years from MRI 
csf_5y_from_mri <- ad_ashs_dates3 %>%
  filter(INDDID %in% autopsy_mris$INDDID == F &
           INDDID %in% pet_mris$INDDID == F,
         is.na(CSFDate)==F) %>% 
  group_by(INDDID) %>% 
  mutate(intvl = abs(intvl)) %>% 
  filter(intvl > 5) %>% distinct()

# Do they have csf that is earlier than that? 
all(csf_5y_from_mri$INDDID %in% csf_mris$INDDID)

excluded_csf_5y_from_mri <- csf_5y_from_mri %>% filter(!INDDID %in% csf_mris$INDDID) %>% select(INDDID) %>% distinct()

n_excluded_csf_5y_from_mri <- n_distinct(excluded_csf_5y_from_mri$INDDID)

# Combine tables of selected mris 
mri_selected_cases <- bind_rows(autopsy_mris, pet_mris, csf_mris) %>% 
  select(INDDID, session_date, ad_marker, everything()) %>%
  group_by(INDDID) %>%
  distinct() %>% 
  ungroup()

# Save table of dx and ad_present
output_2 <- here("objects/id_dx_ad_present.RDS")

id_dx_ad_present <- ad_ashs_dates3 %>% select(1:2, ad_present) %>% distinct() %>% 
  filter(!INDDID %in% (cases_to_remove))

# How many ids were removed of those who had a biomarker and ashs output?
n_distinct(ad_ashs_dates3$INDDID) - n_distinct(id_dx_ad_present)

saveRDS(id_dx_ad_present, output_2)

# MRIs to use for cases
mri_cases <- mri_selected_cases %>% 
  select(INDDID, diagnosis, ad_present, session_date, ad_marker,
         FlywheelSessionLabel) %>% 
  filter(diagnosis != "Normal") %>% 
  distinct()

rm(mri_selected_cases) # now called mri_cases

# Select MRIs for controls ----------
controls <- 
  inddids_controls %>%
  mutate(INDDID = as.character(INDDID)) %>% 
  left_join(ashs_date) %>% 
  left_join(pet) %>% 
  distinct()  

mri_controls <- 
  controls %>% mutate(ad_present = NA) %>% 
  select(INDDID, diagnosis, ad_present, session_date, FlywheelSessionLabel) %>% 
  mutate(ad_marker = NA,
         .after = session_date) %>% 
  distinct()

# Identify controls with +PET to remove
controls_to_remove_positive_pet <- controls %>% 
  filter(PET_read == "Positive") %>% 
  select(INDDID) %>% 
  distinct()

n_controls_to_remove_positive_pet <- n_distinct(controls_to_remove_positive_pet$INDDID)

# Identify controls with non-missing "Date of onset

input <-readxl::read_excel("data/controls_dz_onset_(2023.01.31 13.51).xlsx", 
                          col_types = "text")
controls_with_onset_pos <-
  input %>% filter(
    if_all(
    .cols = GlobalYearOnset,
    .fns = ~!is.na(.x)
  )
) %>% 
  select(INDDID, contains("Global")) %>% left_join(ashs_date)

n_controls_with_onset_pos <- n_distinct(controls_with_onset_pos)

controls_to_remove <- 
  union(controls_to_remove_positive_pet$INDDID, 
        controls_with_onset_pos$INDDID)

mri_controls2 <- mri_controls %>% 
  filter(INDDID %in% controls_to_remove == F) #remove any normals with +PET

# Total controls removed:
n_distinct(mri_controls2) - n_distinct(mri_controls$INDDID)

# Use matching to find best MRIs/controls --------

# # Select most recent MRI for controls with multiple MRIs
# # I should try to match for date of MRI of cases ideally
# mri_controls3 <- mri_controls2 %>% group_by(INDDID) %>% 
#   slice_max(session_date, n=1, with_ties = FALSE)
# 
# #check time between session date and consent date 
# input <-readxl::read_excel("data/controls_consents_(2023.01.31 14.18).xlsx") %>% 
#   mutate(INDDID = as.character(INDDID))
# 
# control_enrollment_date <-
#   input %>% group_by(INDDID) %>% slice_min(SignedDate, n=1, with_ties = FALSE) %>% 
#   select(INDDID, SignedDate)
# 
# control_time_to_mri <-
#   mri_controls3 %>% 
#   left_join(control_enrollment_date) %>% 
#   mutate(date_diff = as.double(
#     difftime(
#       session_date, SignedDate
#       )
#     )
#     ) %>% 
#   select(INDDID, session_date, SignedDate, date_diff)
# 
# hist(control_time_to_mri$date_diff)
# 
# Remove controls with MRI >3 years from enrollment? --------
# 
# controls_long_time_to_mri <- control_time_to_mri %>% 
#   filter(date_diff < (3*365)) %>% 
#   select(INDDID)

# Merge df of selected mri dates for cases and controls
mri_selected_all <- rbind(mri_cases, mri_controls2)

# Remove problematic cases and controls identified above
cases_and_controls_to_remove <- union(cases_to_remove, controls_to_remove)

n_distinct(mri_selected_all$INDDID)

mri_selected_all %<>% filter(!INDDID %in% c(cases_and_controls_to_remove))

n_distinct(mri_selected_all$INDDID)

mri_selected_cases_table <- 
  mri_selected_all %>% 
  filter(diagnosis!="Normal") %>% 
  mutate(diagnosis = as.factor(diagnosis), ad_marker = as.factor(diagnosis)) %>% 
  select(diagnosis, ad_present, session_date, ad_marker) %>% 
  summary()


# Output
output <- here("objects/mri_selected_all.RDS")

saveRDS(mri_selected_all, output)

# Merge Clinical/MRI metadata w/ ASHS ---------------------------

# input
ashs_wide <- readRDS(file = here("objects/ashs_wide.RDS"))
dim(ashs_wide)

mri_selected_all <- readRDS(file = here("objects/mri_selected_all.RDS"))
dim(mri_selected_all)

# output
output <- here("objects/mri_selected_ashs_all.RDS")

# merge selected mris with ashs output
mri_selected_ashs_all <- 
  mri_selected_all %>% 
  left_join(ashs_wide, by = c("INDDID", "FlywheelSessionLabel"))

# add age at MRI variable
# extract session date
mri_selected_ashs_all$session_year <- stringr::str_sub(mri_selected_ashs_all$session_date, start = 1, end = 4)

# Add YOB and other demographics
mri_selected_ashs_all %<>% left_join(demographics)

mri_selected_ashs_all$age_at_mri <- as.numeric(
  lubridate::parse_date_time(
    mri_selected_ashs_all$session_year, "Y") - 
    lubridate::parse_date_time(mri_selected_ashs_all$YOB, "Y")) / 365 

mri_selected_ashs_all$age_at_mri <- round(mri_selected_ashs_all$age_at_mri, digits = 0) 

# Remove individuals less than 50 at time of MRI 
controls_under_50 <- 
  mri_selected_ashs_all %>% 
  filter(age_at_mri<50) %>% 
  select(1:3, age_at_mri) %>% 
  distinct()

n_controls_under_50 <- n_distinct(controls_under_50$INDDID)

cases_and_controls_to_remove <- union(cases_and_controls_to_remove, controls_under_50$INDDID)

n_distinct(mri_selected_ashs_all$INDDID)

mri_selected_ashs_all %<>% filter(!INDDID %in% cases_and_controls_to_remove)

n_distinct(mri_selected_ashs_all$INDDID)

# set ad_present = "Normal" in controls 
mri_selected_ashs_all$ad_present %<>%  as.character()
mri_selected_ashs_all$ad_present[mri_selected_ashs_all$diagnosis ==
                                   "Normal"] <- "Normal"
mri_selected_ashs_all$ad_present <- factor(
  mri_selected_ashs_all$ad_present, 
  levels = c("Normal", "FALSE", "TRUE"),
  labels = c("Control", "LBD-AD", "LBD+AD"))

saveRDS(mri_selected_ashs_all, output)

rm(mri_selected_ashs_all)

# Clean up variable names and classes ----------
# input
mri_selected_ashs <- read_rds(here("objects/mri_selected_ashs_all.RDS"))

# Initialise the dataset as per the template.
dsname <- "mri_selected_ashs"
ds     <- get(dsname)

# Normalize variable names automatically
# Capture the original variable names for use in plots.

vnames <- names(ds)

# Normalise the variable names.

ds %<>% clean_names(numerals="right")

# Confirm the results are as expected.
names(ds)

# Index the original variable names by the new names.

names(vnames) <- names(ds)

vnames

# Note the available variables.

vars <- names(ds)

# Identify the character variables by index.

ds %>%
  sapply(is.character) %>%
  which() %T>%
  print() ->
  chari

# Identify the character variables by name.
ds %>% 
  names() %>% 
  '['(chari) ->
  charc

# Observe the unique levels.

ds[charc] %>% sapply(unique)

# Collapse race
naniar::any_na(ds$race)
ds$race <- fct_collapse(ds$race, 
                        White = "White", 
                        `Non-White or Unknown` = c("More than One Race", "Black or African American",
                                                   "Asian", "Unknown or Not Reported"))
naniar::any_na(ds$race)

fct_lump_n(ds$race, n=2, other_level = "Other")

# How many diagnoses are represented in the dataset.

ds$diagnosis %>% 
  unique() %>%
  length()

# Here is a list of diagnoses and their frequencies in the dataset 
# (for controls there are multiple MRIs for many individuals - these will be matched subsequently )

ds$diagnosis %>%
  table()

# Convert character to numeric where needed
# Identify the variables to process.

cvars <- c("session_year")

# Check the current class of the variables.

ds[cvars] %>% sapply(class)


# Convert to numeric.

ds[cvars] %<>% sapply(as.numeric)

# Review some random values.

sample(ds$session_year, 10)

# Convert character to factor where needed
# Review the distribution of observations across levels.

ds %>%
  select(diagnosis, sex, race, ad_present) %>%
  sapply(table)


# Note the names of the desired variables.
ds %>% 
  select(diagnosis, sex, race, ad_present) %>% 
  names() %T>%
  print() ->
  vnames

# Convert these variables from character to factor.
ds[vnames] %<>% 
  lapply(factor, ordered = FALSE) %>% 
  data.frame() %>% 
  as_tibble()

# Confirm they are now factors.

ds[vnames] %>% sapply(class)

# Verify the distribution has not changed.

ds %>%
  select(all_of(vnames)) %>%
  sapply(table)

# Numeric Variables
ds %>%
  sapply(is.numeric) %>%
  which() %>%
  names %T>%
  print() ->
  numi

ds[numi] %>% 
  summary()

# Note the identifiers.

id <- c("inddid", "session_date", "flywheel_session_label")

# Initialise ignored variables: identifiers.

ignore <- c(id)

# Heuristic for candidate indentifiers to possibly ignore.

ds[vars] %>%
  sapply(function(x) x %>% unique() %>% length()) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names() %T>%
  print() ->
  ids

# Once we have identified all of the variables to ignore we remove them from our list of variables to use.
# Check the number of variables currently.

length(vars)

# Remove the variables to ignore.

vars <- setdiff(vars, ignore)

# Confirm they are now ignored.

length(vars)

# Identify variables that have a single value.

ds %>%
  select(all_of(vars)) %>%
  sapply(function(x) all(x == x[1L])) %>%
  which() %>%
  names() %T>%
  print() ->
  constants

# Missing values ----------
# Count the number of missing values.
ds %>% is.na() %>% sum()

colSums(is.na(ds))

which(colSums(is.na(ds))>0)

names(which(colSums(is.na(ds))>0))

# Remove rows missing ASHS output 
rows_no_ashs <- ds %>% filter(is.na(left_anterior_hippocampus) == T) 

controls_no_ashs <- rows_no_ashs %>% filter(diagnosis == "Normal") %>% 
  select(inddid) %>% left_join(ds)

n_controls_no_ashs <- n_distinct(controls_no_ashs)

cases_and_controls_to_remove <- union(cases_and_controls_to_remove, controls_no_ashs)

# Remove rows missing ashs output (need to figure out how they snuck in in the first place)
ds %<>% filter(is.na(left_anterior_hippocampus) == F)

# No missing values in the first two columns (INDDID and diagnosis)
ds[1:2] %>% is.na() %>% sum()

# Convert education from 0 to NA for one subject
ds$education[ds$education==0] <- NA

# Impute education for subjects with NA 
ds$education <- ds$education %>% na.roughfix()

# Convert ad_marker from NA to "NA"  
ds[ds$diagnosis=="Normal",] %>% select(ad_marker) %>% table()

ds$ad_marker[ds$diagnosis=="Normal"] <- "NA"

# Recount the number of missing values.
ds %>% is.na() %>% sum()

which(colSums(is.na(ds))>0)

names(which(colSums(is.na(ds))>0)) #these are okay to be NA since these regions are not identified in all MRIs

# Rearrange variables so age is in front of df
ds %<>% select(inddid, ad_present, age_at_mri, everything())


# Save cleaned dataframe for future use
saveRDS(ds, "objects/ashs_raw_cleaned.RDS")

# Matching ---------------
## Planning 
### Select type of effect to be estimated
#Direct effect of "case status" (LBD+/-AD) on MTL volumes

### Selecting a target population
#In individuals with LBD

### Selecting covariates to balance
#"To estimate total causal effects, all covariates must be measured prior to treatment (or otherwise not be affected by the treatment)"

# 1. Age at MRI 
# 2. race
# 3. Sex
# 4. Intracranial volume
# 5. year of mri 
# 6. years of education

## Check initial imbalance
## No matching; constructing a pre-match matchit object
# Values of standardized mean differences and eCDF statistics close to zero and values of variance ratios close to one indicate good balance.


#remove missing covariates for MatchIt
ds <- read_rds("objects/ashs_raw_cleaned.RDS")
d_for_matchit <- ds %>% 
#  filter(is.na(education)==F) %>% 
  mutate(case_status = ifelse(diagnosis == "Normal", 0, 1)) %>% 
  select(inddid, diagnosis, ad_present, case_status, age_at_mri, flywheel_session_label,
         race, sex, left_icv, session_year, education)

m.out0 <- matchit(case_status ~ 
                    age_at_mri +
                    race +
                    sex +
                    left_icv +
                    session_year + 
                    education,
                  data = d_for_matchit,
                  method = NULL, distance = "glm")

# Checking balance prior to matching
summary(m.out0)

## 1:1 NN PS matching w/o replacement
m.out1 <- matchit(case_status ~ 
                    age_at_mri +
                    race +
                    sex +
                    left_icv + 
                    session_year +
                    education,
                  data = d_for_matchit,
                  method = "nearest", distance = "glm",
                  ratio = 1,
                  unit.id ="inddid")


# Checking balance after NN matching
plot(summary(m.out1))

# Since the performance was not very good with NN matching, I tried full matching on a probit PS
library(optmatch)
library(cobalt)
# Full matching on a probit PS
m.out2 <- matchit(case_status ~ 
                    age_at_mri +
                    race +
                    sex +
                  #  left_icv + 
                    session_year +
                    education,
                  data = d_for_matchit,
                  method = "full", 
                  distance = "glm", 
                  link = "probit",
                  unit.id ="inddid")
m.out2

## Checking balance after full matching 
summary(m.out2, un = FALSE)
plot(summary(m.out2))

#Vs partial matching
plot(summary(m.out1))

plot(m.out2, type = "qq", interactive = FALSE,
     which.xs = c("age_at_mri", "sex", "session_year"))

#eCDF plot
plot(m.out2, type = "ecdf", interactive = FALSE,
     which.xs = c("age_at_mri", "sex", "session_year"))

#density plot
plot(m.out2, type = "density", interactive = FALSE,
     which.xs = c("age_at_mri", "sex", "session_year"))

# Matched df 
mF <- m.out2
md <- match.data(mF) 

# select highest weighted mri for each control
md_one_mri_per_control <-
  md %>%
  group_by(inddid) %>% 
  slice_max(weights, with_ties = F) %>% 
  select(-distance, -weights, -subclass) %>% 
  ungroup()

# Rerun weighted matching:
# Full matching on a probit PS
m.out3 <- matchit(case_status ~ 
                    age_at_mri +
                    race +
                    sex +
               #     left_icv + # I think when left_icv is included here we lose to much variance in hippocampal volume/adjusted 
                    session_year +
                    education,
                  data = md_one_mri_per_control,
                  method = "full", 
                  distance = "glm", 
                  link = "probit",
                  unit.id ="inddid")

plot(summary(m.out3))
# Still performs very well

# Full matching performed much better than NN so will use that:
mris_matched_cases_and_controls <- match.data(m.out3) 

saveRDS(mris_matched_cases_and_controls, file = "objects/mris_matched_cases_and_controls.RDS")
 
# Save a copy of IDs and session labels for QC (sent to Chris Olm)
mri_sessions_for_qc <- mris_matched_cases_and_controls %>% select(inddid, flywheel_session_label)

write.csv(mri_sessions_for_qc, "reports/mri_sessions_for_qc_032423.csv")
# Use the same to match LBD+AD with LBD-AD
d_lbd_for_matchit <- d_for_matchit %>% filter(ad_present!="Control")

# Full matching on a probit PS - for consistency I used the same matching strategy to compare LBD+AD to LBD-AD
m_lbd <- matchit(ad_present ~ 
                    age_at_mri +
                    race +
                    sex +
                    #left_icv + 
                    session_year +
                    education,
                  data = d_lbd_for_matchit,
                 method = "full", distance = "glm",
                 link = "probit")
                
## Checking balance after full matching 
plot(summary(m_lbd))

# Full matching performed much better than NN so will use that:
mris_matched_lbd_w_ad_vs_no_ad <- match.data(m_lbd) 

saveRDS(mris_matched_lbd_w_ad_vs_no_ad, file = "objects/mris_matched_lbd_w_ad_vs_no_ad.RDS")

# Use the same to match LBD+AD with LBD-AD


# Combine Anterior and posterior hippocampus to make new variable ---------
ds <- read_rds("objects/ashs_raw_cleaned.RDS")

demographic_colnames <- c("inddid",
                          "diagnosis",
                          "ad_present",
                          "ad_marker",
                          "age_at_mri",
                          "race",
                          "sex",
                          "education",
                          "session_year",
                          "yob",
                          "session_date",
                          "flywheel_session_label")

ds_summed_regions <- ds %>% 
  mutate(left_full_hippocampus = 
           left_anterior_hippocampus + left_posterior_hippocampus,
         right_full_hippocampus = 
           right_anterior_hippocampus + right_posterior_hippocampus,
         bilat_full_hippocamp = left_full_hippocampus + right_full_hippocampus,
         bilat_ant_hippocamp = 
           left_anterior_hippocampus + right_anterior_hippocampus,
         bilat_post_hippocamp = 
           left_posterior_hippocampus + right_posterior_hippocampus,
         bilat_erc = left_erc + right_erc,
         bilat_br_35 = left_br_35 + right_br_35,
         bilat_br_36 = left_br_36 + right_br_36,
         bilat_phc = left_phc + right_phc) %>% 
  select(all_of(demographic_colnames),
         left_anterior_hippocampus,
         left_posterior_hippocampus,
         left_full_hippocampus,
         left_erc,
         left_br_35,
         left_br_36,
         left_phc,
         right_anterior_hippocampus,
         right_posterior_hippocampus,
         right_full_hippocampus, 
         right_erc,
         right_br_35,
         right_br_36,
         right_phc,
         bilat_full_hippocamp,
         bilat_ant_hippocamp,
         bilat_post_hippocamp,
         bilat_erc,
         bilat_br_35,
         bilat_br_36,
         bilat_phc,
         left_icv, 
         everything())
         

saveRDS(ds_summed_regions, file = "objects/ashs_summed_regions.RDS")

# Normalize volumes by dividing by ICV -------------
ds_summed_regions <- read_rds("objects/ashs_summed_regions.RDS")

vol_start <- grep("left_anterior_hippocampus$", colnames(ds_summed_regions)) #column index to begin
vol_end <- grep("left_icv$", colnames(ds_summed_regions)) #column index to end

# copy cols to adjust 
ds_adjusted <- ds_summed_regions[vol_start:vol_end] 

# rename them 
colnames(ds_adjusted) <- paste0(colnames(ds_adjusted), "_adjusted")

# divide by ICV and multiply by 100
ds_adjusted %<>% transmute(.*100/(left_icv_adjusted)) 

ds_adjusted <- cbind(ds_summed_regions, ds_adjusted) %>% 
  select(all_of(demographic_colnames),
         session_year,
         left_anterior_hippocampus,
         left_posterior_hippocampus,
         left_full_hippocampus,
         left_erc,
         left_br_35,
         left_br_36,
         left_phc,
         right_anterior_hippocampus,
         right_posterior_hippocampus,
         right_full_hippocampus, 
         right_erc,
         right_br_35,
         right_br_36,
         right_phc,
         bilat_full_hippocamp,
         bilat_ant_hippocamp,
         bilat_post_hippocamp,
         bilat_erc,
         bilat_br_35,
         bilat_br_36,
         bilat_phc,
         left_icv,
         ends_with("_adjusted"),
         everything())

saveRDS(ds_adjusted, file = "objects/ashs_summed_adjusted_for_icv.RDS")

# # full hippocampus
# ds_full_hippocampus <- read_rds("objects/ds_full_hippocampus.RDS")
# hippo_icv_adjusted <- ds_full_hippocampus
# 
# 
# vol_start <- grep("left_full_hippocampus$", colnames(ds_full_hippocampus)) #column index to begin taking outcome vars
# vol_end <- grep("left_icv$", colnames(ds_full_hippocampus)) #column index to end
# 
# hippo_icv_adjusted <- ds_full_hippocampus[vol_start:vol_end] %>% transmute(.*100/(left_icv)) 
# colnames(hippo_icv_adjusted)<- paste0(colnames(hippo_icv_adjusted), "_adjusted")
# hippo_icv_adjusted
# 
# hippo_icv_adjusted <- cbind(ds_full_hippocampus, hippo_icv_adjusted) %>% 
#   as_tibble()
# 
# saveRDS(hippo_icv_adjusted, file = "objects/hippo_icv_adjusted.RDS")

# Export inddids and session_ids ------------
mri_selected_ashs <- read_rds("objects/mri_selected_ashs_all.RDS")
qc_ids <- mri_selected_ashs %>% select(subject = INDDID, 
                                       session = FlywheelSessionLabel)

write_delim(qc_ids, file = here("qc_ids.txt"))

# Look at the difference between participants with and without ASHS output


# See how many cases with ASHS output have PVLT -------------
verbal_testing_data <- read_rds(here("objects/clinical_testing/vlt_raw.RDS")) 
                                
# Initialise the dataset as per the template.
dsname <- "verbal_testing_data"
ds     <- get(dsname)

ds %>% sample_frac()


# Normalize variable names automatically

ds %<>% clean_names(numerals="right")

# Confirm the results are as expected.

names(ds)

# Note the available variables.

vars <- names(ds)

# Note the identifiers.

id <- c("inddid", "inddid_1", "test_date", "visit_id")

# Initialise ignored variables: identifiers.

ignore <- c(id)

# Identify the character variables by index.

ds %>%
  sapply(is.character) %>%
  which()  ->
  chari

# Identify the character variables by name.

ds %>% 
  names() %>% 
  '['(chari) ->
  charc

# Ignore character variables

ignore <- union(ignore, charc)

#Once we have identified all of the variables to ignore we remove them from our list of variables to use.

# Check the number of variables currently.

length(vars)

# Remove the variables to ignore.

vars <- setdiff(vars, ignore)

# Confirm they are now ignored.

length(vars)

#Numeric Variables
ds[vars] %>%
  sapply(is.numeric) %>%
  which() %>%
  names ->
  numi

ds[numi] %>% 
  summary()

# Save cleaned PVLT df
saveRDS(ds, file = "objects/clinical_testing/pvlt_cleaned_cases_and_controls.RDS")

pvlt_cases_with_ashs_and_ad_markers <- 
  mri_selected_ashs %>%
  select(inddid = INDDID, diagnosis, ad_present) %>%
  left_join(ds) %>% 
  filter(diagnosis !="Normal") %>% 
  filter(!is.na(test_date_1)) %>% # These took the VLT 
  select(inddid, diagnosis, ad_present,
         test_date_1) 

pvlt_cases_with_ashs_and_ad_markers_table <- 
  pvlt_cases_with_ashs_and_ad_markers %>% 
  select(inddid, diagnosis, ad_present) %>% distinct() %>% 
  select(ad_present, diagnosis) %>% 
  mutate(diagnosis = as.factor(diagnosis)) %>% summary()

  
n_pvlt_cases_with_ashs_and_ad_markers <-
  pvlt_cases_with_ashs_and_ad_markers$inddid %>% n_distinct()
  
