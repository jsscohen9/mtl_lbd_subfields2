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
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("recode", "dplyr")


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
  select(INDDID) %>% 
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

# #clear workspace
# rm(list = ls())

# Import demographics ----------------------- 
input_file <- here("data/demographic_biomarker_autopsy_(2022.12.01 21.51).xlsx")

dem_biomarkers <- readxl::read_excel(input_file) %>% 
  mutate(INDDID = as.character(INDDID),
         Education = na_if(Education, 999),
         PET_read = Clinical_Read,
         tau_abeta42_ratio = LuminexTTauAbetaRatio)


dem_biomarkers2 <- dem_biomarkers %>% 
  select(c(INDDID, YOB, Race, Sex, 
           Education, AutopsyDate,
           NPDx1, Braak03, 
           ABeta, Braak06, CERAD, 
           CSFDate, tau_abeta42_ratio, 
           PETDate, 
           PETTracer, PET_read = Clinical_Read
  ))

# Save output 
output_file <- here("objects/dem_biomarkers2.RDS")

saveRDS(dem_biomarkers2, file = output_file)

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
  distinct()

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

# Number of cases/controls from original query'
diagnosis <- readRDS(here("objects/id_diagnosis.RDS"))

diagnosis %>% select(diagnosis) %>% table()

diagnosis_cases <- 
  diagnosis %>% 
  filter(diagnosis != "Normal")

diagnosis_cases %>% 
  select(diagnosis) %>% table()

# input
# ashs_wide <- readRDS(here("objects/ashs_wide.RDS"))
dem_biomarkers2 <- readRDS("objects/dem_biomarkers2.RDS")

ad_classification <- 
  diagnosis_cases %>% 
  left_join(dem_biomarkers2) %>% 
  distinct()

# How many cases don't have AD data? (and will be excluded)
exclude_cases_no_ad_marker <- ad_classification %>%
  group_by(INDDID) %>% 
  filter(diagnosis != "Normal") %>% 
  filter(is.na(NPDx1) == TRUE & 
           is.na(tau_abeta42_ratio) == TRUE &
           is.na(PET_read) == TRUE) %>% 
  ungroup() 

x <- exclude_cases_no_ad_marker %>% 
  select(INDDID) %>%
  distinct() %>% 
  pull(INDDID)
  
# Number of subjects retained 
setdiff(as.numeric(unlist(inddids_cases)), as.numeric(x)) %>% length()

# Phenotype of those excluded
exclude_cases_no_ad_marker %>% 
  left_join(diagnosis_cases) %>% 
  select(diagnosis) %>% 
  table()

cases_to_remove_no_ad_marker <- exclude_cases_no_ad_marker %>% select(INDDID)

# How many of them have unprocessed CSF?
exclude_cases_no_ad_marker %>% 
  left_join(dem_biomarkers2) %>% 
  filter(is.na(CSFDate)==F) %>% 
  select(INDDID) %>% 
  distinct() %>% 
  nrow()

csf_to_run <- exclude_cases_no_ad_marker %>% 
  left_join(dem_biomarkers2) %>% 
  filter(is.na(CSFDate)==F) %>% 
  select(INDDID)

csf_to_run %<>% 
  left_join(diagnosis) %>% 
  left_join(dem_biomarkers2) %>%
  select(1:2, CSFDate, tau_abeta42_ratio) %>% 
  distinct()

# This includes a few subjects who have CSF from one timepoint but not another

write_csv(csf_to_run,"reports/csf_to_run.csv")

# For identifying CSF samples to run, find all LBD subjects with CSF available but not yet run

# Exclude them by removing from ad_classification
ad_classification1a <- 
  ad_classification %>%
  group_by(INDDID) %>% 
  filter(diagnosis != "Normal") %>% 
  filter(is.na(NPDx1) == FALSE | 
           is.na(tau_abeta42_ratio) == FALSE |
           is.na(PET_read) == FALSE) %>% 
  ungroup() 

# View(ad_classification1a)

# I think what is happening is rows with CSF present but tau PET are getting eliminated like 118267

# TO DO - Why excluded:

## Autopsy classifications

# Convert Braak03 to Braak06
ad_classification$Braak06[is.na(ad_classification$Braak06) == TRUE & ad_classification$Braak03 == "0"] <- "0"
ad_classification$Braak06[is.na(ad_classification$Braak06) == TRUE & ad_classification$Braak03 == "1"] <- "2"
ad_classification$Braak06[is.na(ad_classification$Braak06) == TRUE & ad_classification$Braak03 == "2"] <- "4"
ad_classification$Braak06[is.na(ad_classification$Braak06) == TRUE & ad_classification$Braak03 == "3"] <- "6"

# Add column with ABC level
ad_classification$abc[ad_classification$ABeta == "0" & ad_classification$CERAD == "0"] <- "Not"
ad_classification$abc[ad_classification$ABeta %in% c("1","2") & ad_classification$CERAD %in% c("0","1")] <- "Low"
ad_classification$abc[ad_classification$ABeta %in% c("1","2") & ad_classification$CERAD %in% c("2","3") & ad_classification$Braak06 %in% c("0","1", "2")] <- "Low"
ad_classification$abc[ad_classification$ABeta %in% c("1","2")  & ad_classification$CERAD %in% c("2","3") & ad_classification$Braak06 %in% c("3","4","5","6")] <- "Intermediate"
ad_classification$abc[ad_classification$ABeta == "3" & ad_classification$Braak06 %in% c("0","1", "2")] <- "Low"
ad_classification$abc[ad_classification$ABeta == "3" & ad_classification$Braak06 %in% c("3","4","5","6")] <- "Intermediate"
ad_classification$abc[ad_classification$ABeta %in% c("4","5") & ad_classification$CERAD %in% c("0","1") & ad_classification$Braak06 %in% c("0","1", "2")] <- "Low"
ad_classification$abc[ad_classification$ABeta %in% c("4","5") & ad_classification$CERAD %in% c("0","1") & ad_classification$Braak06 %in% c("3","4","5","6")] <- "Intermediate"
ad_classification$abc[ad_classification$ABeta %in% c("4","5") & ad_classification$CERAD %in% c("2","3") & ad_classification$Braak06 %in% c("0","1", "2")] <- "Low"
ad_classification$abc[ad_classification$ABeta %in% c("4","5") & ad_classification$CERAD %in% c("2","3") & ad_classification$Braak06 %in% c("3","4")] <- "Intermediate"
ad_classification$abc[ad_classification$ABeta %in% c("4","5") & ad_classification$CERAD %in% c("2","3") & ad_classification$Braak06 %in% c("5","6")] <- "High"

# Classify as ad_present or not
# Autopsy: 
ad_classification$ad_present[ad_classification$abc %in% c("Intermediate", "High")] <- TRUE
ad_classification$ad_present[ad_classification$abc %in% c("Not", "Low")] <- FALSE

# PET
ad_classification$ad_present[is.na(ad_classification$ad_present) == TRUE & ad_classification$PET_read == "Positive"] <- TRUE
ad_classification$ad_present[is.na(ad_classification$ad_present) == TRUE & ad_classification$PET_read == "Negative" ] <- FALSE

# CSF
ad_classification$ad_present[is.na(ad_classification$ad_present) == TRUE & ad_classification$tau_abeta42_ratio > 0.3] <- TRUE
ad_classification$ad_present[is.na(ad_classification$ad_present) == TRUE & ad_classification$tau_abeta42_ratio <= 0.3] <- FALSE

# Group by INDDID and mark as ad_present if any marker is positive
ad_classification %>% 
  group_by(INDDID) %>% 
  summarise(ad_present = mean(ad_present, na.rm = T)) %>% select(ad_present) %>% table()
  
# Calculate interval between MRI (ASHS output) and AD biomarker/autopsy ---------

# First pivot ASHS output to wide
input_file <- here("objects/ashs_t1.RDS") 

ashs_t1 <- readRDS(input_file)

ashs_wide <- ashs_t1 %>%
  unite(col = region, Hemisphere, Region) %>% 
  select(-Label, -Version) %>%
  distinct() %>% 
  pivot_wider(id_cols = c(INDDID, FlywheelSessionLabel), 
              names_from = region, values_from = Volume)

saveRDS(ashs_wide, here("objects/ashs_wide.RDS"))

# Create MRI session date variable 
ashs_date <- ashs_wide %>%   
  select(INDDID, FlywheelSessionLabel) %>%
  mutate(session_date = str_sub(FlywheelSessionLabel, 0, 8)) %>% 
  mutate(session_date = as.Date(session_date, format = '%Y%m%d')) 

# Merge ASHS date with ad_classification df
ad_classification2 <- 
  ad_classification %>% 
  left_join(ashs_date)



# Calculate time between MRI and biomarker
ad_classification3 <- ad_classification2 %>% 
  mutate(intvl_autopsy = as.numeric(as.Date(AutopsyDate) - as.Date(session_date))/365,
         intvl_pet = ifelse(PETTracer %in% c("Florbetaben (amyloid)", "Florbetapir (amyloid)") == T & 
                              is.na(PET_read) ==FALSE, 
                            as.numeric(as.Date(PETDate) - as.Date(session_date))/365,
                            NA),
         intvl_csf = ifelse(is.na(tau_abeta42_ratio) == F,
                            as.numeric(as.Date(CSFDate) - as.Date(session_date))/365,
                            NA))

# Select columns, filter out other pet tracers
ad_classification4 <- ad_classification3 %>%
  filter((PETTracer %in%
            c("Florbetaben (amyloid)", "Florbetapir (amyloid)") & 
            is.na(PET_read) == F) |
           is.na(PETTracer) == TRUE)



ad_classification_intvl <- ad_classification4 %>% 
  select(INDDID, diagnosis, ad_present,
         session_date, FlywheelSessionLabel, 
         starts_with(c("intvl")), NPDx1, 
         tau_abeta42_ratio, YOB, Race, 
         Sex, Education) %>%
  distinct()

# Output
output_1 <- here("objects/ad_classification_intvl.RDS")

saveRDS(ad_classification_intvl, output_1)

# What cases were lost with this step?
x <- ad_classification %>% select(INDDID) %>% distinct()
y <- ad_classification_intvl %>% select(INDDID) %>% distinct()

setdiff(x, y) %>% left_join(diagnosis_cases) %>% select(diagnosis) %>% table()

ids_lost <- setdiff(x, y)

# I think this is due to the rows with tau PET being removed

# Confirm AD_present is consistent for each INDDID--------------

# Take biomarkers dataframe in which each row has single biomarker test for each INDDID

cases_ambiguous_ad_copath  <-
  ad_classification2 %>% 
  select(INDDID, ad_present, diagnosis, abc, PET_read, tau_abeta42_ratio, CSFDate) %>%
  filter(is.na(ad_present)==F) %>% #remove any rows with NAs
  group_by(INDDID) %>%
  summarise(ad_dx_unique = mean(ad_present)) %>%  #if the mean is not 0 or 1 then it has conflicting ad_present
  filter(ad_dx_unique != 0 & ad_dx_unique != 1) %T>%
  print()

# Select mris ---------------------------
# Decide which MRI to use for which subject

# Input
ad_classification_intvl <- readRDS(here("objects/ad_classification_intvl.RDS"))

# Output
output <- here("objects/mri_selected_all.RDS")

# For those with an autopsy
autopsy_mris <- ad_classification_intvl %>% 
  mutate(ad_marker = "autopsy") %>% 
  group_by(INDDID) %>%
  filter(intvl_autopsy < 5,
         is.na(NPDx1) == F) %>% 
  filter(intvl_autopsy == max(intvl_autopsy)) %>% #take earliest MRI within 5 years of autopsy 
  distinct() %>% 
  ungroup()

# #censored participants with autopsy - make sure that if they are included with other biomarkers that it is concordant with autopsy result
# autopsy_mris_censored <- ad_classification_intvl %>% 
#   select(INDDID, intvl_autopsy, session_date, NPDx1) %>% 
#   group_by(INDDID) %>%
#   filter(intvl_autopsy >= 5,
#          is.na(NPDx1) == F) %>% 
#   filter(intvl_autopsy == max(intvl_autopsy)) %>% #take earliest MRI within 5 years of autopsy 
#   filter(INDDID %in% autopsy_mris$INDDID == F) %>% 
#   distinct() %>% 
#   ungroup()


# For PET
pet_mris <- ad_classification_intvl %>%
  mutate(ad_marker = "pet") %>% 
  filter(INDDID %in% autopsy_mris$INDDID == F) %>% 
  group_by(INDDID) %>% 
  mutate(intvl_pet = abs(intvl_pet)) %>% 
  filter(intvl_pet < 5) %>%
  filter(intvl_pet == min(intvl_pet)) %>%
  distinct() %>% 
  ungroup()

# #censored PET due to >= 5 years 
# pet_mris_censored <- ad_classification_intvl %>%
#   filter(INDDID %in% autopsy_mris$INDDID == F &
#            INDDID %in% pet_mris$INDDID == F) %>% 
#   select(INDDID, intvl_pet, session_date) %>% 
#   group_by(INDDID) %>% 
#   mutate(intvl_pet = abs(intvl_pet)) %>% 
#   filter(intvl_pet >= 5) %>%
#   filter(intvl_pet == min(intvl_pet)) %>% 
#   distinct() %>% 
#   ungroup()


# For CSF
csf_mris <- ad_classification_intvl %>%
  mutate(ad_marker = "csf") %>% 
  filter(INDDID %in% autopsy_mris$INDDID == F &
           INDDID %in% pet_mris$INDDID == F) %>% 
  group_by(INDDID) %>% 
  mutate(intvl_csf = abs(intvl_csf)) %>% 
  filter(intvl_csf < 5) %>%
  filter(intvl_csf == min(intvl_csf)) %>% 
  distinct() %>% 
  ungroup()

# #censored CSF due to >= 5 years
# csf_mris_censored <- ad_classification_intvl %>%
#   filter(INDDID %in% autopsy_mris$INDDID == F &
#            INDDID %in% pet_mris$INDDID == F &
#            INDDID %in% csf_mris$INDDID == F) %>% 
#   select(INDDID, intvl_csf, session_date, tau_abeta42_ratio) %>% 
#   group_by(INDDID) %>% 
#   mutate(intvl_csf = abs(intvl_csf)) %>% 
#   filter(intvl_csf >= 5) %>%
#   filter(intvl_csf == min(intvl_csf)) %>% 
#   distinct() %>% 
#   ungroup()

# Combine tables of selected mris 
mri_selected <- bind_rows(autopsy_mris, pet_mris, csf_mris) %>% 
  select(INDDID, session_date, ad_marker, everything()) %>%
  group_by(INDDID) %>% 
  mutate(intvl_marker = ifelse(ad_marker == "autopsy", #create invtl_marker variable to based on which intvl is used
                               intvl_autopsy,
                               ifelse(ad_marker == "pet",
                                      intvl_pet,
                                      ifelse(ad_marker == "csf",
                                             intvl_csf,
                                             NA))),
          .after = ad_marker) %>% 
  distinct() %>% 
  ungroup()


# Remove cases with NPDx1 != LBD or PET/CSF incongruities ---- 
# For those with autopsy
cases_to_remove_autopsied <-
  ad_classification_intvl %>% filter(is.na(NPDx1) == FALSE,
                        NPDx1 != "Lewy body disease") %T>% 
  print() %>% 
  select(INDDID)

cases_ambiguous_ad_copath %<>% select(INDDID)

cases_to_remove <- union(cases_to_remove_autopsied, cases_ambiguous_ad_copath)

cases_to_remove <- union(cases_to_remove, cases_to_remove_no_ad_marker)

# Save table of dx and ad_present
output_2 <- here("objects/id_dx_ad_present.RDS")

id_dx_ad_present <- ad_classification_intvl %>% select(1:3) %>% distinct() %>% 
  filter(!INDDID %in% c(as.vector(cases_to_remove$INDDID)))

saveRDS(id_dx_ad_present, output_2)

id_dx_ad_present %>% filter(diagnosis != "Normal") %>% 
  filter(is.na(ad_present) == F)



# For those without autopsy, were AD markers incongruent?
# Which subjects had CSF and PET?
# For CSF, ad_present if tau_abeta42_ratio > 0.3
ad_classification_intvl %>% 
  filter(diagnosis != "Normal" & 
           is.na(ad_present) != TRUE &
           is.na(NPDx1) == TRUE &
           is.na(intvl_pet) != TRUE) %T>% 
  print() %>% 
  select(INDDID, ad_present, tau_abeta42_ratio) %>% distinct()

# #selected subjects
# subs_included <- union(autopsy_mris$INDDID, pet_mris$INDDID) %>% union(., csf_mris$INDDID)
# 
# #potentially censored subjects
# subs_censored <- union(autopsy_mris_censored$INDDID, pet_mris_censored$INDDID) %>% union(., csf_mris_censored$INDDID)
# 
# #these were excluded from one marker but included based on another
# check_markers_agree <- intersect(subs_included, subs_censored)

# MRIs to use for cases
mri_cases <- mri_selected %>% 
  select(INDDID, diagnosis, ad_present, session_date, ad_marker, intvl_marker,
         FlywheelSessionLabel, YOB, Race, Sex, Education) %>% 
  filter(diagnosis != "Normal") %>% 
  distinct()


#select MRIs for controls (since only controls who had a PET done were included above)
controls <- 
  diagnosis %>% 
  filter(diagnosis == "Normal") %>% 
  left_join(ashs_date) %>% 
  left_join(dem_biomarkers2) %>% 
  distinct()  

#remove controls with +PET
controls_to_remove <- controls %>% 
  filter(PET_read == "Positive") %>% 
  select(INDDID) %>% 
  distinct()

mri_controls <- 
  controls %>% mutate(ad_present = NA) %>% 
  select(INDDID, diagnosis, ad_present, session_date, FlywheelSessionLabel, YOB, Race, Sex, Education) %>% 
  filter(diagnosis == "Normal") %>% 
  mutate(ad_marker = NA,
         intvl_marker = NA, .after = session_date) %>% 
  distinct()

# Remove controls with non-missing "Date of onset" -------

input <-readxl::read_excel("data/controls_dz_onset_(2023.01.31 13.51).xlsx", 
                          col_types = "text")
controls_with_onset_pos <-
  input %>% filter(
    if_all(
    .cols = GlobalYearOnset,
    .fns = ~!is.na(.x)
  )
) %>% 
  select(INDDID)

controls_with_onset_pos

controls_to_remove <- union(controls_to_remove, controls_with_onset_pos)

mri_controls2 <- mri_controls %>% 
  filter(INDDID %in% controls_to_remove$INDDID == F) #remove any normals with +PET


# Select most recent MRI for controls with multiple MRIs
# I should try to match for date of MRI of cases ideally
mri_controls3 <- mri_controls2 %>% group_by(INDDID) %>% 
  slice_max(session_date, n=1, with_ties = FALSE)

#check time between session date and consent date 
input <-readxl::read_excel("data/controls_consents_(2023.01.31 14.18).xlsx") %>% 
  mutate(INDDID = as.character(INDDID))

control_enrollment_date <-
  input %>% group_by(INDDID) %>% slice_min(SignedDate, n=1, with_ties = FALSE) %>% 
  select(INDDID, SignedDate)

control_time_to_mri <-
  mri_controls3 %>% 
  left_join(control_enrollment_date) %>% 
  mutate(date_diff = as.double(
    difftime(
      session_date, SignedDate
      )
    )
    ) %>% 
  select(INDDID, session_date, SignedDate, date_diff)

hist(control_time_to_mri$date_diff)

# Remove controls with MRI >3 years from enrollment? --------
# 
# controls_long_time_to_mri <- control_time_to_mri %>% 
#   filter(date_diff < (3*365)) %>% 
#   select(INDDID)

# Merge df of selected mri dates for cases and controls
mri_selected_all <- mri_cases %>% rbind(., mri_controls3)

# Remove problematic cases and controls identified above

remove <- union(cases_to_remove, controls_to_remove) %>% as.vector()

mri_selected_all %<>% filter(!INDDID %in% c(remove$INDDID))

saveRDS(mri_selected_all, output)

# #clear workspace
# rm(list = ls())

# merge clinical/mri metadata w/ ASHS ---------------------------
# Merge clinical/mri metadata with ashs volume data 


# ***I may be losing subjects because I choose the MRI date that is closest
# but then the MRI for that date doesn't have associated ASHS output.
# So should use ASHS MRI dates rather than all MRI dates for selecting a

# input
ashs_wide <- readRDS(file = here("objects/ashs_wide.RDS"))
mri_selected_all <- readRDS(file = here("objects/mri_selected_all.RDS"))

# output
output <- here("objects/mri_selected_ashs_all.RDS")

# merge selected mris with ashs output
mri_selected_ashs_all <- mri_selected_all %>% left_join(ashs_wide)

# add age at MRI variable
# extract session date
mri_selected_ashs_all$session_year <- stringr::str_sub(mri_selected_ashs_all$session_date, start = 1, end = 4)

mri_selected_ashs_all$age_at_mri <- as.numeric(
  lubridate::parse_date_time(
    mri_selected_ashs_all$session_year, "Y") - 
    lubridate::parse_date_time(mri_selected_ashs_all$YOB, "Y")) / 365 

mri_selected_ashs_all$age_at_mri <- round(mri_selected_ashs_all$age_at_mri, digits = 0) 

# Remove individuals less than 50 at time of MRI 
mri_selected_ashs_all %>% 
  filter(age_at_mri<50) %>% 
  select(1:3) %>% 
  distinct() %>% 
  select(diagnosis) %>% 
  table()

mri_selected_ashs_all %<>% filter(age_at_mri>50)

# set ad_present = "Normal" in controls 
mri_selected_ashs_all$ad_present %<>%  as.character()
mri_selected_ashs_all$ad_present[mri_selected_ashs_all$diagnosis ==
                                   "Normal"] <- "Normal"
mri_selected_ashs_all$ad_present <- factor(
  mri_selected_ashs_all$ad_present, 
  levels = c("Normal", "FALSE", "TRUE"),
  labels = c("Control", "LBD-AD", "LBD+AD"))

saveRDS(mri_selected_ashs_all, output)

# #clear workspace
# rm(list = ls())

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

# How many diagnoses are represented in the dataset.

ds$diagnosis %>% 
  unique() %>%
  length()

# Here is a list of diagnoses and their frequencies in the dataset
ds$diagnosis %>%
  table()

# Convert character to numeric where needed
# Identify the vairables to process.

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

# Missing values
# Count the number of missing values.

ds %>% is.na() %>% sum()

# No missing values in the first two columns (INDDID and diagnosis)

ds[1:2] %>% is.na() %>% sum()

ds$education %<>% na.roughfix()

# Confirm that no missing values remain.

ds %>% is.na() %>% sum()

# rearrange variables so age is in front of df

ds %<>% select(inddid, ad_present, age_at_mri, everything())

# Combine Anterior and posterior hippocampus to make new variable ---------

educat <- grep("education$", colnames(ds)) #column index

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
  select(all_of(c(1:educat)),
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
         everything())
         
# Save cleaned dataframe for future use
saveRDS(ds, file = here("objects/ashs_clean.RDS"))

saveRDS(ds_summed_regions, file = "objects/ds_summed_regions.RDS")

# Normalize volumes by dividing by ICV -------------
ds_summed_regions <- read_rds("objects/ds_summed_regions.RDS")

vol_start <- grep("left_anterior_hippocampus$", colnames(ds_summed_regions)) #column index to begin
vol_end <- grep("left_icv$", colnames(ds_summed_regions)) #column index to end

# copy cols to adjust 
ds_adjusted <- ds_summed_regions[vol_start:vol_end] 

# rename them 
colnames(ds_adjusted) <- paste0(colnames(ds_adjusted), "_adjusted")

# divide by ICV and multiply by 100
ds_adjusted %<>% transmute(.*100/(left_icv_adjusted)) 

ds_adjusted <- cbind(ds_summed_regions, ds_adjusted) %>% 
  select(all_of(c(1:educat)),
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

saveRDS(ds_adjusted, file = "objects/ds_adjusted.RDS")

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
# 
# rm(list=ls())
# 
# Export inddids and session_ids ------------
mri_selected_ashs <- read_rds("objects/mri_selected_ashs_all.RDS")
qc_ids <- mri_selected_ashs %>% select(subject = INDDID, 
                                       session = FlywheelSessionLabel)

write_delim(qc_ids, file = here("qc_ids.txt"))

# Look at the difference between participants with and without ASHS output