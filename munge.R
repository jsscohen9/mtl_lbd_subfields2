## Script name: Munge script 
##
## Purpose of script: Import and wrangle data
##
## Date Created: 2023-01-05
##
## Notes:
##   

## load packages: ---------------------------

library(tidyverse)
library(here)

# querying add'l data ---------------------------

input_file <- here("data/original_INQuery_Output_2022.07.12_09.59_.xlsx")
output_file <- here("reports", "inddids_all.csv")

# Unique IDs to Query for more data in INDD

# get list of unique INDDs from initial query

original_query <- readxl::read_excel(input_file)

inddids_all <- 
  original_query %>% 
  select(INDDID) %>% 
  distinct()

write_csv(inddids_all, file = output_file)

#clear workspace
rm(list = ls())

# collapse diagnoses ---------------------------

input_file <- here("data/original_INQuery_Output_2022.07.12_09.59_.xlsx")
output_file <- here("objects/id_diagnosis.RDS")

# Rename INQuery output

clinical <- readxl::read_excel(input_file) %>% 
  mutate(INDDID = as.character(INDDID))


# assign "MotorDx" and "CognitiveDx" to ClinicalPhenotype_Sum for those without "ClinicalPhenotype1"
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
         diagnosis = ClinicalPhenotype_sum)

#save as "id_diagnosis"
saveRDS(clinical3, file = output_file)

#clear workspace
rm(list = ls())

# import demographics ----------------------- 
input_file <- here("data/demographic_biomarker_autopsy_(2022.12.01 21.51).xlsx")
output_file <- here("objects/dem_biomarkers2.RDS")
  
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

saveRDS(dem_biomarkers2, file = output_file)

# ashs import --------------------
input_file <- here("data/ashs_all_output.csv")
output_file1 <- here("objects/ashs_t1.RDS")
output_file2 <- here("objects/ashs_t2.RDS")
  
ashs <- read_csv(input_file) %>% 
  mutate(INDDID = as.character(INDDID)) %>% 
  filter(Version != "0.1.1") %>% #remove output from ASHS 0.1.1
  rename(FlywheelSessionLabel = MRISession)

# split ashs 
ashs_t1 <- ashs %>% 
  filter(Version == '0.2.0_t1') %>% 
  distinct()

saveRDS(ashs_t1, file = output_file1)

ashs_t2 <- ashs %>% 
  filter(Version == '0.2.0') %>% 
  distinct()

saveRDS(ashs_t2, file = output_file2)

#clear workspace
rm(list = ls())

# mri metadata ------------------------
input_file <- here("data/mri_bids_(2022.12.02 11.21).xlsx")
output_file1 <- here("objects/mri_bids_data_t1.RDS")
output_file2 <- here("objects/mri_bids_data_t2.RDS")
  
mri_bids_data <- readxl::read_excel(input_file) %>% 
  mutate(INDDID = as.character(INDDID))

mri_bids_data_2 <- 
  mri_bids_data %>% 
  select(INDDID, FlywheelSessionLabel, FlywheelProjectLabel, FlywheelAcquisitionMeasurement, FlywheelAcquisitionLabel, DicomInstitutionName, DicomStationName,
         DicomSliceThickness, DicomPixelSpacingX, DicomMagneticFieldStrength, DicomRepetitionTime, DicomSpacingBetweenSlices, BidsFilename)

#collapse factors
mri_bids_data_2$DicomInstitutionName <- fct_collapse(mri_bids_data_2$DicomInstitutionName) 

#T1
mri_bids_data_t1 <- mri_bids_data_2 %>% 
  filter(FlywheelAcquisitionMeasurement == 'T1')

saveRDS(mri_bids_data_t1, output_file1)

#T2
mri_bids_data_t2 <- mri_bids_data_2 %>% 
  filter(FlywheelAcquisitionMeasurement == 'T2')
         
saveRDS(mri_bids_data_t2, output_file2)

# clinical testing -----------------------
mmse <- readxl::read_excel(here("data/mmse_(2022.10.26 15.44).xlsx")) %>% 
  mutate(INDDID = as.character(INDDID))

moca <- readxl::read_excel(here("data/moca_(2022.10.26 16.08).xlsx")) %>% 
  mutate(INDDID = as.character(INDDID))

vlt <- readxl::read_excel(here("data/vlt_(2022.11.08 09.42).xlsx")) %>% 
  mutate(INDDID = as.character(INDDID))

saveRDS(vlt, file = here("objects/vlt_raw.RDS"))

epworth <- readxl::read_excel(here("data/epworth_(2022.10.04 17.59).xlsx")) %>% 
  mutate(INDDID = as.character(INDDID))

#clear workspace
rm(list = ls())

# merge datasets ---------------------------
input_file <- here("objects/ashs_t1.RDS")

ashs_t1 <- readRDS(input_file)

ashs_wide <- ashs_t1 %>%
  unite(col = region, Hemisphere, Region) %>% 
  select(-Label, -Version) %>%
  distinct() %>% 
  pivot_wider(id_cols = c(INDDID, FlywheelSessionLabel), 
              names_from = region, values_from = Volume)

saveRDS(ashs_wide, here("objects/ashs_wide.RDS")) 

#clear workspace
rm(list = ls())

# classify ad_present  ---------------------------
#input
ashs_wide <- readRDS(here("objects/ashs_wide.RDS"))
diagnosis <- readRDS(here("objects/id_diagnosis.RDS"))
dem_biomarkers2 <- readRDS("objects/dem_biomarkers2.RDS")

#output
output_1 <- here("objects/ashs_markers.RDS")
output_2 <- here("objects/id_dx_ad_present.RDS")

ashs_info <- ashs_wide %>% 
  select(INDDID, FlywheelSessionLabel) %>% 
  left_join(diagnosis) %>% 
  left_join(dem_biomarkers2) %>% 
  distinct()

## Autopsy classifications

#convert Braak03 to Braak06
ashs_info$Braak06[is.na(ashs_info$Braak06) == TRUE & ashs_info$Braak03 == "0"] <- "0"
ashs_info$Braak06[is.na(ashs_info$Braak06) == TRUE & ashs_info$Braak03 == "1"] <- "2"
ashs_info$Braak06[is.na(ashs_info$Braak06) == TRUE & ashs_info$Braak03 == "2"] <- "4"
ashs_info$Braak06[is.na(ashs_info$Braak06) == TRUE & ashs_info$Braak03 == "3"] <- "6"

#add column with ABC level
ashs_info$abc[ashs_info$ABeta == "0" & ashs_info$CERAD == "0"] <- "Not"
ashs_info$abc[ashs_info$ABeta %in% c("1","2") & ashs_info$CERAD %in% c("0","1")] <- "Low"
ashs_info$abc[ashs_info$ABeta %in% c("1","2") & ashs_info$CERAD %in% c("2","3") & ashs_info$Braak06 %in% c("0","1", "2")] <- "Low"
ashs_info$abc[ashs_info$ABeta %in% c("1","2")  & ashs_info$CERAD %in% c("2","3") & ashs_info$Braak06 %in% c("3","4","5","6")] <- "Intermediate"
ashs_info$abc[ashs_info$ABeta == "3" & ashs_info$Braak06 %in% c("0","1", "2")] <- "Low"
ashs_info$abc[ashs_info$ABeta == "3" & ashs_info$Braak06 %in% c("3","4","5","6")] <- "Intermediate"
ashs_info$abc[ashs_info$ABeta %in% c("4","5") & ashs_info$CERAD %in% c("0","1") & ashs_info$Braak06 %in% c("0","1", "2")] <- "Low"
ashs_info$abc[ashs_info$ABeta %in% c("4","5") & ashs_info$CERAD %in% c("0","1") & ashs_info$Braak06 %in% c("3","4","5","6")] <- "Intermediate"
ashs_info$abc[ashs_info$ABeta %in% c("4","5") & ashs_info$CERAD %in% c("2","3") & ashs_info$Braak06 %in% c("0","1", "2")] <- "Low"
ashs_info$abc[ashs_info$ABeta %in% c("4","5") & ashs_info$CERAD %in% c("2","3") & ashs_info$Braak06 %in% c("3","4")] <- "Intermediate"
ashs_info$abc[ashs_info$ABeta %in% c("4","5") & ashs_info$CERAD %in% c("2","3") & ashs_info$Braak06 %in% c("5","6")] <- "High"


# Classify as ad_present or not
#autopsy
ashs_info$ad_present[ashs_info$abc %in% c("Intermediate", "High")] <- TRUE
ashs_info$ad_present[ashs_info$abc %in% c("Not", "Low")] <- FALSE

#pet
ashs_info$ad_present[is.na(ashs_info$ad_present) == TRUE & ashs_info$PET_read == "Positive"] <- TRUE
ashs_info$ad_present[is.na(ashs_info$ad_present) == TRUE & ashs_info$PET_read == "Negative" ] <- FALSE

#csf
ashs_info$ad_present[is.na(ashs_info$ad_present) == TRUE & ashs_info$tau_abeta42_ratio > 0.3] <- TRUE
ashs_info$ad_present[is.na(ashs_info$ad_present) == TRUE & ashs_info$tau_abeta42_ratio <= 0.3] <- FALSE

# create MRI session date variable 
ashs_info2 <- ashs_info %>% 
  mutate(session_date = str_sub(FlywheelSessionLabel, 0, 8)) %>% 
  mutate(session_date = as.Date(session_date, format = '%Y%m%d')) 

#calculate time between MRI and biomarker 
ashs_info3 <- ashs_info2 %>% 
  mutate(intvl_autopsy = as.numeric(as.Date(AutopsyDate) - as.Date(session_date))/365,
         intvl_pet = ifelse(PETTracer %in% c("Florbetaben (amyloid)", "Florbetapir (amyloid)") == T & 
                              is.na(PET_read) ==FALSE, 
                            as.numeric(as.Date(PETDate) - as.Date(session_date))/365,
                            NA),
         intvl_csf = ifelse(is.na(tau_abeta42_ratio) == F,
                            as.numeric(as.Date(CSFDate) - as.Date(session_date))/365,
                            NA))

#select columns, filter out other pet tracers
ashs_info4 <- ashs_info3 %>%
  filter(PETTracer %in% c("Florbetaben (amyloid)", "Florbetapir (amyloid)") | is.na(PETTracer) == TRUE)

ashs_markers <- ashs_info4 %>% 
  select(INDDID, diagnosis, ad_present,
         session_date, FlywheelSessionLabel, 
         starts_with(c("intvl")), NPDx1, 
         tau_abeta42_ratio, YOB, Race, 
         Sex, Education) %>%
  distinct()

saveRDS(ashs_markers, output_1)
#save table of dx and ad_present
id_dx_ad_present <- ashs_markers %>% select(1:3) %>% distinct()

saveRDS(id_dx_ad_present, output_2)

#clear workspace
rm(list = ls())

#r select mris ---------------------------
# Decide which MRI to use for which subject

#input
ashs_markers <- readRDS(here("objects/ashs_markers.RDS"))

#output
output <- here("objects/mri_selected_all.RDS")

# for those with an autopsy
autopsy_mris <- ashs_markers %>% 
  mutate(ad_marker = "autopsy") %>% 
  group_by(INDDID) %>%
  filter(intvl_autopsy < 5,
         is.na(NPDx1) == F) %>% 
  filter(intvl_autopsy == max(intvl_autopsy)) %>% #take earliest MRI within 5 years of autopsy 
  distinct() %>% 
  ungroup()


# #censored participants with autopsy - make sure that if they are included with other biomarkers that it is concordant with autopsy result
# autopsy_mris_censored <- ashs_markers %>% 
#   select(INDDID, intvl_autopsy, session_date, NPDx1) %>% 
#   group_by(INDDID) %>%
#   filter(intvl_autopsy >= 5,
#          is.na(NPDx1) == F) %>% 
#   filter(intvl_autopsy == max(intvl_autopsy)) %>% #take earliest MRI within 5 years of autopsy 
#   filter(INDDID %in% autopsy_mris$INDDID == F) %>% 
#   distinct() %>% 
#   ungroup()


# for PET
pet_mris <- ashs_markers %>%
  mutate(ad_marker = "pet") %>% 
  filter(INDDID %in% autopsy_mris$INDDID == F) %>% 
  group_by(INDDID) %>% 
  mutate(intvl_pet = abs(intvl_pet)) %>% 
  filter(intvl_pet < 5) %>%
  filter(intvl_pet == min(intvl_pet)) %>%
  distinct() %>% 
  ungroup()

# #censored PET due to >= 5 years 
# pet_mris_censored <- ashs_markers %>%
#   filter(INDDID %in% autopsy_mris$INDDID == F &
#            INDDID %in% pet_mris$INDDID == F) %>% 
#   select(INDDID, intvl_pet, session_date) %>% 
#   group_by(INDDID) %>% 
#   mutate(intvl_pet = abs(intvl_pet)) %>% 
#   filter(intvl_pet >= 5) %>%
#   filter(intvl_pet == min(intvl_pet)) %>% 
#   distinct() %>% 
#   ungroup()


#for CSF
csf_mris <- ashs_markers %>%
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
# csf_mris_censored <- ashs_markers %>%
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

#combine tables of selected mris 
mri_selected <- bind_rows(autopsy_mris, pet_mris, csf_mris) %>% 
  select(INDDID, session_date, ad_marker, intvl_autopsy, intvl_pet, intvl_csf, everything()) %>%
  group_by(INDDID) %>% 
  distinct() %>% 
  ungroup()

# #create invtl_marker variable to based on which intvl is used
# mri_selected3 <-
#   mri_selected %>%
#   group_by(INDDID) %>%
#   mutate(intvl_marker = ifelse(ad_marker == "autopsy",
#                                intvl_autopsy,
#                                ifelse(ad_marker == "pet",
#                                       intvl_pet,
#                                       ifelse(ad_marker == "csf",
#                                              intvl_csf,
#                                              NA))))

# MRIs to use for cases
mri_cases <- mri_selected %>% 
  select(INDDID, diagnosis, ad_present, session_date, ad_marker, FlywheelSessionLabel, YOB, Race, Sex, Education) %>% 
  filter(diagnosis != "Normal") %>% 
  distinct()

# #selected subjects
# subs_included <- union(autopsy_mris$INDDID, pet_mris$INDDID) %>% union(., csf_mris$INDDID)
# 
# #potentially censored subjects
# subs_censored <- union(autopsy_mris_censored$INDDID, pet_mris_censored$INDDID) %>% union(., csf_mris_censored$INDDID)
# 
# #these were excluded from one marker but included based on another
# check_markers_agree <- intersect(subs_included, subs_censored)


#select MRIs for controls (since only controls who had a PET done were included above)
mri_controls <- ashs_markers %>% 
  select(INDDID, diagnosis, ad_present, session_date, FlywheelSessionLabel, YOB, Race, Sex, Education) %>% 
  filter(diagnosis == "Normal") %>% 
  distinct()

#remove controls with +PET
controls_remove <- mri_controls %>% 
  filter(ad_present == T) %>% 
  select(INDDID) %>% 
  distinct()


mri_controls2 <- mri_controls %>% 
  filter(INDDID %in% controls_remove$INDDID == F) #remove any normals with +PET


#select most recent MRI for controls with multiple MRIs
mri_controls3 <- mri_controls2 %>% group_by(INDDID) %>% 
  slice_max(session_date, n=1, with_ties = FALSE)

#merge df of selected mri dates for cases and controls
mri_selected_all <- mri_cases %>% select(-c(ad_marker)) %>% rbind(., mri_controls3) %>% 
  filter(YOB<1963) #remove individuals less than 60 

saveRDS(mri_selected_all, output)

#clear workspace
rm(list = ls())

# merge clinical/mri metadata w/ ASHS ---------------------------
# Merge clinical/mri metadata with ashs volume data 

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

# set ad_present = "Normal" in controls 
mri_selected_ashs_all$ad_present %<>%  as.character()
mri_selected_ashs_all$ad_present[mri_selected_ashs_all$diagnosis ==
                                   "Normal"] <- "Normal"
mri_selected_ashs_all$ad_present <- factor(
  mri_selected_ashs_all$ad_present, 
  levels = c("Normal", "FALSE", "TRUE"),
  labels = c("Control", "LBD-AD", "LBD+AD"))

saveRDS(mri_selected_ashs_all, output)

#clear workspace
rm(list = ls())
