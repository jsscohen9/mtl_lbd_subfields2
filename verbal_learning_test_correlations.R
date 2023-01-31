#Verbal memory score selection

#input
vlt_raw <- readRDS(here("vlt_raw.RDS"))
mris_selected_all <- readRDS(here("mris_selected_all.RDS"))

#find verbal memory test closest to mri date for each subject
vlt_selected <- mris_selected_all %>% 
  left_join(vlt_raw) %>%
  filter(diagnosis != "Normal") %>% 
  mutate(session_date = str_sub(MRISession, 0, 8)) %>% 
  mutate(session_date = as.Date(session_date, format = '%Y%m%d')) %>%
  select(INDDID, diagnosis, ad_present, session_date, hvlt_date = TestDate, pvlt_date = `TestDate1.`, everything()) %>%
  filter(is.na(PHLRecognitionDiscrimin) == F) %>% 
  pivot_longer(cols = c("hvlt_date", "pvlt_date"), names_to = "test", values_to = "test_date") %>% 
  mutate(intvl_test = as.numeric(as.Date(test_date) - as.Date(session_date))/365) %>% 
  select(INDDID, diagnosis, ad_present, session_date, intvl_test, test, test_date) %>% distinct() %>% 
  filter(is.na(intvl_test)==F) %>% 
  filter(intvl_test > 0) %>% 
  group_by(INDDID) %>% 
  filter(intvl_test == min(intvl_test, na.rm = T)) %>%
  ungroup() 


hvlt <- vlt_raw %>% select(`INDDID.1.`:HVLR_version_num) %>% 
  distinct() %>%
  rename(INDDID = `INDDID.1.`) %>% 
  filter(is.na(INDDID) ==F) %>% 
  mutate(INDDID = as.character(INDDID))


pvlt <- vlt_raw %>% select(`INDDID.2.`:PHLTuesTotalFoils) %>% 
  distinct() %>% 
  rename(INDDID = `INDDID.2.`) %>% 
  filter(is.na(INDDID) ==F) %>%
  mutate(INDDID = as.character(INDDID)) %>% 
  filter(is.na(PHLRecognitionDiscrimin) == F) #remove individuals who could not finish


#select vlts for all cases (regardless of whether they have ashs output)

# HVLT learning - Total recall score. also z-scores for each individual trial then average those z scores
# delayed - including Rapid forgetting index (%retained - measures what is lost between highest score in immediate recall and delayed recall)
# recognition - recognition discrimination index 

# first calculate z-scores
# select hvlt tests to use and add columns for max immediate recall score, "rapid forgetting index" (delayed recall score/max immediate recall score)
hvlt2 <- clinical3 %>% left_join(hvlt) %>% 
  filter(diagnosis != "Normal",
         is.na(TestDate) == FALSE) %>% 
  select(INDDID, TestDate, diagnosis, HVLTRT1, HVLTRT2, HVLTRT3, HVLTRTTotScore, HVLTRDLY, HVLTRECDISTScore) %>% 
  rowwise() %>% 
  mutate(immediate_max = max(HVLTRT1, HVLTRT2, HVLTRT3)) %>% 
  mutate(remember_index = HVLTRDLY/immediate_max)

data <- hvlt2 %>% select(HVLTRT1, HVLTRT2, HVLTRT3, HVLTRTTotScore, HVLTRDLY, HVLTRECDISTScore, immediate_max, remember_index)
z_scores <- sapply(data, function(data) (data-mean(data, na.rm = T))/sd(data, na.rm = T))

#rename z_score column names 
original_cols <- colnames(z_scores) 
colnames(z_scores) <- paste0(original_cols, "_z_score")

#rejoin to original df
hvlt_normed <- cbind(hvlt2, z_scores) %>% as_tibble()

#create new column for mean of immediate recall z-scores
hvlt_normed2 <- hvlt_normed %>% 
  rowwise() %>% 
  mutate(immediate_z_score_mean = mean(HVLTRT1_z_score, HVLTRT2_z_score, HVLTRT3_z_score, na.rm=T)) 

### Library
library(heatmaply)

### Data
hvlt_normed3 <- hvlt_normed2 %>% select(INDDID, diagnosis, TestDate, hvlt_immediate_tot_z = HVLTRTTotScore_z_score, hvlt_delay_z = HVLTRDLY_z_score,
                                        hvlt_rec_disc_z = HVLTRECDISTScore_z_score, hvlt_remember_index_z = remember_index_z_score)

# same for pvlt
pvlt2 <- clinical3 %>% left_join(pvlt) %>% 
  filter(diagnosis != "Normal") %>%
  select(INDDID, `TestDate.1.`, diagnosis, PHLCorrectTrial1, PHLCorrectTrial2, PHLCorrectTrial3, PHLCorrectTrial4, PHLCorrectTrial5,
         PHLCorrect15Total, PHLCorrectTrial9, PHLRecognitionDiscrimin) %>% 
  filter(is.na(PHLRecognitionDiscrimin)==F) %>% 
  rowwise() %>% 
  mutate(immediate_max = max(PHLCorrectTrial1, PHLCorrectTrial2, PHLCorrectTrial3, PHLCorrectTrial4, PHLCorrectTrial5)) %>% 
  mutate(remember_index = PHLCorrectTrial9/immediate_max) 

data <- pvlt2 %>% select(PHLCorrect15Total, PHLCorrectTrial9, PHLRecognitionDiscrimin, remember_index)
z_scores <- sapply(data, function(data) (data-mean(data, na.rm = TRUE))/sd(data, na.rm = TRUE))

#rename z_score column names 
original_cols <- colnames(z_scores) 
colnames(z_scores) <- paste0(original_cols, "_z_score")

#rejoin to original df
pvlt_normed <- cbind(pvlt2, z_scores) %>% as.tibble()

pvlt_normed2 <- pvlt_normed %>% select(INDDID, diagnosis, `TestDate.1.`, diagnosis, pvlt_immediate_tot_z = PHLCorrect15Total_z_score, 
                                       pvlt_delay_z = PHLCorrectTrial9_z_score, 
                                       pvlt_rec_disc_z = PHLRecognitionDiscrimin_z_score, pvlt_remember_index_z = remember_index_z_score)


# check for missing data 99s/88s, etc.
summary(hvlt2)
summary(pvlt2)

# look at individuals with both tests:
both_tests_ids <- intersect(hvlt_normed3$INDDID, pvlt_normed2$INDDID)

hvlt_both <- hvlt_normed3 %>% filter(INDDID %in% both_tests_ids)

pvlt_both <- pvlt_normed2 %>% filter(INDDID %in% both_tests_ids)

x <- hvlt_both %>% select(INDDID, diagnosis,  TestDate, hvlt_immediate_tot_z, hvlt_delay_z, hvlt_rec_disc_z, hvlt_remember_index_z)


y <- pvlt_both %>% select(INDDID, `TestDate.1.`, pvlt_immediate_tot_z, pvlt_delay_z, pvlt_rec_disc_z, pvlt_remember_index_z)

# concurrent validity
# predictive validity

both_tests <- left_join(x, y) %>% 
  mutate(test_diff = abs(TestDate - `TestDate.1.`)) %>% 
  group_by(INDDID, TestDate) %>% 
  slice_min(test_diff, with_ties = FALSE) %>% 
  ungroup

# total, delay, memory, and recall


both_tests2 <- both_tests %>% select(-(c(INDDID, TestDate, test_diff,`TestDate.1.`, diagnosis)))

both_tests2_cor <- cor(both_tests2,use="pairwise.complete.obs")
install.packages("Hmisc")
library(Hmisc)

both_tests2_rcorr <- rcorr(as.matrix(both_tests2), )
both_tests2_rcorr

library(corrplot)

corrplot((both_tests2_cor))

### Let's Plot
hvlt_heatmap <- heatmaply_cor(x = cor(df),
                              xlab = "Features",
                              ylab = "Features",
                              k_col = 2,
                              k_row = 2,
                              main = "HVLT Z-scores")

# PVLT
#remove individuals who couldn't complete the test
pvlt <- pvlt %>% filter(is.na(PHLRecognitionDiscrimin)==F)

data <- vlt_selected %>% filter(test == "pvlt_date") %>%
  left_join(pvlt, by = c("INDDID", "test_date" = "TestDate(1)")) %>% 
  select(PHLCorrectTrial1, PHLCorrectTrial2, PHLCorrectTrial3, PHLCorrectTrial4, PHLCorrectTrial5,
         PHLCorrect15Total, PHLCorrectTrial9, PHLRecognitionDiscrimin) %>% 
  filter(is.na(PHLRecognitionDiscrimin)==F) %>% 
  rowwise() %>% 
  mutate(immediate_max = max(PHLCorrectTrial1, PHLCorrectTrial2, PHLCorrectTrial3, PHLCorrectTrial4, PHLCorrectTrial5)) %>% 
  mutate(remember_index = PHLCorrectTrial9/immediate_max) 

z_scores <- sapply(data, function(data) (data-mean(data))/sd(data))

#rename z_score column names 
original_cols <- colnames(z_scores) 
colnames(z_scores) <- paste0(original_cols, "_z_score")

#rejoin to original df
pvlt_normed <- vlt_selected %>% filter(test == "pvlt_date") %>%
  cbind(., data, z_scores) %>% as.tibble()

#create new column for mean of immediate recall z-scores
pvlt_normed <- pvlt_normed %>% 
  rowwise() %>% 
  mutate(immediate_z_score_mean = mean(PHLCorrectTrial1_z_score, PHLCorrectTrial2_z_score, PHLCorrectTrial3_z_score, PHLCorrectTrial4_z_score, PHLCorrectTrial5_z_score)) 

### Data
df <- pvlt_normed %>% select(18:28)

### Let's Plot
pvlt_heatmap <- heatmaply_cor(x = cor(df),
                              xlab = "Features",
                              ylab = "Features",
                              k_col = 3,
                              k_row = 3,
                              main = "PVLT Z-scores")


hvlt_heatmap
pvlt_heatmap

remove(df, hvlt_heatmap, pvlt_heatmap, z_scores, original_cols, data)

#heatmap of both 

#total, delay, memory, and recall