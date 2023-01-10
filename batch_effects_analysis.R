## Figure 1: Subregion Volumes, unadjusted


#obtain order of regions by mean volume 
df_raw_vol <- ashs_wide %>% select(INDDID, ad_present, contains(c("left_", "right_"))) %>% 
  select(-contains(c("MISC", "Meninges", "ICV", "Sul"))) %>% 
  pivot_longer(cols = contains(c("left_", "right_")), names_to = "Region", values_to = "Volume") %>% 
  mutate(ad_present = as.factor(ad_present))

df_raw_vol %>% 
  group_by(Region) %>% 
  summarise(Avg_vol = mean(Volume)) %>% arrange(desc(Avg_vol)) 

df_raw_vol$Region <- recode_factor(df_raw_vol$Region, 
                                   left_Anterior_hippocampus= "L Ant Hip",
                                   right_Anterior_hippocampus = "R Ant Hip",
                                   left_Posterior_hippocampus = "L Post Hip", 
                                   right_Posterior_hippocampus = "R Post Hip",
                                   left_Br36 = "L Br36",
                                   right_Br36 = "R Br36",
                                   left_PHC = "L PHC", 
                                   right_PHC = "R PHC", 
                                   left_Br35 = "L Br35", 
                                   right_Br35 = "R Br35",
                                   left_ERC = "L ERC", 
                                   right_ERC = "R ERC")

df_raw_vol$ad_present <- recode_factor(df_raw_vol$ad_present,
                                        Normal = "Control",
                                       `FALSE` = "LBD-AD",
                                       `TRUE` = "LBD+AD")


ggplot(df_raw_vol, aes(x = ad_present, y = Volume)) +
  geom_boxplot() + 
  facet_wrap(.~Region, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Subregion Volumes, Unadjusted", 
       y = "Volume (cm^3)",
       x = "Case Status") + 
  theme(axis.text = element_text(size = 5)) +
  theme(strip.text.x = element_text(size = 15)) +
  geom_jitter(alpha = 0.2, size =1)


 
