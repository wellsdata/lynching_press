index <- read_csv("../data/index_feb6.csv") %>%
  as.data.frame()

index2 <- read_csv("/Users/robwells/Code/hcij_lynching_phase_two/Storage_Older_Versions/LayoutBoxes_merged_20230313-1250262023-05-08_14_47_31.115031+00_00_index.csv") %>%
  as.data.frame()

# PreCivil War coverage

jackindex_geo <- read_csv("../data/extracted_articles_aug25.csv")

# 558 articles in our sample
pre_civil_war_index <- jackindex_geo %>% 
  filter(year <= "1861") %>% 
  mutate(date = paste(year,month,day, sep="-")) %>% 
  as.data.frame()

pre_civil_war_index$date <- as.Date(pre_civil_war_index$date)

# Filter out from the current
library(googlesheets4)

googlesheets4::gs4_deauth()
civilwar_done <- read_sheet("https://docs.google.com/spreadsheets/d/1FLwfLRBnhqHasbsKdYLUBs92-DETjF288tL2ivX8obs/edit#gid=0") %>% 
  as.data.frame()

filtered <- civilwar_done %>% 
  full_join(pre_civil_war_index, by=c("ID"="file_id", "Date"="date", "URL"))


filtered <- filtered[!(is.na(filtered$URL)), ]

pre_civil_war_master <- filtered

write.csv(pre_civil_war_master, "../output/pre_civil_war_master.csv")
