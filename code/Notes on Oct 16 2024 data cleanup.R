Notes on Oct 16 2024 Data Cleanup

# I found that 29 Black press articles were past the 1964 cutoff for the Chroniciling America sample(
#   so I cut them out
  
#no longer using master_article_index_june_26_2024 which had 11,396 articles observed by coding team. instead using extracted_articles_index_june_22_2024 with 11,223 articles, about 19% of all 60,042 entries in search


 # Cut 29 articles from BP from 1964-2002 to align with the Chronicling America sample
  #Index of 11,223 articles of text extracted from 60,000 lynching articles: 18.7% of all 60,042 search captured
  extracted_articles_index_june_22_2024 <- read_csv("https://osf.io/download/z39ku/?view_only=6c106acd6cb54f6f849e8c6f9098809f") %>%
    as.data.frame()
  
  
  extracted_articles_index_oct_16_2024 <-
    extracted_articles_index_june_22_2024 |> 
    filter(year < 1964) |> 
    mutate(black_press = str_squish(black_press)) |> 
    mutate(black_press = case_when(
      black_press != "Y" | is.na(black_press) ~ "N",
      black_press=="Y" ~ "Y",
      TRUE ~ black_press
    ))
  
#write.csv(extracted_articles_index_oct_16_2024, "../data/extracted_articles_index_oct_16_2024.csv")  

#Now fix extracted text
#extracted_text_june_22_2024 <- read_csv("https://osf.io/download/p32he/?view_only=6c106acd6cb54f6f849e8c6f9098809f")

extracted_text_oct_16_2024 <- extracted_text_june_22_2024 |> 
  filter(year < 1964) |> 
  mutate(black_press = str_squish(black_press)) |> 
  mutate(black_press = case_when(
    black_press != "Y" | is.na(black_press) ~ "N",
    black_press=="Y" ~ "Y",
    TRUE ~ black_press
  ))


#fact check: 11,194 articles
extracted_text_oct_16_2024 %>%
  distinct(file_id) %>%  
  summarise(total_count = n())

#fact check: 9590 wp; 1604 bp
extracted_text_oct_16_2024 %>%
  distinct(file_id, black_press) %>%  # Ensure distinct articles by file_id
  group_by(black_press) %>%
  summarise(total_count = n(), .groups = "drop")

#write.csv(extracted_text_oct_16_2024, "../data/extracted_text_oct_16_2024.csv")  

#fact check bp extracted text: 1604 articles

black_press_extracted_text_oct_16_2024 <- extracted_text_oct_16_2024 |> 
  filter(black_press=="Y")
black_press_extracted_text_oct_16_2024 %>%
  distinct(file_id) %>%  
  summarise(total_count = n())
#write.csv(black_press_extracted_text_oct_16_2024, "../data/black_press_extracted_text_oct_16_2024.csv") 


#cut the years compare
#master_article_index_10_19

old_master_article_index_10.19 <- read.csv("/Users/robwells/Code/lynching_press/storage_old_article_indexes_lists/master_article_index_10.19.csv")
years_10_19 <- old_master_article_index_10.19 %>% 
  count(year) %>% 
  rename(count = n) %>% 
  mutate(pct_whole = round(count/sum(count)*100,2)) %>% 
  arrange(desc(pct_whole)) %>% 
  rename(pct_whole_old = pct_whole, count_old = count) %>%   mutate(rank_old = dense_rank(desc(pct_whole_old)))


#join and compare dfs


year_compare <- years_10_19 %>% 
  inner_join(years_10_11, by="year") %>% 
  mutate(diff = (count-count_old)) %>% 
  mutate(pct_chg = round(count-count_old)/count_old*100) %>% 
  mutate(pct_chg = round(pct_chg,2))

## fact checking years new vs old
year_compare %>% 
  filter(count_old > count)

#lost five articles in 1851, 1 in 1846. That's it
#Biggest gains in 1871, 1873, 1872, 1917, 1915, 1916


#NY, MA, no difference; PA, 1; DE, 6; ME, 7; SC, 8
#Biggest changes were IN, AZ, IA, CO, ID, CT, AL, A, HI, SC, AK


#cut the comparison with the old index
totals_10_19 <- old_master_article_index_10.19 %>% 
  count(newspaper_state) %>% 
  rename(count_old = n) %>% 
  mutate(pct_whole = round(count_old/sum(count_old)*100,2)) %>% 
  arrange(desc(pct_whole)) %>% 
  mutate(rank_new = dense_rank(desc(pct_whole)))

## join and compare dfs


```{r}
compare <- totals_10_11 %>% 
  inner_join(totals_10_19, by="newspaper_state") %>% 
  mutate(diff = (count-count_old)) %>% 
  mutate(pct_chg = round(count-count_old)/count_old*100) %>% 
  mutate(pct_chg = round(pct_chg,2))


#Biggest changes were AZ, IN, IA, CO, ID, AL, CT, CA, HI
#No difference; PA, WV, WY, NY, DE, SC, MA


#cut the precivil war comparison


#40 new pre-civil war articles extracted.
year_compare %>% 
  filter(year < "1862") %>% 
  summarize(sum(diff))

