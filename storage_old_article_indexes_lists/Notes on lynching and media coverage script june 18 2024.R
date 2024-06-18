Notes on lynching and media coverage script

June 18 2024

#Figure 8: Page One Analysis
Next, we count the number of articles by page number. About 27% of all lynching articles were on Page One.

```{r}
index2$decade <- paste0(substr(index2$year, 0, 3), "0")

pageplacement <- index2 %>% 
  mutate(page = str_replace(page, "seq-", "")) %>% 
  group_by(page, decade) %>% 
  count(page) %>% 
  ungroup()

pageplacement <- pageplacement %>% 
  mutate(pct =(n/sum(n)))


pageplacement$pct <-formattable::percent(pageplacement$pct, 1)

pageplacement %>% 
  top_n(10,pct) %>% 
  ggplot(aes(x = page, y = pct, fill = pct)) +
  geom_col(position = "dodge") + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=c(1:15)) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label= pct, x= page, y= pct), hjust=.5, vjust=0) +
  labs(title = "Page Placement, Lynching Coverage, 1789-1963", 
       subtitle = "Page Number Placement of Lynching Stories",
       caption = "Page 1 stories were 29% of 59,967 pages. Graphic by Rob Wells, 12/27/2023",
       y="Pct of Pages",
       x="Page Number")
ggsave("../output_images_tables/page_placement_6_18_2024.png",device = "png",width=9,height=6, dpi=800)
```