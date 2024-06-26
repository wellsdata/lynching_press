Outakes from June 26 article graphics.off(
  # Ratio
  ```{r}
  
  combo2 %>% 
    ggplot() +
    geom_line(aes(x = year, y=ratio), color = "blue", size = 1) +
    scale_y_continuous(labels = scales::percent)+
    scale_x_continuous(breaks=c(1860, 1865, 1870, 1875, 1880, 1885, 1890, 1895, 1900, 1905, 1910, 1915, 1920, 1925, 1930, 1935, 1940, 1945, 1950, 1955, 1960)) +
    theme(axis.text.x = element_text(angle=90))+ 
    labs(x = "Year", y = "Ratio News to Lynchings") +
    labs(title = "Ratio of News Coverage vs Lynchings, 1865-1963", 
         subtitle = "Media Coverage (Red) vs. Actual Victims (Blue), pct of whole",
         caption = "Tolnay_Beck Victims n = 5871. Media n = 11,396 articles. Graphic by (redacted - peer review), 6/26/2024")
  
  ```
)


### Bar Chart 1890 - 1930
```{r}
df <- combo3 %>%
  pivot_longer(!year, names_to = "type", values_to = "weight")

ggplot(df, aes(x=year, y=weight, fill=type)) +
  geom_bar(stat='identity', position='dodge') +
  scale_x_continuous(labels = c(seq(1890, 1930, 5)), breaks = seq(1890, 1930, 5)) +
  labs(title = "Media Coverage Exceeds Actual Lynchings After 1897", 
       subtitle = "Weighted Samples: Media Coverage (Red) vs. Actual Victims (Blue)",
       caption = "tolnay_beck Victims n = 5871. Media n = 11223 articles. Graphic by (redacted - peer review), 6/25/2024")

#ggsave(here::here("../hcij_lynching_phase_two/narratives/output_images_tables/media_lynching_2.png"),device = "png",width=9,height=6, dpi=800)


```



We extracted a stratified sample of total lynching victims by year and total count of news pages by year. It shows how news coverage lagged the actual incidence of lynching until 1893, and then news coverage exceeded actual lynchings until a sharp dropoff in 1923. One conclusion from the graphic: news coverage lagged significantly during peak lynching activity in the 1890s. 

One potential reason for the dropoff at 1923: Chronicling America reports that newspapers published in the United States more than 95 years ago in the public domain, and those published prior to that time may have some copyright restrictions. That would mean 1927 would be the cutoff date, as of this writing, but the issue of copyright could account for a lower sample of overall papers after that time period. 

Here is the documentation on drawing a stratified sample (proportional allocation)

ssamp(df, n, strata, over=1)

Where:
  
  df is object containing full sampling data frame
n is sample size (integer, or object containing sample size)
strata is variable in sampling data frame by which to stratify (e.g. region)
over (optional) is desired oversampling proportion (defaults to 0; takes value between 0 and 1 as input)

Returns stratified sample using proportional allocation without replacement



black_press_master_june_26_2024 %>% 
  count(decade)
