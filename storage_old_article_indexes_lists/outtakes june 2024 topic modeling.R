# Additional Topic Modeling Work not included in final articles_decades
# June 24 2024




#---------------------------------------------
# Earlier Version with Black and White Papers
#---------------------------------------------
### Process into corpus object
```{r}

# load stopwords
english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")
# create corpus object
corpus <- Corpus(DataframeSource(textdata))
# Preprocessing chain
processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, english_stopwords)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)
```

```{r tm3a}
#DTM: rows correspond to the documents in the corpus. Columns correspond to the terms in the documents. Cells correspond to the weights of the terms. (Girder)
# compute document term matrix with terms >= minimumFrequency
minimumFrequency <- 5
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTM)
# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]
#5 term minimum[1] 1387 3019
#5 term minimum[1] 308597 10339

``` 


## Topic proportions over time{-}

We examine topics in the data over time by aggregating mean topic proportions per decade. These aggregated topic proportions can then be visualized, e.g. as a bar plot. 


```{r}
# append decade information for aggregation
textdata$decade <- paste0(substr(textdata$year, 0, 3), "0")
```

Articles per decade

```{r}
#install.packages("formattable")
articles_decades <- textdata %>% 
  distinct(doc_id, .keep_all=TRUE) %>% 
  count(decade) %>% 
  mutate(pct_total= (n/sum(n))) %>% 
  mutate(pct_total= formattable::percent(pct_total)) %>% 
  # mutate(pct_total = round(pct_total, 1)) %>% 
  arrange(desc(decade))

library(kableExtra)
articles_decades %>%
  kbl(caption = "LOC Lynching Articles by Decade (n=11,223, 6/23/2024)", font_size = 30) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "5em") %>% 
  column_spec(3, width = "5em", background = "yellow") 



#Fact check 308597 rows tabulated
#sum(articles_decades$n)
```


```{r tm12}
# number of topics
# K <- 20
K <- 6
# set random number generator seed
set.seed(9161)
#Latent Dirichlet Allocation, LDA
topicModel2 <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.2))
tmResult <- posterior(topicModel2)
theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicModel2, 10), 2, paste, collapse = " ")  # reset topicnames
```
### Mean topic proportions per decade

```{r}
# Step 1: Check dimensions
n_theta <- nrow(theta)
n_textdata <- length(textdata$decade)

cat("Number of rows in theta: ", n_theta, "\n")
cat("Number of documents in textdata: ", n_textdata, "\n")

# Check if textdata contains all the documents in theta
common_ids <- intersect(rownames(theta), textdata$doc_id) # Assuming textdata has a 'doc_id' column

# Filter textdata to include only the documents present in theta
textdata_filtered <- textdata[textdata$doc_id %in% common_ids, ]

# Check dimensions after filtering
n_textdata_filtered <- nrow(textdata_filtered)
cat("Number of documents in filtered textdata: ", n_textdata_filtered, "\n")

# Ensure the lengths match now
if (n_theta != n_textdata_filtered) {
  stop("The number of rows in 'theta' still does not match the length of 'textdata_filtered$decade'.")
}

# Align rownames of theta with filtered textdata
theta_aligned <- theta[rownames(theta) %in% textdata_filtered$doc_id, ]

# Optional: Verify the order of documents
if (!all(rownames(theta_aligned) == textdata_filtered$doc_id)) {
  # If the order doesn't match, reorder one to match the other
  textdata_filtered <- textdata_filtered[match(rownames(theta_aligned), textdata_filtered$doc_id), ]
}

# Ensure they are now aligned and can be combined
if (!all(rownames(theta_aligned) == textdata_filtered$doc_id)) {
  stop("The document IDs still do not match. Please check the data alignment.")
}

# Step 2: Combine data
topic_data <- data.frame(theta_aligned, decade = textdata_filtered$decade)

# Step 3: Aggregate data
topic_proportion_per_decade <- aggregate(. ~ decade, data = topic_data, FUN = mean)


# get mean topic proportions per decade
# topic_proportion_per_decade <- aggregate(theta, by = list(decade = textdata$decade), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_decade)[2:(K+1)] <- topicNames
# reshape data frame
vizDataFrame <- melt(topic_proportion_per_decade, id.vars = "decade")

# #filter out 1960 - one article
vizDataFrame <- vizDataFrame %>% 
  filter(!decade==1960)
```


```{r}

#add categories
#Updated June 27. See notes: https://docs.google.com/document/d/1BvYAye_8biVUaJchm1PB1Z0hrycBvdX0jBwlB38xKsc/edit
vizDataFrame <- vizDataFrame %>% 
  mutate(category = case_when(
    str_detect(variable, "lynch mob negro men jail hang murder law prison made") ~ "lynch_act",
    str_detect(variable, "law peopl crime race great south lynch countri good public") ~ "critizing_lynching",
    str_detect(variable, "bodi shot fire hang hous jail tree found door head") ~ "lynch_mob",
    str_detect(variable, "negro white murder man year kill lynch charg assault mrs") ~ "lynching_violence",
    #check this
    str_detect(variable, "state court juri governor case unit judg call trial investig") ~ "legal_proceedings",
    str_detect(variable,  "counti sheriff night jail citi morn day deputi mile town") ~ "lynchings",
  ))


vizDataFrame_white_black <- vizDataFrame


```

###Fact Check and Validate Topics
1) theta has the articles and the percentage of likely topics, 1-6
2) topicNames has the corresponding names of the topics
3) filter theta for topic 1, highest percentage, and then look at the topics.
4) Dump the highest pct articles into a folder for researchers to examine
5) Rinse and repeat....

Here are the topics:
  Topic 1: lynching violence - "negro white murder man year kill lynch charg assault mrs"          
Topic 2: lynch-mob - "bodi shot fire hang hous jail tree found door head"  
Topic 3: lynchings - "counti sheriff night jail citi morn day deputi mile town"
Topic 4: lynch act - "lynch mob negro men jail hang murder law prison made"
#check this
Topic 5: criticizing lynching - "law peopl crime race great south lynch countri good public"
Topic 6: legal proceedings? - "state court juri governor case unit judg call trial investig" 

### for legal proceedings?
```{r}
#for topic 1, lynch mob
theta2 <- as.data.frame(theta)

legal<- theta2 %>% 
  #renaming for a general topic
  rename(legal = '6') %>% 
  top_n(20, legal) %>%
  arrange(desc(legal)) %>% 
  select(legal)

# Apply rownames_to_column
legal <- tibble::rownames_to_column(legal, "story_id") 

legal$story_id <- gsub("X", "", legal$story_id)

#Checks out June 23


```


### for critizing_lynching
```{r}
#for topic 1, lynch mob
theta2 <- as.data.frame(theta)

critizing<- theta2 %>% 
  #renaming for a general topic
  rename(critizing = '5') %>% 
  top_n(20, critizing) %>%
  arrange(desc(critizing)) %>% 
  select(critizing)

# Apply rownames_to_column
critizing <- tibble::rownames_to_column(critizing, "story_id") 

critizing$story_id <- gsub("X", "", critizing$story_id)

#Checks out June 23


```


### For lynchings
```{r}
#for topic 1, lynch mob
theta2 <- as.data.frame(theta)

lynchings <- theta2 %>% 
  #renaming for a general topic
  rename(law_enforcement = '3') %>% 
  top_n(20, law_enforcement) %>%
  arrange(desc(law_enforcement)) %>% 
  select(law_enforcement)

# Apply rownames_to_column
lynchings <- tibble::rownames_to_column(lynchings, "story_id") 

lynchings$story_id <- gsub("X", "", lynchings$story_id)

#Checks out June 23


```

### For lynching_violence
```{r}
#for topic 1, lynch mob
theta2 <- as.data.frame(theta)

lynching_violence_topic <- theta2 %>% 
  #renaming for a general topic
  rename(lynching_violence = '4') %>% 
  top_n(20, lynching_violence) %>%
  arrange(desc(lynching_violence)) %>% 
  select(lynching_violence)

# Apply rownames_to_column
lynching_violence_topic <- tibble::rownames_to_column(lynching_violence_topic, "story_id") 

#Checks out June 21
#definitely a lot of lynching coverage but on many different aspects

```


### For unclear
```{r}
#for topic 1, lynch mob
theta2 <- as.data.frame(theta)

unclear_topic <- theta2 %>% 
  #renaming for a general topic
  rename(unclear = '4') %>% 
  top_n(20, unclear) %>%
  arrange(desc(unclear)) %>% 
  select(unclear)

# Apply rownames_to_column
unclear_topic <- tibble::rownames_to_column(unclear_topic, "story_id") 

#Checks out June 21
#no particular pattern seen in the top articles.

```

### For lynch law
```{r}
#for topic 1, lynch mob
theta2 <- as.data.frame(theta)

lynch_law_topic <- theta2 %>% 
  #renaming for a general topic
  rename(lynch_law = '1') %>% 
  top_n(20, lynch_law) %>%
  arrange(desc(lynch_law)) %>% 
  select(lynch_law)

# Apply rownames_to_column
lynch_law_topic <- tibble::rownames_to_column(lynch_law_topic, "story_id") 

#Checks out June 21
#lynch law and lynching coverage
```

### Copy selected files to a new directory
```{r}
#This tutorial shows how to copy files from one directory to another
# https://stackoverflow.com/questions/68995687/r-move-files-to-folder-based-on-list-or-column

inputdir  <- "/Users/robwells/Code/hcij_lynching_phase_two/articles_cleaned_2023_03_08" 

#Create a new folder for the filtered topics
targetdir <- "/Users/robwells/Code/hcij_lynching_phase_two/narratives/code/topic_lynch_law" 


df <- lynch_mob_topic$story_id
filestocopy <- list.files(inputdir, full.names = TRUE)

filestocopy <- unique(grep(paste(df,collapse="|"), filestocopy, value=TRUE))

sapply(filestocopy, function(x) file.copy(from=x, to=targetdir, copy.mode = TRUE))
```





#Figure 15: mainstrean_topics_june23_2024

```{r}
# plot topic proportions per decade as bar plot
ggplot(vizDataFrame, aes(x=decade, y=value, fill=category)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "decade") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values=c("#9933FF",
                             "#33FFFF",
                             "red",
                             "yellow",
                             "darkblue",
                             "green"))+
  #                           "blue"))+ 
  #                           #"pink",
  #                           #"gray",
  #                           #"orange")) +
  labs(title = "Common Narratives in Lynching News Coverage",
       subtitle = "Six Probable Topics in 9,589 extracted articles",
       caption = "Aggregate mean topic proportions per decade. Graphic by (redated - peer review) & (redated - peer review), 6-23-2024")

ggsave(here::here("../lynching_press/output_images_tables/Article_Images/Figure_15_mainstrean_topics_june23_2024.png"),device = "png",width=9,height=6, dpi=800)
```




# Counting of Topics and Visualization

## Topic ranking{-}

First, we try to get a more meaningful order of top terms per topic by re-ranking them with a specific score [@Chang2009]. The idea of re-ranking terms is similar to the idea of TF-IDF. The more a term appears in top levels w.r.t. its probability, the less meaningful it is to describe the topic. Hence, the scoring advanced favors terms to describe a topic.

```{r tm13}
# re-rank top topic terms for topic names
topicNames <- apply(lda::top.topic.words(beta, 10, by.score = T), 2, paste, collapse = " ")
```
We count how often a topic appears as a primary topic within a paragraph This method is also called Rank-1.

```{r tm16}
countsOfPrimaryTopics <- rep(0, K)
names(countsOfPrimaryTopics) <- topicNames
for (i in 1:nDocs(DTM)) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}
sort(countsOfPrimaryTopics, decreasing = TRUE)
```
```{r tm17}
so <- sort(countsOfPrimaryTopics, decreasing = TRUE)
paste(so, ":", names(so))

#create df
x <- unlist(so)  
dl <- data.frame(ID = rep(names(x), sapply(x, length)),
                 Obs = unlist(x))
top_topics <- melt(dl) %>% 
  select(ID, value)

#write.csv(top_topics, "top_topics_3_8.csv")

```


```{r}
# get topic counts per decade
#topic_count_per_decade <- aggregate(theta, by = list(decade = textdata$decade), sum)
topic_count_per_decade <- aggregate(theta, by = list(decade = textdata$decade), sum)
# set topic names to aggregated columns
colnames(topic_count_per_decade)[2:(K+1)] <- topicNames
# reshape data frame
vizDataFrame2 <- melt(topic_count_per_decade, id.vars = "decade")

# #filter out 1960 - one article
# vizDataFrame2 <- vizDataFrame2 %>% 
#    filter(!decade==1960)
```

#fact check
```{r}
colSums(topic_count_per_decade[2:7])

# the process of calculating the topics by decade and aggregating them over the entire corpus yields slightly different terms and therefore slightly different results. They are generally similar except the decade aggregations are slightly different and undercounts

#        negro lynch mob white murder jail kill hang assault shot 
#                                                           512.5 
# law lynch mob crime peopl state man murder men citizen 
#                                                           207.3 
# lynch mob state court counti prison governor juri sheriff trial 
#                                                           205.4 
#               mob crowd men jail man door street made shot rope 
#                                                           174.1 
#             man murder year wife home girl time day found night 
#                                                           162.2 
# negro lynch white worker nation unit state south organ american 
#                                                           125.6 

# Here are the total counts that are not aggregated by decade, which yield different results but are pretty similar

# [1] "627 : negro lynch mob white jail assault shot kill murder hang"         
# [2] "221 : law lynch crime punish peopl state case mob justic fact"          
# [3] "184 : lynch governor court juri counti state prison sheriff trial grand"
# [4] "139 : crowd door jail men street mob rope shot made open"               
# [5] "125 : wife girl son hors year father brother home bodi man"             
# [6] "91 : white negro worker american lynch nation presid unit class south"  

# I think the difference is the summing of the theta probabilities when they are broken out by decades
#topic_count_per_decade <- aggregate(theta, by = list(decade = textdata$decade), sum)

```

```{r}
#add categories
#these have not been verified with Ground Truth as of June 27
vizDataFrame2 <- vizDataFrame2 %>% 
  mutate(category = case_when(
    str_detect(variable, "negro lynch mob white jail assault shot kill murder hang") ~ "lynch_mob",
    str_detect(variable, "lynch governor court juri counti state prison sheriff trial grand") ~ "state",
    str_detect(variable, "law lynch crime punish peopl state case mob justic fact") ~ "legal_issues",
    str_detect(variable, "crowd door jail men street mob rope shot made open") ~ "lynch_mob",
    str_detect(variable, "wife girl son hors year father brother home bodi man") ~ "victim",
    str_detect(variable, "white negro worker american lynch nation presid unit class south") ~ "misc_lynching",
  ))

```

```{r}
# plot topic proportions per decade as bar plot
ggplot(vizDataFrame2, aes(x=decade, y=value, fill=category)) + 
  geom_bar(stat = "identity") + ylab("count of topics") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "decade") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values=c("#9933FF",
                             "#33FFFF",
                             "red",
                             "yellow",
                             "darkblue"))+
  #                           "green",
  #                           "blue"))+ 
  #                           #"pink",
  #                           #"gray",
  #                           #"orange")) +
  labs(title = "Common Narratives in Lynching News Coverage",
       subtitle = "Counts of Probable Topics in 1,387 extracted articles",
       caption = "Topic Modeling: Counts of topic proportions per decade. Graphic by (redated - peer review), 3-16-2023")


```
We sort topics according to their probability within the entire collection:
  
  ```{r tm14}
# What are the most probable topics in the entire collection?
topicProportions <- colSums(theta) / nDocs(DTM)  # mean probabilities over all paragraphs
#longer topic names for this dataframe
topicNames <- apply(terms(topicModel2, 10), 2, paste, collapse = " ")  # reset topicnames
names(topicProportions) <- topicNames     # assign the topic names we created before
sort(topicProportions, decreasing = TRUE) # show summed proportions in decreased order

x <- topicProportions %>% 
  as.data.frame()

x <- tibble::rownames_to_column(x, "row_names")

x <- x %>% 
  rename(proportion = '.')

x <- x %>% 
  arrange(desc(proportion))
x

x <- x %>% 
  mutate(category = case_when(
    str_detect(proportion, "0.36949") ~ "cause_of_lynching",
    str_detect(proportion, "0.14946") ~ "lynch_law",
    str_detect(proportion, "0.14806") ~ "legal_proceedings",
    str_detect(row_names, "mob crowd men jail man door street made shot rope") ~ "lynch_mob",
    str_detect(proportion, "0.11691") ~ "misc_lynching",
    str_detect(proportion, "0.09057") ~ "call_to_action",
  ))
write.csv(x, "../output/twentytopics_6_27_2023.csv")

```


```{r}
xx <- vizDataFrame %>% 
  select(variable, value) %>% 
  group_by(variable) %>% 
  summarise(sum = sum(value)) %>% 
  arrange(desc(sum))

```


######### NOTES BELOW ############
Filter vizDataFrame
```{r}
vizDataFrame3 <- vizDataFrame %>% 
  filter(grepl ("sheriff", variable))

# vizDataFrame1 <- vizDataFrame %>% 
#   filter(grepl ("sheriff", variable))

```




```{r}
# plot topic proportions per decade as bar plot
ggplot(vizDataFrame3, aes(x=decade, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "decade") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Sheriff Term in Lynching News Coverage",
       subtitle = "Word to Filter Topic in 1,387 extracted articles",
       caption = "Aggregate mean topic proportions per decade. Graphic by (redated - peer review), 4-3-2023")


```

```{r}
#filter for The act of lynching, represented by:


bfd <- vizDataFrame %>% 
  filter(category == "lynch_mob") %>% 
  arrange(decade)
```

```{r}
# plot topic proportions per decade as bar plot
ggplot(bfd, aes(x=decade, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "decade") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Mob Narratives In News Coverage",
       subtitle = "Probable Topics in 1,387 extracted articles",
       caption = "Aggregate mean topic proportions per decade. Graphic by (redated - peer review), 4-3-2023")


```


```{r}
#filter for The act of lynching, represented by:

bfd <- vizDataFrame %>% 
  filter(variable == "shot kill fire wound dead") %>% 
  arrange(decade)
```

```{r}
# plot topic proportions per decade as bar plot
ggplot(bfd, aes(x=decade, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Graphic Violence Narratives In News Coverage",
       subtitle = "Probable Topics in 1,465 extracted articles",
       caption = "Aggregate mean topic proportions per decade. Graphic by (redated - peer review), 2-22-2023")


```



```{r}
#filter for Interaction of the mob and law enforcement, represented by:

bfd <- vizDataFrame %>% 
  filter(variable == "mob jail negro sheriff prison") %>% 
  arrange(decade)
```

```{r}
# plot topic proportions per decade as bar plot
ggplot(bfd, aes(x=decade, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Law Enforcement During Lynching Narratives In News Coverage",
       subtitle = "Probable Topics in 1,465 extracted articles",
       caption = "Aggregate mean topic proportions per decade. Graphic by (redated - peer review), 2-22-2023")


```


# Citation & Session Info {-}

This code was adapted from the following:
  
  Schweinberger, Martin. `r format(Sys.time(), '%Y')`. *Topic Modeling with R*. Brisbane: The University of Queensland. url: https://slcladal.github.io/topicmodels.html (Version `r format(Sys.time(), '%Y.%m.%d')`).


```
@manual{schweinberger`r format(Sys.time(), '%Y')`topic,
  author = {Schweinberger, Martin},
  title = {Topic Modeling with R},
  note = {https://slcladal.github.io/topicmodels.html},
  year = {`r format(Sys.time(), '%Y')`},
  organization = "The University of Queensland, Australia. School of Languages and Cultures},
  address = {Brisbane},
  edition = {`r format(Sys.time(), '%Y.%m.%d')`}
}
```

[Back to HOME](https://slcladal.github.io/index.html)

# Keyword Topic Modeling
### Loading data for quanteda
```{r}
install.packages("keyATM")
library(keyATM)
#https://keyatm.github.io/keyATM/


# How to cite
# 
#     Shusei Eshima, Kosuke Imai, and Tomoya Sasaki. 2023. “Keyword Assisted Topic Models.” American Journal of Political Science. Paper (arXiv).
# 
#keyATM can read a document-feature matrix (dfm object) created by quanteda package (this method is strongly recommended).
library(quanteda)
library(readtext)

# Read text files
raw_docs <- readtext("PATH_TO_THE_FOLDER/*.txt",
                     encoding = "UTF-8")
raw_docs <- lynch1 

# Preprocessing with quanteda and create a dfm object
#key_corpus <- corpus(raw_docs, text_field = "text")
raw_docs  <- lynch1 %>%
  select(doc_id, text, newspaper_state, newspaper_name, date, year) %>% 
  distinct(doc_id, .keep_all = TRUE) %>% 
  as.data.frame()

# raw_docs %>% 
#   count(doc_id) %>% 
#   arrange(desc(n))

# load stopwords
english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")
# create corpus object
#following tutorial: https://keyatm.github.io/keyATM/articles/pkgdown_files/Preparation.html


key_corpus <- corpus(raw_docs, text = "text")

# If you use the covariate model, please consider using `docvars` argument
# key_corpus <- corpus(raw_docs, text_field = "text", docvars = COVARIATES)

# You can conduct a variety of preprocessing in this step as shown in the next section
key_token <- tokens(key_corpus)

# Create a document-feature matrix (a dfm object) from a token object
key_dfm <- dfm(key_token)

```

### Preprocessing Data
```{r}
library(magrittr)
data_tokens <- tokens(
    key_corpus,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_separators = TRUE,
    remove_url = TRUE
  ) %>%
  tokens_tolower() %>%
  tokens_remove(
    c(stopwords("english"),
      "may", "shall", "can",
      "must", "upon", "with", "without"
    )
  ) %>%
  tokens_select(min_nchar = 3)

```

### Create DFM
```{r}
data_dfm <- dfm(data_tokens) %>%
              dfm_trim(min_termfreq = 5, min_docfreq = 2)
ncol(data_dfm)  # the number of unique words
#12741

```
### keyATM 
```{r}
keyATM_docs <- keyATM_read(texts = data_dfm)
summary(keyATM_docs)


```

```{r}
keyATM_docs0 <- keyATM_read(texts = data_dfm_len0)

data_dfm_rm0 <- dfm_subset(data_dfm, ntoken(data_dfm) > 0)
```


###Preparing keywords: Create keywords list

```{r}
keywords <- list(
  victim = c("wife", "girl"),
  lynch_mob = c("mob", "rope", "crowd"),
  legal_proceedings = c("punish", "trial", "grand"),
  graphic_violence = c("dragged", "riddled"),
  politics = c("governor", "prison")
)

```


### Checking keywords

```{r}
# 
# Keywords should appear reasonable times (typically more than 0.1% of the corpus) in the documents. The visualize_keywords() function plots the frequency of keywords by topic.

key_viz <- visualize_keywords(docs = keyATM_docs, keywords = keywords)
key_viz

save_fig(key_viz, "../output/keyword.pdf", width = 6.5, height = 4)

#Get actual values
values <- values_fig(key_viz) %>% 
  as.data.frame()

write.csv(values, "../output/keyATM_values.csv")

```

### Choosing keywords with an unsupervised topic model
```{r}
set.seed(225)  # set the seed before split the dfm
docs_withSplit <- keyATM_read(texts = data_dfm,
                              split = 0.3)  # split each document

out <- weightedLDA(
  docs              = docs_withSplit$W_split,  # 30% of the corpus
  number_of_topics  = 10,  # the number of potential themes in the corpus
  model             = "base",
  options           = list(seed = 250)
)
top_words(out)  # top words can aid selecting keywords

out <- keyATM(
  docs              = docs_withSplit,  # 70% of the corpus
  no_keyword_topics = 5,               # number of topics without keywords
  keywords          = keywords,        # selected keywords
  model             = "base",          # select the model
  options           = list(seed = 250)
)


```

### Saving the model
```{r}
#tutorial continues: https://keyatm.github.io/keyATM/articles/pkgdown_files/keyATM_base.html

save(out, file = "../output/keyATM_topic_model.rds")

#To load the model, you can use readRDS() function.

#out <- readRDS(file = "SAVENAME.rds")


```


### Interpreting results
```{r}
top_words(out)


```

### Plot

```{r}
plot<- plot_topicprop(out, show_topic = 1:5)


save_fig(plot, "../output/plot_keyword.pdf", width = 6.5, height = 4)
```

### explore documents that are highly associated with each topic
```{r}
top_docs(out)

```

### visualize 𝜶

```{r}
#keyATM can visualize 𝜶, the prior for the document-topic distribution, and 𝝅, the probability that each topic uses keyword topic-word distribution. Values of these parameters should also stabilize over time.
plot_alpha(out)

```

###  plot 𝝅, the probability that each topic uses keyword topic-word distribution. 
```{r}

plot_pi(out)

#ℹ Plotting pi from the final MCMC draw. Please set `store_pi` to `TRUE` if you want to plot pi over iterations.
```
