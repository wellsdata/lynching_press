Notes on data cleaning and tokenizing the text from June 18 2024


#Filtering and combining indexes
```{r}
#import old index of 1387 files
jackindex_march8 <- rio::import("../data/jackindex_march8.csv")

#import index of 1673 articles
jack_ORIGINAL_index <- read_csv("~/Code/hcij_lynching_phase_two/Storage_Older_Versions/article_text_2023-02-01_original_bak/LayoutBoxes_merged_20230123-162719_index.csv")

#import Oct 19 extraction index of 6507 articles (Aug 23: 3,385 articles)
#matches the number of articles in /Users/robwells/Code/hcij_lynching_phase_two/articles_10_19

jackindex_oct19 <- read_csv("../data/articles_10_19.csv")

```
# Fix URLs

```{r}
# Fix URLs

jackindex_oct19$url_fixed <- sub("sn\\d+/\\K.*", "", jackindex_oct19$URL, perl = TRUE)
jackindex_oct19$url_fixed <- sub("/$", "", jackindex_oct19$url_fixed)

#create date
jackindex_oct19 <- jackindex_oct19 %>% 
  mutate(date = paste(month,day,year, sep="/")) %>% 
  mutate(date = lubridate::mdy(date)) 

#date fixed for url
jackindex_oct19$date2 <- as.character(jackindex_oct19$date)
jackindex_oct19$date2 <- gsub("-", "/", jackindex_oct19$date2)


#create sequence column
# jackindex_oct19$seq <- sub(".*/(seq-\\d+).*", "\\1", jackindex_oct19$url)

jackindex_oct19 <- jackindex_oct19 %>% 
  mutate(url2 = paste(url_fixed, date2, edition, page, "0?user_id=6", sep="/")) 

jackindex_oct19 <- subset(jackindex_oct19, select =-URL)

jackindex_oct19 <- jackindex_oct19 %>% 
  rename(URL = url2) 

#write_csv(jackindex_oct19, "../data/jackindex_fixed_oct19.csv")



```


```{r}
#1090 files in the new index match the original. 
matched <-  jack_ORIGINAL_index %>% 
  inner_join(jackindex_oct19, by=c("file_id",  "article_id"))

#these are the 5417 new articles from the extraction 
new <-  jackindex_oct19 %>% 
  anti_join(jack_ORIGINAL_index, by=c("file_id",  "article_id"))

```


#Checking discrepancy between index and files
```{r}
#list of all 10,776 articles in folder
folder_files <- list.files("/Users/robwells/Code/lynching_press/data/articles_may_16_2024", pattern="*.txt") %>%
  as.data.frame() %>%
  rename(file_id = 1) %>% 
  mutate(index = 1:length(file_id)) 

#cut .txt
folder_files$file_id <- gsub(".txt", "", folder_files$file_id)

#merge the file_id in extracted article index 5_16
# jackindex_oct19 <- jackindex_oct19 %>% 
#   mutate(file_id2 = (paste(file_id, article_id, sep = '_')))


#10735 files in the folder and the index
files_in <- master_article_index_4_20 %>% 
  inner_join(folder_files, by=c("file_id2"="file_id"))

#2576 files not in the folder but in the index
files_out <- master_article_index_4_20 %>% 
  anti_join(folder_files, by=c("file_id2"="file_id"))

#938 LOC articles that are Black press
BP <- files_in %>% 
  filter(black=="Y")

files_in <- files_in %>%
  mutate(file_name = paste0(idcol, "_", article_id)) %>% 
  mutate(date = ymd(date)) %>% 
  rename(index = index.y) %>% 
  subset(select = -c(index.x))

```


### Merge blackindex with cleaned_articles_oct19

```{r}
#714 articles from proquest and Howard, cleaned
blackindex2 <- read.csv("../data/blackindex_master.csv") %>% 
  filter(black_press =="Y") %>% 
  mutate(date = ymd(date))

files_in <- files_in %>%
  mutate(across(c(newspaper_name, newspaper_state, date, url, black_press), trimws))

master_article_index_4_20 <- master_article_index_4_20 %>%
  mutate(across(c(newspaper_name, newspaper_state, date, url, black_press), trimws))

#11449 articles
master_article_index_6_17 <- files_in %>%
  full_join(blackindex2, by = c("file_id", "newspaper_name", "newspaper_state", "date", "year", "url", "black_press", "sn", "article_id", "day", "page", "edition", "mod_id", "index", "article_title", "newspaper_city", "collection", "authors", "document_type", "issn", "find_a_copy", "start_page", "filepath", "black")) %>% 
  subset(select = -c(month.y))

write.csv(master_article_index_6_17, "../data/master_article_index_6_17.csv")
```



```{r}
#Remove articles cleaned out previously
#this file shows the 281 articles dropped during my March 8 and July 17 cleaning process
anti_jack <- read.csv("../data/dropped_articles_July_17.csv")
#clean this file
anti_jack <- anti_jack %>% 
  select(2:14)
#clean article_id
anti_jack$article_id <- gsub("-", "", anti_jack$article_id)
#merge the file_id 
anti_jack <- anti_jack %>% 
  mutate(file_id2 = (paste(file_id, article_id, sep = '_')))

#53 removed
master_article_index_6_17 <- master_article_index_6_17 %>% 
  anti_join(anti_jack, by=c("file_id2"))
#new index is 11396 files

write.csv(master_article_index_6_17, "../data/master_article_index_6_17.csv")
```



# Notes

# finding the articles dropped from the index due to March 8 cleaning
```{r}
# jack_ORIGINAL_index <- read_csv(here:here("~/Code/hcij_lynching_phase_two/Storage_Older_Versions/article_text_2023-02-01_original_bak/LayoutBoxes_merged_20230123-162719_index.csv"))

jack_ORIGINAL_index <- read_csv("~/Code/hcij_lynching_phase_two/Storage_Older_Versions/article_text_2023-02-01_original_bak/LayoutBoxes_merged_20230123-162719_index.csv")

anti_jack <- jack_ORIGINAL_index %>% 
  anti_join(jackindex, by=("file_id")) %>% 
  distinct()

anti_jack %>% 
  count(file_id) %>% 
  arrange(desc(n))

#this file shows the 286 articles dropped during my March 8 cleaning process: 1673 articles in jack Feb 2 index down to 1387 articles now
#write.csv(anti_jack, "~/Code/hcij_lynching_phase_two/dropped_articles_march_8.csv")

```

#121 articles do not match the original list
excluded<- jackindex_march8 %>% 
  anti_join(jackindex_july16, by=c("file_id", "article_id")) %>% 
  distinct()

#1432 articles on new list not on old list
excluded1 <- jackindex_july16 %>% 
  anti_join(jackindex_march8, by=c("file_id", "article_id")) %>% 
  distinct()

#1182 articles on new list not on pre-March8 list
excluded2 <- jackindex_july16 %>% 
  anti_join(jack_ORIGINAL_index, by=c("file_id", "article_id")) %>% 
  distinct()

#160 articles on old list not on new July 16 list
excluded3 <- jack_ORIGINAL_index %>% 
  anti_join(jackindex_july16, by=c("file_id",  "article_id"))

# excluded3 <- jack_ORIGINAL_index %>% 
#   anti_join(jackindex_july16, by=c("file_id", "article_id")) %>% 
#   distinct()

#test - pre-march and march 8 correctly show 286 articles cut from new list
excluded5 <- jack_ORIGINAL_index %>% 
  anti_join(jackindex_march8, by=c("file_id",  "article_id"))

```{r}
notes on previous method which failed due to memory issues

#####################
# Begin SM Code #####
#####################



###
# Load 1,387 stories I reviewed and cleaned on March 8
# previously: Load 1,466  stories provided by jack, create join column, join to files list (previously 848 stories)
#Previous index: jackindex <- read_csv("../sample_corpus_1pct_article_text/stratified_sample_manifest.csv")
###

#Loads the 2246 files from July 16
# files <- list.files("~/Code/hcij_lynching_phase_two/articles_july_16", pattern="*.txt") %>% 
#   as.data.frame() %>%
#   rename(filename = 1) %>%
#   filter(!str_detect(filename,"_bak"))

# jackindex <- rio::import("../data/july_16_article_index.csv") %>%
#   mutate(filename = paste0(file_id,"_",article_id,".txt")) %>%
#   inner_join(files) %>%
#   mutate(filepath = paste0("/Users/robwells/Code/hcij_lynching_phase_two/articles_july_16/",filename))

# index <- rio::import("../data/master_article_index_june_18_2024.csv") %>% 
#   mutate(filepath = paste0("~/Code/lynching_press/data/articles_may_16_2024/",file_id))
###
# Define function to loop through each text file referenced in jackindex df
###

create_article_text <- function(row_value) {
  
  #row_value is the single argument that is passed to the function
  # Take each row of the dataframe
  temp <- index %>%
    slice(row_value)
  
  # Store the filename for  use in constructing articles dataframe
  temp_filename <- temp$filename
  
  # Create a dataframe by reading in lines of a given textfile
  # Add a filename column 
  articles_df_temp <- read_lines(temp$filepath) %>%
    as_tibble() %>%
    mutate(filename = temp_filename)
  
  # Bind results to master articles_df
  # <<- returns to global environment
  articles_df <<- articles_df %>%
    bind_rows(articles_df_temp)
}

###
# Create elements needed to run function
###

# Create empty tibble to store results
articles_df <- tibble()
#running once to test
#create_article_text(2) 
# Create an array of numbers to loop through, from 1 to the number of rows in our index dataframe 
row_values <- 1:nrow(index)

###
# Execute function using lapply
# This loops through each row of the dataframe and append results to master file
###

lapply(row_values, create_article_text)

# Error: The size of the connection buffer (131072) was not large enough
# to fit a complete line:
# * Increase it by setting `Sys.setenv("VROOM_CONNECTION_SIZE")`
#https://github.com/tidyverse/vroom/issues/364
#Sys.setenv(VROOM_CONNECTION_SIZE = 10000000)


###
# Clean up articles_df and join to index dataframe
###

articles_df <- articles_df %>%
  select(filename, sentence=value) %>%
  inner_join(index)

#write.csv(articles_df, "../data/articles_oct_19.csv")

#####################
# End SM Code #####
#####################

# Count the number of unique file names
#9408
articles_df %>%
  summarize(n_unique_files = n_distinct(filename))

# write.csv(articles_df, "articles_july_16.csv")
# lynch <- articles_df



To handle a large number of files without running into memory exhaustion issues, you need to process the files in smaller chunks and possibly save intermediate results to disk to avoid keeping everything in memory at once. Here's an approach that processes files in chunks and writes intermediate results to disk using write_csv and then combines them at the end.

Step-by-Step Approach:
Process Files in Chunks: Read, process, and save intermediate results to disk in chunks.
Combine Intermediate Results: After processing all chunks, combine the intermediate results.
Here is the revised code:

r
Copy code
library(dplyr)
library(readr)
library(purrr)

# Function to process a chunk of rows
process_chunk <- function(chunk_rows, chunk_index) {
  articles_chunk <- tibble()
  
  for (row_value in chunk_rows) {
    temp <- index %>%
      slice(row_value)
    
    temp_filename <- temp$filename
    
    # Read lines from file
    tryCatch({
      articles_df_temp <- read_lines(temp$filepath) %>%
        as_tibble() %>%
        mutate(filename = temp_filename)
      
      # Append to chunk results
      articles_chunk <- bind_rows(articles_chunk, articles_df_temp)
    }, error = function(e) {
      message(paste("Error processing row", row_value, ":", conditionMessage(e)))
    })
  }
  
  # Write chunk to disk
  write_csv(articles_chunk, paste0("articles_chunk_", chunk_index, ".csv"))
}

# Determine chunk size and create chunks
chunk_size <- 500  # Adjust chunk size as needed
row_values <- split(1:nrow(index), ceiling(seq_along(1:nrow(index))/chunk_size))

# Process each chunk
walk2(row_values, seq_along(row_values), process_chunk)

# Combine all chunks into one data frame
chunk_files <- list.files(pattern = "articles_chunk_.*\\.csv")

articles_df <- map_dfr(chunk_files, read_csv)

# Perform the inner join after all files are processed
joined_df <- articles_df %>%
  select(filename, sentence = value) %>%
  inner_join(index, by = "filename")

# View the resulting data frame
print(head(joined_df))
Explanation:
Processing in Chunks:

process_chunk function processes a specified chunk of rows and writes the result to a CSV file.
chunk_size is set to 500, meaning it will process 500 rows at a time. Adjust this size based on your memory capacity.
split(1:nrow(index), ceiling(seq_along(1:nrow(index))/chunk_size)) splits the rows into chunks.
Writing Intermediate Results:

Each processed chunk is written to a CSV file using write_csv, with filenames like articles_chunk_1.csv, articles_chunk_2.csv, etc.
Combining Results:

list.files(pattern = "articles_chunk_.*\\.csv") lists all chunk files.
map_dfr(chunk_files, read_csv) reads and combines all the chunk files into one data frame articles_df.
Inner Join:

After combining all the chunks, inner_join is performed on articles_df and index based on the filename column.
This approach should help in managing memory usage efficiently and avoid running into memory exhaustion issues. Adjust the chunk size based on your specific system and dataset size.


