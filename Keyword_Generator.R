library(tidyverse)
library(udpipe)
library(stringr)
library(plyr)
library(dplyr)

#reads in list of titles and exclusion words 
df1 <- read_csv("C:/Users/DPhillips/Department of Health and Social Care/Evidence and Analysis - Analytical Leadership/Statistics and Data Science/Data/Data catalogue/DHSC Data Catalogue/Keyword Generator/test_titles.csv") %>%
  select(Title = "Title")

#reads in exclusion list .csv
exclusion_words <- read_csv("C:/Users/DPhillips/Department of Health and Social Care/Evidence and Analysis - Analytical Leadership/Statistics and Data Science/Data/Data catalogue/DHSC Data Catalogue/Keyword Generator/Exclusion_Words.csv")

#reads in list of ngrams .csv
ngrams <- read_csv("C:/Users/DPhillips/Department of Health and Social Care/Evidence and Analysis - Analytical Leadership/Statistics and Data Science/Data/Data catalogue/DHSC Data Catalogue/Keyword Generator/ngrams.csv")


#downloads language model
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

#splits titles by word and assigns word type
system.time(
  word_types <- udpipe_annotate(ud_model, x = df1$Title)
)
word_types <- as.data.frame(word_types)

#sets all words to lower case
word_types <- word_types %>% mutate(token = tolower(token))


#assigns selection priority based on word type
word_types$order <- ifelse(word_types$upos == "PROPN", 1, 
                    ifelse(word_types$upos == "NOUN", 1,
                    ifelse(word_types$upos == "VERB", 1, 
                    ifelse(word_types$upos == "ADJ", 1, 
                    ifelse(word_types$upos == "X", 1, 
                    ifelse(word_types$upos == "AUX", 1, 3))))))

 
#reduces priority for words in the exclusion list
for (i in 1:nrow(exclusion_words)) {
  
  word_types$order <- ifelse(grepl(exclusion_words$word[i], word_types$token, fixed = "TRUE") == "TRUE", 2, word_types$order)
  
}

#create column with concatenation of word in row and row below 
for (k in 1:(nrow(word_types) - 1)) {
  
  word_types$phrase[k] <- paste(word_types$token[k], word_types$token[k + 1], sep=" ")
  
}

#joins concatenated column to list of ngrams
word_types <- left_join(word_types, ngrams, by = "phrase")

#loops over list to add in ngrams
for (j in 1:(nrow(word_types) - 1)) {
  
  # if flag is 1 then replace the word with the ngram 
  if(is.na(word_types$flag[j])) {word_types$flag[j] = FALSE} else {if(word_types$flag[j] == 1) {
    
    word_types$token[j] <- word_types$phrase[j]
    word_types$order[j] <- 1
    
    #flags row below for deletion
    word_types$flag[j + 1] <- 2
    
  }}
 
  
}

#deletes redundant rows
word_types <- word_types %>% filter(flag !=2)


#reduces priority for words one letter long
word_types$length <- str_count(word_types$token)
word_types$order <- ifelse(word_types$length == 1 , 4, word_types$order) 

#defines output dataframe
output <- matrix(ncol = 2, nrow = nrow(df1))
output <- as.data.frame(output) 

#loops for every title
for (x in 1:nrow(word_types)) {
  
  #selects rows for corresponding title
  df2 <- word_types %>% filter(doc_id == paste("doc", x, sep="") )
  
  #sorts based on priority
  df2 <- arrange(df2, order) 
  
  #selects two highest priority words
  keywords <- head(df2[,c(6, 15)], 2)
  keywords <- as.data.frame(keywords)
  
  #checks if only one word is presence 
  if (nrow(keywords) == 1) {
      
      #assigns word to output dataframe
      output[x, 1] <- head(keywords, 1)
      output[x, 2] <- ""
      
    
  } else {
  
      #checks if both keywords are the same
      if (grepl(head(keywords, 1), tail(keywords, 1), fixed = "TRUE") == "TRUE") {
              
              #selects three highest priority words
              keywords <- head(df2[,c(6, 15)], 3)
              keywords <- as.data.frame(keywords)
              
            }
    
      #assigns words to output dataframe
      output[x, 1] <- head(keywords, 1)
      output[x, 2] <- tail(keywords, 1)
      
  
  }
  
}

#copy to clipboard
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard-16384",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(output)


