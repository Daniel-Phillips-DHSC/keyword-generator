library(tidyverse)
library(udpipe)
library(stringr)
library(plyr)
library(dplyr)

#reads in list of titles and exclusion words 
df1 <- read_csv("C:/Users/DPhillips/Department of Health and Social Care/Evidence and Analysis - Analytical Leadership/Statistics and Data Science/Data/Data catalogue/DHSC Data Catalogue/Keyword Generator/test_titles.csv") %>%
  select(Title = "Title")
exclusion_words <- read_csv("C:/Users/DPhillips/Department of Health and Social Care/Evidence and Analysis - Analytical Leadership/Statistics and Data Science/Data/Data catalogue/DHSC Data Catalogue/Keyword Generator/Exclusion_Words.csv") 

df1 <- arrange(df1, Title) 

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
word_types$order <- ifelse(word_types$xpos == "NNP" , 1, 
                            ifelse(word_types$xpos == "NNPS", 1,
                                   ifelse(word_types$xpos == "NNS", 1,
                                          ifelse(word_types$xpos == "NN", 1,
                                                 ifelse(word_types$xpos == "VBZ", 1,
                                                      ifelse(word_types$xpos == "JJ", 1, 3))))))

 
#reduces priority for words in the exclusion list
for (i in 1:nrow(exclusion_words)) {
  
  word_types$order <- ifelse(grepl(exclusion_words$word[i], word_types$token, fixed = "TRUE") == "TRUE", 2, word_types$order)
  
}

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
  keywords <- head(df2$token, 2)
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
              keywords <- head(df2$token, 3)
              keywords <- as.data.frame(keywords)
              
            }
    
      #assigns words to output dataframe
      output[x, 1] <- head(keywords, 1)
      output[x, 2] <- tail(keywords, 1)
  
  }
  
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard-16384",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(output)



