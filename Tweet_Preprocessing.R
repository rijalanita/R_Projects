#Preprocessing Text data from Tweets collected on COVID, Ebola and Zika using twitter historical archive search
#Author: Anita Rijal
#Version: 1.0
#Date: May 28 2020


library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(tm)
library(stringr)

Full_data_set <- read_delim("./Total_Tweets", delim = ";") #loading dataframe with all tweets

#labelling tweets with epidemic IDs
pandemic<- Full_data_set %>%
  mutate(Pandemic = case_when(row_number() <= 1200 ~ 'Ebola',
                                row_number() <= 2265 ~ 'Zika',
                                TRUE ~ 'Covid-19')) %>% 
                                select(-(1:2), -(6:20))

#cleaning tweet text by removing numbers, punctuations, URLs etc
pandemic$text<- gsub("https://t.co/[a-z,A-Z,0-9]*{8}", '', pandemic$text) #removing https
pandemic$text<- gsub("http://t.co/[a-z,A-Z,0-9]*{8}", '', pandemic$text) #removing http  
pandemic$text <- gsub(pattern = "\\W", replacement = " ", pandemic$text) #removing punctuations 
pandemic$text <- gsub(pattern = "\\d", replacement = " ", pandemic$text) #removing numbers
pandemic$text <- removeWords(pandemic$text, stopwords())
pandemic$text <- gsub( pattern = "\\b[A-z]\\b{1}", replacement = " ", pandemic$text)
pandemic$text <- stripWhitespace(pandemic$text)

#unest words in the pandemic tweets and remove common stop words
unest_words <- pandemic %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)
  
#adding more relevant terms as stop words to refine the data set
my_stop_words <- tibble(word = 
                      c('ebola', 'virus', 'zika', 
                        'covid', 'covid-19', 'amp', 
                        '19', 'covid19', 'coronavirus', 
                        'zikavirus', 'rt', 'RT'))



unest_words <- unest_words %>% 
    anti_join(my_stop_words, by = "word")

#word frequency
total_word_count <- unest_words %>%
  count(word, sort = TRUE)

ebola_word_count <- unest_words %>%
  filter(Pandemic == 'Ebola') %>% 
  count(word, sort = TRUE) %>% 
  mutate(epidemic = 'Ebola')

zika_word_count <- unest_words %>%
  filter(Pandemic == 'Zika') %>% 
  count(word, sort = TRUE) %>% 
  mutate(epidemic = 'Zika')

covid_word_count <- unest_words %>%
  filter(Pandemic == 'Covid-19') %>% 
  count(word, sort = TRUE) %>% 
  mutate(epidemic = 'Covid-19')

#all word frequency
pandemic_word_freq <- rbind(ebola_word_count, zika_word_count, covid_word_count) %>% 
  arrange(desc(n))

pandemic_word_freq <-pandemic_word_freq %>% 
  group_by(epidemic) %>% 
  mutate(total_words = sum(n))

#saving cleaned and unested tweet words
saveRDS(pandemic_word_freq, file="pandemic_tokens")

