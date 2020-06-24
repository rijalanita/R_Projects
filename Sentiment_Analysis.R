# Sentiment Analysis of Pandemic words
# Version: 1.0
#Author: Anita
#Date: May 25, 2020

library(tidytext)
library(textdata)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tm)
library(stringr)
library(gridExtra)

pandemic_tokens <- readRDS("pandemic_tokens")

pandemic_sentiment<- pandemic_tokens %>% 
                  rename(word_count = n) %>% 
                  inner_join(get_sentiments("bing")) %>% 
                  count(sentiment,word, word_count, total_words) %>% 
                  spread(sentiment,n, fill = 0) %>% 
                  mutate(sentiment = positive - negative) 

#Exploring sentiment in epidemic related tweets using NRC texts
pandemic_nrc <- pandemic_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative")) %>% 
  group_by(sentiment, epidemic) %>% 
  summarise(occurence = n())
 
#create plots for each epidemic
sentiment_plot<- lapply(split(pandemic_nrc, pandemic_nrc$epidemic), function(x){
              #reorder sentiment by word count
              x$sentiment = reorder(x$sentiment, x$occurence)
              
              #make the plot
              p <- ggplot(x, mapping = aes(y= occurence, x = sentiment, fill = epidemic)) +
                geom_bar(stat = "identity") + 
                theme_bw()+
                theme(legend.position = "none") +
                labs(title = unique(x$epidemic))+
                xlab("Sentiment")+
                ylab("Occurence of Sentiment")+
                facet_wrap(~epidemic, ncol= 3, scales = "fixed")+
                coord_flip()
})
  
do.call(grid.arrange,(c(sentiment_plot, ncol=3)))

#Do epidemic tweets tend to be more positive and negative in sentiment

sentiment_scores<- pandemic_tokens %>% 
  inner_join(get_sentiments("afinn")) %>%
  slice_max(pandemic_tokens, n = 50, order_by = n) %>% 
  ggplot(aes(x = row_number(word), value, fill = epidemic)) +
  geom_col(show.legend = F)+
  labs(title = "Positive and Negative Words", subtitle= "Sentiment scores across top 50 most common tweet words")+
  ylab("Sentiment Value")+
  xlab("Top 50 Words")+
  facet_grid(rows = vars(epidemic), scales = "fixed")

#What are the top 20 positive and negative words?
names = c("Zika", "Covid-19", "Ebola")

for ( i in (1:3)){
p<- pandemic_tokens %>% 
  filter(epidemic == names[i]) %>% 
  mutate(ranks = n/total_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, ranks, sort = T) %>% 
  slice_max(pandemic_tokens, n = 30, order_by = ranks) %>% 
  ggplot(aes(reorder(word, ranks), n, fill = sentiment))+
  geom_col(show.legend = F)+
  labs(title = paste("Tweets About", names[i], sep = " "), subtitle = "30 most common positive and negative words")+
  xlab(NULL)+
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 15))+
  facet_wrap(~sentiment,ncol=2, scales = "free_y")+
  coord_flip()
print(assign(paste("plot", i, sep = "_"), p))
}


