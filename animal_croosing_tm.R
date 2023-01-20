### Always install them first!!!
setwd("~/Grad School/SIS_AU/R/Working Directory")
library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)

# Animal Crossing Reviews
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')

user_reviews %>%  count(.,user_name,sort = TRUE)

user_reviews %>% head(10) %>% pull(text)

# - gets rid of selected variable
user_reviews <- user_reviews %>% select(-date)

# we don't need words below since they don't give us much info
user_reviews %>% 
  unnest_tokens(output = word,
                input = text) %>% 
  count(word, sort = T)

# anti_join removes any matches with the words from the stop_words list
# Hence, the code below shows only useful words 
user_reviews %>% 
  unnest_tokens(output = word,
                input = text) %>%
  anti_join(stop_words,by = 'word') %>% 
  count(word,sort = T)

review_words <- user_reviews %>% 
  unnest_tokens(output = word,
                input = text) %>% 
  anti_join(stop_words, by = 'word') %>% 
  filter(str_detect(word, '[:alpha:]')) %>% 
  distinct()

# filter the words that are used at least 100 times
user_who_mention_word <- review_words %>% 
  count(word,name = 'users_n') %>% 
  filter(users_n >= 100)

# word and review correlation
word_correlation <- review_words %>% 
  semi_join(user_who_mention_word, by = 'word') %>% 
  pairwise_cor(item = word,feature = user_name) %>%
  filter(correlation >= 0.2)

# Build a word network plot
graph_from_data_frame(d = word_correlation,
                      vertices = user_who_mention_word %>%
                        semi_join(word_correlation,by = c('word' = 'item1'))) %>% 
  ggraph(layout = 'fr') +
  geom_edge_link(aes(alpha = correlation)) +
  geom_node_point() +
  geom_node_text(aes(color = users_n,
                     label = name) , repel = T)

# function to generate a word graph

generate_word_graph <- function(review_words,
                                min_users_n = 100,
                                min_correlation = 0.2) {
  
  user_who_mention_word <- review_words %>% 
    count(word,name = 'users_n') %>% 
    filter(users_n >= min_users_n)
  
  word_correlation <- review_words %>% 
    semi_join(user_who_mention_word, by = 'word') %>% 
    pairwise_cor(item = word,feature = user_name) %>%
    filter(correlation >= min_correlation)
  
  graph_from_data_frame(d = word_correlation,
                        vertices = user_who_mention_word %>%
                          semi_join(word_correlation,by = c('word' = 'item1'))) %>% 
    ggraph(layout = 'fr') +
    geom_edge_link(aes(alpha = correlation)) +
    geom_node_point() +
    geom_node_text(aes(color = users_n,
                       label = name) , repel = T) +
    labs(title = 'Animal Crossing Text Mining')
}

# Made the graph maker easier to use
review_words %>% 
  generate_word_graph(min_users_n = 50,
                      min_correlation = 0.2)

# Check positive and negative reviews
user_reviews %>% count(grade)
  
  reviews_words_negative <- review_words %>% 
  filter(grade < 5)


reviews_words_positive <- review_words %>% 
  filter(grade >= 5)

reviews_words_negative %>% 
  generate_word_graph(min_users_n = 40,
                      min_correlation = 0.2)

tm_graph <- reviews_words_positive %>% 
  generate_word_graph(min_users_n = 30,
                      min_correlation = 0.25)


ggsave('tm_graph.png')
