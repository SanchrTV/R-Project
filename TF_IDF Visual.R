library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidytext)
library(stopwords)

df <- read.csv('/Users/alex/Desktop/ВШЭ/ML/Проекты/Project ML/df for proj.csv', sep = ';')
df <- as.data.frame(df)

book_words <- df %>%
  unnest_tokens(word, text) %>%
  count(unique_tag, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(unique_tag) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

freq_by_rank <- book_words %>% 
  group_by(unique_tag) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)


book_tf_idf <- book_words %>%
  bind_tf_idf(word, unique_tag, n)

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_tf_idf

library(forcats)

book_tf_idf %>%
  group_by(unique_tag) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = unique_tag)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~unique_tag, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


write.csv(book_tf_idf,'/Users/alex/Desktop/ВШЭ/ML/Проекты/Project ML\\tf-idf_R.csv', row.names = FALSE)
