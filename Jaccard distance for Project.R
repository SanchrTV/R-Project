          #We will see how correlate 2 topic: news and politics
library(tidyverse)
library(tm)

# A function for text preprocessing:
preprocess <- function(x) {
  x <- Corpus(VectorSource(x))
  x <- tm_map(x, function(x) removeWords(x, stopwords("english")))
  x <- x %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) 
  x <- tm_map(x, content_transformer(tolower))
  x <- Boost_tokenizer(x)
  x <-removePunctuation(x)
  x <- stemDocument(x, language = "english")
  return(x)
}

df <- read.csv('/Users/alex/Desktop/ВШЭ/ML/Проекты/Project ML/df for proj.csv', sep = ';')
df <- as.data.frame(df)

                #For News
#Create a vector containing only the text
text_news <- df[df$unique_tag == ' news', ]
# Create a corpus  
docs_news <- Corpus(VectorSource(text_news$text))
docs_news <- docs_news %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) 
docs_news <- tm_map(docs_news, content_transformer(tolower))
docs_news <- Boost_tokenizer(docs_news)
docs_news <- tm_map(docs_news, removeWords, stopwords("english"))
docs_news <- stemDocument(docs_news, language = "english")



                #For Politics
text_politics <- df[df$unique_tag == ' politics', ]
# Create a corpus  
docs_politics <- Corpus(VectorSource(text_politics$text))
docs_politics <- tm_map(docs_politics, function(x) removeWords(x, stopwords("english")))
docs_politics <- docs_politics %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) 
docs_politics <- tm_map(docs_politics, content_transformer(tolower))
docs_politics <- Boost_tokenizer(docs_politics)
docs_politics <-removePunctuation(docs_politics)
docs_politics <- stemDocument(docs_politics, language = "english")


jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

jaccard(docs_news, docs_politics)

text_news <- df[df$unique_tag == ' news', ]$text
docs_news <- preprocess(text_news)
jaccard(docs_news, docs_politics)
jaccard(docs_news, docs_environment)
jaccard(docs_news, docs_life)
jaccard(docs_news, docs_sport)
jaccard(docs_news, docs_culture)
jaccard(docs_news, docs_work)
jaccard(docs_news, docs_technology)
jaccard(docs_news, docs_psychology)
jaccard(docs_news, docs_science)


text_politics <- df[df$unique_tag == ' politics', ]$text
docs_politics <- preprocess(text_politics)
jaccard(docs_politics, docs_news)
jaccard(docs_politics, docs_environment)
jaccard(docs_politics, docs_life)
jaccard(docs_politics, docs_sport)
jaccard(docs_politics, docs_culture)
jaccard(docs_politics, docs_work)
jaccard(docs_politics, docs_technology)
jaccard(docs_politics, docs_psychology)
jaccard(docs_politics, docs_science)



text_environment <- df[df$unique_tag == ' environment', ]$text
docs_environment <- preprocess(text_environment)
jaccard(docs_environment, docs_life)
jaccard(docs_environment, docs_sport)
jaccard(docs_environment, docs_culture)
jaccard(docs_environment, docs_work)
jaccard(docs_environment, docs_technology)
jaccard(docs_environment, docs_psychology)
jaccard(docs_environment, docs_science)




text_life <- df[df$unique_tag == ' life', ]$text
docs_life <- preprocess(text_life)
jaccard(docs_life, docs_sport)
jaccard(docs_life, docs_culture)
jaccard(docs_life, docs_work)
jaccard(docs_life, docs_technology)
jaccard(docs_life, docs_psychology)
jaccard(docs_life, docs_science)

text_sport <- df[df$unique_tag == ' sport and health', ]$text
docs_sport <- preprocess(text_sport)

text_culture <- df[df$unique_tag == ' culture and society', ]$text
docs_culture <- preprocess(text_culture)
jaccard(docs_culture, docs_work)
jaccard(docs_culture, docs_technology)
jaccard(docs_culture, docs_psychology)
jaccard(docs_culture, docs_science)

text_work <- df[df$unique_tag == ' work', ]$text
docs_work <- preprocess(text_work)
jaccard(docs_work, docs_technology)
jaccard(docs_work, docs_psychology)
jaccard(docs_work, docs_science)

text_technology <- df[df$unique_tag == ' technology', ]$text
docs_technology <- preprocess(text_technology)
jaccard(docs_technology, docs_psychology)
jaccard(docs_technology, docs_science)

text_psychology <- df[df$unique_tag == ' psychology', ]$text
docs_psychology <- preprocess(text_psychology)
jaccard(docs_science, docs_psychology)

text_science <- df[df$unique_tag == ' science', ]$text
docs_science <- preprocess(text_science)

jaccard(docs_science, docs_environment)
jaccard(docs_science, docs_politics)

df_for_corr <- read.csv('/Users/alex/Desktop/ВШЭ/R/Project/Jaccard distance for R project.csv', sep = ';')
df_for_corr <- as.data.frame(df_for_corr)

M = cor(df_for_corr)

library(corrplot)

# visualizing correlogram

# as colour
corrplot(df_for_corr, method="color")

# as number
corrplot(df_for_corr, method="number")






df_cor <- data.frame(docs_news, 
                     docs_politics, 
                     docs_environment, 
                     docs_life, 
                     docs_sport, 
                     docs_culture, 
                     docs_work, 
                     docs_technology, 
                     docs_psychology, 
                     docs_science)

df_cor <- tibble(news = list(docs_news),
             politics = list(docs_politics),
             environment = docs_environment,
             life = docs_life,
             sport = docs_sport,
             culture = docs_culture,
             work = docs_work,
             technology = docs_technology,
             psychology = docs_psychology,
             science = docs_science)



