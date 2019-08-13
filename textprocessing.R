# The purpose of this script is to process the summary and title info from the
# MakeoverMonday Week 33 2019 Clinical Trial data.
# It is created in R version 3.6.1 on August 12, 2019
# originally I wanted to do an embedded R in Tableau, but I would not be able
# to publish on Public. So, I will use R to create some tibbles and word counts



library(Rserve);Rserve() #  installed when the intent was to do this in R
library(tidyverse)       #  because all analysis will benefit
library(tidytext)        #  because text analysis requires unique tools
library(readxl)          #  to read xlsx
library(igraph)          #  to convert bigrams into network graphs (near line 74)
library(ggraph)         #  to plot graph from igraph object
library(tm.plugin.webmining) # to explore text mining from the web apps
library(purrr)               # to explore text mining from the web apps
library(topicmodels)         # to conduct topic modeling with LDA
library(openxlsx)            # to write xlsx

source <- read_xlsx("mm33.xlsx")     
data("stop_words")

# This will count the words used in each summary for each condition
df1 <- as_tibble(source) %>%
  unnest_tokens(word, Summary)%>%
  anti_join(stop_words) %>% 
  count(Condition, NCT, word, sort=TRUE) %>%
  ungroup()

# This looks at ALL of the words combined used in the condition summaries
df2 <- df1 %>%
  group_by(NCT) %>%
  summarize(total = sum(n))


# Now we create term frequency for each condition

df3 <- left_join(df1, df2)

# Create a list of most common terms in df3 - to identify a custom stop list

df3.lex <- df3 %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count > 3926)

# Eliminate these top 7 words to cut down the size of the df3

df4 <- df3 %>%
  anti_join(df3.lex)

# Zipf's Law: the frequency that a word appears is proportional to its rank
# Find the important words for the document context by decreasing the weight
# of common words and increasing the weight of uncommon words.

df5 <- df4 %>% bind_tf_idf(word, NCT, n)

# Resort

df6 <- df5 %>% 
  select(-total) %>%
  arrange(desc(tf_idf)) # this could be used to create a word cloud of top 20 words per condition

# Lets keep the top 30 words for each condition

df6_top10 <- df5 %>%
  group_by(NCT) %>%
  top_n(10, tf_idf)

dest.file       <- "top10.xlsx"

write.xlsx(df6_top10, dest.file)

# This is good for this project. Simple enough to explain and make the point

#??????????????????????????????????????????????????????????????????????????????

# But maybe, if we did the analysis of Titles instead of summaries, what would the 
# results look like?

df1_1 <- as_tibble(source) %>%       
  unnest_tokens(word, Title)%>%
  anti_join(stop_words) %>% 
  count(Condition, NCT, word, sort=TRUE) %>%
  ungroup()                           # a lot fewer words than the summary exercise

df2_1 <- df1_1 %>%
  group_by(NCT) %>%
  summarize(total = sum(n))

df3_1 <- left_join(df1_1, df2_1)

# I am not going to remove additional words since I think the title is already lean.

df5_1 <- df3_1 %>% bind_tf_idf(word, NCT, n)
df6_1 <- df5_1 %>% 
  select(-total) %>%
  arrange(desc(tf_idf))

# The max tf_idf is only 3.5. So, titles are NOT more discriminating than the summaries!

##XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Let's explore bigrams


df1_bi <- as_tibble(source) %>%
  unnest_tokens(bigram, Summary, token = "ngrams", n=2)

df1_bi %>% count(bigram, sort=TRUE)

df2_bi <- df1_bi %>%          # separate the bigrams
  separate(bigram, c("word1", "word2"), sep=" ")

df3_bi <-  df2_bi %>%         # filter out stop words
 filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% df3.lex$word)    %>%
  filter(!word2 %in% df3.lex$word)


df4_bi <- df3_bi %>%          # new bigram counts
  count(word1, word2, sort=TRUE)

df5_bi <- df3_bi %>%          # rejoins the bigrams without the stop words
  unite(bigram, word1, word2, sep=" ")

df6_bi <- df5_bi %>%          # create a tf_idf that can turn into a network graph
  count(NCT, bigram) %>%
  bind_tf_idf(bigram, NCT, n) %>%
  arrange(desc(tf_idf))

# Create a df of nodes & weights to generate a network graph of bigrams

df7_bi <- df4_bi %>%
  filter(n>100) %>%
  graph_from_data_frame()

set.seed(2017)
ggraph(df7_bi, layout="fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), vjust=1, hjust=1) # this does not visualize wellat n>20

# The network graph is interesting but too much for this exercise

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Lets investigate a topic modeling approach 

df7 <- df6 %>%
  cast_dtm(NCT, word, n)
set.seed(2019)
LDA_1 <- LDA(df7, k=10, control=list(seed=2019))
LDA_1_tops <- tidy(LDA_1, matrix = "beta")


# This took a loooong time. Since there are really 861 "topics" I don't think this is
# a reasonable approach.
