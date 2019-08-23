# The purpose of this script is to process the summary and title info from the
# MakeoverMonday Week 33 2019 Clinical Trial data.

# Key questions are:
#   1. What are the overarching topics across the 13K trials?
#   2. What are the most common topics for each of the 800+ conditions
#   3. What are the most common topics for each sponsor?
#   4. Which text field is more productive in the analysis - title or condition?

# The analysis plan:
#   1. Create df for each of the 3 text fields: condition, title, summary
#   2. unnest tokens, remove stop words
#   3. Exploration - count words in each df
#   4. Refine stop words to reduce df size
#   5. Explore Co-occurances & correlations using pairwise counts & correlations
#   6. Find characteristic words of summaries
#   7. Isolate top 15 description word associated with condition 
#   8. Data prep for LDA topic modeling of description with cast_dtm
#   9. Explore LDA topic models with range of k=8,16, 24
#   10. Explore gamma probablility of topics associated with summaries
#   11. Connect gammas with condition
#   12. Filter to keep only gammas > 0.9 to look at conditions per topic
#   13. Repeat steps 7, 10, 11, 12 using title instead of condition for analysis
#   14. Output xlsx files for use in Tableau Dashboard


# Created in R version 3.6.1 on August 12, 2019
# originally I wanted to do an embedded R in Tableau, but I would not be able
# to publish on Public. So, I will use R to create some tibbles and word counts

# The following code focuses on analysis of a 12 and 24 topic model built on summmary words and linked to title words
# Other analysis looked at 8 topic models as well as using the Condition name itself. That code is found at the end of the script

library(stringr)         #  to perform regex
library(tidyverse)       #  because all analysis will benefit
library(tidytext)        #  because text analysis requires unique tools
library(readxl)          #  to read xlsx
library(igraph)          #  to convert bigrams into network graphs (near line 74)
library(ggraph)         #  to plot graph from igraph object
library(tm.plugin.webmining) # to explore text mining from the web apps
library(purrr)               # to explore text mining from the web apps
library(topicmodels)         # to conduct topic modeling with LDA
library(openxlsx)            # to write xlsx
library(ggplot2)             # to look at topic modeling
library(widyr)               # to explore co-occurances & correlations
library(multcomp)            # to create Histograms in exploratory steps
library(RcmdrMisc)           # to create Histograms in exploratory steps

source <- read_xlsx("C:/Users/OConnor Analytics/Working Files/MakeoverMonday/2019/mm33/mm33.xlsx")     
data("stop_words")

# 1. Create a df for each of the 3 text fields using dplyr

nct_title <- source %>%
  dplyr::select(NCT, Title)
nct_sum  <- source %>%
  dplyr::select(NCT, Summary)
nct_con  <- source %>%
  dplyr::select(NCT, Condition)

# 2. Unnest tokens, remove stop words for each using tidytext

nct_title <- nct_title %>%
  unnest_tokens(word, Title) %>%
  anti_join(stop_words)
nct_sum  <- nct_sum %>%
  unnest_tokens(word, Summary) %>%
  anti_join(stop_words)
nct_con <- nct_con %>%
  unnest_tokens(word, Condition) %>%
  anti_join(stop_words)

# 3.Exploration - count words in each df, using dplyr

nct_title %>%
  count(word, sort=TRUE)
nct_sum %>%
  count(word, sort=TRUE) # some overlap at the top with title
nct_con %>%
  count(word, sort=TRUE) 

# 4. Refine stop words - especially digits. No need to apply to nct_con

numbers <- "(^[[:digit:]]+[[:punct:]]?)\\d*[[:alpha:]]*"
stop_numbers <- grep(numbers, nct_sum$word, value=TRUE)
stop_numbers_titles <- grep(numbers, nct_title$word, value=TRUE)
# This was a late solve - I probably could have done this more efficiently but have opted not cuz all other
# df's have been completed

my_stopwords <- tibble(word=c(as.character(0:999), as.character((1:500)/100), 
                                  "study", "safety", "patients", "double", "blind", "randomized", "dose",
                                  "subjects", "purpose", "doses", "participants", "evaluate", 
                                  "controlled", "efficacy", "label", "multicenter", "parallel", 
                                  "phase", "placebo", "treatment",
                                   stop_numbers, stop_numbers_titles, "mg", "ii", "iv", "iii", "i", "v" ))



nct_title <- nct_title %>%
  anti_join(my_stopwords)
nct_sum  <- nct_sum %>%
  anti_join(my_stopwords)

# 5. Explore co-occurances & corrrelations - but the network graphs don't display properly


title_word_pairs <- nct_title %>%
  pairwise_count(word, NCT, sort=TRUE, upper=FALSE)
sum_word_pairs   <- nct_sum %>%
  pairwise_count(word, NCT, sort=TRUE, upper=FALSE)
con_word_pairs   <- nct_con %>%
  pairwise_count(word, NCT, sort=TRUE, upper=FALSE)

set.seed(1234)
title_word_pairs %>%
  filter(n>=500) %>%    
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width=50), edge_colour = "darkred") +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

set.seed(12345)
sum_word_pairs %>%
  filter(n>=500) %>%    
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width=100), edge_colour = "cyan4") +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

set.seed(12345)
con_word_pairs %>%
  filter(n>=50) %>%    
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width=100), edge_colour = "royalblue") +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

sum_word_cors <- nct_sum %>%
  group_by(word) %>%
  filter(n()>=10) %>%
  pairwise_cor(word, NCT, sort=TRUE, upper=FALSE)
title_word_cors <- nct_title %>%
  group_by(word) %>%
  filter(n()>=10) %>%
  pairwise_cor(word, NCT, sort=TRUE, upper=FALSE)
con_word_cors <- nct_con %>%
  group_by(word) %>%
  filter(n()>=10) %>%
  pairwise_cor(word, NCT, sort=TRUE, upper=FALSE)

set.seed(12345)
sum_word_cors %>%                             # crazy chart! impossible to read below correlation .8 - but there were some clusters
  filter(correlation >.8) %>%    
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width=correlation), edge_colour = "cyan4") +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

set.seed(12345)
title_word_cors %>%                             # crazy chart! impossible to read below correlation =.8. Less obvious clusters
  filter(correlation >.8) %>%    
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width=correlation), edge_colour = "cyan4") +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

set.seed(12345)
con_word_cors %>%
  filter(correlation >.6) %>%    
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width=correlation), edge_colour = "royalblue") +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()



# 6. Find characteristic terms of summaries - tf-idf
#    This identifies term frequencies for each summary word by trial. 
#    TF is the term frequency, tf-idf indicates how important the term is for the summary
#    Terms with high tf often have a low tf-idf

sum_tf_idf <- nct_sum %>%
  count(NCT, word, sort=TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, NCT, n)

sum_tf_idf %>%
  arrange(-tf_idf) %>%
  dplyr::select(-NCT)  # tf looks good - although there are some NA's? 

#^^^^^^^^^^^^^^^^^^^^^^^^   At this point, the analysis will focus on TITLES  ^^^^^^^^^^^^^^^^^^^
#    ^^^^^^^^^^^^           to see the remaining explortions go to the end of this section


# 7. Connect summary fields to Title "keyword
#    This creates a matrix that indicates the term frequency for each word in each summary and
#    Links it to the the title "keyword". The title keyword comes from the nct_title data frame that
#    split each title into the significant words. Then, for every word in the title, it selects the
#    most descriminating & unique words from the 
sum_tf_idf_title <- full_join(sum_tf_idf, nct_title, by="NCT")

sum_tf_idf_title %>%                  # This extracts the top 15 summary words linked to each title word
  arrange(desc(tf_idf)) %>%
  group_by(word.y)      %>%
  top_n(15, tf_idf)     %>%
  ungroup()



# 8.Data prep for LDA topic modeling of description with cast_dtm

word_counts <- nct_sum %>%
  count(NCT, word, sort=TRUE) %>%
  ungroup()

sum_dtm <- word_counts %>%
  cast_dtm(NCT, word, n)


# 9. Create Topic Models
sum_lda_12 <- LDA(sum_dtm, k=12, control=list(seed = 1234)) # expect this to take 8 minutes
tidy_sum_lda_12 <- tidy(sum_lda_12)

sum_lda_24 <- LDA(sum_dtm, k=24, control=list(seed = 1234)) # expect this to run 10 minutes
tidy_sum_lda_24 <- tidy(sum_lda_24)





# What are the topics about? Extract top terms per topic - not a very helpful exercise, though

top_terms_sum_lda_12 <- tidy_sum_lda_12 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_sum_lda_24 <- tidy_sum_lda_24 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)



# Instead, plot the top terms by topic - much more helpful

top_terms_sum_lda_12 %>%
  mutate(term=reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "_"),
                       levels = rev(paste(term, topic, sep="_")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function (x) gsub("_.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic - 12 Topic Model",
       x = NULL, y = expression (beta)) +
  facet_wrap(~ topic, ncol = 3, scales = "free") 

# in the 12 topic solution, key themes are:
#    pharmokinetics, hiv, diabetes, vaccinations/influenza, renal,
#    cancer, relapsed, nasal/allergic, pain/symptoms
#    and the 3 remaining are overlapping study-type words.



top_terms_sum_lda_24 %>%
  mutate(term=reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "_"),
                       levels = rev(paste(term, topic, sep="_")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function (x) gsub("_.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic - 24 Topic Model",
       x = NULL, y = expression (beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free") 

# the 24 topic solution has an interesting amount of differentiation, with not much more overlap:
#  Lymphoma, women, basal, pharmokinetics, canceer, hepatitis, arthritis, schizophrenia, surgery
#  heart, asthma, breast cancer, influenza, vaccine, diabetes, tumor, infusion/antibody, intections, and 6 overlapping
# So, for the time needed and the differentiation lets stick with the 24 solution.



#   10. Explore gamma probablility of 12 & 24 topics associated with summaries. 
#       This will reveal which topics are associated with which NCT's

lda_gamma_12 <- tidy(sum_lda_12, matrix = "gamma")
lda_gamma_24 <- tidy(sum_lda_24, matrix = "gamma")

ggplot(lda_gamma_12, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribition of probablities for all 12 topics", 
       y = "Number of Clinical Trials", x = expression(gamma))  # the 12 topic version looks better than the 24


ggplot(lda_gamma_24, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribition of probablities for all 24 topics", 
       y = "Number of Clinical Trials", x = expression(gamma))

# the key points here are 1) many values near zero means many trials that DO NOT belong to each topic
# 2) Many values near 1 means these trials do belong to this topic

# plot by topic
ggplot(lda_gamma_12, aes(gamma, fill=as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol=4) +
  scale_y_log10() +
  labs(title = "Distribition of probablities for each topic (12)", 
       y = "Number of Clinical Trials", x = expression(gamma))


ggplot(lda_gamma_24, aes(gamma, fill=as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol=4) +
  scale_y_log10() +
  labs(title = "Distribition of probablities for each topic (24)", 
       y = "Number of Clinical Trials", x = expression(gamma))




#   11. Connect gammas from topic models with TITLE 
#       Can we find any relationships?

lda_gamma_12_title <- full_join(lda_gamma_12, nct_title, by = c("document" = "NCT"))
lda_gamma_24_title <- full_join(lda_gamma_24, nct_title, by = c("document" = "NCT"))

lda_gamma_12_title
lda_gamma_24_title


#   12. Filter to keep only gammas > 0.9 to look at titles per topic

top_titles_12 <- lda_gamma_12_title %>%
  filter(gamma > 0.9) %>%
  count(topic, word, sort=TRUE)

top_titles_24 <- lda_gamma_24_title %>%
  filter(gamma > 0.9) %>%
  count(topic, word, sort=TRUE)

#       Identify the top words for each topic - 
top_titles_12 %>%                                     # the 12 topic model seems to have 9 strong topics
  group_by(topic) %>%
  top_n(5, n) %>%
  group_by(topic, word) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(word = factor(paste(word, topic, sep="_"),
                       levels = rev(paste(word, topic, sep="_")))) %>%
  ggplot(aes(word, n, fill=as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top Title Words for each LDA topic (12)",
       x=NULL, y="Number of Clinical Trials") +
  coord_flip() +
  scale_x_discrete(labels = function (x) gsub("_.+$", "", x)) +
  facet_wrap(~ topic, ncol=3, scales = "free")


top_titles_24 %>%                        # Topic 11 disappears with the filtering, and most topics have low n's
  group_by(topic) %>%                 
  top_n(5, n) %>%
  group_by(topic, word) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(word = factor(paste(word, topic, sep="_"),
                       levels = rev(paste(word, topic, sep="_")))) %>%
  ggplot(aes(word, n, fill=as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top Title Words for each LDA topic (24)",
       x=NULL, y="Number of Clinical Trials") +
  coord_flip() +
  scale_x_discrete(labels = function (x) gsub("_.+$", "", x)) +
  facet_wrap(~ topic, ncol=3, scales = "free")


# The 12 topic solution using titles seems to be the most helpful. 



output   <- lda_gamma_12_title %>%
  filter(gamma > .9)

dest.file <- "C:/Users/OConnor Analytics/Working Files/MakeoverMonday/2019/mm33/gamma_LDA_12_titles.xlsx"
write.xlsx(output, dest.file)




























# @@@@@@@@@@@@@@                @@@@@@@@@@@@@@@@@      8 Topic LDA Code        @@@@@@@@@@@@    @@@@@@@@@@@@@

# 9. Create Topic Models: 8 topic code

sum_lda_8  <- LDA(sum_dtm, k=8,  control=list(seed = 1234)) #11:27 to 11:38
tidy_sum_lda_8  <- tidy(sum_lda_8)

# Extract top terms per topic
top_terms_sum_lda_8 <- tidy_sum_lda_8 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


# Examine what the topics are about
top_terms_sum_lda_8 %>%
  mutate(term=reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "_"),
                       levels = rev(paste(term, topic, sep="_")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function (x) gsub("_.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression (beta)) +
  facet_wrap(~ topic, ncol = 2, scales = "free") 

# in the 8 topic solution, it looks like descriptive titles could be:
#     vaccines for children, pain, diabetes, cancer, pharmokinetics, hepatitis, efficacy & placebos






# @@@@@@@@@@@@@@@@           @@@@@@@@@@@@@@@  CONDITION Centric Code @@@@@@@@@@@@            @@@@@@@@@@@@@@

# 7. Isolate top 15 tf_idf associated with each Title

sum_tf_idf_con <- full_join(sum_tf_idf, nct_con, by="NCT")
sum_tf_idf_con %>%
  arrange(desc(tf_idf)) %>%
  group_by(word.y)      %>%
  top_n(15, tf_idf)     %>%
  ungroup()

# 8.Data prep for LDA topic modeling of description with cast_dtm - same as above

# 9. Create Topic Models - same as above

#.  10. Explore gamma probablility of topics associated with summaries

#   11. Connect gammas from topic models with condition. 

lda_gamma_12_con <- full_join(lda_gamma_12, nct_con, by = c("document" = "NCT"))
lda_gamma_24_con <- full_join(lda_gamma_24, nct_con, by = c("document" = "NCT"))

lda_gamma_12_con
lda_gamma_24_con


#   12. Filter to keep only gammas > 0.9 to look at conditions per topic

top_cons_12 <- lda_gamma_12_con %>%
  filter(gamma > 0.9) %>%
  count(topic, word, sort=TRUE)

top_cons_24 <- lda_gamma_24_con %>%
  filter(gamma > 0.9) %>%
  count(topic, word, sort=TRUE)

#       Identify the top words for each topic - 
top_cons_12 %>%
  group_by(topic) %>%
  top_n(5, n) %>%
  group_by(topic, word) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(word = factor(paste(word, topic, sep="_"),
                       levels = rev(paste(word, topic, sep="_")))) %>%
  ggplot(aes(word, n, fill=as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top Condition Words for each LDA topic (12)",
       x=NULL, y="Number of Clinical Trials") +
  coord_flip() +
  scale_x_discrete(labels = function (x) gsub("_.+$", "", x)) +
  facet_wrap(~ topic, ncol=3, scales = "free")


top_cons_24 %>%
  group_by(topic) %>%
  top_n(5, n) %>%
  group_by(topic, word) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(word = factor(paste(word, topic, sep="_"),
                       levels = rev(paste(word, topic, sep="_")))) %>%
  ggplot(aes(word, n, fill=as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top Condition Words for each LDA topic (24)",
       x=NULL, y="Number of Clinical Trials") +
  coord_flip() +
  scale_x_discrete(labels = function (x) gsub("_.+$", "", x)) +
  facet_wrap(~ topic, ncol=3, scales = "free")

###         ############           ###########         ############ 


#+++++++++++                  ++++++++++++++++++     ++++++++++++++++++ +++++++++++++++++++++++++         

# As a final step, the link between the summaries and title/topics might be self-fulfilling prophesies in a way
# because they are likely to include the terms.


# This may help us understand how the data sets may be connected to each other overall, and
# Within specific conditions. It maybe helpful in generating an efficient list of key words to search for overall
# - like - contains(word). Let's see.



# What if we removed the conditions terms as stop words, and then looked at the resutls?
# 1. Create a df for each of the 3 text fields using dplyr - from above
# 2. Unnest tokens, remove stop words for each using tidytext - from above
# 3.Exploration - count words in each df, using dplyr - from above
# 4. Refine stop words - especially digits. Use an additional set of stop words - the conditions

condition_stopwords <- data.frame(word=(nct_con$word))

nct_title_wo_cons <- nct_title %>%
  anti_join(my_stopwords) %>%
  anti_join(condition_stopwords)

nct_sum_wo_cons <- nct_sum %>%
  anti_join(my_stopwords) %>%
  anti_join(condition_stopwords)

# 5. Explore co-occurances & corrrelations - but the network graphs don't display properly - did not do here
# 6. Find characteristic terms of summaries - tf-idf - did not do here
# 7. Isolate top 15 tf_idf associated with each condition - did not do here


# 8.Data prep for LDA topic modeling of description with cast_dtm

word_counts_wo_cons <- nct_sum_wo_cons %>%
  count(NCT, word, sort=TRUE) %>%
  ungroup()

sum_dtm_wo_cons <- word_counts_wo_cons %>%
  cast_dtm(NCT, word, n)

# 9. Create Topic Models
sum_lda_12_wo_cons <- LDA(sum_dtm_wo_cons, k=12, control=list(seed = 1234)) #6:18 to


tidy_sum_lda_12_wo_cons  <- tidy(sum_lda_12_wo_cons)


# Extract top terms per topic
top_terms_sum_lda_12_wo_cons <- tidy_sum_lda_12_wo_cons %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


# Examine what the topics are about

top_terms_sum_lda_12_wo_cons %>%
  mutate(term=reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "_"),
                       levels = rev(paste(term, topic, sep="_")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function (x) gsub("_.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic - 12 Topic Model",
       x = NULL, y = expression (beta)) +
  facet_wrap(~ topic, ncol = 3, scales = "free") 

# in the 12 topic solution:
#    chemotherapy, pharmokinetics, cp, hcv, vaccine, hba 1c, 
#    and the 6 remaining are overlapping study-type words.

#   10. Explore gamma probablility of 12  topics associated with summaries. 
#       This will reveal which topics are associated with which NCT's

lda_gamma_12_wo_cons <- tidy(sum_lda_12_wo_cons, matrix = "gamma")

ggplot(lda_gamma_12_wo_cons, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribition of probablities for all 12 topics", 
       y = "Number of Clinical Trials", x = expression(gamma))  # the 12 topic version looks better than the 24



# the key points here are 1) many values near zero means many trials that DO NOT belong to each trial
# 2) Many values near 1 means these trials do belong to this topic

# plot by topic
ggplot(lda_gamma_12_wo_cons, aes(gamma, fill=as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol=4) +
  scale_y_log10() +
  labs(title = "Distribition of probablities for each topic (12)", 
       y = "Number of Clinical Trials", x = expression(gamma))




#   11. Connect gammas from topic models with TITLE 
#       Can we find any relationships?

lda_gamma_12_title_wo_cons <- full_join(lda_gamma_12_wo_cons, nct_title_wo_cons, by = c("document" = "NCT"))

lda_gamma_12_title_wo_cons


#   12. Filter to keep only gammas > 0.9 to look at titles per topic

top_titles_12_wo_cons <- lda_gamma_12_title_wo_cons %>%
  filter(gamma > 0.9) %>%
  count(topic, word, sort=TRUE)


#       Identify the top words for each topic - 
top_titles_12_wo_cons %>%
  group_by(topic) %>%
  top_n(5, n) %>%
  group_by(topic, word) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(word = factor(paste(word, topic, sep="_"),
                       levels = rev(paste(word, topic, sep="_")))) %>%
  ggplot(aes(word, n, fill=as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top Summary Words for each LDA topic (12)",
       x=NULL, y="Number of Clinical Trials") +
  coord_flip() +
  scale_x_discrete(labels = function (x) gsub("_.+$", "", x)) +
  facet_wrap(~ topic, ncol=3, scales = "free")


# I think the 12 with the conditions maintained is better than this version w/o conditions

output2   <- lda_gamma_12_title_wo_cons %>%
  filter(gamma > .9)

dest.file2 <- "C:/Users/OConnor Analytics/Working Files/MakeoverMonday/2019/mm33/gamma_LDA_12_titles_wo_cons.xlsx"
write.xlsx(output2, dest.file2)











