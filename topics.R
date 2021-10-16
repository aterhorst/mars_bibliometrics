#--------- start script ---------#

# Perform topic modelling on abstracts


# load the tidyverse

require(tidyverse)
require(tidyr)
require(readxl)

# load NLP packages

require(tidytext)
require(pluralize)
require(topicmodels)

load("~/owncloud/terrestrial analogues/data/scopus_search.RData")



# retrieve search data

scopus_csv_1 <- read.csv("~/owncloud/terrestrial analogues/data/scopus_20210630_1.csv", check.names = F) %>%
  # remove missing author data and irrelevant articles
  filter(str_detect(`Author(s) ID`, "[No author id available]", negate = T),
         str_detect(Authors, "Mars", negate = T)) %>%
  # rename field names so these make sense
  rename(scopus_id = EID) %>%
  # ditch superfluous information
  mutate(scopus_id = str_remove(scopus_id, "2-s2.0-"),
         Abstract = str_remove(Abstract, "©.*$")) 

scopus_csv_2 <- read.csv("~/owncloud/terrestrial analogues/data/scopus_20210630_2.csv", check.names = F) %>%
  # remove missing author data and irrelevant articles
  filter(str_detect(`Author(s) ID`, "[No author id available]", negate = T),
         str_detect(Authors, "Mars", negate = T)) %>%
         #str_detect(tolower(Abstract), paste(filter_terms, collapse = "|"), negate = T)) %>%
  # rename field names so these make sense
  rename(scopus_id = EID) %>%
  # ditch superfluous information
  mutate(scopus_id = str_remove(scopus_id, "2-s2.0-"),
         Abstract = str_remove(Abstract, "©.*$")) 

scopus_csv_3 <- read.csv("~/owncloud/terrestrial analogues/data/scopus_20210630_3.csv", check.names = F) %>%
  # remove missing author data and irrelevant articles
  filter(str_detect(`Author(s) ID`, "[No author id available]", negate = T),
         str_detect(Authors, "Mars", negate = T)) %>%
  # rename field names so these make sense
  rename(scopus_id = EID) %>%
  # ditch superfluous information
  mutate(scopus_id = str_remove(scopus_id, "2-s2.0-"),
         Abstract = str_remove(Abstract, "©.*$")) 


scopus_csv <- bind_rows(scopus_csv_1 %>% select(Title, Abstract), 
                        scopus_csv_2  %>% select(Title, Abstract),
                        scopus_csv_3  %>% select(Title, Abstract)) %>%
  # retrieve correct scopus ids from scopus_search.R
  inner_join(scopus_valid_subjects %>% select(scopus_id, title), by = c("Title" = "title")) %>%
  distinct(scopus_id, .keep_all = T) 

# define custom stop words (search terms)

search_terms <- data.frame(word = c("mars", "martian"))

# tokenise abstracts

unigram <- scopus_csv %>%
  # tokenise abstract
  unnest_tokens(word, Abstract) %>%
  # remove stop words
  anti_join(stop_words) %>%
  # remove search terms as these will be high frequency terms
  anti_join(search_terms) %>%
  # drop rows with NAs
  drop_na() %>%
  # remove numbers
  filter(!str_detect(word, "[0-9]")) %>%
  rowwise() %>%
  mutate(word = SemNetCleaner::singularize(str_squish(word)))

# create document term matrix for topic modelling

unigram_dtm <- unigram %>%
  count(scopus_id, word) %>%
  cast_dtm(scopus_id, word, n)

#  optimal k

## lda tuning

how_many_topics <- ldatuning::FindTopicsNumber(unigram_dtm,
                                               topics = c(seq(2, 20, 2), seq(25, 50, 5), seq(60, 90, 10)),
                                               metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                               method = "Gibbs",
                                               control = list(seed = 1234),
                                               mc.cores = 6L,
                                               verbose = F)

ldatuning::FindTopicsNumber_plot(how_many_topics)

## optimal k using stm

require(stm)

corpus_stm <- readCorpus(unigram_dtm, type = "slam" )

set.seed(1234)

stm_search <- searchK(documents = corpus_stm$documents,
                      vocab = corpus_stm$vocab,
                      K = seq(20,80,10),
                      cores = 4,
                      init.type = "Spectral",
                      data = corpus_stm$meta)

plot(stm_search)

optimum_k_stm <- as.numeric(stm_search$results$K[which.min(stm_search$results$residual)])

## optimal k using harmonic mean of log likelihood

source("https://raw.githubusercontent.com/aterhorst/mars_bibliometrics/main/optimal_k.R")

opti_k1 <- optimal_k(unigram_dtm)
opti_k1

optimum_k_rinker <- as.numeric(str_extract(opti_k1, "[0-9][0-9]"))



# run topic model

model <- topicmodels::LDA(unigram_dtm, 
                          method = "Gibbs", 
                          k = 40,
                          control = list(seed = 1234, best = T))

# explore topic model (beta, gamma values)

corpus_beta <- tidy(model, matrix = "beta")
corpus_gamma <- tidy(model, matrix = "gamma", document_names = rownames(corpus_dtm))


doc_top_topic <- corpus_gamma %>%
  group_by(document) %>%
  slice_max(order_by = gamma, n = 1) %>%
  rename(scopus_id = document) %>%
  inner_join(scopus_csv %>% select(scopus_id, title = Title, abstract = Abstract)) %>%
  drop_na()

top_terms <- corpus_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 10) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(cols = "terms")

write.csv(top_terms, "~/owncloud/terrestrial analogues/data/top_terms.csv", row.names = F)

filter_topic <- sort( c(1,
                        3,
                        5,
                        6,
                        9,
                        10,
                        14,
                        18,
                        21,
                        25,
                        27,
                        31,
                        32,
                        34,
                        36,
                        40))

filter_articles <- c("84903188312",
                     "85074414526",
                     "85012835632",
                     "84871454997",
                     "84947485514",
                     "84883499231",
                     "84867828609",
                     "84879761530",
                     "79957973551",
                     "84930383055",
                     "82755189382",
                     "80051503319",
                     "79959420366",
                     "85085834782",
                     "85083943769",
                     "85043473725",
                     "84956530103",
                     "84878689304",
                     "85082894257",
                     "85077277251",
                     "84991738784",
                     "85050791137",
                     "84991454191",
                     "85088353384",
                     "85091858618",
                     "84864412343",
                     "85006792163",
                     "85047292315",
                     "85078866348",
                     "85091858618",
                     "85092248305",
                     "85100055135",
                     "85074724181",
                     "85070241012")

filter_top_topic <- doc_top_topic %>%
  filter(topic %in% filter_topic, !scopus_id %in% filter_articles) %>%
  distinct(scopus_id) 
  
# save results

save(scopus_csv,
     unigram, 
     unigram_dtm, 
     model, 
     corpus_beta, 
     corpus_gamma, 
     doc_top_topic, 
     top_terms,
     filter_topic,
     filter_top_topic,
     file = "~/owncloud/terrestrial analogues/data/topic_model_results.RData")

#--------- end script ---------#
