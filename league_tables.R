#--------- start script ---------#

# script to create league tables

# load required packages

require(tidyverse)
require(tidyr)
require(xtable)
library(RColorBrewer)
require(ggsci)


load("~/onedrive - csiro/projects/mars_analogue/data/topic_model_results.RData")


# most cited manuscripts

print(xtable(scopus_results %>%
               
               select(title, date, publication_name, volume, issue, cited_by_count) %>%
               mutate(date = as.integer(lubridate::year(date)),
                      title = str_to_sentence(title)) %>%
               
               arrange(desc(cited_by_count)) %>%
               slice(1:20) %>%
               rename(Title = title, Year = date, Source = publication_name, Volume = volume, Issue = issue, Citations = cited_by_count)), 
      type = "latex", 
      latex.environments = NULL, 
      booktabs = TRUE, 
      include.rownames = FALSE, 
      file = "~/onedrive - csiro/projects/mars_analogue/data/most_cited_manuscripts.tex")

# most cited authors

print(xtable(node_data %>%
               select(Author = author, Affiliation = affiliation, Country = country, `h-index` = h_index) %>%
               mutate(`h-index` = as.integer(`h-index`)) %>%
               arrange(desc(`h-index`)) %>%
               slice(1:20)), 
      type = "latex", 
      latex.environments = NULL, 
      booktabs = TRUE, 
      include.rownames = FALSE, 
      file = "~/onedrive - csiro/projects/mars_analogue/data/most_cited_authors.tex")

# most productive authors

print(xtable(node_data %>%
               select(Author = author, Affiliation = affiliation, Country = country, Publications = publications, h_index) %>%
               mutate(Publications = as.integer(Publications)) %>%
               arrange(desc(Publications)) %>%
               slice(1:20)), 
      type = "latex", 
      latex.environments = NULL, 
      booktabs = TRUE, 
      include.rownames = FALSE, 
      file = "~/onedrive - csiro/projects/mars_analogue/data/most_productive_authors.tex")

# top connectors

print(xtable(node_data %>%
               select(Author = author, Affiliation = affiliation, Country = country, Betweenness = betweenness, Reach = reach, h_index) %>%
               mutate(Betweenness = as.integer(Betweenness), Reach = as.integer(Reach)) %>%
               arrange(desc(Betweenness, h_index, Reach)) %>%
               slice(1:20)), 
      type = "latex", 
      latex.environments = NULL, 
      booktabs = TRUE, 
      include.rownames = FALSE, 
      file = "~/onedrive - csiro/projects/mars_analogue/data/top_connectors.tex")

# most productive research groups

print(xtable(node_data %>%
               group_by(affiliation_id, affiliation, country) %>%
               count() %>%
               ungroup() %>%
               group_by(affiliation, country) %>%
               mutate(n = sum(n)) %>%
               drop_na() %>%
               ungroup() %>%
               distinct(affiliation, country, .keep_all = T) %>%
               select(`Research group` = affiliation, Country = country, Articles = n) %>%
               slice_max(order_by = Articles, n = 20)),
      type = "latex", 
      latex.environments = NULL, 
      booktabs = TRUE, 
      include.rownames = FALSE, 
      file = "~/onedrive - csiro/projects/mars_analogue/data/most_productive_groups.tex")


# top subject areas

mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(subject_codes %>% distinct(subject_group) %>% count() %>% pull())

donut <- subject_codes %>%
  group_by(subject_group) %>%
  count() %>%
  ungroup() %>%
  mutate(fraction = round(n/sum(n), 2),
         ypos = cumsum(fraction) - 0.5 * fraction) %>%
  filter(fraction > 0.001) %>%
ggplot(aes(x = 2, y = fraction, fill = subject_group)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 80) +
  geom_text(aes(x = 2.6, label = scales::percent(fraction, accuracy = 1L)), size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = mycolors, name = "Subject") +
  theme_void() +
  xlim(0.8, 2.7)

ggsave("~/onedrive - csiro/projects/mars_analogue/plots/subject_breakdown.png", donut, width = 20, height = 12, units = "cm")
  

# top funding agencies

print(xtable(scopus_results %>%
               
               select(funding_source) %>%
               drop_na() %>%
               mutate(funding_source = ifelse(funding_source == "Mars", "Mars Society", funding_source)) %>%
               group_by(funding_source) %>%
               count() %>%
               ungroup() %>%
               slice_max(order_by = n, n = 10)),
      type = "latex", 
      latex.environments = NULL, 
      booktabs = TRUE, 
      include.rownames = FALSE, 
      file = "~/onedrive - csiro/projects/mars_analogue/data/top_funders.tex")


# top topics

print(xtable(corpus_gamma %>%
               group_by(topic) %>%
               summarise(gamma = mean(gamma)) %>%
               arrange(desc(gamma)) %>%
               left_join(top_terms, by = "topic") %>%
               mutate(topic = reorder(topic, gamma)),
             digits = 4),
      type = "latex", 
      latex.environments = NULL, 
      booktabs = TRUE, 
      include.rownames = FALSE, 
      file = "~/onedrive - csiro/projects/mars_analogue/data/top_topics.tex")

write_csv(corpus_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = reorder(topic, gamma)), "~/onedrive - csiro/projects/mars_analogue/data/top_topics.csv")

# proportion of multinational papers

author_affil <- author_attributes %>%
  left_join(affiliation_info)

p <- authors %>%
  select(scopus_id, author_id) %>%
  left_join(author_affil) %>%
  distinct(scopus_id, country) %>%
  group_by(scopus_id) %>%
  mutate(mix = n()) %>%
  group_by(country) %>%
  summarise(single_country = sum(mix == 1), multi_country = sum(mix > 1), combined = single_country + multi_country) %>%
  pivot_longer(c(single_country, multi_country), values_to = "papers") %>%
  drop_na() %>%
  slice_max(order_by = combined, n = 40) %>%
  ggplot(aes(x = reorder(country, combined),  y = papers, fill = name)) +
  geom_bar(stat = "identity", colour = "white") +
  coord_flip() +
  scale_x_discrete("top 20 countries") +
  scale_y_continuous("number of articles") +
  
  labs(fill = "") +
  theme(legend.position = "bottom") +
  scale_fill_lancet(guide = guide_legend(reverse = TRUE),
                 labels = c("articles with authors from multiple countries","articles with authors from same country"))
  #scale_fill_discrete(guide = guide_legend(reverse = TRUE),
  #                    labels = c("articles with authors from multiple countries","articles with authors from same country"))


ggsave("~/onedrive - csiro/projects/mars_analogue/plots/Fig_6.pdf", p, width = 18, height = 12, units = "cm")

#--------- end script ---------#
