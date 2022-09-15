#--------- start script ---------#

# script to build co-author network

# load required packages

require(tidyverse)
require(tidyr)
require(tidygraph)
require(ggraph)
require(ggforce)

# load Scopus search results

load("~/onedrive - csiro/projects/mars_analogue/data/scopus_search.RData")

# build node list

lone_authors <- authors %>%
  group_by(scopus_id) %>%
  count() %>%
  filter(n < 2) %>%
  select(scopus_id) %>%
  ungroup()

node_list <- author_list %>%
  left_join(author_attributes) %>%
  left_join(affiliation_info) %>%
  left_join(h_index) %>%
  # fix duplicate author_ids
  group_by(author, affiliation_id) %>%
  mutate(publications = sum(publications),
         citations = sum(citations)) %>%
  ungroup() %>%
  distinct(author, affiliation_id, .keep_all = T) %>%
  drop_na()

# build edge list

edge_list <- authors %>%
  inner_join(node_list) %>%
  distinct(scopus_id, author_id) %>%
  anti_join(lone_authors) %>%
  group_by(scopus_id) %>%
  expand(from = author_id, to = author_id) %>% 
  filter(from < to) %>%
  ungroup() 

# calculate collaboration strength (count collaborations)

collaboration_strength <- edge_list %>%
  group_by(from, to) %>%
  count()
  
weighted_edge_list <- edge_list %>%
  left_join(collaboration_strength) %>%
  distinct(from, to, n) 

node_subset <- weighted_edge_list %>%
  pivot_longer(c(from, to), values_to = "author_id") %>%
  distinct(author_id) %>%
  inner_join(author_attributes) %>%
  inner_join(affiliation_info) %>%
  inner_join(h_index)

# create network

net <- tbl_graph(edges = weighted_edge_list, nodes = node_subset, directed = F) %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(),
         betweenness = centrality_betweenness(),
         reach = local_size(mindist = 1),
         group = group_components())
  
node_data <- as_tibble(net %>% activate(nodes)) %>%
  inner_join(scopus_id_authors) %>%
  distinct(author_id, .keep_all = T)

# plot graph

k1 <- ggraph(net %>% filter(group == 1), layout = "stress") + 
  geom_edge_link(aes(width = n), color = "grey") +
  scale_edge_width(range = c(0.5, 2), breaks = c(seq(1, 10, 2)), name = "No. of collaborations") +
  scale_size(range = c(2,10)) +
  geom_node_point(aes(size = betweenness, color = country, alpha = h_index)) +
  geom_node_text(aes(label = author), size = 3, nudge_y = 0.15) +
  # geom_mark_hull(aes(x, y, group = group, label = group), concavity = 30, expand = unit(5, "mm"), alpha = 0.01) +
  
  scale_fill_continuous(guide = "none") +
  labs(color = "Country", alpha = "h-index", size = "Betweenness centrality") +
  guides(size = guide_legend(order = 4), colour = guide_legend(order = 1), alpha = guide_legend(order = 2), width = guide_legend(order = 3)) + 
  
  
  theme_graph() 

ggsave("~/onedrive - csiro/projects/mars_analogue/plots/cluster_1.pdf", k1, width = 294, height = 210, units = "mm", device = cairo_pdf)

k2 <- ggraph(net %>% filter(group == 2), layout = "stress") + 
  geom_edge_link(aes(width = n), color = "grey") +
  scale_edge_width(range = c(0.5, 2), breaks = c(seq(1, 10, 2)), name = "No. of collaborations") +
  scale_size(range = c(2,10)) +
  geom_node_point(aes(size = betweenness, color = country, alpha = h_index)) +
  geom_node_text(aes(label = author), size = 3, nudge_y = 0.15) +
  # geom_mark_hull(aes(x, y, group = group, label = group), concavity = 30, expand = unit(5, "mm"), alpha = 0.01) +
  
  scale_fill_continuous(guide = "none") +
  labs(color = "Country", alpha = "h-index", size = "Betweenness centrality") +
  guides(size = guide_legend(order = 4), colour = guide_legend(order = 1), alpha = guide_legend(order = 2), width = guide_legend(order = 3)) + 
  
  theme_graph() 

ggsave("~/onedrive - csiro/projects/mars_analogue/plots/cluster_2.pdf", k2, width = 40, height = 30, units = "cm", device = cairo_pdf)

k3 <- ggraph(net %>% filter(group == 3), layout = "stress") + 
  geom_edge_link(aes(width = n), color = "grey") +
  scale_edge_width(range = c(0.5, 2), breaks = c(seq(1, 10, 2)), name = "No. of collaborations") +
  scale_size(range = c(2,10)) +
  geom_node_point(aes(size = betweenness, color = country, alpha = h_index)) +
  geom_node_text(aes(label = author), size = 3, nudge_y = 0.15) +
  # geom_mark_hull(aes(x, y, group = group, label = group), concavity = 30, expand = unit(5, "mm"), alpha = 0.01) +
  
  scale_fill_continuous(guide = "none") +
  labs(color = "Country", alpha = "h-index", size = "Betweenness centrality") +
  guides(size = guide_legend(order = 4), colour = guide_legend(order = 1), alpha = guide_legend(order = 2), width = guide_legend(order = 3)) + 
  
  theme_graph() 

ggsave("~/onedrive - csiro/projects/mars_analogue/plots/cluster_3.pdf", k3, width = 40, height = 30, units = "cm", device = cairo_pdf)

k4 <- ggraph(net %>% filter(group == 4), layout = "stress") + 
  geom_edge_link(aes(width = n), color = "grey") +
  scale_edge_width(range = c(0.5, 2), breaks = c(seq(1, 10, 2)), name = "No. of collaborations") +
  scale_size(range = c(2,10)) +
  geom_node_point(aes(size = betweenness, color = country, alpha = h_index)) +
  geom_node_text(aes(label = author), size = 3, nudge_y = 0.1) +
  # geom_mark_hull(aes(x, y, group = group, label = group), concavity = 30, expand = unit(5, "mm"), alpha = 0.01) +
  
  scale_fill_continuous(guide = "none") +
  labs(color = "Country", alpha = "h-index", size = "Betweenness centrality") +
  guides(size = guide_legend(order = 4), colour = guide_legend(order = 1), alpha = guide_legend(order = 2), width = guide_legend(order = 3)) + 
  
  theme_graph() 

ggsave("~/onedrive - csiro/projects/mars_analogue/plots/cluster_4.pdf", k4, width = 40, height = 30, units = "cm", device = cairo_pdf)

k5 <- ggraph(net %>% filter(group == 5), layout = "stress") + 
  geom_edge_link(aes(width = n), color = "grey") +
  scale_edge_width(range = c(0.5, 2), breaks = c(seq(1, 10, 2)), name = "No. of collaborations") +
  scale_size(range = c(2,10)) +
  geom_node_point(aes(size = betweenness, color = country, alpha = h_index)) +
  geom_node_text(aes(label = author), size = 3, nudge_y = 0.1) +
  # geom_mark_hull(aes(x, y, group = group, label = group), concavity = 30, expand = unit(5, "mm"), alpha = 0.01) +
  
  scale_fill_continuous(guide = "none") +
  labs(color = "Country", alpha = "h-index", size = "Betweenness centrality") +
  guides(size = guide_legend(order = 4), colour = guide_legend(order = 1), alpha = guide_legend(order = 2), width = guide_legend(order = 3)) + 
  
  theme_graph() 

ggsave("~/onedrive - csiro/projects/mars_analogue/plots/cluster_5.pdf", k5, width = 40, height = 30, units = "cm", device = cairo_pdf)


save(node_list,
     edge_list,
     collaboration_strength,
     weighted_edge_list,
     node_subset,
     net,
     node_data,
     file = "~/onedrive - csiro/projects/mars_analogue/data/network_data.RData")

#--------- end script ---------#