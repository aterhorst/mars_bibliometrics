#--------- start script ---------#

# Search Scopus for articles about martian analogues

# load the tidyverse

require(tidyverse)
require(tidyr)

# perform search

require(rscopus)
require(readxl)

set_api_key(Sys.getenv("ELSEVIER_KEY"))
set_api_key(Sys.getenv("SCOPUS_KEY"))
have_api_key()

# perform search

# general search 

search_1 <- "TITLE-ABS-KEY ( ( mars  OR  martian )  W/10  ( radiation  OR  psycho*  OR  perchlorate  OR  medic*  
OR  surg*  OR  dental OR nutrition) )  AND  PUBYEAR  >  2009  AND NOT  ( sediment*  OR  geolog*  OR  astrobiology  OR  extremophile )  
AND  ( LIMIT-TO ( LANGUAGE ,  \"English\" ) )" 

search_2 <- "TITLE-ABS-KEY ( ( mars  OR  martian )  W/10  ( \"haughton mars project\"  
OR  neemo  OR  \"nasa extreme environment mission operation\"  OR  \"desert mars analog ramon station\"  
OR  d-mars  OR \"hawaii space exploration analog and simulation\"  OR  hi-seas  OR  amadee  
OR  \"polares program\"  OR  \"mars desert research station\"  OR mdrs OR  \"flashline mars arctic research station\"  
OR  \"concordia station\"  OR  mars-500  OR  \"arctic mars analog svalbard expedition\"  OR  amase  
OR  \"lunares research station\"  OR  \"cooperative adventure for valuing and exercising human behaviour and performance skills\"  
OR  \"human exploration research analog\"  OR  hera  OR  \"human exploration spacecraft testbed for integration and advancement\"  
OR  hestia))  AND  PUBYEAR  >  2009  AND NOT  ( sediment*  OR  geolog*  OR  astrobiology  OR  extremophile )  
AND ( LIMIT-TO ( LANGUAGE ,  \"English\" ) )" 


search_3 <- "TITLE-ABS-KEY ( ( mars  OR  martian )  W/10  ( base  OR  settlement  OR  colony  OR  analog*  OR  habitat ) )  
AND  PUBYEAR  >  2009  AND NOT  ( geolog*  OR  sediment*  OR  astrobiology  OR  biolog*  OR  extremophile )  
AND  ( LIMIT-TO ( LANGUAGE ,  \"English\" ) )"



basic_search_1 <- scopus_search(
  verbose = T,
  query = search_1,
  view = "COMPLETE",
  count = 25,
  max_count = 3000,
  wait_time = 0.5,
  http = "https://api.elsevier.com/content/search/scopus")

basic_search_2 <- scopus_search(
  verbose = T,
  query = search_2,
  view = "COMPLETE",
  count = 25,
  max_count = 3000,
  wait_time = 0.5,
  http = "https://api.elsevier.com/content/search/scopus")

basic_search_3 <- scopus_search(
  verbose = T,
  query = search_3,
  view = "COMPLETE",
  count = 25,
  max_count = 3000,
  wait_time = 0.5,
  http = "https://api.elsevier.com/content/search/scopus")

# translate search results into a data frame

# filter terms to eliminate irrelevant articles (dynamic and growing list of terms)

# filter_terms <- c("multivariate adaptive regression spline",
#                   "urinary",
#                   "urology",
#                   "patients",
#                   "spawning",
#                   "liver failure",
#                   "fish",
#                   "krevelen",
#                   "sexual",
#                   "land\\-use",
#                   "cytometry",
#                   "patients",
#                   "bats",
#                   "coral",
#                   "sumset",
#                   "shorebird",
#                   "seabird",
#                   "logistic modeling",
#                   "larval",
#                   "crab",
#                   "methionyl",
#                   "covid-19",
#                   "supernova",
#                   "photon\\-counting",
#                   "epilepsy",
#                   "sexual violence",
#                   "coronavirus",
#                   "alcohol",
#                   "maryland assessment",
#                   "anterior",
#                   "lunar",
#                   "moon",
#                   "covid",
#                   "blockchain",
#                   "hi-vec",
#                   "titan",
#                   "mesosphere",
#                   "teacher",
#                   "school",
#                   "education",
#                   "recommender",
#                   "helios",
#                   "students",
#                   "dogs",
#                   "magic",
#                   "ascent",
#                   "descent",
#                   "mobile autonomous reconfigurable system",
#                   "medication adherence report scale",
#                   "preschool",
#                   "spatial downscaling",
#                   "nuclear war",
#                   "scotland",
#                   "caregiver",
#                   "hepatic neoplasia",
#                   "medicare\\-allowable reimbursements",
#                   "protein",
#                   "maternal",
#                   "analog circuit",
#                   "apps")
# 
# save(filter_terms, file = "~/owncloud/terrestrial analogues/data/filter_terms.RData")


scopus_results_1 <- gen_entries_to_df(basic_search_1[["entries"]])$df %>%
  select(scopus_id = `dc:identifier`, 
         title = `dc:title`,
         creator = `dc:creator`,
         keywords = authkeywords,
         publication_name = `prism:publicationName`, 
         volume = `prism:volume`, 
         issue = `prism:issueIdentifier`,
         pages = `prism:pageRange`,
         doi = `prism:doi`,
         abstract = `dc:description`,
         cited_by_count = `citedby-count`,
         date = `prism:coverDate`, 
         type = `subtypeDescription`,
         funding_source = `fund-sponsor`) %>%
  mutate(scopus_id = str_squish(str_remove(scopus_id, "SCOPUS_ID:"))) 

scopus_results_2 <- gen_entries_to_df(basic_search_2[["entries"]])$df %>%
  select(scopus_id = `dc:identifier`, 
         title = `dc:title`,
         creator = `dc:creator`,
         keywords = authkeywords,
         publication_name = `prism:publicationName`, 
         volume = `prism:volume`, 
         issue = `prism:issueIdentifier`,
         pages = `prism:pageRange`,
         doi = `prism:doi`,
         abstract = `dc:description`,
         cited_by_count = `citedby-count`,
         date = `prism:coverDate`, 
         type = `subtypeDescription`,
         funding_source = `fund-sponsor`) %>%
  mutate(scopus_id = str_squish(str_remove(scopus_id, "SCOPUS_ID:"))) 

scopus_results_3 <- gen_entries_to_df(basic_search_3[["entries"]])$df %>%
  select(scopus_id = `dc:identifier`, 
         title = `dc:title`,
         creator = `dc:creator`,
         keywords = authkeywords,
         publication_name = `prism:publicationName`, 
         volume = `prism:volume`, 
         issue = `prism:issueIdentifier`,
         pages = `prism:pageRange`,
         doi = `prism:doi`,
         abstract = `dc:description`,
         cited_by_count = `citedby-count`,
         date = `prism:coverDate`, 
         type = `subtypeDescription`,
         funding_source = `fund-sponsor`) %>%
  mutate(scopus_id = str_squish(str_remove(scopus_id, "SCOPUS_ID:"))) 

# combine search results

scopus_combined <- bind_rows(scopus_results_1, scopus_results_2, scopus_results_3) %>%
  distinct(scopus_id, .keep_all = T)
  
# retrieve subject area info from Scopus 

subjects = data.frame()

for(i in 1:nrow(scopus_combined)){
  skip_to_next <- FALSE
  tryCatch({
    message('\r', i, '/', nrow(scopus_combined), appendLF = FALSE)
    # retrieve abstract for each article
    q <- abstract_retrieval(scopus_combined$scopus_id[i], identifier = "scopus_id", verbose = F)
    # extract number of subjects
    nsubjects <- length(q[["content"]][["abstracts-retrieval-response"]][["subject-areas"]][["subject-area"]])
    
    for(j in 1:nsubjects){
      subject_df <- data.frame(scopus_id = scopus_combined$scopus_id[i], 
                               subject = q[["content"]][["abstracts-retrieval-response"]][["subject-areas"]][["subject-area"]][[j]][["$"]],
                               code = q[["content"]][["abstracts-retrieval-response"]][["subject-areas"]][["subject-area"]][[j]][["@code"]])
      subjects <- bind_rows(subjects,subject_df)}}, error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
}

# generalise subject areas

asjc_subject_codes <- read_xlsx("~/onedrive - csiro/projects/mars_analogue/data/asjc.xlsx", sheet = 5, range = "A9:B370") %>%
  rename(code = Code, subject = Description) %>%
  mutate(code = as.character(code),
         subject_group = case_when(str_detect(code, "1000") ~ "Multidisciplinary",
                                   str_detect(code, "11[0-9][0-9]") ~ "Agricultural and Biological Sciences",
                                   str_detect(code, "12[0-9][0-9]") ~ "Arts and Humanities",
                                   str_detect(code, "13[0-9][0-9]") ~ "Biochemistry, Genetics and Molecular Biology",
                                   str_detect(code, "14[0-9][0-9]") ~ "Business, Management and Accounting",
                                   str_detect(code, "15[0-9][0-9]") ~ "Chemical Engineering",
                                   str_detect(code, "16[0-9][0-9]") ~ "Chemistry",
                                   str_detect(code, "17[0-9][0-9]") ~ "Computer Science",
                                   str_detect(code, "18[0-9][0-9]") ~ "Decision Sciences",
                                   str_detect(code, "19[0-9][0-9]") ~ "Earth and Planetary Sciences",
                                   str_detect(code, "20[0-9][0-9]") ~ "Economics, Econometrics and Finance",
                                   str_detect(code, "21[0-9][0-9]") ~ "Energy",
                                   str_detect(code, "22[0-9][0-9]") ~ "Engineering",
                                   str_detect(code, "23[0-9][0-9]") ~ "Environmental Science",
                                   str_detect(code, "24[0-9][0-9]") ~ "Immunology and Microbiology",
                                   str_detect(code, "25[0-9][0-9]") ~ "Materials Science",
                                   str_detect(code, "26[0-9][0-9]") ~ "Mathematics",
                                   str_detect(code, "27[0-9][0-9]") ~ "Medicine",
                                   str_detect(code, "28[0-9][0-9]") ~ "Neuroscience",
                                   str_detect(code, "29[0-9][0-9]") ~ "Nursing",
                                   str_detect(code, "30[0-9][0-9]") ~ "Pharmacology, Toxicology and Pharmaceutics",
                                   str_detect(code, "31[0-9][0-9]") ~ "Physics and Astronomy",
                                   str_detect(code, "32[0-9][0-9]") ~ "Psychology",
                                   str_detect(code, "33[0-9][0-9]") ~ "Social Sciences",
                                   str_detect(code, "34[0-9][0-9]") ~ "Veterinary",
                                   str_detect(code, "35[0-9][0-9]") ~ "Veterinary",
                                   str_detect(code, "36[0-9][0-9]") ~ "Health Professions",
                                   TRUE ~ subject)) %>%
  drop_na()

subject_codes <- subjects %>%
  left_join(asjc_subject_codes %>% select(-subject), by = "code")


# remove invalid subjects

filter_subjects <- read_csv("~/onedrive - csiro/projects/mars_analogue/data/filter_subjects.csv")

scopus_invalid_subjects <- scopus_combined %>%
  inner_join(subject_codes) %>%
  left_join(filter_subjects %>% mutate(keep = T)) %>%
  filter(is.na(keep)) %>%
  distinct(scopus_id)

scopus_valid_subjects <- scopus_combined %>%
  anti_join(scopus_invalid_subjects)
  
# filter invalid topics, remove uncited papers

scopus_results <- scopus_valid_subjects %>%
  inner_join(filter_top_topic) %>%
  mutate(cited_by_count = as.integer(cited_by_count))

# extract journal impact factor

titles <- scopus_results %>%
  filter(type == "Article" | type == "Review" | type == "Conference Paper") %>%
  distinct(publication_name) 

jstats <- data.frame()

for(i in 1:nrow(titles)){
  skip_to_next <- FALSE
  tryCatch({
    message('\r', i, '/', nrow(titles), appendLF = FALSE)
    q <- generic_elsevier_api(title = titles$publication_name[i], 
                     type = c("serial"), 
                     search_type = "scopus",
                     content_type = "content",
                     verbose = F)
  entries <- length(q[["content"]][["serial-metadata-response"]][["entry"]])
 
  for(j in 1:entries){
    df <- data.frame(publication_name = q[["content"]][["serial-metadata-response"]][["entry"]][[j]][["dc:title"]],
                     snip = q[["content"]][["serial-metadata-response"]][["entry"]][[1]][["SNIPList"]][["SNIP"]][[j]][["$"]],
                     sjr = q[["content"]][["serial-metadata-response"]][["entry"]][[1]][["SJRList"]][["SJR"]][[j]][["$"]])
    jstats <- bind_rows(jstats, df)}}, error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
}

articles_with_snip <- scopus_results %>%
  left_join(jstats) 



# get co-authors

authors = data.frame()

for(i in 1:nrow(scopus_results)){
  skip_to_next <- FALSE
  tryCatch({
    message('\r', i, '/', nrow(scopus_results), appendLF = FALSE)
    # retrieve abstract for each article
    q <- abstract_retrieval(scopus_results$scopus_id[i], identifier = "scopus", verbose = F) 
    # extract number of coauthors
    nauthors <- length(q[["content"]][["abstracts-retrieval-response"]][["authors"]][["author"]])
    
    for(j in 1:nauthors){
    coauthor <- data.frame(scopus_id = scopus_results$scopus_id[i], 
                           author = q[["content"]][["abstracts-retrieval-response"]][["authors"]][["author"]][[j]][["ce:indexed-name"]], 
                           author_id = q[["content"]][["abstracts-retrieval-response"]][["authors"]][["author"]][[j]][["@auid"]], 
                           stringsAsFactors = F)
    authors <- bind_rows(authors,coauthor)}}, error = function(e) { skip_to_next <<- TRUE })
  
  if(skip_to_next) { next }
}

# generate list of unique author ids

author_list <- authors %>%
  #inner_join(filter_top_topic) %>%
  distinct(author_id)

# retrieve author info from Scopus 

author_attributes <- data.frame() 

for(i in 1:nrow(author_list)) {
  skip_to_next <- FALSE
  tryCatch({
    message('\r', i, '/', nrow(author_list), appendLF = FALSE)
    # query API
    q <- author_retrieval_id(author_list$author_id[i], identifier = "author_id", verbose = F)
    # extract relevant info and store in data frame
    df <- data.frame(author_id = author_list$author_id[i],
                      author = q[["content"]][["author-retrieval-response"]][[1]][["author-profile"]][["preferred-name"]][["indexed-name"]],
                      citations = as.integer(q[["content"]][["author-retrieval-response"]][[1]][["coredata"]][["citation-count"]]),
                      publications = as.integer(q[["content"]][["author-retrieval-response"]][[1]][["coredata"]][["document-count"]]),
                      affiliation_id =q[["content"]][["author-retrieval-response"]][[1]][["affiliation-current"]][["@id"]])
    # update data frame
    author_attributes <- bind_rows(author_attributes, df)}, error = function(e) { skip_to_next <<- TRUE })
  if(skip_to_next) { next }
}

# retrieve affiliation info from Scopus

affiliation_list <- author_attributes %>%
  distinct(affiliation_id) %>%
  mutate_all(na_if,"") %>%
  drop_na() 

affiliation_info <- data.frame()

for(i in 1:nrow(affiliation_list)){
  skip_to_next <- FALSE
  tryCatch({
    message('\r', i, '/', nrow(affiliation_list), appendLF = FALSE)
    # query API
    q <- affiliation_retrieval(affiliation_list$affiliation_id[i], identifier = "affiliation_id", verbose = F)
    # extract relevant info and store in data frame
    df <- data.frame(affiliation_id = affiliation_list$affiliation_id[i],
                      affiliation = q[["content"]][["affiliation-retrieval-response"]][["affiliation-name"]],
                      country = q[["content"]][["affiliation-retrieval-response"]][["country"]])
    affiliation_info <- bind_rows(affiliation_info, df)}, error = function(e) { skip_to_next <<- TRUE })
  if(skip_to_next) { next }
}

# compute h-index

h_index <- data.frame() 

for(i in 1:nrow(author_list)){
  skip_to_next <- FALSE
  tryCatch({
    message('\r', i, '/', nrow(author_list), appendLF = FALSE)
    q <- author_data(au_id = author_list$author_id[i], verbose = F)
    h_data <- q$df %>%
      select(`citedby-count`) %>%
      mutate(`citedby-count` = as.numeric(`citedby-count`)) %>%
      arrange(desc(`citedby-count`)) %>%
      summarise(h_index = sum(`citedby-count` >= seq_along(`citedby-count`))) %>%
      mutate(author_id = author_list$author_id[i])
    h_index <- bind_rows(h_index, h_data)}, error = function(e) { skip_to_next <<- TRUE })
  if(skip_to_next) { next }
}

# save results

save(scopus_results, 
     articles_with_snip,
     scopus_combined,
     scopus_filtered,
     authors,
     author_list,
     subjects,
     subject_codes,
     author_attributes,
     affiliation_info,
     h_index,
     file = "~/onedrive - csiro/projects/mars_analogue/data/scopus_search.RData")

#--------- end script ---------#
