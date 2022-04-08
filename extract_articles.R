#--------- start script ---------#

# script to extract relevant documents

# load required packages

require(tidyverse)
require(tidyr)
require(fulltext)

# retrieve full text

setwd("~/onedrive - csiro/projects/mars_analogue/pdf")

cache_options_set(full_path = "~/onedrive - csiro/projects/mars_analogue/pdf")

for(i in 1:nrow(scopus_results)){
  skip_to_next <- FALSE
  tryCatch({
    message('\r', i, '/', nrow(scopus_results), appendLF = FALSE)
    ft_get(scopus_results$doi[i], type = "pdf")}, error = function(e) { skip_to_next <<- TRUE})
  if(skip_to_next) { next }
}

#--------- end script ---------#
