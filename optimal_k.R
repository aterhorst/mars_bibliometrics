#' Find Optimal Number of Topics
#' 
#' Iteratively produces models and then compares the harmonic mean of the log 
#' likelihoods in a graphical output.
#' 
#' @param x A \code{\link[tm]{DocumentTermMatrix}}.
#' @param max.k Maximum number of topics to fit (start small [i.e., default of 
#' 30] and add as necessary).
#' @param harmonic.mean Logical.  If \code{TRUE} the harmonic means of the 
#' log likelihoods are used to determine k (see 
#' \url{http://stackoverflow.com/a/21394092/1000343}).  Otherwise just the log 
#' likelihoods are graphed against k (see 
#' \url{http://stats.stackexchange.com/a/25128/7482}). 
#' @param burnin Object of class \code{"integer"}; number of omitted Gibbs 
#' iterations at beginning, by default equals 0.
#' @param iter Object of class \code{"integer"}; number of Gibbs iterations, by 
#' default equals 2000.
#' @param keep Object of class \code{"integer"}; if a positive integer, the 
#' log likelihood is saved every keep iterations.
#' @param method The method to be used for fitting; currently 
#' \code{method = "VEM"} or \code{method= "Gibbs"} are supported.
#' @param \ldots Other arguments passed to \code{??LDAcontrol}.
#' @return Returns the \code{\link[base]{data.frame}} of k (nuber of topics) and 
#' the associated log likelihood.
#' @references \url{http://stackoverflow.com/a/21394092/1000343} \cr
#' \url{http://stats.stackexchange.com/a/25128/7482} \cr
#' Ponweiser, M. (2012). Latent Dirichlet Allocation in R (Diploma Thesis). 
#' Vienna University of Economics and Business, Vienna. 
#' http://epub.wu.ac.at/3558/1/main.pdf \cr\cr
#' Griffiths, T.L., and Steyvers, M. (2004). Finding scientific topics. 
#' Proceedings of the National Academy of Sciences of the United States of America 
#' 101(Suppl 1), 5228 - 5235. \url{http://www.pnas.org/content/101/suppl_1/5228.full.pdf}
#' @keywords k topicmodel 
#' @export
#' @author Ben Marwick and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @examples
#' ## Install/Load Tools & Data
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load_gh("trinker/gofastr")
#' pacman::p_load(tm, topicmodels, dplyr, tidyr,  devtools, LDAvis, ggplot2)
#' 
#' 
#' ## Source topicmodels2LDAvis function
#' devtools::source_url("https://gist.githubusercontent.com/trinker/477d7ae65ff6ca73cace/raw/79dbc9d64b17c3c8befde2436fdeb8ec2124b07b/topicmodels2LDAvis")
#' 
#' data(presidential_debates_2012)
#' 
#' 
#' ## Generate Stopwords 
#' stops <- c(
#'         tm::stopwords("english"),
#'         "governor", "president", "mister", "obama","romney"
#'     ) %>%
#'     gofastr::prep_stopwords() 
#' 
#' 
#' ## Create the DocumentTermMatrix
#' doc_term_mat <- presidential_debates_2012 %>%
#'     with(gofastr::q_dtm_stem(dialogue, paste(person, time, sep = "_"))) %>%           
#'     gofastr::remove_stopwords(stops) %>%                                                    
#'     gofastr::filter_tf_idf() %>%
#'     gofastr::filter_documents() 
#' 
#' 
#' opti_k1 <- optimal_k(doc_term_mat)
#' opti_k1
#'
#' opti_k2 <- optimal_k(doc_term_mat, harmonic.mean = FALSE)
#' opti_k2
optimal_k <- function(x, max.k = 80, harmonic.mean = TRUE, burnin = 1000, iter = 1000, keep = 50, method = "Gibbs", verbose = TRUE, ...){
  
  if (isTRUE(harmonic.mean)) {
    optimal_k1(x, max.k = max.k, burnin = burnin, iter = iter, keep = keep, method = method, verbose = verbose, ...)
  } else {
    optimal_k2(x, max.k = max.k, ...)
  }
}

#' Plots a plot.optimal_k1 Object
#' 
#' Plots a plot.optimal_k1 object
#' 
#' @param x A \code{optimal_k1} object.
#' @param \ldots Ignored.
#' @method plot plot.optimal_k1
#' @export 
plot.optimal_k1 <- function(x, ...){
  
  y <- attributes(x)[["k_dataframe"]]
  y <- y[y[["k"]] == as.numeric(x), ]
  
  ggplot2::ggplot(attributes(x)[["k_dataframe"]], ggplot2::aes_string(x="k", y="harmonic_mean")) + 
    ggplot2::xlab("Number of Topics") + 
    ggplot2::ylab("Harmonic Mean of Log Likelihood") + 
    geom_point(data=y, color="blue", fill=NA,  size = 6, shape = 21) +
    ggplot2::geom_line(size=1) + 
    ggplot2::theme_bw()  + 
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(vjust = -0.25, size = 14),
      axis.title.y = ggplot2::element_text(size = 14, angle=90)
    )
}

#' Prints a optimal_k Object
#' 
#' Prints a optimal_k object
#' 
#' @param x A \code{optimal_k} object.
#' @param \ldots Ignored.
#' @method print optimal_k
#' @export 
print.optimal_k <- function(x, ...){
  
  print(graphics::plot(x))
  
}




optimal_k1 <- function(x, max.k = 80, burnin = 1000, iter = 1000, keep = 50, method = "Gibbs", verbose = TRUE, ...){
  
  
  if (max.k > 20) {
    message("\nGrab a cup of coffee this is gonna take a while...\n")
    flush.console()
  }
  
  tic <- Sys.time()
  
  hm_many <- sapply(2:max.k, function(k){
    if (k %% 10 == 0){
      elapsed <- gsub("^0+", "", as.character(round(as.numeric(difftime(Sys.time(), tic, units = "mins")), 1)))
      cat(sprintf("%s of %s iterations (Time elapsed: %s mins)\n", k, max.k, elapsed)); flush.console()
    }
    fitted <- topicmodels::LDA(x, k = k, method = method, control = list(burnin = burnin, iter = iter, keep = keep, ...))
    logLiks <- fitted@logLiks[-c(1:(burnin/keep))]
    harmonicMean(logLiks)
  })
  
  out <- c(2:max.k)[which.max(hm_many)]
  
  class(out) <- c("optimal_k", "optimal_k1", class(out))
  attributes(out)[["k_dataframe"]] <- data.frame(
    k = 2:max.k, 
    harmonic_mean = hm_many
  )
  if (isTRUE(verbose)) cat(sprintf("Optimal number of topics = %s\n",as.numeric(out)))
  out
}

harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- Rmpfr::median(logLikelihoods)
  as.double(llMed - log(Rmpfr::mean(exp(-Rmpfr::mpfr(logLikelihoods, prec = precision) + llMed))))
}

optimal_k2 <- function(x, max.k = 30, burnin = 1000, iter = 1000, keep = 50, method = "Gibbs", ...){
  
  if (max.k > 20) {
    message("\nGrab a cup of coffee this is gonna take a while...\n")
    flush.console()
  }
  
  tic <- Sys.time()
  
  best_model <- lapply(seq(2, max.k, by=1), function(k){
    if (k %% 10 == 0){
      elapsed <- gsub("^0+", "", as.character(round(as.numeric(difftime(Sys.time(), tic, units = "mins")), 1)))
      cat(sprintf("%s of %s iterations (Time elapsed: %s mins)\n", k, max.k, elapsed)); flush.console()
    }
    topicmodels::LDA(x, k = k, method = method, control = list(burnin = burnin, iter = iter, keep = keep, ...))
  })
  
  out <- data.frame(
    k = c(2:max.k), 
    logLik = sapply(best_model, logLik)
  )
  
  class(out) <- c("optimal_k", "optimal_k2", "data.frame")
  out
}

#' Plots a plot.optimal_k2 Object
#' 
#' Plots a plot.optimal_k2 object
#' 
#' @param x A \code{optimal_k2} object.
#' @param \ldots Ignored.
#' @method plot plot.optimal_k2
#' @export 
plot.optimal_k2 <- function(x, ...){
  
  ggplot2::ggplot(x, ggplot2::aes_string(x="k", y="logLik")) + 
    ggplot2::xlab("Number of Topics") + 
    ggplot2::ylab("Log Likelihood") + 
    ggplot2::geom_smooth(size=.8, se=FALSE) + 
    ggplot2::geom_line(size=1) + 
    ggplot2::theme_bw()  + 
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(vjust = -0.25, size = 14),
      axis.title.y = ggplot2::element_text(size = 14, angle=90)
    )
  
}

if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(ggplot2, topicmodels, Rmpfr)