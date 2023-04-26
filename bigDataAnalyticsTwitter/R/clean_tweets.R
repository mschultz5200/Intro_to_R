library(tidyverse)
library(tidytext)
library(readr)
library(stringr)
library(text2vec)
library(tm)
library(qdap)
library(qdapRegex)
library(stopwords)
library(openNLPdata)
library(SnowballC)

#' @title make.source
#' @description checks to see if the argument is of class VSource
#' @param input either vector or VCorpus
#' @return a vector casted to a Volatile Corpus for additional cleaning
#' @return input is returned if input is already a dataframe
#' @export
make_source <- function(input) {
  if (class(input) != "VCorpus") {
    return(VCorpus(VectorSource(input)))
  } else {
    return(input)
  }
}

#' @title clean_tweets
#' @description Provides the users to customize how to clean the twitter data
#' @import tidyverse
#' @import tidytext
#' @import readr
#' @import text2vec
#' @importFrom tm tm_map content_transformer VectorSource VCorpus removeWords removeNumbers removePunctuation
#' @import qdap
#' @import qdapRegex
#' @import stopwords
#' @import openNLPdata
#' @import SnowballC
#' @import stringr
#' @import methods
#' @param tweets a character vector of tweets from a data frame or normal vector
#' @param options either a numeric vector or string "all"
#' - 'all' runs all of the cleaning functions
#' - numeric vector with range of 1 to 6. 1 remove links. 2 removes new line tabs.
#' 3 removes numbers. 4 removes punctuation. 5 removes stop words.
#' 6 puts the string to lower case.
#' @return a data frame of cleaned tweets
#' @export
clean_tweets <- function(tweets, options = "all") { # "all" is set to the default value of the script
  # test cases to ensure function integrity
  if (!is.vector(tweets, mode = "character")) stop("tweets not character vector")

  if (!is.vector(options, mode = "numeric")) {
    if (options != "all") stop()
  }

  # start of script
  returned <- tweets

  if (is.vector(options, mode = "numeric")) {
    # Modularity of the function
    # remove links
    if (1 %in% options) {
      returned = str_replace_all(returned, "https://t.co/[A-Za-z\\d]+|https://[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp|@[A-Za-z\\d]+|&amp",'')
    }
    # remove new line tabs
    if  (2 %in% options) {
      returned <- str_replace_all(returned, '[\r\n]' , '')
    }
    # remove numbers
    if (3 %in% options) {
      returned <- make_source(returned)
      returned <- tm_map(returned, removeNumbers)
    }
    # remove punctuation
    if (4 %in% options) {
      returned <- make_source(returned)
      returned <- tm_map(returned, removePunctuation)
    }
    # remove stop words
    if (5 %in% options) {
      returned <- make_source(returned)
      returned <- tm_map(returned, removeWords, words = c("https", "t.co", "rt", stopwords('en')))
    }
    # make tweets lower case
    if (6 %in% options) {
      returned <- make_source(returned)
      returned <- tm_map(returned, content_transformer(tolower))
    }
    if ("VCorpus" %in% class(returned)) {
      returned <- data.frame(text=unlist(sapply(returned, `[`, "content")), stringsAsFactors=F)
      rownames(returned) <- 1:nrow(returned)
      return(returned)
    } else {
      return(returned)
    }
  } else {
    # this is if options equal "all"
    returned <- str_replace_all(returned, "https://t.co/[A-Za-z\\d]+|https://[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp|@[A-Za-z\\d]+|&amp",'')
    returned <- str_replace_all(returned, '[\r\n]' , '')
    returned <- VCorpus(VectorSource(returned))

    returned <- tm_map(returned, removePunctuation) %>%
      tm_map(removeWords, words = c("https", "t.co", "rt", stopwords('en'))) %>%
      tm_map(removeNumbers) %>%
      tm_map(content_transformer(tolower))

    returned <- data.frame(text=unlist(sapply(returned, `[`, "content")), stringsAsFactors=F)
    rownames(returned) <- 1:nrow(returned)

    return(returned)
  }
}




