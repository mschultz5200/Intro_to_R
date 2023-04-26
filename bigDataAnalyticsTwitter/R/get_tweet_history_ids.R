#' @title start_pagination
#' @description helper function, collects all of the tweets from the twitter api that are stored in separate pages
#' @import httr
#' @importFrom jsonlite fromJSON flatten
#' @importFrom plyr rbind.fill
#' @param headers contains the bearer token to access the twitter api
#' @param id the id of the twitter account an individual wants to collect from
#' @return a combined data frame of the user's twitter history
#' @export
start_pagination <- function(headers, id) {
  # pagination is the separation of data within a api database
  # return df is the complied dataframe that will be returned
  return_df <- data.frame()
  # next_page will save the next page that is returned from the data frame
  next_page <- ''
  #nextPage is a boolean value that will update to false when all of the available tweets are returned
  nextPage <- TRUE
  index <- 1

  while (nextPage) {
    print(index)
    url_handle <- sprintf('https://api.twitter.com/2/users/%s/tweets', id)
    if (next_page == '') {
      # list of parameters for the first page to be returned
      params <- list(
        `max_results`="100",
        `expansions` = 'author_id',
        `user.fields` = "id,username,name,description,public_metrics",
        `tweet.fields` = 'public_metrics,created_at,entities,referenced_tweets'
        )
    } else {
      # list of parameters to be sent to the api if the the page is greater than one
      params <- list(
        `max_results`="100",
        `expansions` = 'author_id',
        `user.fields` = "id,username,name,description,public_metrics",
        `tweet.fields` = 'public_metrics,created_at,entities,referenced_tweets',
        `pagination_token` = next_page
      )
    }

    # call the twitter api
    response <- httr::GET(url = url_handle,
                          httr::add_headers(.headers = headers),
                          query = params)
    # helps to format the returned  JSON
    obj <- httr::content(response, as = "text")
    json_data <- jsonlite::fromJSON(obj, flatten = T)
    timeline_df <- jsonlite::flatten(as.data.frame(json_data), recursive = F)

    # sets the next page
    next_page <- timeline_df[1, 'meta.next_token']
    index = index + 1
    return_df <- plyr::rbind.fill(return_df, timeline_df)

     # checks if the most recent value for next_page is NULL and will end the function
    if (is.null(next_page)) {
      nextPage = F
    }
  }
  return(return_df)
}

#' @title get_tweet_history_ids
#' @description get the history of account handles in a vector
#' @importFrom plyr rbind.fill
#' @param handles a character vector of all accounts a user wants to collect
#' @param auth_key an object of the auth_key class, will be used as the bearer token
#' @return the complete data frame of all account's timeline
get_tweet_history_ids <- function(handles, auth_key) {
  # checks to see if the parameters fit the function
  if (!is.vector(handles, mode = "character") & !is.list(handles)) {
    stop()
  }
  if (class(auth_key) != "authKey") {
    stop()
  }

  # data frame to be returned
  return_df <- data.frame()

  # removes scientific notation from the twitter ids
  options(scipen = 999)

  # establishes the headers to be passed into the api call
  headers <- c('Authorization' = sprintf('Bearer %s', get_key(auth_key)))

  # loop through all available handles and compile into a single data frame
  for (i in 1:length(handles)) {
    print(paste0(i,'/',length(handles),' iterations'))
    temp <- start_pagination(headers, handles[i])
    return_df <- plyr::rbind.fill(return_df, temp)
  }
  return(return_df)
}
