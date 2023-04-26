#' @title check_auth
#' @description helper function, validates that key passed into newAuth with the Twitter API
#' @import httr
#' @importFrom jsonlite fromJSON
#' @param auth type string to be validated with the twitter api
#' @return returns boolean, False if the bearer token is not real. True if the bearer token is real
#' @export
check_auth <- function(auth) {
  # header with apporiabte bearer token
  headers <- c('Authorization' = sprintf('Bearer %s', auth))
  # test url to access the api
  url_handle <- sprintf('https://api.twitter.com/2/users/id/tweets')
  # api request
  response <- httr::GET(url = url_handle,
                        httr::add_headers(.headers = headers))
  # format the api reponse
  obj <- httr::content(response, as = "text")
  json_data <- jsonlite::fromJSON(obj, flatten = T)
  result <- as.data.frame(json_data)

  # check the error from the api
  if ('status' %in% colnames(result) == TRUE) {
    # return false if the api returns 401 - error associated with incompatible bearer token
    if (result$status == '401') {
      return(F)
    }
  } else {
    return(T)
  }
}

#' @title new_auth
#' @description creates an S4 object of type authKey
#' @import methods
#' @param auth_key argument of type character, supposed to be bearer token compatible with the twitter api
#' @return object of class authKey
#' @export
newAuth <- function(auth_key) {
  # checks the integrity of the argument
  if (typeof(auth_key) != 'character') {
    stop("Argument is not of type key.")
  }
  if (check_auth(auth_key) == F) {
    stop("Not valid bearer token.")
  }
  # set a class of authkey
  setClass("authKey", representation(key = "character"))
  # return a new instance of authKey
  return(new("authKey", key = auth_key))
}

#' @title get_key
#' @description returns the bearer key of an object of auth_key, pays homage to the get functions in other OOP langauges
#' @param key object of class auth_key
#' @return function returns the bearer token of the auth_key
#' @export
get_key <- function(key) {
  # checks the integrity of the argument
  if (class(key) != "authKey") {
    stop("Argument needs to be of class authKey")
  }
  # returns the key value of the authKey class
  return(key@key)
}
