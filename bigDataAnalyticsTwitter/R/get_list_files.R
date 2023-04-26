library(tidyverse)

#' @title get_list_files
#' @description reads through a path and reads all of the files and creates
#' a complied data frame from the files
#' @import tidyverse
#' @importFrom plyr rbind.fill
#' @param path a string to a local directory to read
#' @return a complied data frame from the read files
#' @export
get_list_files <- function(path) {
  # save current working directory to be changed back later
  current_wd <- getwd()
  # sets the temporary working directory to the path inputed as a parameter
  setwd(path)
  # gets the list of file names
  myFiles <- list.files()
  compiled_df <- data.frame()
  # loops through the available files within the directory
  for (file in myFiles) {
    # ensures file is a csv to read
    if (grepl(".csv", file, ignore.case = T)) {
      temp <- read_csv(file)
      compiled_df <- plyr::rbind.fill(compiled_df, temp)
    }
  }
  # revert the working director back to the current
  setwd(current_wd)
  return(compiled_df)
}


