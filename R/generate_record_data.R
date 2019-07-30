


#' sample_zips_by_pop
#'
#'A function for generating records from the built in data included in this package.
#'This function generates \code{s} zipcodes, such that the zipcodes are chosen with frequency that
#'represents the total population of that zipcode
#'
#' @param s The number of samples to produce
#'
#' @return (s)x2 dataframe containing zipcode and town samples.
#' @author Sam Murray<slmurray@andrew.cmu.edu>
#' @import tidyverse
#' @import dplyr
#' @import purrr
#' @import stringr
#' @importFrom readr read_csv
#' @export
sample_zips_by_pop <- function(s, include_town = FALSE){
  pop$zip <- as.numeric(pop$zip)
  zips$City <- as.character(zips$City)

  pop <- pop[pop$zip %in% zips$Zipcode,]
  total = sum(pop$population)
  pop = cbind(pop, c_pop = cumsum(pop$population))
  samples = runif(s, min = 0,max = total) %>%
    map_int(~which.max(pop$c_pop >= .x))

  sample_zips = pop$zip[samples]
  sample_towns = map_chr(sample_zips, ~zips$City[which.max(zips$Zipcode == .x)])
  if(include_town){
    result <- tibble(zipcode = sample_zips, town = sample_towns)
  }else{
    result <- sample_zips
  }

  return(result)
}



sample_names_uniform <- function(s) {
  print("in names")
  names <- names$name %>%
    str_to_upper() %>%
    sample(s, replace = TRUE)
  names = map_chr(names, ~as.character(.x))
  return(result)
}

#' generate_record_data
#'
#'A function that generates random data to use for testing record linkage packages.
#'The generated data contains name, birthyear, zipcode, town, sex, and id.
#'
#' @param s The number of samples to generate
#'
#' @return A (s)x6 dataframe containing record information
#' @export
#'
#' @import tidyverse
#' @import dplyr
#' @import purrr
#' @importFrom readr read_csv
#' @import stringr
generate_record_data <- function(s, ...){
  arg_list <- list(...)
  df = map(names(arg_list), ~arg_list[[.x]](s))
  names(df) <- names(arg_list)
  return(as.data.frame(df))
}
