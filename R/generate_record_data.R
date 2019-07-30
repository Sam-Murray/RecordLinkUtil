


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
sample_zips_by_pop <- function(s){
  zips
  pop
  pop$zip <- as.numeric(pop$zip)
  zips$City <- as.character(zips$City)

  pop <- pop[pop$zip %in% zips$Zipcode,]
  total = sum(pop$population)
  pop = cbind(pop, c_pop = cumsum(pop$population))
  samples = runif(s, min = 0,max = total) %>%
    map_int(~which.max(pop$c_pop >= .x))

  sample_zips = pop$zip[samples]
  sample_towns = map_chr(sample_zips, ~zips$City[which.max(zips$Zipcode == .x)])
  result <- tibble(zipcode = sample_zips, town = sample_towns)
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
generate_record_data <- function(s){

  zips <- sample_zips_by_pop(s)
  names <- names$name %>%
    str_to_upper() %>%
    sample(s, replace = TRUE)
  names = map_chr(names, ~as.character(.x))
  print(names)
  birthyear <-  1940:2010 %>%
    sample(s, replace = TRUE)
  sex <-  c("M","F") %>%
    sample(s, replace = TRUE)
  df = cbind(id = 1:s, zips,names,"birthyear" = birthyear, "sex" = sex)
  df = mutate(df, names = as.character(names), town = as.character(town))
  df
}
