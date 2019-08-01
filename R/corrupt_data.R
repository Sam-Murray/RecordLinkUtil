

#' corrupt_factory
#'
#'Given a function that performs a "corruption" on data, corrupt.factory will generate a function
#'that takes some data x, and with probability error_rate performs the given corruption on it.
#'
#' @param corr.func The corruption function. Must take at least one value
#' @param type_check A function that checks the type of the value given to the corruption function. Default behavior allows any type to be passed to the function. Must take at least one argument, and return a boolean.
#' @param error_message A error message to print if type_check fails. Optional, where default behavior throws a general error message.
#'
#' @return result(val, error_rate = .2, ...), a function that can take a value and an error rate,
#' (Along with other parameters), returns either the same value(\code{val}) passed in, or that value
#' subjected to corr.func(\code{corr.func(val)}) wiith probability = error_rate.
#'
#' @import dplyr
#' @import purrr
#' @examples
#'
#' ## Say I have a corruption function reverse, that takes a string and returns the reverse of that string
#'
#' string_rev <- function(s){
#'    split_str = unlist(strsplit(s))
#'    rev_str = rev(split_str)
#'    return(paste0(rev_str))
#' }
#'
#' #Now we use corrupt.factory to make a function that only returns the reverse of the function with probability = error_rate
#' #Note: We only want to have this work on strings, so we pass in is.character as type_check, and a helpful error mesage
#' string_rev_corr <- corrupt.factory(string_rev, type_check = is.character, error_message = "string_rev_corr cannot reverse a non-character value")
#'
#' #Now we can use the result to corrupt a string of values. The resulting vector should have around 30 of the "hello"s replaced with "olleh".
#' str_vec <- rep("hello", times = 100)
#' corr_string_vec = sapply(str_vec, string_rev_corr, error_rate = .3)
#'

#'
#' @author Sam Murray<slmurray@andrew.cmu.edu>
#'
#' @import tidyverse
#' @import purrr
corrupt_factory <- function(corr.func){


  result <- function(vals, error_rate = .2, ...){
    corrupt <- function(val, error_rate){
      if(rnorm(1)>qnorm(error_rate)){
        return(val)
      }else{
        return(corr.func(val, ...))
      }
    }
    return(modify(vals, corrupt, error_rate))
  }
  result

}


#' corr_numeric
#'
#'A general numeric corruption function. Adds a value from the normal distribution (scaled by scale) to the value.
#'
#' @param val The value to be corrupted. Should be numeric.
#' @param scale The scale applied to the error.
#'
#' @return The value  + the generated error.
#'
#' @author Sam Murray<slmurray@andrew.cmu.edu>
#'
#' @import dplyr
#' @import purrr
corr_numeric <- function(val, scale = 1){

  error = rnorm(1)*scale
  error = as.integer(ifelse(error < 0, floor(error), ceiling(error)))
  return(val + error)
}
#' corr_string
#'
#'A general string corruption function. Either adds, deletes, or substitutes single characters in the string.
#'The number of insertions, deletions, and replacements is generated from the normal, scaled by scale.
#'
#' @param val The value to be corrupted. Should be string
#' @param scale The scale applied to the error.
#'
#' @return The value  + the generated error.
#'
#' @author Sam Murray<slmurray@andrew.cmu.edu>
#'
#' @import tidyverse
#' @import dplyr
#' @import purrr

corr_string <- function(val, scale = 2){


  error = rnorm(1)*scale

  error = ifelse(error < 0,abs(floor(error)), ceiling(error))

  result = val
  for(i in 1:error){
    error_type = sample(1:3,1)
    error_location = sample(str_length(val),1)

    if(error_type == 1){
      new_letter = sample(LETTERS, 1)
      substr(result,error_location,error_location) <-new_letter
    }
    else if(error_type == 2){

      result = paste0(substr(result,1,error_location-1),substr(result,error_location+1,str_length(val)))

    }else{
      new_letter = sample(LETTERS,1)
      result = paste0(substr(result,1,error_location),new_letter,substr(result,error_location+1,str_length(val)))

    }
  }

  result

}

#' corr_replace
#'
#'A general replacement corruption function. Takes a value and a pool of values, and returns a random element from the pool.
#' @param val The value to be corrupted. Should be string
#' @param scale The scale applied to the error.
#'
#' @return The value  + the generated error.
#'
#' @author Sam Murray<slmurray@andrew.cmu.edu>
#'
#' @import tidyverse
#' @import dplyr
#' @import purrr
corr_replace <- function(val, pool = NULL){


  if(is.null(pool)){
    stop("corr.replace tried to replace from an empty pool")
  }
  result = sample(pool,1)

  result

}

#' corrupt_numeric
#'
#'A general numeric corruption function. Adds a value from the normal distribution (scaled by scale) to the value.
#'A built in corruption function, ready to be passed to corrupt data. Does not need to be passed to corrupt.factory.
#'
#' @param val The value to be corrupted. Should be numeric.
#' @param error_rate The probability of the value being corrupted. Default is 2.
#' @param scale The scale applied to the error. Default is 1.
#'
#' @return Either the value handed in, or the value  + the generated error with probability error_rate.
#'
#' @author Sam Murray<slmurray@andrew.cmu.edu>
#'
#' @import tidyverse
#' @import dplyr
#' @import purrr
#' @export

corrupt_numeric <- corrupt_factory(corr_numeric) %>% checkArgs(vals = is.numeric)

#' corrupt_character
#'
#'A general string corruption function. Either adds, deletes, or substitutes single characters in the string.
#'The number of insertions, deletions, and replacements is generated from the normal, scaled by scale.
#'A built in corruption function, ready to be passed to corrupt data. Does not need to be passed to corrupt.factory.
#'
#' @param val The value to be corrupted. Should be string
#' @param error_rate The probability of the value being corrupted. Default is 2.
#' @param scale The scale applied to the error.
#'
#' @return The value passed in, or a corrupted value with probability error rate.
#'
#' @author Sam Murray<slmurray@andrew.cmu.edu>
#'
#' @import tidyverse
#' @import dplyr
#' @import purrr
#' @export
corrupt_character <- corrupt_factory(corr_string) %>% checkArgs(vals = is.character)

#' corrupt_replace
#'
#'A general replacement corruption function. Takes a value and a pool of values, and returns a random element from the pool.
#'A built in corruption function, ready to be passed to corrupt data. Does not need to be passed to corrupt.factory.
#'
#' @param val The value to be corrupted. Should be string
#' @param error_rate The probability of the value being corrupted. Default is 2.
#' @param pool A vector to pull the replacement values from.
#'
#' @return The value passed in, or a replaced value with probability error rate.
#'
#' @author Sam Murray<slmurray@andrew.cmu.edu>
#'
#' @import tidyverse
#'
#' @import dplyr
#' @import purrr
#' @export
corrupt_replace <-  corrupt_factory(corr_replace) %>%  checkArgs(vals = function(x){length(x == 0)})


#' corrupt_data
#'
#'Takes data frame, returns data frame where each column is subjected to the passed in corruption functions.
#' @param df Data frame to be corrupted
#' @param numeric_cor Names of columns to be corrupted numerically
#' @param string_cor Names of columns to be corrupted with string corruption.
#' @param replace_cor Names of columns to be corruped with replacement corruption
#' @param er Vector of error rates for each function, or single value to be applied by all functions. Default is .1.
#' @param v Verbose tag. Default is false
#'
#' @return Corrupted data frame
#' @export
#' @import dplyr
#' @import purrr
#'
#' @examples
#' data_size = 500
#' rec <- generate_data(data_size)
#' error <-  .6
#' corrupt_rec <- corrupt_data(rec, numeric_cor = c("zipcode","birthyear"),string_cor = c("names"),replace_cor = c("town"),er = error, v = TRUE)
#'
#'
#' @export
corrupt_data <- function(df, ... , .v = FALSE, col_names = NULL,col_contams = NULL){
  col_contam <- list(...)

  if(!(is.null(col_names)|is.null(col_contams))){

    unpack_contams <- map2(col_names,col_contams,~rep(.y, times = length(.x)))
    unpack_names <- unlist(col_names)
    names(unpack_contams) <- unpack_names

    col_contam <-  c(col_contam, unpack_contams)
  }

  col_names <- names(col_contam)





  contams <- reduce2(col_names, col_contam, modify_at, .init = df)

  if(.v){
    corr_count = map2(df, contams, ~sum(.x != .y))%>%
    unlist() %>%
    sum()
    print(paste0("Total # of mutations: ",corr_count))
  }

  contams
}




