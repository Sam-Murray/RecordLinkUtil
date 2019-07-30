

#' corrupt.factory
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
corrupt.factory <- function(corr.func, type_check = function(x){TRUE}, error_message = NULL){
  result <- function(val, error_rate = .2, ...){


    if(!type_check(val, ...)){
      if(is.null(error_message)){
        error_message <- "Incorrect type for corruption function"

      }
      stop(error_message)
    }


    if(rnorm(1)>qnorm(error_rate)){
      return(val)
    }else{
      return(corr.func(val, ...))
    }
  }
  result

}


#' corr.numeric
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
corr.numeric <- function(val, scale = 1){
  error = rnorm(1)*scale
  error = ifelse(error < 0, floor(error), ceiling(error))
  return(val + error)
}
#' corr.string
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

corr.string <- function(val, scale = 2){


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

#' corr.replace
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
corr.replace <- function(val, pool = NULL){


  if(is.null(pool)){
    stop("corr.replace tried to replace from an empty pool")
  }
  result = sample(pool,1)

  result

}

#' corrupt.numeric
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

corrupt.numeric <- corrupt.factory(corr.numeric,is.numeric, "corrupt.numeric tried to corrupt non-numeric value using corrupt.numeric")

#' corrupt.string
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
corrupt.character <- corrupt.factory(corr.string,is.character, "corrupt.character tried to corrupt non-character value using corrupt.character")

#' corr.replace
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
corrupt.replace <-  corrupt.factory(corr.replace, function(val, pool = NULL)return(length(pool != 0)), "corrupt.replace tried to replace value from empty pool.")



#' corrupt_data
#'
#'Takes data frame, returns data frame where each column is subjected to a numeric, string, or replacement corruption.
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
corrupt_data <- function(df, names, numeric_cor = NULL, string_cor = NULL, replace_cor = NULL, er = .1, v = FALSE){

  if(length(er) == 1){
    er <- rep(er, 3)
  }else if(!(length(er) == 3)){
    stop("Length of er must either be 1 or 3")
  }


  contams = mutate_at(df,numeric_cor, ~sapply(.x,corrupt.numeric,error_rate = er[1]))
  contams = mutate_at(contams,string_cor, ~sapply(.x,corrupt.character,error_rate = er[2]))
  contams = mutate_at(contams,replace_cor, ~sapply(.x,corrupt.replace,error_rate = er[3],pool = .x))


  corr_count = map2(df, contams, ~sum(.x != .y))%>%
    unlist() %>%
    sum()
  print(paste0("Total # of mutations: ",corr_count))
  contams
}




