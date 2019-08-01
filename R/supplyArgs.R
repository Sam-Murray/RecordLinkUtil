#' typeCheck
#'
#' Takes a type or type check function and a value and determines if that value is of that type.
#'
#'
#' @usage checkArgs(func, ...)
#'
#' @param val Any value
#' @param check Either a string that identifies a class ("numeric"), a function identifies a class (is.numeric), or a list of strings and functions.
#'
#'
#' @author Sam Murray <slmurray@andrew.cmu.edu>
#'

#' @import tidyverse
typeCheck <- function(val, check){
  if(is.list(check)){
    return(all(map_lgl(check, ~typeCheck(val, .x))))
  }else if(is.character(check)){
    return(is(val,check))
  }else if(is.function(check) | class(check) == "function"){
    return(check(val))
  }
}


#' checkArgs
#'
#' Takes a nonprimitive function with arguments, and returns a modified function that checks the types of each argument.
#'
#'
#' @usage checkArgs(func, ...)
#'
#' @param func Any non-primative function with arguments.
#' @param ... Name value pairs of arguments to func, and their associated type(ie "numeric") or type check function(ie is.numeric). Check args will use either.
#'
#' @details If multiple name value pairs in ... contain the same name, checkArgs will do two checks on that argument. However, this is not reccomended.
#'
#' @example
#' #Say we have a function f that should only take a string:
#'str_only_print <- function(x){
#'  if(!is.character(x)){
#'    stop("AAAAAAAAAAAA")
#'  }else{
#'    print(x)
#'  }
#'}
#'#We can add on the argument checks using checkArgs:
#'str_only_print <-  checkArgs(str_only_print, x = is.character)
#'#Now we instead of throwing error "AAAAAAAAAAAA" when being passed a string, it will throw "Incorrect argument type for argument x in str_only_print"
#'
#'
#' @author Sam Murray <slmurray@andrew.cmu.edu>
#'
#'
#'
#' @import tidyverse
#' @importFrom rlang caller_env
#' @export
checkArgs <-  function(func, ...){
  func_name <-   paste0(deparse(substitute(func)), collapse = "")
  if(nchar(func_name) >= 50){
    func_name <- paste0(substr(func_name, 0, 50), " ... ")
  }

  if(!(is.function(func)|(class(func) == "function"))){
    stop(paste("checkArgs only accepts functions, but was passed something with class: ", class(func)))
  }
  if(is.primitive(func)){
    stop(paste("checkArgs was passed primitive function ", func_name, ", but primitive functions are currently unsupported"))

  }
  #Gets the list of arguments to check
  checkList <-  list(...)

  #Checks that the tests are valid tests:
  checkTest <- function(test){
    if(is.character(test)|is.function(test)|class(test)=="function") return(TRUE)
    else if(is.list(test)) return(all(map_lgl(test, checkTest)))
    else return(FALSE)
  }

  if(!all(map_lgl(checkList,checkTest))){
    stop("All checks passed to checkArgs must be either functions or strings denoting classes, or a list of the 2")
  }

  #Gets the list of args for
  argList = names(formals(func))
  #Checks that the variables being checked are in the list of args for func

  if(!all(names(checkList) %in% argList)){
    stop("All name value pairs passed to checkArgs must corrispond to a argument of func")
  }

  output <-  function(){
    par_env <- caller_env()
    my_args <- as.list(match.call())
    my_args <- my_args[2:length(my_args)]

    evaled_args <- map(my_args, eval, envir = par_env)
    names(evaled_args) <-  names(my_args)

    my_args <- evaled_args

    for(var_name in unique(names(checkList))){
       # print(paste0("checking var of name ", var_name))
      var_checks <- checkList[names(checkList) == var_name]
      val <- my_args[[var_name]]
      # print(paste0("Val of that var is ", val))
      # print(paste0("And the checks to perform are: ", var_checks))
      for(check in var_checks){
        if(!typeCheck(val,check)){

          stop(paste0("Incorrect argument of class: ", class(val), " for argument: ", var_name ," in function: ", func_name))
        }
      }
    }

    return(do.call(func,my_args))
  }

  formals(output) <-  formals(func)

  return(output)
}


#' supplyArgs
#'
#' Takes a nonprimitive function with arguments, and returns a modified function supplies default values for named arguments of the function.
#' Default values ARE overridable.
#'
#'
#' @usage supplyArgs(func, ...)
#'
#' @param func Any non-primative function with arguments.
#' @param ... Name value pairs of arguments to func, and their associated value.
#'
#' @details If multiple name value pairs in ... contain the same name, supplyArgs will through an error.
#'
#' @example
#' #Say we have a function print_with_cond that has 2 arguments:
#'print_with_cond <- function(x,y){
#'  if(y){
#'    print(x)
#'  }
#'}
#'#We want to provide default values to that function
#'print_with_cond <-  supplyArgs(print_with_cond, y = TRUE)
#'#Now instead of throwing error "Error in print_with_cond(x) : argument "y" is missing, with no default", print_with_cond(x) will act the same as a call to print(x).
#'
#'
#' @author Sam Murray <slmurray@andrew.cmu.edu>
#'
#' @importFrom rlang caller_env
#' @import tidyverse
#' @export
supplyArgs <-  function(func, ..., .OVERIDE = TRUE){
  func_name <-   paste0(deparse(substitute(func)), collapse = "")
  if(nchar(func_name) >= 50){
    func_name <- paste0(substr(func_name, 0, 50), " ... ")
  }

  if(!(is.function(func)|(class(func) == "function"))){
    stop(paste("supplyArgs only accepts functions, but was passed something with class: ", class(func)))
  }
  if(is.primitive(func)){
    stop(paste("supplyArgs was passed primitive function ", func_name, ", but primitive functions are currently unsupported"))

  }
  #Gets the list of arguments to and values
  valList <-  list(...)



  #Gets the list of args for func
  argList = names(formals(func))
  #Checks that the variables being checked are in the list of args for func


  output <-  function(){
    par_env <- caller_env()
    my_args <- as.list(match.call())
    my_args <- my_args[2:length(my_args)]

    evaled_args <- map(my_args, eval, envir = par_env)
    names(evaled_args) <-  names(my_args)

    my_args <- evaled_args


    if(.OVERIDE){
      my_args <- c(my_args, valList[!(names(valList) %in% names(my_args))])
    }else{
      my_args <- c(my_args[!(names(my_args) %in% names(valList))], valList)
    }


    return(do.call(func,my_args))
  }

  if(.OVERIDE){
    formals(output) <-  formals(func)
  }else{
    formals(output) <-  formals(func)[!(names(formals(func)) %in% names(valList))]
  }


  return(output)
}
#' wrapFunc
#'
#' Takes two functions\code{(func, wrapFunc)}, returns the function composition of the two funtions \code{(wrapFunc(func()))}.
#' The return value will always have same arguments as func.
#'
#'
#' @usage wrapFunc(func, wrapFunc)
#'
#' @param func Any non-primative function with arguments.
#' @param wrapFunc Any function that can take as input the output of func. If this is not flexable enough, use supplyArgs or composeFunc.
#'
#' @details If multiple name value pairs in ... contain the same name, supplyArgs will through an error.
#'
#' @example
#' #Say we have a function print_with_cond that has 2 arguments:
#'print_with_cond <- function(x,y){
#'  if(y){
#'    print(x)
#'  }
#'}
#'#We want to provide default values to that function
#'print_with_cond <-  supplyArgs(print_with_cond, y = TRUE)
#'#Now instead of throwing error "Error in print_with_cond(x) : argument "y" is missing, with no default", print_with_cond(x) will act the same as a call to print(x).
#'
#'
#' @author Sam Murray <slmurray@andrew.cmu.edu>
#'
#' @importFrom rlang caller_env
#' @import tidyverse
#'
#' @export
wrapFunc <- function(func, wrap_func){
  output <-  function(...){
    par_env <- caller_env()
    my_args <- as.list(match.call())
    my_args <- my_args[2:length(my_args)]

    evaled_args <- map(my_args, eval, envir = par_env)
    names(evaled_args) <-  names(my_args)

    my_args <- evaled_args

    return(wrap_func(do.call(func,my_args)))
  }
  if(!is.primitive(func)){
    formals(output) <-  formals(func)
  }

  return(output)
}

