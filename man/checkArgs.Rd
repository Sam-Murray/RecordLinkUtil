% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/supplyArgs.R
\name{checkArgs}
\alias{checkArgs}
\title{checkArgs}
\usage{
checkArgs(func, ...)
}
\arguments{
\item{func}{Any non-primative function with arguments.}

\item{...}{Name value pairs of arguments to func, and their associated type(ie "numeric") or type check function(ie is.numeric). Check args will use either.}
}
\description{
Takes a nonprimitive function with arguments, and returns a modified function that checks the types of each argument.
}
\details{
If multiple name value pairs in ... contain the same name, checkArgs will do two checks on that argument. However, this is not reccomended.
}
\author{
Sam Murray <slmurray@andrew.cmu.edu>
}
