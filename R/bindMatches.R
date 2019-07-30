#' bindMatches
#'
#' Given 2 data frames(dfA and dfB) and a nx2 matrix of matched indices, will return a dataframe containing
#' both dataframes bound by index. If there are identical names in both dataframes, the result
#' will differentiate by adding idA(default is "A.") to the duplicate names in dfA, and idB(default is "B.") to those in dfB.
#'
#' @usage bindMatches(dfA, dfB, inds, idA, idB)
#'
#' @param dfA First data frame to bind
#' @param dfB Second data frame to bind
#' @param inds A nx2 index pair matrix to bind on
#' @param idA What to concatenate to a column name in dfA if there exists a identical column in dfB
#' @param idB What to concatenate to a column name in dfB if there exists a identical column in dfB
#'
#'
#' @return \code{bindMatches} returns a dataframe of the original dataframes bound by index.
#'
#' @author Sam Murray<slmurray@andrew.cmu.edu>
#' @export
#' @import tidyverse

bindMatches <- function(dfA, dfB, inds, idA = "A.", idB = "B.") {

  namesA = names(dfA)
  namesB = names(dfB)
  namesAB = intersect(namesA, namesB)

  if(length(namesAB != 0)){
    dup_inds_A <-  namesA %in% namesAB
    namesA[dup_inds_A] <-  paste0(idA,namesA[dup_inds_A])
    names(dfA) <-  namesA
    dup_inds_B <-  namesB %in% namesAB
    namesB[dup_inds_B] <- paste0(idB,namesB[dup_inds_B])
    names(dfB) <-  namesB

  }

  return(cbind(dfA[inds[,1], ], dfB[inds[,2], ]))

}

#' bindMatches_p
#'
#' Same as bindMatches, but also binds an extra column for the probability of each match
#'
#' @usage bindMatches(dfA, dfB, inds, p, idA, idB)
#'
#' @param dfA First data frame to bind
#' @param dfB First data frame to bind
#' @param inds A nx2 index pair matrix to bind on
#' @param P a nx1 vector of the probability of each pair
#' @param idA What to concatenate to a column name in dfA if there exists a identical column in dfB
#' @param idB What to concatenate to a column name in dfB if there exists a identical column in dfB
#'
#'
#' @return \code{bindMatches} returns a dataframe of the original dataframes bound by index.
#'
#' @author Sam Murray<slmurray@andrew.cmu.edu>
#' @export
#' @import tidyverse

bindMatches_p <- function(dfA, dfB, p , inds, idA = "A.", idB = "B.") {

  namesA = names(dfA)
  namesB = names(dfB)
  namesAB = intersect(namesA, namesB)

  if(length(namesAB != 0)){
    dup_inds_A <-  namesA %in% namesAB
    namesA[dup_inds_A] <-  paste0(idA,namesA[dup_inds_A])
    names(dfA) <-  namesA
    dup_inds_B <-  namesB %in% namesAB
    namesB[dup_inds_B] <- paste0(idB,namesB[dup_inds_B])
    names(dfB) <-  namesB

  }

  return(cbind(dfA[inds[,1], ], dfB[inds[,2], ],p))

}
