#' @title Swaps values on two inputs.
#'
#' @param ... arguments passed to other methods
#' @export
value_swap <- function(...) {
  UseMethod("value_swap")
}

#' @title Swaps values on two variables on a dataframe.
#'
#' @description Swaps values of two variables on a dataframe. Takes an optional index.
#' @param .DF: A dataframe.
#' @param .svp: The name of the first variable involved in the value swap.
#' @param .swp: The name of the second variable involved in the value swap.
#' @param .idx: An optional row index. If ommitted all values are swapped between variables.
#' @examples
#'
#' df <- data.frame(a=c(1, 2),
#'                  b=c(3, 4),
#'                  c=c(4, 6))
#'
#' index <- c(TRUE, FALSE)
#'
#' value_swap(df, "a", "b", index)
#'
#' @export
value_swap.data.frame <- function(.DF, .svp, .swp, .idx=NULL) {

  if(missing(.DF)){
    stop("missing dataframe")
  }

  if(missing(.svp) || missing(.swp)){
    stop("missing variable specification for the swap")
  }

  if(is.null(.idx)){
    .idx <- TRUE
  }

  .DF[.idx, c(.svp, .swp)] <- .DF[.idx, c(.swp, .svp)]
  return(.DF)
}
