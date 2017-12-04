#' @title Swaps values on two inputs of equal length
#'
#' @description Swaps the values on two inputs of equal length
#' @param x An object
#' @param ... Arguments passed to other methods
#' @seealso \code{\link{value_swap.data.frame}}
#' @export
value_swap <- function(x, ...) {
  UseMethod("value_swap")
}


#' @title Swaps values on two inputs of equal length
#'
#' @description Fallback method
#' @param x Argument of a type for which no value_swap method exists yet.
#' @param ... Arguments passed to other methods
#' @export
value_swap.default <- function(x, ...){
  stop("No method found. Consult methods(find_replace) or ?find_replace for more information")
}


#' @title Swaps values on two variables on a dataframe.
#'
#' @description Swaps values of two variables on a dataframe. Takes an optional index.
#' @param x A dataframe.
#' @param .svp The name of the first variable involved in the value swap.
#' @param .swp The name of the second variable involved in the value swap.
#' @param .idx An optional row index. If ommitted all values are swapped between variables.
#' @param ... Arguments passed on to other methods.
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
value_swap.data.frame <- function(x, .svp, .swp, .idx=NULL, ...) {

  if(missing(x)){
    stop("missing dataframe")
  }

  if(missing(.svp) || missing(.swp)){
    stop("missing variable specification for the swap")
  }

  if(is.null(.idx)){
    .idx <- TRUE
  }

  x[.idx, c(.svp, .swp)] <- x[.idx, c(.swp, .svp)]
  return(x)
}
