#' @title Renames the values on an input according to a pattern-value map
#'
#' @param ... arguments passed to other methods
#' @seealso \code{\link{find_replace.data.frame}}
#' @export
find_replace <- function(...){
  UseMethod("find_replace")
}


#' @title Renames the values on an input according to a pattern-value map
#'
#' @description Fallback method
#' @export
find_replace.default <- function(d){
  print("No method found. Consult ?find_replace for type-specific methods.")
}


#' @title Renames the values on a data.frame according to a pattern-value map
#'
#' @param .DF: A dataframe.
#' @param .index: An optional index vector of the same length as .DF.
#' @param .replace: A list of variables changes are applied to.
#' @param .lookup: An optional variable that can be used as alternative lookup.
#' @param .name_vec: A vector that serves as a lookup map. This vector is of the form regexp = "replacement".
#' @param .all_variables: logic, if true, then all varibles will be selected. otherwise, only .replace will be selected
#' @examples
#'
#' model <- rownames(mtcars)
#' new= rep("Merc 1", dim(mtcars)[1])
#' mcars <- cbind(mtcars,
#'               model,
#'               new,
#'               stringsAsFactors=FALSE)
#'
#' make <- c("(?i)^merc"="BMW")
#'
#'
#' find_replace(mcars,
#'              .replace="model",
#'              .name_vec=make)
#'
#' find_replace(mcars,
#'             .replace=c("mpg" ,"model", "new"),
#'             .name_vec=make,
#'             .all_variables=FALSE)
#'
#' find_replace(mcars,
#'             .name_vec=make,
#'             .all_variables = TRUE)
#'
#' index <- with(mcars, model=="Fiat 128")
#' find_replace(mcars,
#'             .name_vec=make,
#'             .all_variables=TRUE,
#'             .index=index)
#' @export
find_replace.data.frame <- function(.DF, .index=NULL, .replace=NULL, .lookup=NULL, .name_vec, .all_variables=FALSE) {
  if(.all_variables) {
    .replace = names(.DF)
  }

  if(is.null(.replace)) {
    stop("no replacement variable defined")
  }

  if(is.null(.lookup)) {
    .lookup <- .replace
  }


  if(!is.null(.index) && length(.index) != dim(.DF)[1]){
    stop("index has wrong dimension")
  }

  if(is.null(.index)) {
    .index <- TRUE
  }

  if(missing(.name_vec)) {
    stop("missing lookup map")
  }

  rename <- function(i, .idx) {
    return(sapply(.DF[.index & .idx, .lookup],
                  function(df){gsub(names(.name_vec)[i],
                                    as.vector(.name_vec)[i], df)
                  }))
  }

  for(i in 1:length(.name_vec)) {
    if(length(.lookup) > 1) {
      .idx <- apply(.DF[, .lookup], 2, stringr::str_detect, names(.name_vec)[i])
      .idx <- !apply(.idx, 1, all)
    } else {
      .idx <- stringr::str_detect(.DF[,.lookup], names(.name_vec)[i])
    }
    .idx[is.na(.idx)] <- FALSE

    .DF[.index & .idx, .replace] <- rename(i, .idx)
  }
  return(.DF)

}
