#' Cleans different formats of unclean date strings
#'
#' @description Cleans different formats of date strings and converts them to a
#'   uniform date format "\%Y-\%m-\%d" as returned by as.Date(). Year must be in
#'   last position. If it encounters a swap in month and day position it swaps
#'   them around.
#'
#' @param date_vec A vector of strings
#' @param unresolved TRUE returns a list including a vector of unresolved values.
#' @return If unresolved is set to true returns a list containing two
#' elements: $Date is a vector of strings with the cleaned or otherwise
#' untouched input data and $unresolved is a logical vector with FALSE if a
#' value was changed and TRUE otherwise. If unresolved is false, only the date
#' vector will be returned.
#' @section Warning: Produces NA if year is in first position and date does
#' not follow the formatting convention of as.Date.
#' @examples
#' clean_date("24.12.2000")
#'
#' clean_date("24/12/00")
#'
#' clean_date("12/24/00")
#'
#' clean_date("12 /2 /2016")
#'
#' clean_date("12 /2 /2016", unresolved=TRUE)$unresolved
#' @export
clean_date <- function(date_vec, unresolved=FALSE) {

  swap_month_day <- function(x, format) {
    for(i in seq_along(x)) {
      t <- try(as.Date(x[i], format), silent=TRUE) # x[i] is a proper date
      if(class(t) != "try-error" & !is.na(as.Date(x[i], format))) { #as.Date("02/14/02") does not give an error but NA
        next
      }

      t <- try(as.Date(x[i]), silent=TRUE)
      if(class(t) == "try-error") {
        pattern_y <- "^([0-9]{1,2})([-/]{1})([0-9]{2})([-/]{1})([0-9]{2})$"
        pattern_Y <- "^([0-9]{1,2})([-/]{1})([0-9]{2})([-/]{1})([0-9]{4})$"
        x[i] <- gsub(pattern_y, "\\3\\2\\1\\4\\5", x[i])
        x[i] <- gsub(pattern_Y, "\\3\\2\\1\\4\\5", x[i])
      }
    }
    return(x)
  }

  ## tidy up
  date_vec <- gsub("[[:blank:]+]", "", date_vec) # " "
  date_vec <- gsub("^$", NA, date_vec) # ""
  date_vec <- gsub("\\.+", "/", date_vec) # 10.01.2010.
  date_vec <- gsub("/{2,}", "/", date_vec) # 10/01//10
  date_vec <- gsub("^\\/+", "", date_vec) # 10/01/10/
  date_vec <- gsub("\\/$+", "", date_vec) # 10/01/10/


  ##format dd/mm/yyyy
  index_long <- grepl("[0-9]{1,2}[/]{1}[0-9]{1,2}[/]{1}[0-9]{4}", date_vec)
  index_long[is.na(index_long)] <- FALSE

  date_vec[index_long] <- swap_month_day(date_vec[index_long], format="%d/%m/%Y")
  date_vec[index_long] <- as.character(as.Date(date_vec[index_long], format = "%d/%m/%Y"))

  ##format dd/mm/yy
  index_short <- grepl("[0-9]{1,2}[/]{1}[0-9]{1,2}[/]{1}[0-9]{2}", date_vec)
  index_short[is.na(index_short)] <- FALSE

  date_vec[index_short] <- swap_month_day(date_vec[index_short], format="%d/%m/%y")
  date_vec[index_short] <- as.character(as.Date(date_vec[index_short], format = "%d/%m/%y"))

  ##format Excel [0-9]{5}
  if(grepl("windows", Sys.getenv("OS"), ignore.case = TRUE)){
    .origin <- "1899-12-30" # see ?as.Date()
  }

  if(grepl("mac", Sys.getenv("OS"), ignore.case = TRUE)){
    .origin <- "1904-01-01"
  }

  index_excel <- grepl("[0-9]{5}", date_vec)
  index_excel[is.na(index_excel)] <- FALSE

  date_vec[index_excel] <- as.character(as.Date(as.numeric(date_vec[index_excel]), origin=.origin))

  ## return unresolved dates
  index_unresolved <- !index_long & !index_short & !index_excel

  if(unresolved) {
    return(list(Date=date_vec,
              unresolved=index_unresolved))
  }
  return(date_vec)
}
