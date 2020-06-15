#' Function to compute overall standard deviation
#'
#' This function allows to compute the overall standard deviation of a variable and returns a nice table.
#'
#' @param data a dataframe
#' @param id a grouping variable
#' @param col column names (character)
#' @param na.rm a logical. Should missing values be removed?
#'
#' @return A dataframe with the overall standard deviation of each col
#' @export
#' @import tidyverse tibble tidyr
#'
#' @examples f.overall_df(iris, Species, c("Sepal.Width", "Sepal.Length"))
f.overall_df <- function(data,id, col, na.rm = TRUE){
  overall_df <- data %>%
    dplyr::summarise_at(.vars = col, .funs = sd, na.rm = na.rm) %>%
    dplyr::rename_at(.vars = col, .funs = function(x){paste0("overall_sd.", x)})
  return(overall_df)
}

