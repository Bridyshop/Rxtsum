#' Function to compute between standard deviation (based on id)
#'
#' @param data a dataframe
#' @param id a grouping variable
#' @param col column names (character)
#' @param na.rm a logical. Should missing values be removed?
#'
#' @return A dataframe with the between standard deviation of each col variables
#' @export
#'
#' @import tidyverse tidyr
#'
#' @examples f.between_df(iris, Species, c("Sepal.Width", "Sepal.Length"))
#'
f.between_df <- function(data, id, col, na.rm = TRUE){
  between_df <- data %>%
    group_by({{ id }}) %>%
    summarise_at(.vars = col, .funs = mean, na.rm = na.rm) %>%
    ungroup() %>%
    summarise_at(.vars = col, .funs = sd, na.rm = na.rm) %>%
    rename_at(.vars = col, .funs = function(x){paste0("btw_sd.", x)})
  return(between_df)
}
