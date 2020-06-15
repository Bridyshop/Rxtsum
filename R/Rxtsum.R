#' Rxtsum
#' Rxtsum
#'
#' This function allows to compute the between/within standard deviation of any grouped variables. It replicates Stata's xtsum methodology and returns a dataframe which is convenient for publication with package like kable.
#'
#' @param data a dataframe
#' @param id a grouping variable
#' @param col column names (character)
#' @param na.rm a logical. Should missing values be removed?
#'
#' @return A dataframe with between/within/overall standard deviation for each col variables defined.
#' @export
#'
#' @examples Rxtsum(iris, Species, c("Sepal.Width", "Sepal.Length"))
#' @import tidyverse stringr tibble tidyr




Rxtsum <- function(data, id, col, na.rm=TRUE) {
  na.rm <- na.rm
  w <- f.within_df(data, {{ id }}, col, na.rm)
  o <- f.overall_df(data, {{ id }}, col, na.rm)
  b <- f.between_df(data, {{ id }}, col, na.rm)

  # Create summary df
  Rxtsum_df <- o %>%
    tibble::add_column(w,b) %>%
    tidyr::gather(key=key,value=value) %>%
    tidyr::separate(key, into = c("prefix", "var"), sep = "_") %>%
    tidyr::pivot_wider(names_from = var, values_from = value)

  return(Rxtsum_df)
}
