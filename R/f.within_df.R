#'  Function to compute within standard deviation (based on id)
#'
#'  This function allows to compute the within standard deviation of any grouped numeric variable
#'
#' @param data a dataframe
#' @param id a grouping variable
#' @param col column names (character)
#' @param na.rm a logical. Should missing values be removed?
#'
#' @return A dataframe with the within sd of each col variables
#' @export
#' @import tidyverse stringr tibble tidyr
#'
#' @examples f.within_df(iris, Species, c("Sepal.Width", "Sepal.Length"))
f.within_df <- function(data, id, col, na.rm = TRUE){
  id_as_string <- colnames(data %>% select({{ id }}))

  # When called via Rxtsum, id == {{ id }}
  if(stringr::str_detect(id_as_string, pattern = "\\{")){
    id_as_string <- stringr::str_trim(str_remove_all(string = stra, pattern = "\\{|\\}|"))
  } else {id_as_string}

  # Compute means by id:
  df1 <- data %>%
    dplyr::group_by({{ id }}) %>%
    dplyr::summarise_at(.vars = col, .funs = mean, na.rm = na.rm) %>%
    dplyr::rename_at(.vars = col, .funs = function(x){paste0("id_mean_", x)})

  # Compute overall means
  mean_overall <- data %>%
    dplyr::summarise_at(.vars = col, .funs = mean, na.rm = na.rm) %>%
    dplyr::rename_at(.vars = col, .funs = function(x){paste0("o_mean_",x)})

  # Create df with col values, and, for each col, id_mean + o_mean
  a <- data %>%
    dplyr::select({{ id }}, col) %>%
    dplyr::left_join(df1, by = id_as_string) %>%
    tibble::add_column(mean_overall)

  # Pull id_mean for later use (to compute deviations)
  pull_id_mean <- a %>%
    dplyr::select(contains("id_mean")) %>%
    tidyr::gather(key=key,value=value) %>%
    dplyr::pull(value)

  # Pull o_mean for later use (to compute deviations)
  pull_o_mean <- a %>%
    dplyr::select(contains("o_mean")) %>%
    tidyr::gather(key=key,value=value) %>%
    dplyr::pull(value)

  # Gather a
  b <- suppressWarnings(a %>%
                          tidyr::gather(key=key,value=value, col))


  # Compute deviations (value - id_mean + o_mean)
  c <- b %>%
    dplyr::mutate(dev = value - pull_id_mean + pull_o_mean) # following Stata's way to compute within sd

  # Within sd is simply the sd of the dev column
  within_df <- c %>%
    dplyr::select({{ id }}, key, dev) %>%
    tidyr::pivot_wider(names_from = key, values_from = dev, id_cols = c({{ id }},key,dev), values_fn = list(dev=list)) %>%
    tidyr::unchop(everything()) %>%
    dplyr::summarise_at(.vars = col, .funs = sd, na.rm = na.rm) %>%
    dplyr::rename_at(.vars = col, .funs = function(x){paste0("within_sd.",x)})
  return(within_df)
}
