#' Get round results
#'
#' Get all results from a single round in the spelling bee
#'
#' @param round Round number
#' @param season Season (e.g. 2012)
#'
#' @return
#' @export
#'
#' @importFrom dplyr tbl_df
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#'
get_round_results <- function(round, season){
  # get table from page
  url <- paste0("http://spellingbee.com/public/results/", season, "/round_results/summary/", round)

  # if no table returns, then no data available - exit
  data <- tryCatch(xml2::read_html(url) %>%
                     rvest::html_node("table"),
                   error = function(e) NULL)
  if(is.null(data)) return(NULL)

  # convert to table
  results <- data %>%
    rvest::html_table() %>%
    tbl_df

  # fix column names
  results %<>%
    dplyr::rename(id = `No.`,
           speller = `Speller's Name`,
           sponsor = `Speller's Sponsor`,
           word_correct = `Correct Spelling`,
           word_given = `Spelling Given`,
           error = Error) %>%
    dplyr::mutate(error = error == "E",
           id_round = row_number())

  # add round and year info
  results %<>%
    dplyr::mutate(season = season, round = round)

  return(results)
}

#' Get season rounds
#'
#' Get all rounds in a single competition
#'
#' @param season Season (e.g. 2012)
#'
#' @return
#' @export
#'
get_season_rounds <- function(season){
  # determine number of rounds in the season
  url <- paste0("http://spellingbee.com/public/results/", season, "/round_results")
  html <- xml2::read_html(url)
  rounds <- rvest::html_nodes(html, "td:nth-child(1)") %>%
    rvest::html_text() %>%
    tidyr::extract_numeric(.) %>%
    na.omit(.) %>%
    # remove first round because it is preliminaries - no actual results
    setdiff(., 1)

  results <- lapply(rounds, function(x) get_round_results(season = season, round = x)) %>%
    dplyr::bind_rows(.)

  return(results)
}

#' Get seasons
#'
#' Get round results from multiple seasons
#'
#' @param seasons Seasons in a vector
#'
#' @return
#' @export
#'
get_seasons <- function(seasons){
  results <- lapply(seasons, get_season_rounds) %>%
    dplyr::bind_rows(.)
  return(results)
}
