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
           id_round = row_number(),
           word_correct = stringr::str_trim(word_correct),
           word_given = stringr::str_trim(word_given))

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

#' Create season url
#'
#' Create url to round results page for each season. Pre-2012, inconsistent pattern.
#'
#' @param season
#'
#' @return
#'
#' @examples
season_url <- function(season){
  if(season >= 2012 & season <= 2016){
    url <- paste0("http://spellingbee.com/public/results/", season, "/round_results")
  } else if(season == 2011){
    url <- "https://web.archive.org/web/20110602064047/http://public.spellingbee.com/public/results/2011/round_results"
  } else if(season == 2010){
    url <- "https://web.archive.org/web/20100803203945/http://public.spellingbee.com/public/results/2010/round_results"
  } else if(season == 2009){
    url <- "https://web.archive.org/web/20090819160403/http://public.spellingbee.com/public/results/2009/round_results"
  } else if(season == 2008){
    url <- "https://web.archive.org/web/20080907205206/http://public.spellingbee.com/public/results/round_results/"
  } else if(season == 2007){
    url <- "https://web.archive.org/web/20070804045202/http://www.spellingbee.com/results.asp"
  } else if(season == 2006){
    url <- "https://web.archive.org/web/20060721121245/http://www.spellingbee.com/results.asp"
  } else if(season == 2005){
    url <- "https://web.archive.org/web/20050901025018/http://www.spellingbee.com/05bee/resultsindex.shtml"
  } else if(season == 2004){
    url <- "https://web.archive.org/web/20040811133841/http://spellingbee.com/04bee/rounds/resultsindex.html"
  } else if(season == 2003){
    url <- "https://web.archive.org/web/20030811070931/http://spellingbee.com/03bee/resultsindex.shtml"
  } else if(season == 2002){
    url <- "https://web.archive.org/web/20020802203649/http://www.spellingbee.com/02bee/results2002.htm"
  } else if(season == 2001){
    url <- "https://web.archive.org/web/20010616205939/http://www.spellingbee.com/results2001.htm"
  } else if(season == 2000){
    url <- "https://web.archive.org/web/20000706222837/http://www.spellingbee.com/00bee/results00.htm"
  } else if(season == 1999){
    url <- "https://web.archive.org/web/20000903063840/http://www.spellingbee.com/99bee/rounds99/results99.htm"
  } else if(season == 1998){
    url <- "https://web.archive.org/web/20000520073508/http://www.spellingbee.com/results98.htm"
  } else if(season == 1997){
    url <- "https://web.archive.org/web/20000817090330/http://www.spellingbee.com/results97.htm"
  } else if(season == 1996){
    url <- "https://web.archive.org/web/20000517183307/http://www.spellingbee.com/results96.htm"
  } else {
    url <- NA
  }

  return(url)
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
