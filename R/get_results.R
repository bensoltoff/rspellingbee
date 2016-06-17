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
  # if no table returns, then no data available - exit
  data <- round_results_table(round, season)
  if(is.null(data)) return(NULL)

  # convert to table
  if(season_format(season) == "D" | season_format(season) == "D2"){
    results <- data %>%
      rvest::html_table() %>%
      as.data.frame %>%
      .[2:(nrow(.)-1),] %>%
      dplyr::tbl_df() %>%
      dplyr::slice(2:n())
  } else if(season_format(season) == "E"){
    results <- data %>%
      rvest::html_table() %>%
      as.data.frame %>%
      dplyr::tbl_df() %>%
      dplyr::slice(2:n())
  } else {
    results <- data %>%
      rvest::html_table(header = TRUE) %>%
      dplyr::tbl_df()
  }

  # standardize column names before fixing
  if((season_format(season) == "B" & (round == 2 | round == 3)) |
     (season_format(season) == "C" & round == 2)){
    results %<>%
      dplyr::rename(Error = `EarnedBonus?`)
  } else if(season_format(season) == "D" | season_format(season) == "D2" | season_format(season) == "E"){
    results %<>%
      dplyr::rename(`No.` = X1,
                    `Speller's Name` = X2,
                    `Speller's Sponsor` = X3,
                    `Correct Spelling` = X4,
                    `Spelling Given` = X5,
                    Error = X6)
  }

  # fix column names
  results %<>%
    dplyr::rename(id = `No.`,
                  speller = `Speller's Name`,
                  sponsor = `Speller's Sponsor`,
                  word_correct = `Correct Spelling`,
                  word_given = `Spelling Given`,
                  error = Error) %>%
    dplyr::mutate(id = tidyr::extract_numeric(id),
                  error = error == "E",
                  id_round = row_number(),
                  word_correct = stringr::str_trim(word_correct),
                  word_given = stringr::str_trim(word_given))

  # add round and year info
  results %<>%
    dplyr::mutate(season = season, round = round)

  return(results)
}

#' Retrieve round results table
#'
#' @param url
#' @param season
#'
#' @return
#'
#' @examples
round_results_table <- function(round, season){
  # build url for round result page
  if(season_format(season) == "A" |
     season_format(season) == "B" |
     season_format(season) == "C"){
    url <- paste0(season_url(season), "/summary/", round)
  } else if(season_format(season) == "D"){
    url <- paste0("https://web.archive.org/web/20070809093424/http://www.spellingbee.com/",
                  substr(season, 3, 4), "bee/rounds/Round",
                  formatC(round, width = 2, format = "d", flag = "0"), ".htm")
  } else if(season_format(season) == "D2"){
    url <- paste0("https://web.archive.org/web/20060721121245/http://www.spellingbee.com/",
                  substr(season, 3, 4), "bee/rounds/Round",
                  formatC(round, width = 2, format = "d", flag = "0"), ".htm")
  } else if(season_format(season) == "E"){
    url <- paste0("https://web.archive.org/web/20050831002454/http://www.spellingbee.com/",
                  substr(season, 3, 4), "bee/rounds/Round",
                  formatC(round, width = 2, format = "d", flag = "0"), ".htm")
  }
  page <- xml2::read_html(url)

  if(season_format(season) == "A"){
    data <- tryCatch(page %>%
                       rvest::html_node("table"),
                     error = function(e) NULL)
  } else if(season_format(season) == "B"){
    data <- tryCatch(page %>%
                       rvest::html_nodes("table") %>%
                       magrittr::extract2(4),
                     error = function(e) NULL)
  } else if(season_format(season) == "C"){
    data <- tryCatch(page %>%
                       rvest::html_nodes("table") %>%
                       magrittr::extract2(5),
                     error = function(e) NULL)
  } else if(season_format(season) == "D" | season_format(season) == "D2" | season_format(season) == "E"){
    data <- tryCatch(page %>%
                       rvest::html_node("center table"),
                     error = function(e) NULL)
  }

  return(data)
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
  rounds <- season_rounds(season)

  results <- lapply(rounds, function(x) get_round_results(season = season, round = x)) %>%
    dplyr::bind_rows(.)

  return(results)
}

#' Determine which rounds were actually played in a given season.
#'
#' @param url
#' @param season
#'
#' @return
#' @export
#'
#' @examples
season_rounds <- function(season){
  url <- season_url(season)
  html <- xml2::read_html(url)

  if(season_format(season) == "A"){
    rounds <- rvest::html_nodes(html, "td:nth-child(1)")
  } else if(season_format(season) == "B"){
    rounds <- rvest::html_nodes(html, "#copyBody td:nth-child(1)")
  } else if(season_format(season) == "C"){
    rounds <- rvest::html_nodes(html, ".b td:nth-child(1)")
  } else if(season_format(season) == "D"){
    rounds <- rvest::html_nodes(html, "b")
  } else if(season_format(season) == "D2"){
    rounds <- rvest::html_nodes(html, "td b")
  } else if(season_format(season) == "E"){
    rounds <- rvest::html_nodes(html, "div center td")
  }

  rounds %<>%
    rvest::html_text() %>%
    tidyr::extract_numeric(.) %>%
    na.omit(.) %>%
    # remove first round because it is preliminaries - no actual results
    setdiff(., 1)

  # correction for various seasons
  if(season == 2011){
    rounds <- 2:20
  } else if(season == 2008){
    rounds <- 2:16
  } else if(season == 2004){
    rounds <- 2:15
  }

  return(rounds)
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

#' Season format
#'
#' Determine which page format is used by the given season. Use to control formatting
#' and interpretation in other functions.
#'
#' @param season
#'
#' @return
#'
#' @examples
season_format <- function(season){
  if(season >= 2012) return("A")
  else if(season < 2012 & season >= 2009) return("B")
  else if(season == 2008) return("C")
  else if(season == 2007) return("D")
  else if(season == 2006) return("D2")
  else if(season == 2005 | season == 2004) return("E")
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
