#' Get speller biography
#'
#' @param season Season of competition
#' @param id ID number of the speller
#'
#' @return
#' @export
#'
#' @examples
get_bio <- function(season, id){
  url <- paste0("https://secure.spellingbee.com/public/spellers/", season, "/", id)

  # extract useful data
  html <- xml2::read_html(url)

  ## get speller name
  speller_name <- rvest::html_node(html, ".roster-bio-speller-name") %>%
    rvest::html_text() %>%
    # remove speller id
    sub(".*,", "", .) %>%
    # trim string white space
    stringr::str_trim(.) %>%
    # remove extra inner white space
    gsub("\\s+", " ", .)

  # get rest of attributes
  data <- rvest::html_nodes(html, "p") %>%
    rvest::html_text()
  data <- dplyr::data_frame(speller = speller_name,
                            sponsor = data[1],
                            grade = data[2],
                            age = data[3],
                            school = data[4],
                            bio = data[5],
                            id = id)

  return(data)
}

#' Get biographies of all spellers in a season
#'
#' @param season Season of competition
#'
#' @return
#' @export
#'
#' @examples
get_season_bios <- function(season){
  # determine number of spellers in the season
  url <- paste0("http://spellingbee.com/public/results/", season, "/round_results/summary/2")
  html <- xml2::read_html(url)
  spellers <- rvest::html_nodes(html, "td:nth-child(1)") %>%
    rvest::html_text() %>%
    as.numeric(.)

  results <- lapply(spellers, function(x) get_bio(season = season, id = x)) %>%
    dplyr::bind_rows(.) %>%
    dplyr::mutate(season = season)

  return(results)
}

#' Get all biographies of spellers in multiple seasons
#'
#' @param seasons Vector of seasons
#'
#' @return
#' @export
#'
#' @examples
get_bios <- function(seasons){
  results <- lapply(seasons, get_season_bios) %>%
    dplyr::bind_rows(.)

  return(results)
}

