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

#' Clean spellers' biographies
#'
#' @param spellers
#'
#' @return
#' @export
#'
#' @examples
clean_bios <- function(spellers){
  # clean up existing attributes
  spellers %<>%
    na.omit %>%
    # convert grade to numeric
    dplyr::mutate(grade = stringr::word(grade, 1),
           grade_num = ifelse(grade == "First", 1, NA),
           grade_num = ifelse(grade == "Second", 2, grade_num),
           grade_num = ifelse(grade == "Third", 3, grade_num),
           grade_num = ifelse(grade == "Fourth", 4, grade_num),
           grade_num = ifelse(grade == "Fifth", 5, grade_num),
           grade_num = ifelse(grade == "Sixth", 6, grade_num),
           grade_num = ifelse(grade == "Seventh", 7, grade_num),
           grade_num = ifelse(grade == "Eighth", 8, grade_num),
           grade = grade_num) %>%
    dplyr::select(-grade_num) %>%
    # extract state name
    dplyr::mutate(state = stringr::str_extract(sponsor, paste0(state.name, collapse = "|"))) %>%
    # home school
    dplyr::mutate(homeschool = grepl("Home", school, ignore.case = TRUE)) %>%
    # extract first name
    dplyr::mutate(first_name = stringr::word(speller, 1))

  # get gender
  spellers_gender <- gender::gender(spellers$first_name) %>%
    dplyr::select(name, gender) %>%
    dplyr::rename(first_name = name)

  spellers <- dplyr::left_join(spellers, spellers_gender) %>%
    unique

  # estimate ethnicity
  ethnicity <- dplyr::data_frame(surname = stringr::word(spellers$speller, -1)) %>%
    unique %>%
    # calculate probabilities
    wru::race.pred(surname.only = TRUE) %>%
    tbl_df %>%
    # gather predictions to tidy data
    tidyr::gather(ethnicity, prob, pred.whi:pred.oth) %>%
    # rename ethnic categories
    dplyr::mutate(ethnicity = gsub("pred.", "", ethnicity),
           ethnicity = ifelse(ethnicity == "whi", "white", ethnicity),
           ethnicity = ifelse(ethnicity == "bla", "black", ethnicity),
           ethnicity = ifelse(ethnicity == "his", "hispanic", ethnicity),
           ethnicity = ifelse(ethnicity == "asi", "asian", ethnicity),
           ethnicity = ifelse(ethnicity == "oth", "other", ethnicity)) %>%
    # keep only most probable ethnicity
    dplyr::group_by(surname) %>%
    dplyr::filter(prob == max(prob))

  spellers %<>%
    dplyr::mutate(surname = stringr::word(speller, -1)) %>%
    dplyr::left_join(ethnicity)

  return(spellers)
}







