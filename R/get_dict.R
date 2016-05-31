get_dict <- function(word){
  url <- paste0("http://www.merriam-webster.com/dictionary/", word)
  ua_string <- "Googlebot/2.1 (+http://www.google.com/bot.html)"
  html <- rvest::html_session(url, httr::user_agent(ua_string))

  type_speech <- html %>%
    rvest::html_nodes(".main-attr em") %>%
    rvest::html_text() %>%
    gsub("[^[:alnum:] ]", "", .) %>%
    unique

  definition <- html %>%
    rvest::html_nodes(".no-count li:nth-child(1) span") %>%
    .[1] %>%
    rvest::html_text() %>%
    gsub(": ", "", .)

  origin <- html %>%
    rvest::html_nodes("div[class^=card-box]") %>%
    # keep only origin field
    .[grepl("origin", ., ignore.case = TRUE)] %>%
    rvest::html_node("p") %>%
    rvest::html_text()
    # stringr::str_split(",") %>%
    # .[[1]] %>%
    # stringr::str_trim()

  # combine info into data frame
  word_info <- dplyr::data_frame(word = word,
                    type = type_speech,
                    definition = definition,
                    origin = origin)

  return(word_info)
}
