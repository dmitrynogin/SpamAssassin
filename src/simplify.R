library(magrittr)

noMarkup <- function(body) gsub("<[^<>]+>", " ", body)
noNumbers <- function(body) gsub("[0-9]+", "number", body)
noLinks <- function(body) gsub("(http|https)://[^\\s]*", "httpAddr", body)
noEmails <- function(body) gsub("[^\\s]+@[^\\s]+", "emailAddr", body)
noMoney <- function(body) gsub("[$]+", "dollar", body)
noJiberish <- function(body) gsub("[^\x20-\x7E]+", " ", body)

simplify <- function(body)
    body %>%
        noMarkup %>%
        noNumbers %>%
        noLinks %>%
        noEmails %>%
        noMoney %>%
        noJiberish
