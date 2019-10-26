
library(httr)
library(jsonlite)
library(knitr)
library(DT)
library(tidyverse)

getData <- function(list_name) {


api_key <- '2rpFu9Mlp7ZAKzXvbujfaj81wWdNp776'

url1 <- "https://api.nytimes.com/svc/books/v3/lists/current/"
url2 <-  ".json?api-key="
url <- paste(url1, list_name, url2, api_key, sep="")

x <-fromJSON(url)

books <- as_tibble(x$results$books)

books <- books %>% 
  select("rank","title", "author", "primary_isbn13", "publisher", "book_image") %>% 
  mutate(book_image = paste("<img src=", dQuote(book_image), " height=\"52\"></img>"))

books

}


