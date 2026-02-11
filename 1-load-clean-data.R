library(tidyverse)
library(httr)
library(jsonlite)

cfbd_key <- "beAaQ9WtoAzNdREzzMCgQEFBD2uVGYy9vsePqSaLjg6I4w8c5O2rhv4oHpz7ojVI"


url <- "https://api.collegefootballdata.com/plays"

response <- GET(
  url,
  add_headers(Authorization = paste("Bearer", cfbd_key)),
  query = list(
    year = 2023,
    seasonType = "regular",
    week = 1,
    down = 4
  )
)

status_code(response)

fourth_downs <- fromJSON(content(response, "text"), flatten = TRUE) %>%
  as_tibble()

head(fourth_downs)

down_4 <- fourth_downs |> 
  filter(down == 4 & playType != c("Punt", ))




