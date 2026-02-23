# Load and clean data for easy usage ----
# This script will outline my process for obtaining fourth down plays from regular season games between 2016 and 2025

# Load packages
library(tidyverse)
library(httr)
library(jsonlite)

cfbd_key <- "beAaQ9WtoAzNdREzzMCgQEFBD2uVGYy9vsePqSaLjg6I4w8c5O2rhv4oHpz7ojVI"


url <- "https://api.collegefootballdata.com/plays"

# Create function to be able to group data from multiple seasons into one dataset
get_week_data <- function(year, week) {
  response <- GET(
    url,
    add_headers(Authorization = paste("Bearer", cfbd_key)),
    query = list(
      year = year,
      seasonType = "regular",
      week = week
    )
  )
  
  fromJSON(content(response, "text"), flatten = TRUE) |> 
    as_tibble() |> 
    filter(down == 4) |> 
    mutate(season = year)
}

# Want to look at 2016-2025 regular seeason games (weeks 1-15)
years <- 2016:2025
weeks <- 1:15

params <- expand_grid(year = years, week = weeks)

# return every fourth down play from 2016-2025 regular seasons
down_4_plays <- pmap_dfr(params, get_week_data)

# I have what I need, but it did take a while to run--had to get every play from 2016-2025, and then wait for R to filter the entire data to only include fourth downs

# Save out dataset as a CSV to avoid having to run this again
write_csv(down_4_plays, "data/down_4_plays")
# this dataset is only 46.5 MB. Very reasonable and can still commit it to Github without violating the 100 MB limit



