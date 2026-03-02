# Analyze and process filtered dataset to find key insights ----

# Load packages and data
library(tidyverse)

down_4_plays <- read_csv("data/down_4_plays")


## see possible play types to determine what counts as "going for it" ----
down_4_plays |> 
  count(playType) |> 
  view()

down_4_plays |> 
  filter(playType == "placeholder") |> 
  view() # related to coin tosses--don't need these plays

down_4_plays |> 
  filter(playType == "Uncategorized") |> 
  view() # these don't seem relevant either

down_4_plays |> 
  filter(playType == "Punt Return") |> 
  view() # every category with the word "punt" can be roped into one punt category

down_4_plays |> 
  filter(playType == "Kickoff") |> 
  view() # kickoffs are not fourth down plays, so can remove these observations

down_4_plays |> 
  filter(playType == "Penalty") |> 
  view() # penalty statistics--not relevant to this exploration

down_4_plays |> 
  filter(playType == "Punt Return") |> 
  view() 

down_4_plays |> 
  filter(playType == "Timeout") |> 
  view() 

down_4_plays |> 
  filter(playType == "Fumble") |> 
  view() # these appear to be fumbles on plays where the team went for it on 4th


## update dataset to filter out unnecessary observations and create go_for_it variable

fourth_down <- down_4_plays |> 
  filter(
    !playType %in% c('End Period', 'End of Game', 'End of Regulation', 'Kickoff',
                     'Kickoff Return (Offense)', 'Kickoff Return Touchdown', 'Penalty',
                     'Timeout', 'Uncategorized', 'placeholder')
  ) |> 
  mutate(
    go_for_it = as.integer(playType %in% c('Fumble', 'Fumble Recovery (Opponent)', 'Fumble Recovery (Own)',
                                'Fumble Return Touchdown', 'Interception', 
                                'Interception Return Touchdown', 'Pass', 'Pass Completion',
                                'Pass Incompletion', 'Pass Interception Return', 'Pass Reception',
                                'Passing Touchdown', 'Rush', 'Rushing Touchdown', 'Sack', 'Safety')
  ))








