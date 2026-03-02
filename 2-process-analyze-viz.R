# Process, analyze, and visualize filtered dataset to find key insights ----

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


## update dataset to filter out unnecessary observations and create go_for_it variable ----
# also add a success variable to determine if the play earned a first down or not

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
  )) |> 
  mutate(
    success = as.integer(yardsGained >= distance)
  )


## analyze fourth down attempt rate trends ----

# fourth down attempt rate this past season:
fourth_down |> 
  filter(season == 2025) |> 
  summarize(
    down_4_attempt_rate = mean(go_for_it)
  ) # teams went for it on 26.4% of 4th down plays


# looking at attempt rates by each season
fourth_down |> 
  summarize(
    down_4_attempt_rate = mean(go_for_it),
    .by = season
  ) # nearly 7% increase over last 10 years

# now let's visualize the above code:
fourth_down |> 
  summarize(
    down_4_attempt_rate = mean(go_for_it),
    .by = season
  ) |> 
  ggplot(aes(season, down_4_attempt_rate)) +
  geom_point(size = 2, color = "red") +
  geom_line() +
  scale_x_continuous(breaks = unique(fourth_down$season)) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    title = "College football is seeing a spike in fourth down conversion attempts",
    subtitle = "Attempt rates have increased from 19.8% to 26.4% over the last 10 seasons",
    x = "Year",
    y = "Fourth down attempt rate"
  )


# look at fourth and short (<5 yards to go) specifically:
fourth_down |> 
  filter(distance <= 5) |> 
  summarize(
    down_4_attempt_rate = mean(go_for_it),
    .by = season
  ) |> # even more pronounced spike in 4th attempts in 4th-and-short situations
  ggplot(aes(season, down_4_attempt_rate)) +
  geom_point(size = 2, color = "red") +
  geom_line() +
  scale_x_continuous(breaks = unique(fourth_down$season)) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    title = "4th-and-short attempts have seen an even higher spike",
    subtitle = "Attempt rates with 5 or fewer yards to go have increased from 34.7% to 46.3%",
    x = "Year",
    y = "Fourth down attempt rate"
  )


## look at fourth down success rates ----

# success rate by year
fourth_down |> 
  filter(go_for_it == 1) |> 
  summarize(success_rate = mean(success),
            .by = season)
# success rate has remained largely steady

# visualize the above code:
fourth_down |> 
  filter(go_for_it == 1) |> 
  summarize(success_rate = mean(success),
            .by = season) |> 
  ggplot(aes(season, success_rate)) +
  geom_point(size = 2, color = "red") +
  geom_line() +
  scale_x_continuous(breaks = unique(fourth_down$season)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0.45, 0.65)) +
  theme_minimal() +
  labs(
    title = "4th down success rates have remained steady",
    subtitle = "Teams convert on fourth down attempts just over 50% of the time",
    x = "Year",
    y = "Fourth down success rate"
  )

# now look at 4th and short success rates:
fourth_down |> 
  filter(go_for_it == 1) |> 
  summarize(success_rate = mean(success),
            .by = season)
# success rate has remained largely steady

# visualize the above code:
fourth_down |> 
  filter(go_for_it == 1) |> 
  summarize(success_rate = mean(success),
            .by = season) |> 
  ggplot(aes(season, success_rate)) +
  geom_point(size = 2, color = "red") +
  geom_line() +
  scale_x_continuous(breaks = unique(fourth_down$season)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0.45, 0.65)) +
  theme_minimal() +
  labs(
    title = "4th down success rates have remained steady",
    subtitle = "Teams convert on fourth down attempts just over 50% of the time",
    x = "Year",
    y = "Fourth down success rate"
  )


# now look at 4th and short:
fourth_down |> 
  filter(go_for_it == 1 & distance <= 5) |> 
  summarize(success_rate = mean(success),
            .by = season) |> 
  ggplot(aes(season, success_rate)) +
  geom_point(size = 2, color = "red") +
  geom_line() +
  scale_x_continuous(breaks = unique(fourth_down$season)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0.45, 0.65)) +
  theme_minimal() +
  labs(
    title = "4th-and-short success rates have remained steady",
    subtitle = "Teams convert on 4th-and-short attempts around 60% of the time",
    x = "Year",
    y = "Fourth down success rate"
  )

  








