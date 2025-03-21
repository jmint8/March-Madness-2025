library(dplyr)
library(purrr)
library(forcats)
library(readr)

### FIRST, READ IN THE FILE NCAA_Hoops_Results_3_16_2025.csv as a data frame.
### Then, you can run this entire R-script to produce game_df, the data frame
###  of all games.

ncaa_mbb = NCAA_Hoops_Results_3_16_2025

# only include games with two D1 teams. Each game is listed twice, so only include one observation per game
ncaa_mbb = subset(ncaa_mbb, D1==2 & teamscore > oppscore)

# get unique list of teams
all_teams <- unique(c(ncaa_mbb$team))
num_teams = length(all_teams)

# convert dates to integer
ncaa_mbb = cbind(ncaa_mbb, "game_date" = as.numeric(as.Date(with(ncaa_mbb, paste(year,month,day,sep="-")),"%Y-%m-%d")))

# Function to create column vector of wins/losses for each team in every game
# subset above such that team is always the winner, and opponent is the loser
# for each game, winner is assigned 1, and loser is assigned -1
transform_wl <- function(game_data, team_id){
  col_w <- if_else(game_data$team == team_id, 1, 0) %>%
    na_if(0)
  col_l <- if_else(game_data$opponent == team_id, -1, 0) %>%
    na_if(0)
  col_all <- coalesce(col_w, col_l) %>%
    tbl_df()
  return(col_all)
}

# Replace NAs with 0 and cbind home/away column
# H=1, N=0, A=-1
game_df <- map(all_teams, ~ transform_wl(ncaa_mbb, team_id = .x)) %>%
  bind_cols() %>%
  setNames(all_teams) %>%
  replace(is.na(.), 0) %>%
  mutate(loc = fct_recode(ncaa_mbb$location, "1" = "H", "-1" = "V", "0" = "N")) %>%
  mutate(loc = as.numeric(as.character(loc))) %>%
  select(loc, everything()) %>%
  as.data.frame()

# calculate score differentials
score_diff <- ncaa_mbb %>%
  mutate(score_diff = abs(teamscore - oppscore)) %>%
  select(score_diff) 

# add score_diff and game_data as columns
game_df = cbind(game_df,score_diff)
game_df = cbind(game_df,"game_date" = ncaa_mbb$game_date)

# add row of all 1's for each team to normalize ratings
game_df = rbind(game_df,c(0,rep(1,num_teams),0,0))
