library(dplyr)
library(purrr)
library(forcats)
library(readr)

### After running step1_setup.R, run this file to produce the ratings

# CALCULATE WEIGHTS BASED ON GAME DATE
first_wt = .7 # this is how much weight the first games have
first_date = min(head(game_df,-1)$game_date) # earliest game in the season
last_date = max(head(game_df,-1)$game_date)
num_days = last_date - first_date
wts = first_wt^((last_date - game_df$game_date)/num_days)

# VIEWING WEIGHTS
# x = seq(0,num_days)
# y = first_wt^((num_days-x)/num_days)
# plot(x,y)


# This is my max score differential for the games
max_pt_diff = 36

# NOW RUN MODELS
# without capping score differential, irregardless of date
#my_lm = lm(score_diff~.+0, data=game_df[,c(1:num_teams+2)]) # do not include game_date as predictor var

# capping score differential, weighting for date
score_diff_capped = with(game_df,ifelse(score_diff>max_pt_diff,max_pt_diff, score_diff))
my_lm = lm(score_diff_capped~.+0, data=game_df[,c(1:(num_teams+1))], weights=wts)

# SHOW TEAM RATINGS
coef(my_lm)
sort(coef(my_lm))

team_ratings = data.frame(all_teams,as.vector(coef(my_lm)[-1]))
colnames(team_ratings) = c('team','rating')

### WRITE MEN'S FILE
#write.csv(team_ratings, "mbb_ratings.csv", row.names=F, quote=F)
