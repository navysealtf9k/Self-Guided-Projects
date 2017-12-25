#Data Initialization
library(dplyr)
library(tidyverse)
library(forcats)
library(ggplot2)
library(sjPlot)
library(car)
library(zoo)
library(dummies)
setwd('/Users/abdullah/Desktop/Stat 538A/ipl-3')
ball_data <- tbl_df(read.csv(file = 'deliveries.csv'))
match_data <- tbl_df(read.csv(file = 'matches.csv'))
match_data <- na.omit(match_data[,1:17])   #checks and removes NA's
match_data$season <- factor(match_data$season)   #Make seasons a factor
match_data$win_by_runs <- factor(match_data$win_by_runs)  #Make win by runs a factor
match_data$win_by_wickets <- factor(match_data$win_by_wickets)  #Make win by wickets a factor
match_data <- match_data %>% arrange(season)  #Arrange data by season

#Focus on RCB
rcb <- filter(match_data, as.character(team1) == "Royal Challengers Bangalore" | as.character(team2) == "Royal Challengers Bangalore")

#Batting trends
batting_data <- filter(ball_data, batting_team == "Royal Challengers Bangalore")

#Bowling trends
bowling_data <- filter(ball_data, bowling_team == "Royal Challengers Bangalore")
bowlers <- count(bowling_data, bowler, sort = TRUE)

#Focusing on RCB
rcb_ball_data <- filter(ball_data, batting_team == "Royal Challengers Bangalore" | bowling_team == "Royal Challengers Bangalore")
rcb_ball_data <- group_by(rcb_ball_data, match_id)

#Analyse games won
wins <- group_by(rcb, winner == "Royal Challengers Bangalore")
wins <- filter(wins, `winner == "Royal Challengers Bangalore"`== TRUE)
wins <- mutate(wins, match_id = id)
wins_1 <- left_join(wins, batting_data) #identify games rcb won
wins_1 <- select(wins_1, season, match_id, inning, batting_team, bowling_team, over, ball, batsman, non_striker, bowler, is_super_over, wide_runs, bye_runs, legbye_runs, noball_runs, penalty_runs, batsman_runs, extra_runs, total_runs, player_dismissed, dismissal_kind, fielder)

#How many runs did each batsmen score in a game?
batsmen <- group_by(wins_1,batsman) #`runs scored by each batrsman in a game
batsmen <- count(wins_1, season, match_id, batsman, batsman_runs) #it works :)

#Runs scored
bat_score <- group_by(batting_data, batsman, match_id, batsman_runs)
bat_score <- count(bat_score, batsman_runs)  #How many different kinds of runs did each player score in every game played 

#How did Kallis perform in games RCB won?
bat_score_kallis <- filter(bat_score, batsman == 'JH Kallis') #Games kallis batted in
matches_played <- inner_join(wins, bat_score_kallis) #Games Kallis batted in and won
player_of_match_kallis <- filter(rcb, player_of_match == "JH Kallis") #Games kallis won the award in

#Look at win record against those teams over time
opponent <- select(rcb, season, team1, team2, winner)
opponent <- filter(opponent, winner == "Royal Challengers Bangalore")
opponent$team1 <- as.character(opponent$team1)
opponent$team2 <- as.character(opponent$team2)
opponent$winner <- as.character(opponent$winner)

loosing_team <- c()

for (i in 1 : nrow(opponent)) {
  if(opponent$team1[i] == "Royal Challengers Bangalore") {
    loosing_team[i] <- opponent$team2[i]
  }
  else {
    loosing_team[i] <- opponent$team1[i]
  }
  print(loosing_team[i])
}
loosing_team <- factor(loosing_team)
opponent <- mutate(opponent, team_name = loosing_team)
opponent_1 <- select(opponent, season, team_name)
opponent_1 <- group_by(opponent, season, team_name)
opponent_1 <- count(opponent_1, season, team_name) #count of wins against a specific team
names(opponent_1) <- c('team_name', 'season', 'wins')

#Teams they lost the most against
defeat <- select(rcb, season, team1, team2, winner)
defeat <- filter(defeat, winner != "Royal Challengers Bangalore")
defeat$team1 <- as.character(defeat$team1)
defeat$team2 <- as.character(defeat$team2)
defeat$winner <- as.character(defeat$winner)

defeated_by <- c()

for (i in 1 : nrow(defeat)) {
  if(defeat$team1[i] == "Royal Challengers Bangalore") {
    defeated_by[i] <- defeat$team2[i]
  }
  else {
    defeated_by[i] <- defeat$team1[i]
  }
  print(defeated_by[i])
}
defeated_by <- factor(defeated_by)
defeat <- mutate(defeat, team_name = defeated_by)
defeat_1 <- select(defeat, season,team_name) 
defeat_1 <- group_by(defeat_1, team_name)
defeat_1 <- count(defeat_1, season, team_name) #count of losses against a specific team
names(defeat_1) <- c('team_name', 'season', 'losses')

#Opposition win loss record breakdown
opposing_team <- full_join(opponent_1, defeat_1) 
opposing_team[is.na(opposing_team)] <- 0  #replace NA values with 0's. 
opposing_team <- group_by(opposing_team, team_name)

#How did rcb perform against teams Kallis won player of the match against
teams_beat <- unique(as.character(player_of_match_kallis$team1)) #Which teams did Kallis win the award against
seasons_beat <- unique(as.character(player_of_match_kallis$season)) #Which seasons did he win the award in
kallis_record <- filter(opposing_team, as.character(team_name) == "Mumbai Indians" | as.character(team_name) == "Delhi Daredevils" | as.character(team_name) == "Kings XI Punjab" | as.character(team_name) == "Rajasthan Royals" | as.character(team_name) == "Kolkata Knight Riders")
kallis_record <- filter(kallis_record, as.character(season) == '2009'| as.character(season) == '2010' | as.character(season) == '2013')
kallis_record <- kallis_record[order(kallis_record$team_name, kallis_record$season), ] #Sorting by team and season
