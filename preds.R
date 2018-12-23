test_data <- 
  shots_2018 %>% 
  mutate(
    homeSkaters = case_when(
      homeSkatersOnIce > 6 ~ 6,
      homeSkatersOnIce < 3 ~ 3,
      TRUE ~ as.numeric(homeSkatersOnIce)),
    awaySkaters = case_when(
      awaySkatersOnIce > 6 ~ 6,
      awaySkatersOnIce < 3 ~ 3,
      TRUE ~ as.numeric(awaySkatersOnIce)),
    score = homeTeamGoals - awayTeamGoals) %>% 
  mutate(
    strengthState = case_when(
      team == "HOME" & homeSkaters == 6 & awaySkaters == 3 ~ "6v3",
      team == "HOME" & homeSkaters == 6 & awaySkaters == 4 ~ "6v4",
      team == "HOME" & homeSkaters == 6 & awaySkaters == 5 ~ "6v5",
      team == "HOME" & homeSkaters == 5 & awaySkaters == 3 ~ "5v3",
      team == "HOME" & homeSkaters == 5 & awaySkaters == 4 ~ "5v4",
      team == "HOME" & homeSkaters == 4 & awaySkaters == 3 ~ "4v3",
      team == "HOME" & homeSkaters == 3 & awaySkaters == 6 ~ "3v6",
      team == "HOME" & homeSkaters == 4 & awaySkaters == 6 ~ "4v6",
      team == "HOME" & homeSkaters == 5 & awaySkaters == 6 ~ "5v6",
      team == "HOME" & homeSkaters == 3 & awaySkaters == 5 ~ "3v5",
      team == "HOME" & homeSkaters == 4 & awaySkaters == 5 ~ "4v5",
      team == "HOME" & homeSkaters == 3 & awaySkaters == 4 ~ "3v4",
      team == "AWAY" & awaySkaters == 6 & homeSkaters == 3 ~ "6v3",
      team == "AWAY" & awaySkaters == 6 & homeSkaters == 4 ~ "6v4",
      team == "AWAY" & awaySkaters == 6 & homeSkaters == 5 ~ "6v5",
      team == "AWAY" & awaySkaters == 5 & homeSkaters == 3 ~ "5v3",
      team == "AWAY" & awaySkaters == 5 & homeSkaters == 4 ~ "5v4",
      team == "AWAY" & awaySkaters == 4 & homeSkaters == 3 ~ "4v3",
      team == "AWAY" & awaySkaters == 3 & homeSkaters == 6 ~ "3v6",
      team == "AWAY" & awaySkaters == 4 & homeSkaters == 6 ~ "4v6",
      team == "AWAY" & awaySkaters == 5 & homeSkaters == 6 ~ "5v6",
      team == "AWAY" & awaySkaters == 3 & homeSkaters == 5 ~ "3v5",
      team == "AWAY" & awaySkaters == 4 & homeSkaters == 5 ~ "4v5",
      team == "AWAY" & awaySkaters == 3 & homeSkaters == 4 ~ "3v4",
      homeSkaters == 6 & awaySkaters == 6 ~ "6v6",
      homeSkaters == 5 & awaySkaters == 5 ~ "5v5",
      homeSkaters == 4 & awaySkaters == 4 ~ "4v4",
      homeSkaters == 3 & awaySkaters == 3 ~ "3v3"),
    scoreStrength = case_when(
      team == "HOME" & score == 1 ~ 1,
      team == "HOME" & score == 2 ~ 2,
      team == "HOME" & score > 2 ~ 3,
      team == "HOME" & score == -1 ~ -1,
      team == "HOME" & score == -2 ~ -2,
      team == "HOME" & score < -2 ~ -3,
      team == "AWAY" & score == -1 ~ 1,
      team == "AWAY" & score == -2 ~ 2,
      team == "AWAY" & score < -2 ~ 3,
      team == "AWAY" & score == 1 ~ -1,
      team == "AWAY" & score == 2 ~ -2,
      team == "AWAY" & score > 2 ~ -3,
      score == 0 ~ 0
    )) %>% 
  mutate(scoreStrength = as.factor(scoreStrength)) %>% 
  select(goal,
         shotDistance,
         shotAngleAdjusted,
         shotType,
         shotRebound,
         shotRush,
         strengthState,
         scoreStrength,
         xGoal) %>% 
  filter(
    strengthState != "3v6",
    strengthState != "6v3",  
    strengthState != "6v6" 
  )


preds <- predict(xg_model, test_data, type="response")
pred_data <- cbind(test_data, preds)
