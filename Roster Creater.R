library(dplyr)
library(stringr)
library(tidyr)
library(cbbdata)
library(randomForest)
library(caret)

#Create 2023 Data
hdi23 <- read.csv("C:/Users/bwdea/OneDrive/Documents/STAT 3274/Final Project/Data/HDI Ratings 2023-24.csv")
hdi23 <- hdi23 %>% rename(Name = Full.Name, Team = X2023.2024.School) %>% select(Name, Team, Rating)

barttorvik_2023 <- cbd_torvik_player_season(year = 2024)
barttorvik_2023 <- barttorvik_2023 %>% rename(Name = player, Team = team)

source("C:/Users/bwdea/OneDrive/Documents/STAT 3274/Final Project/hdi_name_matcher.R")
ratings_23 <- join_hdi_rating(barttorvik_2023, hdi23) %>% select(Name, pos, exp, hgt, Team, conf, mpg, ppg, rpg, apg, spg, bpg, two_pct, three_pct, ft_pct, tov, ast_to, efg, ts, usg, ortg, drtg, rim_pct, mid_pct, obpm, dbpm, bpm, ftr, pfr, HDI_Rating, year) %>%
  rename(Rating = HDI_Rating) %>% filter(!is.na(Rating)) #24 umatched

#Create 2024 Data
hdi24 <- read.csv("C:/Users/bwdea/OneDrive/Documents/STAT 3274/Final Project/Data/HDI Ratings 2024-25.csv")
hdi24 <- hdi24 %>% rename(Name = Full.Name, Team = X2024.2025.School) %>% select(Name, Team, Rating)

barttorvik_2024 <- cbd_torvik_player_season(year = 2025)
barttorvik_2024 <- barttorvik_2024 %>% rename(Name = player, Team = team)

ratings_24 <- join_hdi_rating(barttorvik_2024, hdi24) %>% select(Name, pos, exp, hgt, Team, conf, mpg, ppg, rpg, apg, spg, bpg, two_pct, three_pct, ft_pct, tov, ast_to, efg, ts, usg, ortg, drtg, rim_pct, mid_pct, obpm, dbpm, bpm, ftr, pfr, HDI_Rating, year) %>%
  rename(Rating = HDI_Rating) %>% filter(!is.na(Rating)) #153 Umatched

#Men's data
men_model <- ratings_24 %>% select(Name, Team, mpg, ppg, rpg, apg, spg, bpg, two_pct, three_pct, ft_pct, tov, ast_to, efg, ts, ftr, pfr) %>% na.omit

#Women's data
women <- read.csv("C:/Users/bwdea/OneDrive/Documents/STAT 3274/Final Project/wcbb_players_2025_D1.csv")
women_model <- women %>% mutate(
  mpg = minutes / games_played,
  ppg = points / games_played,
  rpg = rebounds / games_played,
  apg = assists / games_played,
  spg = steals / games_played,
  bpg = blocks / games_played,
  tov = turnovers / games_played,
  two_fg_made = field_goals_made - three_point_made,
  two_fg_att  = field_goals_attempted - three_point_attempted,
  two_pct = ifelse(two_fg_att > 0, two_fg_made / two_fg_att, NA_real_),
  ast_to = ifelse(turnovers > 0, assists / turnovers, NA_real_),
  efg = ifelse(field_goals_attempted > 0, (field_goals_made + 0.5 * three_point_made) / field_goals_attempted, NA_real_),
  ts = ifelse(field_goals_attempted + 0.44 * free_throws_attempted > 0, points / (2 * (field_goals_attempted + 0.44 * free_throws_attempted)), NA_real_),
  ftr = ifelse(field_goals_attempted > 0, free_throws_attempted / field_goals_attempted, NA_real_),
  pfr = personal_fouls / games_played) %>% select(
    player_name, team_name, mpg, ppg, rpg, apg, spg, bpg, two_pct, three_point_percent, free_throw_percent, tov, ast_to, efg, ts, ftr, pfr) %>% rename(
      three_pct = three_point_percent,
      ft_pct = free_throw_percent) %>% na.omit()

#Ratings data for training
ratings <- rbind(ratings_23, ratings_24)
ratings_model_data <- ratings %>% select(mpg, ppg, rpg, apg, spg, bpg, two_pct, three_pct, ft_pct, tov, ast_to, efg, ts, ftr, pfr, Rating) %>% na.omit()

#Split data for training and testing and create model
set.seed(3274)
index <- sample(nrow(ratings_model_data), size = floor(0.8*nrow(ratings_model_data)))
train <- ratings_model_data[index, ]
test  <- ratings_model_data[-index, ]

ratings_model <- randomForest(Rating ~ mpg + ppg + rpg + apg + spg + bpg + two_pct + three_pct + ft_pct + tov + ast_to + efg + ts + ftr + pfr,
                                  data = train, ntree = 1000, importance = TRUE)

print(ratings_model)
importance(ratings_model)
predictions <- predict(ratings_model, newdata = test)
actual <- test$Rating
rmse <- sqrt(mean((predictions - actual)^2))
r_squared <- cor(predictions, actual)^2
cat(" RMSE:", rmse, "\n", "R-squared:", r_squared, "\n")

#Assign ratings
men_model$predicted_rating <- as.numeric(predict(ratings_model, newdata = men_model))
men_model$scaled_rating <- round(40 + (men_model$predicted_rating - min(men_model$predicted_rating)) * (59 / (max(men_model$predicted_rating) - min(men_model$predicted_rating))),1)
men_ratings <- men_model %>% select(Name, Team, scaled_rating) %>% rename(School = Team, Rating = scaled_rating) %>% arrange(desc(Rating))
men_ratings
#write.csv(men_ratings, "men_4099.csv", row.names = FALSE)

women_model$predicted_rating <- as.numeric(predict(ratings_model, newdata = women_model))
women_model$scaled_rating <- round(40 + (women_model$predicted_rating - min(women_model$predicted_rating)) * (59 / (max(women_model$predicted_rating) - min(women_model$predicted_rating))),1)
women_ratings <- women_model %>% select(player_name, team_name, scaled_rating) %>% rename(Name = player_name, School = team_name, Rating = scaled_rating) %>% arrange(desc(Rating))
women_ratings
#write.csv(women_ratings, "women_4099.csv", row.names = FALSE)
