---
title: "Player Rating Assignment"
output: html_document
date: "2025-10-23"
---

Load necessary libraries:

```{R}
library(dplyr)
library(stringr)
library(cbbdata)
library(randomForest)
library(caret)
```

Read in and format the 2023 HDI Database:

```{R}
hdi23 <- read.csv("C:/Users/bwdea/OneDrive/Documents/STAT 3274/Final Project/Data/HDI Ratings 2023-24.csv")
hdi23 <- hdi23 %>% rename(Name = Full.Name, Team = X2023.2024.School) %>% select(Name, Team, Rating)
hdi23
```

Get the barttorivk 2023 Database:

```{R}
barttorvik_2023 <- cbd_torvik_player_season(year = 2024)
barttorvik_2023 <- barttorvik_2023 %>% rename(Name = player, Team = team)
barttorvik_2023
```

Filter and assign the correct HDI ratingt to all Torvik players for the 2023 season:

```{r}
source("hdi_name_matcher.R")

ratings_23 <- join_hdi_rating(barttorvik_2023, hdi23) %>% select(Name, pos, exp, hgt, Team, conf, mpg, ppg, rpg, apg, spg, bpg, two_pct, three_pct, ft_pct, tov, ast_to, efg, ts, usg, ortg, drtg, rim_pct, mid_pct, obpm, dbpm, bpm, ftr, pfr, HDI_Rating, year) %>% rename(Rating = HDI_Rating) %>% filter(!is.na(Rating)) #24 umatched
basic_ratings_23 <- ratings_23 %>% select(Name, pos, exp, hgt, Team, conf, mpg, ppg, rpg, apg, spg, bpg, two_pct, three_pct, ft_pct, tov, ast_to, efg, ts, Rating, year)
write.csv(ratings_23, "barttorvik_23.csv", row.names = FALSE)
basic_ratings_23
```

Read in and format the 2024 HDI Database:

```{R}
hdi24 <- read.csv("C:/Users/bwdea/OneDrive/Documents/STAT 3274/Final Project/Data/HDI Ratings 2024-25.csv")
hdi24 <- hdi24 %>% rename(Name = Full.Name, Team = X2024.2025.School) %>% select(Name, Team, Rating)
hdi24
```

Get the barttorivk 2024 Database:

```{R}
barttorvik_2024 <- cbd_torvik_player_season(year = 2025)
barttorvik_2024 <- barttorvik_2024 %>% rename(Name = player, Team = team)
barttorvik_2024
```

Filter and assign the correct HDI ratingt to all Torvik players for the 2024 season:

```{r}
ratings_24 <- join_hdi_rating(barttorvik_2024, hdi24) %>% select(Name, pos, exp, hgt, Team, conf, mpg, ppg, rpg, apg, spg, bpg, two_pct, three_pct, ft_pct, tov, ast_to, efg, ts, usg, ortg, drtg, rim_pct, mid_pct, obpm, dbpm, bpm, ftr, pfr, HDI_Rating, year) %>% rename(Rating = HDI_Rating) %>% filter(!is.na(Rating)) #153 Umatched
basic_ratings_24 <- ratings_24 %>% select(Name, pos, exp, hgt, Team, conf, mpg, ppg, rpg, apg, spg, bpg, two_pct, three_pct, ft_pct, tov, ast_to, efg, ts, Rating, year)
write.csv(ratings_24, "barttorvik_24.csv", row.names = FALSE)
basic_ratings_24 
```

Combine the 2023 and 2024 datasets for training of random forest model:

```{R}
ratings_23 <- ratings_23 %>% mutate(id = paste(Name, Team, year, sep = '_'))
ratings_24 <- ratings_24 %>% mutate(id = paste(Name, Team, year, sep = '_'))
ratings <- rbind(ratings_23, ratings_24)
```

Run and test the random forest model:

```{R}
ratings_model <- ratings %>% mutate(pos = as.factor(pos), exp = as.factor(exp), hgt = as.factor(hgt)) %>% select(pos, exp, hgt, mpg, ppg, rpg, apg, spg, bpg, two_pct, three_pct, ft_pct, tov, ast_to, efg, ts, usg, ortg, drtg, rim_pct, mid_pct, obpm, dbpm, bpm, ftr, pfr, Rating) %>% na.omit() #8089 rows left

set.seed(123)
train_index <- createDataPartition(ratings_model$Rating, p = 0.8, list = FALSE)
train_data <- ratings_model[train_index, ]
test_data <- ratings_model[-train_index, ]
model <- randomForest(Rating ~ ., data = train_data, ntree = 500, importance = TRUE)

print(model)

importance(model)

predictions <- predict(model, newdata = test_data)

actual <- test_data$Rating
rmse <- sqrt(mean((predictions - actual)^2))
r_squared <- cor(predictions, actual)^2

cat(" RMSE:", rmse, "\n", "R-squared:", r_squared, "\n")
```
