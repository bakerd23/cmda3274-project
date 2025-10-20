library(rvest)
library(dplyr)
library(stringr)
library(hoopR)
library(ncaahoopR)
library(googledrive)
library(googlesheets4)

update_mins_wp_sheet <- function(WINNING_PERCENTAGE, MIN_MINUTES, MAX_MINUTES, MIN_GAMES, rewrite){
  mbb_teams_espn <- espn_mbb_teams(year=most_recent_mbb_season())
  mbb_teams <- mbb_teams_espn %>% select(display_name, conference_short_name, mascot, team_id)
  extra_teams <- as.data.frame(matrix(nrow = 3, ncol = 4))
  colnames(extra_teams) <- colnames(mbb_teams)
  extra_teams$display_name <- c("Lindenwood Lions","Queens University Royals", "Southern Indiana Screaming Eagles")
  extra_teams$conference_short_name <- c("OVC","ASUN","OVC")
  extra_teams$mascot <- c("Lions", "Royals", "Screaming Eagles")
  extra_teams$team_id <- c(2815,2511,88)
  mbb_teams <- rbind(mbb_teams, extra_teams)
  mbb_teams$Teamnm <- ""
  for (i in 1:length(mbb_teams$mascot)) {  
    mascot_length <- nchar(mbb_teams$mascot[i])
    display_length <- nchar(mbb_teams$display_name[i])
    mbb_teams$Teamnm[i] <- str_sub(mbb_teams$display_name[i], 1, display_length - mascot_length)
  }
  mbb_teams <- mbb_teams %>% select(display_name, Teamnm, conference_short_name, team_id)
  mbb_teams <- mbb_teams %>% rename(Team = display_name, Conference = conference_short_name, ID = team_id)
  power5_conferences <- c("ACC", "Big East", "Big Ten", "Big 12", "SEC")
  espn_teams_url <- read_html("https://www.espn.com/mens-college-basketball/standings")
  espn_teams_tibble <- espn_teams_url %>% html_nodes("table") %>% html_table(fill=TRUE)
  espn_teams <- data.frame(matrix(nrow=nrow(espn_teams_tibble[[1]]) - 1, ncol=ncol(espn_teams_tibble[[2]]) + 1))
  for (i in seq(1, 61, by = 2)) {
    conference <- data.frame(matrix(nrow=nrow(espn_teams_tibble[[i]]) - 1, ncol=ncol(espn_teams_tibble[[i + 1]]) + 1))
    conference_teams <- as.data.frame(espn_teams_tibble[[i]][-1, ])
    colnames(conference_teams) <- c("Team")
    conference_stats <- as.data.frame(espn_teams_tibble[[i + 1]][-1, ])
    colnames(conference_stats) <- espn_teams_tibble[[i + 1]][1, , drop = FALSE]
    colnames(conference_stats)[colnames(conference_stats) == "W-L"][1] <- "CONFW-L"
    colnames(conference_stats)[colnames(conference_stats) == "PCT"][1] <- "CONFPCT"
    conference <- cbind(conference_teams, conference_stats)
    if (i == 1){espn_teams <- conference}
    else {espn_teams <- rbind(espn_teams, conference)}
  }
  lowercase <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')
  for (i in 1:length(espn_teams$Team)) {
    str <- espn_teams$Team[i]
    if (grepl("^[0-9]+", str)) {
      str <- str_sub(str, regexpr("[A-Za-z]", str), nchar(str))
      espn_teams$Team[i] <- str
    }
    if (str_sub(str, 3, 3) %in% lowercase){espn_teams$Team[i] <- str_sub(str, 2, nchar(str))}
    else if (str_sub(str, 4, 4) %in% lowercase){espn_teams$Team[i] <- str_sub(str, 3, nchar(str))}
    else if (str_sub(str, 5, 5) %in% lowercase){espn_teams$Team[i] <- str_sub(str, 4, nchar(str))}
    else if (str_sub(str, 6, 6) %in% lowercase){espn_teams$Team[i] <- str_sub(str, 5, nchar(str))}
    else if (str_sub(str, 1, 3) == str_sub(str, 4, 6)){espn_teams$Team[i] <- str_sub(str, 4, nchar(str))}
    else if (str_sub(str, 1, 4) == str_sub(str, 5, 8)){espn_teams$Team[i] <- str_sub(str, 5, nchar(str))}
    else if (str_sub(str, 1, 2) == str_sub(str, 5, 6)){espn_teams$Team[i] <- str_sub(str, 5, nchar(str))}
    else if (str_sub(str, 1, 2) == str_sub(str, 4, 5)){espn_teams$Team[i] <- str_sub(str, 4, nchar(str))}
    if (espn_teams$Team[i] == "CONNUConn Huskies"){espn_teams$Team[i] <- "UConn Huskies"}
    if (espn_teams$Team[i] == "RGVUT Rio Grande Valley Vaqueros"){espn_teams$Team[i] <- "UT Rio Grande Valley Vaqueros"}
    if (espn_teams$Team[i] == "Mass Lowell River Hawks"){espn_teams$Team[i] <- "UMass Lowell River Hawks"}
    if (espn_teams$Team[i] == "Albany Great Danes"){espn_teams$Team[i] <- "UAlbany Great Danes"}
  }
  espn_teams <- merge(espn_teams, mbb_teams, by = "Team", all.x = TRUE)
  espn_teams$PCT <- as.numeric(espn_teams$PCT)
  espn_teams <- espn_teams %>% select(Teamnm, Conference, everything(), -Team)
  espn_teams <- espn_teams %>% rename(Team = Teamnm)
  espn_teams_WP <- espn_teams %>%
    select(Team, Conference, `W-L`, PCT, ID)
  espn_teams_WP <- espn_teams_WP[order(espn_teams_WP$PCT, decreasing=TRUE), ]
  good_teams <- espn_teams_WP[espn_teams_WP$PCT >= WINNING_PERCENTAGE,]
  good_teams <- good_teams[good_teams$Conference %in% power5_conferences,]
  for (i in 1:length(good_teams$Team)) {
    team_id <- good_teams$ID[i]
    team_stats_url <- read_html(paste0("https://www.espn.com/mens-college-basketball/team/stats/_/id/", as.character(team_id)))
    team_stats_tibble <- team_stats_url %>% html_nodes("table") %>% html_table(fill=TRUE)
    team_stats <- as.data.frame(team_stats_tibble[[2]])
    team_players <- as.data.frame(team_stats_tibble[[1]])
    team_stats$Name <- team_players$Name
    team_stats <- team_stats[team_stats$Name != "Total", ]
    team_stats$POS <- substr(team_stats$Name, nchar(team_stats$Name), nchar(team_stats$Name))
    team_stats$Name <- substr(team_stats$Name, 1, nchar(team_stats$Name) - 2)
    team_stats$School <- substr(mbb_teams$Teamnm[mbb_teams$ID == team_id], 1, nchar(mbb_teams$Teamnm[mbb_teams$ID == team_id]) - 1)
    team_stats$`Team Win%` <- espn_teams$PCT[espn_teams$ID == team_id]
    team_stats$`FG%` <- team_stats$`FG%` / 100
    team_stats$`3P%` <- team_stats$`3P%` / 100
    team_stats$`FT%` <- team_stats$`FT%` / 100
    team_stats <- team_stats %>% select(Name, POS, School, everything(), `3P%`, `FT%`, `Team Win%`)
    if (i == 1) {good_teams_stats <- team_stats} 
    else {good_teams_stats <- bind_rows(good_teams_stats, team_stats)}
  }
  desired_players <- good_teams_stats[good_teams_stats$MIN >= MIN_MINUTES & good_teams_stats$MIN <= MAX_MINUTES, ]
  desired_players <- desired_players[good_teams_stats$GP >= MIN_GAMES, ]
  desired_players <- desired_players %>% filter(!is.na(Name))
  desired_players <- desired_players[order(desired_players$MIN, decreasing=TRUE), ]
  if (rewrite == TRUE){
    range_clear("1yUwlxO1juIFZs00KSLkJG_q7OjHFiFXrbsoTICmlyEo", sheet = "ESPN By Player")
    range_write(data = desired_players, "1yUwlxO1juIFZs00KSLkJG_q7OjHFiFXrbsoTICmlyEo", sheet = "ESPN By Player2")
  }
  desired_players <- desired_players[order(desired_players$School), ]
  desired_players <- desired_players[order(desired_players$`Team Win%`, decreasing=TRUE), ]
  if (rewrite == TRUE){
    range_clear("1yUwlxO1juIFZs00KSLkJG_q7OjHFiFXrbsoTICmlyEo", sheet = "ESPN By Team")
    range_write(data = desired_players, "1yUwlxO1juIFZs00KSLkJG_q7OjHFiFXrbsoTICmlyEo", sheet = "ESPN By Team")
  }
  return(desired_players)
}

update_mins_wp_sheet(WINNING_PERCENTAGE=0.000, MIN_MINUTES=8, MAX_MINUTES=20, MIN_GAMES = 0, rewrite = TRUE)
