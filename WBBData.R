# Setup
suppressPackageStartupMessages(
  {
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(wehoop); library(readr); library(tibble); library(here); library(fs)
  })

season = 2025

# Paths
players_path = here("data/wcbb_players_2025.csv")
map_path_txt = here("data/conf_name_map.txt")
out_dir = here("data/processed")
dir_create(out_dir)

# Load players
if (!exists("players"))
  {
  players <- readr::read_csv(players_path, show_col_types = FALSE)  # CHANGE
  }

athlete_col = intersect(c("athleteID","athlete_id","player_id"), names(players))[1]
team_col = intersect(c("team_ID","team_id","teamId"), names(players))[1]
stopifnot(!is.na(athlete_col), !is.na(team_col))

player_name_col = intersect(c("player","player_name","name"), names(players))[1]
if (is.na(player_name_col)) player_name_col = NULL

# Keep original position column if present
if (!exists("players_joined"))
  {
  players_joined = players %>%
    mutate(
      !!rlang::sym(athlete_col) := as.character(.data[[athlete_col]]),
      !!rlang::sym(team_col)    := as.character(.data[[team_col]])
      )
  if (!"position" %in% names(players_joined)) players_joined$position = NA_character_
  }

# Function that finds the statistical mode of non-NA strings.
mode_non_na = function(x)
  {
  x = x[!is.na(x) & x != ""]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
  }

# Function that finds a column by regex patterns (first match).
find_col = function(df, include_patterns, exclude_patterns = NULL)
  {
  nms = tolower(names(df))
  hit = Reduce(`|`, lapply(include_patterns, function(p) grepl(p, nms)))
  if (!is.null(exclude_patterns) && length(exclude_patterns))
    hit = hit & !Reduce(`|`, lapply(exclude_patterns, function(p) grepl(p, nms)))
  idx = which(hit)
  if (length(idx)) names(df)[idx[1]] else NULL
  }

# Function that returns a column or NA vector if missing.
col_or_na = function(df, col_name)
  {
  if (is.null(col_name)) return(rep(NA_character_, nrow(df)))
  df[[col_name]]
  }

# Build team: conference via schedule
sched = wehoop::load_wbb_schedule(seasons = season, season_types = "Regular Season")

h_team_col = find_col(sched, c("^home.*team.*id$", "^home.*id$", "^home_team_id$"))
a_team_col = find_col(sched, c("^away.*team.*id$", "^away.*id$", "^away_team_id$"))

h_conf_id = find_col(sched, c("^home.*conference.*id$", "^home.*group.*id$"))
h_conf_name = find_col(sched, c("^home.*conference.*name$", "^home.*group.*display.*name$", "^home.*group.*name$"))

a_conf_id = find_col(sched, c("^away.*conference.*id$", "^away.*group.*id$"))
a_conf_name = find_col(sched, c("^away.*conference.*name$", "^away.*group.*display.*name$", "^away.*group.*name$"))

h_team_name = find_col(sched, c("^home.*team.*display.*name$", "^home.*team.*name$", "^home.*short.*name$"))
a_team_name = find_col(sched, c("^away.*team.*display.*name$", "^away.*team.*name$", "^away.*short.*name$"))

home_obs = tibble(
  team_id = as.character(col_or_na(sched, h_team_col)),
  team_name_meta = as.character(col_or_na(sched, h_team_name)),
  conference_id = as.character(col_or_na(sched, h_conf_id)),
  conference_name= as.character(col_or_na(sched, h_conf_name))
  )

away_obs = tibble(
  team_id = as.character(col_or_na(sched, a_team_col)),
  team_name_meta = as.character(col_or_na(sched, a_team_name)),
  conference_id = as.character(col_or_na(sched, a_conf_id)),
  conference_name= as.character(col_or_na(sched, a_conf_name))
  )

teams_meta = bind_rows(home_obs, away_obs) %>%
  mutate(across(everything(), ~na_if(.x, ""))) %>%
  filter(!is.na(team_id)) %>%
  group_by(team_id) %>%
  summarise(
    team_name_meta  = mode_non_na(team_name_meta),
    conference_id   = mode_non_na(conference_id),
    conference_name = mode_non_na(conference_name),
    .groups = "drop"
    )

if(!exists("team_id_to_name"))
  {
  team_id_to_name = teams_meta %>% select(team_id, team_name = team_name_meta) %>% distinct()
  }

teams_needed = players %>%
  transmute(team_id = as.character(.data[[team_col]])) %>%
  distinct()

team_conf_auto = teams_needed %>%
  left_join(teams_meta, by = "team_id") %>%
  left_join(team_id_to_name, by = "team_id") %>%
  mutate(team_name = coalesce(team_name, team_name_meta)) %>%
  select(team_id, team_name, conference_name, conference_id)

# Mapping conference to teams
stopifnot(file.exists(map_path_txt))
map_lines = readLines(map_path_txt)

map_df = tibble(raw = map_lines) %>%
  tidyr::replace_na(list(raw = "")) %>%
  mutate(
    team_id = stringr::str_match(raw, '^"([0-9]+)"')[,2],
    conf    = stringr::str_match(raw, '"\\s*=\\s*"(.*?)"')[,2]
    ) %>%
  filter(!is.na(team_id), !is.na(conf), conf != "")

conf_name_map = stats::setNames(map_df$conf, map_df$team_id)
team_conf_override = tibble(
  team_id = names(conf_name_map),
  conference_name = unname(conf_name_map),
  conference_id   = unname(conf_name_map)
  )

# Final conference map
team_conf_map_final = team_conf_auto %>%
  left_join(team_conf_override, by = "team_id", suffix = c("_auto","_hard")) %>%
  mutate(
    conference_name = coalesce(conference_name_hard, conference_name_auto),
    conference_id   = coalesce(conference_id_hard,   conference_id_auto)
    ) %>%
  select(team_id, team_name, conference_name, conference_id) %>%
  arrange(team_name, team_id)

# Join back to players
players_enriched = players_joined %>%
  left_join(team_conf_map_final, by = setNames("team_id", team_col)) %>%
  transmute(
    athlete_id = as.character(.data[[athlete_col]]),
    player_name = if (!is.null(player_name_col)) .data[[player_name_col]] else NA_character_,
    position,
    team_id = .data[[team_col]],
    team_name,
    conference_name,
    conference_id,
    blocks = coalesce(.data[["blocks"]], 0),
    defensive_rebounds = coalesce(.data[["defensiveRebounds"]], 0),
    steals = coalesce(.data[["steals"]], 0),
    defensive_rebounds_per_game = coalesce(.data[["avgDefensiveRebounds"]], NA_real_),
    blocks_per_game = coalesce(.data[["avgBlocks"]], NA_real_),
    steals_per_game = coalesce(.data[["avgSteals"]], NA_real_),
    personal_fouls = coalesce(.data[["fouls"]], NA_real_),
    rebounds = coalesce(.data[["rebounds"]], 0),
    minutes = coalesce(.data[["minutes"]], 0),
    minutes_per_game = coalesce(.data[["avgMinutes"]], NA_real_),
    rebounds_per_game = coalesce(.data[["avgRebounds"]], NA_real_),
    assist_turnover_ratio = coalesce(.data[["assistTurnoverRatio"]], NA_real_),
    games_played = coalesce(.data[["gamesPlayed"]], 0),
    games_started = coalesce(.data[["gamesStarted"]], NA_real_),
    assists = coalesce(.data[["assists"]], 0),
    field_goals_attempted = coalesce(.data[["fieldGoalsAttempted"]], 0),
    field_goals_made = coalesce(.data[["fieldGoalsMade"]], 0),
    field_goal_percent = coalesce(.data[["fieldGoalPct"]], NA_real_),
    free_throw_percent = coalesce(.data[["freeThrowPct"]], NA_real_),
    free_throws_attempted = coalesce(.data[["freeThrowsAttempted"]], 0),
    free_throws_made = coalesce(.data[["freeThrowsMade"]], 0),
    offensive_rebounds = coalesce(.data[["offensiveRebounds"]], 0),
    points = coalesce(.data[["points"]], 0)
    ) %>% arrange(desc(minutes), desc(games_played))

# D1-only cut and saves
players_d1 = players_enriched %>% filter(!is.na(conference_name))

readr::write_csv(players_enriched, file.path(out_dir, "wcbb_players_2025_with_team_conf_pos.csv"))

readr::write_csv(players_d1, file.path(out_dir, "wcbb_players_2025_D1.csv"))
