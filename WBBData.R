# WBB PLAYER PIPELINE + PCA RATING
# Author: Caleb Ramsey

# SETUP
library(dplyr); library(tidyr); library(purrr); library(stringr)
library(readr); library(tibble); library(fs); library(wehoop)
library(lubridate); library(here); library(rlang)

# D1 labels
D1_CONF = c(
  "ACC","AAC","AEC","ASUN","A10","B12",
  "Big East","Big Sky","Big South","Big Ten","Big West",
  "CAA","CUSA","Horizon","Ivy League","MAAC","MAC","MEAC",
  "MVC","Mountain West","NEC","OVC","Pac 12","Patriot","SEC","SoCon",
  "Southland","Summit League","Sun Belt","SWAC","WAC","WCC")

# Sample-size thresholds
MIN_MINUTES = 200
MIN_GAMES   = 10

season = 2025
players_path = here("main/wcbb_players_2025_D1.csv")
out_dir = here("main/processed"); dir_create(out_dir)

# Helper functions
mode_non_na = function(x)
  {
  x = x[!is.na(x) & x != ""]; if (!length(x))(return(NA_character_)) 
  names(sort(table(x), T))[1]
  }

find_col = function(df, include_patterns, exclude_patterns = NULL)
  {
  nms = tolower(names(df))
  hit = Reduce(`|`, lapply(include_patterns, \(p) grepl(p, nms)))
  if (!is.null(exclude_patterns) && length(exclude_patterns)) hit = hit & !Reduce(`|`, lapply(exclude_patterns, \(p) grepl(p, nms)))
  idx = which(hit); if (length(idx)) names(df)[idx[1]]else NULL
  }
col_or_na = function(df, col_name) if(is.null(col_name)){rep(NA_character_, nrow(df))} else{df[[col_name]]}
pick_num = function(df, candidates, default = 0)
  {
  nm = intersect(candidates, names(df))
  if(length(nm)) suppressWarnings(as.numeric(df[[nm[1]]]))else{rep(default, nrow(df))}
  }

pick_chr = function(df, candidates, default = NA_character_)
  {
  nm = intersect(candidates, names(df))
  if(length(nm)) as.character(df[[nm[1]]])else{rep(default, nrow(df))}
  }

to_min = function(x)
  {
  if (is.numeric(x))return(as.numeric(x)); ifelse(is.na(x) | x == "", NA_real_, as.numeric(sub(":.*","",x)) + as.numeric(sub(".*:","",x))/60) 
  }

# Build 2025 season totals
players_g = wehoop::load_wbb_player_box(seasons = season); stopifnot(nrow(players_g) > 0)

players_wehoop =
  players_g %>%
  transmute(
    athlete_id = pick_chr(., c("athlete_id","player_id","athleteId","playerId")),
    player_name = pick_chr(., c("display_name","athlete_display_name","player","player_name","name")),
    team_id = pick_chr(., c("team_id","teamId")),
    team_name = pick_chr(., c("team_display_name","team_short_display_name","team_name")),
    minutes = to_min(pick_chr(., c("minutes","mins","time_played"))),
    points = pick_num(., c("points","pts")),
    rebounds = pick_num(., c("rebounds","reb_total")),
    defensive_rebounds = pick_num(., c("defensive_rebounds","defensiveRebounds","dreb")),
    offensive_rebounds = pick_num(., c("offensive_rebounds","offensiveRebounds","oreb")),
    assists = pick_num(., c("assists","ast")),
    steals = pick_num(., c("steals","stl")),
    blocks = pick_num(., c("blocks","blk")),
    turnovers = pick_num(., c("turnovers","tov")),
    personal_fouls = pick_num(., c("personal_fouls","pf")),
    field_goals_made = pick_num(., c("field_goals_made","fgm")),
    field_goals_attempted = pick_num(., c("field_goals_attempted","fga")),
    three_point_made = pick_num(., c("three_point_field_goals_made","three_points_made","three_pm","tpm")),
    three_point_attempted = pick_num(., c("three_point_field_goals_attempted","three_points_attempted","three_pa","tpa")),
    free_throws_made = pick_num(., c("free_throws_made","ftm")),
    free_throws_attempted = pick_num(., c("free_throws_attempted","fta"))
    ) %>% group_by(athlete_id, player_name, team_id, team_name) %>%
  summarise(
    across(c(minutes, points, rebounds, defensive_rebounds, offensive_rebounds,
             assists, steals, blocks, turnovers, personal_fouls,
             field_goals_made, field_goals_attempted,
             three_point_made, three_point_attempted,
             free_throws_made, free_throws_attempted), \(x) sum(x, na.rm = T)),
    games_played = dplyr::n(), .groups = "drop"
    ) %>%
  mutate(
    field_goal_percent = if_else(field_goals_attempted > 0, field_goals_made / field_goals_attempted, NA_real_),
    three_point_percent = if_else(three_point_attempted > 0, three_point_made / three_point_attempted, NA_real_),
    free_throw_percent = if_else(free_throws_attempted  > 0, free_throws_made / free_throws_attempted, NA_real_)
    ) %>% filter(minutes > 0)

write_csv(players_wehoop, players_path)

# Map teams to conferences
players = readr::read_csv(players_path, show_col_types = F)
athlete_col = intersect(c("athleteID","athlete_id","player_id"), names(players))[1]
team_col = intersect(c("team_ID","team_id","teamId"), names(players))[1]
player_name_col = intersect(c("player","player_name","name","display_name"), names(players))[1]
if(is.na(player_name_col)){player_name_col = NULL}

players_joined =
  players %>%
  mutate(!!rlang::sym(athlete_col) := as.character(.data[[athlete_col]]),
         !!rlang::sym(team_col)    := as.character(.data[[team_col]])) %>%
  {if(!"position" %in% names(.)) mutate(., position = NA_character_) else .}

sched = wehoop::load_wbb_schedule(seasons = season, season_types = "Regular Season")
pick_exact = function(df, candidates)
  {
  ln = tolower(names(df)); for(c in tolower(candidates)){
    i = match(c, ln, nomatch = 0); if(i > 0) return(names(df)[i])
    }; NULL
  }

gid_col = pick_exact(sched, c("game_id","id"))
h_col = pick_exact(sched, c("home_team_id","home_id"))
a_col = pick_exact(sched, c("away_team_id","away_id"))

h_team_col = find_col(sched, c("^home.*team.*id$","^home.*id$","^home_team_id$"))
a_team_col = find_col(sched, c("^away.*team.*id$","^away.*id$","^away_team_id$"))
h_conf_id = find_col(sched, c("^home.*conference.*id$","^home.*group.*id$"))
h_conf_name = find_col(sched, c("^home.*conference.*name$","^home.*group.*display.*name$","^home.*group.*name$"))
a_conf_id = find_col(sched, c("^away.*conference.*id$","^away.*group.*id$"))
a_conf_name = find_col(sched, c("^away.*conference.*name$","^away.*group.*display.*name$","^away.*group.*name$"))
h_team_name = find_col(sched, c("^home.*team.*display.*name$","^home.*team.*name$","^home.*short.*name$"))
a_team_name = find_col(sched, c("^away.*team.*display.*name$","^away.*team.*name$","^away.*short.*name$"))

home_obs = tibble::tibble(
  team_id = as.character(col_or_na(sched, h_team_col)),
  team_name_meta = as.character(col_or_na(sched, h_team_name)),
  conference_id = as.character(col_or_na(sched, h_conf_id)),
  conference_name = as.character(col_or_na(sched, h_conf_name))
  )

away_obs = tibble::tibble(
  team_id = as.character(col_or_na(sched, a_team_col)),
  team_name_meta = as.character(col_or_na(sched, a_team_name)),
  conference_id = as.character(col_or_na(sched, a_conf_id)),
  conference_name = as.character(col_or_na(sched, a_conf_name))
  )

teams_meta =
  dplyr::bind_rows(home_obs, away_obs) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), ~na_if(.x, ""))) %>%
  dplyr::filter(!is.na(team_id)) %>%
  dplyr::group_by(team_id) %>%
  dplyr::summarise(
    team_name_meta = mode_non_na(team_name_meta),
    conference_id  = mode_non_na(conference_id),
    conference_name = mode_non_na(conference_name),
    .groups = "drop"
    )

team_id_to_name = teams_meta %>% dplyr::select(team_id, team_name = team_name_meta) %>% dplyr::distinct()

team_conf_auto =
  players %>% dplyr::transmute(team_id = as.character(.data[[team_col]])) %>% dplyr::distinct() %>%
  dplyr::left_join(teams_meta, by = "team_id") %>%
  dplyr::left_join(team_id_to_name, by = "team_id") %>%
  dplyr::mutate(team_name = dplyr::coalesce(team_name, team_name_meta)) %>%
  dplyr::select(team_id, team_name, conference_name, conference_id)

map_file = "main/conf_name_map.txt"
if (file.exists(map_file)) {
  map_lines = readLines(map_file)
  map_df = tibble::tibble(raw = map_lines) %>%
    dplyr::mutate(
      team_id = stringr::str_match(raw, '^"([0-9]+)"')[,2],
      conf    = stringr::str_match(raw, '"\\s*=\\s*"(.*?)"')[,2]
      ) %>%
    dplyr::filter(!is.na(team_id), !is.na(conf), conf != "")
  team_conf_override = tibble::tibble(team_id = map_df$team_id, conference_name = map_df$conf, conference_id = map_df$conf)
  team_conf_auto = team_conf_auto %>%
    dplyr::left_join(team_conf_override, by = "team_id", suffix = c("_auto","_hard")) %>%
    dplyr::mutate(conference_name = dplyr::coalesce(conference_name_hard, conference_name_auto),
                  conference_id   = dplyr::coalesce(conference_id_hard,   conference_id_auto)) %>%
    dplyr::select(team_id, team_name, conference_name, conference_id)
  }

team_conf_map_final = team_conf_auto %>% dplyr::rename(team_name_map = team_name) %>%
  dplyr::select(team_id, team_name_map, conference_name, conference_id)

players_enriched =
  players_joined %>%
  dplyr::left_join(team_conf_map_final, by = setNames("team_id", team_col)) %>%
  dplyr::mutate(team_name = dplyr::coalesce(.data[["team_name"]], .data[["team_name_map"]])) %>%
  dplyr::transmute(
    athlete_id = as.character(.data[[athlete_col]]),
    player_name = if (!is.null(player_name_col)) .data[[player_name_col]] else NA_character_,
    position,
    team_id = .data[[team_col]],
    team_name,
    conference_name,
    blocks = dplyr::coalesce(.data[["blocks"]], 0),
    defensive_rebounds = dplyr::coalesce(.data[["defensive_rebounds"]], 0),
    steals = dplyr::coalesce(.data[["steals"]], 0),
    rebounds = dplyr::coalesce(.data[["rebounds"]], 0),
    minutes = dplyr::coalesce(.data[["minutes"]], 0),
    games_played = dplyr::coalesce(.data[["games_played"]], 0),
    assists = dplyr::coalesce(.data[["assists"]], 0),
    field_goals_attempted = dplyr::coalesce(.data[["field_goals_attempted"]], 0),
    field_goals_made      = dplyr::coalesce(.data[["field_goals_made"]], 0),
    field_goal_percent    = dplyr::coalesce(.data[["field_goal_percent"]], NA_real_),
    three_point_made      = dplyr::coalesce(.data[["three_point_made"]], 0),
    three_point_attempted = dplyr::coalesce(.data[["three_point_attempted"]], 0),
    three_point_percent   = dplyr::coalesce(.data[["three_point_percent"]],
                                            dplyr::if_else(dplyr::coalesce(.data[["three_point_attempted"]],0) > 0,
                                                           .data[["three_point_made"]]/.data[["three_point_attempted"]],
                                                           NA_real_)),
    free_throw_percent    = dplyr::coalesce(.data[["free_throw_percent"]], NA_real_),
    free_throws_attempted = dplyr::coalesce(.data[["free_throws_attempted"]], 0),
    free_throws_made      = dplyr::coalesce(.data[["free_throws_made"]], 0),
    offensive_rebounds = dplyr::coalesce(.data[["offensive_rebounds"]], 0),
    points = dplyr::coalesce(.data[["points"]], 0),
    turnovers        = dplyr::coalesce(.data[["turnovers"]], 0),
    personal_fouls   = dplyr::coalesce(.data[["personal_fouls"]], 0)
    ) %>%
  dplyr::arrange(dplyr::desc(minutes), dplyr::desc(games_played)) %>%
  dplyr::filter(conference_name %in% D1_CONF)

readr::write_csv(players_enriched, file.path(out_dir, "wcbb_players_2025_with_team_conf_pos.csv"))

