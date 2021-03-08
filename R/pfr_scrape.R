# credit to Anthony Reinhard
# https://github.com/ajreinhard/NFL-public/blob/main/pfr/pfr%20pbp%20scrape.R

# note that '199711300phi' is broken

library(nflfastR)
library(rvest)
library(tidyverse)
library(magrittr)

# get pfr game ids to find the pages to scrape
get_pfr_game_ids <- function(year){
  
  cleaned_names <- c(
    "week",
    "day",
    "date",
    "time",
    "winner_tie",
    "at",
    "loser_tie",
    "boxscore",
    "pts_win",
    "pts_lose",
    "yds_win",
    "to_win",
    "yds_lose",
    "to_lose"
  )
  
  raw_url <- glue::glue("https://www.pro-football-reference.com/years/{year}/games.htm")
  
  raw_html <- read_html(raw_url)
  
  get_boxscore <- function(row_id){
    
    raw_html %>% 
      html_node("#games") %>% 
      html_node("tbody") %>% 
      html_node(glue::glue("tr:nth-child({row_id})")) %>% 
      html_node("td:nth-child(8) > a") %>% 
      html_attr("href") %>% 
      str_remove("/boxscores/") %>% 
      str_remove("\\.htm")
    
  }
  
  raw_table <- raw_html %>% 
    html_node("#games") %>% 
    html_table() %>% 
    set_names(nm = cleaned_names) %>% 
    mutate(row_id = row_number()) %>% 
    filter(week != "Week" & week != "") %>% 
    mutate(boxscore = map_chr(row_id, get_boxscore))
  
  tibble(raw_table)
  
}


# function to get the pfr page
get_pfr_pbp <- function(game_id) {
  
  if(game_id == '199711300phi') {return(NULL)}
  
  game_html <- read_html(paste0('https://www.pro-football-reference.com/boxscores/', game_id, '.htm'))
  
  # for getting game info
  box_teams_html <- game_html %>% html_nodes(xpath = '//div[@id="all_player_offense"]//td[@data-stat="team"]') %>% html_text
  away_team_abbr <- box_teams_html %>% unique %>% .[1]
  home_team_abbr <- box_teams_html %>% unique %>% .[2]
  season <- game_html %>% html_nodes(xpath = '//div[@id = "inner_nav"]//li/a') %>% html_text %>% .[2] %>% gsub(' NFL Scores & Schedule', '', .) %>% as.numeric()
  
  return(
    list(
      game_html,
      away_team_abbr,
      home_team_abbr,
      season
    )
  )
}

# for saving one game
scrape_game <- function(row) {
  
  week <- row$week
  
  pbp <- get_pfr_pbp(row$boxscore)
  
  data <- pbp[[1]]
  away <- pbp[[2]]
  home <- pbp[[3]]
  season <- pbp[[4]]
  
  if (!row$boxscore %in% c('199711300phi')) {
    xml2::write_html(data, glue::glue('raw/{season}_{formatC(week, width=2, flag=\"0\")}_{away}_{home}.html'))
  }
  
}

# get the whole schedule
sched <- map_df(1994 : 1998, ~{get_pfr_game_ids(.x)}) %>%
  mutate(
    week = case_when(
      week == "WildCard" ~ "18",
      week == "Division" ~ "19",
      week == "ConfChamp" ~ "20",
      week == "SuperBowl" ~ "21",
      TRUE ~ week
    ),
    week = as.integer(week)
  )

sched

# save schedule
saveRDS(sched, "data/pfr_schedule.rds")

# test on one game
scrape_game(sched %>% dplyr::slice(1))
xml2::read_html("raw/1994_01_ATL_DET.html")

# save all the games
purrr::walk(
  926 : nrow(sched), 
  ~{scrape_game(sched %>% dplyr::slice(.x))}
  )

# how to read the game
game_html <- xml2::read_html("raw/1994_01_ATL_DET.html")

game_html
