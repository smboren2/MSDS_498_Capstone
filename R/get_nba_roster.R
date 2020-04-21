get_nba_roster <- function(team_abbr, season_end) {
  
  library(rvest)
  
  Sys.sleep(runif(1, 3, 4))
  
  suppressMessages(
    devtools::source_url("https://raw.githubusercontent.com/smboren2/MSDS_498_Capstone/master/R/helper_functions.R")
  )
  
  url <- paste0("https://www.basketball-reference.com/teams/", team_abbr, "/", season_end, ".html")
  
  rosters <- read_html(url)
  
  parsed_tbl <- extract_bbref_table(rosters, "#roster")
  
  # extract player id
  player_nodes <- html_nodes(rosters, "#roster > tbody > tr > [data-stat='player']")
  player_id <- player_nodes %>% html_attr("data-append-csv")
  
  # create season as 'year start' - 'last two digits of year end'
  season <- paste(season_end - 1, substr(season_end, 3, 4), sep = "-")
  
  final_df <- data.frame(team_abbr, season, player_id, parsed_tbl, stringsAsFactors = FALSE)
  names(final_df) <- c("team_abbr", "season", "player_id", names(parsed_tbl))

  return(final_df)
  
}
