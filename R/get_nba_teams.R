get_nba_teams <- function() {
  
  library(rvest)
  
  url <- "https://www.basketball-reference.com/teams/"
  
  teams <- read_html(url)
  
  team_nodes <- html_nodes(teams, "#teams_active > tbody > tr > [data-stat='franch_name'] > a")
  
  team_name <- team_nodes %>% html_text() 
  team_abbr <- team_nodes %>% html_attr("href") %>% gsub(., pattern = "\\/|teams", replacement = "")
  
  final_df <- data.frame(team_abbr, team_name, stringsAsFactors = FALSE)
  
  return(final_df)

}
