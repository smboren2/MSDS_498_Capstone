get_nba_team_season_data <- function(team_abbr, season_end) {
  
  library(rvest)
  
  Sys.sleep(runif(1, 3, 4))
  
  suppressMessages(
    devtools::source_url("https://raw.githubusercontent.com/smboren2/MSDS_498_Capstone/master/R/helper_functions.R")
  )
  
  url <- paste0("https://www.basketball-reference.com/teams/", team_abbr, "/", season_end, ".html")
  
  # ----- get main rosters table that doesn't require reparsing -----
  # NEED TO LOOK INTO THIS FURTHER AS REPARSING HTML SHOULD NOT REMOVE A TABLE
  html_origin <- read_html(url)
  
  roster_tbl <- extract_bbref_table(html_origin, "#roster")
  
  # ----- get other tables on page that require parsing out html comments -----
  html_parsed <- parse_htmlcomments(html_origin)
  
  parsed_tbl_ids <- html_nodes(html_parsed, "table") %>% html_attr("id") %>% unique() %>% .[!is.na(.)]
  includ_tbl_ids <- paste0("#", c("per_game","totals","per_minute","per_poss",
                                 "advanced","shooting","salaries2"))
  
  other_tbls <- lapply(includ_tbl_ids, function(x) extract_tables_teampage(html_parsed, x))
  names(other_tbls) <- gsub(includ_tbl_ids, pattern = "#", replacement = "")
  
  all_tbls <- c(list(roster = roster_tbl), other_tbls)
  
  season <- paste(season_end - 1, substr(season_end, 3, 4), sep = "-")
  
  clean_list <- vector("list", length(all_tbls))
  
  for(i in 1:length(all_tbls)) {
    
    table_name <- names(all_tbls)[i]
    
    df_origin <- all_tbls[[i]]
    cols <- select_df_cols(table_name)
    
    df_new <- df_origin[,cols]
    df_fin <- data.frame(team_abbr, season, df_new, stringsAsFactors = FALSE)
    
    clean_list[[i]] <- df_fin
    
  }
  
  final_df <- purrr::reduce(clean_list, dplyr::full_join, by = c("team_abbr","season","player"))
  return(final_df)
  
}
