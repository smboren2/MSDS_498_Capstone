parse_table <- function(table) {
  
  convert_to_numeric <- function(x) {
    # tries to make numeric columns numeric (from char)
    numeric_x <- suppressWarnings(as.numeric(x))
    
    if(!all(is.na(numeric_x))) {
      x <- numeric_x
    }
    
    return(x)
  }
  
  empty_string_to_na <- function(x) {
    
    if(class(x) == "character") {
      res <- ifelse(stringr::str_trim(x, "both") == "", NA, x)
    } else {
      res <- x
    }
    
    return(res)
  }
  
  num_convert <- lapply(table, convert_to_numeric)
  na_convert <- lapply(num_convert, empty_string_to_na)
  
  df <- as.data.frame(na_convert, stringsAsFactors = FALSE)
  df <- df[colMeans(is.na(df)) != 1]
  
  return(df)
}

extract_bbref_table <- function(html_obj, table_id) {
  
  headr_nodes <- html_nodes(html_obj, paste(table_id, "> thead > tr"))
  headr_child <- paste0(":nth-child(", length(headr_nodes), ") > th")
  
  headr_css <- paste(table_id, paste0("> thead > tr", headr_child))     # css for table headers
  table_css <- paste(table_id, "> tbody > tr > td")                     # css for table values
  colm1_css <- paste(table_id, "> tbody > tr > th:nth-child(1)")        # css for values in first column
  
  # constructing the table
  table_heads <- html_nodes(html_obj, headr_css) %>% html_attr("data-stat")
  
  table_colm1 <- html_nodes(html_obj, colm1_css) %>% html_text()
  table_value <- html_nodes(html_obj, table_css) %>% html_text()
  
  table_matrix <- cbind(table_colm1, matrix(table_value, ncol = length(table_heads) - 1, byrow = TRUE))
  
  table_df <- as.data.frame(table_matrix, stringsAsFactors = FALSE)
  names(table_df) <- table_heads
  
  final_df <- parse_table(table_df)
  return(final_df)
  
}

parse_htmlcomments <- function(html_obj) {
  
  parsed_html <- 
    html_obj %>%
    html_nodes(xpath = '//comment()') %>%     # select comment nodes
    html_text() %>%                           # extract comment text
    paste(collapse = '') %>%                  # collapse to a single string
    read_html()
  
  return(parsed_html)
  
}

extract_tables_teampage <- function(html_parsed_obj, tbl_id) {
  
  if(tbl_id == "#salaries2") {
    
    tbl <- 
      html_nodes(html_parsed_obj, tbl_id) %>% 
      html_table() %>% 
      data.frame(stringsAsFactors = FALSE)
    
    names(tbl) <- c("ranker", "player", "salary")
    
    } else {
      
      tbl <- extract_bbref_table(html_parsed_obj, tbl_id)
    }
  
  return(tbl)

}

select_df_cols <- function(tbl_name) {
  
  if(tbl_name == "roster") {
    
    cols <- c('number','player','pos','height','weight',
              'birth_date','birth_country','years_experience','college')
  }
  
  if(tbl_name == "per_game") {
    
    cols <- c('player','age','g','gs','mp_per_g',
              'fg_per_g','fga_per_g',
              'fg3_per_g','fg3a_per_g',
              'fg2_per_g','fg2a_per_g',
              'ft_per_g','fta_per_g',
              'orb_per_g','drb_per_g','trb_per_g',
              'ast_per_g','stl_per_g','blk_per_g',
              'tov_per_g','pf_per_g','pts_per_g')
  }
  
  if(tbl_name == "totals") {
    
    cols <- c('player','mp',
              'fg','fga','fg_pct',
              'fg3','fg3a','fg3_pct',
              'fg2','fg2a','fg2_pct',
              'efg_pct',
              'ft','fta','ft_pct',
              'orb','drb','trb',
              'ast','stl','blk',
              'tov','pf','pts')
  }
  
  if(tbl_name == "per_minute") {
    
    cols <- c('player',
              'fg_per_mp','fga_per_mp',
              'fg3_per_mp','fg3a_per_mp',
              'fg2_per_mp','fg2a_per_mp',
              'ft_per_mp','fta_per_mp',
              'orb_per_mp','drb_per_mp','trb_per_mp',
              'ast_per_mp','stl_per_mp','blk_per_mp',
              'tov_per_mp','pf_per_mp','pts_per_mp')
  }
  
  if(tbl_name == "per_poss") {
    
    cols <- c('player',
              'fg_per_poss','fga_per_poss',
              'fg3_per_poss','fg3a_per_poss',
              'fg2_per_poss','fg2a_per_poss',
              'ft_per_poss','fta_per_poss',
              'orb_per_poss','drb_per_poss','trb_per_poss',
              'ast_per_poss','stl_per_poss','blk_per_poss',
              'tov_per_poss','pf_per_poss','pts_per_poss',
              'off_rtg','def_rtg')
  }
  
  if(tbl_name == "advanced") {
    
    cols <- c('player',
              'per','ts_pct',
              'fg3a_per_fga_pct','fta_per_fga_pct',
              'orb_pct','drb_pct','trb_pct',
              'ast_pct','stl_pct','blk_pct',
              'tov_pct','usg_pct',
              'ows','dws','ws','ws_per_48',
              'obpm','dbpm','bpm','vorp')
  }
  
  if(tbl_name == "shooting") {
    
    cols <- c('player',
              'avg_dist','fg2a_pct_fga',
              'pct_fga_00_03','pct_fga_03_10','pct_fga_10_16','pct_fga_16_xx',
              'fg3a_pct_fga',
              'fg_pct_00_03','fg_pct_03_10','fg_pct_10_16','fg_pct_16_xx',
              'fg2_pct_ast',
              'pct_fg2_dunk','fg2_dunk',
              'fg3_pct_ast','pct_fg3a_corner','fg3_pct_corner','fg3a_heave','fg3_heave')
  }
  
  if(tbl_name == "salaries2") {
    
    cols <- c("player","salary")
  }
  
  return(cols)
  
}
  
