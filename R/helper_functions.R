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
  
  return(df)
}

extract_bbref_table <- function(html_obj, table_id) {
  
  headr_css <- paste(table_id, "> thead > tr > th")               # css for table headers
  table_css <- paste(table_id, "> tbody > tr > td")               # css for table values
  colm1_css <- paste(table_id, "> tbody > tr > th:nth-child(1)")  # css for values in first column
  
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
