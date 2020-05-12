plot_univariate <- function(df, cols) {
  
  require(dplyr)
  require(ggplot2)
  require(tidyr)
  
  main_df <- df[,cols]
  
  is_histcol <- function(x) class(x) == "numeric" & length(unique(x)) > 30
  is_barcol <- function(x) (class(x) != "numeric" | length(unique(x)) <= 30) & (length(unique(x)) != length(x))
  
  bardata <- main_df %>% select_if(is_barcol) %>% gather()
  histdata <- main_df %>% select_if(is_histcol) %>% gather()
    
  p1 <-
    if(ncol(bardata) != 0) {
      ggplot(bardata, aes(value)) + 
        geom_bar(stat = "count") + 
        facet_wrap(~ key, scales = "free")
    }
  
  p2 <-
    if(ncol(histdata != 0)) {
      ggplot(histdata, aes(value)) + 
        geom_histogram() +
        facet_wrap(~ key, scales = "free")
    }
  
  return(list(p1, p2))
  
}
