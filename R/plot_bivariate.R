plot_bivariate <- function(df, cols, target) {
  
  require(dplyr)
  require(ggplot2)
  require(tidyr)
  
  all_cols <- if(target %in% cols) cols else c(cols, target)
 
  main_df <- df[,all_cols]
  
  is_histcol <- function(x) class(x) == "numeric" & length(unique(x)) > 30
  is_barcol <- function(x) (class(x) != "numeric" | length(unique(x)) <= 30) & (length(unique(x)) != length(x))
    
  bardata <- main_df %>% select_if(is_barcol) %>% cbind(main_df[,target]) %>% gather(key = variable, value = value, -target)
  histdata <- main_df %>% select_if(is_histcol) %>% gather(key = variable, value = value, -target)
    
  p1 <-
    if(ncol(bardata) != 0) {
      ggplot(bardata, aes_string("value", target)) + 
        geom_boxplot() + 
        facet_wrap(~ variable, scales = "free")
    }
  
  p2 <-
    if(ncol(histdata != 0)) {
      ggplot(histdata, aes_string("value", target)) + 
        geom_point() +
        facet_wrap(~ variable, scales = "free")
    }
  
  return(list(p1, p2))
    
}
