library(tidyverse)

# import functions
devtools::source_url("https://raw.githubusercontent.com/smboren2/MSDS_498_Capstone/master/R/get_nba_team_season_data.R")

# import abbreviations
team_abbrev <- read_csv("https://raw.githubusercontent.com/smboren2/MSDS_498_Capstone/master/data/nba_team_abbreviations.csv")

completed_season <- 2010:2019
team_abbr <- team_abbrev$team_abbr

team_seasons <- 
  expand.grid(team_abbr, completed_season) %>%
  mutate(Var1 = case_when(Var1 == "CHA" & Var2 >= 2015 ~ "CHO",
                          Var1 == "NJN" & Var2 >= 2013 ~ "BRK",
                          Var1 == "NOH" & Var2 >= 2014 ~ "NOP",
                          TRUE ~ as.character(Var1)))

df_list <- vector("list", length = nrow(team_seasons))
names(df_list) <- apply(team_seasons, 1, FUN = function(x) paste(x, collapse = "-"))

for(i in 1:nrow(team_seasons)) {
  
  team <- team_seasons$Var1[i]
  season <- team_seasons$Var2[i]
  
  list_name <- paste(team, season, sep = "-")
  df_list[[list_name]] <- get_nba_team_season_data(team_abbr = team, season_end = season)
  
}

# manually accounting for change in team names
new_abbrevs <- 
  data.frame(team_abbr = c("CHO","BRK","NOP"),
             team_name = c("Charlotte Hornets","Brooklyn Nets","New Orleans Pelicans"),
             stringsAsFactors = FALSE)

# append old names and new names
all_abbrevs <- 
  bind_rows(new_abbrevs, team_abbrev) %>%
  mutate(team_name = case_when(team_abbr == "CHA" ~ "Charlotte Bobcats",
                               team_abbr == "NJN" ~ "New Jersey Nets",
                               team_abbr == "NOH" ~ "New Orleans Hornets",
                               TRUE ~ as.character(team_name)))

# join team name for final export
all_data <- 
  bind_rows(df_list) %>%
  inner_join(all_abbrevs, by = "team_abbr") %>% 
  select(team_abbr, team_name, season, everything())

write_csv(all_data, "INSERT PATH")
