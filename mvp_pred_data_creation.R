library(tidyverse)
library(nflfastR)
library(xml2)
library(rvest)
library(stringr)
library(rpart.plot)
library(glmtree)
library(partykit)
library(gsisdecoder)
library(Rcpp)

#player stats- qbs have to play all 16 games, create two data sets, one passer, one rusher, then merge to get rushing stats as well, total yd rank and total td rank, missed less than two games
player_stats <- load_pbp(c(2000:2020))


important_stats <- player_stats %>% select(passer_player_name, passer, posteam, season, season_type, pass_attempt, passing_yards, pass_touchdown,
                                           interception, rushing_yards, air_epa, epa, air_wpa, yards_gained, cpoe, rush_touchdown,
                                           complete_pass, rusher_player_name, game_id, week, game_seconds_remaining, result) %>% 
  filter(season_type == "REG")



qb_games_played <- important_stats %>% unique() %>%
group_by(passer_player_name, season, game_id) %>%
  summarize(passes = n()) 
qb_games_played <- qb_games_played %>%
  group_by(passer_player_name, season) %>% summarize(tot_games_played = n())


qb_passing_stats <- important_stats %>% group_by(season, passer_player_name, posteam) %>% 
  summarize(avg_air_epa = mean(air_epa),
            epa_play = mean(epa, na.rm = TRUE),
            avg_wpa = mean(air_wpa, na.rm = TRUE),
            tot_completions = sum(complete_pass, na.rm = TRUE),
            tot_attempts = sum(pass_attempt, na.rm = TRUE),
            tot_passing_yards = sum(passing_yards, na.rm = TRUE),
            tot_passing_tds = sum(pass_touchdown, na.rm = TRUE),
            tot_ints = sum(interception, na.rm = TRUE),
            avg_cpoe = mean(cpoe, na.rm = TRUE)) %>% 
  filter(tot_attempts >= 150)

write_csv(qb_passing_stats, "qb_passing_stats_0.csv")


###############################################

#qb_passing_stats <- read_csv("qb_passing_stats_0.csv")

qb_passing_stats <- qb_passing_stats %>% 
  group_by(season) %>%
  mutate(passing_yard_rank = order(order(tot_passing_yards, season, decreasing=TRUE)),
         cpoe_rank = order(order(avg_cpoe, season, decreasing=TRUE)),
         avg_wpa_rank = order(order(avg_wpa, season, decreasing=TRUE)),
         epa_rank = order(order(epa_play, season, decreasing=TRUE)),
         passing_td_rank = order(order(tot_passing_tds, season, decreasing=TRUE)),
         completion_percentage = tot_completions/tot_attempts,
         comp_percentage_rank = order(order(completion_percentage, season, decreasing=TRUE)),
         passing_epa_rank = order(order(avg_air_epa, season, decreasing=TRUE))) 

qb_passing_stats <- merge(qb_games_played, qb_passing_stats)

qb_rushing_stats <- important_stats %>%
  mutate(passer_player_name = rusher_player_name) %>%
  group_by(season, passer_player_name, posteam) %>% 
  summarize(tot_rushing_yards = sum(rushing_yards, na.rm = TRUE),
            tot_rushing_tds = sum(rush_touchdown, na.rm = TRUE))

qb_overall_stats_merged <- merge(qb_rushing_stats, qb_passing_stats)
qb_overall_stats <- qb_overall_stats_merged %>% 
  mutate(tot_yards = tot_rushing_yards + tot_passing_yards,
         tot_tds = tot_rushing_tds + tot_passing_tds)
qb_overall_stats <- qb_overall_stats %>% group_by(season) %>%
  mutate(tot_yard_rank = order(order(tot_yards, season, decreasing=TRUE)),
         tot_td_rank = order(order(tot_tds, season, decreasing=TRUE)),
         played_ten_games = if_else(tot_games_played >= 10, 1,0))

  

#league standings

standings <- read_csv("standings.csv") %>% 
  mutate(team = case_when(
    team == 'OAK' ~ 'LV',
    team == 'SD' ~ 'LAC',
    team == 'STL' ~ 'LA',
    TRUE ~ team
  ),
        posteam = team) %>% select(wins, losses, pct, posteam, season)


qb_passing_stats_merged <- merge(standings, qb_overall_stats)
qb_passing_stats_final <- qb_passing_stats_merged %>% group_by(season) %>% 
  mutate(win_rank = order(order(wins, season, decreasing=TRUE)))

write.csv(qb_passing_stats_final,"QB_Season_Stats.csv", row.names = FALSE)


# get mvp table
link = "https://en.wikipedia.org/wiki/Associated_Press_NFL_Most_Valuable_Player_Award"
content <- read_html(link)
tables <- html_table(content)
mvps <- tables[[2]]

# put names in the same form as nflfastR
name.formatter <- function(name) {
  paste0(
    substring(  strsplit(name, " ")[[1]][1], 1, 1), 
    ".", 
    strsplit(name, " ")[[1]][2])
}

mvps <- mvps %>% mutate(Player = sapply(Player, name.formatter))
mvps[mvps$Season == 1963,]$Player = "Y.Tittle" # special case
mvps[mvps$Season == 1973,]$Player = "O.Simpson" # special case
mvps <- mvps %>% filter(Season != 2003)
mvps <- bind_rows(mvps, 
                  tibble(Season=2003,Player="P.Manning",Position="Quarterback",Team="Indianapolis Colts",Ref=""),
                  tibble(Season=2003,Player="S.McNair",Position="Quarterback",Team="Tennessee Titans",Ref="")) %>%
        arrange(Season)
View(mvps)

# join the tibbles
mvps <- mvps %>% mutate(MVP = 1) %>% 
  mutate(passer_player_name = Player, season = Season) %>% 
  select(passer_player_name, Position, MVP, season) %>% distinct()
Data <-read_csv("QB_Season_Stats.csv")

final_qb_mvp <- merge(mvps, Data, all = TRUE) %>% replace_na(list(MVP= 0))
final_qb_mvp$Position <- NULL


#creating csv
write.csv(final_qb_mvp,"Final_QB_MVP.csv", row.names = FALSE)

### RANKINGS WERE FUCKED UP; FIX THEM !!!
G <- read_csv("Final_QB_MVP.csv") %>% arrange(season, -MVP)
D <- G %>% group_by(season) %>%
  mutate(tot_td_rank = rank(-tot_tds),
         tot_yard_rank = rank(-tot_yards),
         win_rank = rank(-wins),
         epa_rank = rank(-epa_play),
         cpoe_rank = rank(-avg_cpoe)) %>%
  ungroup()

write.csv(D,"Final_QB_MVP.csv", row.names = FALSE)


