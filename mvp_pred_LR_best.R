library(tidyverse)
library(gt)
library(paletteer)
options(scipen = 50)

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 13, hjust = 0.5),
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      axis.text = element_text(size = 12)
    )
}

####################################################################################

# E <- D %>% filter(season==2003) %>% select(passer_player_name, season, wins, win_rank)
# E %>% mutate(wr = rank(-wins)) %>% arrange(wr)


### RANKINGS WERE FUCKED UP; FIX THEM !!!
G <- read_csv("Final_QB_MVP.csv") %>% arrange(season, -MVP)
D <- G %>% group_by(season) %>%
           mutate(tot_td_rank = rank(-tot_tds),
                  tot_yard_rank = rank(-tot_yards),
                  win_rank = rank(-wins),
                  epa_rank = rank(-epa_play),
                  cpoe_rank = rank(-avg_cpoe)) %>%
           ungroup()

# check
# D %>% filter(season==2020) %>% select(season, passer_player_name, wins, win_rank) %>% arrange(win_rank)
# D %>% filter(season==2020) %>% select(season, passer_player_name, tot_tds, tot_td_rank) %>% arrange(tot_td_rank)

D.train <- D %>% filter(season <= 2016 & season >= 2003 & !(season %in% c(2005,2006,2012))) # (error in 2001, 2002 data, so do >= 2003)
D.test <- D %>% filter(season >= 2017)

#m <- glm(MVP ~ tot_td_rank + tot_yard_rank + win_rank + epa_rank  + tot_ints + cpoe_rank + tot_games_played,
#m <- glm(MVP ~ tot_td_rank +  tot_yard_rank + win_rank + epa_rank + tot_ints + tot_games_played,
#m <- glm(MVP ~ tot_td_rank +  tot_yard_rank + win_rank + tot_ints,
m <- glm(MVP ~ tot_td_rank +  tot_yard_rank + win_rank + epa_rank + tot_ints, 
         data = D.train, family = binomial)
summary(m)

### training & testing accuracy

D.train <- D.train %>% mutate(pred = predict(m, D.train, type = "response")) %>%
                       group_by(season) %>%
                       mutate(pred_mvp = as.numeric(pred == max(pred, na.rm=TRUE))) %>%
                       mutate(mvp_prob = pred / sum(pred)) %>% # normalize the probabilities
                       ungroup()
D1 <- D.train %>% mutate(name = passer_player_name, s = season) %>% 
                  select(name,s,mvp_prob, pred_mvp, MVP,tot_td_rank, tot_yard_rank,
                         win_rank, epa_rank, tot_ints) %>%
                  arrange(s, -mvp_prob) %>% mutate(mvp_prob = round(mvp_prob,3))
D11 <- D1 %>% group_by(s) %>% filter(row_number() <= 3) %>% ungroup() 
#View(D1)
View(D11)
#########
D.test <- D.test %>% mutate(pred = predict(m, D.test, type = "response")) %>%
                     group_by(season) %>%
                     mutate(pred_mvp = as.numeric(pred == max(pred, na.rm=TRUE))) %>%
                     mutate(mvp_prob = pred / sum(pred)) %>% # normalize the probabilities
                     ungroup()
D2 <- D.test %>% mutate(name = passer_player_name, s = season) %>% 
                 select(name,s,mvp_prob, pred_mvp, MVP,tot_td_rank, tot_yard_rank, 
                        win_rank, epa_rank, tot_ints) %>%
                 arrange(s, -mvp_prob) %>% mutate(mvp_prob = round(mvp_prob,3))
D22 <- D2 %>% group_by(s) %>% filter(row_number() <= 3) %>% ungroup() 
#View(D2)
View(D22)
#########

accuracy <- function(data) {
  a = (data %>% filter(MVP == 1))$MVP
  b = (data %>% filter(MVP == 1))$pred_mvp
  x = sum(abs(a-b), na.rm=TRUE)
  x
  n = length(a)
  n
  c(n-x, n, (n - x) / n)
}

print("training accuracy")
accuracy(D.train)
print("testing accuracy")
accuracy(D.test)

m

####################################################################################

#### TABLES for the ARTICLE

v1 = D1 %>% filter(s == 2003) %>% filter(row_number() <= 5) 
v2 = D1 %>% filter(s == 2009) %>% filter(row_number() <= 5)
v3 = D1 %>% filter(s == 2013) %>% filter(row_number() <= 5)
v4 = D1 %>% filter(s == 2015) %>% filter(row_number() <= 5)
v5 = D2 %>% filter(s == 2020) %>% filter(row_number() <= 5)


v1 %>% gt() %>%
  data_color(
    columns = mvp_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  )

