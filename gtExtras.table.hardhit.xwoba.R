library(baseballr)
library(tidyverse)
library(gtExtras)
library(systemfonts)

sc_2025 %>% 
  mutate(
    team_batting = ifelse(inning_topbot == "Bot",
                          home_team,
                          away_team),
    team_pitching = ifelse(inning_topbot == "Bot",
                           away_team,
                           home_team),
    bat_spray_angle = atan((hc_x - 125.42)/
                             (198.27 - hc_y)),
    bat_spray_angle = ifelse(
      stand == "L", -bat_spray_angle, bat_spray_angle
    ),
    oppo_pull = ifelse(
      bat_spray_angle < 0, "pull", "oppo"
    ),
    hardhit = ifelse(
      launch_speed >= 95, "hardhit", "not hardhit"
    )) %>% 
  filter(type == "X" & 
           hardhit == "hardhit") %>% 
  group_by(team_batting) %>% 
  summarise(hardhit.balls = n(),
            xwOBA_numerator = sum(estimated_woba_using_speedangle,
                                  na.rm = T),
            xwOBA = round(xwOBA_numerator/hardhit.balls,3)) %>% 
  arrange(-xwOBA) %>% 
  select(team_batting, xwOBA) %>% 
  mutate(xwOBA_rankings = ntile(-xwOBA, 30)) %>% 
  gt() %>% 
  data_color(
    columns = xwOBA_rankings,
    palette = c("#5182AF",
                "#99C5E3",
                "#C4D8F3",
                "#CBDAF1",
                "#D3DEED" ,
                "#EFB6B0",
                "#F5796A",
                "#D73529",
                "#CF191A",
                "#9C0824"),
    domain = c(0,3,6,9,12,
               15,18,21,24,
               27,30),
    reverse = T,
    #Make every column in the table the same conditional format
    target_columns = team_batting
  )%>% 
  cols_label(
    team_batting = "Team",
    xwOBA = "xwOBA",
    xwOBA_rankings = "Rank"
  ) %>% 
  cols_width(
    everything() ~ px(90)
  ) %>% 
  cols_align(
    align = "center"
  ) %>% 
  tab_options(
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.lr.color = "black",
    table_body.border.bottom.color = "black",
    table_body.hlines.style = "none"
  )












