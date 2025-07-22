library(baseballr)
library(tidyverse)
library(gtExtras)
library(systemfonts)
library(paletteer)
library(ggthemes)
library(webshot2)


#get pitchers season stats 
bref_pitchers = daily_pitcher_bref("2025-03-20", Sys.Date())


bref_pitchers_basic_stats = bref_pitchers %>% 
  select(bbref_id,
         season,
         Age,G,GS,
         IP, ERA, BAbip,
         SO_perc,WHIP)


#get player ids 
baseball_people = chadwick_player_lu()

baseball_ids = baseball_people %>% 
  select(name_first,
         name_last,
         key_mlbam,
         key_bbref,
         key_fangraphs,
         key_retro) %>% 
  mutate(full_name = paste0(name_last,", ",name_first)) %>% 
  select(-name_first, -name_last)



#join id to main sc_2025 table to get pitcher names
mutated_sc_2025 %>% 
  inner_join(baseball_ids,
             by = c("pitcher" = "key_mlbam")) %>% 
  rename(c("pitcher_name" = "full_name",
           "batter_name" = "player_name")) %>% 
  inner_join(bref_pitchers_basic_stats,
             by = c("key_bbref" = "bbref_id")) %>% 
  group_by(team_batting) %>% 
  filter(((GS >= 10) | ((G >= 20 & IP >= 30))) & (ERA < 3.50)) %>%
  filter(events %in% c("single", "double", "triple", 
                       "home_run", "force_out", "field_out",
                       "strikeout", "sac_fly", "sac_bunt", 
                       "double_play", "triple_play", "strikeout_double_play",
                       "fielders_choice", "walk", 
                       "grounded_into_double_play", "hit_by_pitch",
                       "sac_fly_double_play", "fielders_choice_out",
                       "field_error")) %>% 
  summarise(pas = n(),
            xwOBA_n = sum(estimated_woba_using_speedangle, na.rm = T),
            xwOBA = round(xwOBA_n/pas,3)) %>% 
  arrange(desc(xwOBA)) %>% 
  select(-pas,-xwOBA_n) %>% 
  gt() %>% 
  cols_label(
    team_batting = "Team",
    xwOBA = "xwOBA"
  ) %>% 
  data_color(
    columns = xwOBA,
    palette = paletteer_c("ggthemes::Red-Blue-White Diverging", 30),
    reverse = T
  ) %>% 
  tab_header(
    title = "xwOBA Against <= 3.50 ERA Pitchers"
  ) %>% 
  tab_source_note(
    source_note = "Qualified pitchers have at least 10 starts or 20 games and 30 innings pitched"
  ) %>% 
  tab_style(
    style = cell_text(
      color = "black",
      weight = "bold",
      align = "center"
    ),
    locations = cells_title()
  ) %>% 
  tab_style(
    style = list(cell_text(
      color = "grey20",
      align = "center",
      font = "italic",
      weight = "bold"
    ),
    cell_fill(
      color = 'grey50'
    )),
    locations = cells_source_notes()
  ) %>% 
  cols_width(
    everything() ~ px(125)
  ) %>% 
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_body()
  ) %>% 
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_column_labels()
  ) 











#Yankee players ranked by xwOBA vs good pitching
mutated_sc_2025 %>% 
  inner_join(baseball_ids,
             by = c("pitcher" = "key_mlbam")) %>% 
  rename(c("pitcher_name" = "full_name",
           "batter_name" = "player_name")) %>% 
  inner_join(bref_pitchers_basic_stats,
             by = c("key_bbref" = "bbref_id")) %>% 
  group_by(batter_name) %>% 
  filter(team_batting == "NYY") %>% 
  filter(((GS >= 10) | ((G >= 20 & IP >= 30))) & (ERA < 3.50)) %>%
  filter(events %in% c("single", "double", "triple", 
                       "home_run", "force_out", "field_out",
                       "strikeout", "sac_fly", "sac_bunt", 
                       "double_play", "triple_play", "strikeout_double_play",
                       "fielders_choice", "walk", 
                       "grounded_into_double_play", "hit_by_pitch",
                       "sac_fly_double_play", "fielders_choice_out",
                       "field_error")) %>% 
  summarise(pas = n(),
            xwOBA_n = sum(estimated_woba_using_speedangle, na.rm = T),
            xwOBA = round(xwOBA_n/pas,3)) %>% 
  arrange(desc(xwOBA)) %>% 
  select(-xwOBA_n) %>% 
  filter(pas >= 25) %>% 
  gt() %>% 
  cols_label(
    pas = "Plate Appearances",
    batter_name = "Player",
    xwOBA = "xwOBA"
  ) %>% 
  data_color(
    columns = xwOBA,
    palette = paletteer_c("ggthemes::Red-Blue-White Diverging", 16),
    reverse = T,
    domain = c(.200,.255,.260,.310,.360,.450)
  ) %>% 
  tab_header(
    title = "Yankees vs xwOBA Against <= 3.50 ERA Pitchers"
  ) %>% 
  tab_source_note(
    source_note = "Qualified pitchers have at least 10 starts or 20 games and 30 innings pitched"
  ) %>% 
  tab_style(
    style = cell_text(
      color = "black",
      weight = "bold",
      align = "center"
    ),
    locations = cells_title()
  ) %>% 
  tab_style(
    style = list(cell_text(
      color = "grey20",
      align = "center",
      font = "italic",
      weight = "bold"
    ),
    cell_fill(
      color = 'grey50'
    )),
    locations = cells_source_notes()
  ) %>% 
  cols_width(
    everything() ~ px(125)
  ) %>% 
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_body()
  ) %>% 
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_column_labels()
  )
