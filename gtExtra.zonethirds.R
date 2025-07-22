library(baseballr)
library(tidyverse)
library(gtExtras)
library(systemfonts)
library(paletteer)
library(ggthemes)
library(webshot2)

sc_2025 %>% 
mutate(zone.location = ifelse(
    zone %in% c("1", "2", "3",
                "4", "5", "6",
                "7", "8", "9"),
    "in.zone", "out.zone"
  ),
 zone.thirds = case_when(
  #top third
  ((plate_z >= (sz_top - (sz_top - sz_bot)/3) & plate_z <= sz_top) &
     zone %in% c("1", "2", "3","4", "5", "6")) ~ "High",
  #middle third
  ((plate_z <= (sz_top - (sz_top - sz_bot)/3) & plate_z >= (sz_bot + (sz_top - sz_bot)/3)) &
     zone %in% c("1", "2", "3","4", "5", "6", "7", "8", "9")) ~ "Middle",
  #lower third
  ((plate_z <= (sz_bot + (sz_top - sz_bot)/3) & plate_z >= sz_bot) &
     zone %in% c("4", "5", "6", "7", "8", "9")) ~ "Low"
),
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
  bat_spray_angle <= -.275, "pull", "oppo"
),
 hardhit = ifelse(
  launch_speed >= 95, "hardhit", "not hardhit"
)) %>% 
  filter(team_batting == "NYY" &
           events %in% 
           c("single", "double", "triple", 
           "home_run", "force_out", "field_out",
           "strikeout", "sac_fly", "sac_bunt", 
            "double_play", "triple_play", "strikeout_double_play",
            "fielders_choice", "walk", 
            "grounded_into_double_play", "hit_by_pitch",
            "sac_fly_double_play"))%>% 
  group_by(player_name,zone.thirds) %>%
  summarise(
    PAs = n(),
    xwOBA_num = sum(estimated_woba_using_speedangle, na.rm = T),
    xwOBA = round(xwOBA_num/PAs, 3)
  ) %>% 
  filter(!is.na(zone.thirds)) %>% 
  select(player_name,zone.thirds ,PAs, xwOBA) %>%
  ungroup() %>% 
  ######################gt table
  gt(groupname_col = "zone.thirds",
     rowname_col = "player_name",
     row_group_as_column = T) %>% 
  cols_label(
    player_name = "Player",
    zone.thirds = "Third of Zone",
    PAs = "Plate Appearances",
    xwOBA = "xwOBA"
  ) %>% 
  cols_width(
    everything() ~ px(200)
  ) %>% 
  cols_align(align = "center") %>% 
  tab_header(
    title = "Yankee Hitters Zone Metrics",
    subtitle = "Zone is split into three horizontal thirds - all in the strikezone"
  ) %>% 
  data_color(
    columns = xwOBA,
    palette = paletteer_c("ggthemes::Red-Blue Diverging", 10),
    domain = c(0,.313,.700),
    reverse = T
  ) %>% 
  tab_options(
    table.background.color = "#FFFFF0",
    data_row.padding = px(0.5)
  ) %>% 
  tab_style(
    style = list(
      cell_text(
        align = "center",
        font = "bold",
        v_align = "middle",
        color = 'black'
      )
    ),
    locations = cells_row_groups()
  ) %>% 
  tab_style(
    style = list(
      cell_text(
        color = 'black',
        font = "bold",
        align = "center"
      ),
      cell_fill(
        color = "#EEEEE0"
      )
    ),
    locations = cells_title()
  ) %>% 
  tab_style(
    style = list(
      cell_text(
        color = 'black',
        font = "bold",
        align = "center"
      )
    ),
    locations = cells_column_labels(columns = everything())
  )

