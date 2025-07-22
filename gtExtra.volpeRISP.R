library(baseballr)
library(tidyverse)
library(gtExtras)
library(systemfonts)
library(paletteer)
library(ggthemes)
library(webshot2)

#Volpe Swings with RISP
sc_2025 %>% 
  mutate(
    team_batting = ifelse(inning_topbot == "Bot",
                          home_team,
                          away_team),
    team_pitching = ifelse(inning_topbot == "Bot",
                           away_team,
                           home_team),
    inning_bins = cut(inning, seq(1,9, by = 3),
                      include.lowest = T),
    on_base = ifelse(
      (!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b)),
      "runners on",
      "no runners on"
    ),
    risp = ifelse(
      (!is.na(on_2b) | !is.na(on_3b)),
      "RISP",
      "no RISP"
    ),
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
    ),
    sweetspot = ifelse(
      (launch_angle >= 8 | launch_angle <=32),
      "sweetspot",
      "not sweetspot"
    ),
    count = case_when(
      (balls == 0 & strikes == 0) ~ "First Pitch",
      ((balls == 0 & strikes == 1) |
         (balls == 0 & strikes == 2) |
         (balls == 1 & strikes == 2)) ~ "Pitchers Count",
      ((balls == 1 & strikes == 1) |
         (balls == 2 & strikes == 2)) ~ "Even",
      ((balls == 1 & strikes == 0) |
         (balls == 2 & strikes == 0) |
         (balls == 2 & strikes == 1) |
         (balls == 3 & strikes == 0) |
         (balls == 3 & strikes == 1)) ~ "Batters Count",
      (balls == 3 & strikes == 2) ~ "Full Count"
    )) %>% 
  filter(team_batting == "NYY", 
         player_name == "Volpe, Anthony" &
           description %in% c("foul", "swinging_strike",
              "hit_into_play", "foul_tip", "swinging_strike_blocked") &
           risp == "RISP") %>% 
  ggplot(aes(x = plate_x, y = plate_z))+
    geom_rect(
      xmin = -10/12, ymin = 1.59,
      xmax = 10/12, ymax = 3.38,
      alpha = 0, color = "turquoise4",
      linewidth = 1.5
    )+
    geom_density_2d_filled(alpha = 0.5,
                           show.legend = F)+
    scale_fill_brewer(palette = "RdBu",
                    direction = -1)+
    theme_bw()+
    coord_fixed()+
    facet_wrap(~count)+
    xlim(-1.5, 1.5) +
    ylim(0.5, 4.5)
  
  




#Volpe field outs with RISP
sc_2025 %>% 
  mutate(
    team_batting = ifelse(inning_topbot == "Bot",
                          home_team,
                          away_team),
    team_pitching = ifelse(inning_topbot == "Bot",
                           away_team,
                           home_team),
    inning_bins = cut(inning, seq(1,9, by = 3),
                      include.lowest = T),
    on_base = ifelse(
      (!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b)),
      "runners on",
      "no runners on"
    ),
    risp = ifelse(
      (!is.na(on_2b) | !is.na(on_3b)),
      "RISP",
      "no RISP"
    ),
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
    ),
    sweetspot = ifelse(
      (launch_angle >= 8 | launch_angle <=32),
      "sweetspot",
      "not sweetspot"
    ),
    count = case_when(
      (balls == 0 & strikes == 0) ~ "First Pitch",
      ((balls == 0 & strikes == 1) |
         (balls == 0 & strikes == 2) |
         (balls == 1 & strikes == 2)) ~ "Pitchers Count",
      ((balls == 1 & strikes == 1) |
         (balls == 2 & strikes == 2)) ~ "Even",
      ((balls == 1 & strikes == 0) |
         (balls == 2 & strikes == 0) |
         (balls == 2 & strikes == 1) |
         (balls == 3 & strikes == 0) |
         (balls == 3 & strikes == 1)) ~ "Batters Count",
      (balls == 3 & strikes == 2) ~ "Full Count"
    )) %>% 
  filter(team_batting == "NYY", 
         player_name == "Volpe, Anthony" &
           description %in% c("hit_into_play") &
           events %in% c("field_out", "grounded_into_double_play",
                         "force_out", "double_play", "field_error",
                         "fielders_choice_out", "fielders_choice") &
           risp == "RISP") %>% 
  ggplot(aes(x = plate_x, y = plate_z))+
  geom_rect(
    xmin = -10/12, ymin = 1.59,
    xmax = 10/12, ymax = 3.38,
    alpha = 0, color = "turquoise4",
    linewidth = 1.5
  )+
  geom_density_2d_filled(alpha = 0.5,
                         show.legend = F)+
  scale_fill_brewer(palette = "RdBu",
                    direction = -1,
                    na.value = "transparent")+
  theme_bw()+
  coord_fixed()+
  xlim(-1.5, 1.5) +
  ylim(0.5, 4.5)+
  ggtitle("Anthony Volpe Field Outs RISP")+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())
  

#Volpe RISP hits 
sc_2025 %>% 
  mutate(
    team_batting = ifelse(inning_topbot == "Bot",
                          home_team,
                          away_team),
    team_pitching = ifelse(inning_topbot == "Bot",
                           away_team,
                           home_team),
    inning_bins = cut(inning, seq(1,9, by = 3),
                      include.lowest = T),
    on_base = ifelse(
      (!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b)),
      "runners on",
      "no runners on"
    ),
    risp = ifelse(
      (!is.na(on_2b) | !is.na(on_3b)),
      "RISP",
      "no RISP"
    ),
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
    ),
    sweetspot = ifelse(
      (launch_angle >= 8 | launch_angle <=32),
      "sweetspot",
      "not sweetspot"
    ),
    count = case_when(
      (balls == 0 & strikes == 0) ~ "First Pitch",
      ((balls == 0 & strikes == 1) |
         (balls == 0 & strikes == 2) |
         (balls == 1 & strikes == 2)) ~ "Pitchers Count",
      ((balls == 1 & strikes == 1) |
         (balls == 2 & strikes == 2)) ~ "Even",
      ((balls == 1 & strikes == 0) |
         (balls == 2 & strikes == 0) |
         (balls == 2 & strikes == 1) |
         (balls == 3 & strikes == 0) |
         (balls == 3 & strikes == 1)) ~ "Batters Count",
      (balls == 3 & strikes == 2) ~ "Full Count"
    )) %>% 
  filter(team_batting == "NYY", 
         player_name == "Volpe, Anthony" &
           description %in% c("hit_into_play") &
           events %in% c("single", "double", "triple", "home_run") &
           risp == "RISP") %>% 
  ggplot(aes(x = plate_x, y = plate_z))+
  geom_rect(
    xmin = -10/12, ymin = 1.59,
    xmax = 10/12, ymax = 3.38,
    alpha = 0, color = "turquoise4",
    linewidth = 1.5
  )+
  geom_density_2d_filled(alpha = 0.5,
                         show.legend = F)+
  scale_fill_brewer(palette = "RdBu",
                    direction = -1,
                    na.value = "transparent")+
  theme_bw()+
  coord_fixed()+
  xlim(-1.5, 1.5) +
  ylim(0.5, 4.5)+
  ggtitle("Anthony Volpe Hits RISP")+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())





sc_2025 %>% 
  mutate(
    team_batting = ifelse(inning_topbot == "Bot",
                          home_team,
                          away_team),
    team_pitching = ifelse(inning_topbot == "Bot",
                           away_team,
                           home_team),
    inning_bins = cut(inning, seq(1,9, by = 3),
                      include.lowest = T),
    on_base = ifelse(
      (!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b)),
      "runners on",
      "no runners on"
    ),
    risp = ifelse(
      (!is.na(on_2b) | !is.na(on_3b)),
      "RISP",
      "no RISP"
    ),
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
    ),
    sweetspot = ifelse(
      (launch_angle >= 8 | launch_angle <=32),
      "sweetspot",
      "not sweetspot"
    ),
    count = case_when(
      (balls == 0 & strikes == 0) ~ "First Pitch",
      ((balls == 0 & strikes == 1) |
         (balls == 0 & strikes == 2) |
         (balls == 1 & strikes == 2)) ~ "Pitchers Count",
      ((balls == 1 & strikes == 1) |
         (balls == 2 & strikes == 2)) ~ "Even",
      ((balls == 1 & strikes == 0) |
         (balls == 2 & strikes == 0) |
         (balls == 2 & strikes == 1) |
         (balls == 3 & strikes == 0) |
         (balls == 3 & strikes == 1)) ~ "Batters Count",
      (balls == 3 & strikes == 2) ~ "Full Count"
    ),
    hit.or.not = ifelse(events %in% c("single", "double",
                                      "triple", "home_run"),
                        "hit", "not hit"),
    player_types = case_when(
      player_name %in% c("Albies, Ozzie", "Altuve, Jose", "Carroll, Corbin",
                         "Neto, Zach", "Hoerner, Nico", "Bichette, Bo", "Swanson, Dansby") ~ "Similar Players",
      player_name == "Volpe, Anthony" ~ "Volpe"
    )) %>% 
  filter(!is.na(player_types) &
           risp == "RISP" &
           (plate_x <=1 & plate_x >=0.3) &
           (plate_z <= 2.5 & plate_z >=1.5)) %>% 
  group_by(player_types,hit.or.not) %>% 
  summarise(
    bat.speed = mean(bat_speed, na.rm = T),
    swing.length = mean(swing_length, na.rm = T),
    swing_path_tilt = mean(swing_path_tilt, na.rm = T),
    attack.angle = mean(attack_angle, na.rm = T),
    intercept.x = mean(intercept_ball_minus_batter_pos_x_inches, na.rm = T),
    intercept.y = mean(intercept_ball_minus_batter_pos_y_inches, na.rm = T),
    ev = median(launch_speed,na.rm = T),
    LA = median(launch_angle, na.rm = T)
  ) %>% 
  gt() %>% 
  cols_align(
    align = "center"
  ) %>% 
  cols_width(
    everything() ~ px(150)
  ) %>% 
  tab_header(
    title = "Volpe vs Similar Hitters with RISP",
    subtitle = "Similar hitters include Albies, Altuve, Carroll, Neto, Hoerner, Bichette, and Swanson. Guys physically built similarly to Volpe"
  ) %>% 
  cols_label(
    hit.or.not = "Hit/No Hit",
    bat.speed = "Bat Speed",
    swing.length = "Swing Length",
    swing_path_tilt = "Swing Tilt",
    attack.angle = "Attack Angle",
    intercept.x = "Contact Distance To Batter (x)",
    intercept.y = "Contact Distance To Batter (y)",
    ev = "EV",
    LA = "LA"
  ) %>% 
  tab_style(
   style =  list(cell_fill(
      color = 'lightsteelblue'
    ),
    cell_text(
      color = 'black',
      weight = 'bold'
    )),
    locations = cells_title()
  ) %>% 
  tab_style(
    style = list(cell_fill(
      color = '#FFE4E1'
    ),
    cell_text(
      color = 'grey10',
      weight = 'bold'
    )),
    locations = cells_row_groups()
  ) %>% 
  tab_style(
    style = cell_fill(
      color = 'lavenderblush'
    ),
    locations = cells_body()
  ) %>% 
  gt_highlight_cols(
    columns = c(swing.length, intercept.x, intercept.y, LA),
    fill = "#EED5D2"
  ) %>% 
  tab_style(
    style = list(
      cell_text(
        color = 'grey30',
        weight = "bold"
      )
    ),
    locations = cells_column_labels()
  )
  
