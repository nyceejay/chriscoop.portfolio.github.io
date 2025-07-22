library(baseballr)
library(tidyverse)
library(gtExtras)
library(systemfonts)
library(paletteer)
library(ggthemes)
library(webshot2)


################## xwOBA heaters top of shadow  zone

mutated_sc_2025 %>% 
  filter(attack_zone_shadow == "shadow" &
         plate_z >= (sz_top - 1/6*(sz_top - sz_bot)) &
         team_batting == "NYY" &
         pitch_type %in% c("FF", "SI", "FC") &
         events %in% c("single", "double", "triple", 
                       "home_run", "force_out", "field_out",
                       "strikeout", "sac_fly", "sac_bunt", 
                       "double_play", "triple_play", "strikeout_double_play",
                       "fielders_choice", "walk", 
                       "grounded_into_double_play", "hit_by_pitch",
                       "sac_fly_double_play", "fielders_choice_out",
                       "field_error")) %>% 
  group_by(player_name) %>% 
  summarise(fastball.pas = n(),
            xwOBA_numerator = sum(estimated_woba_using_speedangle,na.rm = T),
            xwOBA.fastball.top.shadow = round(xwOBA_numerator/fastball.pas,3)) %>% 
  select(-xwOBA_numerator) %>% 
  inner_join(
    mutated_sc_2025 %>% 
      filter(attack_zone_shadow == "shadow" &
               plate_z <= (sz_bot + 1/6*(sz_top - sz_bot)) &
               team_batting == "NYY" &
               pitch_type %in% c("CU", "KC", "CS", "SL",
                                 "ST", "SV", "KN") &
               events %in% c("single", "double", "triple", 
                             "home_run", "force_out", "field_out",
                             "strikeout", "sac_fly", "sac_bunt", 
                             "double_play", "triple_play", "strikeout_double_play",
                             "fielders_choice", "walk", 
                             "grounded_into_double_play", "hit_by_pitch",
                             "sac_fly_double_play", "fielders_choice_out",
                             "field_error")) %>% 
      group_by(player_name) %>% 
      summarise(breaking.pas = n(),
                xwOBA_numerator = sum(estimated_woba_using_speedangle,na.rm = T),
                xwOBA.breaking.bot.shadow = round(xwOBA_numerator/breaking.pas,3)) %>% 
      select(-xwOBA_numerator),
    by = "player_name"
  ) %>% 
  inner_join(
    mutated_sc_2025 %>% 
      filter(attack_zone_shadow == "shadow" &
               plate_z <= (sz_bot + 1/6*(sz_top - sz_bot)) &
               team_batting == "NYY" &
               pitch_type %in% c("CH", "FS", "FO", "SC") &
               events %in% c("single", "double", "triple", 
                             "home_run", "force_out", "field_out",
                             "strikeout", "sac_fly", "sac_bunt", 
                             "double_play", "triple_play", "strikeout_double_play",
                             "fielders_choice", "walk", 
                             "grounded_into_double_play", "hit_by_pitch",
                             "sac_fly_double_play", "fielders_choice_out",
                             "field_error")) %>% 
      group_by(player_name) %>% 
      summarise(offspeed.pas = n(),
                xwOBA_numerator = sum(estimated_woba_using_speedangle,na.rm = T),
                xwOBA.offspeed.bot.shadow = round(xwOBA_numerator/offspeed.pas,3)) %>% 
      select(-xwOBA_numerator),
    by = "player_name"
  ) %>% 
  filter(fastball.pas >= 10 &
           breaking.pas >= 10 &
           offspeed.pas >= 10) %>% 
  gt() %>% 
  cols_label(
    player_name = "Player",
    fastball.pas = "Plate Apps.(fastball)",
    breaking.pas = "Plate Apps.(breaking)",
    offspeed.pas = "Plate Apps.(offspeed)",
    xwOBA.fastball.top.shadow = "xwOBA (fastball)",
    xwOBA.breaking.bot.shadow = "xwOBA (breaking)",
    xwOBA.offspeed.bot.shadow = "xwOBA (offspeed)"
  ) %>% 
  data_color(
    columns = c(xwOBA.fastball.top.shadow,
                xwOBA.breaking.bot.shadow,
                xwOBA.offspeed.bot.shadow),
    palette = c("#2E5A87",
                "#4878A4",
                "#CBDAF1",
                "#ECBBB0",
                "#F5796A",
                "#D73529",
                "#CF191A",
                "#9C0824"),
    domain = c(0, .05, .10, 
               .150, .250,
               .300, .350, .400,
               .500, .800)
  ) %>% 
  ################# header
  tab_style(
    style = list(
      cell_text(
        color = 'black',
        weight = 'bold',
        align = "center",
        font = "Courier-Bold"
      ),
      cell_borders(
        sides = "all",
        color = "black"
      )
    ),
    locations = cells_title()
  ) %>% 
  #######################body
  tab_style(
    style = list(
      cell_text(
        color = 'black',
        align = "center",
        font = "Courier-Oblique"
      ),
      cell_borders(
        sides = "all",
        color = "black"
      )
    ),
    locations = cells_body()
  ) %>% 
  ####################### Column labels
  tab_style(
    style = list(
      cell_text(
        color = 'black',
        align = "center",
        v_align = "middle"
      ),
      cell_fill(
        color = "#EEE5DE"
      ),
      cell_borders(
        sides = 'all',
        color = 'black'
      )
    ),
    locations = cells_column_labels()
  ) %>%
  ####################### Player name col label
  tab_style(
    style = list(
      cell_text(
        color = 'black',
        align = "center",
        v_align = "middle"
      ),
      cell_fill(
        color = "#FFFFE0"
      ),
      cell_borders(
        sides = 'all',
        color = 'black'
      )
    ),
    locations = cells_column_labels(columns = player_name)
  ) %>%
  ####################### player column
  tab_style(
    style = list(
      cell_text(
        color = 'black',
        weight = "bold",
        align = "center"
      ),
      cell_fill(
        color = "#FFFFE0"
      ),
      cell_borders(
        sides = "r",
        color = "black",
        weight = px(2),
        style = "dashed"
      )
    ),
    locations = cells_body(columns = player_name)
  ) %>%
  #######################Table options
  tab_options(table.border.bottom.color = "black",
              table.border.top.color = "black",
              table_body.border.top.color = "black",
              table_body.border.bottom.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "black",
  ) %>% 
  cols_width(
    everything() ~ px(125)
  ) %>% 
  cols_align(align = "center") %>% 
  tab_header(
    title = "How Yankee hitters perform on the top/bottom edges of the zone",
    subtitle = "2025 Season"
  ) %>%
  tab_source_note(
    source_note = "Top edge of the zone for fastballs, bottom edge of the zone for breaking/offspeed"
  )
  gt_theme_espn()











