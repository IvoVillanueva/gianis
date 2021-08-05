library(gt)
library(janitor)
library(tidyverse)
library(nbastatR)
library(ggtext)
library(paletteer)
logs <- game_logs(seasons = 2021, season_types = "Playoffs")
logs <- logs%>% 
  filter((slugTeam == "MIL" & slugOpponent == "BKN") | (slugTeam == "BKN" & slugOpponent == "MIL"))

box_scores(game_ids = logs$idGame,
          league = "NBA",
          box_score_types = "matchups",
          result_types = "player")

df <- dataBoxScorePlayerNBA %>% filter(namePlayerDefense == "Giannis Antetokounmpo") %>% 
  distinct()
df <- df %>% mutate(
  game_number = group_indices(., idGame)
) 

df %>% 
  select(game_number, namePlayerOffense, PCT_DEFENDER_TOTAL_TIME) %>% 
  pivot_wider(names_from = game_number, values_from = PCT_DEFENDER_TOTAL_TIME) %>% 
  clean_names()%>% 
  gt() %>% 
  tab_header(
    title = md(paste0("**Porcentaje de ", first(df$namePlayerDefense), " defendiendo**")),
    subtitle = md("BUCKS VS NETS")) %>%
  tab_spanner(
    label =md("Game Number"),
    columns = 2:8
  ) %>% 
  cols_label(name_player_offense = "Ofensive Player",
             x1 = "1",
             x2 = "2",
             x3 = "3",
             x4 = "4",
             x5 = "5",
             x6 = "6",
             x7 = "7")  %>%
  data_color(
    columns = 2:8,
    colors = scales::col_numeric(
      palette = paletteer_d("rcartocolor::Mint",
                  direction = 1
                  ) %>% as.character(),
      domain = c(0, max(df$PCT_DEFENDER_TOTAL_TIME)),
    na.color ="#f4f4f4"
    )
  )%>% 
  fmt_percent(columns = 2:8,
              decimals = 0) %>%fmt_missing(
                columns = 2:8,
                missing_text = "0%"
              ) %>% 
  opt_row_striping() %>% 
  tab_options(
  table.font.names = "Chivo",
  table.background.color = "#f4f4f4",
  heading.title.font.size = 15,
  table.font.size = 10,
  column_labels.font.size = 13,
  column_labels.font.weight = "bold",
  data_row.padding = px(5)
  ) %>% 
  tab_source_note(
  source_note = md("Data from ***stats.nba.com***.<br>
    Credit: **Ivo Villanueva**." )) %>% 
  gtsave("gianis.html")
  
webshot::install_phantomjs()

