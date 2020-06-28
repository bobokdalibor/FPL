require(tidyverse)
require(fplr)
require(plotly)

# create dashboard

Sys.getenv("bobok.dalibor")

all_players <- fpl_get_player_all()

player_types <- fpl_get_player_types() %>%
  select(id, singular_name, singular_name_short)

teams <- fpl_get_teams() %>%
  select(id, 
         team_name = name, 
         team_strength = strength)

all_players %>%
  filter(str_detect(news, "Joined|terminated", negate = TRUE),
         total_points > 0) %>%
  # filter(total_points >= 15) %>%
  mutate(full_name = paste(first_name,second_name, sep = " ")) %>%
  left_join(player_types, by = c("element_type" = "id")) %>%
  left_join(teams, by = c("team" = "id")) %>%
  mutate(web_name_warning = case_when(news != "" ~ paste("!",web_name, sep = "-"),
                                         TRUE ~ web_name)) %>%
  mutate(groupping_var = 1) -> all_players_details

# make it rectangles
# show names only for those that have high form / high points / good value?
all_players_details %>%
  ggplot(aes(x = total_points, y = now_cost, group = groupping_var, label1 = full_name, label2 = team_name, label3 = team_strength, label4 = selected_by_percent, label5 = ict_index, label6 = news, label7 = cost_change_start, label8 = bonus, label9 = form)) +
  geom_smooth(method = "lm", formula = y ~ x, fullrange = TRUE, colour = "firebrick", se = FALSE) +
  geom_ribbon(stat = "smooth", method = "lm", alpha = .15, fullrange = TRUE) +
  geom_text(aes(label = web_name_warning, colour = singular_name, alpha = 0.9, size = form)) +
  scale_size(range = c(1.5,3.5)) +
  labs(title = "VFM for FPL players") +
  theme(legend.title = element_blank()) +
  scale_x_continuous(limits = c(0,max(all_players_details$total_points+10)),
                     breaks = seq(0,max(all_players_details$total_points+10),5)) +
  scale_y_continuous(limits = c(3,13),
                     breaks = seq(3,13,1)) -> vfm_graph
ggplotly(vfm_graph)

chart_link <- api_create(vfm_graph, filename = "vfm-graph", sharing = "public")



# form --------------------------------------------------------------------

all_players_details %>%
  ggplot(aes(x = form, y = now_cost, group = groupping_var, label1 = full_name, label2 = team_name, label3 = team_strength, label4 = selected_by_percent, label5 = ict_index, label6 = news, label7 = cost_change_start, label8 = bonus, label9 = total_points)) +
  geom_smooth(method = "lm", formula = y ~ x, fullrange = TRUE, colour = "firebrick", se = FALSE) +
  geom_ribbon(stat = "smooth", method = "lm", alpha = .15, fullrange = TRUE) +
  geom_text(aes(label = web_name_warning, colour = singular_name, alpha = 0.9, size = form)) +
  scale_size(range = c(1.5,3.5)) +
  labs(title = "form_v_cost_fpl_players") +
  scale_x_continuous(limits = c(0,max(all_players_details$form+5)),
                     breaks = seq(0,max(all_players_details$form+5),1)) +
  scale_y_continuous(limits = c(3,13),
                     breaks = seq(3,13,1)) -> form_plot

chart_link <- api_create(form_plot, filename = "form-plot", sharing = "public")


all_players_details %>%
  ggplot(aes(x = total_points, y = now_cost, group = groupping_var, label1 = full_name, label2 = team_name, label3 = team_strength, label4 = selected_by_percent, label5 = ict_index, label6 = news, label7 = cost_change_start, label8 = bonus, label9 = form)) +
  facet_wrap(~singular_name) +
  geom_smooth(method = "lm",formula = y ~ x, fullrange = TRUE, aes(color = singular_name, fill = singular_name)) +
  geom_text(aes(label = web_name_warning, colour = singular_name, alpha = 0.9, size = form)) +
  scale_size(range = c(1.5,3.5)) +
  labs(title = "vfm_fpl_players") +
  scale_x_continuous(limits = c(0,max(all_players_details$total_points+5)),
                     breaks = seq(0,max(all_players_details$total_points+5),5)) +
  scale_y_continuous(limits = c(3,13),
                     breaks = seq(3,13,1)) -> faceted_vfm

chart_link <- api_create(faceted_vfm, filename = "faceted_vfm", sharing = "public")


this_season_players_data <- lapply(all_players_details$id, fpl_get_player_detailed)

require(plyr)
require(magrittr)
this_season_players_data %>%
  purrr::map_dfr("history") %>%
  select(player_id = element,
         total_points,
         value,
         round) -> older_gameweek_data

all_players_details %>%
  left_join(older_gameweek_data, by = c("id" = "player_id")) %>%
  group_by(id) %>%
  dplyr::mutate(cumsum_points = cumsum(total_points.y)) -> all_players_details_plus_other_gwks


# add regression line
fit <- lm(now_cost ~ total_points, data = all_players_details)
# add all the hover over text
vfm_plot_interactive <- all_players_details_plus_other_gwks %>%
  filter(total_points.x >= 10) %>%
  plot_ly(
    x = ~cumsum_points, 
    y = ~value, 
    frame = ~round, 
    text = ~web_name_warning,
    size = ~form,
    sizes = c(6,18),
    color = ~singular_name,
    type = 'scatter',
    mode = 'text'
   ) %>%
  layout(
    title = "Value For Money FPL players",
    yaxis = list(
      title = "Value",
      zeroline = F
    ),
    xaxis = list(
      title = "Cummulative points",
      zeroline = F, 
      showgrid = T
    )
  )

chart_link <- api_create(vfm_plot_interactive, filename = "vfm-plot-interactive", sharing = "public")


this_season_players_data[[.]]$history

this_season_players_data_df <- plyr::ldply(this_season_players_data)


all_players_details %>%
  filter(ict_index > 35) %>%
  ggplot(aes(x = total_points, y = ict_index)) +
  geom_smooth(method = "lm", formula = y ~ x, fullrange = TRUE) +
  geom_text(aes(label = web_name_warning, colour = singular_name, alpha = 0.9)) +
  scale_x_continuous(limits = c(0,max(all_players_details$total_points+5)),
                     breaks = seq(0,max(all_players_details$total_points+5),5))

ggplotly()
options(scipen=999)

all_players_details %>%
  top_n(20, wt = transfers_in_event) %>%
  ggplot(aes(x = reorder(full_name,transfers_in_event), y = transfers_in_event, fill = singular_name)) +
  geom_col() +
  coord_flip() +
  labs(title = "Most transfers in",
       x = "",
       y = "",
       fill = "Position") -> trans_in

chart_link <- api_create(trans_in, filename = "trans_in", sharing = "public")

all_players_details %>%
  top_n(20, wt = transfers_out_event) %>%
  ggplot(aes(x = reorder(full_name,transfers_out_event), y = transfers_out_event, fill = singular_name)) +
  geom_col() +
  coord_flip() +
  labs(title = "Most transfers out",
       x = "",
       y = "",
       fill = "Position")  -> trans_out

chart_link <- api_create(trans_out, filename = "trans_out", sharing = "public")

all_players_details %>%
  top_n(20, wt = cost_change_start) %>%
  ggplot(aes(x = reorder(full_name,cost_change_start), y = cost_change_start, fill = singular_name)) +
  geom_col() +
  coord_flip() +
  labs(title = "Cost since start",
       x = "",
       y = "",
       fill = "Position") -> cost_change

chart_link <- api_create(cost_change, filename = "cost_change", sharing = "public")

browseURL("https://plot.ly/dashboard/bobok.dalibor:24/view")
