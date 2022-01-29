# load packages
library(tidyverse)
library(CGPfunctions) # to makes Slope Graph
library(ggbump)       # to makes Bump Chart

# main dataset
set.seed(2000)
df1 = tibble(var = 'A', val = rnorm(n = 25, mean = 5.5, sd = 1.5))
df2 = tibble(var = 'B', val = rnorm(n = 15, mean = 4.5, sd = 1.0))
df3 = tibble(var = 'C', val = rnorm(n = 20, mean = 5.0, sd = 1.0))
df4 = tibble(var = 'D', val = rnorm(n = 10, mean = 4.0, sd = 0.5))
df5 = tibble(var = 'E', val = rnorm(n = 13, mean = 5.0, sd = 1.5))

main_df <- rbind(df1, df2, df3, df4, df5) %>%
  mutate(Variable = as_factor(var),
         value = round(val, 2)) %>%
  select(Variable, value)

# df for dot strip plot
set.seed(2000)
dfa = tibble(var = LETTERS[seq( from = 1, to = 20 )], val = sample(1:100, 20), year = '2018')
dfb = tibble(var = LETTERS[seq( from = 1, to = 20 )], val = sample(1:100, 20), year = '2019')
dfc = tibble(var = LETTERS[seq( from = 1, to = 20 )], val = sample(1:100, 20), year = '2020')
dfd = tibble(var = LETTERS[seq( from = 1, to = 20 )], val = sample(1:100, 20), year = '2021')
dfe = tibble(var = LETTERS[seq( from = 1, to = 20 )], val = sample(1:100, 20), year = '2022')

df_for_rank_dotstrip <- rbind(dfa, dfb, dfc, dfd, dfe) %>%
  rename(Variable = var,
         value = val)

# df for bump
set.seed(2021)
df_6 = tibble(team = c('A','B','C','D','E'),
              game_1 = sample(1:5),
              game_2 = sample(1:5),
              game_3 = sample(1:5),
              game_4 = sample(1:5),
              game_5 = sample(1:5))

df_for_bump <- df_6 %>%
  pivot_longer(!team, names_to = 'game', values_to = 'position') %>%
  mutate(game = as.numeric(recode(game,
                                  game_1 = 1,
                                  game_2 = 2,
                                  game_3 = 3,
                                  game_4 = 4,
                                  game_5 = 5)))

# df for slope
df_for_slope = tibble(var = c('A','B','C','D', 'E',
                              'A','B','C','D', 'E',
                              'A','B','C','D', 'E'),
                      year = c('2010','2010','2010','2010','2010',
                               '2015','2015','2015','2015','2015',
                               '2020','2020','2020','2020','2020'),
                      val = c(10,8,6,4,2,
                              13,9,11,5,7,
                              12,13,9,7,5))



# palette
palette_ranking <- c('#3c0d03',
                     '#8d1c06',
                     '#e67424',
                     '#ed9b49',
                     '#f5c34d')

#theme
theme_ranking <- theme_minimal() +
  theme(
    plot.title = element_text(color = '#3c0d03',
                              hjust = 0.5,
                              face = 'bold'),
    plot.subtitle = element_text(color = '#3c0d03',
                                 hjust = 0.5),
    plot.caption = element_text(color = '#3c0d03',
                                hjust = 1),
    axis.title = element_text(color = '#8d1c06',
                              face = 'bold'),
    axis.text = element_text(color = '#8d1c06',
                             face = 'bold'),
    panel.background = element_rect(fill = '#ffffff',
                                    color = NA),
    plot.background = element_rect(fill = '#ffffff',
                                   color = 'lightblue')
  )

# Ordered Bar
ordered_bar <- main_df %>%
  group_by(Variable) %>%
  summarise(number_of_data = n()) %>%
  ggplot(aes(x = reorder(Variable, -number_of_data),
             y = number_of_data,
             color = reorder(Variable, -number_of_data),
             fill = reorder(Variable, -number_of_data))) +
  geom_col(width = .6) +
  scale_color_manual(values = palette_ranking,
                     guide = 'none') +
  scale_fill_manual(values = palette_ranking,
                    guide = 'none') +
  theme_ranking +
  xlab('Variable') +
  ylab('number of data') +
  labs(title = 'Ordered Bar',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("ranking_ordered_bar.png", plot(ordered_bar), width = 7, height = 5, dpi = 300)

# Ordered Column
ordered_column <- main_df %>%
  group_by(Variable) %>%
  summarise(number_of_data = n()) %>%
  ggplot(aes(x = number_of_data,
             y = reorder(Variable, number_of_data),
             color = reorder(Variable, -number_of_data),
             fill = reorder(Variable, -number_of_data))) +
  geom_col(width = .6) +
  scale_color_manual(values = palette_ranking,
                     guide = 'none') +
  scale_fill_manual(values = palette_ranking,
                    guide = 'none') +
  theme_ranking +
  xlab('number of data') +
  ylab('Variable') +
  labs(title = 'Ordered Column',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("ranking_ordered_column.png", plot(ordered_column), width = 7, height = 5, dpi = 300)

# Ordered Proportional Symbol
ordered_prop_symbol <- main_df %>%
  group_by(Variable) %>%
  summarise(number_of_data = n()) %>%
  ggplot(aes(x = reorder(Variable, -number_of_data),
             y = 0,
             color = reorder(Variable, -number_of_data),
             fill = reorder(Variable, -number_of_data))) +
  geom_point(aes(size = number_of_data), show.legend = FALSE) +
  geom_text(aes(label = Variable),
            size = 7,
            color = 'white') +
  scale_size_area(max_size = 40) +
  scale_color_manual(values = palette_ranking,
                     guide = 'none') +
  scale_fill_manual(values = palette_ranking,
                    guide = 'none') +
  scale_y_continuous(limits = c(-1,1)) +
  theme_ranking +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = 'Ordered Proportional Symbol',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("ranking_ordered_prop_symbol.png", plot(ordered_prop_symbol), width = 7, height = 5, dpi = 300)

# Dot Strip Plot
dot_strip_plot <- df_for_rank_dotstrip %>%
  ggplot(aes(x = value,
             y = year,
             color = year,
             fill = year)) +
  geom_point(alpha = .7,
             size = 7) +
  geom_text(aes(label = Variable),
            size = 3,
            color = 'white') +
  stat_summary(fun = "median",
               geom = "point",
               shape = 124,
               size = 10, ) + 
  scale_color_manual(values = palette_ranking,
                     guide = 'none') +
  scale_fill_manual(values = palette_ranking,
                    guide = 'none') +
  theme_ranking +
  theme(
    axis.title.y = element_blank()
  ) +
  labs(title = 'Dot Strip Plot',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("ranking_dot_strip_plot.png", plot(dot_strip_plot), width = 7, height = 5, dpi = 300)

# Slope Graph
slope_graph <- CGPfunctions::newggslopegraph(
  dataframe = df_for_slope,
  Times = year,
  Measurement = val,
  Grouping = var,
  Title = 'Slope Graph',
  SubTitle = '2010 - 2015 - 2020',
  Caption = 'visualization by PanggahDPutra, 2022',
  XTextSize = 12,
  YTextSize = 4,
  TitleTextSize = 14,
  SubTitleTextSize = 10,
  CaptionTextSize = 8,
  TitleJustify = "center",
  SubTitleJustify = "center",
  CaptionJustify = "right",
  LineThickness = 1.2,
  LineColor = palette_ranking,
  DataTextSize = 5,
  DataTextColor = '#8d1c06',
  DataLabelPadding = 0.1,
  DataLabelFillColor = '#ffffff',
  WiderLabels = TRUE) +
  theme(plot.title = element_text(color = '#3c0d03'),
        plot.subtitle = element_text(color = '#3c0d03'),
        plot.caption = element_text(color = '#3c0d03'),
        panel.background = element_rect(fill = '#ffffff',
                                        color = NA),
        plot.background = element_rect(fill = '#ffffff',
                                       color = 'lightblue')) 

ggsave("ranking_slope_graph.png", plot(slope_graph), width = 7, height = 5, dpi = 300)

# Lollipop
lollipop <- main_df %>%
  group_by(Variable) %>%
  summarise(number_of_data = n()) %>%
  ggplot(aes(x = reorder(Variable, -number_of_data),
             y = number_of_data,
             color = Variable,
             fill = Variable)) +
  geom_col(alpha = 1,
           width = .05,
           color = '#3c0d03',
           fill = '#3c0d03',
           show.legend = FALSE) +
  geom_point(alpha = 1,
             size = 24,
             color = '#ed9b49',
             show.legend = FALSE) +
  geom_point(alpha = 1,
             size = 21,
             color = '#e67424',
             show.legend = FALSE) +
  geom_point(alpha = 1,
             size = 18,
             color = '#8d1c06',
             show.legend = FALSE) +
  geom_point(alpha = 1,
             size = 15,
             color = '#3c0d03',
             show.legend = FALSE) +
  geom_text(aes(label = number_of_data), 
            size = 5,
            color = '#f5c34d') +
  scale_y_continuous(limits = c(0,35)) +
  theme_ranking +
  theme(axis.text.y = element_blank()) +
  xlab('Variable') +
  ylab('number of data') +
  labs(title = 'Lollipop',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("ranking_lollipop.png", plot(lollipop), width = 7, height = 5, dpi = 300)

# Bump Chart
bump <- df_for_bump %>%
  ggplot(aes(x = game,
             y = position,
             color = team)) +
  ggbump::geom_bump(size = 3,
                    smooth = 20,
                    show.legend = FALSE) +
  geom_point(size = 7) +
  geom_text(aes(label = position),
            size = 4,
            color = 'white') +
  geom_text(data = df_for_bump %>% filter(game == min(game)),
            aes(x = game - .1,
                label = team),
            size = 5,
            hjust = 1) +
  geom_text(data = df_for_bump %>% filter(game == max(game)),
            aes(x = game + .1,
                label = team),
            size = 5,
            hjust = 0) +
  scale_color_manual(values = palette_ranking,
                     guide = 'none') +
  scale_y_reverse() +
  theme_ranking +
  xlab('Game-') +
  ylab('Ranking') +
  labs(title = 'Bump Chart',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("ranking_bump.png", plot(bump), width = 7, height = 5, dpi = 300)

# df for dot strip plot
df_for_dotstrip = tibble(Variable = c('A','B','C','D', 'E',
                                      'A','B','C','D', 'E',
                                      'A','B','C','D', 'E'),
                         year = c('2018','2018','2018','2018','2018',
                               '2019','2019','2019','2019','2019',
                               '2020','2020','2020','2020','2020'),
                         value = c(1,2,3,4,5,
                                   2,4,5,3,1,
                                   3,5,2,1,4))