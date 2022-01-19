# load packages
library(tidyverse)

# dataset
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

ggsave("1_ordered_bar.png", plot(ordered_bar), width = 8, height = 5, dpi = 300)

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

ggsave("2_ordered_column.png", plot(ordered_column), width = 8, height = 5, dpi = 300)

# Ordered Dot Strip Plot
ordered_dot_strip_plot <- main_df %>%
  ggplot(aes(x = reorder(Variable, -value),
             y = value,
             color = reorder(Variable, -value),
             fill = reorder(Variable, -value))) +
  geom_point(alpha = .5, size = 5) +
  scale_color_manual(values = palette_ranking,
                     guide = 'none') +
  scale_fill_manual(values = palette_ranking,
                    guide = 'none') +
  scale_y_continuous(limits = c(0,10),
                     breaks = c(0,2,4,6,8,10)) +
  stat_summary(fun = 'max',
               geom = "text",
               label = "max",
               size = 5,
               vjust = -1) + 
  theme_ranking +
  xlab('Variable') +
  labs(title = 'Ordered Dot Strip Plot',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("3_ordered_dot_strip_plot.png", plot(ordered_dot_strip_plot), width = 8, height = 5, dpi = 300)
