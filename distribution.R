# load packages
library(tidyverse)   
library(ggalt)        # to makes Dumbbell Plot
library(ggdist)       # to makes Stat Dot Plot
library(ggbeeswarm)   # to makes Beeswarm Plot
library(patchwork)    # to makes annotation on Population Pyramid
library(ggridges)     # to makes Stat Density Ridges Plot

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

# df for pyramid
set.seed(2000)
df1 = tibble(gender = 'Male', age = '15-20',
             value = sample((5:10), size = 1))
df2 = tibble(gender = 'Male', age = '21-25',
             value = sample((10:15), size = 1))
df3 = tibble(gender = 'Male', age = '26-30',
             value = sample((15:20), size = 1))
df4 = tibble(gender = 'Male', age = '31-35',
             value = sample((20:25), size = 1))
df5 = tibble(gender = 'Male', age = '36-40',
             value = sample((25:30), size = 1))
df6 = tibble(gender = 'Male', age = '41-45',
             value = sample((30:35), size = 1))
df7 = tibble(gender = 'Male', age = '46-50',
             value = sample((35:40), size = 1))
df8 = tibble(gender = 'Male', age = '51-55',
             value = sample((40:45), size = 1))
df9 = tibble(gender = 'Male', age = '56-60',
             value = sample((45:50), size = 1))
df10 = tibble(gender = 'Female', age = '15-20',
              value = sample((5:10), size = 1))
df11 = tibble(gender = 'Female', age = '21-25',
              value = sample((10:15), size = 1))
df12 = tibble(gender = 'Female', age = '26-30',
              value = sample((15:20), size = 1))
df13 = tibble(gender = 'Female', age = '31-35',
              value = sample((20:25), size = 1))
df14 = tibble(gender = 'Female', age = '36-40',
              value = sample((25:30), size = 1))
df15 = tibble(gender = 'Female', age = '41-45',
              value = sample((30:35), size = 1))
df16 = tibble(gender = 'Female', age = '46-50',
              value = sample((35:40), size = 1))
df17 = tibble(gender = 'Female', age = '51-55',
              value = sample((40:45), size = 1))
df18 = tibble(gender = 'Female', age = '56-60',
              value = sample((45:50), size = 1))

df_for_pop_pyramid <- rbind(df1, df2, df3, df4, df5, df6,
                            df7,df8, df9, df10, df11, df12,
                            df13, df14, df15, df16, df17, df18) %>%
  mutate(age = as.factor(age))

# df for cumulative curve
set.seed(123)
df_for_cum_curve = tibble(
  var = c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A',
          'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B'),
  year = c('2015', '2016', '2017', '2018',
           '2019', '2020', '2021', '2022',
           '2015', '2016', '2017', '2018',
           '2019', '2020', '2021', '2022'),
  val = c(sample(10:50, size = 4, replace = TRUE),
          sample(50:100, size = 4, replace = TRUE),
          sample(10:50, size = 4, replace = TRUE),
          sample(50:100, size = 4, replace = TRUE)))

df_for_cum_curve <- df_for_cum_curve %>%
  group_by(var) %>%
  mutate(cumulative_val = cumsum(val))

# palette
palette_distr <-c('#4e3910',
                  '#845d29',
                  '#4fb6ca',
                  '#178f92',
                  '#175f5d')

# theme
theme_distr <- theme_minimal() +
  theme(
    plot.title = element_text(color = '#1d1f54',
                              hjust = 0.5,
                              face = 'bold'),
    plot.subtitle = element_text(color = '#1d1f54',
                                 hjust = 0.5),
    plot.caption = element_text(color = '#1d1f54',
                                hjust = 1),
    axis.title = element_text(color = '#1d1f54',
                              face = 'bold'),
    axis.text = element_text(color = '#1d1f54',
                             face = 'bold'),
    panel.background = element_rect(fill = '#FFFCDE',
                                    color = NA),
    plot.background = element_rect(fill = '#FFFCDE',
                                   color = 'lightblue')
  )

# Histogram
histogram <- main_df %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 25,
                 alpha = .9,
                 fill = '#178f92',
                 color = '#175f5d') +
  scale_x_continuous(limits = c(0,10),
                     breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(limits = c(0,15),
                     breaks = c(0,5,10,15)) +
  theme_distr +
  ylab('number of data') +
  labs(title = 'Histogram',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("dist_histogram.png", plot(histogram), width = 7, height = 5, dpi = 300)

# Dot Plot (Dumbbell Plot)
dumbbell_plot <- main_df %>%
  group_by(Variable) %>%
  summarise(min = min(value), max = max(value)) %>%
  ggplot(aes(x = min,
             xend = max,
             y = Variable,
             color = Variable,
             fill = Variable), ) +
  ggalt::geom_dumbbell(size =3,
                       size_x = 12,
                       size_xend = 12,
                       color = '#d8c29d',
                       colour_x = '#d8c29d',
                       colour_xend = '#d8c29d') +
  ggalt::geom_dumbbell(size = 3,
                       size_x = 9,
                       size_xend = 9,
                       color = '#d8c29d',
                       colour_x = '#4e3910',
                       colour_xend = '#175f5d') +
  geom_text(aes(x = min,
                y = Variable,
                label = round(min, 1)), 
            size = 3,
            color = 'white') +
  geom_text(aes(x = max,
                y = Variable,
                label = round(max, 1)), 
            size = 3,
            color = 'white') +
  scale_color_manual(values = palette_distr,
                     guide = 'none') +
  scale_fill_manual(values = palette_distr,
                    guide = 'none') +
  scale_x_continuous(limits = c(0,10),
                     breaks = c(0,2,4,6,8,10)) +
  theme_distr +
  xlab('min <<<value>>> max') +
  labs(title = 'Dot Plot (Dumbbell Plot)',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("dist_dumbbell_plot.png", plot(dumbbell_plot), width = 7, height = 5, dpi = 300)

# Dot Strip Plot
dot_strip_plot <- main_df %>%
  ggplot(aes(x = Variable,
             y = value,
             color = Variable,
             fill = Variable)) +
  geom_point(alpha = .5, size = 5) +
  scale_color_manual(values = palette_distr,
                     guide = 'none') +
  scale_fill_manual(values = palette_distr,
                    guide = 'none') +
  scale_y_continuous(limits = c(0,10),
                     breaks = c(0,2,4,6,8,10)) +
  theme_distr +
  labs(title = 'Dot Strip Plot',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("dist_dot_strip_plot.png", plot(dot_strip_plot), width = 7, height = 5, dpi = 300)

# Barcode Plot
barcode_plot <- main_df %>%
  ggplot(aes(x = Variable,
             y = value,
             color = Variable,
             fill = Variable)) +
  geom_point(shape = 95,
             size = 25,
             alpha = .7) +
  scale_color_manual(values = palette_distr,
                     guide = 'none') +
  scale_fill_manual(values = palette_distr,
                    guide = 'none') +
  scale_y_continuous(limits = c(0,10),
                     breaks = c(0,2,4,6,8,10)) +
  theme_distr +
  labs(title = 'Barcode Plot',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("dist_barcode_plot.png", plot(barcode_plot), width = 7, height = 5, dpi = 300)

# Box Plot
box_plot <- main_df %>%
  ggplot(aes(x = Variable,
             y = value,
             color = Variable,
             fill = Variable)) +
  geom_boxplot(alpha = .7,
               size = .5,
               outlier.size = 2) +
  scale_color_manual(values = palette_distr,
                     guide = 'none') +
  scale_fill_manual(values = palette_distr,
                    guide = 'none') +
  scale_y_continuous(limits = c(0,10),
                     breaks = c(0,2,4,6,8,10)) +
  theme_distr +
  labs(title = 'Box Plot',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("dist_box_plot.png", plot(box_plot), width = 7, height = 5, dpi = 300)

# Violin Plot
violin_plot <- main_df %>%
  ggplot(aes(x = Variable,
             y = value,
             color = Variable,
             fill = Variable)) +
  geom_violin(size = .5,
              bw = .25) +
  scale_color_manual(values = palette_distr,
                     guide = 'none') +
  scale_fill_manual(values = palette_distr,
                    guide = 'none') +
  scale_y_continuous(limits = c(0,10),
                     breaks = c(0,2,4,6,8,10)) +
  theme_distr +
  labs(title = 'Violin Plot',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("dist_violin_plot.png", plot(violin_plot), width = 7, height = 5, dpi = 300)

# Population Pyramid
female_side <- df_for_pop_pyramid %>%
  filter(gender == 'Female') %>%
  ggplot(aes(x = value,
             y = reorder(age, desc(age)),
             fill = gender)) +
  geom_col(fill = '#178f92') +
  geom_text(aes(label = value),
            hjust = 2,
            color = 'white') +
  scale_x_continuous(limits = c(0, 50)) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.subtitle = element_text(color = '#4fb6ca',
                                 face = 'bold'),
    axis.title.x = element_text(color = '#1d1f54',
                                face = 'bold'),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = '#4fb6ca',
                               face = 'bold'),
    panel.background = element_rect(fill = '#FFFCDE',
                                    color = NA),
    plot.background = element_rect(fill = '#FFFCDE',
                                   color = '#FFFCDE')
  ) +
  xlab('Female') +
  labs(subtitle = 'Age')

male_side <- df_for_pop_pyramid %>%
  filter(gender == 'Male') %>%
  ggplot(aes(x = -value,
             y = reorder(age, desc(age)),
             fill = gender)) +
  geom_col(fill = '#845d29') +
  geom_text(aes(label = value),
            hjust = -1,
            color = 'white') +
  scale_x_continuous(limits = c(-50, 0)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(color = '#1d1f54',
                                face = 'bold'),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = '#FFFCDE',
                                    color = NA),
    plot.background = element_rect(fill = '#FFFCDE',
                                   color = '#FFFCDE')
  ) +
  xlab('Male')

pop_pyramid <- (male_side|female_side) +
  patchwork::plot_annotation(
    title = "Population Pyramid",
    caption = 'visualization by PanggahDPutra, 2022',
    theme = theme(
      plot.title = element_text(color = '#1d1f54',
                                hjust = 0.5,
                                face = 'bold'),
      plot.caption = element_text(color = '#1d1f54',
                                  hjust = 1),
      panel.background = element_rect(fill = '#FFFCDE',
                                      color = NA),
      plot.background = element_rect(fill = '#FFFCDE',
                                     color = 'lightblue')))

ggsave("dist_pop_pyramid.png", plot(pop_pyramid), width = 7, height = 5, dpi = 300)

# Cumulative Curve 
cumulative_curve <- df_for_cum_curve %>%
  ggplot(aes(x = year,
             y = cumulative_val,
             group = var,
             color = var)) +
  geom_smooth(size = 1.5,
              se = FALSE) +
  geom_text(data = subset(df_for_cum_curve,year == '2022'),
            aes(label = var),
            hjust = -1) +
  scale_color_manual(values = c('#d8c29d', '#4fb6ca'),
                     guide = 'none') +
  scale_y_continuous(limits = c(0,500)) +
  theme_distr +
  ylab('cumulative value') +
  labs(title = 'Cumulative Curve',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("dist_cumulative_curve.png", plot(cumulative_curve), width = 7, height = 5, dpi = 300)

# Frequency Polygons 
freq_polygons <- main_df %>%
  ggplot(aes(x = value)) +
  geom_freqpoly(bins = 25,
                size = 1.5,
                color = '#4fb6ca') +
  scale_x_continuous(limits = c(0,10),
                     breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(limits = c(0,15),
                     breaks = c(0,5,10,15)) +
  theme_distr +
  ylab('frequency') +
  labs(title = 'Frequency Polygons',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("dist_freq_polygons.png", plot(freq_polygons), width = 7, height = 5, dpi = 300)

# Beeswarm
beeswarm <- main_df %>%
  ggplot(aes(x = Variable,
             y = value,
             color = Variable,
             fill = Variable)) +
  ggbeeswarm::geom_beeswarm(alpha = .7,
                            size = 8,
                            cex = 3.5,
                            show.legend = FALSE) +
  scale_color_manual(values = palette_distr,
                     guide = 'none') +
  scale_fill_manual(values = palette_distr,
                    guide = 'none') +
  scale_y_continuous(limits = c(0,10),
                     breaks = c(0,2,4,6,8,10)) +
  theme_distr +
  labs(title = 'Beeswarm',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("dist_beeswarm.png", plot(beeswarm), width = 7, height = 5, dpi = 300)