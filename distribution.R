# load packages
library(tidyverse)   
library(ggdist)       # to makes Dot Plot
library(ggbeeswarm)   # to makes Beeswarm Plot
library(ggalt)        # to makes Dumbbell Plot

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

# Histograms
histograms <- main_df %>%
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

ggsave("dist_histograms.png", plot(histograms), width = 7, height = 5, dpi = 300)