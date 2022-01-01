#load packages
library(tidyverse)

#data preparation
set.seed(2000)
df1 = tibble(var = 'A', val = rnorm(n = 25, mean = 5.5, sd = 1.5))
df2 = tibble(var = 'B', val = rnorm(n = 15, mean = 4.5, sd = 1.0))
df3 = tibble(var = 'C', val = rnorm(n = 20, mean = 5.0, sd = 1.0))
df4 = tibble(var = 'D', val = rnorm(n = 10, mean = 4.0, sd = 0.5))
df5 = tibble(var = 'E', val = rnorm(n = 13, mean = 5.0, sd = 1.5))

data_vis <- rbind(df1, df2, df3, df4, df5) %>%
  mutate(Variable = as_factor(var),
         value = round(val, 2))

palette_ranking <- c("#0a3351", "#2f70a1", "#4692b0", "#72aeb6", "#abc9c8")

theme_ranking <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 1),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    axis.line.x.bottom = element_line( linetype = "solid")
  )

#ordered_bar
ordered_bar <- data_vis %>%
  group_by(Variable) %>%
  summarise(number_of_data = n()) %>%
  ggplot(aes(x = reorder(Variable, -number_of_data),
             y = number_of_data,
             color = reorder(Variable, -number_of_data),
             fill = reorder(Variable, -number_of_data))) +
  scale_color_manual(values = palette_ranking, guide = 'none') +
  scale_fill_manual(values = palette_ranking, guide = 'none') +
  theme_ranking +
  geom_col(alpha = .9, width = .6) +
  xlab('Variable') +
  ylab('number of data') +
  labs(title = 'Ordered Bar',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("1_ordered_bar.png", plot(ordered_bar), dpi = 300)

##########ordered_column_chart####
ordered_column <- data_vis %>%
  group_by(Variable) %>%
  summarise(number_of_data = n()) %>%
  ggplot(aes(y = reorder(Variable, number_of_data), x= number_of_data,
             color = reorder(Variable, -number_of_data), fill = reorder(Variable, -number_of_data))) +
  scale_color_manual(values = palette_ranking, guide = 'none') +
  scale_fill_manual(values = palette_ranking, guide = 'none') +
  theme_ranking +
  geom_col(alpha = .9, width = .6) +
  xlab('number of data') +
  ylab('Variable') +
  labs(title = 'Ordered Column',
       caption = 'visualization by PanggahDPutra, 2022')

ggsave("2_ordered_column.png", plot(ordered_column), dpi = 300)