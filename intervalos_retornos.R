library(tidyquant)
library(tidyverse)


papel <- tq_get(
  "DIRR3.SA", 
  from = "2025-02-28", 
  to = "2025-08-29"
) |> 
  tq_mutate(select = adjusted,               
            mutate_fun = ROC,               
            n = 1,                
            col_rename ='return_d',
            type ='discrete'
  ) |> 
  na.omit()

range(papel$return_d)

breaks <- seq(-0.22, 0.12, by = 0.02)

ggplot(papel, aes(x = return_d)) +
  geom_histogram(
    aes(
      y = after_stat(count / sum(count))),
    breaks = breaks,
    colour = "white",
    fill = "royalblue") +
  geom_label(
    aes(
      label = scales::percent(
        after_stat(
          count / sum(count)
        ), 
        accuracy = 0.1),
      y = after_stat(count / sum(count))),
    stat = "bin",
    breaks = breaks,
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent,
                     n.breaks = 20)+
  scale_x_continuous(labels = scales::percent,
                     n.breaks = 20)+
  theme_minimal()

# Tabela de freqências
tab_freq <- papel %>%
  mutate(interval = cut(return_d, breaks = breaks, right = FALSE, include.lowest = TRUE)) %>%
  janitor::tabyl(interval) %>%
  mutate(cum_freq = cumsum(percent)) |> 
  janitor::adorn_pct_formatting(,,,,percent:cum_freq)


# Estatísticas descritivas

papel |> 
  select(date, return_d) |>
  as.xts() |>
  PerformanceAnalytics::table.Stats()


papel |>
  select(date, return_d) |>
  as.xts() |>
  PerformanceAnalytics::AverageDrawdown()
