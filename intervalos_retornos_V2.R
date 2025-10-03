library(tidyquant)
library(tidyverse)


# Criamos um environment para servir de cache
.cache_env <- new.env()

# Função que busca dados, mas só baixa de novo se necessário
get_cached_data <- function(ativo, data_ini, data_fin) {
  
  # Criamos uma chave única com base no ticker e datas
  key <- paste(ativo, data_ini, data_fin, sep = "_")
  
  if (!exists(key, envir = .cache_env)) {
    message("📥 Baixando dados de ", ativo, "...")
    
    dados <- tq_get(
      ativo,
      from = data_ini,
      to   = data_fin
    )
    
    # Guarda no environment
    assign(key, dados, envir = .cache_env)
  } else {
    message("✅ Usando cache para ", ativo)
  }
  
  # Retorna o objeto armazenado
  get(key, envir = .cache_env)
}

# Agora adaptamos sua função para usar esse cache:
calc_prob <- function(ativo, data_ini = "2023-01-01", data_fin = "2025-08-31",
                      nro = 1, valor = 0.01) {
  
  papel <- get_cached_data(ativo, data_ini, data_fin) |>
    tq_mutate(
      select = adjusted,               
      mutate_fun = ROC,               
      n = nro,                
      col_rename = "return_d",
      type = "discrete"
    ) |>
    drop_na() |>
    summarise(prob = sum(return_d <= valor) / n())
  
  return(papel)
}



# Cálculo isolado

calc_prob(ativo = "WEGE3.SA", data_ini = "2023-01-01",
          data_fin ="2025-09-19" ,
          nro = 44, valor = ((40.05/36.35)-1))

#Mapeando a função
probs <- map_df(8:1, ~calc_prob(ativo = "PETR4.SA", data_ini = "2024-01-01",
                                data_fin ="2025-09-08" ,
                                nro = .x, valor = ((6.59/7.78)-1))) |>
  mutate(n = 8:1) |>
  select(n, prob)



#Gráfico
ggplot(probs, aes(x = n, y = prob)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  # scale_x_continuous(breaks = 13:1) +
  scale_x_reverse(breaks = 8:1)+
  theme_minimal() +
  labs(
    x = "Número de dias",
    y = "Probabilidade"
  ) +
  geom_text(aes(label = scales::percent(prob, accuracy = 0.1)), vjust = -0.5, size = 3)







range(papel$return_d)

breaks <- seq(-0.10, 0.20, by = 0.01)

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
