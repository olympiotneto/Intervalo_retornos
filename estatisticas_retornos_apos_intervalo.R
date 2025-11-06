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



# Exemplo: Baixar dados de uma ação (você pode substituir pelos seus dados)
bova11 <- get_cached_data("BOVA11.SA", "2021-11-03", "2025-11-05")


# Calcular retornos diários
dados_retornos <- bova11 %>%
  tq_mutate(select = adjusted,
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "retorno")

# DEFINA SUA FAIXA AQUI
limite_inferior <- 0.019  # -2%
limite_superior <- 0.02   # +2%

# Identificar dias onde o retorno está dentro da faixa
dados_com_flag <- dados_retornos %>%
  mutate(
    dentro_faixa = retorno >= limite_inferior & retorno <= limite_superior,
    id_evento = ifelse(dentro_faixa, row_number(), NA)
  ) %>%
  filter(!is.na(id_evento))

# Função para extrair os próximos 4 dias de retornos
extrair_proximos_dias <- function(data_ref, df_completo, n_dias = 4) {
  idx <- which(df_completo$date == data_ref)
  
  if(length(idx) == 0 || idx + n_dias > nrow(df_completo)) {
    return(NULL)
  }
  
  df_completo %>%
    slice((idx + 1):(idx + n_dias)) %>%
    mutate(
      data_evento = data_ref,
      dia_apos = row_number()
    ) %>%
    select(data_evento, dia_apos, date, retorno)
}

# Aplicar para todos os eventos
analise_pos_evento <- dados_com_flag %>%
  pull(date) %>%
  map_dfr(~extrair_proximos_dias(.x, dados_retornos))

# Resumo estatístico dos retornos nos 4 dias seguintes
resumo_por_dia <- analise_pos_evento %>%
  group_by(dia_apos) %>%
  summarise(
    n_eventos = n(),
    retorno_medio = mean(retorno, na.rm = TRUE),
    retorno_mediano = median(retorno, na.rm = TRUE),
    desvio_padrao = sd(retorno, na.rm = TRUE),
    min = min(retorno, na.rm = TRUE),
    max = max(retorno, na.rm = TRUE),
    prob_positivo = mean(retorno > 0, na.rm = TRUE)
  )

print("=== RESUMO DOS RETORNOS NOS 4 DIAS SEGUINTES ===")
print(resumo_por_dia)

# Visualização
grafico_boxplot <- analise_pos_evento %>%
  ggplot(aes(x = factor(dia_apos), y = retorno)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Distribuição dos Retornos nos 4 Dias Após Evento",
    subtitle = paste0("Evento: Retorno entre ", 
                      scales::percent(limite_inferior,accuracy = 0.01), " e ", 
                      scales::percent(limite_superior, accuracy = 0.01)),
    x = "Dia Após o Evento",
    y = "Retorno Diário"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

grafico_media <- resumo_por_dia %>%
  ggplot(aes(x = dia_apos, y = retorno_medio)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Retorno Médio por Dia Após o Evento",
    x = "Dia Após o Evento",
    y = "Retorno Médio"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

print(grafico_boxplot)
print(grafico_media)

# Retorno acumulado nos 4 dias
retorno_acumulado <- analise_pos_evento %>%
  group_by(data_evento) %>%
  summarise(retorno_acum_4d = prod(1 + retorno) - 1)

print("=== ESTATÍSTICAS DO RETORNO ACUMULADO (4 DIAS) ===")
print(summary(retorno_acumulado$retorno_acum_4d))

# Gráfico do retorno acumulado
grafico_acumulado <- retorno_acumulado %>%
  ggplot(aes(x = retorno_acum_4d)) +
  geom_histogram(fill = "steelblue", alpha = 0.7, bins = 30) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Distribuição do Retorno Acumulado (4 Dias)",
    x = "Retorno Acumulado",
    y = "Frequência"
  ) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal()

print(grafico_acumulado)