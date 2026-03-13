# Carregando bibliotecas necessárias
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyquant)
library(tidyverse)
library(plotly)
library(DT)
library(PerformanceAnalytics)
library(xts)

options(OutDec = ",")

# ==========================================
# FUNÇÕES AUXILIARES (Backend)
# ==========================================

# Environment para cache
.cache_env <- new.env()

# Função para buscar dados com cache
get_cached_data <- function(ativo, data_ini, data_fin) {
  key <- paste(ativo, data_ini, data_fin, sep = "_")
  
  if (!exists(key, envir = .cache_env)) {
    withProgress(message = paste("Baixando dados de", ativo), value = 0, {
      incProgress(0.5)
      
      dados <- tryCatch({
        tq_get(
          ativo,
          from = data_ini,
          to   = data_fin
        )
      }, error = function(e) {
        showNotification(
          paste("Erro ao baixar", ativo, ":", e$message),
          type = "error",
          duration = 5
        )
        return(NULL)
      })
      
      if (!is.null(dados)) {
        assign(key, dados, envir = .cache_env)
      }
      incProgress(0.5)
    })
  }
  
  get(key, envir = .cache_env)
}

# Função para calcular probabilidades
calc_prob <- function(ativo, data_ini, data_fin, nro = 1, valor = 0.01) {
  dados <- get_cached_data(ativo, data_ini, data_fin)
  
  if (is.null(dados)) return(NULL)
  
  papel <- dados %>%
    tq_mutate(
      select = adjusted,               
      mutate_fun = ROC,               
      n = nro,                
      col_rename = "return_d",
      type = "discrete"
    ) %>%
    drop_na() %>%
    summarise(prob = sum(return_d <= valor) / n())
  
  return(papel)
}

# Função para obter dados completos com retornos
get_returns_data <- function(ativo, data_ini, data_fin, nro = 1) {
  dados <- get_cached_data(ativo, data_ini, data_fin)
  
  if (is.null(dados)) return(NULL)
  
  dados %>%
    tq_mutate(
      select = adjusted,               
      mutate_fun = ROC,               
      n = nro,                
      col_rename = "return_d",
      type = "discrete"
    ) %>%
    drop_na()
}

# ==========================================
# INTERFACE DO USUÁRIO (UI)
# ==========================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Análise de Probabilidades - Ações BR",
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 350,
    
    # CSS customizado
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .small-box {
          border-radius: 10px;
        }
      "))
    ),
    
    # Menu de navegação
    sidebarMenu(
      menuItem("📊 Análise Principal", tabName = "main", icon = icon("chart-line")),
      menuItem("📈 Estatísticas", tabName = "stats", icon = icon("calculator")),
      menuItem("ℹ️ Sobre", tabName = "about", icon = icon("info-circle"))
    ),
    
    hr(),
    
    # Inputs principais
    h4("Parâmetros da Análise", style = "padding-left: 15px;"),
    
    div(style = "padding: 0 15px;",
        
        # Seleção do ativo
        pickerInput(
          "ativo",
          "Selecione o Ativo:",
          choices = c(
            "Petrobras PN" = "PETR4.SA",
            "BOVA11" = "BOVA11.SA",
            "Petrobras ON" = "PETR3.SA",
            "Vale" = "VALE3.SA",
            "Itaú Unibanco" = "ITUB4.SA",
            "Bradesco" = "BBDC4.SA",
            "Banco do Brasil" = "BBAS3.SA",
            "WEG" = "WEGE3.SA",
            "Ambev" = "ABEV3.SA",
            "B3" = "B3SA3.SA",
            "Magazine Luiza" = "MGLU3.SA",
            "Lojas Renner" = "LREN3.SA",
            "Natura" = "NTCO3.SA"
          ),
          selected = "PETR4.SA",
          options = list(
            style = "btn-primary",
            size = 10,
            `live-search` = TRUE
          )
        ),
        
        # Ou entrada manual
        textInput(
          "ativo_manual",
          "Ou digite o código (ex: VALE3):",
          value = ""
        ),
        
        # Datas
        dateRangeInput(
          "daterange",
          "Período de Análise:",
          start = Sys.Date() - 365,
          end = Sys.Date(),
          format = "dd/mm/yyyy",
          separator = " até ",
          language = "pt-BR"
        ),
        
        # Período de retorno
        sliderInput(
          "nro_dias",
          "Número de dias para retorno:",
          min = 1,
          max = 252,
          value = 5,
          step = 1
        ),
        
        # Valor alvo
        numericInput(
          "valor_inicial",
          "Valor inicial (R$):",
          value = 1.00,
           min = NA,
           max = NA,
          step = 0.01
        ),

        # Valor alvo
        numericInput(
          "valor_final",
          "Valor final (R$):",
          value = 1.00,
          min = NA,
          max = NA,
          step = 0.01
        ),
        
        # Meta de retorno
        tags$style("#meta_retorno { background-color: steelblue; 
                   color: white; 
                   font-weight: bold; }"),
        numericInput(
          "meta_retorno",
          "Meta de Retorno (%):",
          value = 1.00,
          min = NA,
          max = NA,
          step = 0.01
        ),
        
        # Taxa de Juros
        numericInput(
          "Selic",
          "Taxa Selic Anual (%):",
          value = 6,
          min = -100.00,
          max = 100.00,
          step = 0.25
        ),
        
        # Botão de atualizar
        br(),
        actionButton(
          "update",
          "Atualizar Análise",
          icon = icon("refresh"),
          class = "btn-success btn-block"
        )
    )
  ),
  
  # Body
  dashboardBody(
    tabItems(
      # Tab Principal
      tabItem(
        tabName = "main",
        
        # Primeira linha - KPIs
        fluidRow(
          valueBoxOutput("box_prob"),
          valueBoxOutput("box_media"),
          valueBoxOutput("box_volatilidade")
        ),
        
        # Segunda linha - Gráficos principais
        fluidRow(
          box(
            title = "Probabilidade por Período",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_prob_periodo", height = "400px")
          ),
          
          box(
            title = "Distribuição de Retornos",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_histogram", height = "400px")
          )
        ),
        
        # Terceira linha - Série temporal e tabela
        fluidRow(
          box(
            title = "Série Temporal de Retornos",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("plot_serie_temporal", height = "350px")
          ),
          
          box(
            title = "Tabela de Frequências",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            div(style = "overflow-x: auto;",
                DT::dataTableOutput("tabela_freq", height = "350px")
            )
          )
        )
      ),
      
      # Tab Estatísticas
      tabItem(
        tabName = "stats",
        
        fluidRow(
          box(
            title = "Estatísticas Descritivas",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            DT::dataTableOutput("stats_table")
          ),
          
          box(
            title = "Análise de Risco",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            uiOutput("risk_metrics")
          )
        ),
        
        fluidRow(
          box(
            title = "Análise de Quantis",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_quantis", height = "400px")
          )
        )
      ),
      
      # Tab Sobre
      tabItem(
        tabName = "about",
        box(
          title = "Sobre a Aplicação",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          h4("📊 Análise de Probabilidades Empíricas"),
          p("Esta aplicação foi desenvolvida para analisar probabilidades empíricas de retornos de ações da bolsa brasileira."),
          br(),
          h5("Funcionalidades:"),
          tags$ul(
            tags$li("Cálculo de probabilidades de retorno para diferentes períodos"),
            tags$li("Visualização interativa da distribuição de retornos"),
            tags$li("Estatísticas descritivas completas"),
            tags$li("Análise de risco e drawdown"),
            tags$li("Cache inteligente para otimizar downloads de dados")
          ),
          br(),
          h5("Dados:"),
          p("Os dados são obtidos em tempo real do Yahoo Finance através do pacote tidyquant."),
          br(),
          h5("Metodologia:"),
          p("A probabilidade empírica é calculada como a proporção de retornos observados que são menores ou iguais à meta estabelecida."),
          br(),
          tags$div(
            class = "alert alert-info",
            tags$strong("Aviso: "),
            "Esta ferramenta é para fins educacionais e não constitui recomendação de investimento."
          )
        )
      )
    )
  )
)

# ==========================================
# SERVIDOR (Server)
# ==========================================

server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    dados = NULL,
    ativo_usado = NULL
  )
  
 
  # Determinar qual ativo usar
  observe({
    if (input$ativo_manual != "") {
      values$ativo_usado <- glue::glue("{toupper(input$ativo_manual)}.SA") 
    } else {
      values$ativo_usado <- input$ativo
    }

  })
  
  # faz os cálculos da meta de retorno reativo
  retorno <-  reactive({
    v1 <- input$valor_inicial
    v2 <- input$valor_final
    retorno <- ((v2/v1)-1)*100
    return(retorno)
    }) |> 
    bindEvent(c(input$valor_inicial, input$valor_final))

  #Observa alteração nos inputs

  observeEvent(c(input$valor_inicial, input$valor_final), {

    valor <- retorno()

    updateNumericInput(
      session,
      inputId = "meta_retorno",
      value = valor |>
        scales::number(accuracy = 0.01),
      min = NA,
      max = NA,
      step = 0.01
    )
  },
  ignoreInit = TRUE
  )

  
  
  
  # Processar dados quando botão é clicado
  observeEvent(input$update, {
    req(values$ativo_usado)
    #Texto do kpi
    # browser()
    showNotification("Processando dados...", type = "default")
  
    
    values$dados <- get_returns_data(
      values$ativo_usado,
      input$daterange[1],
      input$daterange[2],
      input$nro_dias
    )
    
    if (!is.null(values$dados)) {
      showNotification("Dados atualizados com sucesso!", type = "message")
    }
    
  })
  
  #Grava o none reativo do papel parea usar quando clicar no botao
  
  papel_kpi <- reactive({
    return(values$ativo_usado)
  }) |> 
    bindEvent(input$update)
  
  # KPIs - Value Boxes
  output$box_prob <- renderValueBox({
    req(values$dados)
    
    prob <- values$dados %>%
      summarise(prob = sum(return_d <= (input$meta_retorno/100)) / n()) %>%
      pull(prob)


  
  #Pega o nome do ativo após apertar o botão
    
    
    valueBox(
      # value = paste0(round(prob * 100, 2), "%"),
      value = paste0(scales::percent(prob, accuracy = 0.01, decimal.mark = ",")),
      subtitle = paste("Prob. de retorno ≤", input$meta_retorno, "% em ", papel_kpi()),
      icon = icon("percentage"),
      color = if (prob > 0.6) "green" else if (prob > 0.4) "yellow" else "red"
    )
  })
  
  output$box_media <- renderValueBox({
    req(values$dados)
    
    media <- mean(values$dados$return_d, na.rm = TRUE)
    
    valueBox(
      value = paste0(scales::percent(media, accuracy = 0.01, decimal.mark = "," )),
      subtitle = paste("Retorno médio", input$nro_dias, "dias"),
      icon = icon("chart-line"),
      color = if (media < 0) "red" else "green"
    )
  })
  
  output$box_volatilidade <- renderValueBox({
    req(values$dados)
    
  vol <- sd(values$dados$return_d, na.rm = TRUE)

    
    valueBox(
      value = paste0(scales::percent(vol, accuracy = 0.01, decimal.mark = "," )),
      subtitle = paste0("Volatilidade média diária em ",input$nro_dias, " dias"),
      icon = icon("chart-area"),
      color = if (vol > 10) "green" else if (vol > 5) "yellow" else "red"
    )
  })
  
  # Gráfico de probabilidade por período
  output$plot_prob_periodo <- renderPlotly({
    req(values$dados)
    
    periodos <- c(1, 5, 10, 21, 42, 63, 126, 252)
    periodos <- periodos[periodos <= max(1, input$nro_dias * 2)]
    
    probs_df <- map_df(periodos, function(p) {
      calc_prob(
        values$ativo_usado,
        input$daterange[1],
        input$daterange[2],
        p,
        input$meta_retorno/100
      ) %>%
        mutate(periodo = p)
    })
    
    if (nrow(probs_df) > 0) {
      p <- plot_ly(probs_df, 
                   x = ~periodo, 
                   y = ~prob,
                   type = 'scatter',
                   mode = 'lines+markers',
                   line = list(color = 'rgb(22, 96, 167)', width = 3),
                   marker = list(size = 10, color = 'rgb(22, 96, 167)'),
                   text = ~paste('Período:', periodo, 'dias<br>',
                                 'Probabilidade:', round(prob*100, 1), '%'),
                   hovertemplate = '%{text}<extra></extra>')
      
      p <- p %>% layout(
        title = list(text = "Probabilidade vs Período de Retorno"),
        xaxis = list(title = "Período (dias)", gridcolor = 'rgb(230, 230, 230)'),
        yaxis = list(title = "Probabilidade", 
                     tickformat = '.0%',
                     gridcolor = 'rgb(230, 230, 230)'),
        plot_bgcolor = 'rgb(248, 248, 248)',
        paper_bgcolor = 'white',
        hovermode = 'closest'
      )
      
      p
    }
  })
  
  # Histograma de retornos
  output$plot_histogram <- renderPlotly({
    req(values$dados)
    
    p <- plot_ly(values$dados, 
                 x = ~return_d * 100,
                 type = 'histogram',
                 nbinsx = 30,
                 marker = list(color = 'rgba(66, 146, 198, 0.7)',
                               line = list(color = 'rgb(8, 48, 107)', width = 1)),
                 name = 'Frequência')
    
    # Adicionar linha vertical na meta
    p <- p %>% add_trace(
      x = c(input$meta_retorno, input$meta_retorno),
      y = c(0, max(hist(values$dados$return_d * 100, plot = FALSE)$counts)),
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'red', width = 2, dash = 'dash'),
      name = 'Meta'
    )
    
    p <- p %>% layout(
      title = list(text = paste("Distribuição de Retornos -", input$nro_dias, "dias")),
      xaxis = list(title = "Retorno (%)", gridcolor = 'rgb(230, 230, 230)'),
      yaxis = list(title = "Frequência", gridcolor = 'rgb(230, 230, 230)'),
      plot_bgcolor = 'rgb(248, 248, 248)',
      paper_bgcolor = 'white',
      showlegend = TRUE
    )
    
    p
  })
  
  # Série temporal
  output$plot_serie_temporal <- renderPlotly({
    req(values$dados)
    
    p <- plot_ly(values$dados, 
                 x = ~date, 
                 y = ~return_d * 100,
                 type = 'scatter',
                 mode = 'lines',
                 line = list(color = 'rgb(100, 100, 100)', width = 1),
                 fill = 'tozeroy',
                 fillcolor = 'rgba(100, 100, 100, 0.2)',
                 name = 'Retorno')
    
    # Adicionar linha de referência em zero
    p <- p %>% add_trace(
      x = range(values$dados$date),
      y = c(0, 0),
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'black', width = 1, dash = 'dash'),
      name = 'Zero'
    )
    
    # Adicionar linha da meta
    p <- p %>% add_trace(
      x = range(values$dados$date),
      y = c(input$meta_retorno, input$meta_retorno),
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'red', width = 1, dash = 'dash'),
      name = 'Meta'
    )
    
    p <- p %>% layout(
      title = list(text = "Evolução dos Retornos ao Longo do Tempo"),
      xaxis = list(title = "Data",
                   tickformat = "%m/%Y",
                   rangeslider = list(visible = TRUE),
                   gridcolor = 'rgb(230, 230, 230)'),
      yaxis = list(title = "Retorno (%)", gridcolor = 'rgb(230, 230, 230)'),
      plot_bgcolor = 'rgb(248, 248, 248)',
      paper_bgcolor = 'white',
      showlegend = TRUE
    )
    
    p
  })
  
  # Tabela de frequências
  output$tabela_freq <- DT::renderDataTable({
    req(values$dados)
    
    breaks <- seq(
      floor(min(values$dados$return_d * 100) / 1) * 1,
      ceiling(max(values$dados$return_d * 100) / 1) * 1,
      by = 1
    )
    
    if (length(breaks) > 20) {
      breaks <- seq(
        floor(min(values$dados$return_d * 100) / 2) * 2,
        ceiling(max(values$dados$return_d * 100) / 2) * 2,
        by = 2
      )
    }
    
    tab_freq <- values$dados %>%
      mutate(interval = cut(return_d * 100, breaks = breaks, right = FALSE, include.lowest = TRUE)) %>%
      count(interval) %>%
      mutate(
        freq_pct = n / sum(n) * 100,
        freq_acum = cumsum(freq_pct)
      ) %>%
      rename(
        "Intervalo (%)" = interval,
        "Frequência" = n,
        "Freq. (%)" = freq_pct,
        "Freq. Acum. (%)" = freq_acum
      )
    
    DT::datatable(
      tab_freq,
      options = list(
        pageLength = 5
        # dom = 't',
        # scrollY = "300px",
        # scrollCollapse = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(c("Freq. (%)", "Freq. Acum. (%)"), 1)
  })
  
  # Tabela de estatísticas
  output$stats_table <- DT::renderDataTable({
    req(values$dados)
    
    stats_xts <- values$dados %>%
      select(date, return_d) %>%
      filter(!is.na(return_d)) %>%
      column_to_rownames("date") %>%
      as.xts()
    
    if (nrow(stats_xts) > 0) {
      stats_df <- PerformanceAnalytics::table.Stats(stats_xts)
      
      # Mapear corretamente os nomes das estatísticas
      stats_names <- c(
        "Observations" = "Observações",
        "NAs" = "Valores NA",
        "Minimum" = "Mínimo",
        "Quartile 1" = "1º Quartil",
        "Median" = "Mediana",
        "Arithmetic Mean" = "Média Aritmética",
        "Geometric Mean" = "Média Geométrica",
        "Quartile 3" = "3º Quartil",
        "Maximum" = "Máximo",
        "SE Mean" = "Erro Padrão",
        "LCL Mean (0.95)" = "IC Inferior (95%)",
        "UCL Mean (0.95)" = "IC Superior (95%)",
        "Variance" = "Variância",
        "Stdev" = "Desvio Padrão",
        "Skewness" = "Assimetria",
        "Kurtosis" = "Curtose"
      )
      
      # Criar dataframe com as estatísticas disponíveis
      stats_clean <- data.frame(
        Estatística = stats_names[rownames(stats_df)],
        Valor = as.numeric(stats_df[,1])
      )
      
      # Ajustar valores que devem ser em percentual
      stats_clean$Valor[stats_clean$Estatística %in% c("Mínimo", "1º Quartil", "Mediana", 
                                                       "Média Aritmética", "Média Geométrica", 
                                                       "3º Quartil", "Máximo", "Erro Padrão",
                                                       "IC Inferior (95%)", "IC Superior (95%)",
                                                       "Variância", "Desvio Padrão")] <- 
        stats_clean$Valor[stats_clean$Estatística %in% c("Mínimo", "1º Quartil", "Mediana", 
                                                         "Média Aritmética", "Média Geométrica", 
                                                         "3º Quartil", "Máximo", "Erro Padrão",
                                                         "IC Inferior (95%)", "IC Superior (95%)",
                                                         "Variância", "Desvio Padrão")] * 100
      
      DT::datatable(
        stats_clean,
        options = list(
          pageLength = 20,
          dom = 't',
          scrollY = "400px",
          scrollCollapse = TRUE
        ),
        rownames = FALSE
      ) %>%
        formatRound("Valor", 2) %>%
        formatStyle(
          "Valor",
          background = styleColorBar(range(stats_clean$Valor, na.rm = TRUE), 'lightblue'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    } else {
      DT::datatable(data.frame(Mensagem = "Dados insuficientes"))
    }
  })
  
  # Métricas de risco
  output$risk_metrics <- renderUI({
    req(values$dados)
    
    # Preparar dados
    returns_clean <- values$dados$return_d[!is.na(values$dados$return_d)]
    # browser()
    if (length(returns_clean) > 0) {
      # Calcular VaR e CVaR
      # var_95 <- quantile(returns_clean, 0.05, na.rm = TRUE) * 100
      var_95 <- VaR(R = returns_clean , method = "modified") * 100 
      # returns_below_var <- returns_clean[returns_clean <= var_95 / 100]
      # cvar_95 <- if (length(returns_below_var) > 0) {
      #   mean(returns_below_var, na.rm = TRUE) * 100
      # } else {
      #   NA
      # }
      cvar_95 <- CVaR(R = returns_clean , method = "modified") * 100
      # Preparar série temporal para outras métricas
      stats_xts <- NULL
      max_dd <- NA
      avg_dd <- NA
      sharpe <- NA
      
      tryCatch({
        # Criar data frame limpo
        df_for_xts <- values$dados %>%
          select(date, return_d) %>%
          filter(!is.na(return_d))
        
        # Converter para xts
        if (nrow(df_for_xts) > 0) {
          stats_xts <- xts(
            x = df_for_xts$return_d,
            order.by = df_for_xts$date
          )
           # browser()
          # Calcular métricas se temos dados suficientes
          if (length(stats_xts) > 1) {
            # MaxDrawdown
            tryCatch({
              # dd_result <- maxDrawdown(stats_xts)
              # max_dd <- as.numeric(dd_result) * 100
              max_dd <- maxDrawdown(stats_xts) |> 
                scales::percent(accuracy = 0.01, decimal.mark = ",")
            }, error = function(e) {
              max_dd <- NA
            })
            
            # Average Drawdown
            tryCatch({
              avg_dd_result <- AverageDrawdown(stats_xts)
              avg_dd <- as.numeric(avg_dd_result) * 100
            }, error = function(e) {
              avg_dd <- NA
            })
            
            # Sharpe Ratio
            tryCatch({
              sharpe_result <- SharpeRatio(stats_xts, Rf = input$Selic*input$nro_dias/25200, FUN = "StdDev")
              sharpe <- as.numeric(sharpe_result[1])
            }, error = function(e) {
              sharpe <- NA
            })
            
            # Sharpe Ratio ajustado
            tryCatch({
              sharpe_ajus_result <- AdjustedSharpeRatio(stats_xts, Rf = input$Selic*input$nro_dias/25200)
              sharpe_ajus <- as.numeric(sharpe_ajus_result[1])
            }, error = function(e) {
              sharpe <- NA
            })
          }
        }
      }, error = function(e) {
        # Se houver erro, mantém os valores NA
      })
      
      # Criar UI
      tags$div(
        class = "risk-metrics",
        
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-body",

         # VaR
            tags$div(
              style = "margin-bottom: 15px;",
              tags$h5(
                # tags$icon("exclamation-triangle"),
                " Value at Risk (95%):"
              ),
              tags$h4(
                paste0(round(var_95, 2), "%"),
                class = if(var_95 < -10) "text-danger" else if(var_95 < -5) "text-warning" else "text-info"
              )
            ),

            # CVaR
            if (!is.na(cvar_95)) {
              tags$div(
                style = "margin-bottom: 15px;",
                tags$h5(
                  # tags$icon("chart-line"),
                  " CVaR (95%):"
                ),
                tags$h4(
                  paste0(round(cvar_95, 2), "%"),
                  class = if(cvar_95 < -10) "text-danger" else if(cvar_95 < -5) "text-warning" else "text-info"
                )
              )
            } else {
              tags$div()
            },

            # Max Drawdown
            if (!is.na(max_dd)) {
              tags$div(
                style = "margin-bottom: 15px;",
                tags$h5(
                  # tags$icon("arrow-down"),
                  " Drawdown Máximo:"
                ),
                tags$h4(
                  # paste0(round(max_dd, 2), "%"),
                  max_dd,
                  class = if(max_dd > 0.2) "text-danger" else if(max_dd > .10) "text-warning" else "text-success"
                )
              )
            } else {
              tags$div()
            },

            # Average Drawdown
            if (!is.na(avg_dd)) {
              tags$div(
                style = "margin-bottom: 15px;",
                tags$h5(
                  # tags$icon("arrows-alt-v"),
                  " Drawdown Médio:"
                ),
                tags$h4(
                  paste0(round(avg_dd, 2), "%"),
                  class = if(avg_dd > 10) "text-danger" else if(avg_dd > 5) "text-warning" else "text-success"
                )
              )
            } else {
              tags$div()
            },

            # Sharpe Ratio
            if (!is.na(sharpe) && !is.infinite(sharpe)) {
              tags$div(
                style = "margin-bottom: 15px;",
                tags$h5(
                  # tags$icon("balance-scale"),
                  paste0(" Sharpe Ratio (Selic: ",input$Selic,"% a.a.):")
                ),
                tags$h4(
                  round(sharpe, 3),
                  class = if(sharpe < 0) "text-danger" else if(sharpe > 1) "text-success" else "text-info"
                )
              )
            } else {
              tags$div()
            },
         
         # Sharpe Ratio ajustado
         if (!is.na(sharpe) && !is.infinite(sharpe)) {
           tags$div(
             style = "margin-bottom: 15px;",
             tags$h5(
               # tags$icon("balance-scale"),
               paste0(" Sharpe Ratio Ajustado (Selic: ",input$Selic,"% a.a.):")
             ),
             tags$h4(
               round(sharpe_ajus, 3),
               class = if(sharpe < 0) "text-danger" else if(sharpe > 1) "text-success" else "text-info"
             )
           )
         } else {
           tags$div()
         },

            tags$hr(),
            tags$small(
              class = "text-muted",
              paste("Métricas calculadas para o período de", input$nro_dias, "dias")
            )
          )
        )
      )
    } else {
      tags$div(
        class = "alert alert-warning",
        "Dados insuficientes para calcular métricas de risco"
      )
    }
  })
  
  # Gráfico de quantis
  output$plot_quantis <- renderPlotly({
    req(values$dados)
    
    quantis <- seq(0.01, 0.99, 0.01)
    valores_quantis <- quantile(values$dados$return_d * 100, quantis)
    
    df_quantis <- data.frame(
      Quantil = quantis * 100,
      Retorno = valores_quantis
    )
    
    p <- plot_ly(df_quantis, 
                 x = ~Quantil, 
                 y = ~Retorno,
                 type = 'scatter',
                 mode = 'lines',
                 line = list(color = 'rgb(67, 67, 67)', width = 2),
                 fill = 'tozeroy',
                 fillcolor = 'rgba(67, 67, 67, 0.2)')
    
    # Destacar quartis
    quartis_especiais <- c(5, 25, 50, 75, 95)
    valores_especiais <- quantile(values$dados$return_d * 100, quartis_especiais/100)
    
    p <- p %>% add_trace(
      x = quartis_especiais,
      y = valores_especiais,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 10, color = 'red'),
      name = 'Quartis Principais',
      text = ~paste('Q', quartis_especiais, ':', round(valores_especiais, 2), '%'),
      hovertemplate = '%{text}<extra></extra>'
    )
    
    p <- p %>% layout(
      title = list(text = "Análise de Quantis dos Retornos"),
      xaxis = list(title = "Quantil (%)", gridcolor = 'rgb(230, 230, 230)'),
      yaxis = list(title = "Retorno (%)", gridcolor = 'rgb(230, 230, 230)'),
      plot_bgcolor = 'rgb(248, 248, 248)',
      paper_bgcolor = 'white',
      showlegend = TRUE
    )
    
    p
  })
  
  # Auto-update on load - removido shinyjs
#   observe({
#     if (is.null(values$dados)) {
#       isolate({
#         updateActionButton(session, "update", label = "Carregar Dados Iniciais")
#       })
#     }
#   })
 }

# ==========================================
# EXECUTAR A APLICAÇÃO
# ==========================================

shinyApp(ui = ui, server = server)