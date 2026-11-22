library(shiny)
library(bslib)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)

df_pr <- readRDS("exportacoes_pr.rds")

ui <- page_navbar(
  title = tags$span(
    style = "display: inline-flex; align-items: center; gap: 12px; margin-right: 20px;",
    tags$img(
      src = "bandeira_do__pr.jpg",
      height = "34px",
      style = "border-radius: 4px;"
    ),
    "Paraná no Comércio Exterior"
  ),
  window_title = "Paraná no Comércio Exterior",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#003366",
    success = "#007A33",
    "navbar-dark-active-color" = "#FFFFFF",
    "navbar-dark-hover-color" = "#A9DFBF"
  ),
  fillable = FALSE,
  header = tags$style(HTML("
    .navbar-brand { padding-top: 0px; padding-bottom: 0; }
    .navbar .nav-link.active { color: #FFFFFF !important; font-weight: 600; }
  ")),
  
  sidebar = sidebar(
    selectInput(
      "filtro_mes",
      "Mês:",
      choices = c(
        "Todos" = "Todos",
        "Janeiro" = "01",
        "Fevereiro" = "02",
        "Março" = "03",
        "Abril" = "04",
        "Maio" = "05",
        "Junho" = "06",
        "Julho" = "07",
        "Agosto" = "08",
        "Setembro" = "09",
        "Outubro" = "10",
        "Novembro" = "11",
        "Dezembro" = "12"
      ),
      selected = "Todos"
    ),
    selectInput(
      "ano",
      "Ano:",
      choices = sort(unique(df_pr$ANO), decreasing = TRUE)
    ),
    selectInput(
      "via",
      "Via de Transporte:",
      choices = c("Todas", sort(unique(df_pr$VIA[!is.na(df_pr$VIA)]))),
      selected = "Todas"
    )
  ),
  nav_panel(
    "Panorama Comercial",
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Receita Total Exportada (FOB)",
        value = textOutput("kpi_receita"),
        theme = "primary"
      ),
      value_box(
        title = "Volume Físico Embarcado",
        value = textOutput("kpi_volume"),
        theme = value_box_theme(bg = "#00923F", fg = "#FFFFFF")
      )
    ),
    layout_columns(
      col_widths = c(6, 6, 12),
      card(
        card_header("Sazonalidade Mensal das Exportações (FOB)"),
        plotlyOutput("graf_sazonalidade", height = "320px")
      ),
      card(
        card_header("Principais Destinos das Exportações"),
        plotlyOutput("graf_destinos", height = "320px")
      ),
      card(
        card_header("Principais Setores Exportados (FOB)"),
        plotlyOutput("graf_setores", height = '320px')
      )
    )
  ),
  nav_panel(
    "Valor Agregado e Estatística",
    layout_columns(
      value_box(
        title = "Preço Médio Ponderado (US$/t)",
        value = textOutput("kpi_preco_medio"),
        theme = "primary"
      ),
      value_box(
        title = "Mediana do Preço por Tonelada (US$/t)",
        value = textOutput("kpi_preco_mediana"),
        theme = value_box_theme(bg = "#00923F", fg = "#FFFFFF")
      )
    ),
    layout_columns(
      card(
        card_header("Distribuição do Preço Médio (US$/t) por Via de Transporte (Escala Log)"),
        plotlyOutput("graf_boxplot_via")
      ),
      card(
        card_header("Volume (t) vs. Receita (FOB) por Setor (Escala Log-Log)"),
        plotlyOutput("graf_dispersao_setor")
      )
    ),
    card(
      card_header("Distribuição Geral do Preço Médio por Tonelada (Escala Log)"),
      plotlyOutput("graf_hist_preco")
    )
  )
)
server <- function(input, output, session){
  dados_filtrados <- reactive({
    req(input$ano, input$via)
    
    dados <- df_pr |> filter(ANO == input$ano)
    
    if (input$via != "Todas"){
      dados <- dados |> filter(VIA == input$via)
    }
    
    if (input$filtro_mes != "Todos") {
      dados <- dados |> filter(MES == input$filtro_mes)
    }
    
    dados
  })
  
  output$kpi_receita <- renderText({
    total <- sum(dados_filtrados()$VL_FOB, na.rm = TRUE)
    dollar(total, prefix = "US$ ", scale_cut = cut_short_scale())
  })
  
  output$kpi_volume <- renderText({
    total_t <- sum(dados_filtrados()$TONELADAS, na.rm = TRUE)
    paste(number(total_t, scale_cut = cut_short_scale()), " t")
  })
  
  output$graf_sazonalidade <- renderPlotly({
    req(input$ano, input$via)
    
    df_saz <- df_pr |>
      filter(ANO == input$ano)
    
    if (input$via != "Todas") {
      df_saz <- df_saz |> filter(VIA == input$via)
    }
    
    meses_abrev <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                     "Jul", "Ago", "Set", "Out", "Nov", "Dez")
    
    dados_saz <- df_saz |> 
      mutate(MES_NUM = as.numeric(MES)) |> 
      group_by(MES_NUM) |> 
      summarise(receita = sum(VL_FOB, na.rm = TRUE), .groups = "drop") |> 
      arrange(MES_NUM) |> 
      mutate(
        mes_nome = factor(meses_abrev[MES_NUM], levels = meses_abrev),
        rotulo = paste0(
          "<b>", mes_nome, "/", input$ano, "</b><br>",
          "Receita FOB: ", dollar(receita, prefix = "US$ ", scale_cut = cut_short_scale())
        )
      )
    
    p <- ggplot(dados_saz, aes(x = mes_nome, y = receita, group = 1)) +
      geom_line(color = "#003366", linewidth = 1.1) +
      geom_point(aes(text = rotulo), color = "#003366", size = 2.5) +
      scale_y_continuous(labels = dollar_format(prefix = "US$ ", scale_cut = cut_short_scale())) +
      labs(x = "", y = "Receita FOB (US$)") +
      theme_minimal()
    
    if (!is.null(input$filtro_mes) && input$filtro_mes != "Todos") {
      mes_sel <- as.numeric(input$filtro_mes)
      ponto_destaque <- dados_saz |> filter(MES_NUM == mes_sel)
      
      p <- p + geom_point(
        data = ponto_destaque,
        aes(x = mes_nome, y = receita, text = rotulo),
        color = "#00923F", size = 4.5
      )
    }
    
    ggplotly(p, tooltip = "text") |> 
      config(displayModeBar = FALSE)
  })
  
  output$graf_destinos <- renderPlotly({
    df <- dados_filtrados()
    total_fob <- sum(df$VL_FOB, na.rm = TRUE)
    
    dados_dest <- df |> 
      group_by(PAIS) |> 
      summarise(
        receita = sum(VL_FOB, na.rm = TRUE),
        toneladas = sum(TONELADAS, na.rm = TRUE),
        .groups = "drop"
      ) |> 
      arrange(desc(receita)) |> 
      slice_head(n = 10) |> 
      mutate(
        pct = receita / total_fob,
        rotulo = paste0(
          "<b>", PAIS, "</b><br>",
          "Receita FOB: ", dollar(receita, prefix = "US$ ", scale_cut = cut_short_scale()), "<br>",
          "Participação: ", percent(pct, accuracy = 0.1), "<br>",
          "Volume: ", number(toneladas, scale_cut = cut_short_scale()), " t"
        )
      )
    
    p <- ggplot(dados_dest, aes(x = reorder(PAIS, receita), y = receita, text = rotulo)) +
      geom_col(fill = "#00923F", alpha = 0.9) +
      geom_text(
        aes(label = percent(pct, accuracy = 0.1)),
        nudge_y = max(dados_dest$receita, na.rm = TRUE) * 0.08,
        hjust = 0,
        size = 3.2,
        color = "#003366"
      ) +
      coord_flip() +
      scale_y_continuous(
        labels = dollar_format(prefix = "US$ ", scale_cut = cut_short_scale()),
        expand = expansion(mult = c(0, 0.18))
      ) +
      labs(x = "", y = "Receita FOB (US$)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") |> 
      config(displayModeBar = FALSE)
  })
  
  output$graf_setores <- renderPlotly({
    dados_setores <- dados_filtrados() |> 
      group_by(SETOR) |> 
      summarise(receita = sum(VL_FOB, na.rm = TRUE), .groups = "drop") |> 
      slice_max(receita, n = 10) |> 
      mutate(
        setor_curto = ifelse(nchar(SETOR) > 32, paste0(substr(SETOR, 1, 29), "..."), SETOR),
        rotulo_valor = dollar(receita, prefix = "US$ ", scale_cut = cut_short_scale())
      )
    
    plot_ly(
      data = dados_setores,
      type = "treemap",
      labels = ~setor_curto,
      parents = "",
      values = ~receita,
      text = ~rotulo_valor,
      textinfo = "label+text+percent root",
      textposition = "middle center",
      textfont = list(size = 12),
      hovertemplate = "<b>%{label}</b><br>Receita: %{text}<br>Participação: %{percentRoot:.1%}<extra></extra>",
      pathbar = list(visible = FALSE),
      tiling = list(pad = 4),
      marker = list(
        cornerradius = 6,
        colors = ~receita,
        colorscale = "Greens",
        reversescale = TRUE,
        line = list(width = 2, color = "white")
      )
    ) |> 
      layout(
        margin = list(t = 5, l = 5, r = 5, b = 5),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) |> 
      config(displayModeBar = FALSE)
  })
  output$kpi_preco_medio <- renderText({
    df <- dados_filtrados()
    total_fob <- sum(df$VL_FOB, na.rm = TRUE)
    total_ton <- sum(df$TONELADAS, na.rm = TRUE)
    preco_pond <- ifelse(total_ton > 0, total_fob / total_ton, 0)
    dollar(preco_pond, prefix = "US$ ", accuracy = 0.01)
  })
  
  output$kpi_preco_mediana <- renderText({
    df <- dados_filtrados() |> filter(PRECO_MEDIO_TON > 0)
    mediana <- median(df$PRECO_MEDIO_TON, na.rm = TRUE)
    dollar(mediana, prefix = "US$ ", accuracy = 0.01)
  })
  
  output$graf_boxplot_via <- renderPlotly({
    resumo_box <- dados_filtrados() |> 
      filter(PRECO_MEDIO_TON > 0, !is.na(VIA)) |> 
      group_by(VIA) |> 
      summarise(
        n = n(),
        receita = sum(VL_FOB, na.rm = TRUE),
        q1 = quantile(PRECO_MEDIO_TON, 0.25, na.rm = TRUE),
        mediana = median(PRECO_MEDIO_TON, na.rm = TRUE),
        q3 = quantile(PRECO_MEDIO_TON, 0.75, na.rm = TRUE),
        iqr = q3 - q1,
        lim_inf = max(min(PRECO_MEDIO_TON, na.rm = TRUE), q1 - 1.5 * iqr),
        lim_sup = min(max(PRECO_MEDIO_TON, na.rm = TRUE), q3 + 1.5 * iqr),
        media = mean(PRECO_MEDIO_TON, na.rm = TRUE),
        .groups = "drop"
      ) |> 
      arrange(mediana) |> 
      mutate(
        VIA = factor(VIA, levels = VIA),
        rotulo = paste0(
          "<b>", VIA, "</b><br>",
          "Mediana: ", dollar(mediana, prefix = "US$ ", accuracy = 0.01), "/t<br>",
          "Média: ", dollar(media, prefix = "US$ ", accuracy = 0.01), "/t<br>",
          "Q1 – Q3: ", dollar(q1, prefix = "US$ ", accuracy = 0.01), " a ", dollar(q3, prefix = "US$ ", accuracy = 0.01), "<br>",
          "Receita Total: ", dollar(receita, prefix = "US$ ", scale_cut = cut_short_scale()), "<br>",
          "Registros: ", number(n, big.mark = ".")
        )
      )
    
    plot_ly(resumo_box) |> 
      add_trace(
        type = "box",
        y = ~VIA,
        q1 = ~q1,
        median = ~mediana,
        q3 = ~q3,
        lowerfence = ~lim_inf,
        upperfence = ~lim_sup,
        fillcolor = "rgba(0, 146, 63, 0.85)",
        line = list(color = "#003366", width = 1.5),
        hoverinfo = "none",
        showlegend = FALSE
      ) |> 
      add_markers(
        x = ~mediana,
        y = ~VIA,
        text = ~rotulo,
        hoverinfo = "text",
        marker = list(color = "#003366", size = 7, symbol = "diamond"),
        name = "Mediana (detalhes)"
      ) |> 
      layout(
        xaxis = list(title = "Preço Médio (US$/t) — Escala Log", type = "log"),
        yaxis = list(title = ""),
        showlegend = FALSE,
        margin = list(t = 10, b = 40)
      ) |> 
      config(displayModeBar = FALSE)
  })
  
  output$graf_dispersao_setor <- renderPlotly({
    dados_disp <- dados_filtrados() |> 
      group_by(SETOR) |> 
      summarise(
        receita = sum(VL_FOB, na.rm = TRUE),
        toneladas = sum(TONELADAS, na.rm = TRUE),
        .groups = "drop"
      ) |> 
      filter(receita > 0, toneladas > 0) |> 
      mutate(
        preco_medio = receita / toneladas,
        setor_curto = ifelse(nchar(SETOR) > 35, paste0(substr(SETOR, 1, 32), "..."), SETOR),
        rotulo = paste0(
          "<b>", setor_curto, "</b><br>",
          "Receita: ", dollar(receita, prefix = "US$ ", scale_cut = cut_short_scale()), "<br>",
          "Volume: ", number(toneladas, scale_cut = cut_short_scale()), " t<br>",
          "Preço Médio: ", dollar(preco_medio, prefix = "US$ ", accuracy = 0.01), "/t"
        )
      )
    
    p <- ggplot(dados_disp, aes(x = toneladas, y = receita, text = rotulo)) +
      geom_point(color = "#00923F", alpha = 0.75, size = 3) +
      scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
      scale_y_log10(labels = dollar_format(prefix = "US$ ", scale_cut = cut_short_scale())) +
      labs(x = "Volume Embarcado (t) — Escala Log", y = "Receita FOB (US$) — Escala Log") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") |> 
      config(displayModeBar = FALSE)
  })
  
  output$graf_hist_preco <- renderPlotly({
    dados_hist <- dados_filtrados() |> 
      filter(PRECO_MEDIO_TON > 0)
    
    req(nrow(dados_hist) > 0)
    
    mediana_val <- median(dados_hist$PRECO_MEDIO_TON, na.rm = TRUE)
    
    # Pré-calcula as faixas no R em log10 (envia ~40 barras ao navegador em vez da base toda)
    h <- hist(log10(dados_hist$PRECO_MEDIO_TON), breaks = 40, plot = FALSE)
    total_n <- sum(h$counts)
    
    df_bins <- data.frame(
      xmin = 10^h$breaks[-length(h$breaks)],
      xmax = 10^h$breaks[-1],
      count = h$counts
    ) |> 
      filter(count > 0) |> 
      mutate(
        pct = count / total_n,
        rotulo = paste0(
          "<b>Faixa de Preço:</b> ",
          dollar(xmin, prefix = "US$ ", accuracy = 0.01), " a ",
          dollar(xmax, prefix = "US$ ", accuracy = 0.01), "/t<br>",
          "<b>Registros:</b> ", number(count, big.mark = "."),
          " (", percent(pct, accuracy = 0.1), ")"
        )
      )
    
    p <- ggplot(df_bins) +
      geom_rect(
        aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = count, text = rotulo),
        fill = "#003366", color = "white", alpha = 0.85
      ) +
      geom_vline(
        xintercept = mediana_val,
        color = "#00923F", linetype = "dashed", linewidth = 1
      ) +
      scale_x_log10(labels = dollar_format(prefix = "US$ ", scale_cut = cut_short_scale())) +
      scale_y_continuous(labels = label_number(big.mark = ".")) +
      labs(
        x = paste0(
          "Preço Médio (US$/t) — Escala Log (linha verde = Mediana: ",
          dollar(mediana_val, prefix = "US$ ", accuracy = 0.01), "/t)"
        ),
        y = "Frequência (Registros)"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") |> 
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)


