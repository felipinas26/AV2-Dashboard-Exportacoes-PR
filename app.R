library(shiny)
library(bslib)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)

df_pr <- readRDS("exportacoes_pr.rds")

ui <- page_navbar(
  title = tags$span(
    tags$img(
      src = "bandeira_do__pr.jpg",
      height = "28px",
      style = "margin-right: 8px; border_radius: 2px; vertical-align: middle;"
    ),
    "Exportações do Paraná"
  ),
  window_title = "Exportações do Paraná",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#003366",
    success = "#007A33"
  ),
  fillable = FALSE,
  
  sidebar = sidebar(
    selectInput(
      "ano",
      "ANO:",
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
        title = "Volume Fisico Embarcado",
        value = textOutput("kpi_volume"),
        theme = "teal"
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
      ),
    )
  ),
  
  nav_panel(
    "Valor Agregado e Estatistica",
    "Espaco reservado para a Aba 2"
  )
)

server <- function(input, output, session){
  dados_filtrados <- reactive({
    req(input$ano, input$via)
    
    dados <- df_pr |> filter(ANO == input$ano)
    
    if (input$via != "Todas"){
      dados <- dados |> filter(VIA == input$via)
    }
    
    dados
  })
  
  output$kpi_receita <- renderText({
    total <- sum(dados_filtrados()$VL_FOB, na.rm = TRUE)
    dollar(total, prefix = "US$ ", scale_cut = cut_short_scale())
  })
  
  output$kpi_volume <- renderText({
    total_t <- sum(dados_filtrados()$TONELADAS, na.rm = TRUE)
    paste(number(total_t, scale_cut = cut_short_scale()), "t")
  })
  
  output$graf_sazonalidade <- renderPlotly({
    dados_mes <- dados_filtrados() |> 
      group_by(MES) |> 
      summarise(receita = sum(VL_FOB, na.rm = TRUE), .groups = "drop") |>
      mutate(
        rotulo = paste0(
          "<b>Mês:</b> ", MES, "<br>",
          "<b>Receita:</b> ", dollar(receita, prefix = "US$ ", scale_cut = cut_short_scale())
        )
      )
    
    p <- ggplot(dados_mes, aes(x = MES, y = receita, group = 1, text = rotulo)) +
      geom_line(color = "#2c3e50", linewidth = 1) +
      geom_point(color = "#2c3e50", size = 2.5) +
      scale_y_continuous(labels = dollar_format(prefix = "US$", scale_cut = cut_short_scale())) +
      labs(x = "Mes", y = "Receita (FOB)")
    theme_minimal()
    
    ggplotly(p, tooltip = "text") |> 
      config(displayModeBar = FALSE)
  })
  
  output$graf_destinos <- renderPlotly({
    dados_paises <- dados_filtrados() |> 
      group_by(PAIS) |> 
      summarise(receita = sum(VL_FOB, na.rm = TRUE), .groups = "drop") |> 
      slice_max(receita, n = 10) |> 
      mutate(
        rotulo = paste0(
          "<b>País:</b> ", PAIS, "<br>",
          "<b>Receita:</b> ", dollar(receita, prefix = "US$ ", scale_cut = cut_short_scale())
        )
      )
    
    p <- ggplot(dados_paises, aes(x = reorder(PAIS, receita), y = receita)) +
      geom_col(fill = "#18BC9C", width = 0.7) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format(prefix = "US$", scale_cut = cut_short_scale())) +
      labs(x = NULL, y = "Receita (FOB)") +
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
        colorscale = "Teal",
        line = list(width = 2, color = "white")
      )
    ) |> 
      layout(
        margin = list(t = 5, l = 5, r = 5, b = 5),
        paper_bgcolor = "rgba(0,0,0,0)"
      ) |> 
      config(displayModeBar = FALSE)
  })
  
}

shinyApp(ui, server)


