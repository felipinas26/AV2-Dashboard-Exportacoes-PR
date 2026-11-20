library(shiny)
library(bslib)
library(dplyr)
library(scales)

df_pr <- readRDS("exportacoes_pr.rds")

ui <- page_navbar(
  title = "Exportações do Paraná",
  
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
        theme = "info"
      )
    )
  ),
  
  nav_panel(
    "Valor Agregado e Estatistica",
    "Espaco reservado para o Boxplot"
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
}

shinyApp(ui, server)


