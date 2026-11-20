library(shiny)
library(bslib)
library(dplyr)

df_pr <- readRDS("exportacoes_pr.rds")

ui <- page_navbar(
  title = "Exportações do Paraná",
  sidebar = sidebar(
    selectInput("ano", "ANO:", choices = sort(unique(df_pr$ANO), decreasing = TRUE))),
  nav_panel(
   "Panorama Comercial",
   textOutput("teste_linhas")
),
  nav_panel(
    "Valor Agregado e Estatistica",
    "Espaco reservado para o Boxplot"
  )
)

server <- function(input, output, session){
  dados_filtrados <- reactive({
    df_pr |> filter(ANO == input$ano)
  })
  output$teste_linhas <- renderText({
    paste("Registros encontrados:", nrow(dados_filtrados()))
  })
}

shinyApp(ui, server)


