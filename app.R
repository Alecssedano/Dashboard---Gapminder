library(shiny)
library(ggplot2)
library(dplyr)
library(gapminder)
library(DT)

ui <- fluidPage(
  
  titlePanel("Dashboard Gapminder"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "continente",
        "Selecciona un continente:",
        choices = unique(gapminder$continent)
      )
      
    ),
    
    mainPanel(
      
      plotOutput("grafico"),
      dataTableOutput("tabla")
      
    )
    
  )
)

server <- function(input, output) {
  
  datos <- reactive({
    
    gapminder %>%
      filter(continent == input$continente)
    
  })
  
  output$grafico <- renderPlot({
    
    ggplot(datos(),
           aes(x = gdpPercap,
               y = lifeExp,
               color = country)) +
      
      geom_point(size = 3) +
      
      theme_minimal()
    
  })
  
  output$tabla <- renderDataTable({
    
    datos()
    
  })
  
}

shinyApp(ui = ui, server = server)
