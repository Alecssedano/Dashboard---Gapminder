library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(gapminder)
library(DT)
library(scales)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinycssloaders)

mundo_base <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name, geometry) %>%
  st_transform(4326)

mundo_base <- mundo_base %>%
  mutate(
    name = case_when(
      name == "United States of America" ~ "United States",
      name == "Democratic Republic of the Congo" ~ "Congo, Dem. Rep.",
      name == "Republic of the Congo" ~ "Congo, Rep.",
      name == "Yemen" ~ "Yemen, Rep.",
      TRUE ~ name
    )
  )

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = span(
      "Dashboard Económico Mundial",
      style = "font-weight:bold;"
    ),
    titleWidth = 350
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Resumen Global",
        tabName = "resumen",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Mapa Mundial",
        tabName = "mapa",
        icon = icon("globe")
      ),
      
      menuItem(
        "Evolución Temporal",
        tabName = "temporal",
        icon = icon("clock")
      ),
      
      menuItem(
        "Base de Datos",
        tabName = "datos",
        icon = icon("table")
      ),
      
      hr(),
      
      selectInput(
        "continente",
        "Continente:",
        choices = c(
          "Todos",
          unique(as.character(gapminder$continent))
        ),
        selected = "Todos"
      ),
      
      sliderInput(
        "anio",
        "Año:",
        min = min(gapminder$year),
        max = max(gapminder$year),
        value = 2007,
        step = 5,
        sep = ""
      )
      
    )
    
  ),
  
  dashboardBody(
    
    tabItems(

      tabItem(
        
        tabName = "resumen",
        
        fluidRow(
          
          valueBoxOutput("pbiBox", width = 3),
          valueBoxOutput("vidaBox", width = 3),
          valueBoxOutput("paisBox", width = 3),
          valueBoxOutput("poblacionBox", width = 3)
          
        ),
        
        fluidRow(
          
          box(
            title = "PIB vs Esperanza de Vida (Animado)",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            withSpinner(
              plotlyOutput("scatterPlot", height = "400px")
            )
          ),
          
          box(
            title = "Top 10 Países por PIB",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            withSpinner(
              plotlyOutput("barPlot", height = "400px")
            )
          )
          
        ),
        
        fluidRow(
          
          box(
            
            width = 12,
            title = "Interpretación Económica",
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            HTML("
            <h4>Relación entre crecimiento económico y bienestar</h4>
            
            <p>
            Este dashboard analiza cómo el crecimiento económico,
            representado mediante el PIB per cápita, se relaciona
            con indicadores de bienestar social como la esperanza
            de vida y la dinámica poblacional.
            </p>
            
            <p>
            Los resultados muestran que los países con mayores
            niveles de ingreso tienden a presentar mejores
            indicadores sociales, aunque existen diferencias
            estructurales entre continentes.
            </p>
            
            <p>
            Asimismo, el comportamiento histórico evidencia
            procesos de convergencia parcial entre regiones,
            donde algunos países emergentes han incrementado
            significativamente sus niveles de ingreso y calidad
            de vida durante las últimas décadas.
            </p>
            ")
            
          )
          
        )
        
      ),
      
      tabItem(
        
        tabName = "mapa",
        
        fluidRow(
          
          box(
            title = "Mapa Mundial Económico",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            withSpinner(
              leafletOutput("mapaMundial", height = 650)
            )
          )
          
        )
        
      ),
      
      tabItem(
        
        tabName = "temporal",
        
        fluidRow(
          
          box(
            title = "Evolución Temporal de la Esperanza de Vida",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            withSpinner(
              plotlyOutput("linePlot", height = "550px")
            )
          )
          
        )
        
      ),
      
      tabItem(
        
        tabName = "datos",
        
        fluidRow(
          
          box(
            title = "Base de Datos Mundial",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            withSpinner(
              dataTableOutput("tabla")
            )
          )
          
        )
        
      )
      
    )
    
  )
)

server <- function(input, output, session) {
  
  datos_filtrados <- reactive({
    
    datos <- gapminder %>%
      filter(year == input$anio)
    
    if(input$continente != "Todos"){
      
      datos <- datos %>%
        filter(continent == input$continente)
      
    }
    
    datos
    
  })

  output$pbiBox <- renderValueBox({
    
    pbi_med <- mean(datos_filtrados()$gdpPercap)
    
    valueBox(
      dollar(round(pbi_med, 2)),
      "PIB per cápita promedio",
      icon = icon("dollar-sign"),
      color = "green"
    )
    
  })
  
  output$vidaBox <- renderValueBox({
    
    vida_med <- mean(datos_filtrados()$lifeExp)
    
    valueBox(
      round(vida_med, 1),
      "Esperanza de vida promedio",
      icon = icon("heart"),
      color = "blue"
    )
    
  })
  
  output$paisBox <- renderValueBox({
    
    valueBox(
      length(unique(datos_filtrados()$country)),
      "Número de países",
      icon = icon("globe"),
      color = "yellow"
    )
    
  })
  
  output$poblacionBox <- renderValueBox({
    
    poblacion_total <- sum(datos_filtrados()$pop)
    
    valueBox(
      comma(poblacion_total),
      "Población total",
      icon = icon("users"),
      color = "red"
    )
    
  })

  output$scatterPlot <- renderPlotly({
    
    datos_animacion <- gapminder
    
    if(input$continente != "Todos"){
      
      datos_animacion <- datos_animacion %>%
        filter(continent == input$continente)
      
    }
    
    suppressWarnings({
      
      p <- datos_animacion %>%
        
        plot_ly(
          
          x = ~gdpPercap,
          y = ~lifeExp,
          
          size = ~sqrt(pop),
          
          color = ~continent,
          
          frame = ~year,
          
          ids = ~country,
          
          text = ~paste(
            "País:", country,
            "<br>PIB per cápita: $", comma(round(gdpPercap, 2)),
            "<br>Esperanza de vida:", round(lifeExp, 1), " años",
            "<br>Población:", comma(pop)
          ),
          
          type = "scatter",
          mode = "markers",
          
          sizes = c(8, 45),
          
          marker = list(
            opacity = 0.7,
            line = list(
              width = 1,
              color = "#FFFFFF"
            )
          )
          
        ) %>%
        
        layout(
          
          title = "Evolución Económica Mundial",
          
          xaxis = list(
            title = "PIB per cápita (Escala Logarítmica)",
            type = "log"
          ),
          
          yaxis = list(
            title = "Esperanza de vida"
          )
          
        ) %>%
        
        animation_opts(
          frame = 900,
          transition = 400,
          redraw = FALSE
        ) %>%
        
        animation_slider(
          currentvalue = list(
            prefix = "Año: "
          )
        )
      
      p
      
    })
    
  })

  output$barPlot <- renderPlotly({
    
    top_paises <- datos_filtrados() %>%
      arrange(desc(gdpPercap)) %>%
      slice(1:10)
    
    p <- ggplot(
      top_paises,
      aes(
        x = reorder(country, gdpPercap),
        y = gdpPercap,
        fill = continent
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      scale_y_continuous(labels = comma) +
      
      theme_minimal() +
      
      labs(
        x = "País",
        y = "PIB per cápita",
        fill = "Continente"
      )
    
    ggplotly(p)
    
  })
  
  output$mapaMundial <- renderLeaflet({
    
    df_actual <- datos_filtrados()
    
    req(nrow(df_actual) > 0)
    
    datos_mapa <- mundo_base %>%
      
      left_join(
        df_actual,
        by = c("name" = "country")
      ) %>%
      
      filter(!is.na(gdpPercap))
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = datos_mapa$gdpPercap,
      na.color = "transparent"
    )
    
    leaflet(datos_mapa) %>%
      
      addProviderTiles("CartoDB.DarkMatter") %>%
      
      setView(
        lng = 10,
        lat = 20,
        zoom = 2
      ) %>%
      
      addPolygons(
        
        fillColor = ~pal(gdpPercap),
        
        weight = 0.5,
        
        color = "white",
        
        fillOpacity = 0.85,
        
        smoothFactor = 0.2,
        
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#FFFFFF",
          fillOpacity = 0.95,
          bringToFront = TRUE
        ),
        
        popup = ~paste0(
          "<b>País:</b> ", name, "<br>",
          "<b>Continente:</b> ", continent, "<br>",
          "<b>PIB per cápita:</b> $", comma(round(gdpPercap, 2)), "<br>",
          "<b>Esperanza de vida:</b> ", round(lifeExp, 1), " años<br>",
          "<b>Población:</b> ", comma(pop)
        )
        
      ) %>%
      
      addLegend(
        pal = pal,
        values = ~gdpPercap,
        title = "PIB per cápita",
        position = "bottomright"
      )
    
  })

  output$linePlot <- renderPlotly({
    
    datos_temp <- gapminder
    
    if(input$continente != "Todos"){
      
      datos_temp <- datos_temp %>%
        filter(continent == input$continente)
      
    }
    
    p <- ggplot(
      datos_temp,
      aes(
        x = year,
        y = lifeExp
      )
    ) +
      
      geom_line(
        aes(
          group = country,
          color = continent
        ),
        alpha = 0.12,
        linewidth = 0.4
      ) +
      
      stat_summary(
        aes(
          group = continent,
          color = continent
        ),
        fun = mean,
        geom = "line",
        linewidth = 1.5
      ) +
      
      theme_minimal() +
      
      labs(
        x = "Año",
        y = "Esperanza de vida",
        color = "Continente"
      )
    
    suppressWarnings(
      ggplotly(p)
    )
    
  })

  output$tabla <- renderDataTable({
    
    datos_filtrados() %>%
      
      select(
        country,
        continent,
        year,
        lifeExp,
        pop,
        gdpPercap
      ) %>%
      
      mutate(
        gdpPercap = round(gdpPercap, 2),
        lifeExp = round(lifeExp, 1)
      )
    
  },
  
  extensions = c("Buttons"),
  
  options = list(
    
    pageLength = 10,
    
    scrollX = TRUE,
    
    dom = "Bfrtip",
    
    buttons = c(
      "copy",
      "csv",
      "excel",
      "print"
    )
    
  ))
  
}
shinyApp(ui, server)