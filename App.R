# =========================================================
# INSTALAR PAQUETES
# (EJECUTAR SOLO UNA VEZ)
# =========================================================

install.packages("shiny")
install.packages("plotly")
install.packages("DT")
install.packages("dplyr")
install.packages("gapminder")
install.packages("rsconnect")
install.packages("htmltools")
install.packages("bslib")

# =========================================================
# LIBRERÍAS
# =========================================================

library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(gapminder)
library(rsconnect)
library(htmltools)
library(bslib)

# =========================================================
# INTERFAZ UI
# =========================================================

ui <- fluidPage(
  
  # =========================================================
  # CONFIGURACIÓN HTML
  # =========================================================
  
  tags$head(
    
    # Tailwind CSS
    tags$script(
      src = "https://cdn.tailwindcss.com"
    ),
    
    # Fuente Google
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"
    ),
    
    # CSS PERSONALIZADO
    tags$style(HTML("

      body{
        background:#f8f9ff;
        font-family:'Inter', sans-serif;
        padding:20px;
      }

      .card{
        background:white;
        border-radius:20px;
        padding:24px;
        margin-bottom:20px;
        box-shadow:0 4px 12px rgba(0,0,0,0.08);
      }

      .title-main{
        font-size:42px;
        font-weight:700;
        color:#111827;
      }

      .subtitle{
        color:#6b7280;
        font-size:18px;
      }

      .section-title{
        font-size:24px;
        font-weight:600;
        margin-bottom:15px;
      }

      .metric{
        font-size:40px;
        font-weight:700;
        color:#111827;
      }

    "))
  ),
  
  # =========================================================
  # HEADER
  # =========================================================
  
  div(
    class = "card",
    
    h1(
      class = "title-main",
      "Global Statistics Dashboard"
    ),
    
    p(
      class = "subtitle",
      "Gapminder Interactive Dashboard"
    )
  ),
  
  # =========================================================
  # FILTROS
  # =========================================================
  
  div(
    class = "card",
    
    fluidRow(
      
      column(
        6,
        
        selectInput(
          inputId = "continent",
          label = "Select Continent",
          
          choices = c(
            "All",
            unique(gapminder$continent)
          ),
          
          selected = "All"
        )
      ),
      
      column(
        6,
        
        sliderInput(
          inputId = "year",
          label = "Select Year",
          
          min = min(gapminder$year),
          max = max(gapminder$year),
          
          value = 2007,
          step = 5,
          sep = ""
        )
      )
    )
  ),
  
  # =========================================================
  # GRÁFICO
  # =========================================================
  
  div(
    class = "card",
    
    h3(
      class = "section-title",
      "GDP vs Life Expectancy"
    ),
    
    plotlyOutput(
      outputId = "bubblePlot",
      height = "500px"
    )
  ),
  
  # =========================================================
  # MÉTRICAS
  # =========================================================
  
  fluidRow(
    
    column(
      4,
      
      div(
        class = "card text-center",
        
        h4("World Population"),
        
        div(
          class = "metric",
          textOutput("populationMetric")
        )
      )
    ),
    
    column(
      4,
      
      div(
        class = "card text-center",
        
        h4("Average Life Expectancy"),
        
        div(
          class = "metric",
          textOutput("lifeMetric")
        )
      )
    ),
    
    column(
      4,
      
      div(
        class = "card text-center",
        
        h4("Average GDP per Capita"),
        
        div(
          class = "metric",
          textOutput("gdpMetric")
        )
      )
    )
  ),
  
  # =========================================================
  # TABLA
  # =========================================================
  
  div(
    class = "card",
    
    h3(
      class = "section-title",
      "Country Statistics"
    ),
    
    DTOutput("countryTable")
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {
  
  # =========================================================
  # DATOS FILTRADOS
  # =========================================================
  
  filtered_data <- reactive({
    
    data <- gapminder %>%
      filter(year == input$year)
    
    if(input$continent != "All"){
      
      data <- data %>%
        filter(continent == input$continent)
    }
    
    return(data)
  })
  
  # =========================================================
  # GRÁFICO BURBUJAS
  # =========================================================
  
  output$bubblePlot <- renderPlotly({
    
    plot_ly(
      
      data = filtered_data(),
      
      x = ~gdpPercap,
      y = ~lifeExp,
      
      size = ~pop,
      
      color = ~continent,
      
      text = ~paste(
        "Country:", country,
        "<br>Population:", format(pop, big.mark = ","),
        "<br>GDP per Capita:", round(gdpPercap,2),
        "<br>Life Expectancy:", round(lifeExp,1)
      ),
      
      hoverinfo = "text",
      
      type = "scatter",
      mode = "markers"
      
    ) %>%
      
      layout(
        
        title = paste(
          "Gapminder Data - Year",
          input$year
        ),
        
        xaxis = list(
          title = "GDP per Capita"
        ),
        
        yaxis = list(
          title = "Life Expectancy"
        )
      )
  })
  
  # =========================================================
  # MÉTRICAS
  # =========================================================
  
  output$populationMetric <- renderText({
    
    total_pop <- sum(filtered_data()$pop)
    
    paste0(
      round(total_pop / 1000000000,2),
      " B"
    )
  })
  
  output$lifeMetric <- renderText({
    
    avg_life <- mean(filtered_data()$lifeExp)
    
    paste0(
      round(avg_life,1),
      " Years"
    )
  })
  
  output$gdpMetric <- renderText({
    
    avg_gdp <- mean(filtered_data()$gdpPercap)
    
    paste0(
      "$",
      round(avg_gdp,0)
    )
  })
  
  # =========================================================
  # TABLA
  # =========================================================
  
  output$countryTable <- renderDT({
    
    datatable(
      
      filtered_data() %>%
        select(
          country,
          continent,
          lifeExp,
          pop,
          gdpPercap
        ),
      
      options = list(
        pageLength = 10
      )
      
    )
  })
  
}

# =========================================================
# EJECUTAR APP
# =========================================================

shinyApp(ui = ui, server = server)

# =========================================================
# SUBIR A SHINYAPPS.IO
# =========================================================

# 1. Crear cuenta:
# https://www.shinyapps.io/

# 2. Configurar cuenta:

rsconnect::setAccountInfo(
  name = "TU_USUARIO",
  token = "TU_TOKEN",
  secret = "TU_SECRET"
)

# 3. Subir aplicación:

rsconnect::deployApp()

# =========================================================
# FIN
# =========================================================