# =========================================================
# GLOBAL STATISTICS DASHBOARD
# =========================================================

# =========================================================
# LIBRERÍAS
# =========================================================

library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(gapminder)
library(htmltools)
library(bslib)
library(stringi)
library(stringr)
library(ggplot2)

# =========================================================
# UI
# =========================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$script(
      src = "https://cdn.tailwindcss.com"
    ),
    
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"
    ),
    
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
        font-size:38px;
        font-weight:700;
        color:#111827;
      }

    "))
  ),
  
  # HEADER
  
  div(
    class = "card",
    
    h1(
      class = "title-main",
      "Global Statistics - Trends & Regression"
    ),
    
    p(
      class = "subtitle",
      "Interactive Analytics Dashboard"
    )
  ),
  
  # CONTROLES
  
  div(
    class = "card",
    
    fluidRow(
      
      column(
        4,
        
        selectInput(
          "continent",
          "Select Continent",
          
          choices = c(
            "All",
            unique(gapminder$continent)
          ),
          
          selected = "All"
        )
      ),
      
      column(
        4,
        
        sliderInput(
          "year",
          "Select Year",
          
          min = min(gapminder$year),
          max = max(gapminder$year),
          
          value = 2007,
          step = 5,
          sep = ""
        )
      ),
      
      column(
        4,
        
        selectInput(
          "variable",
          "Select Variable",
          
          choices = c(
            "Life Expectancy",
            "GDP per Capita",
            "Population"
          )
        )
      )
    )
  ),
  
  # TENDENCIAS
  
  div(
    class = "card",
    
    h3(
      class = "section-title",
      "Historical Regional Trends"
    ),
    
    plotlyOutput(
      "trendPlot",
      height = "450px"
    )
  ),
  
  # REGRESIÓN
  
  div(
    class = "card",
    
    h3(
      class = "section-title",
      "Regression Analysis"
    ),
    
    plotlyOutput(
      "regressionPlot",
      height = "500px"
    )
  ),
  
  # MÉTRICAS
  
  fluidRow(
    
    column(
      4,
      
      div(
        class = "card text-center",
        
        h4("Population"),
        
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
        
        h4("Average GDP"),
        
        div(
          class = "metric",
          textOutput("gdpMetric")
        )
      )
    )
  ),
  
  # TABLA
  
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

server <- function(input, output, session){
  
  filtered_data <- reactive({
    
    data <- gapminder %>%
      filter(year == input$year)
    
    if(input$continent != "All"){
      
      data <- data %>%
        filter(continent == input$continent)
    }
    
    data
  })
  
  trend_data <- reactive({
    
    data <- gapminder
    
    if(input$continent != "All"){
      
      data <- data %>%
        filter(continent == input$continent)
    }
    
    data
  })
  
  # TRENDS
  
  output$trendPlot <- renderPlotly({
    
    yvar <- switch(
      input$variable,
      "Life Expectancy" = "lifeExp",
      "GDP per Capita" = "gdpPercap",
      "Population" = "pop"
    )
    
    trend <- trend_data() %>%
      group_by(year, continent) %>%
      summarise(
        value = mean(.data[[yvar]], na.rm = TRUE),
        .groups = "drop"
      )
    
    plot_ly(
      trend,
      x = ~year,
      y = ~value,
      color = ~continent,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = paste(input$variable, "Trend"),
        xaxis = list(title = "Year"),
        yaxis = list(title = input$variable)
      )
  })
  
  # REGRESSION
  
  output$regressionPlot <- renderPlotly({
    
    model <- lm(
      lifeExp ~ gdpPercap,
      data = filtered_data()
    )
    
    r2 <- round(summary(model)$r.squared, 3)
    
    plot_ly(
      
      data = filtered_data(),
      
      x = ~gdpPercap,
      y = ~lifeExp,
      
      type = "scatter",
      mode = "markers",
      
      color = ~continent,
      
      text = ~paste(
        "Country:", country,
        "<br>GDP:", round(gdpPercap,2),
        "<br>Life Exp:", round(lifeExp,1)
      ),
      
      hoverinfo = "text"
      
    ) %>%
      
      add_lines(
        x = ~gdpPercap,
        y = ~predict(model),
        name = "Regression Line"
      ) %>%
      
      layout(
        
        title = paste(
          "Regression Analysis (R² =",
          r2,
          ")"
        ),
        
        xaxis = list(
          title = "GDP per Capita"
        ),
        
        yaxis = list(
          title = "Life Expectancy"
        )
      )
  })
  
  # METRICS
  
  output$populationMetric <- renderText({
    
    total_pop <- sum(filtered_data()$pop)
    
    paste0(
      round(total_pop / 1000000,1),
      " M"
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
  
  # TABLE
  
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
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui = ui, server = server)

