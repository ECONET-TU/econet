library(shiny)
library(plotly)
library(DT)
library(leaflet)
library(ggplot2)

# Read the data
wagedata <- "https://raw.githubusercontent.com/ECONET-TU/econet/main/wagedata.csv" 
wagedata <- read.csv(wagedata)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Combined Visualizations with Shiny"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
      sliderInput("obs", "Number of observations:", 1, max(wagedata$obs, na.rm = TRUE), 500),
      selectInput("sex", "Choose a Sex: ", choices = c("All", unique(wagedata$sex))),
      selectInput("province", "Choose a Province: ", choices = c("All", unique(wagedata$province)))
    ),
    
    # Show a plot of the generated data
    mainPanel(
      tabsetPanel(
        tabPanel("Interactive Plot", plotlyOutput("plot")),
        tabPanel("Data Table", DTOutput("table")),
        tabPanel("Interactive Map", leafletOutput("map")),
        tabPanel("Histogram 1", plotOutput("distPlot")),
        tabPanel("Histogram 2", plotOutput("distPlot2"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter the data based on inputs
  filtered_data <- reactive({
    data_filtered <- wagedata
    
    if (input$sex != "All") {
      data_filtered <- data_filtered[data_filtered$sex == input$sex, ]
    }
    
    if (input$province != "All") {
      data_filtered <- data_filtered[data_filtered$province == input$province, ]
    }
    
    data_filtered
  })
  
  # Interactive Plot
  output$plot <- renderPlotly({
    plot_ly(x = rnorm(input$obs), type = "histogram")
  })
  
  # Interactive data table
  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  # Interactive Map
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addMarkers(lat = 27.7172, lng = 85.3240, popup = "Kathmandu, Nepal") # Adjust this to use your dataset's lat/lng columns
  })
  
  
  # Histogram: Wage
  output$distPlot2 <- renderPlot({
    x <- filtered_data()$wage
    bins <- seq(min(as.numeric(x)), max(as.numeric(x)), length.out = input$bins + 1)
    
    hist((as.numeric(x)), breaks = bins, col = 'darkblue', border = 'white',
         xlab = 'Wage',
         main = 'Distribution of the Sample by Wage')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
