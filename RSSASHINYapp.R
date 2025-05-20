#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(shiny)
library(caret)
library(dplyr)
library(ggplot2)
library(lubridate)
library(GGally)

# Read the dataset
data_path <- "C:/Users/divya/Downloads/IDSFinalProject/energy_static_weather_data.csv"
data <- read.csv(data_path)
data$date <- as.Date(data$date) # Convert date column to Date type

# Define UI
ui <- fluidPage(
  titlePanel("Energy Usage Analysis and Visualization"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange",
                     label = "Select Date Range",
                     start = min(data$date), 
                     end = max(data$date),
                     min = min(data$date),
                     max = max(data$date)),
      selectInput("county", "Select County", choices = unique(data$in.county)),
      selectInput("visualization", "Select Visualization", 
                  choices = c("Time Series Plot", "Scatterplot Matrix")),
      numericInput("sqft", "Square Footage", value = 1000),
      numericInput("bedrooms", "Number of Bedrooms", value = 3),
      actionButton("predict", "Predict Energy Usage"),
      tags$style(HTML(".shiny-output-error { visibility: hidden; } .shiny-output-error:before { visibility: hidden; }")) # Hide error messages
    ),
    mainPanel(
      plotOutput("visualizationPlot"),
      tags$div(style = "font-weight: bold; color: #007bff; margin-top: 20px;", textOutput("predictionOutput"))
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Filter data based on date range and county
  filteredData <- reactive({
    data %>%
      filter(date >= input$dateRange[1],
             date <= input$dateRange[2],
             in.county == input$county)
  })
  
  # Predict energy usage
  predictEnergyUsage <- eventReactive(input$predict, {
    county_data <- filteredData()
    selected_data <- county_data[, c("in.sqft", "in.bedrooms", "total_energy_usage")]
    set.seed(10)
    split_data <- createDataPartition(selected_data$total_energy_usage, p = 0.8, list = FALSE)
    train_data <- selected_data[split_data, ]
    X_train <- subset(train_data, select = -total_energy_usage)
    y_train <- train_data$total_energy_usage
    model <- lm(y_train ~ ., data = X_train)
    
    newdata <- data.frame(
      in.sqft = input$sqft,
      in.bedrooms = input$bedrooms
    )
    return(predict(model, newdata = newdata))
  })
  
  output$predictionOutput <- renderText({
    req(predictEnergyUsage())
    paste("Predicted Energy Usage:", round(predictEnergyUsage(), 2), "kWh")
  })
  
  # Visualization Plot
  output$visualizationPlot <- renderPlot({
    req(input$visualization)
    county_data <- filteredData()
    
    if (input$visualization == "Time Series Plot") {
      ggplot(county_data, aes(x = date, y = total_energy_usage, group = 1)) +
        geom_line(color = "blue", size = 1) +
        labs(title = "Time Series of Energy Usage",
             x = "Date", y = "Total Energy Usage (kWh)") +
        theme_minimal()
    } else if (input$visualization == "Scatterplot Matrix") {
      selected_cols <- county_data[, c("total_energy_usage", "Dry.Bulb.Temperature...C.",
                                       "Relative.Humidity....", "Wind.Speed..m.s.", "Global.Horizontal.Radiation..W.m2.")]
      ggpairs(selected_cols, 
              title = "Scatterplot Matrix of Energy Variables",
              progress = FALSE)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
