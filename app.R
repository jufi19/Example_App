
#options(shiny.maxRequestSize = 30*1024*1024)
#####################################################
#               R - Shiny Applications              #
#####################################################

#install.packages("tidyverse")
#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("shinyWidgets")
#options("install.lock"=FALSE)

library(tidyverse)
library(shiny)
library(shinythemes)
library(dplyr)
#library(shinyWidgets)
europe <- readRDS("europe.rds")


ui <- fluidPage(
  
  theme = shinytheme("darkly"),
  titlePanel("Exploring European Temperatures"),
  
  "This is a shiny app, based on average yearly temperatures for European countries and
  cities within the years",strong( " 2000 to 2019"),".","It plots the temperature trend for the chosen year range for a specific city
  To get a plot choose your range of years, country and city and the temperature measure.",
  
  sidebarLayout(
    
    sidebarPanel(
      strong("Choose a year range you want to explore:"),
      sliderInput(inputId = "Year", label = "Year", #gives a slider
                  min = 2000,
                  max = 2019,
                  step = 1,
                  value = c(2000,2019),
                  sep = ""),
      br(),br(),
      strong("Choose a country and city you want to display:"),
      
      selectInput(inputId = "drop_down_1", label = "Country", #gives a dropdown for the country to choose from
                  choices = unique(europe$Country)),
      
      uiOutput(outputId = "city_dropdown"), # dynamic dropdown for the corresponding cities
      
      # Input: A simple radio button input ----
      radioButtons(inputId = "cel_far", label = "Choose a temperature version:",
                   choices = list("Celsius" = "Celsius", "Fahrenheit" = "Fahrenheit"),
                   selected = "Celsius"),
      
      actionButton(inputId = "button", label = "Go!"),
      #fileInput(inputId = "file", label="File")
      #downloadButton(outputId = "download", label = "Download")
      
      
    ),
    
    
    mainPanel(
      #"This is the main panel",
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Info",
                           br(),
                           #h3("Your chosen input"),
                           textOutput(outputId = "text_output")),
                  tabPanel(title = "Data"),
                  tabPanel(title = "Plots", plotOutput(outputId = "city_plot"))
                  
                  
      )
    )
  )
)

server <- function(input, output, session) {
  # output$download <- downloadHandler(
  #   filename = "Temperature_Europe.csv",
  #   content = function(file){
  #     write.csv(data, "Temperature.csv")
  #   }
  # )
  
  #europe<-eventReactive(input$file,{
  #readRDS(input$file$datapath)
  #})
  
  # Reactive function to filter data based on inputs and calculate mean temperature
  data <- eventReactive(input$button, {
    
    # Filtering data by country and city
    country <- input$drop_down_1
    city <- input$drop_down_2
    city_temp_data <- europe %>% 
      filter(Country == country & City == city & Year >= input$Year[1] & Year <= input$Year[2])
    
    # Convert temperature to Celsius or Fahrenheit based on user selection
    if (input$cel_far == "Celsius") {
      # Celsius selected
      city_temp_data$AvgTemperature <- (city_temp_data$AvgTemperatureF - 32) * 5 / 9
    } else {
      # Fahrenheit selected
      city_temp_data$AvgTemperature <- city_temp_data$AvgTemperatureF
    }
    
    # Calculate mean temperature for each year
    year_mean_temp_data <- city_temp_data %>% 
      group_by(Year) %>% 
      summarize(mean_temp = mean(AvgTemperature))
    
    return(year_mean_temp_data)
  })
  
  # Generate plot based on selected country and city
  output$city_plot <- renderPlot({
    ggplot(data(), aes(x = Year, y = mean_temp)) +
      geom_point(color = "lightgrey") +
      geom_smooth(color = "blue", se = FALSE) +
      labs(x = "Year", y = "Mean Temperature (Fahrenheit)", title = paste("Mean Temperature Trend for", input$drop_down_2, ",", input$drop_down_1)) +
      theme_bw()
  })
  
  # Generate city dropdown based on selected country
  output$city_dropdown <- renderUI({
    country <- input$drop_down_1
    cities <- unique(europe %>% filter(Country == country) %>% pull(City))
    selectInput(inputId = "drop_down_2", label = "City", choices = cities)
  })
  
  # Reactive function to display text output
  text <- reactive({
    paste("Your inputs are:"," ",   "Year range:", input$Year[1], "to", input$Year[2], "Country:", input$drop_down_1, ",", "City:", input$drop_down_2, ",","Temperature measure:", input$cel_far)
  })
  
  # Displays text output
  output$text_output <- renderText({
    text()
  })
  
  # Observe changes in inputs and update plot only when button is pressed
  observeEvent(input$button, {
    data()
  })
  
  observe({
    print(input$drop_down_1)
    print(input$drop_down_2)
    print(input$Year[1])
    print(input$Year[2])
    print(input$cel_far)
  })
  
}


shinyApp(ui = ui, server = server)

