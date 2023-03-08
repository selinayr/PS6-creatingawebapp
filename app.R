library(shiny)
library(tidyverse)
library(rsconnect)

data <- read_delim("UAH-lower-troposphere-long.csv.bz2")

# Define the UI
ui <- fluidPage(
  # Application title
  titlePanel("My Shiny App"),
  
  tabsetPanel(
    # General Info
    tabPanel("General Info",
             p("This app uses satellite temperature data from",
               strong("UAH")),
             p("Temperature",
               em("temp"),
               "is measured as deviation (deg C) from 1991-2020 baseline"),
             p("The dataset contains", nrow(data),
               "observations and", ncol(data), "variables"),
             p("Here is a small (random) sample of data:"),
             mainPanel(
               # Display a small table of the data
               dataTableOutput("sampleMain")
             )
    ),
    
    # Plot
    tabPanel("plot",
             sidebarLayout(
               sidebarPanel(
                 p("You can analyze the global temperature for different regions. Select the regions you are interested in. You see a monthly scatterplot and the corresponding trend lines."),
                 checkboxInput("trend", "Show trend lines", TRUE),
                 radioButtons("color", "Palette",
                              choices = c("orangered", "green")),
                 checkboxGroupInput("region", "Select the region(s) to display:",
                                    choices = unique(data$region), selected = "globe")
               ),
               mainPanel(
                 plotOutput("plot"),
                 textOutput("plotDesc")
               )
             )
    ),
    
    # Table
    tabPanel("Tables",
             sidebarLayout(
               sidebarPanel(
                 p("This panel displays average temperature over different time periods", br(), em("months, "), em("years"), " and ", em("decades"), "."),   
                 
                 # a radio button group to select the time period to display
                 radioButtons("timePeriod", "Select the time period to display:",
                              choices = c("month", "year", "decade"), selected = "month")
               ),
               mainPanel(
                 textOutput("tablePanelPeriodDesc"),
                 dataTableOutput("tablePanelPeriod")
                 
               )
             )
    )
  )
  
)

# Define the server
server <- function(input, output) {
  output$sampleMain <- renderDataTable({
    data[sample(nrow(data), 10), ]
  })
  data$month <- as.Date(paste(data$year, data$month, "01", sep = "-"))
  output$plot <- renderPlot({
    dataFiltered <- data %>% filter(region %in% input$region)
    print(dataFiltered)
    
    # plot the data
    # x axis is the year-month
    # add a random color to each filtered region
    p <- ggplot(dataFiltered, aes(x = month, y = temp, color = region)) +
      geom_point() +
      labs(x = "Year-Month", y = "Temperature (deg C)", color = "Region")
    if (input$trend) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = input$color)
    }
    output$plotDesc <- renderText({
      paste("Time range:", min(dataFiltered$month), "to", max(dataFiltered$month),
            "Number of observations:", nrow(dataFiltered))
    })
    
    p
    
  })
  output$tablePanelPeriod <- renderDataTable({
    print(data)
    
    isMonth <- input$timePeriod == "month"
    isYear <- input$timePeriod == "year"
    isDecade <- input$timePeriod == "decade"
    
    if(isMonth){
      data <- data %>% 
        group_by(year, month) %>% 
        summarise(temp = mean(temp)) %>% 
        arrange(year, month)
    } else if(isYear){
      data <- data %>% 
        group_by(year) %>% 
        summarise(temp = mean(temp)) %>% 
        arrange(year)
    } else if(isDecade){
      data <- data %>% 
        mutate(decade = year - year %% 10) %>% 
        group_by(decade) %>% 
        summarise(temp = mean(temp)) %>% 
        arrange(decade)
    }
    output$tablePanelPeriodDesc <- renderText({
      paste("Temperature range:", min(data$temp), "to", max(data$temp))
    })
    data
  })
  
  
  
  
  
}

tryCatch(
  shinyApp(ui = ui, server = server,
           options=list(port=3838, launch.browser=FALSE)
           , onStart=cat("starting shiny server...\n")),
  interrupt = function(e) {
    cat("Exiting app by key-interrupt...")
    stopApp()		
  }	 
  
)
