library(shiny)
library(tidyverse)

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
                 # text explaining the plot and options
                 p("You can analyze the global temperature for different regions. Select the regions you are interested in. You see a monthly scatterplot and the corresponding trend lines."),
                 
                 # a checkbox asking if the user wants to see the trend lines
                 checkboxInput("trend", "Show trend lines", TRUE),
                 
                 # a radio button group to select the color of the trend lines
                 radioButtons("color", "Palette",
                              choices = c("orangered", "green")),
                 
                 # a checkbox group to select the regions to display, default select `global`
                 checkboxGroupInput("region", "Select the region(s) to display:",
                                    choices = unique(data$region), selected = "globe")
               ),
               mainPanel(
                 # Display a plot of the generated distribution
                 plotOutput("plot"),
                 
                 # a text to display the number of observations in the subset and range of the dates
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
                 # a text to show the temperature range
                 textOutput("tablePanelPeriodDesc"),
                 
                 # Display a table of the data
                 dataTableOutput("tablePanelPeriod")
                 
               )
             )
    )
  )
  
)

# Define the server
server <- function(input, output) {
  ############################
  # General Info
  ############################
  
  # Generate a short random sample of the data as a table (random 10) to display on "General Info" page
  output$sampleMain <- renderDataTable({
    data[sample(nrow(data), 10), ]
  })
  
  
  
  ############################
  # Plot
  ############################
  
  # for the data, convert year and month to a date
  data$month <- as.Date(paste(data$year, data$month, "01", sep = "-"))
  
  # Generate a plot of the generated distribution
  output$plot <- renderPlot({
    # filter the data based on the user's selection
    dataFiltered <- data %>% filter(region %in% input$region)
    
    # display `dataFiltered` in the console
    print(dataFiltered)
    
    # plot the data
    # x axis is the year-month
    # add a random color to each filtered region
    p <- ggplot(dataFiltered, aes(x = month, y = temp, color = region)) +
      geom_point() +
      labs(x = "Year-Month", y = "Temperature (deg C)", color = "Region")
    
    
    # add trend lines if the user wants them
    if (input$trend) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = input$color)
    }
    
    # display the number of observations in the subset and range of the dates
    output$plotDesc <- renderText({
      paste("Time range:", min(dataFiltered$month), "to", max(dataFiltered$month),
            "Number of observations:", nrow(dataFiltered))
    })
    
    p
    
  })
  
  ############################
  # Table
  ############################
  
  # Generate a table of the data
  output$tablePanelPeriod <- renderDataTable({
    # filter the data based on the user's selection
    # if user selects `month`, 
    # then sort by `year` and `month`,
    # and display the average temperature for each month.
    # columns are `year`, `month`, `temp`
    
    # if user selects `year`,
    # then sort by `year`,
    # and display the average temperature for each year.
    # columns are `year`, `temp`
    
    # if user selects `decade`,
    # then sort by `decade`,
    # and display the average temperature for each decade.
    # columns are `decade`, `temp`
    
    # display `dataFiltered` in the console
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
    
    # a text to show the temperature range
    output$tablePanelPeriodDesc <- renderText({
      paste("Temperature range:", min(data$temp), "to", max(data$temp))
    })
    data
  })
  
  
  
  
  
}

tryCatch(
  
  # Run the application
  shinyApp(ui = ui, server = server,
           options=list(port=3838, launch.browser=FALSE)
           , onStart=cat("starting shiny server...\n")),
  
  # this is for the case when the user presses Ctrl+C to stop the app from the console
  interrupt = function(e) {
    cat("Exiting app by key-interrupt...")
    stopApp()		
  }	 
  
)
