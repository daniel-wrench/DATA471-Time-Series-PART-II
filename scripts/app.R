######################################################################
### Shiny app to provide interactive EDA of electricity generation ###
######################################################################

# Load libraries
library(shiny)
library(shinydashboard)
library(RCurl)
library(tidyverse)
library(magrittr)
library(plotly)
library(imputeTS)
library(lubridate)
library(timetk)
library(viridis)

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title="Spanish Electricity Generation"),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
            selectInput("time_period_select", "Use Average of Each",
                        c("Hour" = "hour",
                          "Day" = "day",
                          "Week" = "week",
                          "Month" = "month",
                          "Quarter" = "quarter",
                          "Half Year" = "halfyear",
                          "Year" = "year"
                        ), selected = c("Month"="month")),
            dateRangeInput("date_select", "Select Date Range",
                           start = "2015-01-01",
                           end = "2018-12-31",
                           min = "2015-01-01",
                           max = "2018-12-31"),
            checkboxGroupInput("gen_method_select", "Select Generation Methods",
                        c("Hard Coal",
                          "Lignite",
                          "Biomass",
                          "Gas",
                          "Oil",
                          "Waste",
                          "Nuclear",
                          "Other",
                          "Solar",
                          "Hydro Pumped",
                          "Hydro River",
                          "Hydro Reservoir",
                          "Wind Onshore",
                          "Other Renewable"
                         )),
            selectInput("vis_select", "Select Visualisation Method",
                        c("Overlaid",
                          "Side-by-side"
                        )),
            selectInput("center", "Y Values",
                        c("Raw",
                          "Mean Centered"
                        )),
          conditionalPanel(
            "input.vis_select == 'Side-by-side'",
            selectInput("y_scale_select", "Y Scale",
                        c("Shared",
                          "Independent"
                        )))
        ),
        dashboardBody(
          fluidPage(
            box(plotlyOutput('linePlot'), width = NULL, height = 800)
          )
        
    )
)

data <- read.csv(url("https://raw.githubusercontent.com/danielpetterson/DATA471-Time-Series/main/Data/energy_dataset_app.csv"))
server <- function(input, output) {
  
    # Read in and transform dataset
    df_long <- reactive({
     
       # Change to datetime var
      data$time <- as_datetime(data$time, tz="CET")
      
      # Removing outliers
      outlier_rows <- data[data$time %in% ymd_hms('2017-11-12 20:00:00', '2017-11-14 11:00:00', '2017-11-14 18:00:00'),]
      
      outlier_rows[outlier_rows == 0] <- NA
      
      data[data$time %in% ymd_hms('2017-11-12 20:00:00', '2017-11-14 11:00:00', '2017-11-14 18:00:00'),] <- outlier_rows
      
      
      data <- data %>% mutate(across(generation.biomass:price.actual, .fns = na_interpolation, option = 'linear')) %>% 
          select(c(1, starts_with("generation")))
      
            
        # Giving vars simpler names
        names(data) <- c("time",
                           "Biomass",
                           "Lignite",
                           "Gas",
                           "Hard Coal",
                           "Oil",
                           "Hydro Pumped",
                           "Hydro River",
                           "Hydro Reservoir",
                           "Nuclear",
                           "Other",
                           "Other Renewable",
                           "Solar",
                           "Waste",
                           "Wind Onshore")
        
        # Mean center data
        center_scale <- function(x) {
          scale(x, scale=FALSE)
        }
        
        if (input$center == "Mean Centered") {
        data %<>%
          mutate_all(., center_scale) %>%
          mutate(time = data$time)
        }
    
        
        # Subset to user selected dates
        data %<>%
          filter(time >= as_datetime(input$date_select[1])) %>%
          filter(time <= as_datetime(input$date_select[2])) %>%
          pivot_longer(cols = -time, names_to = "generation_source")
        

        # Subset to selected generation methods
        if(length(input$gen_method_select) > 0) {
            data %<>%
                dplyr::filter(generation_source %in% input$gen_method_select)
        }
        data
    })
    

    output$linePlot <- renderPlotly({
        req(df_long())
      # Plotting margins
      m <- list(
        l = 100,
        r = 50,
        b = 100,
        t = 75,
        pad = 4
      )
      
      # Smooth by user selected time period
      plot1_data <- df_long() %>% group_by(generation_source) %>% 
        summarise_by_time(time, .by = input$time_period_select, adjusted = mean(value))
      
      plot1 <- ggplot(data=plot1_data) + 
        geom_line(aes(x = time, y = adjusted, color = generation_source), stat = "identity", lwd = 1, alpha = 0.8) +
        ggtitle("Power Generation by Method") +
        ylab("Power Generated (MW)") +
        xlab("Time") +
        scale_colour_viridis_d(name = "Generation\nMethod")+
        theme(plot.title = element_text(hjust = 0.5),panel.background = element_rect(fill = 'white', color = 'white')) 
      
      if (input$vis_select == "Overlaid") {
        plot1 <- plot1
     } else if (input$y_scale_select == "Shared") {
        plot1 <- plot1 +
         theme(legend.position="none") +
         facet_wrap(~generation_source)
     } else {
        plot1 <- plot1 +
         theme(legend.position="none") +
         facet_wrap(~generation_source, scales = "free_y")
     }
      
      if (input$center == "Mean Centered") {
        plot1 <-plot1+
          geom_hline(yintercept=0, linetype="dashed")+
          labs(x="Time", y="Difference from Mean (MW)", title="Mean Centered Variation Over Time")
      }
      
      # Adjust output size
      ggplotly(plot1) %>%
       layout(autosize = T, width = NULL, height = 600, margin = m)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
