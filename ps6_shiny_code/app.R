#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)

trop <- read_delim("UAH-lower-troposphere-long.csv.bz2")

ui <- fluidPage(
  titlePanel("UAH Lower Troposphere Dataset Shiny App"),
  tabsetPanel(
    tabPanel("Main",
             h1("Introduction"),
             p("This app utilizes satellite temperature data from the University of Alabama in Huntsville"),
             p("This dataset contains", nrow(trop), "observations along with", ncol(trop) ,"variables")
    ),
    tabPanel("Graph",
             sidebarLayout(
               sidebarPanel(
                 selectInput("region", "Select a region to display:",
                             choices = c("globe", "globe_land", "globe_ocean", 
                                         "nh", "nh_land", "nh_ocean", 
                                         "sh", "sh_land", "sh_ocean",
                                         "trpcs", "trpcs_land", "trpcs_ocean",
                                         "noext", "noext_land", "noext_ocean",
                                         "soext", "soext_land", "soext_ocean",
                                         "nopol", "nopol_land", "nopol_ocean",
                                         "sopol", "sopol_land", "sopol_ocean",
                                         "usa48", "usa49", "aust"),
                             selected = "globe"),
                 selectInput("graph_type", "Select type of graph:", 
                             choices = c("Scatter Plot", "Line Plot")),
                 radioButtons("Colour", "Select a Colour:", choices = c("Black",
                                                                      "Red", "Yellow", "Blue"), selected = "Black")
               ),
               mainPanel(
                 plotOutput("graph_trop"),
                 textOutput("num_nonmissing_graph_obs")
               )
             )
    ), 
    tabPanel("Table",
             sidebarLayout(
               sidebarPanel(
                 selectInput("region_table", "Select a region to display:",
                             choices = c("globe", "globe_land", "globe_ocean", 
                                         "nh", "nh_land", "nh_ocean", 
                                         "sh", "sh_land", "sh_ocean",
                                         "trpcs", "trpcs_land", "trpcs_ocean",
                                         "noext", "noext_land", "noext_ocean",
                                         "soext", "soext_land", "soext_ocean",
                                         "nopol", "nopol_land", "nopol_ocean",
                                         "sopol", "sopol_land", "sopol_ocean",
                                         "usa48", "usa49", "aust"),
                             selected = "globe")
               ),
               mainPanel(
                 tableOutput("table_trop"),
                 textOutput("avg_temp")
               )              
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$graph_trop <- renderPlot({
    selected_region <- filter(trop, region == input$region)
    
    if (input$graph_type == "Scatter Plot") {
      ggplot(selected_region, aes(year, temp, alpha=0.5)) + 
        geom_point(col = input$Colour)
    } else {
      ggplot(selected_region, aes(year, temp, alpha=0.5)) + 
        geom_line(col = input$Colour)
    }
  })
  output$table_trop <- renderTable({
    selected_region <- filter(trop, region == input$region_table)
    selected_region %>% select(region, year, temp) %>% arrange(year)
  }) 
  
  output$avg_temp <- renderText({
    selected_region <- filter(trop, region == input$region_table)
    paste("Average temperature for", input$region_table, "is", mean(selected_region$temp))
  })
  
  output$num_nonmissing_graph_obs <- renderText({
    selected_region <- filter(trop, region == input$region)
    paste("There are", nrow(filter(selected_region, !is.na(temp))), "non-missing observations.")
  })
}
shinyApp(ui = ui, server = server)
