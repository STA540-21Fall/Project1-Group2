#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(scales)
library(ggiraphExtra) 
library(shinydashboard)


### DATA WRANGLING STEER CLEAR ###

# load data

df <- read.csv("data/final.txt") %>%
  mutate(Region = ifelse(is.na(Region), "Caribbean island and unincorporated territory", Region)) %>%
  mutate(case_per = cases/population*100)



# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme(theme = "yeti"),
    
    # Application title
    # titlePanel("Covid Visualizations"),
    navbarPage("Socio-Economic Status vs. Covid Cases",
               
               tabPanel("Introduction",
                        navlistPanel(
                            "Instructions",
                            tabPanel("County View"),
                            tabPanel("Map View"),
                            "Data Insights",
                            tabPanel("Interesting County Views"),
                            tabPanel("Interesting Map Views"),
                            "-----",
                            tabPanel("Data Sources")
                        )
               ),#end tabpanel 2
               
               tabPanel("County View",
                        
                        sidebarLayout(
                          sidebarPanel(
                            #input choices
                            
                            # select peak
                            selectInput(inputId = "s_peak", 
                                        label = ("select peak"), 
                                        choices = unique(df$peak), 
                                        selected = unique(df$peak)[1]),
                            
                            # select region
                            selectInput(inputId = "s_region", 
                                        label = ("select region"), 
                                        choices = unique(df$Region), 
                                        multiple = TRUE,
                                        selected = unique(df$Region)[1]),
                            
                            uiOutput("s_state"),
                            
                            # select urban index
                            checkboxGroupInput(inputId = "s_urban", 
                                               label = ("select urbanization code(s)"), 
                                               choices = seq(1, 6),
                                               selected = c(1, 6)),
                            
                            # select factor
                            radioButtons(inputId = "s_factor", 
                                         label = ("select a factor"), 
                                         choices = c("median.income", 
                                                     "per_dem"), 
                                         selected = "median.income"),
                            
                            # select range to display
                            sliderInput(inputId = "slider", 
                                        label = ("slide for the range of 
                                                 cases-per-10-people to display"), 
                                        min = 0, 
                                        max = 50, 
                                        step = 5,
                                        value = c(0, 30))
                            
                          ), 
                          
                          mainPanel(
                            # output plot
                            plotOutput("dotplot")
                          )
                          ) #end sidebar layout
                        ),#end tabpanel County View
               
               tabPanel("Map View",
                        plotOutput('Covid_Map'),
                        
                        hr(),
                        
                        fluidRow( 
                            column(6, div(align = "left",h3("Time Period (Covid Cases Only)"),
                                          
                                          radioButtons(inputId ="peaks2",
                                                             label = "",
                                                             choices = c("Peak 1 (7/20/20)" = "pk1",
                                                                         "Peak 2 (1/8/21)" = "pk2",
                                                                         "Peak 3 (8/30/21)" = "pk3"
                                                             ),
                                                             selected = "pk1"))
                                   
                                   
                            ),#End Left Column Time Period
                            
                            column(6,
                                   div(align = "left",h3("Data"),
                                       radioButtons(inputId ="data",
                                                          label = "",
                                                          choices = c("Covid Cases" = "CC",
                                                                      "Urban Index" = "URB",
                                                                      "Median-Income" = "MI"
                                                          ),
                                                          selected = "CC"
                                       ),
                                       
                                   )# end div
                            ),#end middle Region
                            
                            
                        ),#end fluid row
                        fluidRow( column(12, style = "margin-top:100px;margin-left:35 px;",
                        submitButton(text = "Update Map")
                        ))
               )#end tab panel map view
               
    )#End NavbarPage
    
    
)#End UI


server <- function(input, output, session) {
  
  # filter data according to input choices
  selectedData <- reactive({
    df %>%
      filter(Region %in%  input$s_region,
             peaks == input$s_peak,
             urban_code  %in% input$s_urban
      )
  })
  
  # update state choices according to region input
  output$s_state = renderUI(
    selectInput(inputId = "s_region", 
                label = ("select state"), 
                choices = c("None", 
                            df$state[which(df$Region == input$s_region)]), 
                selected = "None")
  )
  
  # dotplot output
  output$dotplot <- renderPlot({
    ggplot(selectedData(), aes_string(x = input$s_factor, y = "case_per",
                                      color = "Region")) +
      geom_point(alpha = 0.4) +
      scale_y_continuous(limits = input$slider) +
      theme_bw()
  })

}#End server

# Run the application 
shinyApp(ui = ui, server = server)