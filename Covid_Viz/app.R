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




#wrangle data here



#As of now you will be able to search from all NCAA players
#After selecting an NCAA player and searching you can then
#Select from the X NBA players most similar to your selected player
#Then you by hitting the compare button a radar graph will display
#Both players' college stats

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme(theme = "yeti"),
    
    # Application title
  #  titlePanel("Covid Visualizations"),
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
                        
                        plotOutput('Covid_Econ'),
                        
                        hr(),
                        
                        fluidRow( 
                            column(4, div(align = "left",h3("Time Period"),
                                      
                                       checkboxGroupInput(inputId ="peaks",
                                                          label = "",
                                                          choices = c("Peak 1 (7/20/20)" = "pk1",
                                                                      "Peak 2 (1/8/21)" = "pk2",
                                                                      "Peak 3 (8/30/21)" = "pk3"
                                                          ),
                                                          selected = "pk1"))
                                      
                                                                 
                            ),#End Left Column Time Period
                           
                            column(4,
                                   div(align = "left",h3("Regions"),
                                       checkboxGroupInput(inputId ="regions",
                                                          label = "",
                                                          choices = c("Northeast" = "NE",
                                                                      "South" = "STH",
                                                                      "Midwest" = "MW",
                                                                      "West" = "WE"
                                                            ),
                                                          selected = c("NE","STH","MW","WE")
                                                          )
                                   )# end div
                            ),#end middle Region
                            
                            column(4, div(align = "left",h3("Urban Index"),
                                          checkboxGroupInput(inputId ="urban",
                                                             label = "",
                                                             choices = c("1 - Urban" = "1",
                                                                         "2" = "2",
                                                                         "3" = "3",
                                                                         "4" = "4",
                                                                         "5" = "5",
                                                                         "6 - Rural" = "6"
                                                             ),
                                                             selected = c("1","6")
                                          )
                                          ),
                                                      
                            )#End Right column Urban Index
                
               ),#end fluid row
               fluidRow( column(12, style = "margin-top:23px;margin-left:35 px;",
                                submitButton(text = "Update Graphic")
               ))       
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
    output$Covid_Econ <- renderPlot({
        
    })#end render plot

}#End server

# Run the application 
shinyApp(ui = ui, server = server)