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
    theme = shinytheme(theme = "darkly"),
    
    # Application title
    titlePanel("Covid ____ Visualizations"),
    navbarPage("Data of Interest",
               
               tabPanel("Masking by Region",
                        
                        plotOutput('Covid_Masking'),
                        
                        hr(),
                        
                        fluidRow( 
                            column(4, div(align = "left",h3("Time Period"),
                                      
                                       checkboxGroupInput(inputId ="peaks",
                                                          label = NULL,
                                                          choices = c("Peak 1 (Date-Date)" = "pk1",
                                                                      "Peak2 (Date-Date)" = "pk2"
                                                          ),
                                                          selected = "pk1"))
                                      
                                   
                                   #Needs implementation for NCAA picture and player attributes, height weight                                  
                            ),#End Left Column Time Period
                           
                            column(4,
                                   div(align = "cetner",h3("Regions"),
                                       checkboxGroupInput(inputId ="regions",
                                                          label = NULL,
                                                          choices = c("Northeast" = "NE",
                                                                      "South" = "STH",
                                                                      "Midwest" = "MW",
                                                                      "West" = "WE"
                                                            ),
                                                          selected = c("NE","STH","MW","WE")
                                                          )
                                   )# end div
                            ),#end middle display
                            
                            column(4, div(align = "left",h3("Urban Index"),
                                          checkboxGroupInput(inputId ="urban",
                                                             label = NULL,
                                                             choices = c("1 - Urban" = "1",
                                                                         "2" = "2",
                                                                         "3" = "3",
                                                                         "4" = "4",
                                                                         "5" = "5",
                                                                         "6 - Rural" = "6"
                                                             ),
                                                             selected = c("1","6")
                                          )),
                                                                  
                            )#End Right column NBA player display
                
               )#end fluid row
                        
               ),#end tabpanel Masking by Region
               
               tabPanel("Panel 2"
               )#end tabpanel NBA
               
    )#End NavbarPage
    
    
)#End UI


server <- function(input, output, session) {
    output$Covid_Masking <- renderPlot({
        
    })#end render plot

}#End server

# Run the application 
shinyApp(ui = ui, server = server)