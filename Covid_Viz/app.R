library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(scales)
library(ggiraphExtra) 
library(shinydashboard)
library(urbnmapr)
library(maps)
library(sf)
library(Rcpp)


### DATA WRANGLING STEER CLEAR ###

# load data

df <- read_csv("data/final_daily.txt") %>%
  mutate(Region = ifelse(is.na(Region), "Caribbean island and unincorporated territory", Region)) %>%
  mutate(cases = ifelse(is.na(cases), 0, cases)) %>%
  drop_na(population) %>%
  mutate(case_per = cases/population*1000)

df$income_group <- cut(df$median.income,
                       breaks = c(0,40100,120400,Inf), 
                       labels = c('Low Income','Middle Income','Upper Income'))

counties <- get_urbn_map(map = "counties", sf = TRUE)
total <- merge(counties, df, by.x = "county_fips", by.y = "fips")
total_rep <- total %>% 
  select(c("case_per","income_group","unemployed.rate"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme(theme = "yeti"),
  
  # Application title
  # titlePanel("Covid Visualizations"),
  navbarPage("Socio-Economic Status vs. Covid Cases",
             
             tabPanel("Introduction",
                      navlistPanel(
                        "Instructions",
                        tabPanel("County View",
                                 h2("County Level Scatter Plot"),
                                 p("The \"County View\" tab is designed to give customized 
                                        scatterplots comparing Covid cases to politcal 
                                        affiliation or median income."),
                                 h3("Data Filters"),
                                 h4("Time Period"),p("The time period filter allows for selecting data corresponding 
                                     to the 7 day average of covid cases on that date.3 Peaks in covid cases have been 
                                     identified using background data. The peaks are July 20th 2020; January 8th 2021 and August 27th 2021.
                                        Selecting a date will display case data corresponding to the 7 day average of cases on that date."),
                                 h4("Regions"),p("The regions filter allows for selection of US counties based on 
                                                     census designated region to anlyze trends in different regions of the country.
                                                   To unselect regions use backspace."),
                                 h4("Urban Index"), p("The urban index filter allows for county data points to be filtered by
                                                          the NCHS urbanization classification index."), strong("1 = Urban, 6 = Rural"),
                        ),
                        tabPanel("Map View",
                                 h2("County Level Map Plot"),
                                 p("The \"Map View\" tab is designed to give customized 
                                        map plots on Covid cases, median income and urbanization
                                   under each peak.")),
                        "Data Insights",
                        tabPanel("Interesting County Views"),
                        "-----",
                        tabPanel("Data Sources",
                                 h2("Data Source"),
                                 h4("Median County Income (2019) - U.S. of labor statistics"),
                                 h4("Urbanization Index - CDC NCHS Urban-Rural Classification"),
                                 h4("Covid-19 Case Counts by County - New York Times"),
                                 h4("U.S. 2020 Election Data by County - Tony McGovern public Github data"))
                      )
             ),#end tabpanel 2
             
             tabPanel("County View",
                      
                      sidebarLayout(
                        sidebarPanel(
                          #input choices
                          
                          # select peak
                          selectInput(inputId = "s_peak", 
                                      label = ("Select Peak"), 
                                      choices = c("Peak 1 (7/20/20)" ="peak 1",
                                                  "Peak 2 (1/8/21)" = "peak 2",
                                                  "Peak 3 (8/27/21)" ="peak 3"), 
                                      selected = unique(df$peaks)[1]),
                          
                          # select region
                          selectInput(inputId = "s_region", 
                                      label = ("Select Region"), 
                                      choices = unique(df$Region), 
                                      multiple = TRUE,
                                      selected = unique(df$Region)[1]),
                          
                          # select urban index
                          checkboxGroupInput(inputId = "s_urban", 
                                             label = ("Select Urbanization Level(s)"), 
                                             choices = c("1-Urban"=1,
                                                         2,
                                                         3,
                                                         4,
                                                         5,
                                                         "6-Rural"=6),
                                             selected = c(1, 6)),
                          
                          # select factor
                          radioButtons(inputId = "s_factor", 
                                       label = ("Select a Factor"), 
                                       choices = c("Median Income" = "median.income", 
                                                   "% Democrat Votes" = "per_dem"), 
                                       selected = "median.income"),
                          
                          # select range to display
                          sliderInput(inputId = "slider", 
                                      label = ("Slide for the range of 
                                                 cases-per-1000-people to display"), 
                                      min = 0, 
                                      max = 8, 
                                      step = 1,
                                      value = c(0, 4))
                          
                        ), #end sidebar panel
                        
                        mainPanel(
                          # output plot
                          plotOutput("dotplot")
                        ) #end main panel
                      ) #end sidebar layout
             ),#end tabpanel County View
             
             tabPanel("Map View",
                      plotOutput('Covid_Map'),
                      
                      hr(),
                      
                      fluidRow( 
                        column(6, div(align = "left",h3("Time Period (Covid Cases Only)"),
                                      
                                      radioButtons(inputId ="peaks2",
                                                   label = "",
                                                   choices = c("Peak 1 (7/20/20)" = "peak 1",
                                                               "Peak 2 (1/8/21)" = "peak 2",
                                                               "Peak 3 (8/30/21)" = "peak 3"
                                                   ),
                                                   selected = "peak 1"))
                               
                               
                        ),#End Left Column Time Period
                        
                        column(6,
                               div(align = "left",h3("Data"),
                                   varSelectInput("variable", "Variable:",
                                                  total_rep, selected = "case_per"),
                                   
                                   # radioButtons(inputId ="data",
                                   #                    label = "",
                                   #                    choices = c("Covid Cases" = "cases",
                                   #                                "Urban Index" = "unemployed.rate",
                                   #                                "Median-Income" = "income_group"
                                   #                    ),
                                   #                    selected = "cases"
                                   # ),
                                   
                               )# end div
                        ),#end middle Region
                        
                        
                      )#end fluid row
                      
             )#end tab panel map view
             
  )#End NavbarPage
  
  
)#End UI


server <- function(input, output) {
  
  
  # filter data according to input choices
  selectedData <- reactive({
    df %>%
      filter(Region %in%  input$s_region,
             peaks == input$s_peak,
             urban_code  %in% input$s_urban
      )
  })
  
  # dotplot output
  output$dotplot <- renderPlot({
    ggplot(selectedData(), aes_string(x = input$s_factor, y = "case_per",
                                      color = "Region")) +
      geom_point(alpha = 0.5, size = 2.5) +
      labs(x = if(input$s_factor == "median.income"){"Median Income"} else {"% Deomcrat Votes"},
           y = "Covid Case per 1000")+
      scale_y_continuous(limits = input$slider) +
      theme_bw()
  })
  
  # filter data according to input choices for map
  
  selectedData_new <- reactive({
    total %>%
      filter(peaks == input$peaks2)
  })
  
  # map output
  output$Covid_Map <- renderPlot({
    ggplot()+
      geom_sf(selectedData_new(), mapping = aes(fill = !!input$variable), color = "white", lwd = 0.1) +
      theme_dark()
  })
  
}#End server

# Run the application 
shinyApp(ui = ui, server = server)