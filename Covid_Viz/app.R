library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(ggiraphExtra) 
library(shinydashboard)
library(urbnmapr)
library(maps)
library(sf)
library(Rcpp)
library(tidyverse)


### DATA WRANGLING STEER CLEAR ###

# load data

df <- read_csv("data/final.csv")

###codes to download packages
# install.packages(“devtools”)
# devtools::install_github(“UrbanInstitute/urbnmapr”)

counties <- get_urbn_map(map = "counties", sf = TRUE)
total <- merge(counties, df, by.x = "county_fips", by.y = "fips")
total_rep <- total %>% 
  select(c("Case.per","Income.Group","Unemployed.Rate")) %>% 
  st_set_geometry(NULL)

total_rep1 <- total_rep %>% 
  select(c("Case.per"))

total_rep2 <- total_rep %>% 
  select(c("Income.Group","Unemployed.Rate"))

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
                                        voting or median income on a county level."),
                                 h3("Data Filters"),
                                 h4("Peaks"),p("The Peak filter allows for selecting data corresponding 
                                     to the 7 day average of covid cases on the specified date.3 Peaks in covid cases have been 
                                     identified using background data. The peaks chosen are July 20th 2020, January 8th 2021 and August 27th 2021.
                                        Selecting a date will display case data corresponding to the 7 day average of cases on that date."),
                                 h4("Regions"),p("The regions filter allows for selection of US counties based on 
                                                     census designated region to anlyze trends in different regions of the country.
                                                   To unselect regions use backspace."),
                                 h4("Urban Index"), p("The urban index filter allows for county data points to be filtered by
                                                          the NCHS urbanization classification index."), strong("1 = Urban, 6 = Rural"),
                                 h4("Factor"), p("The factor selection allows for the x-axis variable to be toggled between
                                                 median county income and % democrat votes."),
                                 h4("Slide for Cases-per-1000"),p("The slide allows for the y-axis range to be varied to allow for easier
                                                                   viewing of specific areas of the plot."),
                                 h3("Plot Information"),
                                 h4("Indiviudal County Information"),p("CLicking and dragging on the plot will create a small square on the plots.
                                                                       The points inside of this square will have their data information displayed 
                                                                       below the plot sorted in descending order of Covid Cases per 1000."),
                                 h3("Region Trendlines"), p("The trendlines on the scatterplot are linear regression lines of best fit for each region.")
                        ),
                        tabPanel("Map View",
                                 h2("County Level Map Plot"),
                                 p("The \"Map View\" tab is designed to give customized 
                                        map plots on Covid cases, unemployment and median income
                                   for each covid case peak."),
                                 p("The median-income and unemployment data is static and does not change between time periods. 
                                   While selecting a peak displayed a map of the 7-day averages of covid cases from the specified date.
                                   Blank counties and states in the map are caused by a lack of county or state level data of the variable.")),
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
                          checkboxGroupInput(inputId = "s_peak", 
                                      label = ("Select Peak(s)"), 
                                      choices = c("Peak 1 (7/20/20)" ="Peak 1",
                                                  "Peak 2 (1/8/21)" = "Peak 2",
                                                  "Peak 3 (8/27/21)" ="Peak 3"), 
                                      selected = unique(df$Peaks)[1]),
                          
                          # select region
                          selectInput(inputId = "s_region", 
                                      label = ("Select Region(s)"), 
                                      choices = unique(df$Region), 
                                      multiple = TRUE,
                                      selected = unique(df$Region)[1]), 
                          
                          # select urban index
                          checkboxGroupInput(inputId = "s_urban", 
                                             label = ("Select Urbanization Level(s) to display"), 
                                             choices = c("1-Urban"=1,
                                                         2,
                                                         3,
                                                         4,
                                                         5,
                                                         "6-Rural"=6),
                                             selected = c(1,2,3,4,5, 6)),
                          
                          # select factor
                          radioButtons(inputId = "s_factor", 
                                       label = ("Select a Factor"), 
                                       choices = c("Median Income" = "Median.Income", 
                                                   "% Democrat Votes" = "Per.Vote.Dem"), 
                                       selected = "Median.Income"),
                          
                          # select range to display
                          sliderInput(inputId = "slider", 
                                      label = ("Slide for the range of 
                                                 cases-per-1000-people to display"), 
                                      min = 0, 
                                      max = 8, 
                                      step = 0.5,
                                      value = c(0, 3))
                          
                        ), #end sidebar panel
                        
                        mainPanel(
                          # output plot
                          plotOutput("dotplot", brush = brushOpts(id = "dot_brush")),
                          
                          # output text
                          verbatimTextOutput("brush_info")
                        ) #end main panel
                      ) #end sidebar layout
             ),#end tabpanel County View
             
             tabPanel("Map View",
                      splitLayout(cellWidths = c("50%", "50%"),
                                  plotOutput('Covid_Map'),
                                  plotOutput('Covid_Map2')),
                      
                      hr(),
                      
                      fluidRow( 
                        column(6, div(align = "left",h3("Time Period (Covid Cases Only)"),
                                      varSelectInput("variable_case", "Variable:",
                                                     total_rep1,
                                                     selectize = FALSE,
                                                     selected = "Case.per"),
                                      radioButtons(inputId ="peaks2",
                                                   label = "",
                                                   choices = c("Peak 1 (7/20/20)" = "Peak 1",
                                                               "Peak 2 (1/8/21)" = "Peak 2",
                                                               "Peak 3 (8/30/21)" = "Peak 3"
                                                   ),
                                                   selected = "Peak 1"))
                               
                               
                        ),#End Left Column Time Period
                        
                        column(6,
                               div(align = "left",h3("Data"),
                                   varSelectInput("variable", "Variable:",
                                                  total_rep2,
                                                  selectize = FALSE,
                                                  selected = "Income.Group"),

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
             Peaks %in% input$s_peak,
             Urban.Code  %in% input$s_urban
      )
  
  })
  
  # dotplot output
  output$dotplot <- renderPlot({
    ggplot(selectedData(), aes_string(x = input$s_factor, y = "Case.per",
                                      color = "Region", group = "Region")) +
      geom_point(alpha = 0.5, size = 2.5) +
      geom_smooth(se = FALSE, method = "lm", size = 0.7) +
      theme_bw() +
      labs(x = if(input$s_factor == "Median.Income"){"Median Income"} else {"% Deomcrat Votes"},
           y = "Covid Case per 1000",
           title = paste0("Covid Case per 1000 people vs. ", 
                     if(input$s_factor == "Median.Income"){"Median Income"}
                      else {"% Deomcrat Votes"})) +
      theme(plot.title = element_text(size = 25)) +
      scale_y_continuous(limits = input$slider) +
      facet_wrap(~Peaks) 
  })
  
  # text output by brushing
  selected_points <- reactiveVal()
  
  observeEvent(input$dot_brush,{
    selected_points(brushedPoints(selectedData() %>%
                                    select(County, Region, Peaks, State, !!input$s_factor, Case.per), 
                                  input$dot_brush))
  })
  
  
  
  output$brush_info <- renderPrint({
    if (!is.null(input$dot_brush)) {
    print(selected_points() %>%
            as.data.frame() %>%
            mutate(Case.per = round(Case.per, 2)) %>%
            arrange(desc(Case.per)), 
          row.names = FALSE)
    }
      else {cat("Click and drag on point(s) to see details")}

  })
  
  # filter data according to input choices for map
  selectedData_new <- reactive({
    total %>%
      filter(Peaks == input$peaks2)
  })
  
  # map output for cases
  .e <- environment()
  output$Covid_Map <- renderPlot({
    ggplot(environment = .e)+
      geom_sf(selectedData_new(), mapping = aes(fill = !!input$variable_case), color = "grey", lwd = 0.01) +
      theme_bw()+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
      scale_fill_viridis_c(option = "magma",limits = c(0,3))
  })
  
  # map output for income group
  output$Covid_Map2 <- renderPlot({
    ggplot()+
      geom_sf(data = selectedData_new(), mapping = aes(fill = !!input$variable), 
              color = "grey", lwd = 0.01) +
      theme_bw()+
      theme(axis.text.x = element_blank(),axis.text.y = element_blank()) 
  })
  
}#End server

# Run the application 
shinyApp(ui = ui, server = server)
