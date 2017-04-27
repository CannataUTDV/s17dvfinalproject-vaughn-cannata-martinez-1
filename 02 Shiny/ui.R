#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(title = "DV Project 6"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crosstab examples", tabName = "menu1", icon = icon("th"),
        menuSubItem("Safety Index", tabName = "crosstab1", icon = icon("check")),
        menuSubItem("Lowest Inspection Scores", tabName = "crosstab2", icon = icon("check")),
        menuSubItem("People per Restaurant", tabName = "crosstab3", icon = icon("check"))
      ),
      menuItem("Barchart examples", tabName = "menu2", icon = icon("bar-chart"),
        menuSubItem("Average Scores by Zip Code", tabName = "barchart1", icon = icon("check")),
        menuSubItem("Public Transit", tabName = "barchart2", icon = icon("check")),
        menuSubItem("Nonnative Residents", tabName = "barchart3", icon = icon("check"))
      ),
      menuItem("Boxplot example", tabName = "boxplot", icon = icon("archive"))
    )
  ),
  dashboardBody(    
    tabItems(
      # Begin Crosstab1 tab content.
      tabItem("Safety Index, scores by year: A low Safety Index indicates a relatively high number of low inspection scores per period", tabName = "crosstab1",
        tabsetPanel(
            tabPanel("Data", "Set Safety Index Levels:",
              sliderInput("KPI1", "Low:", 
                          min = 0.0, max = 5.0,  value = 2.5),
              sliderInput("KPI2", "Medium:", 
                          min = 5.0, max = 10.0,  value = 7.5),
              actionButton(inputId = "click1",  label = "To get data, click here"),
              hr(), # Add space after button.
              DT::dataTableOutput("data1")
            ),
            tabPanel("Crosstab", plotOutput("plot1", height=1500))
          )
        ),
      # End Crosstab1 tab content.
      # Begin Crosstab2 content.
      tabItem("Lowest inspection scores by year and zip code", tabName = "crosstab2",
              tabsetPanel(
                tabPanel("Data", "Set Cutoff Score:",
                         sliderInput("Cutoff", "Cutoff:", 
                                     min = 20, max = 100,  value = 70),
                         actionButton(inputId = "click3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data3")
                ),
                tabPanel("Crosstab", plotOutput("plot3", height=1500))
              )
      ),
      # End Crosstab2 content.
      # Begin Crosstab3 tab content.
      tabItem("Number of people per restaurant in a given year", tabName = "crosstab3",
              tabsetPanel(
                tabPanel("Data", "Number of People per Resturant:",
                         sliderInput("Density1", "Low:", 
                                     min = 0.0, max = 50.0,  value = 30),
                         sliderInput("Density2", "Medium:", 
                                     min = 50, max = 1000,  value = 200),
                         actionButton(inputId = "click4",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data4")
                ),
                tabPanel("Crosstab", plotOutput("plot4", height=1500))
              )
      ),
      # End Crosstab3 tab content.
      # Begin Barchart1 tab content.
      tabItem("Average inspection score per zip code in a given year", tabName = "barchart1",
        tabsetPanel(
          tabPanel("Data",  
                   uiOutput("regions2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                   actionButton(inputId = "click2",  label = "To get data, click here"),
                   hr(), # Add space after button.
                   DT::dataTableOutput("data2")
          ),
          tabPanel("Barchart", "Black = Average Score per Year, Red = Grand Average Score, and  Blue = (Grand Average Score - Average Score per Year)", plotOutput("plot2", height=1500))
        )
      ),
      # End Barchart1 tab content.
      # Begin Barchart2 tab content.
      tabItem("Transit stops and restaurant density", tabName = "barchart2",
              tabsetPanel(
                tabPanel("Data", "Minimum Stops per Zip Code:",
                         sliderInput("Stops", "Slider is preset on average number of stops", 
                                     min = 1.0, max = 237.0,  value = 61),
                         actionButton(inputId = "click5", label = "Set minimum"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data5")
                ),
                tabPanel("Barchart", "Black = Number of Restaurants, Red = Average Number of Restaurants in Region, and Blue = Average Number of Restaurants in Selected Zip Codes", plotOutput("plot5", height=1500))
              )
      ),
      # End Barchart2 tab content.
      # Begin Boxplot tab content.
      tabItem("Restaurants with scores below cutoff value", tabName = "boxplot",
              tabsetPanel(
                tabPanel("Data", "Minimum score below:",
                         sliderInput("ScoreCutoff", "Slider is preset on cutoff for reinspection", 
                                     min = 36, max = 100,  value = 70),
                         actionButton(inputId = "click7", label = "Set cutoff"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data7")
                ),
                tabPanel("Boxplot", "Select zip code to view:", 
                         uiOutput("zip2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click8",  label = "Show data"),
                         hr(), # Add space after button.
                         
                         plotOutput("plot8", height=1500))
              )
      )
      # End Boxplot tab content.
      
    )
  ), skin = "green"
)

