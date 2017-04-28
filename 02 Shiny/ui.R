#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Austin Restaurants"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Boxplot example", tabName = "boxplot", icon = icon("archive")),
      menuItem("Histogram example", tabName = "hist", icon = icon("signal")),
      menuItem("Scatterplot examples", tabName = "menu3", icon = icon("line-chart"),
        menuSubItem("Scatterplot 1", tabName = "scatter1", icon = icon("check")),
        menuSubItem("Scatterplot 2", tabName = "scatter2", icon = icon("check")),
        menuSubItem("Scatterplot 3", tabName = "scatter3", icon = icon("check"))
      ),
      menuItem("Crosstab examples", tabName = "menu1", icon = icon("th"),
        menuSubItem("Safety Index", tabName = "crosstab1", icon = icon("check")),
        menuSubItem("Lowest Inspection Scores", tabName = "crosstab2", icon = icon("check")),
        menuSubItem("People per Restaurant", tabName = "crosstab3", icon = icon("check"))
      ),
      menuItem("Barchart examples", tabName = "menu2", icon = icon("bar-chart"),
        menuSubItem("Average Scores by Zip Code", tabName = "barchart1", icon = icon("check")),
        menuSubItem("Public Transit", tabName = "barchart2", icon = icon("check")),
        menuSubItem("Nonnative Residents", tabName = "barchart3", icon = icon("check"))
      )
      
    )
  ),
  dashboardBody(
    tabItems(
      # Begin Boxplot tab content. ----------------------------------------------------------
      tabItem("Austin-area restaurants with scores below cutoff value, 2014-2017", tabName = "boxplot",
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
                         hr(), # Add space after button.
                         
                         plotOutput("plot7", height=1000))
              )
      ),
      # End Boxplot tab content. ____________________________________________________________
      # Begin Histogram tab content. ---------------------------------------------------------- 
      tabItem("Histogram of scores in selected year(s)", tabName = "hist",
              tabsetPanel(
                tabPanel("Data", "Choose Year(s) to Display:",
                         checkboxGroupInput("selectedYears", label = "Years", 
                                            choices = list(2014, 2015, 2016, 2017)),
                         # See http://shiny.rstudio.com/gallery/widget-gallery.html
                         actionButton(inputId = "click8",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data8")
                ),
                tabPanel("Histogram", plotOutput("plot8", height=1000))
              )
      ),
      # End Histogram tab content. ____________________________________________________________
      # Begin Scatter1 tab content. ----------------------------------------------------------    
      tabItem("Scatterplot 1", tabName = "scatter1",
              tabsetPanel(
                tabPanel("Data",  
                         uiOutput("regions2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click9",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data9")
                ),
                tabPanel("Plot", plotOutput("plot9", height=1000))
              )
      ),
      # End Scatter1 tab content. ____________________________________________________________
      # Begin Scatter2 tab content. ----------------------------------------------------------     
      tabItem("Scatterplot 2", tabName = "scatter2",
              tabsetPanel(
                tabPanel("Data",  
                         uiOutput("regions2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click10",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data10")
                ),
                tabPanel("Plot", plotOutput("plot10", height=1000))
              )
      ),      
      # End Scatter2 tab content. ____________________________________________________________
      # Begin Scatter3 tab content. ----------------------------------------------------------     
      tabItem("Scatterplot 3", tabName = "scatter3",
              tabsetPanel(
                tabPanel("Data",  
                         uiOutput("regions2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click11",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data11")
                ),
                tabPanel("Plot", plotOutput("plot11", height=1000))
              )
      ),        
      # End Scatter3 tab content. ____________________________________________________________
      # Begin Crosstab1 tab content. ----------------------------------------------------------
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
      # End Crosstab1 tab content. ____________________________________________________________
      # Begin Crosstab2 tab content. ----------------------------------------------------------
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
      # End Crosstab2 tab content. ____________________________________________________________
      # Begin Crosstab3 tab content. ----------------------------------------------------------
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
      # End Crosstab3 tab content. ____________________________________________________________
      
      # Begin Barchart1 tab content. ----------------------------------------------------------
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
      # End Barchart1 tab content. ____________________________________________________________
      # Begin Barchart2 tab content. ----------------------------------------------------------
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
      # End Barchart2 tab content. ____________________________________________________________
      # Begin Barchart3 tab content. ----------------------------------------------------------
      tabItem("Nonnative residents and restaurant density", tabName = "barchart3",
              tabsetPanel(
                tabPanel("Data", "Percent nonnative population:",
                         sliderInput("Nonnative", "Slider is preset on average percentage for region", 
                                     min = 7, max = 36,  value = 17.4),
                         actionButton(inputId = "click6", label = "Set minimum"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data6")
                ),
                tabPanel("Barchart", "Black = Number of Restaurants, Red = Average Number of Restaurants in Region, and Blue = Average Number of Restaurants in Selected Zip Codes", plotOutput("plot6", height=1500))
              )
      )
      # End Barchart3 tab content. ____________________________________________________________
      
      
    )
  ), skin = "green"
)

