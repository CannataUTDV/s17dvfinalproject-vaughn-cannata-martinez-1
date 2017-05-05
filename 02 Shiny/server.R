# server.R
require(ggplot2)
require(quantreg)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(plotly)
library(rgdal)
library(rgeos)
library(leaflet)
library(maps)


# The following query is for the select list in the first Barcharts tab.
regions = query(
  
  data.world(propsfile = "www/.data.world"),  
  dataset="kvaughn/finalproject", type="sql",  
  query="select distinct `Zip Code` as D, `Zip Code` as R 
  from Restaurant_Inspection_Scores order by 1"
) 
region_list <- as.list(regions$D, regions$R)
region_list <- append(list("All" = "All"), region_list)

# This is for the Boxplot.
zipcodes = query(
  
  data.world(propsfile = "www/.data.world"),
  dataset="kvaughn/finalproject", type="sql",
  query="select distinct `Zip Code` as zipcode
  from Restaurant_Inspection_Scores order by 1"
)

zip_list <- as.list(zipcodes$zipcode)


# This code was adapted from http://stackoverflow.com/questions/33176378/mapping-zip-code-vs-county-shapefile-in-r,
# and can be used to create a set of polygons from a shapefile.

# dat<-readOGR("www/atx/",
#                   layer = "atx", GDAL1_integer64_policy = TRUE) 
# 
# # ----- Transform to EPSG 4326 - WGS84 (required)
# subdat<-spTransform(subdat, CRS("+init=epsg:4326"))
# 
# # ----- save the data slot
# subdat_data<-subdat@data[,c("ZCTA5CE10", "ALAND10", "GEOID10")]
# 
# # ----- to write to geojson we need a SpatialPolygonsDataFrame
# subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)

# Begin shinyServer section ------------------------------------------------------------
shinyServer(function(input, output) { 
  
  # This widget is for the Boxplot tab.
  minScore = reactive({input$ScoreCutoff}) 
  
  # This widget is for the Histogram tab.
  years_selected = reactive({input$selectedYears})
  
  # These widgets are for the first Crosstabs tab.
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
  
  # This widget is for the second Crosstabs tab.
  Cutoff = reactive({input$Cutoff})
  
  # These widgets are for the third Crosstabs tab.
  density_High = reactive({input$Density1})
  density_Medium = reactive({input$Density2})
  
  # This widget is for the first Barcharts tab.
  output$regions2 <- renderUI({selectInput("selectedRegions", "Choose Zip Codes:", region_list, multiple = TRUE) })
  
  # This widget is for the second Barcharts tab.
  minStops = reactive({input$Stops})
  
  # This widget is for the third Crosstabs tab.
  minPercent = reactive({input$Nonnative})

# Begin Boxplot Tab ------------------------------------------------------------
  df7 <- eventReactive(input$click7, {
    print("Getting from data.world")
    allscores = query(
      
      data.world(propsfile = "www/.data.world"),
      dataset="kvaughn/finalproject", type="sql",
      query="select
      `Restaurant Name` as name, `Zip Code` as zipcode, `Facility ID` as ID, Score
      from `Restaurant_Inspection_Scores.csv/Restaurant_Inspection_Scores`
      ORDER BY `Zip Code`, `Restaurant Name`"
    ) 
    
    tdf = allscores %>% group_by(ID) %>% summarize(min_score = min(Score)) %>% filter(min_score < minScore())
    dplyr::right_join(allscores, tdf, by = "ID") 
    
  })
  
  output$data7 <- renderDataTable({DT::datatable(df7(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  output$zip2 <- renderUI({
    zip <- as.data.frame(df7()) %>% group_by(zipcode) %>% distinct() %>% arrange(zipcode)
    selectInput("selectedZip", "Choose Zip Code:", zip, multiple = FALSE)
  })
  
  
  output$plot7 <- renderPlotly({
    tplot <- as.data.frame(df7()) %>% filter(zipcode == input$selectedZip)
    p <- ggplot(tplot) +
      theme_minimal() +
      labs(title = "Restaurants with failing scores in selected zip code:") +
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) +
      theme(axis.text.y=element_text(size=10, hjust=0.5)) +
      geom_boxplot(aes(x=name, y=Score)) +
      geom_hline(yintercept = minScore(), color = "red", alpha = 0.5) + 
      geom_text(aes( .75, minScore(), label = round(minScore(), 2)), color="red")
    ggplotly(p)
    
  })  
  
# End Boxplot Tab ______________________________________________________________
  
# Begin Histogram Tab -------------------------------------------------------
  
  df8 <- eventReactive(input$click8, {
    print("Getting from data.world")
    print(years_selected())
    tdf = query(
      
      data.world(propsfile = "www/.data.world"),
      dataset="kvaughn/finalproject", type="sql",
      query="select year(`Inspection Date`) as `Inspection Year`,  
      `Zip Code`, Score
      from Restaurant_Inspection_Scores
      where cast(year(`Inspection Date`) as string) in (?, ?, ?, ?)",
      queryParameters = years_selected()
    )
    
  })
   output$data8 <- renderDataTable({DT::datatable(df8(), rownames = FALSE,
                                                  extensions = list(Responsive = TRUE, FixedHeader = TRUE)
   )
   })
  output$plot8 <- renderPlotly({
    p <- ggplot(df8(), aes(Score)) +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) +
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_histogram(binwidth = 5, center = 2.5)
    ggplotly(p)
  })
# End Histogram Tab ____________________________________________________________
  
# Begin Scatterplot Tab 1 ------------------------------------------------------------------
  df9 <- eventReactive(input$click9, {
    print("Getting from data.world")
    query(
      
      data.world(propsfile = "www/.data.world"),
      dataset = "kvaughn/finalproject", type = "sql", 
      query = "select srp.`Zip Code` as zipcode, srp.stops, srp.restaurants, 
        srp.ZPOP as population, 100 * (fb.Noncitizens + fb.Naturalized) / fb.Native as percent_nonnative
        FROM `stops-restaurants-pop.csv/stops-restaurants-pop` srp
        JOIN `foreignbirth.csv/foreignbirth` fb ON srp.`Zip Code` = fb.`Zip Code`
        ORDER BY srp.`Zip Code`"
    )
  })
  
  output$data9 <- renderDataTable({DT::datatable(df9(), rownames = FALSE,
                            extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
    output$plot9 <- renderPlotly({
      tdf1 <- as.data.frame(df9())
      corrtest = cor.test(tdf1$population, tdf1$restaurants)
      pval = corrtest$p.value
      corr = corrtest$estimate
      p <- ggplot(tdf1) +
        theme_minimal() +
        theme(plot.caption = element_text(size=12)) +
        labs(x = "Population per Zip Code", y = "Number of Restaurants per Zip Code", 
           title = "Population by number of restaurants", 
           caption = paste("Correlation =", corr)) +
        geom_point(aes(x=population, y=restaurants, color = zipcode), 
                   show.legend = FALSE, shape = 21, fill = "red") +
        scale_colour_gradient(low = "black", high = "black") +
        geom_quantile(aes(x=population, y=restaurants), 
                      quantiles = 0.5, size = 2, color = "red", alpha = 0.5) +
        annotate("text", x = 45000, y = 50, 
                 label = paste("Correlation =", round(corr, 3), ", p-value:", round(pval, 3)))
      ggplotly(p)
    })
  
    output$plot10 <- renderPlotly({
      tdf2 <- as.data.frame(df9())
      corrtest = cor.test(tdf2$population, tdf2$stops)
      pval = corrtest$p.value
      corr = corrtest$estimate
      q <- ggplot(tdf2) +
        theme_minimal() +
        labs(x = "Population per Zip Code", y = "Number of Transit Stops per Zip Code",
           title = "Population by number of transit stops", 
           subtitle = paste("Correlation =", corr)) +
        geom_point(aes(x=population, y=stops, color = zipcode), 
                   show.legend = FALSE, shape = 21, fill = "blue") +
        scale_colour_gradient(low = "black", high = "black") +
        geom_quantile(aes(x=population, y=stops), 
                      quantiles = 0.5, size = 2, color = "blue", alpha = 0.5) +
        annotate("text", x = 50000, y = 25,
                 label = paste("Correlation =", round(corr, 3), ", p-value:", round(pval, 3)))
      ggplotly(q)
    })
  
    output$plot11 <- renderPlotly({
      tdf3 <- as.data.frame(df9())
      corrtest = cor.test(tdf3$restaurants, tdf3$stops)
      pval = corrtest$p.value
      corr = corrtest$estimate
      r <- ggplot(tdf3) +
        theme_minimal() +
        labs(x = "Number of Restaurants per Zip Code", y = "Number of Transit Stops per Zip Code",
             title = "Number of restaurants by number of transit stops") +
        geom_point(aes(x=restaurants, y=stops, color = zipcode), 
                   show.legend = FALSE, shape = 21, fill = "purple") +
        scale_colour_gradient(low = "black", high = "black") +
        geom_quantile(aes(x=restaurants, y=stops), 
                      quantiles = 0.5, size = 2, color = "purple", alpha = 0.5) +
        annotate("text", x = 300, y = 20, 
                 label = paste("Correlation =", round(corr, 3), ", p-value:", round(pval, 8)))
      ggplotly(r)
    })
  
# End Scatterplot Tab 1 ____________________________________________________________  
  
# Begin Crosstab Tab 1 ------------------------------------------------------------------
  df1 <- eventReactive(input$click1, {
        print("Getting from data.world")
        query(
          
          data.world(propsfile = "www/.data.world"),
            dataset="kvaughn/finalproject", type="sql",
            query="select
            cast(year(`Inspection Date`) as string) as `Inspection Year`,
            count(DISTINCT `Facility ID`), `Zip Code`, 
            min(Score) as min_score,
            sum(Score) / count(Score) as average_score,
            (sum(Score) / count(Score)) / (100 - min(Score)) as kpi,

            case
            when (sum(Score) / count(Score)) / (100 - min(Score)) < ? then '03 Low'
            when (sum(Score) / count(Score)) / (100 - min(Score)) < ? then '02 Medium'
            else '01 High'
            end AS `Safety Index`
            
            from `Restaurant_Inspection_Scores.csv/Restaurant_Inspection_Scores`
            group by `Zip Code`, year(`Inspection Date`)
            order by `Zip Code`",
            queryParameters = list(KPI_Low(), KPI_Medium())
          )
  })
  output$data1 <- renderDataTable({DT::datatable(df1(), rownames = FALSE,
                                extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  output$plot1 <- renderPlotly({
    p <- ggplot(df1()) +
      theme_minimal() +
      labs(x = "Inspection Year", y = "Zip Code",
           title = "Restaurant inspection scores by year, colored by Safety Index") +
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=10, hjust=0.5)) +
      geom_text(aes(x=`Inspection Year`, y=as.character(`Zip Code`), 
                    label=round(average_score, 2)), size=4) +
      geom_tile(aes(x=`Inspection Year`, y=as.character(`Zip Code`), fill=`Safety Index`), alpha=0.50)
    ggplotly(p)
  })
# End Crosstab Tab 1 ___________________________________________________________
  
# Begin Crosstab Tab 2 ------------------------------------------------------------------
  df3 <- eventReactive(input$click3, {
    print("Getting from data.world")
    query(
      
      data.world(propsfile = "www/.data.world"),
      dataset="kvaughn/finalproject", type="sql",
      query="select cast(year(`Inspection Date`) as string) as `Inspection Year`,
      `Zip Code`, count(`Process Description`) as numInspections, min(Score) as `Minimum Score`,

      case
      when min(Score) < ? then 'Below Cutoff'
      else 'Above Cutoff'
      end as Rating

      from `Restaurant_Inspection_Scores.csv/Restaurant_Inspection_Scores`
      group by `Zip Code`, year(`Inspection Date`)
      order by `Zip Code`",
      queryParameters = Cutoff()
    )
  })
  output$data3 <- renderDataTable({DT::datatable(df3(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  output$plot3 <- renderPlotly({
    p <- ggplot(df3()) +
      theme_minimal() +
      labs(x = "Inspection Year", y = "Zip Code",
           title = "Lowest inspection scores by year and zip code") +
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=10, hjust=0.5)) +
      geom_text(aes(x=`Inspection Year`, y=as.character(`Zip Code`), label=`Minimum Score`), size=4) +
      geom_tile(aes(x=`Inspection Year`, y=as.character(`Zip Code`), fill=Rating), alpha=0.50)
    ggplotly(p)
  })
  # End Crosstab Tab 2 ___________________________________________________________

  # Begin Crosstab Tab 3 ------------------------------------------------------------------
  df4 <- eventReactive(input$click4, {
    print("Getting from data.world")
    query(
      
      data.world(propsfile = "www/.data.world"),
      dataset="kvaughn/finalproject", type="sql",
      query="SELECT zip.ZCTA5 as zipcode, 
      year(`Inspection Date`) as `Inspection Year`, zip.ZPOP as population, 
      count(distinct ris.`Facility ID`) as num_restaurants,
      sum(Score) / count(`Process Description`) as average_score,
      max(zip.ZPOP)/count(distinct ris.`Facility ID`) as density_display,

      case
      when max(zip.ZPOP)/count(distinct ris.`Facility ID`) < ? then '01 High Density'
      when max(zip.ZPOP)/count(distinct ris.`Facility ID`) < ? then '02 Medium Density'
      else '03 Low Density'
      end as Density

      FROM `nrippner`.`fips-to-zip-code-crosswalk`.`fips_zip_x` as zip
      LEFT JOIN `Restaurant_Inspection_Scores.csv/Restaurant_Inspection_Scores` as ris 
      ON ris.`Zip Code` = zip.ZCTA5
      GROUP BY zip.ZCTA5, year(`Inspection Date`)
      order by zip.ZCTA5",
      queryParameters = list(density_High(), density_Medium())
    )
  })
  output$data4 <- renderDataTable({DT::datatable(df4(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  output$plot4 <- renderPlotly({
    p <- ggplot(df4()) +
      theme_minimal() +
      labs(x = "Inspection Year", y = "Zip Code",
           title = "Number of people per restaurant in a given year") +
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=10, hjust=0.5)) +
      geom_text(aes(x=`Inspection Year`, y=as.character(`zipcode`), 
                    label=round(density_display, 2)), size=4) +
      geom_tile(aes(x=`Inspection Year`, y=as.character(`zipcode`), fill=Density), alpha=0.50)
    ggplotly(p)
  })
# End Crosstab Tab 3 ___________________________________________________________
  
# Begin Barchart Tab 1 ------------------------------------------------------------------
  df2 <- eventReactive(input$click2, {
    if(input$selectedRegions == 'All') region_list <- input$selectedRegions
    else region_list <- append(list("Skip" = "Skip"), input$selectedRegions)
    print("Getting from data.world")
      tdf = query(
        
        data.world(propsfile = "www/.data.world"),
        dataset="kvaughn/finalproject", type="sql",
        query="select year(`Inspection Date`) as `Inspection Year`,  
            `Zip Code`, Score, sum(Score) / count(Score) as average_score
                from Restaurant_Inspection_Scores
                where ? = 'All' or cast(`Zip Code` as string) in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                group by year(`Inspection Date`), `Zip Code`",
       queryParameters = region_list
      )
      
      # This makes 'Inspection Year' a string for ease of graphing
      tdf$`Inspection Year` <- as.character(tdf$`Inspection Year`)
          
    # This creates a "window average" - an average across each individual pane.
    tdf2 = tdf %>% group_by(`Zip Code`) %>% 
      summarize(window_avg_score = mean(average_score))
    dplyr::inner_join(tdf, tdf2, by = "Zip Code")
  })
  output$data2 <- renderDataTable({DT::datatable(df2(), rownames = FALSE,
                        extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot2 <- renderPlotly({
    plot_ly(
      data = as.data.frame(df2()),
      y = ~`Inspection Year`,
      x = ~average_score,
      color = ~as.character(`Zip Code`),
      type = "bar"
    ) %>% group_by(`Zip Code`) %>%
      subplot()
    
  })
# End Barchart Tab 1 ___________________________________________________________
  
# Begin Barchart Tab 2 ------------------------------------------------------------------
  df5 <- eventReactive(input$click5, {
    
    print("Getting from data.world")
    tempdf = query(
      
      data.world(propsfile = "www/.data.world"),
      dataset="kvaughn/finalproject", type="sql",
      query="select ris.`Zip Code`, 
        (sum(ris.`Score`) / count(ris.`Process Description`)) as average_score,
        (srp.stops/srp.restaurants) as stops_per_rest,
        srp.stops, srp.restaurants, srp.ZPOP as population
        FROM `Restaurant_Inspection_Scores.csv/Restaurant_Inspection_Scores` ris
        JOIN `stops-restaurants-pop.csv/stops-restaurants-pop` srp
        ON ris.`Zip Code` = srp.`Zip Code`
        WHERE srp.stops > ?
        GROUP BY ris.`Zip Code`, (srp.stops/srp.restaurants), 
        srp.stops, srp.restaurants, srp.ZPOP
        ORDER BY ris.`Zip Code`",
        queryParameters = minStops()
    ) 
    
    # This gets the average number of restaurants within the selected group, and adds a column
    # to the existing data frame filled with that number.  This is for graphing purposes.
    tempdf2 = summarize(tempdf, window_avg = mean(restaurants))
    dplyr::mutate(tempdf, win_avg = as.numeric(tempdf2[1,1]))

  })
      
  output$data5 <- renderDataTable({DT::datatable(df5(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  
  output$plot5 <- renderPlotly({
    p <- ggplot(df5(), aes(x=as.character(`Zip Code`), y=restaurants)) +
      theme_minimal() +
      #scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) +
      theme(axis.text.y=element_text(size=10, hjust=0.5)) +
      geom_col(aes(fill = stops), show.legend = FALSE, position = "dodge") +
      scale_fill_gradient(low = "dark gray", high = "dark gray") +
      # Add number of restaurants label.
      geom_text(mapping=aes(x=as.character(`Zip Code`), y=restaurants, 
                            label=restaurants)) +
      # Add reference lines with labels.
      geom_hline(aes(yintercept = 110.21), color="red") +
      geom_text(aes( 2, 110.21, label = '110.21'), color="red") +
      geom_hline(aes(yintercept = win_avg), color="blue") +
      geom_text(aes( 2, win_avg, label = round(win_avg, 2)), color="blue") +
      labs(x = "Zip Code", y = "Number of Restaurants")
    ggplotly(p)
  })
# End Barchart Tab 2 ___________________________________________________________  
  
# Begin Barchart Tab 3 + Map ------------------------------------------------------------------
  df6 <- eventReactive(input$click6, {
    print("Getting from data.world")
    tempdf = query(
      
      data.world(propsfile = "www/.data.world"),
      dataset="kvaughn/finalproject", type="sql",
      query="SELECT srp.`Zip Code`, fb.Native, fb.Naturalized, fb.Noncitizens, 
        (fb.Naturalized + fb.Noncitizens) as Nonnative, 
        (fb.Naturalized + fb.Noncitizens + fb.Native) as Population,
        100*(fb.Naturalized + fb.Noncitizens) / (fb.Naturalized + fb.Noncitizens + fb.Native) as Percent,
        srp.restaurants, zcc.LAT, zcc.LNG
        FROM `foreignbirth.csv/foreignbirth` fb
        JOIN `stops-restaurants-pop.csv/stops-restaurants-pop` srp
        ON fb.`Zip Code` = srp.`Zip Code`
        JOIN `ZipCodeCoordinates.csv/ZipCodeCoordinates` zcc
        ON srp.`Zip Code` = zcc.ZIP
        WHERE (100*(fb.Naturalized + fb.Noncitizens) / (fb.Naturalized + fb.Noncitizens + fb.Native)) > ?
        ORDER BY srp.`Zip Code`",
      queryParameters = minPercent()
    ) 
    
    # This gets the average number of restaurants within the selected group, and adds a column
    # to the existing data frame filled with that number.  This is for graphing purposes.
    tempdf2 = summarize(tempdf, window_avg = mean(restaurants))
    dplyr::mutate(tempdf, win_avg = as.numeric(tempdf2[1,1]))
    
  })  

  output$data6 <- renderDataTable({DT::datatable(df6(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  
  output$plot6 <- renderPlotly({
    p <- ggplot(df6(), aes(x=as.character(`Zip Code`), y=restaurants)) +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) +
      theme(axis.text.y=element_text(size=10, hjust=0.5)) +
      geom_col(aes(fill = Percent), show.legend = FALSE, position = "dodge") +
      scale_fill_gradient(low = "dark gray", high = "dark gray") +
      # Add number of restaurants label.
      geom_text(mapping=aes(x=as.character(`Zip Code`), y=restaurants, 
                            label=restaurants, hjust=1.25),colour="black") +
      # Add reference lines with labels.
      geom_hline(aes(yintercept = 110.21), color="red") +
      geom_text(aes( 2, 110.21, label = '110.21', vjust = -.5, hjust = -.25), color="red") +
      geom_hline(aes(yintercept = win_avg), color="blue") +
      geom_text(aes( 2, win_avg, label = round(win_avg, 2), vjust = -.5, 
                     hjust = -.25), color="blue") +
      labs(x = "Zip Code", y = "Number of Restaurants")
    ggplotly(p)
  })
  
  
  
  output$Map1 <- renderLeaflet({
    mdf <- as.data.frame(df6())
    leaflet(width = 600, height = 900) %>%
      addTiles() %>%
      setView(-97.742589, 30.270569, zoom = 10) %>%
#      addPolygons(data=subdat, color = "#444444", weight = 1, smoothFactor = 0.5,
#      opacity = 1.0, fillOpacity = 0.1) %>%
      addCircleMarkers(lng = mdf$LNG, lat = mdf$LAT, radius = mdf$Percent/2) %>%
      addMarkers(lng = mdf$LNG,
        lat = mdf$LAT,
        options = markerOptions(riseOnHover = TRUE),
        popup = as.character(paste(mdf$`Zip Code`, 
          ", Percent Non-Native: ", round(mdf$Percent, 2),
          ", Number of restaurants: ", mdf$restaurants,
          ", Total Population: ", mdf$Population)) )
  })

# End Barchart Tab 3 + Map ___________________________________________________________
  

 })

# End shinyServer section _______________________________________________________________