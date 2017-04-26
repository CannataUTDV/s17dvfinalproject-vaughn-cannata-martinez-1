# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(plotly)


# Below is a data.world token to reduce connectivity issues.
connection <- data.world(token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Omt2YXVnaG4iLCJpc3MiOiJhZ2VudDprdmF1Z2huOjo2ZGQ3N2FjZS1kYjFkLTQ2ZTktODZmZi04MzIzYzQ2MTYyN2UiLCJpYXQiOjE0ODQ2OTcyNzUsInJvbGUiOlsidXNlcl9hcGlfd3JpdGUiLCJ1c2VyX2FwaV9yZWFkIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.FGeVf26qEOhxgRU9idrxcL75Jp84MOak_L0bGoZ33Yi1VFM9_McW7-vEtv3_AbkRH1NPfzWDy2Vn8LHSWGcAZg")

# The following query is for the select list in the first Barcharts tab.
regions = query(
  connection,
  #data.world(propsfile = "www/.data.world"),  
  dataset="kvaughn/s-17-dv-project-6", type="sql",  
  query="select distinct `Zip Code` as D, `Zip Code` as R 
  from Restaurant_Inspection_Scores order by 1"
) # %>% View()
region_list <- as.list(regions$D, regions$R)
region_list <- append(list("All" = "All"), region_list)

shinyServer(function(input, output) { 
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
  
  # This widget is for the Boxplot tab.
  minScore = reactive({input$ScoreCutoff})
  
# Begin Crosstab Tab 1 ------------------------------------------------------------------
  df1 <- eventReactive(input$click1, {
        print("Getting from data.world")
        query(
            connection,
            #propsfile = "www/.data.world",
            dataset="kvaughn/s-17-dv-project-6", type="sql",
            query="select
            cast(year(`Inspection Date`) as string) as `Inspection Year`,
            count(DISTINCT `Facility ID`), `Zip Code`, 
            min(Score) as min_score,
            sum(Score) / count(Score) as average_score,
            (sum(Score) / count(Score)) / (100 - min(Score)) as `Safety Index`,

            case
            when (sum(Score) / count(Score)) / (100 - min(Score)) < ? then '03 Low'
            when (sum(Score) / count(Score)) / (100 - min(Score)) < ? then '02 Medium'
            else '01 High'
            end AS kpi
            
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
  
  output$plot1 <- renderPlot({ggplot(df1()) +
    theme(axis.text.x=element_text(angle=90, size=14, vjust=0.5)) + 
    theme(axis.text.y=element_text(size=14, hjust=0.5)) +
    geom_text(aes(x=`Inspection Year`, y=as.character(`Zip Code`), label=round(average_score, 2)), size=4) +
    geom_tile(aes(x=`Inspection Year`, y=as.character(`Zip Code`), fill=kpi), alpha=0.50)
  })
# End Crosstab Tab 1 ___________________________________________________________
  
# Begin Crosstab Tab 2 ------------------------------------------------------------------
  df3 <- eventReactive(input$click3, {
    print("Getting from data.world")
    query(
      connection,
      #propsfile = "www/.data.world",
      dataset="kvaughn/s-17-dv-project-6", type="sql",
      query="select cast(year(`Inspection Date`) as string) as `Inspection Year`,
      `Zip Code`, count(`Process Description`) as numInspections, min(Score) as min_score,

      case
      when min(Score) < ? then 'Below Cutoff'
      else 'Above Cutoff'
      end as rating

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
  
  output$plot3 <- renderPlot({ggplot(df3()) +
      theme(axis.text.x=element_text(angle=90, size=14, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=14, hjust=0.5)) +
      geom_text(aes(x=`Inspection Year`, y=as.character(`Zip Code`), label=min_score), size=4) +
      geom_tile(aes(x=`Inspection Year`, y=as.character(`Zip Code`), fill=rating), alpha=0.50)
  })
  # End Crosstab Tab 2 ___________________________________________________________

  # Begin Crosstab Tab 3 ------------------------------------------------------------------
  df4 <- eventReactive(input$click4, {
    print("Getting from data.world")
    query(
      connection,
      #propsfile = "www/.data.world",
      dataset="kvaughn/s-17-dv-project-6", type="sql",
      query="SELECT zip.ZCTA5 as zipcode, 
      year(`Inspection Date`) as `Inspection Year`, zip.ZPOP as population, 
      count(distinct ris.`Facility ID`) as num_restaurants,
      sum(Score) / count(`Process Description`) as average_score,
      max(zip.ZPOP)/count(distinct ris.`Facility ID`) as density_display,

      case
      when max(zip.ZPOP)/count(distinct ris.`Facility ID`) < ? then '01 High Density'
      when max(zip.ZPOP)/count(distinct ris.`Facility ID`) < ? then '02 Medium Density'
      else '03 Low Density'
      end as density

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
  
  output$plot4 <- renderPlot({ggplot(df4()) +
      theme(axis.text.x=element_text(angle=90, size=14, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=14, hjust=0.5)) +
      geom_text(aes(x=`Inspection Year`, y=as.character(`zipcode`), label=round(density_display, 2)), size=4) +
      geom_tile(aes(x=`Inspection Year`, y=as.character(`zipcode`), fill=density), alpha=0.50)
  })
  # End Crosstab Tab 3 ___________________________________________________________
  
  # Begin Barchart Tab 1 ------------------------------------------------------------------
  df2 <- eventReactive(input$click2, {
    if(input$selectedRegions == 'All') region_list <- input$selectedRegions
    else region_list <- append(list("Skip" = "Skip"), input$selectedRegions)
    print("Getting from data.world")
      print(input$selectedRegions)
      tdf = query(
        connection,
        #propsfile = "www/.data.world",
        dataset="kvaughn/s-17-dv-project-6", type="sql",
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
    print(tdf2)
    dplyr::inner_join(tdf, tdf2, by = "Zip Code")
  })
  output$data2 <- renderDataTable({DT::datatable(df2(), rownames = FALSE,
                        extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot2 <- renderPlot({ggplot(df2(), aes(x=`Inspection Year`, y=average_score)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_col(position = "dodge") + 
      coord_flip() +
      facet_wrap(~`Zip Code`, ncol = 1) + 
       
      # Add avg_score, and (avg_score - window_avg_score) label.
      geom_text(mapping=aes(x=`Inspection Year`, y=average_score, label=round(average_score, 2)),colour="black") +
      geom_text(mapping=aes(x=`Inspection Year`, y=average_score, label=round(average_score - window_avg_score, 2)),colour="blue", hjust=-.75) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = window_avg_score), color="red") +
      geom_text(aes( -1, window_avg_score, label = round(window_avg_score, 2), vjust = -.5, hjust = -.25), color="red")
  })
  # End Barchart Tab 1 ___________________________________________________________
  
  # Begin Barchart Tab 2 ------------------------------------------------------------------
  df5 <- eventReactive(input$click5, {
    
    print("Getting from data.world")
    tempdf = query(
      connection,
      #propsfile = "www/.data.world",
      dataset="kvaughn/s-17-dv-project-6", type="sql",
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
  
  
  output$plot5 <- renderPlot({ggplot(df5(), aes(x=as.character(`Zip Code`), y=restaurants)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) +
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_col(position = "dodge") +
      coord_flip() +
      # Add number of restaurants label.
      geom_text(mapping=aes(x=as.character(`Zip Code`), y=restaurants, label=restaurants, hjust=1.25), colour="black") +
      # Add reference lines with labels.
      geom_hline(aes(yintercept = 110.21), color="red") +
      geom_text(aes( -1, 110.21, label = '110.21', vjust = -.5, hjust = -.25), color="red") +
      geom_hline(aes(yintercept = win_avg), color="blue") +
      geom_text(aes( -1, win_avg, label = round(win_avg, 2), vjust = -.5, hjust = -.25), color="blue")
  })
  # End Barchart Tab 2 ___________________________________________________________  
  # Begin Barchart Tab 3 ------------------------------------------------------------------
  df6 <- eventReactive(input$click6, {
    print("Getting from data.world")
    tempdf = query(
      connection,
      #propsfile = "www/.data.world",
      dataset="kvaughn/s-17-dv-project-6", type="sql",
      query="SELECT srp.`Zip Code`, fb.Native, fb.Naturalized, fb.Noncitizens, 
        (fb.Naturalized + fb.Noncitizens) as Nonnative, 
        (fb.Naturalized + fb.Noncitizens + fb.Native) as Population,
        100*(fb.Naturalized + fb.Noncitizens) / (fb.Naturalized + fb.Noncitizens + fb.Native) as Percent,
        srp.restaurants
        FROM `foreignbirth.csv/foreignbirth` fb
        JOIN `stops-restaurants-pop.csv/stops-restaurants-pop` srp
        ON fb.`Zip Code` = srp.`Zip Code`
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
  
  
  output$plot6 <- renderPlot({ggplot(df6(), aes(x=as.character(`Zip Code`), y=restaurants)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) +
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_col(position = "dodge") +
      coord_flip() +
      # Add number of restaurants label.
      geom_text(mapping=aes(x=as.character(`Zip Code`), y=restaurants, label=restaurants, hjust=1.25),colour="black") +
      # Add reference lines with labels.
      geom_hline(aes(yintercept = 110.21), color="red") +
      geom_text(aes( -1, 110.21, label = '110.21', vjust = -.5, hjust = -.25), color="red") +
      geom_hline(aes(yintercept = win_avg), color="blue") +
      geom_text(aes( -1, win_avg, label = round(win_avg, 2), vjust = -.5, hjust = -.25), color="blue")
  })
  # End Barchart Tab 3 ___________________________________________________________
  
  # Begin Boxplot Tab ------------------------------------------------------------
  df7 <- eventReactive(input$click7, {
    print("Getting from data.world")
    allscores = query(
      connection,
      #propsfile = "www/.data.world",
      dataset="kvaughn/finalproject", type="sql",
      query="select
        `Restaurant Name` as name, `Zip Code` as zipcode, `Facility ID`as ID, Score
        from `Restaurant_Inspection_Scores.csv/Restaurant_Inspection_Scores`
        ORDER BY `Zip Code`, `Restaurant Name`"
    ) 
    
    tdf = allscores %>% group_by(ID) %>% summarize(min_score = min(Score)) %>% filter(min_score < 70)
    dplyr::right_join(allscores, tdf, by = "ID") 
    
  })
  output$data7 <- renderDataTable({DT::datatable(df7(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  output$plot7 <- renderPlot({ggplot(df7()) +
      theme(axis.text.x=element_text(angle=90, size=14, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=14, hjust=0.5)) +
      geom_boxplot(aes(x=name, y=Score)) +
      facet_wrap(~zipcode, ncol = 1) + 
      coord_flip()
  })  
  
  # End Boxplot Tab ______________________________________________________________
})
