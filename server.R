shinyServer(function(input, output, session){
  # reactive used for creating map
  air_map <- reactive({
    sum_df %>% 
      filter(year %in% input$years, 
             month >= input$months[[1]] & month <= input$months[[2]],
             date >= input$dates[[1]] & date <= input$dates[[2]],
             time >= input$times[[1]] & time <= input$times[[2]]) %>% 
      group_by(Station.name.district) %>% 
      summarise(lat = first(Latitude), 
                long = first(Longitude),
                min_pm10 = min(PM10),
                avg_pm10 = mean(PM10),
                max_pm10 = max(PM10),
                min_pm2.5 = min(PM2.5),
                avg_pm2.5 = mean(PM2.5),
                max_pm2.5 = max(PM2.5)) %>% 
      select(Station.name.district, lat, long, contains(input$finedust))
  })
  # Updating the date choices for month option
  observe({
    dates <- unique(sum_df[sum_df$year == input$years & 
                             sum_df$month == input$months, "date"])
    updateSelectInput(session, "dates",
                      choices = dates,
                      selected = dates[1])
  })
  # leaflet map of Seoul city
  output$map <- renderLeaflet({
    # assigning colors by the group of the pollutant level
    if (input$finedust == "PM10") {
      getColor <- sapply(air_map()$avg_pm10, getColor1)
    } else {
      getColor <- sapply(air_map()$avg_pm2.5, getColor2)
    }
    # icon of the map pin
    icons <- awesomeIcons(
      icon = "ios-cloud",
      iconColor = 'white',
      library = 'ion',
      markerColor = getColor)
    
    leaflet(air_map()) %>%
      addTiles() %>%
      addAwesomeMarkers(lat = air_map()$lat, 
                 lng = air_map()$long,
                 popup = ~paste('<b>',Station.name.district,'</b><br/>',
                                "Min:", round(air_map()[[4]]), '<br/>',
                                "Avg:", round(air_map()[[5]]), '<br/>',
                                "Max:", round(air_map()[[6]])),
                 icon = icons) %>% 
      addLegend(
        colors = c("#0099FF", "#4CBB17", "#F9A602", "#CC0000"),
        labels = if (input$finedust == "PM10") {
          c("Good: 0 ~ 30", "Normal: 30 ~ 80", "Bad: 80 ~ 150", "Very Bad: 150 ~ 600")
        } else {
          c("Good: 0 ~ 15", "Normal: 15 ~ 35", "Bad: 35 ~ 70", "Very Bad: 70 ~ 500")
        },
        opacity = 0.8
      )
  })
    # infoBox of neighborhood with fresh air
    output$good_neighborhood <- renderInfoBox({
    best_neighborhood = air_map() %>% arrange(air_map()[[5]])
    best = best_neighborhood %>% 
      summarise(first(Station.name.district), first(best_neighborhood[[5]]))
    col = ifelse(input$finedust == "PM10", getColor1(best[[2]]), getColor2(best[[2]]))
    
    infoBox(
      "Cleanest Air (Average)", best[[1]], icon = icon("thumbs-up", lib = "glyphicon"),
      color = col)
  })
  # infoBox of neighborhood with bad air
  output$bad_neighborhood <- renderInfoBox({
    worst_neighborhood = air_map() %>% arrange(desc(air_map()[[5]]))
    worst = worst_neighborhood %>% 
      summarise(first(Station.name.district), first(worst_neighborhood[[5]]))
    col = ifelse(input$finedust == "PM10", getColor1(worst[[2]]), getColor2(worst[[2]]))

    infoBox(
      "Worst Air (Average)", worst[[1]], icon = icon("thumbs-down", lib = "glyphicon"),
      color = col)
  })
  
  # infoBox with the min,avg,max values
  output$good_min <- renderInfoBox({
    best = air_map() %>% arrange(air_map()[[5]])
    min = best %>% summarise(first(best[[4]]))
    col = ifelse(input$finedust == "PM10", getColor1(min), getColor2(min))
    infoBox("Minimum", min,  
            icon = icon("stats", lib = "glyphicon"), 
            fill =T, color = col)
  })
  
  output$good_avg <- renderInfoBox({
    best = air_map() %>% arrange(air_map()[[5]])
    avg = best %>% summarise(first(best[[5]]))
    col = ifelse(input$finedust == "PM10", getColor1(avg), getColor2(avg))
    infoBox("Average", round(avg), 
            icon = icon("stats", lib = "glyphicon"), 
            fill =T, color = col)
  })
  
  output$good_max <- renderInfoBox({
    best = air_map() %>% arrange(air_map()[[5]])
    max = best %>% summarise(first(best[[6]]))
    col = ifelse(input$finedust == "PM10", getColor1(max), getColor2(max))
    infoBox("Maximum", max, 
            icon = icon("stats", lib = "glyphicon"), 
            fill =T, color = col)
  })
  
  output$bad_min <- renderInfoBox({
    worst = air_map() %>% arrange(desc(air_map()[[5]]))
    min = worst %>% summarise(first(worst[[4]]))
    col = ifelse(input$finedust == "PM10", getColor1(min), getColor2(min))
    infoBox("Minimum", min, 
            icon = icon("stats", lib = "glyphicon"), 
            fill =T, color = col)
  })
  
  output$bad_avg <- renderInfoBox({
    worst = air_map() %>% arrange(desc(air_map()[[5]]))
    avg = worst %>% summarise(first(worst[[5]]))
    col = ifelse(input$finedust == "PM10", getColor1(avg), getColor2(avg))
    infoBox("Average", round(avg), 
            icon = icon("stats", lib = "glyphicon"), 
            fill =T, color = col)
  })
  
  output$bad_max <- renderInfoBox({
    worst = air_map() %>% arrange(desc(air_map()[[5]]))
    max = worst %>% summarise(first(worst[[6]]))
    col = ifelse(input$finedust == "PM10", getColor1(max), getColor2(max))
    infoBox("Maximum", max,  
            icon = icon("stats", lib = "glyphicon"), 
            fill =T, color = col)
  })
  # updating the choices of the month for the trend tab
  observe({
    allyear <- if(input$years2 == "All") c("All") else c("All","1":"12")    
    updateSelectInput(session, "months2",
                      choices = allyear,
                      selected = allyear[1])
  })  
  # reactive used for selecting time for the trend tab
  trend_yr_mon <- reactive({
    sum_trend = sum_df %>% 
      select(dates, year, month, SO2, NO2, O3, CO, PM10, PM2.5) %>% 
      gather(key = type, value = value, 4:9)
    
    if (input$years2 == "All"){
      sum_trend = sum_trend %>% 
        filter(type %in% input$pollutants)
    } else if (input$months2 == "All"){
      sum_trend = sum_trend %>% 
        filter(year %in% input$years2,
               type %in% input$pollutants)
    } else {
      sum_trend = sum_trend %>% 
      filter(year %in% input$years2, 
             month %in% input$months2,
             type %in% input$pollutants) }
    
    sum_trend %>% 
      group_by(dates, type) %>% 
      summarise(month = first(month), avg = round(mean(value),4))
  })
  # line plot of the trend of pollutants selected
  output$trend <- renderPlotly({

    trend_plot = trend_yr_mon() %>% 
      ggplot(aes(x = as.Date(dates), y = avg, color = type)) +
      labs(x = "Timeline", y = "Daily Average")+
      geom_line() + geom_smooth(se = F) + theme_bw() +
      scale_color_brewer(name = "", palette="Set1")
    ggplotly(trend_plot)
  })
  # box plot of the pollutants selected
  output$pollutant_box <- renderPlotly({
    plot_ly(trend_yr_mon(), y = ~avg, color = ~type, type = "box", colors = "Set1") %>% 
      layout(yaxis = list(title = "Daily Average"))
  })
  # correlation heatmap of the pollutants selected
  output$correlation <- renderPlotly({
    poll_corr = trend_yr_mon() %>% 
      spread(key = type, value = avg) %>%
      data.frame() %>% 
      select(input$pollutants)
    
    cor <- round(cor(poll_corr),3)
    
    plot_ly(source = "heat_plot") %>% 
      add_heatmap(x = names(poll_corr), y = names(poll_corr), z = cor,
                  zauto = FALSE, zmin = -1, zmax = 1)
  })
  #reactive used to create bar plots
  good_day_barplot <- reactive({
    sum_df %>% 
      group_by(dates) %>% 
      summarise(year = first(year), 
                SO2_avg = mean(SO2),
                NO2_avg = mean(NO2),
                O3_avg = mean(O3),
                CO_avg = mean(CO),
                PM10_avg = mean(PM10),
                PM2.5_avg = mean(PM2.5),
                SO2_max = max(SO2),
                NO2_max = max(NO2),
                O3_max = max(O3),
                CO_max = max(CO),
                PM10_max = max(PM10),
                PM2.5_max = max(PM2.5)) %>%
      select(year, contains(input$pollute_bar))
  })
  # bar plot with daily average
  output$good_avg_bar <- renderPlotly({
    if (input$pollute_bar == "SO2"){
      good_day_bar = good_day_barplot() %>% 
        mutate(level = cut(good_day_barplot()[[2]], breaks = c(0, 0.021, 0.051, 0.151, 1),
                           include.lowest = T, right = F,
                           labels = c('Good', 'Normal', 'Bad', 'Very Bad')))
    } else if (input$pollute_bar == "NO2"){
      good_day_bar = good_day_barplot() %>%
        mutate(level = cut(good_day_barplot()[[2]], breaks = c(0, 0.03, 0.06, 0.20, 2),
                           include.lowest = T, right = T,
                           labels = c('Good', 'Normal', 'Bad', 'Very Bad')))
    } else if (input$pollute_bar == "O3"){
      good_day_bar = good_day_barplot() %>%
        mutate(level = cut(good_day_barplot()[[2]], breaks = c(0, 0.03, 0.09, 0.15, 0.5),
                           include.lowest = T, right = T,
                           labels = c('Good', 'Normal', 'Bad', 'Very Bad')))
    } else if (input$pollute_bar == "CO"){
      good_day_bar = good_day_barplot() %>%
        mutate(level = cut(good_day_barplot()[[2]], breaks = c(0, 2, 9, 15, 50),
                           include.lowest = T, right = T,
                           labels = c('Good', 'Normal', 'Bad', 'Very Bad')))
    } else if (input$pollute_bar == "PM10"){
      good_day_bar = good_day_barplot() %>%
        mutate(level = cut(good_day_barplot()[[2]], breaks = c(0, 30, 80, 150, 600),
                           include.lowest = T, right = T,
                           labels = c('Good', 'Normal', 'Bad', 'Very Bad')))
    } else if (input$pollute_bar == "PM2.5"){
      good_day_bar = good_day_barplot() %>%
        mutate(level = cut(good_day_barplot()[[2]], breaks = c(0, 15, 35, 75, 500),
                           include.lowest = T, right = T,
                           labels = c('Good', 'Normal', 'Bad', 'Very Bad')))
    }
    
    plot_good_days = good_day_bar %>% 
      group_by(year, level) %>% 
      summarise(n = n()) %>% 
      ggplot(aes(x = year, y = n))+
      geom_bar(aes(fill = level), stat = "identity", position = "dodge")+
      theme_bw()+ labs(x = "Year", y = "Count of Days")+
      scale_fill_manual(name = "", values = c("Good" = "#0099FF", "Normal" = "#00CC33", "Bad" = "#FFCC00", "Very Bad" = "#CC0000"))
    ggplotly(plot_good_days)
  })
  # bar plot with daily max
  output$good_max_bar <- renderPlotly({
    if (input$pollute_bar == "SO2"){
      good_day_bar = good_day_barplot() %>% 
        mutate(level = cut(good_day_barplot()[[3]], breaks = c(0, 0.02, 0.05, 0.15, 1),
                           include.lowest = T, right = T,
                           labels = c('Good', 'Normal', 'Bad', 'Very Bad')))
    } else if (input$pollute_bar == "NO2"){
      good_day_bar = good_day_barplot() %>%
        mutate(level = cut(good_day_barplot()[[3]], breaks = c(0, 0.03, 0.06, 0.20, 2),
                           include.lowest = T, right = T,
                           labels = c('Good', 'Normal', 'Bad', 'Very Bad')))
    } else if (input$pollute_bar == "O3"){
      good_day_bar = good_day_barplot() %>%
        mutate(level = cut(good_day_barplot()[[3]], breaks = c(0, 0.03, 0.09, 0.15, 0.5),
                           include.lowest = T, right = T,
                           labels = c('Good', 'Normal', 'Bad', 'Very Bad')))
    } else if (input$pollute_bar == "CO"){
      good_day_bar = good_day_barplot() %>%
        mutate(level = cut(good_day_barplot()[[3]], breaks = c(0, 2, 9, 15, 50),
                           include.lowest = T, right = T,
                           labels = c('Good', 'Normal', 'Bad', 'Very Bad')))
    } else if (input$pollute_bar == "PM10"){
      good_day_bar = good_day_barplot() %>%
        mutate(level = cut(good_day_barplot()[[3]], breaks = c(0, 30, 80, 150, 600),
                           include.lowest = T, right = T,
                           labels = c('Good', 'Normal', 'Bad', 'Very Bad')))
    } else if (input$pollute_bar == "PM2.5"){
      good_day_bar = good_day_barplot() %>%
        mutate(level = cut(good_day_barplot()[[3]], breaks = c(0, 15, 35, 75, 500),
                           include.lowest = T, right = T,
                           labels = c('Good', 'Normal', 'Bad', 'Very Bad')))
    }
    
    plot_good_days = good_day_bar %>% 
      group_by(year, level) %>% 
      summarise(n = n()) %>% 
      ggplot(aes(x = year, y = n))+
      geom_bar(aes(fill = level), stat = "identity", position = "dodge")+
      theme_bw()+ labs(x = "Year", y = "Count of Days")+
      scale_fill_manual(name = "", values = c("Good" = "#0099FF", "Normal" = "#00CC33", "Bad" = "#FFCC00", "Very Bad" = "#CC0000"))
    ggplotly(plot_good_days)
  })
  # density plot of the pollutant selected by year
  output$pollutant_density <- renderPlotly({
      density_plot = ggplot(data = sum_df, aes(x = .data[[input$pollute_bar]]))+
        geom_density(aes(fill = as.factor(year)), alpha=0.3)+
        scale_fill_discrete(name = "") +
        theme_bw() + labs(x = "Pollutant Level", y = "Density")
      ggplotly(density_plot)
  })
  # presenting the dataset 
  output$data <- DT::renderDataTable({
    datatable(sum_df[1:12], rownames= F) %>% 
      formatStyle(input$selected,
                  background = "skyblue", fontWeight = "bold")
  })
  # presenting the item info dataset
  output$infodata <- DT::renderDataTable({
    datatable(item_info[2:7], rownames = F) %>% 
      formatStyle(input$selected,
                  background = "skyblue", fontWeight = "bold")
  })
  # link to my linkedin page
  output$lkdn <- renderMenu({
    menuItem("LinkedIn", icon = icon("linkedin"),
             href = "https://www.linkedin.com/in/ryanhpark")
  })
  # link to my github page
  output$github <- renderMenu({
    menuItem("GitHub", icon = icon("github"),
             href = "https://www.github.com/ryanhpark")
  })
})

  