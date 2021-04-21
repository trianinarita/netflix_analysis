shinyServer(function(input, output) {
  
# Wes Anderson Palette
  pal <- wes_palette("GrandBudapest2", 20, type = "continuous")
  
# Amount Content Each Year
  output$plot_contentyear <- renderPlotly({
    netflix$year <- year(netflix$date_added)
    
    plis <- netflix %>% 
      select("country", "year", "type") %>% 
      mutate(country = as.character(country)) %>% 
      separate_rows(country, sep = ",") %>% 
      mutate(country = as.factor(country))
    
    amount_by_country <- na.omit(plis) %>%
      mutate(country = na_if(country," ")) %>% 
      group_by(country, type, year) %>%
      summarise(count = n()) %>% 
      ungroup() %>% 
      filter(country %in% input$content_country) %>% 
      filter(type == input$content_types)
      
    plot_content <- ggplot(amount_by_country,aes(year, count, color = country, group = country, 
                                                 text = glue("Country: {country}
                     Type: {type}
                     Count: {count}
                     Year: {year}")
     )) +
      geom_line() +
      labs(title = "Content Amount in Each Year",
           x = "Year",
           y = "Total Content",
           color = "Country",
           legend = "Type"
      ) +
      theme_minimal() +
      scale_color_manual(values = wes_palette("GrandBudapest2", n = 200, type = "continuous")) +
      scale_x_continuous(limits = c(2010, 2021), n.breaks = 12) +
      scale_fill_gradientn(colours = pal)
    
    ggplotly(plot_content, tooltip = "text")
  })
  
# Type for Info Box
  netflix_agg <- netflix %>%
    count(type) %>% 
    mutate(text = paste("Total:", n)) %>% 
    group_by(type) %>%
    ungroup()
  
  type_movies <- netflix_agg %>% 
    filter(netflix_agg$type == "Movie")
  
  type_tvshows <- netflix_agg %>% 
    filter(netflix_agg$type == "TV Show")  
  
# Show Types
 # output$plot_showtypes <- renderPlotly({
 #   netflix_agg <- netflix %>%
 #     count(type) %>% 
 #     mutate(text = paste("Total:", n)) %>% 
 #     group_by(type) %>%
 #     ungroup()
 #   
 #   plot_type <- ggplot(netflix_agg, aes(x = type, y = n, text = text)) + 
 #     geom_col(aes(fill = type)) +
 #     labs(x = "Genre",
 #          y = "Genre Count") +
 #          #title = "Show Categories") +
 #     theme_minimal() +
 #     scale_y_continuous(breaks = seq(0, 5000, 500)) +
 #     scale_fill_brewer(palette = "YlOrRd")
 #   
 #   ggplotly(plot_type, tooltip = "text") %>% 
 #     layout(showlegend = FALSE)
 #  })
 
# Top 20 Genres
  output$plot_20genres <- renderPlotly({
    plot_genre <- netflix %>%
      select(listed_in) %>%
      mutate(listed_in = str_split(listed_in,',')) %>%
      unnest(listed_in) %>%
      group_by(listed_in) %>%
      count() %>%
      arrange(desc(n)) %>%
      head(20) %>%
      mutate(text = paste("Total:", n)) %>% 
      
      ggplot() +
      geom_col(aes(x = n, y = reorder(listed_in, n), fill = n, text = text)) +
      theme_minimal() +
      #scale_fill_gradient(low = "lightgoldenrod1", high = "firebrick") +
      labs(x = "Genre Count",
           y = "Genre",
           title = "Top 20 Genres",
           fill = "Count") +
      scale_fill_gradientn(colours = pal)
    
    ggplotly(plot_genre, tooltip = "text") %>% 
      layout(showlegend = FALSE)
  })
  
# Top 20 Countries  
  output$plot_20countries <- renderPlotly({
    plot_country <- netflix %>%
      filter(!str_detect(country,',')) %>%
      group_by(country) %>%
      count() %>%
      arrange(desc(n)) %>%
      head(20) %>%
      mutate(text = paste("Total:", n)) %>% 
      
      ggplot() + 
      geom_col(aes(x = n, y = reorder(country,n), fill = n, text = text)) +
      #scale_fill_gradient(low = "lightgoldenrod1", high = "firebrick") +
      theme_minimal() +
      labs(x = "Country",
           y = "Count",
           title = "Top 20 Countries",
           fill = "Count") +
      scale_fill_gradientn(colours = pal)
    
    ggplotly(plot_country, tooltip = "text")
  })
  
# Movie Duration
  output$plot_movieduration <- renderPlotly({
    pal <- wes_palette("GrandBudapest2", 20, type = "continuous")
    duration1 <- na.omit(netflix) %>% 
      filter(type == "Movie") %>%
      select("country", "duration") %>% 
      mutate(country = as.character(country)) %>% 
      separate_rows(country, sep = ",") %>% 
      mutate(country = as.factor(country))
    
    duration1$duration <- as.numeric(gsub(" min","", duration1$duration))
    
    duration_subset <- duration1[duration1$country %in% 
                                   c("United States", "India", "United Kingdom",
                                     "Canada", "France", "Japan", "Spain", "South Korea",
                                     "Mexico", "Australia"),]
    
    plot_duration <- ggplot(duration_subset,aes(x = country, y = duration, color = country, fill = country)) +
      geom_boxplot(aes(outlier.colour = country)) +
      labs(x = "Country",
           y = "Duration (in min)",
           title = "Box-Plots of Movie Duration in Top 10 Countries",
           margin = list(t = 54),
           legend = list(x = 100, y = 0.5)) +
      theme_minimal()+
      scale_fill_manual(values = wes_palette("GrandBudapest2", n = 10, type = "continuous")) +
      scale_color_manual(values = wes_palette("GrandBudapest2", n = 10, type = "continuous"))
    #scale_x_discrete(guide = guide_axis(n.dodge=0))
    
    fig <- ggplotly(plot_duration)
    fig$x$data <- lapply(fig$x$data, FUN = function(x){
      x$marker$outliercolor = x$line$color 
      x$marker$color = x$line$color 
      x$marker$line = x$line$color
      return(x)
    })
    fig
  })
  
# Full Data
  output$data_table <- DT::renderDataTable({
    
    DT::datatable(netflix, 
                  options = list(scrollX = T))
  })

# Download
  output$download <- downloadHandler( 
    filename = function() {paste("data-", Sys.Date(), ".csv", sep="")},
    content = function(file) {write.csv(netflix, file, row.names = F)
  })  
  
  })