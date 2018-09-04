server <- function(input, output) {
  
  
  output$dygraph <- renderDygraph({
    
    # Need to convert dates from shiny input 
    start.date <- 
      input$myDateRange[1] %>%
      as_date() %>% 
      format(format = "%Y%m%d")
    
    end.date <- 
      input$myDateRange[2] %>%
      as_date() %>% 
      format(format = "%Y%m%d")
    
    
    # Here we are scraping coinmarketcap.com for the prices of the ripple cryptocurrency starting at the
    # 'start.date' and ending at 'today.date'
    myTable <- 
      paste0("https://coinmarketcap.com/currencies/", input$cryptoSelection ,"/historical-data/?start=", start.date, "&end=" , end.date) %>%
      read_html() %>%
      html_nodes("table") %>%
      html_table() %>% 
      .[[1]] %>%
      as.tibble()
    # 
    # # Convert Character Date from the Coin Market Cap website to date values for our charts 
    myTable <-
      myTable %>%
      mutate(Date = mdy(Date)) %>% 
      as.data.frame()
    
    
    row.names(myTable) <- myTable$Date
    
    myTable <- subset(myTable, select=-c(Date))
    myTable$mean <- apply(myTable[, 1:3], 1, mean)
    
    
    dygraph(xts::as.xts(as.matrix(myTable)), height = ) %>%
      dyCandlestick() %>% 
      dyUnzoom() %>% 
      dyRangeSelector()
    

    
  })
}


# input_size <- reactive(input$size)