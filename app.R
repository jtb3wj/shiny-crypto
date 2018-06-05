# Author: Jacob Bailey



# Make sure that the tidyverse package is installed
if(require(tidyverse)){
  print("Nice job: tidyverse is installed!")
}else{
  install.packages("tidyverse")
  library(tidyverse)
}


# Make sure that the rvest package is installed
if(require(rvest)){
  print("Nice job: rvest is installed!")
}else{
  install.packages("rvest")
  library(rvest)
}

# Make sure that lubridate package is installed
if(require(lubridate)){
  print("Nice job: lubridate is installed!")
}else{
  install.packages("lubridate")
  library(lubridate)
}

# Make sure that the shiny package is installed
if(require(shiny)){
  print("Nice job: shiny is installed!")
}else{
  install.packages("shiny")
  library(shiny)
}

# Make sure that ggthemes is installed
if(require(ggthemes)){
  print("Nice job: ggthemes is installed!")
}else{
  install.packages("ggthemes")
  library(ggthemes)
}

# Make sure that jsonlite is installed
if(require(jsonlite)){
  print("Nice job: jsonlite is installed!")
}else{
  install.packages("jsonlite")
  library(jsonlite)
}


coin.listing <- 
  "https://api.coinmarketcap.com/v2/listings/" %>% 
  parse_url() %>% 
  build_url() %>% 
  fromJSON(flatten = TRUE) %>% 
  .[[1]] %>% 
  arrange(name)

coin.listing.input <- coin.listing$name
names(coin.listing.input) <- coin.listing$website_slug

# Define UI for application that draws a histogram; adding in some more css styling
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      
      body {
        background-color:#202020;
      }

      h2 {
        color:#FFFFFF;
      }
      
      img {
        border-radius: 25px;
      }

    "))
  ),
   # Application title
   titlePanel("Find your crypto price!"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         dateRangeInput("myDateRange",
                     "Select Date Range: ",
                     start = today("UTC") - 31,  # at least a month ago
                     end = today("UTC") - 1 ,  # today
                     min = ymd("2012-01-01"),  # 2012-10-03~close to when bitcoin became a big thing
                     max = Sys.Date(),
                     format = "mm/dd/yyyy",
                     separator = "")
      ,
      selectInput("cryptoSelection",
                   "Select Crypto Currency: ",
                  coin.listing.input)),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot", height = "800px")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
   output$distPlot <- renderPlot({

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
       mutate(Date = mdy(Date))
     # 
     # # Clean up Prices so that we have dollar amounts along with cents
     myTable <-
       myTable %>%
       mutate(`Open Price` = sprintf("$%0.2f", `Open*`), `High Price` = sprintf("$%0.2f", High),
              `Low Price` = sprintf("$%0.2f", Low), `Close Price` = sprintf("$%0.2f", `Close**`))
     
     
     
     ggplot(myTable, aes(Date)) +
       scale_x_date(date_labels = "%m-%d-%y") +
       geom_line(aes(y = `Open*`, colour = "`Open*`")) +
       geom_line(aes(y = High, colour = "High")) +
       geom_line(aes(y = Low, colour = "Low")) +
       geom_line(aes(y = `Close**`, colour = "Close")) +
       geom_point(aes(y = `Open*`, colour = "Open")) +
       labs(x = "Date", y = "USD Equivalent $", title = str_to_title(str_replace_all(input$cryptoSelection, "[:punct:]", " "))) +
       theme_economist() +
       scale_color_economist() +
       geom_text(aes(y = `Open*`, 
                     label = sprintf("$%01.4f", `Open*`)),
                 check_overlap = TRUE,
                 fontface = "bold",
                 size = 3)
     
  
     

   })
}


input_size <- reactive(input$size)


# Run the application 
shinyApp(ui = ui, server = server)

