# Define UI for application that draws a histogram; adding in some more css styling
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
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
      dygraphOutput("dygraph", height = "650px")
    )
  )
    )
