
library(shiny)
library(RMySQL)

options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root",
  "password" = "dolphin"
))

databaseName <- "moviesurvey"
table <- "moviesurvey"

saveData <- function(data) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

loadData <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

# Define the fields we want to save from the form
fields <- c("name", "godfather", "shawshank", "schindler", "raging_bull", "casablanca", "citizen_kane")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    titlePanel("Survey: Top Six Movies of All Time"),
    h4("Rate each movie from 0 to 5 (zero=did not see - 5=highest rating)"),
    h6(""),
    DT::dataTableOutput("responses", width = 300), tags$hr(),
    textInput("name", "Enter your Name to begin Survey:", ""),
    sliderInput("godfather", "The Godfather (1972) R",
                0, 5, 0, ticks = TRUE),
    sliderInput("shawshank", "The Shawshank Redemption (1994) R",
                0, 5, 0, ticks = TRUE),
    sliderInput("schindler", "Schindler's List (1993) R",
                0, 5, 0, ticks = TRUE),
    sliderInput("raging_bull", "Raging Bull (1980) R  ",
                0, 5, 0, ticks = TRUE),
    sliderInput("casablanca", "Casablanca (1942)",
                0, 5, 0, ticks = TRUE),
    sliderInput("citizen_kane", "Citizen Kane (1941)",
                0, 5, 0, ticks = TRUE),
    actionButton("submit", "Submit")
  ),
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    })     
  }
)