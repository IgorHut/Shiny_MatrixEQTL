## MatrixEQTL Shiny App##
#########################

library(shiny)
library(shinydashboard)



server <- function(input, output, session) { 
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  # The currently selected tab from the first box
  output$tabset1Selected <- renderText({
    input$tabset1
  })
  
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]], 
                  icon = icon("superpowers"))
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })
}

