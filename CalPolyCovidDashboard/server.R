#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    set.seed(122)
    
    hist_data <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- hist_data[seq_len(input$slider)]
        hist(data)
    })
})
