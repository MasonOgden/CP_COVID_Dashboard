#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


dashboardPage(
    dashboardHeader(title = "Cal Poly COVID-19\nVisual Dashboard"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            box(plotOutput("plot1", height=250)),
            
            box(
                title="Controls",
                sliderInput("slider", "Number of observations", 1, 100, 50)
            )
        )
    )
)