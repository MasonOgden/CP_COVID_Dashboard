#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)

round_up_nearest <- function(num) {
    num_zeros <- floor(log10(num))
    divisor <- as.integer(str_c(c(1, rep(0, num_zeros)), collapse=""))
    ceiling(num / divisor) * divisor
}

cp_ready <- read_csv("cp_dashboard_data.csv") %>%
    mutate(updated = mdy(updated)) %>%
    filter(wday(updated) %in% 2:6, # only keep weekdays (it's only updated on weekdays)
           updated != "2020-10-13") %>% # we don't have data on this day 
    mutate(daily_pos_on_campus = c(0, diff(total_pos_on_campus_res)),
           .after = 'total_pos_on_campus_res') %>%
    mutate(daily_pos_off_campus = c(0, diff(total_pos_off_campus_res)),
           .after = 'total_pos_off_campus_res')

font_settings <- list(
    family = "Source Sans Pro"
)

# https://universitymarketing.calpoly.edu/brand-guidelines/typography/

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    activate <- 3
    
    observeEvent(once = TRUE, ignoreInit = FALSE, eventExpr = 'shiny:connected', {
        # event will be called when activate changes, which is only once, when it is initially assigned
        showModal(modalDialog(
            box(title = p(h2('Welcome to the Cal Poly COVID-19 Visual Dashboard', style = 'font-family: Utopia; text-align: center'),
                             br(),
                             h4('Updated October 16th, 4:30 PM', style = 'font-family: Utopia; text-align: center')),
                width = '100%'),
            tags$img(src='cp_official2_wide2.jpg', style="display: block; margin-left: auto; margin-right: auto",
                     width = '100%',
                     height = '100%'),
            br(),
            fluidRow(
                valueBox(20, p("New Positive Tests", br(), br(), style = 'line-height: 0.01cm'), icon = icon("chart-line"), color = 'red', width = 2),
                valueBox(10000, "Total Tests Performed", icon = icon("plus-square"), color = 'green', width = 2),
                valueBox(10, p("Students Currently",
                          br(),
                          p("in Isolation"),
                          style = 'line-height: 0.01cm'), icon = icon('hourglass-end'), color = 'yellow', width = 2),
                valueBox(10, p("Students Currently",
                          br(),
                          p("in Quarantine"),
                          style = 'line-height: 0.01cm'), icon = icon('exclamation-triangle'), color = 'orange', width = 2),
                valueBox(234, p("Residents Quarantined" ,
                          br(),
                          p("in Place"),
                          style = 'line-height: 0.01cm'), icon = icon('house-user'), color = 'purple', width = 2),
                valueBox(0, p("Residents Currently",
                          br(),
                          p("Hospitalized"),
                          style = 'line-height: 0.01cm'), icon = icon('ambulance'), color = 'maroon', width = 2)
            )#,
            #h4('Created by Mason Ogden and Sydney Ozawa', style = 'text-align: center')
        ))
    })
    
    dates_to_show <- reactive({
        input$date_slider
    })
    
    plot_data <- reactive({cp_ready %>%
        filter(updated >= first(dates_to_show()),
               updated <= last(dates_to_show()))
    })
    
    pie_data <- cp_ready %>%
        select(total_pos_on_campus_res, total_pos_off_campus_res) %>%
        slice_tail() %>%
        pivot_longer(cols = c(total_pos_on_campus_res, total_pos_off_campus_res)) %>%
        rename(location = name, num_cases = value) %>%
        add_column(location_formatted = c("On-campus", "Off-campus"),
                   .after = 'location')
    
    output$case_pie <- renderPlotly({
        case_pie <- pie_data %>%
        plot_ly(labels = ~location_formatted, values = ~num_cases,
                marker = list(colors = c('#1b543b', '#9c9c80'))) %>%
        add_pie(hole = 0.4,
                text = str_c(pie_data$location_formatted, ': ', pie_data$num_cases),
                hoverinfo = 'text') %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               showlegend = FALSE,
               font = font_settings) %>%
        config(displaylogo = FALSE)
    })
        
    
    output$cp_daily_plot <- renderPlotly({
        cp_daily_plot <- plot_data() %>%
            plot_ly(type = 'bar') %>%
            add_trace(x = ~updated,
                      y = ~daily_pos_on_campus,
                      name = 'On-Campus',
                      text = str_c(strftime(plot_data()$updated, format="%b %d, %Y"), ':\n', plot_data()$daily_pos_on_campus, " new positive on-campus tests"),
                      hoverinfo = 'text',
                      marker = list(color = '#154734')) %>%
            add_trace(x = ~updated,
                      y = ~daily_pos_off_campus,
                      name = 'Off-Campus',
                      text = str_c(strftime(plot_data()$updated, format="%b %d, %Y"), ':\n', plot_data()$daily_pos_off_campus, " new positive off-campus tests"),
                      hoverinfo = 'text',
                      marker = list(color = '#F8E08E')) %>%
            layout(yaxis = list(title = 'New Positive Tests'),
                   xaxis = list(title = ''),
                   barmode = 'stack',
                   font = font_settings) %>%
            config(displaylogo = FALSE)
    })
    
    dates_to_show2 <- reactive({
        input$date_slider2
    })
    
    plot_data2 <- reactive({cp_ready %>%
            filter(updated >= first(dates_to_show2()),
                   updated <= last(dates_to_show2()))
    })
    
    y_max_rounded <- reactive({
        round_up_nearest(max(max(plot_data2()$total_tests_CHW), max(plot_data2()$total_tests_otp), na.rm = TRUE))
    })
    
    output$testing_totals_plot <- renderPlotly({
        testing_totals_plot <- plot_data2() %>%
            plot_ly(type = 'scatter', mode = 'lines+markers') %>%
            add_trace(x = ~updated, 
                      y = ~total_tests_CHW,
                      color = I("#154734"),
                      name = "Campus Health and Wellbeing (since Jul. 8, 2020)",
                      text = str_c(strftime(plot_data2()$updated, format="%b %d, %Y"), ':\n', plot_data2()$total_tests_CHW, ' total CHW tests performed'),
                      hoverinfo = 'text') %>%
            add_trace(x = ~updated,
                      y = ~total_tests_otp,
                      color = I("#F8E08E"),
                      name = "Ongoing Testing Program (since Oct. 3, 2020)",
                      text = str_c(strftime(plot_data2()$updated, format="%b %d, %Y"), ':\n', plot_data2()$total_tests_otp, ' total OTP tests performed'),
                      hoverinfo = 'text') %>%
            layout(yaxis = list(range = c(0, y_max_rounded()),
                                title = "Total Tests Performed"),
                   xaxis = list(title = ""),
                   legend = list(x = 0.03, y = 0.94),
                   font = font_settings) %>%
            config(displaylogo = FALSE)
    })
    
       
    dates_to_show3 <- reactive({
        input$date_slider3
    })    
     
    plot_data3 <- reactive({cp_ready %>%
        filter(updated >= first(dates_to_show3()),
               updated <= last(dates_to_show3()))
    })
    
    y_max_rounded2 <- reactive({
        round_up_nearest(max(max(plot_data3()$total_on_campus_res_iso), max(plot_data3()$total_on_campus_res_quar), na.rm = TRUE))
    })
        
    output$iso_quar_totals_plot <- renderPlotly({plot_data3() %>%
        plot_ly(type = 'scatter', mode = 'markers+lines') %>%
        add_trace(x = ~updated,
                  y = ~total_on_campus_res_iso,
                  color = I('#154734'),
                  name = 'Isolation', 
                  text = str_c(strftime(plot_data3()$updated, format="%b %d, %Y"), ':\n', plot_data3()$total_on_campus_res_iso, ' total resident students in isolation'),
                  hoverinfo = 'text') %>%
        add_trace(x = ~updated,
                  y = ~total_on_campus_res_quar,
                  color = I('#F8E08E'),
                  name = 'Quarantine', 
                  text = str_c(strftime(plot_data3()$updated, format="%b %d, %Y"), ':\n', plot_data3()$total_on_campus_res_quar, ' total resident students in isolation'),
                  hoverinfo = 'text') %>%
        layout(yaxis = list(title = "Number of Resident Students",
                            range = c(0, y_max_rounded2())),
               xaxis = list(title = ''),
               legend = list(x = 0.03, y = 0.94),
               font = font_settings) %>%
        config(displaylogo = FALSE)
    })
    
    dates_to_show4 <- reactive({
        input$date_slider4
    })    
    
    plot_data4 <- reactive({cp_ready %>%
            filter(updated >= first(dates_to_show4()),
                   updated <= last(dates_to_show4()))
    })
    
    y_max_rounded3 <- reactive({
        round_up_nearest(max(plot_data4()$total_on_quar_current_quar_in_place, na.rm = TRUE))
    })
    
    output$quar_in_place_plot <- renderPlotly({
        plot_data4() %>%
            plot_ly(type = 'scatter', mode = 'markers+lines') %>%
            add_trace(x = ~updated,
                      y = ~total_on_quar_current_quar_in_place,
                      color = I('#154734'),
                      text = str_c(strftime(plot_data4()$updated, format="%b %d, %Y"), ':\n', plot_data4()$total_on_quar_current_quar_in_place, ' total resident students'),
                      hoverinfo = 'text') %>%
            layout(showlegend=FALSE,
                   xaxis = list(title = ''),
                   yaxis = list(title = 'Number of Resident Students',
                   range = c(0, y_max_rounded3())),
                   font = font_settings) %>%
            config(displaylogo = FALSE)
            
    })
})
