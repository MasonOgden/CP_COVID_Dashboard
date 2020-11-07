# font problems: https://stackoverflow.com/questions/55100069/ggplot-with-customized-font-not-showing-properly-on-shinyapps-io
#                https://community.rstudio.com/t/loading-custom-fonts-into-shinyapps/61168

# Cal Poly Typography: https://universitymarketing.calpoly.edu/brand-guidelines/typography/
# looks like you have to keep fonts in the www directory to eventually so that they load when it gets deployed.

#--------------------Packages--------------------#
library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(rvest)
library(purrr)
library(rdrop2)

# Get Dropbox authorization token
drop_auth(rdstoken = 'token.rds')

#--------------------To Do Next--------------------#

# 1. Convert all functions below to work with dropbox rather than local files
# 2. Figure out how to get all fonts and needed images onto app when it's published
# 3. Fix font problems on landing page
# 4. Work on landing page in general
# 5. Add employee testing numbers to testing plot


#--------------------Define Functions--------------------#

get_unused_html_names <- function(html_directory, current_data) {
    
    file_names <- list.files(paste0(getwd(), '/', html_directory))
    
    file_dates <- file_names %>%
        str_extract(pattern = r'(\w{3}_\d{1,2})') %>%
        str_replace('_', ' ') %>%
        str_c(', 2020') %>%
        mdy()
    
    file_df <- data.frame(file_dates, file_names)
    
    
    recent_data_date <- current_data %>%
        slice_tail() %>%
        pull(updated)
    
    unused_file_names <- file_df %>% 
        filter(file_dates > recent_data_date) %>%
        arrange(file_dates) %>%
        pull(file_names)
    
    if (length(unused_file_names) == 0) { # if there aren't any new file names
        return(NA)
    } else {
        str_c(html_directory, '/', unused_file_names)
    }
}

get_testing_table_info <- function(testing_table) {
    if (nrow(testing_table) == 11) {  # if they haven't changed the tables
        testing_table %>%
            pull(num) %>%
            str_remove_all(',') %>%
            as.numeric() %>%
            return()
        
    } else {
        testing_table %>%
            extract_testing_table_info() %>%
            return()
    }
}

get_iso_table_info <- function(isolation_table) {
    if (nrow(isolation_table) == 3) {  # if they haven't changed the tables
        isolation_table %>%
            pull(num) %>%
            str_remove_all(',') %>%
            as.numeric() %>%
            return()
        
    } else {
        isolation_table %>%
            extract_iso_table_info() %>%
            return()
    }
}

get_quar_table_info <- function(quar_table) {
    if (nrow(quar_table) == 2) {
        quar_table %>%
            pull(num) %>%
            str_remove_all(',') %>%
            as.numeric() %>%
            return()
        
    } else {
        quar_table %>%
            extract_quar_table_info() %>%
            return()
    }
}

get_qip_table_info <- function(qip_table) {
    if (nrow(qip_table) == 2) {
        qip_table %>%
            pull(num) %>%
            str_remove_all(',') %>%
            as.numeric() %>%
            return()
        
    } else {
        qip_table %>%
            extract_qip_table_info() %>%
            return()
    }
}

extract_testing_table_info <- function(testing_table) {
    c('total student tests from ongoing testing program', 'total employee tests from ongoing testing program',
      'total tests performed by campus health and wellbeing', 'chw in prior weekday', 'total positive tests of on-campus',
      'new positive tests of on-campus', 'total positive tests of off-campus', 'new positive tests of off-campus',
      'total recovered cases for on-campus', 'new recovered cases for on-campus', 'currently hospitalized') %>%
        map_dbl(~extract_associated_number(testing_table, .x))
}

extract_iso_table_info <- function(isolation_table) {
    c('total on-campus resident', 'new on-campus resident', 'remaining') %>%
        map_dbl(~extract_associated_number(isolation_table, .x))
}

extract_quar_table_info <- function(quar_table) {
    c('total', 'new') %>%
        map_dbl(~extract_associated_number(quar_table, .x))
}

extract_qip_table_info <- function(qip_table) {
    c('total', 'new') %>%
        map_dbl(~extract_associated_number(qip_table, .x))
}

extract_associated_number <- function(this_table, pattern) {
    this_table %>%
        filter(str_detect(str_to_lower(name), pattern)) %>% 
        pull(num) %>%
        str_remove_all(pattern=',') %>%
        as.numeric()
}

get_new_row <- function(unused_file) {
    
    updated <- unused_file %>%
        str_extract(pattern = r'(\w{3}_\d{1,2})') %>%
        str_replace('_', ' ') %>%
        str_c(', 2020') %>%
        mdy()
    
    my_html <- read_html(unused_file)
    
    html_tables <- my_html %>%
        html_nodes('table')
    
    testing_nums <- html_tables %>%
        .[[1]] %>%
        html_table() %>%
        rename(name = X1, num = X2) %>%
        get_testing_table_info()
    
    isolation_nums <- html_tables %>%
        .[[2]] %>%
        html_table() %>%
        rename(name = X1, num = X2) %>%
        get_iso_table_info()
    
    quar_nums <- html_tables %>%
        .[[3]] %>%
        html_table() %>%
        rename(name = X1, num = X2) %>%
        get_quar_table_info()
    
    qip_nums <- html_tables %>%
        .[[4]] %>%
        html_table() %>%
        rename(name = X1, num = X2) %>%
        get_qip_table_info()
    
    numerics <- c(testing_nums, isolation_nums, quar_nums, qip_nums)
    data.frame(updated = updated, total_tests_CHW = numerics[1], total_tests_empl_otp = numerics[2], total_tests_otp = numerics[3], total_tests_CHW_past_72 = numerics[4],
               total_pos_on_campus_res = numerics[5], new_pos_on_campus_res_past_72 = numerics[6], total_pos_off_campus_res = numerics[7],
               new_pos_off_campus_res_past_72 = numerics[8], total_recov_on_campus_res = numerics[9], new_recov_on_campus_res_past_72 = numerics[10],
               current_hosp_on_campus_res = numerics[11], total_on_campus_res_iso = numerics[12], new_on_campus_res_iso_past_72 = numerics[13],
               remaining_iso_beds = numerics[14], total_on_campus_res_quar = numerics[15], new_on_campus_res_quar_past_72 = numerics[16],
               total_on_quar_current_quar_in_place = numerics[17], new_on_campus_quar_in_place_past_72 = numerics[18])
}

append_new_data <- function(current_data, html_directory) {
    
    new_files <- html_directory %>%
        get_unused_html_names(current_data)
    
    if (is.na(new_files)) { # if there aren't any new file names to add to our data
        return(current_data)
    } else {
        new_data <- new_files %>%
            map_df(get_new_row)
        
        rbind(current_data, new_data) %>%
            return()
    }
}

round_up_nearest <- function(num) {
    num_zeros <- floor(log10(num))
    divisor <- as.integer(str_c(c(1, rep(0, num_zeros)), collapse=""))
    ceiling(num / divisor) * divisor
}



cp_updated <- drop_read_csv('/mn_html_files/cp_dashboard_data.csv') %>%
    mutate(updated = ymd(updated)) %>%
    append_new_data('html_files')

write.csv(cp_updated, file=paste0(getwd(), '/cp_dashboard_data.csv'), row.names=FALSE)
drop_upload('cp_dashboard_data.csv', path = 'mn_html_scraping')
#write.csv(cp_updated, file=paste0(getwd(), '/CalPolyCovidDashboard/cp_dashboard_data.csv'))
    

cp_ready <- drop_read_csv('/mn_html_files/cp_dashboard_data.csv') %>% # read in the new data
    mutate(updated = ymd(updated)) %>%
    filter(wday(updated) %in% 2:6, # keep only weekdays
           updated != "2020-10-13") %>% # we don't have data on this day
    mutate(updated = ymd(updated),
           daily_pos_on_campus = c(0, diff(total_pos_on_campus_res)),
           .after = 'total_pos_on_campus_res') %>%
    mutate(daily_pos_off_campus = c(0, diff(total_pos_off_campus_res)),
           .after = 'total_pos_off_campus_res')

font_settings <- list(
    family = "Source Sans Pro"
)

#--------------------Server--------------------#

shinyServer(function(input, output) {
    
    activate <- 3
    
    # eventExpr = 'shiny:connected'
    
    #---------------Landing Page---------------#
    
    observeEvent(once = TRUE, ignoreInit = FALSE, eventExpr = 'shiny:connected', {
        # event will be called when activate changes, which is only once, when it is initially assigned
        showModal(modalDialog(
            box(title = p(h2('Welcome to the Cal Poly COVID-19 Visual Dashboard', style = 'font-family: "Abolition"; text-align: center'),
                             #br(),
                             h4(textOutput('today_formatted'), style = 'font-family: Utopia; text-align: center')),
                width = '100%'),
            tags$img(src='cp_official2_wide2.jpg', style="display: block; margin-left: auto; margin-right: auto",
                     width = '100%',
                     height = '100%'),
            br(),
            fluidRow(
                box(width=1),
                column(p("New Positive Student Tests", style='color: #ffffff;'),
                       h3(textOutput("new_positive"), style='color: #ffffff;'),
                       width = 2,
                       style = "background-color:#154734; border-right: 3px solid white; border-left: 3px solid white;"),
                column(p("Total Tests Performed", style='color: #ffffff;'),
                       h3(textOutput("total_tests"), style='color: #ffffff;'),
                       width=2,
                       style = "background-color:#154734; border-right: 3px solid white; border-left: 3px solid white;"),
                column(p("Students Currently in Isolation", style='color: #ffffff;'),
                       h3(textOutput("stud_in_isolation"), style='color: #ffffff;'),
                       width = 2,
                       style = "background-color:#154734; border-right: 3px solid white; border-left: 3px solid white;"),
                column(p("Students Currently in Quarantine", style='color: #ffffff;'),
                       h3(textOutput("stud_in_quarantine"), style='color: #ffffff;'),
                       width = 2,
                       style = "background-color:#154734; border-right: 3px solid white; border-left: 3px solid white;"),
                column(p("Residents Currently Quarantined in Place", style='color: #ffffff;'),
                       h3(textOutput("stud_qip"), style='color: #ffffff;'),
                       width = 2,
                       style = "background-color:#154734; border-right: 3px solid white; border-left: 3px solid white;"),
                box(width = 1)
            )#,
            #h4('Created by Mason Ogden and Sydney Ozawa', style = 'text-align: center')
        ))
    })
    
    #----------Text Outputs for Landing Page----------#
    
    today_row <- cp_ready %>%
        slice_tail()
    
    output$today_formatted <- renderText({today_row %>%
            pull(updated) %>%
            strftime(format = "Updated %B %d, %Y")})
    
    output$new_positive <- renderText({today_row %>%
            select(daily_pos_on_campus, daily_pos_off_campus) %>%
            mutate(new_pos_students = daily_pos_on_campus + daily_pos_off_campus) %>%
            pull(new_pos_students)})
    
    output$total_tests <- renderText({today_row %>%
            select(total_tests_CHW, total_tests_empl_otp, total_tests_otp) %>%
            mutate(total_tests = total_tests_CHW + total_tests_empl_otp + total_tests_otp) %>%
            pull(total_tests)})
    
    output$stud_in_isolation <- renderText({today_row %>%
            pull(total_on_campus_res_iso)})
    
    output$stud_in_quarantine <- renderText({today_row %>%
            pull(total_on_campus_res_quar)})
    
    output$stud_qip <- renderText({today_row %>%
            pull(total_on_quar_current_quar_in_place)})
    
    #----------Reactives and Plots for Dashboard----------#
    
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
                            range = c(0, y_max_rounded2() + 2)),
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
