#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# first-aid

library(shiny)
library(plotly)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(dashboardthemes)

### creating custom theme object
customTheme <- shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Source Sans Pro" # Font of everything
    ,appFontColor = "rgb(45,45,45)"
    ,primaryFontColor = "rgb(15,15,15)"
    ,infoFontColor = "rgb(15,15,15)"
    ,successFontColor = "rgb(15,15,15)"
    ,warningFontColor = "rgb(15,15,15)"
    ,dangerFontColor = "rgb(15,15,15)"
    ,bodyBackColor = "rgb(240,240,240)" # background of body (behind the boxes)
    
    ### header
    ,logoBackColor = "#154734" # background of header (top)
    
    ,headerButtonBackColor = "#154734" # background of menu button background when mouse is not over it
    ,headerButtonIconColor = "rgb(220,220,220)" # color of menu icon when mouse is not over it
    ,headerButtonBackColorHover = "#3A913F" # color of menu button background when mouse is over it
    ,headerButtonIconColorHover = "rgb(60,60,60)" # color menu button icon when mouse is over it
    
    ,headerBackColor = "#154734" # Top bar to the right of header (not including button)
    ,headerBoxShadowColor = "#dfdfdf" # not sure what this does
    ,headerBoxShadowSize = "3px 5px 5px" # or this
    
    ### sidebar
    ,sidebarBackColor = "rgb(255,255,255)" # background of entire sidebar
    ,sidebarPadding = 0 # how much space on each side of tabs in sidebar
    
    ,sidebarMenuBackColor = "transparent" # background color of non-active tabs in sidebar
    ,sidebarMenuPadding = 0 # how much space on each side of tabs, and on top and bottom in sidebar
    ,sidebarMenuBorderRadius = 0 # makes the current sidebar overlay color curved at the edges (like in blue gradient)
    
    ,sidebarShadowRadius = "3px 5px 5px" # size of sidebar shadow
    ,sidebarShadowColor = "#dfdfdf" # color of shadow on right side of sidebar (between sidebar and body)
    
    ,sidebarUserTextColor = "rgb(115,115,115)" # not sure
    
    ,sidebarSearchBackColor = "rgb(240,240,240)"   # \
    ,sidebarSearchIconColor = "rgb(100,100,100)"   # |- control colors of search bar, which I don't have
    ,sidebarSearchBorderColor = "rgb(220,220,220)" # /
    
    ,sidebarTabTextColor = "rgb(100,100,100)" # color of text in sidebar tabs
    ,sidebarTabTextSize = 14 # size of text in sidebar tabs
    ,sidebarTabBorderStyle = "none" # not sure
    ,sidebarTabBorderColor = "none" # not sure
    ,sidebarTabBorderWidth = 0 # not sure
    
    ,sidebarTabBackColorSelected = "rgb(230,230,230)" # color of active tab rgb(230,230,230) #3A913F
    ,sidebarTabTextColorSelected = "rgb(0,0,0)" # color of text of active tab (gets darker when selected)
    ,sidebarTabRadiusSelected = "0px" # makes selection highlight rounded at the edges
    
    ,sidebarTabBackColorHover = "rgb(245,245,245)" # background color of tab you're hovering over rgb(245,245,245) #A4D65E
    ,sidebarTabTextColorHover = "rgb(0,0,0)" # text color of tab you're hovering over (gets darker)
    ,sidebarTabBorderStyleHover = "none solid none none" # 'solid' adds the vertical solid line on right side of active tab
    ,sidebarTabBorderColorHover = "rgb(200,200,200)" # color of vertical solid line on right side of active tab
    ,sidebarTabBorderWidthHover = 4 # width of vertical solid line
    ,sidebarTabRadiusHover = "0px" # rounds edges of hovering highlight
    
    ,boxBackColor = "rgb(248,248,248)" # background color of boxes
    ,boxBorderRadius = 0 # how much to round the edges of the boxes
    ,boxShadowSize = "none" # toggle box shadows
    ,boxShadowColor = "" # color of box shadow
    ,boxTitleSize = 18 # font size of box title
    ,boxDefaultColor = "rgb(248,248,248)" # changes color of little horizontal bar on top of some boxes (like my text boxes) rgb(225,225,225)
    ,boxPrimaryColor = "rgb(95,155,213)" # \
    ,boxInfoColor = "rgb(180,180,180)"   #  \
    ,boxSuccessColor = "rgb(112,173,71)" #   |- control horizontal bar colors of different statuses
    ,boxWarningColor = "rgb(237,125,49)" #  /
    ,boxDangerColor = "rgb(232,76,34)"   # /
    
    ,tabBoxTabColor = "rgb(248,248,248)"          # \
    ,tabBoxTabTextSize = 14                       #  \
    ,tabBoxTabTextColor = "rgb(100,100,100)"      #   \ 
    ,tabBoxTabTextColorSelected = "rgb(45,45,45)" #    |- settings for if you have boxes with tabs
    ,tabBoxBackColor = "rgb(248,248,248)"         #   /
    ,tabBoxHighlightColor = "rgb(200,200,200)"    #  /
    ,tabBoxBorderRadius = 5                       # /
    
    ### inputs
    ,buttonBackColor = "rgb(215,215,215)" # not sure
    ,buttonTextColor = "rgb(45,45,45)" # not sure
    ,buttonBorderColor = "rgb(150,150,150)" # not sure
    ,buttonBorderRadius = 5 # not sure
    
    ,buttonBackColorHover = "rgb(190,190,190)" # not sure
    ,buttonTextColorHover = "rgb(0,0,0)" # not sure
    ,buttonBorderColorHover = "rgb(150,150,150)" # not sure
    
    ,textboxBackColor = "rgb(255,255,255)" # not sure
    ,textboxBorderColor = "rgb(118,118,118)" # not sure
    ,textboxBorderRadius = 5 # not sure
    ,textboxBackColorSelect = "rgb(245,245,245)" # not sure
    ,textboxBorderColorSelect = "rgb(108,108,108)" # not sure
    
    ### tables                             # \
    ,tableBackColor = "rgb(248,248,248)"   #  \
    ,tableBorderColor = "rgb(238,238,238)" #   |- settings for if you display tables
    ,tableBorderTopSize = 1                #  /
    ,tableBorderRowSize = 1                # /
    
)

cp_ready <- read_csv("cp_dashboard_data.csv") %>%
    mutate(updated = mdy(updated)) %>%
    filter(wday(updated) %in% 2:6, # only keep weekdays (it's only updated on weekdays)
           updated != "2020-10-13")

dates <- cp_ready %>%
    pull(updated)

qip_begin_index <- min(which(!is.na(cp_ready$total_on_quar_current_quar_in_place)))



# Cal Poly COVID-19 Visual Dashboard
dashboardPage(
    dashboardHeader(title = span("Cal Poly COVID-19 Visual Dashboard", style = "color: white; font-family: Utopia; font-size: 22px"),
                    titleWidth = 375),
    dashboardSidebar(
        collapsed = FALSE,
        width = 375, # 417 is width + button but shadow overlaps
        sidebarMenu(
            menuItem(span("Daily New Cases", style = "font-family: Utopia; font-size: 18px"), tabName = "daily_stats", icon = icon("chart-line")),
            menuItem(span("Testing", style = "font-family: Utopia; font-size: 18px"), tabName = "testing", icon = icon("plus-square")),
            menuItem(span("Isolation & Quarantine", style = "font-family: Utopia; font-size: 18px"), tabName = "iso_quar", icon = icon("exclamation-triangle")),
            menuItem(span("Quarantine in Place", style = "font-family: Utopia; font-size: 18px"), tabName = "quar_in_place", icon = icon('door-closed'))
        )
    ),
    dashboardBody(
        tags$head(tags$style(HTML('
            .modal.in .modal-dialog{
                width:100%;
                height:100%;
                margin:0px;
            }
            
            .modal-content{
                width:100%;
                height:100%;
            }
         '))),
        
        customTheme,
        
        tabItems(
            tabItem(tabName = "daily_stats",
                fluidRow(
                    box(title = span("Cal Poly Daily Positive COVID-19 Cases", style = "font-family: Utopia; font-size: 20px"),
                        solidHeader = TRUE,
                        plotlyOutput("cp_daily_plot"),
                        width = 8),
            
                    box(width = 4,
                        title = span("Controls", style = "font-family: Utopia; font-size: 20px"),
                        solidHeader = TRUE,
                        dateRangeInput("date_slider", label = "Dates to show:", min = first(dates),
                                       start = first(dates), end = last(dates), max = last(dates),
                                       format = 'mm-dd-yyyy'), 
                        footer = p(strong('Note:'), br(), "Data updated Monday through Friday. Numbers reflect data from previous weekday.")
                    )
                )
            ),
            tabItem(tabName = 'testing',
                    fluidRow(
                        box(title = span("Cal Poly Testing", style = "font-family: Utopia; font-size: 20px"),
                            plotlyOutput("testing_totals_plot"),
                            width = 8),
                        
                        box(width = 4,
                            title = span("Controls", style = "font-family: Utopia; font-size: 20px"),
                            dateRangeInput("date_slider2", label = "Dates to show:", min = first(dates),
                                           start = first(dates), end = last(dates), max = last(dates),
                                           format = 'mm-dd-yyyy'),
                            footer = p(strong('Note:'), br(), 'Cal Poly provides COVID-19 testing through its Campus Health and Wellbeing only to currently enrolled students. 
                                        These numbers reflect only positive cases identified directly through Campus Health and Wellbeing. 
                                        Students can also access testing off campus.')
                        )
                    )
            ),
            tabItem(tabName = 'iso_quar',
                    fluidRow(
                        box(title = span("Total On-Campus Resident Students in Isolation/Quarantine", style = "font-family: Utopia; font-size: 20px"),
                            plotlyOutput('iso_quar_totals_plot'),
                            width = 8),
                        
                        box(width = 4,
                            title = span("Controls", style = "font-family: Utopia; font-size: 20px"),
                            dateRangeInput("date_slider3", label = "Dates to show:", min = first(dates),
                                           start = first(dates), end = last(dates), max = last(dates),
                                           format = 'mm-dd-yyyy'),
                            footer = p(strong('Isolation:'), br(), 'Students who test positive for COVID-19 are placed in isolation in on-campus University Housing facilities and 
                                       provided with direct dining, laundry and other support services. Per the CDC, an individual who has tested positive
                                       can leave isolation if it has been 10 days since their symptoms first appeared, they haven’t had a fever in at least 
                                       24 hours without the use of fever-reducing medications, and their symptoms have improved.',
                                       br(),
                                       br(),
                                       strong('Quarantine:'),
                                       br(),
                                       'Students who have been exposed to someone with COVID-19 must self-quarantine for 14 days by county health order.')
                        )
                    )
                
            ),
            tabItem(tabName = 'quar_in_place',
                    fluidRow(
                        box(title = span("Total On-Campus Resident Students Currenly Quarantining in Place", style = "font-family: Utopia; font-size: 20px"),
                            plotlyOutput("quar_in_place_plot"),
                            width = 8),
                        
                        box(width = 4,
                            title = span("Controls", style = "font-family: Utopia; font-size: 20px"),
                            dateRangeInput("date_slider4", label = 'Dates to show', min = dates[qip_begin_index],
                                           start = dates[qip_begin_index], end = last(dates), max = last(dates),
                                           format = 'mm-dd-yyyy'),
                            footer = p(strong('Quarantine in Place:'), br(), 'In the event of possible exposures in University Housing facilities, the university can, as a precaution, place 
                                        floors or sections of buildings on “Quarantine in Place” for 14 days, in consultation with San Luis Obispo County
                                        Department of Public Health.')
                        )
                    )
                )
        )
    )
)