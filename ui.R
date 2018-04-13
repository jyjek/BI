library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(shinyBS)
library(dplyr)
library(lubridate)
library(shinyWidgets)
library(glue)
library(highcharter)
library(shinycustomloader)
library(shinycssloaders)
library(shinymaterial)
library(shinybulma)

options(shiny.sanitize.errors = FALSE)
dashboardPage( skin="blue",#style = 'overflow-x: scroll',
               
               dashboardHeader(
                 title="Rozetka"
                # tags$li(class = "dropdown", actionLink("sad",class = "my_class", "Log Out",icon("power-off")))
                 #dropdownMenuOutput("messageMenu")
                 ),
               
               dashboardSidebar(
                 uiOutput("sidebarUserPanel"),
                 sidebarMenu(
                   menuItem('Time Line', icon=icon("line-chart"), tabName = "tm" ),
                   menuItem('Categories',tabName = "ct")
                  
                  
                 )),
               dashboardBody(useShinyjs(),
                                   uiOutput("bw1"),
                          
                             tags$head(
                               tags$link(rel = "stylesheet", type = "text/css", href = "cust.css")
                             ),
                      tabItems(
                               tabItem("tm",
                                      uiOutput("sk"),
                                      uiOutput("ab"),br(),
                                      
                                      conditionalPanel(
                                        condition="($('html').hasClass('shiny-busy'))",
                                        p("It's took like 15 sec..."),
                                        img(src="ajax-loader-bar.gif")
                                        
                                      ),
                                     #  verbatimTextOutput("value1"),
                                    #  withLoader( DT::dataTableOutput("row") , type="html", loader="loader4")
                                    fluidRow(
                                      valueBoxOutput("view7",width = 3),
                                      valueBoxOutput("view28",width = 3),
                                      valueBoxOutput("sales7",width = 3),
                                      valueBoxOutput("sales28",width = 3)
                                    ),
                                    
                                    fluidRow(
                                      column(6, 
                                             materialSwitch(inputId = "wk", label = "Daily/Weekly",
                                                            status = "danger", right = TRUE),
                                             highchartOutput("plot1")),
                                      column(6, DT::dataTableOutput("row"))
                                    )),
                               tabItem("ct",
                                       uiOutput("cat"))
                         
               )            
               ))
