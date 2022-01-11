
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
library(RColorBrewer)
library(tidyverse)
library(rjson)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(sf)
library(shiny)





# set mapping colour for each outbreak
covid_col = "#cc4c02"
covid_other_col = "#662506"
sars_col = "#045a8d"
h1n1_col = "#4d004b"
ebola_col = "#016c59"

ui = bootstrapPage(
  # tags$head(includeHTML("gtag.html")),
  navbarPage(
    

    
    
    
    theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">COVID-19 Health Equity & Justice Dashboard</a>'), id="nav",
             windowTitle = "COVID-19 tracker",
             
             tabPanel("County Level Analysis",
                      div(class="outer",
                          # tags$head(includeCSS("styles.css")),
                          tabPanel("Map Tab",
                                   tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                                   leafletOutput("map", width = "100%", height = "100%"), 
                                   
                                   absolutePanel(id = "controls", class = "panel panel-default",
                                                 top = 100, left = 40, width = 300, fixed=TRUE,
                                                 draggable = TRUE, height = "auto",
                                                 
                                                 span(tags$i(h6("Reported Numbers of COVID-19 Vaccination for Senior Population per State.")), 
                                                      style="color:#045a8d"),
                                                 h3(textOutput("reactive_case_count"), align = "left"),
                                                 h4(textOutput("reactive_death_count"), align = "left"),
                                                 h6(textOutput("clean_date_reactive"), align = "left"),
                                                 h6(textOutput("reactive_country_count"), align = "left"),
                                                 plotOutput("epi_curve", height="150px", width="100%"),
                                                 plotOutput("cumulative_plot", height="150px", width="100%")

                                                 
                                   )
                                   
                          )
                          
                          
                          
                      )
             ), 
             tabPanel("About this site",
                      tags$div(
                        tags$h4("Last update"),
                        
                        "This site has been developed for",
                        tags$a(href="https://mitpolicyhackathon.org", "MIT Policy Hackathon,"),
                        tags$a(href="https://prb.org", "PRB,"),
                        "Our aim is to provide a tool that help in providing equity to vulnerable people in the time of pandemic."

                        
                      )
             )
             
 
))


























