
rm(list = ls())

#*******************************************************************************
# Housekeeping =================================================================
#*******************************************************************************

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
library(readr)
library(readxl)
library(dplyr)
library(data.table)
library(haven)
library(rsconnect)

#*******************************************************************************
# Data Importing =================================================================
#*******************************************************************************

Vaccine_Hesitancy <- read_csv("Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates (1).csv")
#Vaccination <- read_excel("Vaccination.xlsx")
# Hospital_Capacity_95_interval <- fread("Hospital_Capacity_95%interval.csv")
covid_Chronicdiseases_bystate_2018 <- read_excel("covid_Chronicdiseases_bystate_2018.xlsx")
# Case_byRace <- read_excel("Case_byRace.xlsx")
# vaccine_hospital_info <- read_dta("vaccine_hospital_info.dta")
# state_polygons <- read_excel("state_polygons.xlsx")
# States <- st_read(dsn = 'cb_2018_us_state_500k/cb_2018_us_state_500k.shp')
USA <- st_read(dsn = 'cb_2018_us_county_500k/cb_2018_us_county_500k.shp')
senior_percent <- read_csv("prb_senior_percent1.csv")
connector <- read.delim("2020_Gaz_counties_national.txt")
#state_lonlat <- read_excel("state_lonlat.xlsx")

#*******************************************************************************
# Data Wrangling ===============================================================
#*******************************************************************************



Vaccine_Hesitancy$`FIPS Code` <- as.character(Vaccine_Hesitancy$`FIPS Code`)

for (i in 1:nrow(Vaccine_Hesitancy)){
  if (nchar(Vaccine_Hesitancy[i, 1]) == 4){
    
    Vaccine_Hesitancy[i, 1] <- paste0("0", Vaccine_Hesitancy[i, 1])
  }
}

colnames(Vaccine_Hesitancy)[1] <- "FIPS"
#colnames(Vaccination)[1] <- "state"
# Hospital_Capacity_95_interval <- Hospital_Capacity_95_interval[-c(1), ]
# hospital_capacity <- Hospital_Capacity_95_interval 
# dim(hospital_capacity)
# dim(Hospital_Capacity_95_interval)
# hospital_capacity$state <- NULL
# hospital_capacity$collectionDate <- NULL
# hospital_capacity$Notes <- NULL
# 
# hospital_capacity    <-   sapply(hospital_capacity[, 2:ncol(hospital_capacity)], as.numeric)
# Hospital_Capacity_95_interval <- as.data.frame(Hospital_Capacity_95_interval)
# hospital_capacity <- as.data.frame(hospital_capacity)
# hospital_capacity$state <- Hospital_Capacity_95_interval$statename

# hospital_capacity <- hospital_capacity %>%
#   group_by(hospital_capacity$state) %>%
#   summarise(across(everything(), list(mean)))
# 
# colnames(hospital_capacity)[1] <- "state"
# Case_byRace <- Case_byRace[-c(52:70), ]
# 
# colnames(Case_byRace)[1] <- "state"

###  Maxim Data ================================================================

state_names <- c('United States', 'Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming');

young_population <- c(275119242.705, 4070106.9639999997, 639763.25, 6085548.82, 2503211.1720000003, 33541602.456, 4959792.026, 2927415.9379999996, 795368.054, 624426.816, 17191049.792, 9178484.569, 1139674.86, 1529126.1809999999, 10560937.67, 5667405.567, 2609937.8249999997, 2438854.7849999997, 3725072.832, 3906712.438, 1063911.108, 5092929.482, 5721666.42, 8202474.765, 4735195.254, 2480233.0960000004, 5087330.1959999995, 872025.639, 1623668.576, 2632999.301, 1110781.575, 7407897.414, 1727181.58, 16068860.855999999, 8830485.559, 645155.487, 9646904.025, 3339876.937, 3469552.7260000003, 10392785.501999998, 870013.875, 4268356.720000001, 739169.6760000001, 5736732.722, 25573221.089, 2879392.794, 498677.60000000003, 7224663.483, 6470327.692, 1418905.665, 4811940.375, 482749.91199999995);

senior_population <- c(54364880.295, 851425.0360000001, 91394.75, 1335852.18, 527310.828, 5826475.544000001, 847926.9739999999, 629590.0619999999, 191440.94599999997, 88389.184, 4542262.208, 1531532.431, 267331.14, 297786.819, 2026592.33, 1087547.433, 553623.1749999999, 474950.215, 752178.1680000001, 738605.562, 286229.892, 962872.518, 1171907.58, 1764080.2349999999, 922146.746, 486552.9039999999, 1064217.804, 208551.361, 313883.424, 505259.699, 255493.425, 1474473.5860000001, 379137.42, 3267915.144, 1770337.4409999999, 120153.513, 2046312.9749999999, 640906.063, 771954.274, 2390468.498, 187111.125, 949683.28, 153547.324, 1150101.278, 3787537.9110000003, 370486.206, 124669.40000000001, 1365899.517, 1223284.308, 365881.33499999996, 1020714.6249999999, 99578.088)
ratio_young <- c(0.0007860987762019218, 0.0012837009066870315, 0.0002466537426149439, 0.0010406128004737624, 0.0010246039282218416, 0.0007790862119446976, 0.0004125374590857895, 0.0005433119289111421, 0.0005094496792550332, 0.0009847431023846357, 0.0010772698714780152, 0.0009606814647573876, 0.00027639462012876194, 0.0004987162011059702, 0.0006150116782196671, 0.0006428867595457195, 0.00047706117290361133, 0.0006052020846333417, 0.0007540792158127662, 0.0011260618921448244, 0.00020763012843738441, 0.0006150880374589095, 0.0004193533533540042, 0.0006420379398753322, 0.000317769367319948, 0.0014795786758584563, 0.0007415480919571905, 0.0006015878163875868, 0.00045397195640497514, 0.0010568935582106328, 0.00014944432257079886, 0.000956465728940776, 0.0010507870284258126, 0.00037542175852169817, 0.0006248127538554983, 0.0005301047683719072, 0.0005896710473389414, 0.0010050370308000362, 0.00034217667052683954, 0.0005959326302662684, 0.00046275124060521453, 0.000932162014799925, 0.0005840336989148888, 0.0010680992643254611, 0.0011302447939353516, 0.0003708768050768415, 9.104078466728805e-05, 0.00047077625248611183, 0.00033347615495082407, 0.0006593109204338824, 0.00038822176802221913, 0.0004567582396576387);

ratio_senior <- c(0.010116659818169107, 0.01238394403990723, 0.004343794364555951, 0.010041530193857229, 0.011755874658428218, 0.008992743486919199, 0.007710569660448142, 0.01177273983082662, 0.008274092001196026, 0.011562500678816088, 0.009118143802234678, 0.011123499349521773, 0.002091039599801205, 0.008684064689914969, 0.009665979541134451, 0.01197740862122011, 0.01060107355513071, 0.010268444662142115, 0.010642425346224619, 0.01232457548160191, 0.003511163676783276, 0.009112317400256302, 0.010676609839830544, 0.009015462950300557, 0.007945590039527179, 0.01475481893331789, 0.010854920822204173, 0.00837203838722491, 0.008541387645879637, 0.01064007283905697, 0.005252581353120927, 0.013701839213551024, 0.008574727337649763, 0.006882063642714953, 0.008372980007487736, 0.01311655365415741, 0.010834119839366215, 0.012878642404105328, 0.004254137985380207, 0.011011648980952185, 0.013147267432655327, 0.010181289071447061, 0.01198978889400899, 0.011549417650503647, 0.012873798532389132, 0.006297130533383475, 0.0021176006301466114, 0.0076923667291991585, 0.0049162733149357134, 0.008409830471401336, 0.007845483746252779, 0.0076824130224312);

maxim_data <- data.frame(state_names, young_population, senior_population, ratio_young, ratio_senior)


#*******************************************************************************
# Data Analysis =================================================================
#*******************************************************************************

#hypothesis testing:

for(i in 1:length(state_names)){
  m <- senior_population[[i]]
  n <- young_population[[i]]
  p_hat <- ratio_senior[[i]]
  q_hat <- ratio_young[[i]]
  Tn <- (p_hat - q_hat) / sqrt((p_hat * (1-p_hat) / m) + (q_hat * (1-q_hat) / n))
  p_value = 1 - pnorm(Tn)
  maxim_data$Stats[i] <- Tn
  
}

maxim_data <- maxim_data[-c(1), ]
colnames(maxim_data)
colnames(maxim_data)[1] <- "state"
#vaccine_hospital_info <- vaccine_hospital_info[1:51, ]
# state_polygons <- state_polygons[-c(1), ]
# state_polygons$state <- NULL
# colnames(state_polygons)[5] <- "state"
# state_data <- merge(vaccine_hospital_info, maxim_data, by = "state")
# state_data <- left_join(state_data, hospital_capacity, by = "state")
# state_data <- left_join(state_data, Vaccination, by = "state")
# state_data <- left_join(state_data, Case_byRace, by = "state")
# state_data <- left_join(state_data, state_lonlat, by = "state")
# state_data <- left_join(state_data, state_polygons, by = "state")

#colnames(state_data)[1] <- "NAME"

# states_sf_real <- st_as_sf(States)
# states_sf_states <- left_join(states_sf_real, state_data, by = "NAME")

for (i in 1:nrow(connector)){
  if (nchar(connector[i, 2]) == 4){
    
    connector[i, 2] <- paste0("0", connector[i, 2])
  }
}



states_sf <- st_as_sf(USA)
states_sf$FIPS <- paste0(states_sf$STATEFP, states_sf$COUNTYFP)


for (i in 1:nrow(senior_percent)){
  if (nchar(senior_percent[i, 1]) == 4){
    senior_percent[i, 1] <- paste0("0", senior_percent[i, 1])
  }
}

states_sf_coef <- left_join(states_sf, senior_percent, by = "FIPS")
states_sf_coef <- subset(states_sf_coef, states_sf_coef$TimeFrame == "2019")
states_sf_coef <- left_join(states_sf_coef, connector, by = "GEOID")
states_sf_coef <- left_join(states_sf_coef, Vaccine_Hesitancy, by = "FIPS")
states_sf_coef <- left_join(states_sf_coef, covid_Chronicdiseases_bystate_2018, by = "FIPS")


#*******************************************************************************
# UI ===========================================================================
#*******************************************************************************

# set mapping color for each outbreak
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


#*******************************************************************************
# Server =======================================================================
#*******************************************************************************


server <-  function(input, output, session) {
  bins <- c(0.05, .1, .15, .2, .25, 0.3, 0.35, 0.4, 0.45, 0.50)
  mypal <- colorBin("Blues", domain = states_sf_coef$Data, bins = bins)
 # mypal_state <- colorBin("Greens", domain = states_sf_states$Stats, bins = bins)
  
  output$map <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$Stamen.Watercolor)%>%
      setView(lat = 39.8283, lng = -98.5795, zoom = 5) %>%
      addTiles() %>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Senior Population %", "Fully Vaccinated Adults %",  "Social Vulnerability Index (SVI)",  "Estimated hesitant",  "CVAC Level of Concern",
                          "Obesity Prevalence", "Heart Disease Prevalence", "Adult Population %","Black Population %", "Hispanic Population %","Asian Population %",  "White Population %"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      hideGroup(c("Minority Population %", "Fully Vaccinated Adults %", "Social Vulnerability Index (SVI)", "Estimated hesitant", "Hispanic Population %", 
                  "Black Population %", "Obesity Prevalence", "Heart Disease Prevalence", "Adult Population %", "CVAC Level of Concern", "Asian Population %",  "White Population %"))  %>%
      addPolygons(
        data = states_sf_coef,
        fillColor = ~mypal(states_sf_coef$Data),
        stroke = FALSE,
        smoothFactor = 0.2,
        fillOpacity = 0.4,
        group = "Senior Population %", 
        popup = paste("Region: ", states_sf_coef$FIPS, "<br>",
                      "Avg_yield: ", states_sf_coef$Data, "<br>")) %>%
      
      addLegend(position = "bottomleft",
                pal = mypal,
                values = states_sf_coef$Data,
                title = "% Senior Population",
                opacity = 1) %>%
      
      addCircleMarkers(data = states_sf_coef, lat = states_sf_coef$INTPTLAT, lng = states_sf_coef$INTPTLONG, weight = 1, radius = as.numeric(states_sf_coef$`Social Vulnerability Index (SVI)`) * 10
                       ,
                       fillOpacity = 0.2, color = "#D4AC0D", group = "Social Vulnerability Index (SVI)",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#045a8d"),
                         textsize = "15px", direction = "auto"))  %>%
      addCircleMarkers(data = states_sf_coef, lat = states_sf_coef$INTPTLAT, lng = states_sf_coef$INTPTLONG, weight = 1, radius = as.numeric(states_sf_coef$`Estimated hesitant`) * 40
                       ,
                       fillOpacity = 0.2, color = "#424949", group = "Estimated hesitant",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#045a8d"),
                         textsize = "15px", direction = "auto"))  %>%

      addCircleMarkers(data = states_sf_coef, lat = states_sf_coef$INTPTLAT, lng = states_sf_coef$INTPTLONG, weight = 1, radius = as.numeric(states_sf_coef$`Percent Hispanic`) * 40 ,
                       fillOpacity = 0.2, color = "#D35400 ", group = "Hispanic Population %",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#045a8d"),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = states_sf_coef, lat = states_sf_coef$INTPTLAT, lng = states_sf_coef$INTPTLONG, weight = 1, radius = as.numeric(states_sf_coef$`Percent non-Hispanic Black`) * 40
                       
                       ,
                       fillOpacity = 0.2, color = "#2C3E50", group = "Black Population %",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#045a8d"),
                         textsize = "15px", direction = "auto")) %>%
      
      
      addCircleMarkers(data = states_sf_coef, lat = states_sf_coef$INTPTLAT, lng = states_sf_coef$INTPTLONG, weight = 1, radius = as.numeric(states_sf_coef$Obesity_prevalence) /4 ,
                       fillOpacity = 0.2, color = "#E74C3C" , group = "Obesity Prevalence",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#045a8d"),
                         textsize = "15px", direction = "auto")) %>%
      
      
      addCircleMarkers(data = states_sf_coef, lat = states_sf_coef$INTPTLAT, lng = states_sf_coef$INTPTLONG, weight = 1, radius = as.numeric(states_sf_coef$`Heart disease_prevalence`)  ,
                       fillOpacity = 0.2, color = "#7B241C" , group = "Heart Disease Prevalence",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#045a8d"),
                         textsize = "15px", direction = "auto")) %>%
      
      
      addCircleMarkers(data = states_sf_coef, lat = states_sf_coef$INTPTLAT, lng = states_sf_coef$INTPTLONG, weight = 1, radius = as.numeric(states_sf_coef$`Percent adults fully vaccinated against COVID-19 (as of 6/10/21)`) *10 ,
                       fillOpacity = 0.2, color = "#196F3D" , group = "Fully Vaccinated Adults %",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#045a8d"),
                         textsize = "15px", direction = "auto")) %>%
      
      
      addCircleMarkers(data = states_sf_coef, lat = states_sf_coef$INTPTLAT, lng = states_sf_coef$INTPTLONG, weight = 1, radius = as.numeric(states_sf_coef$`county_pop2018_18 and older`) / 1000000,
                       fillOpacity = 0.2, color = "#424949" , group = "Adult Population %",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#045a8d"),
                         textsize = "15px", direction = "auto"))  %>%
      
      
      addCircleMarkers(data = states_sf_coef, lat = states_sf_coef$INTPTLAT, lng = states_sf_coef$INTPTLONG, weight = 1, radius = as.numeric(states_sf_coef$`CVAC Level Of Concern`) * 100000 ,
                       fillOpacity = 0.2, color = "#873600" , group = "CVAC Level of Concern",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#045a8d"),
                         textsize = "15px", direction = "auto"))  %>%
      
      
      addCircleMarkers(data = states_sf_coef, lat = states_sf_coef$INTPTLAT, lng = states_sf_coef$INTPTLONG, weight = 1, radius = as.numeric(states_sf_coef$`Percent non-Hispanic Asian`) * 100  ,
                       fillOpacity = 0.2, color = "#0B5345" , group = "Asian Population %",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#045a8d"),
                         textsize = "15px", direction = "auto")) %>%
      
      
      addCircleMarkers(data = states_sf_coef, lat = states_sf_coef$INTPTLAT, lng = states_sf_coef$INTPTLONG, weight = 1, radius = as.numeric(states_sf_coef$`Percent non-Hispanic White`) * 5 ,
                       fillOpacity = 0.2, color = "#186A3B" , group = "White Population %",
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#045a8d"),
                         textsize = "15px", direction = "auto")) 
    
    
  })
  
### Overlay ====================================================================
  
  #### Text in the Overlay =====================================================
  output$reactive_case_count <- renderText({
    "MIT Policy Hackathon 2021"
  })
  
  output$reactive_death_count <- renderText({
    "Morteza Malaki, Mohsin Khan, Maxim Sinelnikov, Xingyan Lin, Thodoris Bias"
  })
  
  output$clean_date_reactive <- renderText({
    " "
  })
  
  output$reactive_country_count <- renderText({
    "October 24, 2021"
  })
  
  
#### Overlay Curves ============================================================
  # output$epi_curve <- renderPlot({
  #   ggplot(data = state_data) + geom_point( mapping = aes(y = state_data$InBedsOccCOVID__Numbeds_Est_1 , x = state_data$totalnumberdosefor65adminste, 
  #                                                         color = state_data$NAME)) +
  #     theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
  #           plot.margin = margin(5, 12, 5, 5)) + labs (x = "Total Number of Doses Administered to Seniors", y = "Hospitalization for COVID")
  # })
  # 
  # 
  # 
  # output$cumulative_plot <- renderPlot({
  #   ggplot(data = state_data) + geom_point( mapping = aes(y = state_data$inpatbeds_occ_covid_est ,  x = state_data$`People 65+ who have received a booster dose`, 
  #                                                         color = state_data$NAME)) + 
  #     theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
  #           plot.margin = margin(5, 12, 5, 5)) + labs (x = "Total Number of Booster Shot Administered to Seniors", y = "Hospitalization for COVID")
  # })
}


#*******************************************************************************
# Running App ==================================================================
#*******************************************************************************
runApp(shinyApp(ui, server), launch.browser = TRUE)


#*******************************************************************************
# Deploying App ================================================================
#*******************************************************************************

# rsconnect::setAccountInfo(name='mortezamaleki1441',
#                           token='044C11F0F0F7AF3E61F0421E2AB559B2',
#                           secret='4McNcv5qRZesKRPqolB7BiXVC4yMUINKmjgWikF8')
# 
# options( rsconnect.max.bundle.size = 30000000000)
# rsconnect::deployApp()





















