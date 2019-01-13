#Create an app that shows the mean departure times for that origin and destination selected 
library(shiny)
library(dplyr)
library(mosaic)
library(base)
library(plotly)
library(ggplot2)
library(nycflights13)
library(igraph)
require(visNetwork)
library(lubridate)
#setwd("~/git/SparklingWaterH2O/ShinyApp")

data(flights)
data(weather)

dat <- load("airportFullDat.Rda")
datcar <- load("carrierData.Rda")
dat2 <- load("FinalDataYear.Rda")
nrow(FinalDat)
airportsVis <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header = TRUE)
data2 <- stateFin2

DatNew <- FinalDat

#DatNew <- DatNew[sample(1:nrow(DatNew),1000, replace=FALSE),]
DatNew$time_hour <- as.POSIXct(DatNew$time_hour)
DatNew$hour <- hour(DatNew$time_hour)
#DatNew$hour <- hour(as.POSIXct(DatNew$time_hour))
DatNew$week <- weekdays(as.Date(DatNew$time_hour))
DatNew$hour <- as.numeric(DatNew$hour)
DatNew$hour <- as.factor(DatNew$hour)
DatNew$weekend<- ifelse(DatNew$week %in% c("Saturday", "Sunday"), "weekend","weekday")
data3 <- DatNew
data3$carrier <- as.character(data3$carrier)
carrierDat$codeCar <- as.character(carrierDat$codeCar)

data3 <- carrierDat %>% inner_join(data3, by = c("codeCar" = "carrier"))
data3 <- plyr::rename(data3, c("code" = "carrier"))

#data3$year <- as.factor(data3$year)
data3$weekend <- as.factor(data3$weekend)
data2$OriginAirport <- as.factor(data2$OriginAirport)
data2$DestAirport <- as.factor(data2$DestAirport)
data2$OriginState <- as.factor(data2$OriginState)
data2$DestState <- as.factor(data2$DestState)
data3$week <- as.factor(data3$week)
data3$carrier <- as.factor(data3$carrier)
data3$month <- as.factor(data3$month)
data3$month <- plyr::mapvalues(data3$month, from = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), 
                               to = c("January", "February", "March", "April", "May", 
                                      "June", "July", "August", "September", 
                                      "October", "November", "December"))
data3$season <- ifelse(data3$month %in% c("March", "April", "May"), "spring", 
                       ifelse(data3$month %in% c("June", "July", "August"), "summer", 
                              ifelse(data3$month %in% c("September", "October", "November"), "fall", "winter")))

data3$season <- as.factor(data3$season)
namesDat3 <- data3[,c(2,4,19,22,23,24)]
namesDat3$month <- as.factor(namesDat3$month)
namesDat3$carrier <- as.factor(namesDat3$carrier)
namesDat3$week <- as.factor(namesDat3$week)
namesDat3$weekend <- as.factor(namesDat3$weekend)
namesDat4 <- namesDat3[,c(2, 4, 5, 6)]


#Flights Dataset Analysis
flights$hour <- ifelse(flights$hour == 24, 0, flights$hour)
flights_weather <- left_join(flights, weather)
flights_weather$total <- flights_weather$dep_delay + flights_weather$arr_delay
flights_weather2 <- filter(flights_weather, total > 0)

DatNew <- flights_weather2
DatNew$hour <- hour(as.POSIXct(DatNew$time_hour))
DatNew$week <- weekdays(as.Date(DatNew$time_hour))
DatNew$hour <- as.numeric(DatNew$hour)
DatNew$hour <- as.factor(DatNew$hour)
DatNew$weekend<- ifelse(DatNew$week %in% c("Saturday", "Sunday"), "weekend","weekday")
DatNew$month <- as.factor(DatNew$month)
DatNew$month <- plyr::mapvalues(DatNew$month, from = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), 
                                to = c("January", "February", "March", "April", "May", 
                                       "June", "July", "August", "September", 
                                       "October", "November", "December"))
DatNew$season <- ifelse(DatNew$month %in% c("March", "April", "May"), "spring", 
                        ifelse(DatNew$month %in% c("June", "July", "August"), "summer", 
                               ifelse(DatNew$month %in% c("September", "October", "November"), "fall", "winter")))

flights_weather2 <- DatNew
flights_weather3 <- flights_weather2[,c(20, 21, 22, 23, 24, 25, 26, 27, 28)]
nrow(flights_weather3)

# Define UI for application that draws a histogram
ui <- navbarPage("Flights Analysis",
  tabPanel("Graphical",
    sidebarLayout(
      sidebarPanel(
        actionButton("go", "Graphical Flights Comparison"),
        
        selectInput("response", "Choose a response predictor:",
                    choices = names(namesDat3)),
        
        selectInput("yearInitial", "Choose year range (initial):",
                    choices = sort(unique(data3$year))),
        
        selectInput("yearEnd", "Choose year range (end):",
                    choices = sort(unique(data3$year)))
      ),
      mainPanel(
        plotlyOutput('plot', height = "900px")
      )
    )
  ),
  tabPanel("Table Summary",
           sidebarLayout(
             sidebarPanel(
               actionButton("go1", "Airport Flights Data"),
               
               actionButton("go2", "State Flights Data"),
               
               
               selectInput("origin", "Choose a origin airport:",
                           choices = sort(unique(data2$OriginAirport))),

               selectInput("destination", "Choose a destination airport:",
                           choices = sort(unique(data2$DestAirport))),

               selectInput("originState", "Choose a origin state:",
                           choices = sort(unique(data2$OriginFState))),

               selectInput("destState", "Choose a destination state:",
                           choices = sort(unique(data2$DestFState)))
             ),
             mainPanel(
               dataTableOutput("view"),
               dataTableOutput("view2")
             )
           )
  ),
  tabPanel("2013 Weather Flights",
           sidebarLayout(
             sidebarPanel(
               actionButton("go3", "Weather and Flight Delays"),
               
               selectInput("response2", "Choose a response predictor:",
                           choices = names(namesDat4)),
               
               selectInput("weatherDis", "Choose a weather phenomena:", 
                           choices = names(flights_weather3))
             ),
             mainPanel(
               plotlyOutput('plot2', height = "900px")
             )
           )
  ),
  tabPanel("VizNetwrk",
           sidebarLayout(
             sidebarPanel(
               actionButton("go5", "Sample 500 Flights")
             ),
             mainPanel(
               visNetworkOutput("network", height = "400px")
               #dataTableOutput("view3")
             ))
  )
)



server <- shinyServer(function(input, output) {
  
  df_subset <- eventReactive(input$go1, {
    a <- data2 %>% filter(OriginAirport == input$origin & DestAirport == input$destination)
    return(a)
  })
  
  df_subsetVis <- eventReactive(input$go5, {
    dat1 <- FinalDat[sample(nrow(FinalDat), 500, replace = FALSE, prob = NULL),]
    dat2 <- dat1[,c(1, 13, 14, 6)]
    dat3 <- suppressWarnings(inner_join(dat2, airportsVis, by = c("origin" = "iata")))
    dat4 <- suppressWarnings(inner_join(dat3, airportsVis, by = c("dest" = "iata")))
    dat5 <- dat4[,c(5, 11, 4)]
    dat5$airport.x <- sort(dat5$airport.x)
    dat5$airport.y <- sort(dat5$airport.y)
    return(dat5)
  })
  
  
  df_subset2 <- eventReactive(input$go2, {
    b <- data2 %>% filter(OriginFState == input$originState & DestFState == input$destState) %>% arrange(desc(meanDelay))
    return(b)
  })
  
  #Q. how to group by an input and an already existing variable? 
  df_weekend <- eventReactive(input$go, {
    DatCarrier <- data3 %>% filter(year >= input$yearInitial & year <= input$yearEnd) %>% 
      mutate(yearF = as.factor(year)) %>% 
      group_by_("yearF", input$response) %>% 
      summarise(meanDep = mean(dep_delay))
    return(DatCarrier)
  })
  
  #Q. how to group by an input and an already existing variable? 
  df_weather <- eventReactive(input$go3, {
    return(flights_weather2)
  })
  
  output$plot <- renderPlotly({
    dfweek <-  df_weekend()
    p = ggplot(dfweek, aes_string(x = "yearF", y = "meanDep", group = input$response, color = input$response)) + 
      geom_point() + geom_line() + ggtitle(paste("Mean departure delay overtime by", input$response))
    ggplotly(p)
  })  
  
  output$plot2 <- renderPlotly({
    dfweek2 <-  df_weather()
    p1 = ggplot(dfweek2, aes_string(x = input$weatherDis, y = "total", group = input$response2, color = input$response2)) + 
      geom_smooth()  + ggtitle(paste("Total Delay in" , input$weatherDis, "in 2013")) 
    ggplotly(p1)
  })
  
  output$network <- renderVisNetwork({
    dfweek2 <-  df_subsetVis()
    e2 <- dfweek2
    g2=graph.data.frame(e2)
    E(g2)$width <- dat1$dep_delay/7
    visIgraph(g2) %>% visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  })
  
  output$view <- renderDataTable(df_subset())
  
  output$view2 <- renderDataTable(df_subset2())
  
  #output$view3 <- renderDataTable(head(df_subsetVis()))
  
})

shinyApp(ui = ui, server = server)
