# Name: Lê Hiền Hiếu
# Roll number: HE181040

# Load packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(DT)

#-------------------------------------------------------------------------------

#UI
ui <- dashboardPage(
  dashboardHeader(title = "My Weather Forecast"),
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu()
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
                        .skin-blue .main-header .navbar {background-color: #696aad;}
                        .skin-blue .main-header .logo {background-color: #696aad;}
                        .content-wrapper
                        .small-value-box .small-box-footer {max-height: 30px;}
                        .right-side {background-color: #FFFFFF;}
                        #tempBox {display: inline;}
                        #img {display: inline;}"))),
    fluidRow(
        # Add search bar
      tags$div(
        class = "input-group", 
        style = "margin-left: 40%",
        tags$span(textInput("searchText", NULL, placeholder = "Search city")),
        tags$span(actionButton("searchButton", icon("search"), width="100%")),
        textOutput("errorMessage"),
      )
    ),
    
    br(),
    
    fluidRow(
      column(6,
             tags$span(class="current-time",
                       uiOutput("dateText"),
                       style="font-size:20px; color:#696aad;"),
             tags$span(class = "city-name",
                       uiOutput("location"),
                       style = "font-size:40px; font-weight:bold; display: block;"),
             tags$span(class = "temp",
                       uiOutput("img"), 
                       uiOutput("tempBox"),
                       style = "font-size:70px; margin-left: 120px; display: block;"),
             valueBoxOutput("weatherMainBox",width=10),
             fluidRow(
               valueBoxOutput("feelTempBox",width = 5),
               valueBoxOutput("pressureBox",width = 5),
             ),
             fluidRow(
               valueBoxOutput("humidityBox",width = 5),
               valueBoxOutput("windBox",width = 5)
             ),
      ),
      column(6,
             tags$div(style='',
                 box(
                   leafletOutput("map", height = 480), width = 20, height = 500))
             )
      
    ),
    br(),
    br(),
    fluidRow(
      column(7,fluidRow(
        box(selectInput("features", 
                        "Feature", c('pressure','humidity','sea_level','rain')), width = 3),
        box(plotOutput("pt1", width = "700px", height = "400px"), width = 12))),
      column(5,fluidRow(dataTableOutput("pt2")))
    )
  )
)

#-------------------------------------------------------------------------------
# Server
api_key <- "4831c023a83caa684fc881e0e3798fbb"
lat_default = 21.028511
lon_default = 105.804817

server <- function(input, output, session){
#---------------------Current weather-------------------------------------------
  # Setup time to reset data after 5 minutes
  autoInvalidate <- reactiveTimer(300000) # milliseconds
  
  
  # Get current weather data by coor
  current_coor_data <- function(lat, lon, api_key){
    current_coor_api <- "https://api.openweathermap.org/data/2.5/weather?lat=%s&lon=%s&appid=%s"
    api_call <- sprintf(current_coor_api, lat, lon, api_key)
    if(GET(api_call)$status == 200){
      json <- fromJSON(api_call)
      return(json)
    }
  }
  
  # Reset weather data after 5 mins
  reset_data <- function(lat, lon){
    reactivePoll(
      intervalMillis = 300000, 
      session, 
      checkFunc = function(){
        Sys.time()
      }, 
      valueFunc = function(){
        # your function
      }
    )}
  
  # Get current weather data by city searching
  observeEvent(input$searchButton, {
    current_city_api <- "https://api.openweathermap.org/data/2.5/weather?q=%s&appid=%s"
    api_call <- sprintf(current_city_api,input$searchText, api_key)
    json <- tryCatch(
      {
        fromJSON(api_call)
        },
      error = function(e) {
        output$errorMessage <- renderText({paste0("Wrong city name!")})
        return(NULL)
      })
    if (!is.null(json)) {
    renderMap(output, json)
    renderGUI(input, output, json)
    renderPlot2(input, output, json$coord$lat, json$coord$lon)
    }
    })
  
  # Get current weather data by map clicking
  observeEvent(input$map_click, {
    click_info <- input$map_click
    json <- current_coor_data(click_info$lat, click_info$lng, api_key)
    renderMap(output, json)
    renderGUI(input, output, json)
    renderPlot2(input, output, click_info$lat, click_info$lng)
    })

  
  # Render GUI
  renderGUI <- function(input, output, json){
    # Get time
    output$dateText <- renderText({
      autoInvalidate()
      paste(format(Sys.time(),"%b %d, %Y, %I.%M%p"))
    })
    
    # Get location
    output$location <- renderText({
      paste0(json$name)
    })
  
    # Get temp
    output$tempBox <- renderText({
      paste0(format(json$main$temp - 273.2, digits = 2), " °C")
    })
    
    # Get feel-like temp
    output$feelTempBox <- renderValueBox({
      valueBox(
        paste0(format(json$main$feels_like - 273.2, digits = 2), " °C"), "feels like",
        color = "purple", icon = icon("thermometer-half")
      )
    })
    
    # Get pressure
    output$pressureBox <- renderValueBox({
      valueBox(
        paste0(json$main$pressure, " hpa"), "air pressure",
        color = "purple", icon = icon("area-chart")
      )
    })
    
    # Get humidity
    output$humidityBox <- renderValueBox({
      valueBox(
        paste0(json$main$humidity, " %"), "humidity",
        color = "purple", icon = icon("tint")
      )
    })
    
    # Get weather condition
    output$weatherMainBox <- renderValueBox({
      weather_tibble <- as_tibble(json$weather)
      valueBox(
        paste0(weather_tibble$description), "weather condition",
        color = "purple", icon = icon("cloud")
      )
    })
    
    # Get Wind
    output$windBox <- renderValueBox({
      valueBox(
        paste0(json$wind$speed, " km/h"), "wind speed",
        color = "purple", icon = icon("tachometer-alt")
      )
    })
    
    # Get icon weather
    output$img <- renderUI({
      icon_link <- paste('https://openweathermap.org/img/wn/',json$weather$icon,'@2x.png', sep='')
      tags$img(src = icon_link)})

  }
  
  # Render Map
  renderMap <- function(output, json){
    output$map <- renderLeaflet({
      leaflet() %>%
        addMarkers(lng=json$coord$lon, lat=json$coord$lat) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(json$coord$lon, json$coord$lat, zoom = 11) %>% 
        addProviderTiles(providers$OpenWeatherMap.Classic,  
                         options=providerTileOptions(apiKey=api_key))              
    })
  }
  
  
#---------------------Forecast--------------------------------------------------
  # Get weather forecast
  renderPlot2 <- function(input, output, lat, lon){
    data_forecast <- function(lat, lon, api_key){
      weather_forecast_api <- "https://api.openweathermap.org/data/2.5/forecast?lat=%s&lon=%s&appid=%s"
      api_call <- sprintf(weather_forecast_api, lat, lon, api_key)
      if(GET(api_call)$status == 200){
        json <- fromJSON(api_call)
        return(json)
      }
    }
    
    output$pt1 <- renderPlot({ 
      weather_forecast_data <- data_forecast(lat, lon, api_key)
      weather_forecast <- as_tibble(weather_forecast_data$list)
      weather_forecast$new_date <- ymd_hms(weather_forecast$dt_txt)
      
      feature <- input$features
      
      if (feature == 'rain'){
        ggplot(weather_forecast, aes(x = new_date)) +
          geom_col(aes(y = rain[["3h"]]), fill = "#696aad") +
          labs(x = "", y = "Rain (mm)") +
          scale_x_datetime(date_breaks = "12 hour", date_labels = "%a-%d\n%H:%M") + 
          theme_classic()
      } else {
        ggplot(weather_forecast, aes(x = new_date)) +
          geom_line(aes_string(y = paste0("main$", feature)), linewidth = 1.5, color="#696aad") +
          labs(x = "", y = "") +
          scale_x_datetime(date_breaks = "12 hour", date_labels = "%a-%d\n%H:%M") + 
          theme_classic()}
      })
    
    output$pt2 <- renderDataTable({
      weather_forecast_data <- data_forecast(lat, lon, api_key)
      weather_forecast <- as_tibble(weather_forecast_data$list)
      
      # Create new_date for "dt" time data
      weather_forecast <- weather_forecast %>%
        mutate(new_date = format(as.POSIXct(as.numeric(dt)), "%d %a %b %Y"))
      
      # Create new dataframe
      table_plot <- weather_forecast %>%
        group_by(new_date) %>%
        summarize(
          Temperature = first(format(main$temp - 273.2, digits=4)),
          Condition = first(weather[[1]]$description)) %>%
        rename(Date = new_date)
          
      
      datatable(table_plot, 
                options = list(searching = FALSE,
                               paging = FALSE), 
                rownames=FALSE)
    })
  }
  
  #Show default location
  json_default <- current_coor_data(lat_default, lon_default, api_key)
  renderMap(output, json_default)
  renderGUI(input, output, json_default)
  renderPlot2(input, output, lat_default, lon_default)
}

#-------------------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
