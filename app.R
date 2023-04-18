
# load libraries
library(shiny)
library(shinyWidgets)
library(data.table)
library(leaflet)
library(RColorBrewer)

# load met data
load("met.RData")

# weathers setup
weathers_var <- c("wind.sp", "temp", "dew.point", "atm.press", "rh")
weathers_label <- c("Wind speed", "Temperature", "Dew point", "Atmospheric pressure", "Relative humidity")
weathers_unit <- c(" km/h", "&#8451;", "&#8451;", " hPa", "%")


# ui
ui <- 
  shinyUI(
    bootstrapPage(
      tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
      leafletOutput("map", width = "100%", height = "100%"),
      # # add u of u health logo
      # absolutePanel(
      #   top = 10, right = 10, width = 100, height = 100,
      #   img(src = "hsclogo.png", width = 100, height = 100)
      # ),
      
      absolutePanel(
        fixed = F, draggable = T,
        top = "5%", left = "5%", right = "auto", bottom = "auto", width = "auto", height = "auto",
        
        # add multiple check box for weather conditions
        checkboxGroupButtons(
          inputId = "weathers", label = "Weather conditions", 
          choices = weathers_label, selected = weathers_label, 
          status = "#AC162C"
        ),
        
        # input the number and percentage of the representatives at country level
        fluidRow(
          # tags$head(
          #   tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
          # ),
          column(3, numericInput(
            inputId = "us_n", label = "Country-N",
            value = 1, min = 1, max = 10, step = 1,
            width = "80%"
          )),
          column(3, numericInput(
            inputId = "us_perc", label = "Country-%",
            value = 10, min = 2, max = 100, step = 2,
            width = "80%"
          )),
          # input the number and percentage of the representatives at state level
          column(3, numericInput(
            inputId = "state_n", label = "State-N",
            value = 1, min = 1, max = 10, step = 1,
            width = "80%"
          )),
          column(3, numericInput(
            inputId = "state_perc", label = "State-%",
            value = 10, min = 2, max = 100, step = 2,
            width = "80%"
          ))
        ),
        fluidRow(
          # slider of hours
          column(6, sliderInput(
            inputId = "hours", label = "Hour",
            min = 0, max = 23, value = c(0, 23), step = 1
          )),
          # setSliderColor("#AC162C", 1),
          column(6, sliderInput(
            inputId = "day", label = "August",
            min = 1, max = 31, value = 1, step = 1,
            animate = T
          ))
        )
      )
    )
    
  )

# server
server <- function(input, output) {
  station_data <- reactive({
    req(input$weathers, input$hours, input$day)
    
    w <- weathers_var[match(input$weathers, weathers_label)]
    
    met1 <- met[hour %between% input$hours & day == input$day, ]
    # number of records and median value of weathers at station level
    met_station <- met1[, c(.N, lapply(.SD, median, na.rm = T)), 
                        by = .(USAFID, STATE, `STATION NAME`, LON, LAT), 
                        .SDcols = w]
    
    # median value of weathers at state level
    met_state <- met1[, lapply(.SD, median, na.rm = T), by = STATE, .SDcols = w]
    # merge state-level median values
    met_station <- merge(met_station, met_state, 
                         by = "STATE", suffixes = c(".station", ".state"))
    
    # median value of weathers at country level
    met_us <- met1[, lapply(.SD, median, na.rm = T), .SDcols = w]
    # add suffix
    colnames(met_us) <- paste0(colnames(met_us), ".us")
    # merge country-level median values
    met_station <- cbind(met_station, met_us)
    
  })
  
  us_data <- reactive({
    req(station_data(), input$weathers, input$us_n, input$us_perc)
    
    w <- weathers_var[match(input$weathers, weathers_label)]
    
    res <- copy(station_data())
    res$weather_dist <- sqrt(rowSums(
      (as.matrix(res[, paste0(w, ".", "station"), with = F]) -
         as.matrix(res[, paste0(w, ".", "us"), with = F])) ^ 2,
    ))
    
    res <- res[order(weather_dist),
                   head(.SD, min(input$us_n, ceiling(.N * input$us_perc/100)))]
    
    # res$text <- paste0(
    #   "USAFID: ", res$USAFID, "<br>",
    #   "Name: ", res$`STATION NAME`, "<br>",
    #   "Number of records: ", res$N, "<br>",
    #   # sapply(w, \(x) {
    #   #   paste0(weathers_label[match(x, weathers_var)], ": ",
    #   #          unlist(res[, paste0(x, ".station"), with = F]) |> round(1),
    #   #          weathers_unit[match(x, weathers_var)], "<br>")
    #   # }) |> apply(1, paste, collapse = ""),
    #   "Distance score: ", round(res$weather_dist, 1)
    # )
    
    res <- res[!is.na(weather_dist), ]
  })
  
  state_data <- reactive({
    req(station_data(), input$weathers, input$state_n, input$state_perc)
    
    w <- weathers_var[match(input$weathers, weathers_label)]
    
    res <- copy(station_data())
    res$weather_dist <- sqrt(rowSums(
      (as.matrix(res[, paste0(w, ".", "station"), with = F]) -
         as.matrix(res[, paste0(w, ".", "state"), with = F])) ^ 2,
    ))
    
    res <- res[order(weather_dist),
                   head(.SD, min(input$state_n, ceiling(.N * input$state_perc/100))),
                   by = STATE]
    
    # res$text <- paste0(
    #   "USAFID: ", res$USAFID, "<br>",
    #   "Name: ", res$`STATION NAME`, "<br>",
    #   "Number of records: ", res$N, "<br>",
    #   # sapply(w, \(x) {
    #   #   paste0(weathers_label[match(x, weathers_var)], ": ",
    #   #          unlist(res[, paste0(x, ".station"), with = F]) |> round(1),
    #   #          weathers_unit[match(x, weathers_var)], "<br>")
    #   # }) |> apply(1, paste, collapse = ""),
    #   "Distance score: ", round(res$weather_dist, 1)
    # )
    
    res <- res[!is.na(weather_dist), ]
  })
  
  colorPalette <- reactive({
    colorBin(palette = "YlOrBr", domain = state_data()$weather_dist, bins = seq(0,10,2))
  })
  
  basemap <- reactive(
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      fitBounds(-123.4, 18.4, -66.1, 47.8) |>
      addLegend("bottomleft", pal = colorPalette(), values = state_data()$weather_dist, title = "Distance score")
  )
  
  output$map <- renderLeaflet({basemap()})
  
  observeEvent(
    input$weathers,
    leafletProxy("map") |>
      clearMarkers() |>
      addMarkers(
        data = us_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text
      ) |>
      addCircles(
        data = state_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text,
        weight = 1, radius = ~N*100,
        fillOpacity = 0.8, color = "#AC162C", fillColor = ~colorPalette()(weather_dist),
      )
  )
  observeEvent(
    input$us_n,
    leafletProxy("map") |>
      clearMarkers() |>
      addMarkers(
        data = us_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text
      ) |>
      addCircles(
        data = state_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text,
        weight = 1, radius = ~N*100,
        fillOpacity = 0.8, color = "#AC162C", fillColor = ~colorPalette()(weather_dist),
      )
  )
  observeEvent(
    input$us_perc,
    leafletProxy("map") |>
      clearMarkers() |>
      addMarkers(
        data = us_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text
      ) |>
      addCircles(
        data = state_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text,
        weight = 1, radius = ~N*100,
        fillOpacity = 0.8, color = "#AC162C", fillColor = ~colorPalette()(weather_dist),
      )
  )
  observeEvent(
    input$state_n,
    leafletProxy("map") |>
      clearMarkers() |>
      addMarkers(
        data = us_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text
      ) |>
      addCircles(
        data = state_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text,
        weight = 1, radius = ~N*100,
        fillOpacity = 0.8, color = "#AC162C", fillColor = ~colorPalette()(weather_dist),
      )
  )
  observeEvent(
    input$state_perc,
    leafletProxy("map") |>
      clearMarkers() |>
      addMarkers(
        data = us_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text
      ) |>
      addCircles(
        data = state_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text,
        weight = 1, radius = ~N*100,
        fillOpacity = 0.8, color = "#AC162C", fillColor = ~colorPalette()(weather_dist),
      )
  )
  observeEvent(
    input$state_perc,
    leafletProxy("map") |>
      clearMarkers() |>
      addMarkers(
        data = us_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text
      ) |>
      addCircles(
        data = state_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text,
        weight = 1, radius = ~N*100,
        fillOpacity = 0.8, color = "#AC162C", fillColor = ~colorPalette()(weather_dist),
      )
  )
  observeEvent(
    input$hours,
    leafletProxy("map") |>
      clearMarkers() |>
      addMarkers(
        data = us_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text
      ) |>
      addCircles(
        data = state_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text,
        weight = 1, radius = ~N*100,
        fillOpacity = 0.8, color = "#AC162C", fillColor = ~colorPalette()(weather_dist),
      )
  )
  observeEvent(
    input$day,
    leafletProxy("map") |>
      clearMarkers() |>
      addMarkers(
        data = us_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text
      ) |>
      addCircles(
        data = state_data(), 
        lng = ~LON, lat = ~LAT, # popup = ~text,
        weight = 1, radius = ~N*1000,
        fillOpacity = 0.8, color = "#AC162C", fillColor = ~colorPalette()(weather_dist),
      )
  )
}

# create the Shiny app
shinyApp(ui, server)
