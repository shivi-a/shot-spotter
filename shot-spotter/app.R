
library(shiny)
library(tidyverse)
library(leaflet)
library(readr)

shot <- read_rds("shot_file.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Gunshots in Camden City, NJ", "Data"),
   
   br(),
     
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        radioButtons("Year", 
                     "Year:", 
                     c(2013, 2014, 2015, 2016, 2017),
                     selected = 2013),
        
         sliderInput("Month",
                     "Month:",
                     min = 1,
                     max = 12,
                     value = 5),
        
        p("Note: ShotSpotter data available beginning 05/2013"),
        br(),
        
        checkboxGroupInput("Type",
                           "Type:",
                           unique(shot$type), 
                           selected = unique(shot$type))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         h6("Thanks to data from the ", a("ShotSpotter Project", href="http://justicetechlab.org/shotspotter-data/"), " by the Justice Tech Lab, we can visualize gunshot data in Camden, NJ"),
         leafletOutput("mapPlot")
      )
   )
)

# Define server logic required to draw a Leaflet plot

server <- function(input, output) {
  
   output$mapPlot <- renderLeaflet({
     
     shot_select <- shot %>% filter(year == input$Year, month == input$Month, type %in% input$Type)
     
     pal <- colorFactor(palette = c("red", "blue", "green"), 
                        levels = c("Multiple Gunshots", "Single Gunshot", "Gunshot or Firecracker"))
     
     leaflet(options = leafletOptions(minZoom = 12, dragging = TRUE))  %>% 
       addProviderTiles("Esri.WorldStreetMap")  %>% 
       setView(lng = -75.119621, lat = 39.931364, zoom = 13) %>% 
       addCircleMarkers(
         data = shot_select, 
         color = ~pal(shot_select$type), 
         lng = shot_select$longitude, 
         lat = shot_select$latitude, 
         radius = 1, 
         label = shot_select$date) %>% 
       
       setMaxBounds(lng1 = -75.119621 + .075, 
                    lat1 = 39.931364 + .05, 
                    lng2 = -75.119621 - .075, 
                    lat2 = 39.931364 - .05)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

