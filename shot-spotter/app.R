
# Load in neccessary dependencies

library(shiny)
library(tidyverse)
library(leaflet)
library(readr)

# Read in Camden shotspotter data from rds file created from R script

shot <- read_rds("shot_file.rds")

# Define UI for application that draws a histogram

ui <- fluidPage(
   
   # Application title
  
   titlePanel("Gunshots in Camden City, NJ", "Data"),
   
   # Add line break for Shiny App aesthetics
   
   br(),
     
   # Sidebar with input options for year, month, and type of gunshot 
   
   sidebarLayout(
      sidebarPanel(
        
        # Radio button selector for year so that viewers can choose one year at
        # a time. 2018 is not allowed as an option because that data is very
        # small and looks to be an artifact of the dataset - has an unusual
        # uniform distribution
        
        radioButtons("Year", 
                     "Year:", 
                     c(2013, 2014, 2015, 2016, 2017),
                     selected = 2013),
        
        # Slider input to choose the month to view so that viewers can toggle
        # between the month of that particular year. The data starts at 05/2013,
        # thus the default month value is 05 since the default year is 2013.
        # Animation is toggled on such that a button can be pressed for the
        # months to be automatically cycled through
        
        sliderInput("Month",
                     "Month:",
                     min = 1,
                     max = 12,
                     value = 5,
                     animate = TRUE),
        
        # Include note about the limitations of ShotSpotter data and a line beak
        # for aesthetic purposes
        
        p("Note: ShotSpotter data available beginning 05/2013"),
        
        br(),
        
        checkboxGroupInput("Type",
                           "Type:",
                           unique(shot$type), 
                           selected = unique(shot$type))
      ),
      
      # Show a plot of the generated Leaflet map. In this instance, the use of
      # the map overlay provided by Leaflet seemed far superior to the shapefile
      # approach, which may recreate the boundaries of Camden City, but lacks
      # context for the overlaid points to have any meaning
      
      mainPanel(
        
        # Add a note thanking the Justice Tech Lab (and provide a link to their
        # site!)
        
        h6("Thanks to data from the ", a("ShotSpotter Project", href="http://justicetechlab.org/shotspotter-data/"), " by the Justice Tech Lab, we can visualize gunshot data in Camden, NJ"),
        leafletOutput("mapPlot")
      )
   )
)

# Define server logic required to draw a Leaflet plot

server <- function(input, output, session) {
  
   output$mapPlot <- renderLeaflet({
     
     if (length(input$Type) == 0 | (input$Year == 2013 & input$Month < 5)) {
        
       leaflet(options = leafletOptions(minZoom = 13, dragging = TRUE))  %>% 
         addProviderTiles("Esri.WorldStreetMap")  %>% 
         setView(lng = -75.119621, lat = 39.931364, zoom = 13) %>% 
         
         # Fix the bounds to which the leaflet map may be dragged, such that it is
         # limited to the Camden City area
         
         setMaxBounds(lng1 = -75.119621 + .075, 
                      lat1 = 39.931364 + .05, 
                      lng2 = -75.119621 - .075, 
                      lat2 = 39.931364 - .05)
       
     }
     
     else {
     
     shot_select <- shot %>% filter(year == input$Year, month == input$Month, type %in% input$Type)
     
     pal <- colorFactor(palette = c("red", "blue", "green"), 
                        levels = c("Multiple Gunshots", "Single Gunshot", "Gunshot or Firecracker"))
     
     leaflet(options = leafletOptions(minZoom = 13, dragging = TRUE))  %>% 
       addProviderTiles("Esri.WorldStreetMap")  %>% 
       setView(lng = -75.119621, lat = 39.931364, zoom = 13) %>% 
       addCircleMarkers(
         data = shot_select, 
         
         # Color the points by the type of gunshot to provide another layer of
         # information and allow viewers to assess the "severity" of the
         # gunshots being recorded at a given time and place
         
         color = ~pal(shot_select$type), 
         
         # Use the longitude and latitude from the Camden, NJ shotspotter
         # dataset to inform the marker positons
         
         lng = shot_select$longitude, 
         lat = shot_select$latitude, 
         
         # Make the radius of the points small - size 1 - so that multiple
         # points nearby can be more clearly visualized
         
         radius = 1, 
         label = shot_select$date) %>% 
       
       # Fix the bounds to which the leaflet map may be dragged, such that it is
       # limited to the Camden City area
       
       setMaxBounds(lng1 = -75.119621 + .075, 
                    lat1 = 39.931364 + .05, 
                    lng2 = -75.119621 - .075, 
                    lat2 = 39.931364 - .05)
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

