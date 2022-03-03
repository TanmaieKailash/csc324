library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(rgdal)
library(RColorBrewer)

#data section
raw_data <- filter(
  read.csv("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app/language.csv"),
  countrycodes =="IN")

#subsetting for just columns being used
clean_data <- subset(raw_data, 
                     select = c("Name", "latitude", "longitude", "family"))

end_langs <- subset(filter(read.csv("unesco_atlas_languages_limited_dataset.csv"), 
                           Countries == "India"), 
                    select = c("Name.in.English", "Degree.of.endangerment"))
#s1 <- shapefile("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app/maps-master/Survey-of-India-Index-Maps/Boundaries/India-States.shp")


#colour Palette
factpal <- colorFactor(brewer.pal(10, "Spectral"), clean_data$family)

#label 
labels <- sprintf(
  "<strong>%s</strong><br/> Language Family: %s<br/> Status:...",
  clean_data$Name, clean_data$family
) %>% 
  lapply(htmltools::HTML)

#UI skeleton
ui <- fluidPage(
  theme = shinytheme("slate"),
  navbarPage(
    "The Languages of India",
    tabPanel(
      "About"
    ),
    tabPanel(
      "A Map of Indian Languages"
    ),
    mainPanel(
      fluidRow(
      align =  "left",
      leafletOutput("map", height = 800, width = 700)
      ),
    ),
    tabPanel(
      "Number of Speakers Over The Years"
    ),
    tabPanel(
      "Navbar 3"
    )
  )
)

#Server skeleton
server <- function(input, output, session){
  output$map <- renderLeaflet({
    my_map = leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter")%>%
      addCircleMarkers(
        data = clean_data,
        stroke = FALSE,
        lng = ~ clean_data$longitude,
        lat = ~ clean_data$latitude,
        # set the opacity of the circles
        fillOpacity = 0.5,
        # set the radius of the circles
        radius = 5,
        #
        color = ~factpal(family),
        
        # highlight = highlightOptions(color = ~factpal(family),
        #                              fillOpacity = 1,
        #                              bringToFront = TRUE),
        
        # create custom labels
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                 padding = "3px 8px"),
                                    textsize = "15px",
                                    direction = "auto"),
        
      )%>%
      
      setMaxBounds(lng1 = 68.7
                   , lat1 = 37
                   , lng2 = 97.25
                   , lat2 = 5 ) #%>%
    
    # addLegend("bottomright", pal = factpal, values = clean_data$family,
    #           title = "Earthquake Magnitude",
    #           opacity = 1)
    

  })
  
}

shinyApp(ui, server)

