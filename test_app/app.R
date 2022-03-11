library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(shinyWidgets)
library(plotly)


#-------------- Data set -----------------
#data section
raw_data <- filter(
  read.csv("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app/language.csv"),
  countrycodes =="IN")

#subsetting for just columns being used
clean_data <- subset(raw_data, 
                     select = c("Name", "latitude", "longitude", "family"))

end_langs <- subset(filter(read.csv("endangered_languages.csv"), 
                           Countries == "India"), 
                    select = c("Name.in.English", "Degree.of.endangerment", "Number.of.speakers", "Latitude", "Longitude"))
#s1 <- shapefile("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app/maps-master/Survey-of-India-Index-Maps/Boundaries/India-States.shp")


#colour Palette
factpal <- colorFactor(brewer.pal(10, "Spectral"), clean_data$family)
endpal <- colorFactor(brewer.pal(5, "Spectral"), end_langs$Degree.of.endangerment)

#label 
labels <- sprintf(
  "<strong>%s</strong><br/> Language Family: %s<br/>",
  clean_data$Name, clean_data$family
) %>% 
  lapply(htmltools::HTML)

label2 <- sprintf(
  "<strong>%s</strong><br/> Language name: %s<br/> Number of Speakers: %d",
  end_langs$Degree.of.endangerment, end_langs$Name.in.English, 
  end_langs$Number.of.speakers
) %>% 
  lapply(htmltools::HTML)



setwd("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app")

my_spdf = readOGR(dsn = "IND_adm",
                  layer = "IND_adm1")

#-------------- UI body -----------------
ui <- fluidPage(
  theme = shinytheme("slate"),
  navbarPage(
    "The Languages of India",
    tabPanel(
      "About"
    ),#tabPanel
    
    #tabPanel
      tabPanel(
        "Map",
        mainPanel(
          fluidRow(
            column( 8,
            h2("Language Map"),
            leafletOutput("family_map", height = 800, width = 700)
            ), 
            column(8, 
                   plotlyOutput("lang_fam_plot")
                   )
          ),#fluidRow
      )#mainPanel
      ),#tabPanel
    
    #tabPanel Menu 2----
    tabPanel(
      "Endangered Languages",
      mainPanel(
        fluidRow(
          column( 9,
                  h2("Endangered Languages Map"),
                  leafletOutput("endangered_langs", height = 800, width = 700)
          ), 
          #FIXME: fix the radio buttons and make it reactive so it switches between maps
          column(3, 
                 radioButtons("radio", h3("Select view:"),
                              choices = list ("Blah" = 1,
                                              "Blah2" = 2),
                              )
                 )
        )
      )
    ),
    
    
    #tabPanel Menu 3----
      tabPanel(
        "Number of speakers by language",
        mainPanel(
          fluidRow(
            align =  "left",
            leafletOutput("lang_map", height = 800, width = 700)
          )#fluidRow
        )#mainPanel
        
      )#tabPanel
  )
)

#-------------- Server body -----------------
server <- function(input, output, session){
  
  #-------------- Language families map ----------------- 
  output$family_map <- renderLeaflet({
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
                                    direction = "auto")
        
      )%>%
      
      setMaxBounds(lng1 = 68.7
                   , lat1 = 37
                   , lng2 = 97.25
                   , lat2 = 5 ) %>%
    
    addLegend("bottomleft", pal = factpal, values = clean_data$family,
               title = "Language Family",
               opacity = 1)%>%
      setView(lng = 70, lat = 25, zoom = 5)
    

  })
  
  #-------------- Language speakers map ----------------- 
  
  output$lang_map <- renderLeaflet({
    my_map = leaflet(my_spdf) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addPolygons(stroke = FALSE,
                  fillColor = "#9E0142",
                  highlight = highlightOptions(#stroke = TRUE,
                    weight = 5,
                    color = "white",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = ~NAME_1)%>%
      
      setMaxBounds(lng1 = 68.7
                   , lat1 = 37
                   , lng2 = 97.25
                   , lat2 = 5 ) #%>%
    
    # addLegend("bottomright", pal = factpal, values = clean_data$family,
    #           title = "Earthquake Magnitude",
    #           opacity = 1)
    
    
  })
  
  
  
  #-------------- Language Families Barchart ----------------- 
  output$lang_fam_plot <- renderPlotly(
    fam_bar <- plot_ly(
      x = lang_fams$n,
      y = lang_fams$family,
      type = "bar",
      orientation = 'h',
      name = "Language Families",
      color = "#9E0142"
    )%>% 
      layout(
      title = "Language families with frequency",
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent")
  )

  #-------------- Endangered Languages----------------- 
  
  output$endangered_langs <- renderLeaflet({
      my_map = leaflet() %>%
        addProviderTiles("CartoDB.DarkMatter")%>%
        addCircleMarkers(
          data = end_langs,
          stroke = FALSE,
          lng = ~ end_langs$Longitude,
          lat = ~ end_langs$Latitude,
          # set the opacity of the circles
          fillOpacity = 0.5,
          # set the radius of the circles
          radius = 5,
          #
          color = ~endpal(Degree.of.endangerment),
          
          # highlight = highlightOptions(color = ~factpal(family),
          #                              fillOpacity = 1,
          #                              bringToFront = TRUE),
          
          # create custom labels
          label = label2,
          labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                   padding = "3px 8px"),
                                      textsize = "15px",
                                      direction = "auto")
          
        )%>%
        
        setMaxBounds(lng1 = 68.7
                     , lat1 = 37
                     , lng2 = 97.25
                     , lat2 = 5 ) %>%
      
      addLegend("bottomleft", pal = endpal, values = end_langs$Degree.of.endangerment,
                title = "Degree of Endangerment",
                opacity = 1) %>%
        setView(lng = 70, lat = 25, zoom = 5)
      
  })
  
  
  #-------------- Endangered Language Density----------------- 
  
}


#-------------- Shiny app function call -----------------
shinyApp(ui, server)

