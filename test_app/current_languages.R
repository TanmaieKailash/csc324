library(tidyverse)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(plotly)

#filtering for languages of India
# raw_data <- filter(
#   read.csv("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app/language.csv"),
#   countrycodes =="IN")

#subsetting for just columns being used
clean_data <- subset(raw_data, 
                     select = c("Name", "latitude", "longitude", "family"))

# 
# end_langs <- subset(filter(read.csv("unesco_atlas_languages_limited_dataset.csv"), 
#                            Countries == "India"), 
#                     select = c("Name.in.English", "Degree.of.endangerment"))
lang_fams <- count(clean_data, family)


# #read states --> This is not working, 
# states <- geojsonio::geojson_read("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app/GeoJson-Data-of-Indian-States-master/Indian_States.geojson", what = "sp")
# class()
# names(states$State_Name)


#census data 2011

census_data <- read.csv("pop_speakers_2011.csv")

#palette
factpal <- colorFactor(brewer.pal(10, "Spectral"), clean_data$family)
 
#Adding polygons

setwd("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app")

my_spdf = readOGR(dsn = "IND_adm",
                  layer = "IND_adm1")

#data for 2011 choropleth maps

data_2011 <- read.csv("pop_speakers_2011.csv")

# case_name_state <- function(state_name, new_name){
#   case_when(
#   )
# }




#mutating data for census 2011 so it will work with shape file
state_names <- (count(data_2011, area_name))$area_name
state_names <- state_names[state_names!="INDIA"]
shp_states <- my_spdf$NAME_1

data_2011$area_name[data_2011$area_name == "NCT OF DELHI"] <- "Delhi"

for (val in 1:31){
  data_2011$area_name[data_2011$area_name == state_names[val]] <- shp_states[val]
}

for (val in 32:35){
  data_2011$area_name[data_2011$area_name == state_names[val]] <- shp_states[val+1]
}




my_map = leaflet(my_spdf) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(stroke = FALSE,
              fillColor = "#9E0142",
              highlight = highlightOptions(#stroke = TRUE,
                                           weight = 5,
                                           color = "white",
                                           fillOpacity = 0.7,
                                           bringToFront = TRUE),
              label = ~NAME_1)
  



#Language Family bar chart
fam_bar <- plot_ly(
  x = lang_fams$n,
  y = lang_fams$family,
  type = "bar",
  orientation = 'h',
  name = "Language Families",
  color = "#9E0142"
)

fam_bar <- fam_bar%>% layout(
  title = "Language families with frequency",
  plot_bgcolor = "transparent",
  paper_bgcolor = "transparent")


# Hindi table creation

hindi_2011 <- filter(data_2011, language_code==6000)
hindi_2011 <- subset(hindi_2011, select = c(area_name, total))
hindi_2011 <- subset(hindi_2011, area_name != "INDIA")
colnames(hindi_2011)[colnames(hindi_2011)== "total"] <- "hindi_total"



