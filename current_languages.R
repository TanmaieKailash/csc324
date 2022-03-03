library(tidyverse)
library(leaflet)
library(rgdal)
library(RColorBrewer)

#filtering for languages of India
raw_data <- filter(
  read.csv("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app/language.csv"),
  countrycodes =="IN")

#subsetting for just columns being used
clean_data <- subset(raw_data, 
                     select = c("Name", "latitude", "longitude", "family"))

end_langs <- subset(filter(read.csv("unesco_atlas_languages_limited_dataset.csv"), 
                           Countries == "India"), 
                    select = c("Name.in.English", "Degree.of.endangerment"))
lang_fams <- count(clean_data, family)


s1 <- shapefile("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app/maps-master/Survey-of-India-Index-Maps/Boundaries/India-States.shp")


factpal <- colorFactor(brewer.pal(10, "Spectral"), clean_data$family)
 
my_map = leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter")%>%
  addCircleMarkers(
    data = clean_data,
    stroke = FALSE,
    lng = ~ clean_data$longitude,
    lat = ~ clean_data$latitude,
    # set the opacity of the circles
    fillOpacity = 0.6,
    # set the radius of the circles
    radius = 5,
    #
    color = ~factpal(family),
    # create custom labels
    label = paste(paste("Language Name: ", clean_data$Name),
                  paste("Language Family: " , clean_data$family), sep = "\n"),
    )

# clean_data <- clean_data %>% 
#    mutate(color = case_when(str_detect(family, "Austro-Asiatic") ~ "#D81B60",
#                             str_detect(family, "Dravidian") ~ "#1E88E5",
#                             str_detect(family, "Great Andamanese") ~ "#FFC107",
#                             str_detect(family, "Indo-European") ~ "#004D40",
#                             str_detect(family, "Nahali") ~ "#B1D9F5",
#                             str_detect(family, "other") ~ "#6968AE",
#                             str_detect(family, "Shompen") ~ "#A8BF24",
#                             str_detect(family, "Sino-Tibetan") ~ "#FEEE3F",
#                             str_detect(family, "South Andamanese ") ~ "#958C34",
#                             str_detect(family, "Sulung ") ~ "#A2C59B",
#                            TRUE ~ "a default"))


