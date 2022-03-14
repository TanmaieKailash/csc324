library(tidyverse)
library(leaflet)
library(leaflet.extras2)
library(rgdal)
library(RColorBrewer)
library(plotly)

# 
# #filtering for languages of India
# # raw_data <- filter(
# #   read.csv("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app/language.csv"),
# #   countrycodes =="IN")
# # 
# # #subsetting for just columns being used
# # clean_data <- subset(raw_data, 
# #                      select = c("Name", "latitude", "longitude", "family"))
# # 
# # # 

# lang_fams <- count(clean_data, family)
# 
# 
# #census data 2011
census_data <- read.csv("pop_speakers_2011.csv")
# 
# #palette
# # factpal <- colorFactor(brewer.pal(10, "Spectral"), clean_data$family)
# 
# 
# #Adding polygons
# sbuy
# # setwd("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app")
# # 
# # my_spdf = readOGR(dsn = "IND_adm",
# #                   layer = "IND_adm1")
# 
# #data for 2011 choropleth maps
# data_2011 <- read.csv("pop_speakers_2011.csv")
# 
# # case_name_state <- function(state_name, new_name){
# #   case_when(
# #   )
# # }
# 
# 
# 
# 
# # #mutating data for census 2011 so it will work with shape file
# # state_names <- (count(data_2011, area_name))$area_name
# # state_names <- state_names[state_names!="INDIA"]
# # shp_states <- my_spdf$NAME_1
# # 
# # data_2011$area_name[data_2011$area_name == "NCT OF DELHI"] <- "Delhi"
# # 
# # for (val in 1:31){
# #   data_2011$area_name[data_2011$area_name == state_names[val]] <- shp_states[val]
# # }
# # 
# # for (val in 32:35){
# #   data_2011$area_name[data_2011$area_name == state_names[val]] <- shp_states[val+1]
# # }
# 
# #---- Functions------
# #functions for cleaning merging with my_spdf 
# 
# 
# # #Language name vector
# # lang_vec <- c("assamese", "bengali", "bodo",
# #               "dogri", "gujarati", "hindi",
# #               "kannada", "kashmiri", "konkani",
# #               "maithili", "malayalam", "manipuri", 
# #               "marathi", "nepali", "odhia", "punjabi",
# #               "sanskrit", "santali", " sindhi", 
# #               "tamil", "telugu", "urdu")
# # 
# # 
# # #function for lang code
# # lang_code <- function(x){
# #   return (match(x, lang_vec) * 1000)
# # }
# # 
# # 
# # #function for single language table census data
# # census_lang_table <- function(code){
# #   return(subset(filter(data_2011, language_code == code & area_name != "INDIA"), 
# #          select = c(area_name, total)))
# # }
# 
# # 
# # 
# # 
# #
# # 
# # 
# # #palnum 
# # 
# # 
# # gen_base_map <- function(){
# #   leaflet() %>%
# #     addProviderTiles("CartoDB.DarkMatter")%>%
# #     setMaxBounds(lng1 = 68.7
# #                  , lat1 = 37
# #                  , lng2 = 97.25
# #                  , lat2 = 5 ) %>%
# #     setView(lng = 70, lat = 25, zoom = 5)
# # }
# # 
# # 
# # #map function
# # generate_choro_map <- function(x){
# #   pal_num <- colorQuantile("Blues", domain = x, n = 9)
# #   gen_base_map()%>%
# #     addPolygons(data = my_spdf,
# #                 stroke = FALSE,
# #                 fillColor =  ~pal_num(x),
# #                 highlight = highlightOptions(#stroke = TRUE,
# #                   weight = 5,
# #                   color = "white",
# #                   fillOpacity = 1,
# #                   bringToFront = TRUE),
# #                 label = ~x) %>%
# #     addLegend("bottomleft", pal = pal_num, values = x,
# #               title = "Number of speakers",
# #               opacity = 1)
# # }
# 
# 
# 
# 
# 
# #---- Adding 2011 Columns to my_spdf ----
# # #Assamese
# # code <- lang_code("assamese")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "assamese_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(assamese_total_11 = sum(assamese_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$assamese_total_11[is.na(my_spdf$assamese_total_11)] = 0
# # 
# # #Bengali
# # code <- lang_code("bengali")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "bengali_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(bengali_total_11 = sum(bengali_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$bengali_total_11[is.na(my_spdf$bengali_total_11)] = 0
# # 
# # #Bodo
# # code <- lang_code("bodo")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "bodo_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(bodo_total_11 = sum(bodo_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$bodo_total_11[is.na(my_spdf$bodo_total_11)] = 0
# # 
# # #Dogri
# # code <- lang_code("dogri")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "dogri_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(dogri_total_11 = sum(dogri_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$dogri_total_11[is.na(my_spdf$dogri_total_11)] = 0
# # 
# # #Gujarati
# # code <- lang_code("gujarati")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "gujarati_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(gujarati_total_11 = sum(gujarati_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$gujarati_total_11[is.na(my_spdf$gujarati_total_11)] = 0
# # 
# # # Hindi
# # code <- lang_code("hindi")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "hindi_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(hindi_total_11 = sum(hindi_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$hindi_total_11[is.na(my_spdf$hindi_total_11)] = 0
# # 
# # #Kannada
# # code <- lang_code("kannada")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "kannada_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(kannada_total_11 = sum(kannada_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$kannada_total_11[is.na(my_spdf$kannada_total_11)] = 0
# # 
# # #Kashmiri
# # code <- lang_code("kashmiri")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "kashmiri_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(kashmiri_total_11 = sum(kashmiri_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$kashmiri_total_11[is.na(my_spdf$kashmiri_total_11)] = 0
# # 
# # #Konkani
# # code <- lang_code("konkani")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "konkani_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(konkani_total_11 = sum(konkani_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$konkani_total_11[is.na(my_spdf$konkani_total_11)] = 0
# # 
# # #Maithili
# # code <- lang_code("maithili")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "maithili_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(maithili_total_11 = sum(maithili_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$maithili_total_11[is.na(my_spdf$maithili_total_11)] = 0
# # 
# # #Malayalam
# # code <- lang_code("malayalam")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "malayalam_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(malayalam_total_11 = sum(malayalam_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$malayalam_total_11[is.na(my_spdf$malayalam_total_11)] = 0
# # 
# # #Manipuri
# # code <- lang_code("manipuri")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "manipuri_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(manipuri_total_11 = sum(manipuri_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$manipuri_total_11[is.na(my_spdf$manipuri_total_11)] = 0
# # 
# # #Marathi
# # code <- lang_code("marathi")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "marathi_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(marathi_total_11 = sum(marathi_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$marathi_total_11[is.na(my_spdf$marathi_total_11)] = 0
# # 
# # #Nepali
# # code <- lang_code("nepali")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "nepali_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(nepali_total_11 = sum(nepali_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$nepali_total_11[is.na(my_spdf$nepali_total_11)] = 0
# # 
# # #Odhia
# # code <- lang_code("odhia")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "odhia_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(odhia_total_11 = sum(odhia_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$odhia_total_11[is.na(my_spdf$odhia_total_11)] = 0
# # 
# # #Punjabi
# # code <- lang_code("punjabi")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "punjabi_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(punjabi_total_11 = sum(punjabi_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$punjabi_total_11[is.na(my_spdf$punjabi_total_11)] = 0
# # 
# # #Santali
# # code <- lang_code("santali")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "santali_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(santali_total_11 = sum(santali_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$santali_total_11[is.na(my_spdf$santali_total_11)] = 0
# # 
# # #Tamil
# # code <- lang_code("tamil")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "tamil_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(tamil_total_11 = sum(tamil_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$tamil_total_11[is.na(my_spdf$tamil_total_11)] = 0
# # 
# # #Telugu
# # code <- lang_code("telugu")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "telugu_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(telugu_total_11 = sum(telugu_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$telugu_total_11[is.na(my_spdf$telugu_total_11)] = 0
# # 
# # #Urdu
# # code <- lang_code("urdu")
# # df <- census_lang_table(code)
# # colnames(df) <- c("NAME_1", "urdu_total_11")
# # df <- df %>% group_by(NAME_1) %>% summarize(urdu_total_11 = sum(urdu_total_11))
# # my_spdf <- merge(my_spdf, df, by="NAME_1")
# # my_spdf$urdu_total_11[is.na(my_spdf$urdu_total_11)] = 0
# 
# 
# #---- 2011 Maps ----
# 
# assamese_map_2011 = generate_choro_map(my_spdf$assamese_total_11)
# 
# bengali_map_2011 = generate_choro_map(my_spdf$bengali_total_11)
# 
# bodo_map_2011 = generate_choro_map(my_spdf$bodo_total_11)
# 
# dogri_map_2011 = generate_choro_map(my_spdf$dogri_total_11)
# 
# gujarati_map_2011 = generate_choro_map(my_spdf$gujarati_total_11)
# 
# hindi_map_2011 = generate_choro_map(my_spdf$hindi_total_11)
# 
# kannada_map_2011 = generate_choro_map(my_spdf$kannada_total_11)
# 
# kashmiri_map_2011 = generate_choro_map(my_spdf$kashmiri_total_11)
# 
# konkani_map_2011 = generate_choro_map(my_spdf$konkani_total_11)
# 
# maithili_map_2011 = generate_choro_map(my_spdf$maithili_total_11)
# 
# malayalam_map_2011 = generate_choro_map(my_spdf$malayalam_total_11)
# 
# manipuri_map_2011 = generate_choro_map(my_spdf$manipuri_total_11)
# 
# marathi_map_2011 = generate_choro_map(my_spdf$marathi_total_11)
# 
# nepali_map_2011 = generate_choro_map(my_spdf$nepali_total_11)
# 
# odhia_map_2011 = generate_choro_map(my_spdf$odhia_total_11)
# 
# punjabi_map_2011 = generate_choro_map(my_spdf$punjabi_total_11)
# 
# santali_map_2011 = generate_choro_map(my_spdf$santali_total_11)
# 
# tamil_map_2011 = generate_choro_map(my_spdf$tamil_total_11)
# 
# telugu_map_2011 = generate_choro_map(my_spdf$telugu_total_11)
# 
# urdu_map_2011 = generate_choro_map(my_spdf$urdu_total_11)
# 
# 
# 
# 
# #Language Family bar chart
# fam_bar <- plot_ly(
#   x = lang_fams$n,
#   y = lang_fams$family,
#   type = "bar",
#   orientation = 'h',
#   name = "Language Families",
#   color = "#9E0142"
# )
# 
# fam_bar <- fam_bar%>% layout(
#   title = "Language families with frequency",
#   plot_bgcolor = "transparent",
#   paper_bgcolor = "transparent")
# 
# 
# 
# 
# labels <- sprintf(
#   "<strong>%s</strong><br/> Language Family: %s<br/>",
#   clean_data$Name, clean_data$family
# ) %>% 
#   lapply(htmltools::HTML)
# 



# setting wd for ease of file porting
setwd("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app")

#-------------- Loading datasets, basic objects, color palettes, and labels -----------------
#languages + family with location data frame creation ----
clean_data <- subset(filter(
  read.csv("language.csv"),
  countrycodes =="IN"), 
  select = c("Name", "latitude", "longitude", "family"))

#endangered languages data frame creation ----
end_langs <- subset(filter(read.csv("endangered_languages.csv"), 
                           Countries == "India"), 
                    select = c("Name.in.English", "Degree.of.endangerment", "Number.of.speakers", "Latitude", "Longitude"))

#shapefile dataframe creation----
my_spdf = readOGR(dsn = "IND_adm",
                  layer = "IND_adm1")

#Census data frame creation----
data_2011 <- read.csv("pop_speakers_2011.csv")
# census_data_2001
# census_data_1991












