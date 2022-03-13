library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(shinyWidgets)
library(plotly)

# setting wd for ease of file porting
setwd("/Users/tanmaiekailash/Desktop/cs_workspace/rStudio_workspace/shiny/test_app")

#-------------- Loading datasets, basic objects, color palettes, and labels-----
  #----languages + family with location data frame creation ----
clean_data <- subset(filter(
  read.csv("language.csv"),
  countrycodes =="IN"), 
  select = c("Name", "latitude", "longitude", "family"))

  #----endangered languages data frame creation ----
end_langs <- subset(filter(read.csv("endangered_languages.csv"), 
                           Countries == "India"), 
                    select = c("Name.in.English", "Degree.of.endangerment", "Number.of.speakers", "Latitude", "Longitude"))

  #----shapefile dataframe creation----
my_spdf = readOGR(dsn = "IND_adm",
                  layer = "IND_adm1")

  #Census data frame creation----
data_2011 <- read.csv("pop_speakers_2011.csv")
# census_data_2001
# census_data_1991



#Color palettes ----
#for lang_fam_map
factpal <- colorFactor(brewer.pal(10, "Spectral"), clean_data$family)

#for end_lang_map
endpal <- colorFactor(brewer.pal(5, "YlOrRd"), end_langs$Degree.of.endangerment)

#labels ---- 
#label for lang_fam_map
labels <- sprintf(
  "<strong>%s</strong><br/> Language Family: %s<br/>",
  clean_data$Name, clean_data$family
) %>% 
  lapply(htmltools::HTML)

#label for end_lang_map
label2 <- sprintf(
  "<strong>%s</strong><br/> Language name: %s<br/> Number of Speakers: %d",
  end_langs$Degree.of.endangerment, end_langs$Name.in.English, 
  end_langs$Number.of.speakers
) %>% 
  lapply(htmltools::HTML)

#-------------- adding columns to my_spdf from census data------
#mutating data for census 2011 so it will work with shape file----
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

#Functions to aid cleaning and merging with my_spdf----
#Language name vector
lang_vec <- c("assamese", "bengali", "bodo",
              "dogri", "gujarati", "hindi",
              "kannada", "kashmiri", "konkani",
              "maithili", "malayalam", "manipuri", 
              "marathi", "nepali", "odhia", "punjabi",
              "sanskrit", "santali", " sindhi", 
              "tamil", "telugu", "urdu")

#function for lang code
lang_code <- function(x){
  return (match(x, lang_vec) * 1000)
}

#function for single language table census data
census_lang_table <- function(code){
  return(subset(filter(data_2011, language_code == code & area_name != "INDIA"), 
                select = c(area_name, total)))
}


#Adding 2011 Columns to my_spdf ----

#Assamese
code <- lang_code("assamese")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Assamese")
df <- df %>% group_by(NAME_1) %>% summarize(Assamese = sum(Assamese))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Assamese[is.na(my_spdf$Assamese)] = 0

#Bengali
code <- lang_code("bengali")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Bengali")
df <- df %>% group_by(NAME_1) %>% summarize(Bengali = sum(Bengali))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Bengali[is.na(my_spdf$Bengali)] = 0

#Bodo
code <- lang_code("bodo")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Bodo")
df <- df %>% group_by(NAME_1) %>% summarize(Bodo = sum(Bodo))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Bodo[is.na(my_spdf$Bodo)] = 0

#Dogri
code <- lang_code("dogri")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Dogri")
df <- df %>% group_by(NAME_1) %>% summarize(Dogri = sum(Dogri))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Dogri[is.na(my_spdf$Dogri)] = 0

#Gujarati
code <- lang_code("gujarati")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Gujarati")
df <- df %>% group_by(NAME_1) %>% summarize(Gujarati = sum(Gujarati))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Gujarati[is.na(my_spdf$Gujarati)] = 0

# Hindi
code <- lang_code("hindi")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Hindi")
df <- df %>% group_by(NAME_1) %>% summarize(Hindi = sum(Hindi))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Hindi[is.na(my_spdf$Hindi)] = 0

#Kannada
code <- lang_code("kannada")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Kannada")
df <- df %>% group_by(NAME_1) %>% summarize(Kannada = sum(Kannada))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Kannada[is.na(my_spdf$Kannada)] = 0

#Kashmiri
code <- lang_code("kashmiri")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Kashmiri")
df <- df %>% group_by(NAME_1) %>% summarize(Kashmiri = sum(Kashmiri))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Kashmiri[is.na(my_spdf$Kashmiri)] = 0

#Konkani
code <- lang_code("konkani")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Konkani")
df <- df %>% group_by(NAME_1) %>% summarize(Konkani = sum(Konkani))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Konkani[is.na(my_spdf$Konkani)] = 0

#Maithili
code <- lang_code("maithili")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Maithili")
df <- df %>% group_by(NAME_1) %>% summarize(Maithili = sum(Maithili))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Maithili[is.na(my_spdf$Maithili)] = 0

#Malayalam
code <- lang_code("malayalam")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Malayalam")
df <- df %>% group_by(NAME_1) %>% summarize(Malayalam = sum(Malayalam))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Malayalam[is.na(my_spdf$Malayalam)] = 0

#Manipuri
code <- lang_code("manipuri")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Manipuri")
df <- df %>% group_by(NAME_1) %>% summarize(Manipuri = sum(Manipuri))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Manipuri[is.na(my_spdf$Manipuri)] = 0

#Marathi
code <- lang_code("marathi")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Marathi")
df <- df %>% group_by(NAME_1) %>% summarize(Marathi = sum(Marathi))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Marathi[is.na(my_spdf$Marathi)] = 0

#Nepali
code <- lang_code("nepali")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Nepali")
df <- df %>% group_by(NAME_1) %>% summarize(Nepali = sum(Nepali))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Nepali[is.na(my_spdf$Nepali)] = 0

#Odhia
code <- lang_code("odhia")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Odhia")
df <- df %>% group_by(NAME_1) %>% summarize(Odhia = sum(Odhia))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Odhia[is.na(my_spdf$Odhia)] = 0

#Punjabi
code <- lang_code("punjabi")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Punjabi")
df <- df %>% group_by(NAME_1) %>% summarize(Punjabi = sum(Punjabi))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Punjabi[is.na(my_spdf$Punjabi)] = 0

#Santali
code <- lang_code("santali")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Santali")
df <- df %>% group_by(NAME_1) %>% summarize(Santali = sum(Santali))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Santali[is.na(my_spdf$Santali)] = 0

#Tamil
code <- lang_code("tamil")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Tamil")
df <- df %>% group_by(NAME_1) %>% summarize(Tamil = sum(Tamil))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Tamil[is.na(my_spdf$Tamil)] = 0

#Telugu
code <- lang_code("telugu")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Telugu")
df <- df %>% group_by(NAME_1) %>% summarize(Telugu = sum(Telugu))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Telugu[is.na(my_spdf$Telugu)] = 0

#Urdu
code <- lang_code("urdu")
df <- census_lang_table(code)
colnames(df) <- c("NAME_1", "Urdu")
df <- df %>% group_by(NAME_1) %>% summarize(Urdu = sum(Urdu))
my_spdf <- merge(my_spdf, df, by="NAME_1")
my_spdf$Urdu[is.na(my_spdf$Urdu)] = 0






#--------- Map creation functions -------
gen_base_map <- function(){
  leaflet() %>%
    addProviderTiles("CartoDB.DarkMatter")%>%
    setMaxBounds(lng1 = 68.7
                 , lat1 = 37
                 , lng2 = 97.25
                 , lat2 = 5 ) %>%
    setView(lng = 70, lat = 25, zoom = 5)
}

generate_choro_map <- function(x){
  pal_num <- colorQuantile("Blues", domain = x, n = 9)
  gen_base_map()%>%
    addPolygons(data = my_spdf,
                stroke = FALSE,
                fillColor =  ~pal_num(x),
                highlight = highlightOptions(#stroke = TRUE,
                  weight = 5,
                  color = "white",
                  fillOpacity = 1,
                  bringToFront = TRUE),
                label = ~x)
}



#---- Maps---- 

#----lang_fam_map----
lang_fam_map = gen_base_map()%>%
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
    label = labels,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")
    
  )%>%
  addLegend("bottomleft", pal = factpal, values = clean_data$family,
            title = "Language Family",
            opacity = 1)



#----end_lang_map----
end_lang_map = gen_base_map()%>%
    addCircleMarkers(
      data = end_langs,
      stroke = FALSE,
      lng = ~ end_langs$Longitude,
      lat = ~ end_langs$Latitude,
      fillOpacity = 0.5,
      radius = 5,
      color = ~endpal(Degree.of.endangerment),
      label = label2,
      labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                               padding = "3px 8px"),
                                  textsize = "15px",
                                  direction = "auto")
    )%>%
    addLegend("bottomleft", pal = endpal, values = end_langs$Degree.of.endangerment,
              title = "Degree of Endangerment",
              opacity = 1) 

#----vul_lang_map----
df <- filter(end_langs, Degree.of.endangerment == "Vulnerable")
vul_lang_map =gen_base_map()%>%
  addCircleMarkers(
    data = df,
    stroke = FALSE,
    lng = ~ df$Longitude,
    lat = ~ df$Latitude,
    fillOpacity = 0.5,
    radius = 5,
    color = ~endpal(Degree.of.endangerment),
    label = df$Name.in.English,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")
    )%>%
  addLegend("bottomleft", pal = endpal, values = df$Degree.of.endangerment,
            title = "Degree of Endangerment",
            opacity = 1) 

#----def_end_lang_map----
df <- filter(end_langs, Degree.of.endangerment == "Definitely endangered")
def_end_lang_map =gen_base_map()%>%
  addCircleMarkers(
    data = df,
    stroke = FALSE,
    lng = ~ df$Longitude,
    lat = ~ df$Latitude,
    fillOpacity = 0.5,
    radius = 5,
    color = ~endpal(Degree.of.endangerment),
    label = df$Name.in.English,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")
  )%>%
  addLegend("bottomleft", pal = endpal, values = df$Degree.of.endangerment,
            title = "Degree of Endangerment",
            opacity = 1) 

#----crit_end_map----
df <- filter(end_langs, Degree.of.endangerment == "Critically endangered")
crit_end_map =gen_base_map()%>%
  addCircleMarkers(
    data = df,
    stroke = FALSE,
    lng = ~ df$Longitude,
    lat = ~ df$Latitude,
    fillOpacity = 0.5,
    radius = 5,
    color = ~endpal(Degree.of.endangerment),
    label = df$Name.in.English,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")
  )%>%
  addLegend("bottomleft", pal = endpal, values = df$Degree.of.endangerment,
            title = "Degree of Endangerment",
            opacity = 1) 

#----ex_map ----
df <- filter(end_langs, Degree.of.endangerment == "Extinct")
ex_map  =gen_base_map()%>%
  addCircleMarkers(
    data = df,
    stroke = FALSE,
    lng = ~ df$Longitude,
    lat = ~ df$Latitude,
    fillOpacity = 0.5,
    radius = 5,
    color = ~endpal(Degree.of.endangerment),
    label = df$Name.in.English,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")
  )%>%
  addLegend("bottomleft", pal = endpal, values = df$Degree.of.endangerment,
            title = "Degree of Endangerment",
            opacity = 1)


#----sev_end_map  ----
df <- filter(end_langs, Degree.of.endangerment == "Severely endangered")
sev_end_map   =gen_base_map()%>%
  addCircleMarkers(
    data = df,
    stroke = FALSE,
    lng = ~ df$Longitude,
    lat = ~ df$Latitude,
    fillOpacity = 0.5,
    radius = 5,
    color = ~endpal(Degree.of.endangerment),
    label = df$Name.in.English,
    labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                             padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")
  )%>%
  addLegend("bottomleft", pal = endpal, values = df$Degree.of.endangerment,
            title = "Degree of Endangerment",
            opacity = 1)




#-------------- UI body -----------------
ui <- fluidPage(
  theme = shinytheme("slate"),
  navbarPage(
    "Visualizing The Languages of India",

    tabPanel(
      "About"
    ),#tabPanel
    
      tabPanel(
        "Languages And Language Families of India",
        mainPanel(
          tabsetPanel(
            tabPanel("Map of languages and language families",
            leafletOutput("family_map", height = 800, width = 700)
            ), 
            tabPanel("Comparing number of languages in language families with number of speakers",
                     fluidRow(
                       column(6, 
                     plotlyOutput("lang_fam_plot")),
                     column(6,
                            plotlyOutput("num_speakers_fam"))
                     )
                   )
          )#tabsetPanel
      )#mainPanel
      ),#tabPanel
    
    tabPanel(
      "Endangered Languages",
      mainPanel(
        tabsetPanel(
          tabPanel("Map of endangered languages with degree of endangerment",
                 leafletOutput("endangered_langs", height = 800, width = 700)),
          tabPanel("Vulnerable Languages",
                   leafletOutput("vul_lang", height = 800, width = 700)),
          tabPanel("Definitely Endangered Languages",
                   leafletOutput("def_end_lang", height = 800, width = 700)),
          tabPanel("Critically Endangered Languages",
                   leafletOutput("cri_end_lang", height = 800, width = 700)),
          tabPanel("Extinct Languages",
                   leafletOutput("ex_lang", height = 800, width = 700)),
          tabPanel("Severely Endangered Language",
                   leafletOutput("sev_lang", height = 800, width = 700))
        )
        )#mainPanel
        ),#tabPanel
    
      tabPanel(
        "Number of speakers by language",
        mainPanel(
          fluidRow(
            column(9,
            leafletOutput("lang_map", height = 800, width = 700)
            ),
            column (3,
            selectInput("choose_language",
                        label = "Choose language",
                        list("Assamese", "Bengali", "Bodo", 
                             "Dogri", "Gujarati", "Hindi", 
                             "Kannada", "Kashmiri", "Konkani", 
                             "Maithili", "Malayalam", "Manipuri", "Marathi",
                             "Nepali", "Odhia", "Punjabi", 
                             "Santali", "Tamil", "Telugu", "Urdu"))
            )
            
          )#fluidRow
        )#mainPanel
        
      )#tabPanel
  )
)#fluidPage

#-------------- Server body -----------------
server <- function(input, output, session){
  
  #-------------- Language families map ----------------- 
  output$family_map <- renderLeaflet({
    lang_fam_map
  })
  
  
  #-------------- All Endangered Languages----------------- 
  output$endangered_langs <- renderLeaflet({
      my_map <- end_lang_map 
    })
  
  #-------------- Vulnerable Languages map ----------------- 
  output$vul_lang <- renderLeaflet({
    my_map <- vul_lang_map 
  })
  
  #-------------- Definitely endangered map ----------------- 
  output$def_end_lang <- renderLeaflet({
    my_map <- def_end_lang_map 
  })
  
  #-------------- Critically endangered map ----------------- 
  output$cri_end_lang <- renderLeaflet({
    my_map <- crit_end_map 
  })
  
  #-------------- Extinct map ----------------- 
  output$ex_lang <- renderLeaflet({
    my_map <- ex_map 
  })
  
  #-------------- Severely endangered map ----------------- 
  output$sev_lang <- renderLeaflet({
    my_map <- sev_end_map 
  })
  
  #-------------- Language speakers map ----------------- 
  
  output$lang_map <- renderLeaflet({
    my_map <- generate_choro_map(my_spdf[[input$choose_language]])
  })
  
  
  
  #-------------- Language Families Barchart ----------------- 
  output$lang_fam_plot <- renderPlotly(
    fam_bar <- plot_ly(
      x = lang_fams$n,
      y = lang_fams$family,
      type = "bar",
      orientation = 'h',
      name = "Language Families", 
      color = I("bisque3")
    )%>% 
      layout(
      title = "Language families with frequency",
      plot_bgcolor = "transparent")
  )


  
  
  #-------------- Endangered Language Density----------------- 
  
}


#-------------- Shiny app function call -----------------
shinyApp(ui, server)

