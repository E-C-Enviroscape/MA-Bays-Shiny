#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf) # GIS package
library (readxl)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(maptools)
library(gridExtra)
library(plotly)

# Loading and tidying data xlsx files
Flats_Data <- read_csv("Flats_Data_MassBays_edit.csv")
#Flats_Data <- merge(Flats_Data, Stressor_Data, by = "EMBAYMENT NAME", all = TRUE)

Tidy_Flats_Data <- Flats_Data %>% 
  #gather(Year, FlatsAcreage, 8:10) %>% # these columns correspond to the "..._MassBays.csv" files
  gather(Year, FlatsAcreage, 8:10) %>% # these columns correspond to the "..._edit.csv" files
  drop_na(FlatsAcreage)
Tidy_Flats_Data$Year <- as.factor(Tidy_Flats_Data$Year)

Marsh_Data <- read_csv("Marsh_Data_MassBays_edit.csv")
#Marsh_Data <- merge(Marsh_Data, Stressor_Data, by = "EMBAYMENT NAME", all = TRUE)

Tidy_Marsh_Data <- Marsh_Data %>% 
  #gather(Year, MarshAcreage, 8:11) %>% # these columns correspond to the "..._MassBays.csv" files
  gather(Year, MarshAcreage, 8:11) %>% # these columns correspond to the "..._edit.csv" files
  drop_na(MarshAcreage)
Tidy_Marsh_Data$Year <- as.factor(Tidy_Marsh_Data$Year)

Seagrass_Data <- read_csv("Seagrass_Data_MassBays.csv")
#Seagrass_Data <- merge(Seagrass_Data, Stressor_Data, by = "EMBAYMENT NAME", all = TRUE)

Tidy_Seagrass_Data <- Seagrass_Data %>% 
  gather(Year, SeagrassAcreage, 8:16)%>%
  drop_na(SeagrassAcreage)
Tidy_Seagrass_Data$Year <- as.factor(Tidy_Seagrass_Data$Year)

MassBaysEmbayments <- st_read(dsn = "massbays_estuarine_embays_2021", layer = "massbays_estuarine_embays_2021")

MassBaysEmbayments <-
  st_transform(MassBaysEmbayments, crs = "+init=epsg:4326")

MassBaysEmbayments <- st_zm(MassBaysEmbayments, drop = T, what = "ZM")

# Loading stressor data xlsx files
Stressor_Data <- read_csv("EDA2Redo_TH_7-15-2020.csv")
Stressor_Data <- merge(Flats_Data, Stressor_Data, by = "EMBAYMENT NAME", all = TRUE)
#Stressor_Data <- subset(Stressor_Data, select = -c(5:14, 29:33)) # these columns correspond to the "..._MassBays.csv" files
Stressor_Data <- subset(Stressor_Data, select = -c(5:11, 26:30)) # these columns correspond to the "..._edit.csv" files

##### Normalizing Stressor Data ################
Stressor_Data_norm <- as.data.frame(apply(Stressor_Data[ ,5:18], 2, function(x) (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))))
Stressor_Data_norm$`EMBAYMENT NAME` <- Stressor_Data$`EMBAYMENT NAME`
Stressor_Data_norm$EcoType <- Stressor_Data$EcoType
Stressor_Data_norm$`MassBays Region` <- Stressor_Data$`MassBays Region`
Stressor_Data_norm$Northeastern_category <- Stressor_Data$Northeastern_category

##### Tidy Stressor Data ################
Tidy_Stressor_Data_norm <-gather(Stressor_Data_norm, key = "Stressor", value = "Stressor_Value", tidal.flushing, 
                                 unhardenable.uncorrected, total.shoreline, unhard.std, hard.of.hard, high.intensity,
                                 stormwater, StormwaterSTD, pop.density, percent.septic, septic.acre, nutrient, bacteria, tidal.restrict)


embayment_choices = c(
  "SELECT EMBAYMENT",
  "ANNISQUAM RIVER",
  "BACK RIVER / FORE RIVER / HINGHAM BAY",
  "BARNSTABLE HARBOR",
  "BELLE ISLE CREEK / WINTHROP BAY",
  "BLACK ROCK CREEK",
  "BLACKS CREEK / QUINCY BAY",
  "BLUEFISH RIVER / BACK RIVER / DUXBURY BAY",
  "BOAT MEADOW CREEK / ROCK HARBOR",
  "BOSTON HARBOR ISLANDS",
  "CHASE GARDEN CREEK",
  "CHELSEA CREEK / MYSTIC RIVER / CHARLES RIVER",
  "COASTAL PLUM ISLAND SOUND",
  "COHASSET HARBOR",
  "DANVERS RIVER",
  "EEL RIVER / PLYMOUTH HARBOR",
  "ELLISVILLE HARBOR",
  "ESSEX RIVER / ESSEX BAY",
  "FOREST RIVER/ SOUTH RIVER/ SALEM HARBOR",
  "GLOUCESTER HARBOR",
  "HERRING RIVER / HERRING POND",
  "IPSWICH RIVER",
  "JONES RIVER / KINGSTON BAY",
  "LITTLE HARBOR",
  "MANCHESTER HARBOR",
  "MARBLEHEAD HARBOR",
  "MERRIMACK RIVER",
  "NAMSKAKET CREEK / LITTLE NAMSKAKET CREEK",
  "NEPONSET RIVER / DORCHESTER BAY",
  "NORTH RIVER / SOUTH RIVER",
  "PAINE'S CREEK / STONY BROOK",
  "PAMET RIVER / LITTLE PAMET RIVER",
  "PARKER RIVER",
  "PROVINCETOWN HARBOR",
  "QUIVETT CREEK",
  "ROCKPORT HARBOR (SANDY BAY)",
  "ROWLEY RIVER",
  "SALEM SOUND / BEVERLY HARBOR",
  "SANDWICH HARBOR",
  "SAUGUS RIVER / PINES RIVER / LYNN HARBOR",
  "SCITUATE HARBOR",
  "SCORTON CREEK",
  "SESUIT CREEK / SESUIT HARBOR",
  "WEIR RIVER / STRAITS POND",
  "WELLFLEET HARBOR"
)

ecotype_choices = c("SELECT ECOTYPE","Blue",
                    "Green",
                    "Orange",
                    "Yellow")

ecotype_definitions = c("High-energy, little or no modern sediment, exposed embayments (with relatively low shallow water habitat area)",
                        "Medium- to high-energy, abundant modern sediment, exposed embayments",
                        "Low- to medium-energy, little or no modern sediment, protected embayments",
                        "Low-energy, abundant modern sediment, protected embayments")

regions_choices = c(
  "SELECT REGION",
  "Cape Cod",
  "Lower North Shore",
  "Massachusetts Bay",
  "South Shore",
  "Upper North Shore"
)

data_displayed =c("Percent Remaining", "Acres", "Ecosystem Services")

category_choices = c("SELECT CATEGORY","1", "2", "3", "4", "NA")

ui <- fluidPage(
  titlePanel(
    p(
      img(height = '210px', width = '1286px', src = "MassBays_Image.png"),
      br(),
      h1("MassBays Data Visualization Application", align = "center"),
      br(),
      h4("This is a data exploration tool meant to display and compare embayment-level information about habitats and stressors throughout MassBays. ", align = "center")
    )
  ),
  headerPanel(
    fluidRow(
      h5("Estuarine embayments delineated in the EDA 2.0 by Geosyntec. As data selections are made below, the map will highlight relevant embayment(s).", align = "center")
    )
  ),
  fluidRow(
    wellPanel(
      shinycssloaders::withSpinner(    
        leafletOutput("mymap"),
        size = 2,
        color = "#0080b7"
      )
    )
  ),
  fluidRow(column(12,   
                  tabsetPanel(id = "plot_tabs",
                              tabPanel("Habitats by Ecotype", value = "EcoType",
                                       fluidRow(column(12,
                                                       selectInput("ecotype", label = "MassBays EcoType", choices = ecotype_choices)
                                       )),
                                       fluidRow(column(12,
                                                       textOutput("definition")
                                        )),
                                       fluidRow(column(12, div(style="height:50px"),
                                                       tabsetPanel(id = "ecotype_data_tabs",
                                                                   tabPanel("Totals",
                                                                            sidebarPanel(radioButtons("data_ecotype_totals_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("ecotype_totals_profile", height = "700px"))
                                                                   ),
                                                                   tabPanel("Seagrass",
                                                                            sidebarPanel(radioButtons("data_ecotype_seagrass_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("ecotype_seagrass_profile", height = "700px"))
                                                                            #verbatimTextOutput("hoverData")
                                                                   ),
                                                                   tabPanel("Salt Marsh",
                                                                            sidebarPanel(radioButtons("data_ecotype_marsh_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("ecotype_marsh_profile", height = "700px"))
                                                                   ),
                                                                   tabPanel("Tidal Flats",
                                                                            sidebarPanel(radioButtons("data_ecotype_flats_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("ecotype_flats_profile", height = "700px"))
                                                                   ),
                                                                   tabPanel("Benthic Communities",
                                                                            #sidebarPanel(radioButtons("benthic_displayed","Choose data option", data_displayed)),
                                                                            #mainPanel(plotlyOutput("benthic_profile", height = "700px"))
                                                                   ),
                                                                   tabPanel("Diadromous Fish",
                                                                            #sidebarPanel(radioButtons("diadromous_displayed","Choose data option", data_displayed)),
                                                                            #mainPanel(plotlyOutput("diadromous_profile", height = "700px"))
                                                                   )
                                                       )
                                       ))
                                       #tabPanel(id = "ecotype_tables","Tables",
                                       #    tabsetPanel(id = "ecotype_data",
                                       #       tabPanel("Tidal Flats", value = "Tidal Flats",
                                       #           dataTableOutput("ecotype_flats_table")
                                       #       ),
                                       #       tabPanel("Salt Marsh", value = "Salt Marsh",
                                       #           dataTableOutput("ecotype_marsh_table")
                                       #       ),
                                       #       tabPanel("Seagrass", value = "Seagrass",
                                       #           dataTableOutput("ecotype_seagrass_table")
                                       #       )
                                       #)
                              ),
                              # tabPanel("Stressor-Resource Categories", value = "Northeastern Category",
                              #           fluidRow(column(12,div(style="height:20px"),
                              #                          tabsetPanel(id = "main_category_tabs",
                              #                                      tabPanel("About",
                              #                                               fluidRow(   
                              #                                               (column(12,
                              #                                                     br(),
                              #                                                     br(),
                              #                                                     h5("Stressor-Resource Categories are clusters of embayments with similar present-day stressor and resource levels.", align = "left"),
                              #                                                     br(),
                              #                                                     h5("Northeastern University conducted a Principal Component Analysis which grouped the 39 embayments into four clusters 
                              #                                                         based on analysis of combined stressor and resource data.", align = "left"),
                              #                                                     br(),
                              #                                                     h5("Red arrows generally reflect the magnitude of each variable’s contribution to the determination of clusters.", align = "left"),
                              #                                                     br(),
                              #                                                     h5("See table for embayment IDs and cluster assignments.", align = "left"),
                              #                                                     br(),
                              #                                                     h5("Choose the 'Stressors' tab above to select Stressor-Resource Categories and view their associated Driving Stressors and 
                              #                                                        Other Stressors", align = "left")
                              #                                                     ))
                              #                                                   # (column(8,
                              #                                                   #     img(src = "massbays_pca_plot.png")
                              #                                                   #     )),
                              #                                               ),
                              #                                               fluidRow(column(12,div(style="height:20px"),
                              #                                                       column(6,
                              #                                                              img(src = "massbays_pca_plot.png")
                              #                                                       ),
                              #                                                       column(6,
                              #                                                              img(src = "pca_table.png", height = '903px', width = '600px', align = "center")
                              #                                                       )
                              #                                               ))
                              #                                             ),
                              #                                      tabPanel("Stressors",
                              #                                               fluidRow(column(12,
                              #                                                               selectInput("category", label = "Northeastern Category", choices = category_choices)
                              #                                               )),
                              #                                               fluidRow(column(12,
                              #                                                 tabsetPanel(id = "category_data_tabs",
                              #                                                             tabPanel("Driving Stressors",
                              #                                                                     div(style="height:20px"),
                              #                                                                     h5("'Driving Stressors' are the variable(s) that contributed most to this Stressor-Resource Category’s 
                              #                                                                       clustering (see figure in About tab). All stressor levels have been adjusted to values between 0 and 1 
                              #                                                                       for comparison purposes.", align = "left"),
                              #                                                                     div(style="height:20px"),
                              #                                                                     plotlyOutput("driving_stressor_profile", height = "700px")
                              #                                                             ),
                              #                                                             tabPanel("Other Stressors",
                              #                                                                     div(style="height:20px"),
                              #                                                                     h5("‘Other Stressors’ are the remaining variable(s) for this Stressor-Resource Category that did not 
                              #                                                                        greatly influence clustering (see figure in About tab). All stressor levels have been adjusted to 
                              #                                                                        values between 0 and 1 for comparison purposes.", align = "left"),
                              #                                                                     div(style="height:20px"),
                              #                                                                     plotlyOutput("other_stressor_profile", height = "700px")
                              #                                                             )
                              #                                                           )
                              #                                               ))
                              #                                             )
                              #                                     )
                              #                          # tabsetPanel(id = "category_data_tabs",
                              #                          #             tabPanel("Driving Stressors",
                              #                          #                      plotlyOutput("driving_stressor_profile", height = "700px")
                              #                          #             ),
                              #                          #             tabPanel("Other Stressors",
                              #                          #                    plotlyOutput("other_stressor_profile", height = "700px")
                              #                          #             )
                              #                          # )
                              #          ))
                              #          # fluidRow(column(12,
                              #          #                 tabsetPanel(id = "category_data_tabs",
                              #          #                             tabPanel("Totals",
                              #          #                                      sidebarPanel(radioButtons("data_category_totals_displayed","Choose data option", data_displayed)),
                              #          #                                      mainPanel(plotlyOutput("category_totals_profile", height = "500px"))
                              #          #                             ),
                              #          #                             tabPanel("Seagrass",
                              #          #                                      sidebarPanel(radioButtons("data_category_seagrass_displayed","Choose data option", data_displayed)),
                              #          #                                      mainPanel(plotlyOutput("category_seagrass_profile", height = "500px"))
                              #          #                             ),
                              #          #                             tabPanel("Salt Marsh",
                              #          #                                      sidebarPanel(radioButtons("data_category_marsh_displayed","Choose data option", data_displayed)),
                              #          #                                      mainPanel(plotlyOutput("category_marsh_profile", height = "500px"))
                              #          #                             ),
                              #          #                             tabPanel("Tidal Flats",
                              #          #                                      sidebarPanel(radioButtons("data_category_flats_displayed","Choose data option", data_displayed)),
                              #          #                                      mainPanel(plotlyOutput("category_flats_profile", height = "500px"))
                              #          #                             )
                              #          #                 )
                              #          # ))
                              # ),
                              # tabPanel("MassBays Region", value = "MassBays Region",
                              #          fluidRow(column(12,
                              #                          selectInput("region", label = "MassBays Region", choices = regions_choices)
                              #          )),
                              #          fluidRow(column(12,
                              #                          tabsetPanel(id = "region_data_tabs",
                              #                                      tabPanel("Totals",
                              #                                               sidebarPanel(radioButtons("data_region_totals_displayed","Choose data option", data_displayed)),
                              #                                               mainPanel(plotlyOutput("region_totals_profile", height = "500px"))
                              #                                      ),
                              #                                      tabPanel("Seagrass",
                              #                                               sidebarPanel(radioButtons("data_region_seagrass_displayed","Choose data option", data_displayed)),
                              #                                               mainPanel(plotlyOutput("region_seagrass_profile",height = "500px"))
                              #                                      ),
                              #                                      tabPanel("Salt Marsh",
                              #                                               sidebarPanel(radioButtons("data_region_marsh_displayed","Choose data option", data_displayed)),
                              #                                               mainPanel(plotlyOutput("region_marsh_profile",height = "500px"))
                              #                                      ),
                              #                                      tabPanel("Tidal Flats",
                              #                                               sidebarPanel(radioButtons("data_region_flats_displayed","Choose data option", data_displayed)),
                              #                                               mainPanel(plotlyOutput("region_flats_profile", height = "500px"))
                              #                                      )
                              #                          )
                              #          ))
                              # ),
                              tabPanel("Embayment", value = "Embayment",
                                       fluidRow(column(12,
                                                       selectInput("embayment", label = "MassBays Embayment", choices = embayment_choices)
                                       )),
                                       fluidRow(column(12,
                                                       textOutput("bay_ecotype"),
                                                       textOutput("bay_category"),
                                                       textOutput("bay_region"),
                                       )),
                                       fluidRow(column(12,div(style="height:10px"),
                                                       tabsetPanel(id = "embayment_data_tabs",
                                                                   tabPanel("Habitat Data",
                                                                            sidebarPanel(radioButtons("data_embayment_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("embayment_profiles", height = "600px"))
                                                                   ),
                                                                   # tabPanel("Stressor Data",
                                                                   #          plotlyOutput("embayment_stressor_profiles", height = "600px")
                                                                   # ),
                                                                   tabPanel(id = "emabyment_tables","Data Tables",
                                                                            tabsetPanel(id = "embayment_data",
                                                                                        tabPanel("Tidal Flats", value = "Tidal Flats",
                                                                                                 dataTableOutput("embayment_flats_table")
                                                                                        ),
                                                                                        tabPanel("Salt Marsh", value = "Salt Marsh",
                                                                                                 dataTableOutput("embayment_marsh_table")
                                                                                        ),
                                                                                        tabPanel("Seagrass", value = "Seagrass",
                                                                                                 dataTableOutput("embayment_seagrass_table")
                                                                                        )
                                                                            )
                                                                   )
                                                       )
                                       ))
                              )
                  )
  ))
)

server <- function(input, output, session) {
  
  #############################################################################
  #################### Display Ecotype Color Defintions #######################
  
  output$definition <-renderText(
    if(input$ecotype =="SELECT ECOTYPE"){
      "Ecotype Definition:"
    }
    
    else if(input$ecotype =="Blue"){
      "Ecotype Definition: High-energy, little or no modern sediment, exposed embayments (with relatively low shallow water habitat area)"
    }
    else if(input$ecotype=="Green"){
      "Ecotype Definition: Medium- to high-energy, abundant modern sediment, exposed embayments"
    }
    else if(input$ecotype=="Orange"){
      "Ecotype Definition: Low- to medium-energy, little or no modern sediment, protected embayments"
    }
    else{
      "Ecotype Definition: Low-energy, abundant modern sediment, protected embayments"
    }
  )
  ################################################################################ 
  #################### Creating the leaflet base map #############################
  
  ########### Create base map with all of the embayment polygons ############
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>% setView(-70.5, 42.2, zoom = 8) %>% addPolygons(data=MassBaysEmbayments, weight = 1, layerId = ~NAME)
  })
  
  ########## Change map to show selected embayment polygon ################
  map_proxy=leaflet::leafletProxy("mymap")
  observeEvent(input$embayment,{
    map_proxy %>% clearGroup("highlighted_polygon")
    if(input$plot_tabs!=""){
      if(input$embayment!=""){
        selected_embayment_polygon <- subset(MassBaysEmbayments,MassBaysEmbayments$NAME==input$embayment)
      }
      map_proxy %>% leaflet::addPolygons(data=selected_embayment_polygon, weight = 2, color = "red", group = "highlighted_polygon")
    }
  })
  
  # clicking on a polygon updates selection of embayment
  observe({ 
    event <- input$mymap_shape_click
    print(event$id)
    
    updateSelectInput(session,
                      inputId = "embayment",
                      label = "MassBays Embayment",
                      choices = embayment_choices,
                      selected = event$id)
    
  })

  ######### Change map to show selected ecotype polygons ###################
  map_proxy=leaflet::leafletProxy("mymap")
  observeEvent(input$ecotype,{
    map_proxy %>% clearGroup("highlighted_polygon")
    if(input$plot_tabs=="EcoType"){
      if(input$ecotype!=""){
        selected_ecotype_polygon <- subset(MassBaysEmbayments,MassBaysEmbayments$ecotype==input$ecotype)
      }
      map_proxy %>% leaflet::addPolygons(data=selected_ecotype_polygon, weight = 2, color = "red", group = "highlighted_polygon")
    }
  })

  ######### Change map to show selected MassBays Regions polygons ###########
  map_proxy=leaflet::leafletProxy("mymap")
  observeEvent(input$region,{
    map_proxy %>% clearGroup("highlighted_polygon")
    if(input$plot_tabs=="MassBays Region"){
      if(input$region!=""){
        selected_region_polygon <- subset(MassBaysEmbayments,MassBaysEmbayments$mb_region==input$region)
      }
      map_proxy %>% leaflet::addPolygons(data=selected_region_polygon, weight = 2, color = "red", group = "highlighted_polygon")
    }
  })
  
  ######### Change map to show selected Northeastern Categories polygons #############
  map_proxy=leaflet::leafletProxy("mymap")
  observeEvent(input$category,{
    map_proxy %>% clearGroup("highlighted_polygon")
    if(input$plot_tabs=="Northeastern Category"){
      if(input$category!=""){
        selected_category_polygon <- subset(MassBaysEmbayments,MassBaysEmbayments$ne_cat==input$category)
      }
      map_proxy %>% leaflet::addPolygons(data=selected_category_polygon, weight = 2, color = "red", group = "highlighted_polygon")
    }
  })
  
  ################################################################################   
  ################ Generate Embayment data profiles and tables ###################
  
  ########## Tidal Flats data profiles #################
  
  #output$embayment_flats_profile <- renderPlot({
  #   if(input$plot_tabs!=""){
  #       if(input$embayment!=""){
  #           selected_flats_embayment <- subset(Tidy_Flats_Data,Tidy_Flats_Data$`EMBAYMENT NAME`==input$embayment)
  #           validate(
  #               need(selected_flats_embayment$FlatsAcreage!="", "NO TIDAL FLAT DATA AVAILABLE")
  #           )
  #           ggplot(selected_flats_embayment,aes(x = Year, y = FlatsAcreage))+
  #               geom_line(aes(group=1))+
  #               geom_point()+
  #               theme(legend.position="none")+
  #               ylab("Acres")+
  #               ggtitle("Tidal Flats")
  #       }
  #   }
  #})
  
  ########## Salt Marsh data profiles #################
  
  #output$embayment_marsh_profile <- renderPlot({
  #    if(input$plot_tabs!=""){
  #        if(input$embayment!=""){
  #            selected_marsh_embayment <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$`EMBAYMENT NAME`==input$embayment)
  #            validate(
  #                need(selected_marsh_embayment$MarshAcreage!="", "NO SALT MARSH DATA AVAILABLE")
  #            )
  #            ggplot(selected_marsh_embayment,aes(x = Year, y = MarshAcreage))+
  #                geom_line(aes(group=1))+
  #                geom_point()+
  #                theme(legend.position="none")+
  #                ylab("Acres")+
  #               ggtitle("Salt Marsh")
  #        }
  #    }
  #})
  
  
  #### Providing Embayment Info ####
  output$bay_ecotype <-renderText({
      if(input$embayment!=""){
        if(input$embayment=="SELECT EMBAYMENT"){
          "Ecotype: NA"
        }
        else{
          selected_flats_embayment <- subset(Tidy_Flats_Data,Tidy_Flats_Data$`EMBAYMENT NAME`==input$embayment)
          if(selected_flats_embayment$EcoType[1]=="Blue"){
            "Ecotype: High-energy, little or no modern sediment, exposed embayments (with relatively low shallow water habitat area)"
          }
          else if(selected_flats_embayment$EcoType[1]=="Green"){
            "Ecotype: Medium- to high-energy, abundant modern sediment, exposed embayments"
          }
          else if(selected_flats_embayment$EcoType[1]=="Orange"){
            "Ecotype: Low- to medium-energy, little or no modern sediment, protected embayments"
          }
          else{
            "Ecotype: Low-energy, abundant modern sediment, protected embayments"
          }
        }
      }
    })
    
  output$bay_category <-renderText({
    if(input$embayment!=""){
      selected_flats_embayment <- subset(Tidy_Flats_Data,Tidy_Flats_Data$`EMBAYMENT NAME`==input$embayment)
      paste("Stressor-Resource Category: ", selected_flats_embayment$Northeastern_category[1])
      }
    })  
  
    output$bay_region <-renderText({
      if(input$embayment!=""){
        selected_flats_embayment <- subset(Tidy_Flats_Data,Tidy_Flats_Data$`EMBAYMENT NAME`==input$embayment)
      paste("MassBays Region: ", selected_flats_embayment$`MassBays Region`[1])
      }
    })
  
  ###### Generating Embayment Profiles with all habitats on one graph ######    
  output$embayment_profiles <- renderPlotly({
    if(input$plot_tabs!=""){
      if(input$embayment!=""){
        selected_flats_embayment <- subset(Tidy_Flats_Data,Tidy_Flats_Data$`EMBAYMENT NAME`==input$embayment)
        selected_flats_embayment$percent_remaining<-(NA)
        for(i in 1:nrow(selected_flats_embayment)) {
          selected_flats_embayment$percent_remaining[i]<-(((selected_flats_embayment$FlatsAcreage)[i])/((selected_flats_embayment$FlatsAcreage)[1])*100)
        }
        validate(
          need(selected_flats_embayment$FlatsAcreage!="", "NO TIDAL FLAT DATA AVAILABLE")
        )
        colnames(selected_flats_embayment)[colnames(selected_flats_embayment) == "FlatsAcreage"] <- "Acreage"
        selected_marsh_embayment <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$`EMBAYMENT NAME`==input$embayment)
        selected_marsh_embayment$percent_remaining<-(NA)
        for(i in 1:nrow(selected_marsh_embayment)) {
          selected_marsh_embayment$percent_remaining[i]<-(((selected_marsh_embayment$MarshAcreage)[i])/((selected_marsh_embayment$MarshAcreage)[1])*100)
        }
        validate(
          need(selected_marsh_embayment$MarshAcreage!="", "NO SALT MARSH DATA AVAILABLE")
        )
        colnames(selected_marsh_embayment)[colnames(selected_marsh_embayment) == "MarshAcreage"] <- "Acreage"
        selected_seagrass_embayment <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$`EMBAYMENT NAME`==input$embayment)
        selected_seagrass_embayment$percent_remaining<-(NA)
        for(i in 1:nrow(selected_seagrass_embayment)) {
          selected_seagrass_embayment$percent_remaining[i]<-(((selected_seagrass_embayment$SeagrassAcreage)[i])/((selected_seagrass_embayment$SeagrassAcreage)[1])*100)
        }
        validate(
          need(selected_seagrass_embayment$SeagrassAcreage!="", "NO SEAGRASS DATA AVAILABLE")
        )
        colnames(selected_seagrass_embayment)[colnames(selected_seagrass_embayment) == "SeagrassAcreage"] <- "Acreage"
        flats_marsh_embayment_data<-merge(selected_flats_embayment, selected_marsh_embayment, all = TRUE)
        all_selected_embayment_data<-merge(flats_marsh_embayment_data,selected_seagrass_embayment, all = TRUE)
        all_selected_embayment_data$Year <- as.numeric(as.character(all_selected_embayment_data$Year))
        
        if(input$data_embayment_displayed=="Percent Remaining"){
          e <-ggplot(all_selected_embayment_data,aes(x = Year, y = percent_remaining, color = Habitat,
                                                     text = (paste("Year: ",Year, "\n",
                                                                   "Percent remaining: ",percent_remaining, "\n"))
                                                     ))+
            geom_line(aes(group=Habitat))+
            geom_point()+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            #theme(legend.position="none")+
            ylab("Percent Remaining")
          ggplotly(e, tooltip = "text") %>%
            rangeslider(1760, 2040, thickness=0.01)
        }
        else{
          e <-ggplot(all_selected_embayment_data,aes(x = Year, y = Acreage, color = Habitat,
                                                     text = (paste("Year: ",Year, "\n",
                                                                   "Acres: ",Acreage, "\n"))
                                                     ))+
            geom_line(aes(group=Habitat))+
            geom_point()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            #theme(legend.position="none")+
            ylab("Acres")
          ggplotly(e, tooltip = "text") %>%
            rangeslider(1760, 2040, thickness=0.01)
        }
      }
    }
  })
  
  ####### Generate individual embayment stressor plots ###############
  # output$embayment_stressor_profiles <- renderPlotly({
  #   if(input$plot_tabs!=""){
  #     if(input$embayment!=""){
  #       selected_embayment_stressor <- subset(Tidy_Stressor_Data_norm,Tidy_Stressor_Data_norm$`EMBAYMENT NAME`==input$embayment)
  #       zz<-ggplot(selected_embayment_stressor,aes(x = Stressor, y = Stressor_Value, fill = Stressor, text = (paste("Stressor Value: ", Stressor_Value))))+
  #         geom_col()+
  #         theme(legend.position="none")+
  #         ylab("Relative Stressor Value")
  #       ggplotly(zz, tooltip = "text")
  #     }
  #   }
  # })
  
  ####### Generate individual embayment data tables #################
  output$embayment_flats_table <- renderDataTable({
    if(input$plot_tabs!=""){
      if(input$embayment!=""){
        selected_flats_embayment_table <- subset(Tidy_Flats_Data,Tidy_Flats_Data$`EMBAYMENT NAME`==input$embayment)
      }
      selected_flats_embayment_table
    }
  })
  output$embayment_marsh_table <- renderDataTable({
    if(input$plot_tabs!=""){
      if(input$embayment!=""){
        selected_marsh_embayment_table <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$`EMBAYMENT NAME`==input$embayment)
      }
      selected_marsh_embayment_table 
    }
  })
  
  output$embayment_seagrass_table <- renderDataTable({
    if(input$plot_tabs!=""){
      if(input$embayment!=""){
        selected_seagrass_embayment_table <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$`EMBAYMENT NAME`==input$embayment)
      }
      selected_seagrass_embayment_table
    }
  })
  
  ################################################################################   
  ################# Generate Ecotype data profiles and tables ####################
  
  ######### Ecotype: Totals data profiles ###################
  
  output$ecotype_totals_profile <- renderPlotly({
    if(input$plot_tabs=="EcoType"){
      if(input$ecotype_data_tabs=="Totals"){
        if(input$ecotype!=""){
          ###Seagrass###
          selected_ecotype_seagrass <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$EcoType==input$ecotype)
          validate(
            need(selected_ecotype_seagrass$SeagrassAcreage!="", "NO SEAGRASS DATA AVAILABLE")
          )
          selected_ecotype_seagrass <- aggregate(selected_ecotype_seagrass$SeagrassAcreage, by=list(Category=selected_ecotype_seagrass$Year), FUN=sum)
          colnames(selected_ecotype_seagrass)[colnames(selected_ecotype_seagrass) == "Category"] <- "Year"
          colnames(selected_ecotype_seagrass)[colnames(selected_ecotype_seagrass) == "x"] <- "TotalAcreage"
          for(i in 1:nrow(selected_ecotype_seagrass)) {
            selected_ecotype_seagrass$percent_remaining[i]<-(((selected_ecotype_seagrass$TotalAcreage)[i])/((selected_ecotype_seagrass$TotalAcreage)[1])*100)
          }
          selected_ecotype_seagrass$Year <- as.numeric(as.character(selected_ecotype_seagrass$Year))
          selected_ecotype_seagrass$Habitat<-(NA)
          for(i in 1:nrow(selected_ecotype_seagrass)) {
            selected_ecotype_seagrass$Habitat[i]<-("Seagrass")
          }
          ###Salt Marsh###
          selected_ecotype_marsh <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$EcoType==input$ecotype)
          validate(
            need(selected_ecotype_marsh$MarshAcreage!="", "NO marsh DATA AVAILABLE")
          )
          selected_ecotype_marsh <- aggregate(selected_ecotype_marsh$MarshAcreage, by=list(Category=selected_ecotype_marsh$Year), FUN=sum)
          colnames(selected_ecotype_marsh)[colnames(selected_ecotype_marsh) == "Category"] <- "Year"
          colnames(selected_ecotype_marsh)[colnames(selected_ecotype_marsh) == "x"] <- "TotalAcreage"
          for(i in 1:nrow(selected_ecotype_marsh)) {
            selected_ecotype_marsh$percent_remaining[i]<-(((selected_ecotype_marsh$TotalAcreage)[i])/((selected_ecotype_marsh$TotalAcreage)[1])*100)
          }
          selected_ecotype_marsh$Year <- as.numeric(as.character(selected_ecotype_marsh$Year))
          selected_ecotype_marsh$Habitat<-(NA)
          for(i in 1:nrow(selected_ecotype_marsh)) {
            selected_ecotype_marsh$Habitat[i]<-("Salt Marsh")
          }
          ###Tidal Flats###
          selected_ecotype_flats <- subset(Tidy_Flats_Data,Tidy_Flats_Data$EcoType==input$ecotype)
          validate(
            need(selected_ecotype_flats$FlatsAcreage!="", "NO flats DATA AVAILABLE")
          )
          selected_ecotype_flats <- aggregate(selected_ecotype_flats$FlatsAcreage, by=list(Category=selected_ecotype_flats$Year), FUN=sum)
          colnames(selected_ecotype_flats)[colnames(selected_ecotype_flats) == "Category"] <- "Year"
          colnames(selected_ecotype_flats)[colnames(selected_ecotype_flats) == "x"] <- "TotalAcreage"
          for(i in 1:nrow(selected_ecotype_flats)) {
            selected_ecotype_flats$percent_remaining[i]<-(((selected_ecotype_flats$TotalAcreage)[i])/((selected_ecotype_flats$TotalAcreage)[1])*100)
          }
          selected_ecotype_flats$Year <- as.numeric(as.character(selected_ecotype_flats$Year))
          selected_ecotype_flats$Habitat<-(NA)
          for(i in 1:nrow(selected_ecotype_flats)) {
            selected_ecotype_flats$Habitat[i]<-("Tidal Flats")
          }   
          ecotype_totals1 <- merge(selected_ecotype_seagrass,selected_ecotype_marsh, all = TRUE)
          ecotype_totals <- merge(ecotype_totals1,selected_ecotype_flats, all = TRUE)
          
          if(input$data_ecotype_totals_displayed=="Percent Remaining"){
            d <-ggplot(ecotype_totals,aes(x = Year, y = percent_remaining, color = Habitat,
                                          text = (paste("Year: ",Year, "\n",
                                                        "Percent remaining: ",percent_remaining, "\n"))
                                          ))+
              geom_line(aes(group=Habitat), linetype = 2)+
              geom_point(shape = ifelse(ecotype_totals$Year == 2050, 9, 1), size = 2.5)+
              geom_line(data=subset(ecotype_totals, Year < 2050),aes(group = Habitat, color = Habitat))+
              scale_y_log10()+
              scale_x_continuous(breaks = seq(1760, 2060, by = 20))+
              #theme(legend.position="none")+
              ylab("Percent Remaining")
            ggplotly(d, tooltip = "text") %>%
              rangeslider(1760, 2060, thickness=0.01)
          }
          else{
            d <-ggplot(ecotype_totals,aes(x = Year, y = TotalAcreage, color = Habitat,
                                          text = (paste("Year: ",Year, "\n",
                                                        "Acres: ",TotalAcreage, "\n"))
                                          ))+
              geom_line(aes(group=Habitat), linetype = 2)+
              geom_point(shape = ifelse(ecotype_totals$Year == 2050, 9, 1), size = 2.5)+
              geom_line(data=subset(ecotype_totals, Year < 2050),aes(group = Habitat, color = Habitat))+
              scale_x_continuous(breaks = seq(1760, 2060, by = 20))+
              #theme(legend.position="none")+
              ylab("Acres")
            ggplotly(d, tooltip = "text") %>%
              rangeslider(1760, 2060, thickness=0.01)
          }
        }
      }
    }
  })
  
  ######### Ecotype: Seagrass data profiles #################         
  
  output$ecotype_seagrass_profile <- renderPlotly({
    if(input$plot_tabs=="EcoType"){
      if(input$ecotype!=""){
        selected_seagrass_ecotype <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$EcoType==input$ecotype)
        validate(
          need(selected_seagrass_ecotype$SeagrassAcreage!="", "NO SEAGRASS DATA AVAILABLE")
        )
        selected_seagrass_ecotype$Year <- as.numeric(as.character(selected_seagrass_ecotype$Year))
        seagrass_ecotype_per_rem<-selected_seagrass_ecotype %>%
          group_by(`EMBAYMENT NAME`) %>%
          select(SeagrassAcreage, Year, `EMBAYMENT NAME`, Northeastern_category, `MassBays Region`) %>%
          mutate(percent_remaining = (SeagrassAcreage/(SeagrassAcreage[nrow=1]))*100)
        if(input$data_ecotype_seagrass_displayed=="Percent Remaining"){
          c <-ggplot(seagrass_ecotype_per_rem,aes(x = Year, y = percent_remaining, 
                                                text = (paste("Year: ",Year, "\n",
                                                "Percent remaining: ",percent_remaining, "\n",
                                                "Embayment: ",`EMBAYMENT NAME`, "\n",
                                                "Stressor-Resource Category: ",Northeastern_category, "\n",
                                                "MassBays Region: ",`MassBays Region`))
                                                ))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`), linetype = 2)+ # linetype = 2 makes the line dashed
            geom_point(shape = ifelse(seagrass_ecotype_per_rem$Year == 2050, 9, 1), size = 2.5)+ # make a different symbol for the 2050 data point
            geom_line(data=subset(seagrass_ecotype_per_rem, Year <= 2017),aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+ # plots the interval up to 2017 as a solid line over the dashed line for the complete record
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2060, by = 20))+
            theme(legend.position="none")+
            ylab("Percent Remaining")+
            ggtitle("Displaying all embayments within this ecotype")
          ggplotly(c, tooltip = "text") %>%
            rangeslider(1760, 2060, thickness=0.01)
        }
        else{
          c <-ggplot(seagrass_ecotype_per_rem,aes(x = Year, y = SeagrassAcreage, 
                                                  text = (paste("Year: ",Year, "\n",
                                                                "Acres: ",SeagrassAcreage, "\n",
                                                                "Embayment: ",`EMBAYMENT NAME`, "\n",
                                                                "Stressor-Resource Category: ",Northeastern_category, "\n",
                                                                "MassBays Region: ",`MassBays Region`))
                                                  ))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`), linetype = 2)+
            #geom_point()+
            geom_point(shape = ifelse(seagrass_ecotype_per_rem$Year == 2050, 9, 1), size = 2.5)+
            geom_line(data=subset(seagrass_ecotype_per_rem, Year <= 2017),aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            scale_x_continuous(breaks = seq(1760, 2060, by = 20))+
            theme(legend.position="none")+
            ylab("Acres")+
            ggtitle("Displaying all embayments within this ecotype")
          ggplotly(c, tooltip = "text") %>%
            rangeslider(1760, 2060, thickness=0.01)
        }
      }
    }
  })
  
  ######### Ecotype: Salt Marsh data profiles #################         
  
  output$ecotype_marsh_profile <- renderPlotly({
    if(input$plot_tabs=="EcoType"){
      if(input$ecotype!=""){
        selected_marsh_ecotype <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$EcoType==input$ecotype)
        validate(
          need(selected_marsh_ecotype$MarshAcreage!="", "NO SALT MARSH DATA AVAILABLE")
        )
        selected_marsh_ecotype$Year <- as.numeric(as.character(selected_marsh_ecotype$Year))
        marsh_ecotype_per_rem<-selected_marsh_ecotype %>%
          group_by(`EMBAYMENT NAME`) %>%
          select(MarshAcreage, Year, `EMBAYMENT NAME`, Northeastern_category, `MassBays Region`) %>%
          mutate(percent_remaining = (MarshAcreage/(MarshAcreage[nrow=1]))*100)
        if(input$data_ecotype_marsh_displayed=="Percent Remaining"){
          b <-ggplot(marsh_ecotype_per_rem,aes(x = Year, y = percent_remaining, 
                                               text = (paste("Year: ",Year, "\n",
                                                            "Percent remaining: ",percent_remaining, "\n",
                                                            "Embayment: ",`EMBAYMENT NAME`, "\n",
                                                            "Stressor-Resource Category: ",Northeastern_category, "\n",
                                                            "MassBays Region: ",`MassBays Region`))
                                               ))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`), linetype = 2)+
            geom_point(shape = ifelse(marsh_ecotype_per_rem$Year == 2050, 9, 1), size = 2.5)+
            geom_line(data=subset(marsh_ecotype_per_rem, Year <= 2005),aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2060, by = 20))+
            theme(legend.position="none")+
            ylab("Percent Remaining")+
            ggtitle("Displaying all embayments within this ecotype")
          ggplotly(b, tooltip = "text") %>%
            rangeslider(1760, 2060, thickness=0.01)
        }
        else{
          b <-ggplot(marsh_ecotype_per_rem,aes(x = Year, y = MarshAcreage, 
                                               text = (paste("Year: ",Year, "\n",
                                                             "Acres: ",MarshAcreage, "\n",
                                                             "Embayment: ",`EMBAYMENT NAME`, "\n",
                                                             "Stressor-Resource Category: ",Northeastern_category, "\n",
                                                             "MassBays Region: ",`MassBays Region`))
                                              ))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`), linetype = 2)+
            geom_point(shape = ifelse(marsh_ecotype_per_rem$Year == 2050, 9, 1), size = 2.5)+
            geom_line(data=subset(marsh_ecotype_per_rem, Year <= 2005),aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            scale_x_continuous(breaks = seq(1760, 2060, by = 20))+
            theme(legend.position="none")+
            ylab("Acres")+
            ggtitle("Displaying all embayments within this ecotype")
          ggplotly(b, tooltip = "text") %>%
            rangeslider(1760, 2060, thickness=0.01)
        }
      }
    }
  })
  
  ######### Ecotype: Tidal Flats data profiles #################      
  
  output$ecotype_flats_profile <- renderPlotly({
    if(input$plot_tabs=="EcoType"){
      if(input$ecotype!=""){
        selected_flats_ecotype <- subset(Tidy_Flats_Data,Tidy_Flats_Data$EcoType==input$ecotype)
        validate(
          need(selected_flats_ecotype$FlatsAcreage!="", "NO TIDAL FLAT DATA AVAILABLE")
        )
        selected_flats_ecotype$Year <- as.numeric(as.character(selected_flats_ecotype$Year))
        flats_ecotype_per_rem<-selected_flats_ecotype %>%
          group_by(`EMBAYMENT NAME`) %>%
          select(FlatsAcreage, Year, `EMBAYMENT NAME`, Northeastern_category, `MassBays Region`) %>%
          mutate(percent_remaining = (FlatsAcreage/(FlatsAcreage[nrow=1]))*100)
        if(input$data_ecotype_flats_displayed=="Percent Remaining"){
          a <-ggplot(flats_ecotype_per_rem,aes(x = Year, y = percent_remaining, 
                                               text = (paste("Year: ",Year, "\n",
                                                             "Percent remaining: ",percent_remaining, "\n",
                                                             "Embayment: ",`EMBAYMENT NAME`, "\n",
                                                             "Stressor-Resource Category: ",Northeastern_category, "\n",
                                                             "MassBays Region: ",`MassBays Region`))
                                              ))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`), linetype = 2)+
            geom_point(shape = ifelse(flats_ecotype_per_rem$Year == 2050, 9, 1), size = 2.5)+
            geom_line(data=subset(flats_ecotype_per_rem, Year <= 2005),aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2060, by = 20))+
            theme(legend.position="none")+
            ylab("Percent Remaining")+
            ggtitle("Displaying all embayments within this ecotype")
          ggplotly(a, tooltip = "text") %>%
            rangeslider(1760, 2060, thickness=0.01)
        }
        else{
          a <-ggplot(flats_ecotype_per_rem,aes(x = Year, y = FlatsAcreage, 
                                               text = (paste("Year: ",Year, "\n",
                                                             "Acres: ",FlatsAcreage, "\n",
                                                             "Embayment: ",`EMBAYMENT NAME`, "\n",
                                                             "Stressor-Resource Category: ",Northeastern_category, "\n",
                                                             "MassBays Region: ",`MassBays Region`))
                                              ))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`), linetype = 2)+
            geom_point(shape = ifelse(flats_ecotype_per_rem$Year == 2050, 9, 1), size = 2.5)+
            geom_line(data=subset(flats_ecotype_per_rem, Year <= 2005),aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            scale_x_continuous(breaks = seq(1760, 2060, by = 20))+
            theme(legend.position="none")+
            ylab("Acres")+
            ggtitle("Displaying all embayments within this ecotype")
          ggplotly(a, tooltip = "text") %>%
            rangeslider(1760, 2060, thickness=0.01)
        }
      }
    }
  })
  
  ########## Generate ecotype data tables ###################
  #        output$ecotype_flats_table <- renderDataTable({
  #            if(input$plot_tabs=="EcoType"){
  #                if(input$ecotype_data!=""){
  #                    if(input$ecotype!=""){
  #                        selected_flats_ecotype_table <- subset(Tidy_Flats_Data,Tidy_Flats_Data$EcoType==input$ecotype)
  #                    }
  #                selected_flats_ecotype_table
  #                }
  #            }
  #        })
  #        output$ecotype_marsh_table <- renderDataTable({
  #            if(input$plot_tabs=="EcoType"){
  #                if(input$ecotype_data!=""){
  #                    if(input$ecotype!=""){
  #                        selected_marsh_ecotype_table <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$EcoType==input$ecotype)
  #                    }
  #                selected_marsh_ecotype_table
  #                }
  #            }
  #        })
  #        
  #        output$ecotype_seagrass_table <- renderDataTable({
  #            if(input$plot_tabs=="EcoType"){
  #                if(input$ecotype_data!=""){
  #                    if(input$ecotype!=""){
  #                        selected_seagrass_ecotype_table <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$EcoType==input$ecotype)
  #                    }
  #                selected_seagrass_ecotype_table
  #                }
  #            }
  #        })
  #
  
  ################################################################################   
  ########## Generate Northeastern Category Stressor Profiles #############

  # output$driving_stressor_profile <- renderPlotly({
  #   if(input$main_category_tabs=="Stressors"){
  #   if(input$category!=""){
  #      if(input$category_data_tabs=="Driving Stressors"){
  #        selected_category_stressor <- subset(Tidy_Stressor_Data_norm,Tidy_Stressor_Data_norm$Northeastern_category==input$category)
  #             validate(
  #             need(selected_category_stressor$Stressor_Value!="", "NO DATA AVAILABLE"))
  #         filtered_stressor <-selected_category_stressor %>%
  #           filter(
  #             if(input$category=="1"){
  #               Stressor == "unhard.std" | Stressor == "hard.of.hard"| Stressor == "tidal.flushing"
  #             }
  #             else if(input$category=="2"){
  #               Stressor == "percent.septic"
  #             }
  #             else if(input$category=="3"){
  #               Stressor == "bacteria" | Stressor == "hard.of.hard" | Stressor == "tidal.restrict"
  #             }
  #             else{
  #               Stressor == "bacteria" | Stressor == "tidal.restrict"
  #             }
  #           )
  #        xx<-ggplot(filtered_stressor,aes(x = `EMBAYMENT NAME`, y = Stressor_Value, fill = `EMBAYMENT NAME`,
  #                                         text = (paste("Embayment: ",`EMBAYMENT NAME`))))+
  #       geom_col(position = "dodge")+
  #       facet_grid(.~Stressor)+
  #       #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  #       theme(axis.title.x=element_blank(),
  #             axis.text.x=element_blank(),
  #             axis.ticks.x=element_blank())+
  #       theme(legend.position="none")+
  #       ylab("Relative Stressor Value")
  #     ggplotly(xx, tooltip = "text")
  #      }
  #   }
  #   }
  # })
  #     
  # output$other_stressor_profile <- renderPlotly({
  #   if(input$category!=""){
  #     if(input$category_data_tabs=="Other Stressors"){
  #       selected_category_stressor <- subset(Tidy_Stressor_Data_norm,Tidy_Stressor_Data_norm$Northeastern_category==input$category)
  #       validate(
  #         need(selected_category_stressor$Stressor_Value!="", "NO DATA AVAILABLE"))
  #       filtered_stressor <-selected_category_stressor %>%
  #         filter(
  #           if(input$category=="1"){
  #             Stressor == "unhardenable.uncorrected" | Stressor == "total.shoreline" |
  #               Stressor == "high.intensity" | Stressor == "stormwater" | Stressor == "StormwaterSTD" | 
  #               Stressor == "pop.density" | Stressor == "percent.septic" | Stressor == "septic.acre" | 
  #               Stressor == "nutrient" | Stressor == "bacteria" | Stressor == "tidal.restrict" 
  #           }
  #           else if(input$category=="2"){
  #             Stressor == "tidal.flushing" | Stressor == "unhardenable.uncorrected" | Stressor == "total.shoreline" |
  #               Stressor == "unhard.std" | Stressor == "hard.of.hard" | Stressor == "high.intensity" | Stressor == "stormwater" |
  #               Stressor == "StormwaterSTD" | Stressor == "pop.density" | Stressor == "septic.acre" | Stressor == "nutrient" | 
  #               Stressor == "bacteria" | Stressor == "tidal.restrict"
  #           }
  #           else if(input$category=="3"){
  #             Stressor == "tidal.flushing" | Stressor == "unhardenable.uncorrected" | Stressor == "total.shoreline" |
  #               Stressor == "unhard.std" | Stressor == "high.intensity" | Stressor == "stormwater" |
  #               Stressor == "StormwaterSTD" | Stressor == "pop.density" | Stressor == "percent.septic" |
  #               Stressor == "septic.acre" | Stressor == "nutrient"
  #           }
  #           else{
  #             Stressor == "tidal.flushing" | Stressor == "unhardenable.uncorrected" | Stressor == "total.shoreline" |
  #               Stressor == "unhard.std" | Stressor == "hard.of.hard" | Stressor == "high.intensity" | Stressor == "stormwater" |
  #               Stressor == "StormwaterSTD" | Stressor == "pop.density" | Stressor == "percent.septic" |
  #               Stressor == "septic.acre" | Stressor == "nutrient"
  #           }
  #         )
  #       yy<-ggplot(filtered_stressor,aes(x = `EMBAYMENT NAME`, y = Stressor_Value, fill = `EMBAYMENT NAME`,
  #                                        text = (paste("Embayment: ",`EMBAYMENT NAME`))))+
  #         geom_col(position = "dodge")+
  #         facet_grid(.~Stressor)+
  #         #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  #         theme(axis.title.x=element_blank(),
  #               axis.text.x=element_blank(),
  #               axis.ticks.x=element_blank())+
  #         theme(legend.position="none")+
  #         ylab("Relative Stressor Value")
  #       ggplotly(yy, tooltip = "text")
  #       }
  #     }
  #   })

  
  ################################################################################   
  ########## Generate Northeastern Category data profiles and tables #############
  
  # ############ Northeastern Category: Totals data profiles ###################
  # 
  # output$category_totals_profile <- renderPlotly({
  #   if(input$plot_tabs=="Northeastern Category"){
  #     if(input$category_data_tabs=="Totals"){
  #       if(input$category!=""){
  #         ###Seagrass###
  #         selected_category_seagrass <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$Northeastern_category==input$category)
  #         validate(
  #           need(selected_category_seagrass$SeagrassAcreage!="", "NO SEAGRASS DATA AVAILABLE")
  #         )
  #         selected_category_seagrass <- aggregate(selected_category_seagrass$SeagrassAcreage, by=list(Category=selected_category_seagrass$Year), FUN=sum)
  #         colnames(selected_category_seagrass)[colnames(selected_category_seagrass) == "Category"] <- "Year"
  #         colnames(selected_category_seagrass)[colnames(selected_category_seagrass) == "x"] <- "TotalAcreage"
  #         for(i in 1:nrow(selected_category_seagrass)) {
  #           selected_category_seagrass$percent_remaining[i]<-(((selected_category_seagrass$TotalAcreage)[i])/((selected_category_seagrass$TotalAcreage)[1])*100)
  #         }
  #         selected_category_seagrass$Year <- as.numeric(as.character(selected_category_seagrass$Year))
  #         selected_category_seagrass$Habitat<-(NA)
  #         for(i in 1:nrow(selected_category_seagrass)) {
  #           selected_category_seagrass$Habitat[i]<-("Seagrass")
  #         }
  #         ###Salt Marsh###
  #         selected_category_marsh <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$Northeastern_category==input$category)
  #         validate(
  #           need(selected_category_marsh$MarshAcreage!="", "NO marsh DATA AVAILABLE")
  #         )
  #         selected_category_marsh <- aggregate(selected_category_marsh$MarshAcreage, by=list(Category=selected_category_marsh$Year), FUN=sum)
  #         colnames(selected_category_marsh)[colnames(selected_category_marsh) == "Category"] <- "Year"
  #         colnames(selected_category_marsh)[colnames(selected_category_marsh) == "x"] <- "TotalAcreage"
  #         for(i in 1:nrow(selected_category_marsh)) {
  #           selected_category_marsh$percent_remaining[i]<-(((selected_category_marsh$TotalAcreage)[i])/((selected_category_marsh$TotalAcreage)[1])*100)
  #         }
  #         selected_category_marsh$Year <- as.numeric(as.character(selected_category_marsh$Year))
  #         selected_category_marsh$Habitat<-(NA)
  #         for(i in 1:nrow(selected_category_marsh)) {
  #           selected_category_marsh$Habitat[i]<-("Salt Marsh")
  #         }
  #         ###Tidal Flats###
  #         selected_category_flats <- subset(Tidy_Flats_Data,Tidy_Flats_Data$Northeastern_category==input$category)
  #         validate(
  #           need(selected_category_flats$FlatsAcreage!="", "NO flats DATA AVAILABLE")
  #         )
  #         selected_category_flats <- aggregate(selected_category_flats$FlatsAcreage, by=list(Category=selected_category_flats$Year), FUN=sum)
  #         colnames(selected_category_flats)[colnames(selected_category_flats) == "Category"] <- "Year"
  #         colnames(selected_category_flats)[colnames(selected_category_flats) == "x"] <- "TotalAcreage"
  #         for(i in 1:nrow(selected_category_flats)) {
  #           selected_category_flats$percent_remaining[i]<-(((selected_category_flats$TotalAcreage)[i])/((selected_category_flats$TotalAcreage)[1])*100)
  #         }
  #         selected_category_flats$Year <- as.numeric(as.character(selected_category_flats$Year))
  #         selected_category_flats$Habitat<-(NA)
  #         for(i in 1:nrow(selected_category_flats)) {
  #           selected_category_flats$Habitat[i]<-("Tidal Flats")
  #         }   
  #         category_totals1 <- merge(selected_category_seagrass,selected_category_marsh, all = TRUE)
  #         category_totals <- merge(category_totals1,selected_category_flats, all = TRUE)
  #         
  #         if(input$data_category_totals_displayed=="Percent Remaining"){
  #           w <-ggplot(category_totals,aes(x = Year, y = percent_remaining, color = Habitat, 
  #                                          text = (paste("Year: ",Year, "\n",
  #                                                        "Percent remaining: ",percent_remaining, "\n"))
  #                                         ))+
  #             geom_line(aes(group=Habitat))+
  #             geom_point()+
  #             scale_y_log10()+
  #             scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #             #theme(legend.position="none")+
  #             ylab("Percent Remaining")
  #           ggplotly(w, tooltip = "text") %>%
  #             rangeslider(1760, 2040, thickness=0.01)
  #         }
  #         else{
  #           w <-ggplot(category_totals,aes(x = Year, y = TotalAcreage, color = Habitat, 
  #                                          text = (paste("Year: ",Year, "\n",
  #                                                        "Acres: ",TotalAcreage, "\n"))
  #           ))+
  #             geom_line(aes(group=Habitat))+
  #             geom_point()+
  #             scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #             #theme(legend.position="none")+
  #             ylab("Acres")
  #           ggplotly(w, tooltip = "text") %>%
  #             rangeslider(1760, 2040, thickness=0.01)
  #         }
  #       }
  #     }
  #   }
  # })
  # 
  # ######### Northeastern Category: Seagrass data profiles #################         
  # 
  # output$category_seagrass_profile <- renderPlotly({
  #   if(input$plot_tabs=="Northeastern Category"){
  #     if(input$category!=""){
  #       selected_seagrass_category <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$Northeastern_category==input$category)
  #       validate(
  #         need(selected_seagrass_category$SeagrassAcreage!="", "NO SEAGRASS DATA AVAILABLE")
  #       )
  #       selected_seagrass_category$Year <- as.numeric(as.character(selected_seagrass_category$Year))
  #       seagrass_category_per_rem<-selected_seagrass_category %>%
  #         group_by(`EMBAYMENT NAME`) %>%
  #         select(SeagrassAcreage, Year, `EMBAYMENT NAME`, Northeastern_category, `MassBays Region`) %>%
  #         mutate(percent_remaining = (SeagrassAcreage/(SeagrassAcreage[nrow=1]))*100)
  #       if(input$data_category_seagrass_displayed=="Percent Remaining"){
  #         v <-ggplot(seagrass_category_per_rem,aes(x = Year, y = percent_remaining, 
  #                                                  text = (paste("Year: ",Year, "\n",
  #                                                                "Percent Remaining: ",percent_remaining, "\n",
  #                                                                "Embayment: ",`EMBAYMENT NAME`, "\n",
  #                                                                "Stressor-Resource Category: ",Northeastern_category, "\n",
  #                                                                "MassBays Region: ",`MassBays Region`))
  #                                                 ))+
  #           geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
  #           geom_point()+
  #           scale_y_log10()+
  #           scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #           theme(legend.position="none")+
  #           ylab("Percent Remaining")+
  #           ggtitle("Displaying all embayments within this category")
  #         ggplotly(v, tooltip = "text") %>%
  #           rangeslider(1760, 2040, thickness=0.01)
  #       }
  #       else{
  #         v <-ggplot(seagrass_category_per_rem,aes(x = Year, y = SeagrassAcreage, 
  #                                                  text = (paste("Year: ",Year, "\n",
  #                                                                "Acres: ",SeagrassAcreage, "\n",
  #                                                                "Embayment: ",`EMBAYMENT NAME`, "\n",
  #                                                                "Stressor-Resource Category: ",Northeastern_category, "\n",
  #                                                                "MassBays Region: ",`MassBays Region`))
  #                                                 ))+
  #           geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
  #           geom_point()+
  #           scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #           theme(legend.position="none")+
  #           ylab("Acres")+
  #           ggtitle("Displaying all embayments within this category")
  #         ggplotly(v, tooltip = "text") %>%
  #           rangeslider(1760, 2040, thickness=0.01)
  #       }
  #     }
  #   }
  # })
  # 
  # ############# Northeastern Category: Salt Marsh data profiles #################       
  # 
  # output$category_marsh_profile <- renderPlotly({
  #   if(input$plot_tabs=="Northeastern Category"){
  #     if(input$category!=""){
  #       selected_marsh_category <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$Northeastern_category==input$category)
  #       validate(
  #         need(selected_marsh_category$MarshAcreage!="", "NO SALT MARSH DATA AVAILABLE")
  #       )
  #       selected_marsh_category$Year <- as.numeric(as.character(selected_marsh_category$Year))
  #       marsh_category_per_rem<-selected_marsh_category %>%
  #         group_by(`EMBAYMENT NAME`) %>%
  #         select(MarshAcreage, Year, `EMBAYMENT NAME`, Northeastern_category, `MassBays Region`) %>%
  #         mutate(percent_remaining = (MarshAcreage/(MarshAcreage[nrow=1]))*100)
  #       if(input$data_category_marsh_displayed=="Percent Remaining"){
  #         u <-ggplot(marsh_category_per_rem,aes(x = Year, y = percent_remaining, 
  #                                               text = (paste("Year: ",Year, "\n",
  #                                                             "Percent Remaining: ",percent_remaining, "\n",
  #                                                             "Embayment: ",`EMBAYMENT NAME`, "\n",
  #                                                             "Stressor-Resource Category: ",Northeastern_category, "\n",
  #                                                             "MassBays Region: ",`MassBays Region`))
  #                                               ))+
  #           geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
  #           geom_point()+
  #           scale_y_log10()+
  #           scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #           theme(legend.position="none")+
  #           ylab("Percent Remaining")+
  #           ggtitle("Displaying all embayments within this category")
  #         ggplotly(u, tooltip = "text") %>%
  #           rangeslider(1760, 2040, thickness=0.01)
  #       }
  #       else{
  #         u <-ggplot(marsh_category_per_rem,aes(x = Year, y = MarshAcreage, 
  #                                               text = (paste("Year: ",Year, "\n",
  #                                                             "Acres: ",MarshAcreage, "\n",
  #                                                             "Embayment: ",`EMBAYMENT NAME`, "\n",
  #                                                             "Stressor-Resource Category: ",Northeastern_category, "\n",
  #                                                             "MassBays Region: ",`MassBays Region`))
  #                                               ))+
  #           geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
  #           geom_point()+
  #           scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #           theme(legend.position="none")+
  #           ylab("Acres")+
  #           ggtitle("Displaying all embayments within this category")
  #         ggplotly(u, tooltip = "text") %>%
  #           rangeslider(1760, 2040, thickness=0.01)
  #       }
  #     }
  #   }
  # }) 
  # 
  # ############## Northeastern Category: Salt Marsh data profiles #################    
  # 
  # output$category_flats_profile <- renderPlotly({
  #   if(input$plot_tabs=="Northeastern Category"){
  #     if(input$category!=""){
  #       selected_flats_category <- subset(Tidy_Flats_Data,Tidy_Flats_Data$Northeastern_category==input$category)
  #       validate(
  #         need(selected_flats_category$FlatsAcreage!="", "NO TIDAL FLAT DATA AVAILABLE")
  #       )
  #       selected_flats_category$Year <- as.numeric(as.character(selected_flats_category$Year))
  #       flats_category_per_rem<-selected_flats_category %>%
  #         group_by(`EMBAYMENT NAME`) %>%
  #         select(FlatsAcreage, Year, `EMBAYMENT NAME`, Northeastern_category, `MassBays Region`) %>%
  #         mutate(percent_remaining = (FlatsAcreage/(FlatsAcreage[nrow=1]))*100)
  #       if(input$data_category_flats_displayed=="Percent Remaining"){
  #         t <-ggplot(flats_category_per_rem,aes(x = Year, y = percent_remaining, 
  #                                               text = (paste("Year: ",Year, "\n",
  #                                                             "Percent Remaining: ",percent_remaining, "\n",
  #                                                             "Embayment: ",`EMBAYMENT NAME`, "\n",
  #                                                             "Stressor-Resource Category: ",Northeastern_category, "\n",
  #                                                             "MassBays Region: ",`MassBays Region`))
  #                                               ))+
  #           geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
  #           geom_point()+
  #           scale_y_log10()+
  #           scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #           theme(legend.position="none")+
  #           ylab("Percent Remaining")+
  #           ggtitle("Displaying all embayments within this category")
  #         ggplotly(t, tooltip = "text") %>%
  #           rangeslider(1760, 2040, thickness=0.01)
  #       }
  #       else{
  #         t <-ggplot(flats_category_per_rem,aes(x = Year, y = FlatsAcreage, 
  #                                               text = (paste("Year: ",Year, "\n",
  #                                                             "Acres: ",FlatsAcreage, "\n",
  #                                                             "Embayment: ",`EMBAYMENT NAME`, "\n",
  #                                                             "Stressor-Resource Category: ",Northeastern_category, "\n",
  #                                                             "MassBays Region: ",`MassBays Region`))
  #         ))+
  #           geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
  #           geom_point()+
  #           scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #           theme(legend.position="none")+
  #           ylab("Acres")+
  #           ggtitle("Displaying all embayments within this category")
  #         ggplotly(t, tooltip = "text") %>%
  #           rangeslider(1760, 2040, thickness=0.01)
  #       }
  #     }
  #   }
  # })
  # 
  ################################################################################   
  ############ Generate MassBays Regions data profiles and tables ################
  
  ############ Generate MassBays Regions: Totals data profiles ###################
  
  # output$region_totals_profile <- renderPlotly({
  #   if(input$plot_tabs=="MassBays Region"){
  #     if(input$region_data_tabs=="Totals"){
  #       if(input$region!=""){
  #         ###Seagrass###
  #         selected_region_seagrass <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$`MassBays Region`==input$region)
  #         validate(
  #           need(selected_region_seagrass$SeagrassAcreage!="", "NO SEAGRASS DATA AVAILABLE")
  #         )
  #         selected_region_seagrass <- aggregate(selected_region_seagrass$SeagrassAcreage, by=list(Category=selected_region_seagrass$Year), FUN=sum)
  #         colnames(selected_region_seagrass)[colnames(selected_region_seagrass) == "Category"] <- "Year"
  #         colnames(selected_region_seagrass)[colnames(selected_region_seagrass) == "x"] <- "TotalAcreage"
  #         for(i in 1:nrow(selected_region_seagrass)) {
  #           selected_region_seagrass$percent_remaining[i]<-(((selected_region_seagrass$TotalAcreage)[i])/((selected_region_seagrass$TotalAcreage)[1])*100)
  #         }
  #         selected_region_seagrass$Year <- as.numeric(as.character(selected_region_seagrass$Year))
  #         selected_region_seagrass$Habitat<-(NA)
  #         for(i in 1:nrow(selected_region_seagrass)) {
  #           selected_region_seagrass$Habitat[i]<-("Seagrass")
  #         }
  #         ###Salt Marsh###
  #         selected_region_marsh <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$`MassBays Region`==input$region)
  #         validate(
  #           need(selected_region_marsh$MarshAcreage!="", "NO marsh DATA AVAILABLE")
  #         )
  #         selected_region_marsh <- aggregate(selected_region_marsh$MarshAcreage, by=list(Category=selected_region_marsh$Year), FUN=sum)
  #         colnames(selected_region_marsh)[colnames(selected_region_marsh) == "Category"] <- "Year"
  #         colnames(selected_region_marsh)[colnames(selected_region_marsh) == "x"] <- "TotalAcreage"
  #         for(i in 1:nrow(selected_region_marsh)) {
  #           selected_region_marsh$percent_remaining[i]<-(((selected_region_marsh$TotalAcreage)[i])/((selected_region_marsh$TotalAcreage)[1])*100)
  #         }
  #         selected_region_marsh$Year <- as.numeric(as.character(selected_region_marsh$Year))
  #         selected_region_marsh$Habitat<-(NA)
  #         for(i in 1:nrow(selected_region_marsh)) {
  #           selected_region_marsh$Habitat[i]<-("Salt Marsh")
  #         }
  #         ###Tidal Flats###
  #         selected_region_flats <- subset(Tidy_Flats_Data,Tidy_Flats_Data$`MassBays Region`==input$region)
  #         validate(
  #           need(selected_region_flats$FlatsAcreage!="", "NO flats DATA AVAILABLE")
  #         )
  #         selected_region_flats <- aggregate(selected_region_flats$FlatsAcreage, by=list(Category=selected_region_flats$Year), FUN=sum)
  #         colnames(selected_region_flats)[colnames(selected_region_flats) == "Category"] <- "Year"
  #         colnames(selected_region_flats)[colnames(selected_region_flats) == "x"] <- "TotalAcreage"
  #         for(i in 1:nrow(selected_region_flats)) {
  #           selected_region_flats$percent_remaining[i]<-(((selected_region_flats$TotalAcreage)[i])/((selected_region_flats$TotalAcreage)[1])*100)
  #         }
  #         selected_region_flats$Year <- as.numeric(as.character(selected_region_flats$Year))
  #         selected_region_flats$Habitat<-(NA)
  #         for(i in 1:nrow(selected_region_flats)) {
  #           selected_region_flats$Habitat[i]<-("Tidal Flats")
  #         }   
  #         region_totals1 <- merge(selected_region_seagrass,selected_region_marsh, all = TRUE)
  #         region_totals <- merge(region_totals1,selected_region_flats, all = TRUE)
  #         
  #         if(input$data_region_totals_displayed=="Percent Remaining"){
  #           s <-ggplot(region_totals,aes(x = Year, y = percent_remaining, color = Habitat))+
  #             geom_line(aes(group=Habitat))+
  #             geom_point()+
  #             scale_y_log10()+
  #             scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #             #theme(legend.position="none")+
  #             ylab("Percent Remaining")
  #           ggplotly(s)
  #         }
  #         else{
  #           s <-ggplot(region_totals,aes(x = Year, y = TotalAcreage, color = Habitat))+
  #             geom_line(aes(group=Habitat))+
  #             geom_point()+
  #             scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #             #theme(legend.position="none")+
  #             ylab("Acres")
  #           ggplotly(s)
  #         }
  #       }
  #     }
  #   }
  # })
  # 
  # ######### MassBays Region: Seagrass data profiles #################         
  # 
  # output$region_seagrass_profile <- renderPlotly({
  #   if(input$plot_tabs=="MassBays Region"){
  #     if(input$region!=""){
  #       selected_seagrass_region <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$`MassBays Region`==input$region)
  #       validate(
  #         need(selected_seagrass_region$SeagrassAcreage!="", "NO SEAGRASS DATA AVAILABLE")
  #       )
  #       selected_seagrass_region$Year <- as.numeric(as.character(selected_seagrass_region$Year))
  #       seagrass_region_per_rem<-selected_seagrass_region %>%
  #         group_by(`EMBAYMENT NAME`) %>%
  #         select(SeagrassAcreage, Year, `EMBAYMENT NAME`) %>%
  #         mutate(percent_remaining = (SeagrassAcreage/(SeagrassAcreage[nrow=1]))*100)
  #       if(input$data_region_seagrass_displayed=="Percent Remaining"){
  #         r <-ggplot(seagrass_region_per_rem,aes(x = Year, y = percent_remaining, text = seagrass_region_per_rem$`EMBAYMENT NAME`))+
  #           geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
  #           geom_point()+
  #           scale_y_log10()+
  #           scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #           theme(legend.position="none")+
  #           ylab("Percent Remaining")+
  #           ggtitle("Displaying all embayments within this region")
  #         ggplotly(r)
  #       }
  #       else{
  #         r <-ggplot(seagrass_region_per_rem,aes(x = Year, y = SeagrassAcreage, text = seagrass_region_per_rem$`EMBAYMENT NAME`))+
  #           geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
  #           geom_point()+
  #           scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #           theme(legend.position="none")+
  #           ylab("Acres")+
  #           ggtitle("Displaying all embayments within this region")
  #         ggplotly(r)
  #       }
  #     }
  #   }
  # })
  # 
  # ############# MassBays Region: Salt Marsh data profiles #################       
  # 
  # output$region_marsh_profile <- renderPlotly({
  #   if(input$plot_tabs=="MassBays Region"){
  #     if(input$region!=""){
  #       selected_marsh_region <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$`MassBays Region`==input$region)
  #       validate(
  #         need(selected_marsh_region$MarshAcreage!="", "NO SALT MARSH DATA AVAILABLE")
  #       )
  #       selected_marsh_region$Year <- as.numeric(as.character(selected_marsh_region$Year))
  #       marsh_region_per_rem<-selected_marsh_region %>%
  #         group_by(`EMBAYMENT NAME`) %>%
  #         select(MarshAcreage, Year, `EMBAYMENT NAME`) %>%
  #         mutate(percent_remaining = (MarshAcreage/(MarshAcreage[nrow=1]))*100)
  #       if(input$data_region_marsh_displayed=="Percent Remaining"){
  #         q <-ggplot(marsh_region_per_rem,aes(x = Year, y = percent_remaining, text = marsh_region_per_rem$`EMBAYMENT NAME`))+
  #           geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
  #           geom_point()+
  #           scale_y_log10()+
  #           scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #           theme(legend.position="none")+
  #           ylab("Percent Remaining")+
  #           ggtitle("Displaying all embayments within this region")
  #         ggplotly(q)
  #       }
  #       else{
  #         q <-ggplot(marsh_region_per_rem,aes(x = Year, y = MarshAcreage, text = marsh_region_per_rem$`EMBAYMENT NAME`))+
  #           geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
  #           geom_point()+
  #           scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #           theme(legend.position="none")+
  #           ylab("Acres")+
  #           ggtitle("Displaying all embayments within this region")
  #         ggplotly(q)
  #       }
  #     }
  #   }
  # }) 
  # 
  # ############## MassBays Region: Tidal FLats data profiles #################    
  # 
  # output$region_flats_profile <- renderPlotly({
  #   if(input$plot_tabs=="MassBays Region"){
  #     if(input$region!=""){
  #       selected_flats_region <- subset(Tidy_Flats_Data,Tidy_Flats_Data$`MassBays Region`==input$region)
  #       validate(
  #         need(selected_flats_region$FlatsAcreage!="", "NO TIDAL FLAT DATA AVAILABLE")
  #       )
  #       selected_flats_region$Year <- as.numeric(as.character(selected_flats_region$Year))
  #       flats_region_per_rem<-selected_flats_region %>%
  #         group_by(`EMBAYMENT NAME`) %>%
  #         select(FlatsAcreage, Year, `EMBAYMENT NAME`) %>%
  #         mutate(percent_remaining = (FlatsAcreage/(FlatsAcreage[nrow=1]))*100)
  #       if(input$data_region_flats_displayed=="Percent Remaining"){
  #         p <- ggplot(flats_region_per_rem,aes(x = Year, y = percent_remaining, text = flats_region_per_rem$`EMBAYMENT NAME`))+
  #           geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
  #           geom_point()+
  #           scale_y_log10()+
  #           scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #           theme(legend.position="none")+
  #           ylab("Percent Remaining")+
  #           ggtitle("Displaying all embayments within this region")
  #         ggplotly(p)
  #       }
  #       else{
  #         p <- ggplot(flats_region_per_rem,aes(x = Year, y = FlatsAcreage, text = flats_region_per_rem$`EMBAYMENT NAME`))+
  #           geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
  #           geom_point()+
  #           scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
  #           theme(legend.position="none")+
  #           ylab("Acres")+
  #           ggtitle("Displaying all embayments within this region")
  #         ggplotly(p)
  #       }
  #     }
  #   }
  # }) 
}

shinyApp(ui, server)