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
Flats_Data <- read_csv("~/Dropbox/MA-Bays-Shiny/MassBays-Shiny/Flats_Data_MassBays.csv")

Tidy_Flats_Data <- Flats_Data %>% 
  gather(Year, FlatsAcreage, 8:10) %>%
  drop_na(FlatsAcreage)
Tidy_Flats_Data$Year <- as.factor(Tidy_Flats_Data$Year)

#Grouped_Flats_Data<-tibble(split(Tidy_Flats_Data, Tidy_Flats_Data$`EMBAYMENT NAME`))

Marsh_Data <- read_csv("~/Dropbox/MA-Bays-Shiny/MassBays-Shiny/Marsh_Data_MassBays.csv")

Tidy_Marsh_Data <- Marsh_Data %>% 
  gather(Year, MarshAcreage, 8:11) %>%
  drop_na(MarshAcreage)
Tidy_Marsh_Data$Year <- as.factor(Tidy_Marsh_Data$Year)

Seagrass_Data <- read_csv("~/Dropbox/MA-Bays-Shiny/MassBays-Shiny/Seagrass_Data_MassBays.csv")

Tidy_Seagrass_Data <- Seagrass_Data %>% 
  gather(Year, SeagrassAcreage, 8:15)%>%
  drop_na(SeagrassAcreage)
Tidy_Seagrass_Data$Year <- as.factor(Tidy_Seagrass_Data$Year)

MassBaysEmbayments <- st_read(dsn = "~/Dropbox/MA-Bays-Shiny/MassBays-Shiny/massbays_estuarine_embays_2021", layer = "massbays_estuarine_embays_2021")

MassBaysEmbayments <-
  st_transform(MassBaysEmbayments, crs = "+init=epsg:4326")

MassBaysEmbayments <- st_zm(MassBaysEmbayments, drop = T, what = "ZM")

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

ecotype_choices = c("SELECT ECOTYPE","Blue", "Green", "Orange", "Yellow")

regions_choices = c(
  "SELECT REGION",
  "Cape Cod",
  "Lower North Shore",
  "Massachusetts Bay",
  "South Shore",
  "Upper North Shore"
)

data_displayed =c("Percent Remaining", "Acres")

category_choices = c("SELECT CATEGORY","1", "2", "3", "4", "NA")

ui <- fluidPage(
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
                              tabPanel("EcoType", value = "EcoType",
                                       fluidRow(column(12,
                                                       selectInput("ecotype", label = "MassBays EcoType", choices = ecotype_choices)
                                       )),
                                       fluidRow(column(12,
                                                       tabsetPanel(id = "ecotype_data_tabs",
                                                                   tabPanel("Totals",
                                                                            sidebarPanel(radioButtons("data_ecotype_totals_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("ecotype_totals_profile", height = "500px"))
                                                                   ),
                                                                   tabPanel("Seagrass",
                                                                            sidebarPanel(radioButtons("data_ecotype_seagrass_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("ecotype_seagrass_profile", height = "500px"))
                                                                   ),
                                                                   tabPanel("Salt Marsh",
                                                                            sidebarPanel(radioButtons("data_ecotype_marsh_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("ecotype_marsh_profile", height = "500px"))
                                                                   ),
                                                                   tabPanel("Tidal Flats",
                                                                            sidebarPanel(radioButtons("data_ecotype_flats_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("ecotype_flats_profile", height = "500px"))
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
                              tabPanel("Northeastern Category", value = "Northeastern Category",
                                       fluidRow(column(12,
                                                       selectInput("category", label = "Northeastern Category", choices = category_choices)
                                       )),
                                       fluidRow(column(12,
                                                       tabsetPanel(id = "category_data_tabs",
                                                                   tabPanel("Totals",
                                                                            sidebarPanel(radioButtons("data_category_totals_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("category_totals_profile", height = "500px"))
                                                                   ),
                                                                   tabPanel("Seagrass",
                                                                            sidebarPanel(radioButtons("data_category_seagrass_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("category_seagrass_profile", height = "500px"))
                                                                   ),
                                                                   tabPanel("Salt Marsh",
                                                                            sidebarPanel(radioButtons("data_category_marsh_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("category_marsh_profile", height = "500px"))
                                                                   ),
                                                                   tabPanel("Tidal Flats",
                                                                            sidebarPanel(radioButtons("data_category_flats_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("category_flats_profile", height = "500px"))
                                                                   )
                                                       )
                                       ))
                              ),
                              tabPanel("MassBays Region", value = "MassBays Region",
                                       fluidRow(column(12,
                                                       selectInput("region", label = "MassBays Region", choices = regions_choices)
                                       )),
                                       fluidRow(column(12,
                                                       tabsetPanel(id = "region_data_tabs",
                                                                   tabPanel("Totals",
                                                                            sidebarPanel(radioButtons("data_region_totals_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("region_totals_profile", height = "500px"))
                                                                   ),
                                                                   tabPanel("Seagrass",
                                                                            sidebarPanel(radioButtons("data_region_seagrass_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("region_seagrass_profile",height = "500px"))
                                                                   ),
                                                                   tabPanel("Salt Marsh",
                                                                            sidebarPanel(radioButtons("data_region_marsh_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("region_marsh_profile",height = "500px"))
                                                                   ),
                                                                   tabPanel("Tidal Flats",
                                                                            sidebarPanel(radioButtons("data_region_flats_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("region_flats_profile", height = "500px"))
                                                                   )
                                                       )
                                       ))
                              ),
                              tabPanel("Embayment", value = "Embayment",
                                       fluidRow(column(12,
                                                       selectInput("embayment", label = "MassBays Embayment", choices = embayment_choices)
                                       )),
                                       fluidRow(column(12,
                                                       tabsetPanel(id = "embayment_data_tabs",
                                                                   tabPanel("Graph",
                                                                            sidebarPanel(radioButtons("data_embayment_displayed","Choose data option", data_displayed)),
                                                                            mainPanel(plotlyOutput("embayment_profiles", height = "500px"))
                                                                   ),
                                                                   tabPanel(id = "emabyment_tables","Tables",
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
  
  ################################################################################ 
  #################### Creating the leaflet base map #############################
  
  ########### Create base map with all of the embayment polygons ############
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>% setView(-70.5, 42.2, zoom = 8) %>% addPolygons(data=MassBaysEmbayments, weight = 1)
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
          e <-ggplot(all_selected_embayment_data,aes(x = Year, y = percent_remaining, color = Habitat))+
            geom_line(aes(group=Habitat))+
            geom_point()+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            #theme(legend.position="none")+
            ylab("Percent Remaining")
          ggplotly(e)
        }
        else{
          e <-ggplot(all_selected_embayment_data,aes(x = Year, y = Acreage, color = Habitat))+
            geom_line(aes(group=Habitat))+
            geom_point()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            #theme(legend.position="none")+
            ylab("Acres")
          ggplotly(e)
        }
      }
    }
  })
  
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
            d <-ggplot(ecotype_totals,aes(x = Year, y = percent_remaining, color = Habitat))+
              geom_line(aes(group=Habitat))+
              geom_point()+
              scale_y_log10()+
              scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
              #theme(legend.position="none")+
              ylab("Percent Remaining")
            ggplotly(d)
          }
          else{
            d <-ggplot(ecotype_totals,aes(x = Year, y = TotalAcreage, color = Habitat))+
              geom_line(aes(group=Habitat))+
              geom_point()+
              scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
              #theme(legend.position="none")+
              ylab("Acres")
            ggplotly(d)
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
          select(SeagrassAcreage, Year, `EMBAYMENT NAME`) %>%
          mutate(percent_remaining = (SeagrassAcreage/(SeagrassAcreage[nrow=1]))*100)
        if(input$data_ecotype_seagrass_displayed=="Percent Remaining"){
          c <-ggplot(seagrass_ecotype_per_rem,aes(x = Year, y = percent_remaining, text = seagrass_ecotype_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Percent Remaining")+
            ggtitle("Displaying all embayments within this ecotype")
          ggplotly(c)
        }
        else{
          c <-ggplot(seagrass_ecotype_per_rem,aes(x = Year, y = SeagrassAcreage, text = seagrass_ecotype_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Acres")+
            ggtitle("Displaying all embayments within this ecotype")
          ggplotly(c)
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
          select(MarshAcreage, Year, `EMBAYMENT NAME`) %>%
          mutate(percent_remaining = (MarshAcreage/(MarshAcreage[nrow=1]))*100)
        if(input$data_ecotype_marsh_displayed=="Percent Remaining"){
          b <-ggplot(marsh_ecotype_per_rem,aes(x = Year, y = percent_remaining, text = marsh_ecotype_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Percent Remaining")+
            ggtitle("Displaying all embayments within this ecotype")
          ggplotly(b)
        }
        else{
          b <-ggplot(marsh_ecotype_per_rem,aes(x = Year, y = MarshAcreage, text = marsh_ecotype_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Acres")+
            ggtitle("Displaying all embayments within this ecotype")
          ggplotly(b)
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
          select(FlatsAcreage, Year, `EMBAYMENT NAME`) %>%
          mutate(percent_remaining = (FlatsAcreage/(FlatsAcreage[nrow=1]))*100)
        if(input$data_ecotype_flats_displayed=="Percent Remaining"){
          a <-ggplot(flats_ecotype_per_rem,aes(x = Year, y = percent_remaining, text = flats_ecotype_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Percent Remaining")+
            ggtitle("Displaying all embayments within this ecotype")
          ggplotly(a)
        }
        else{
          a <-ggplot(flats_ecotype_per_rem,aes(x = Year, y = FlatsAcreage, text = flats_ecotype_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Acres")+
            ggtitle("Displaying all embayments within this ecotype")
          ggplotly(a)
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
  ########## Generate Northeastern Category data profiles and tables #############
  
  ############ Northeastern Category: Totals data profiles ###################
  
  output$category_totals_profile <- renderPlotly({
    if(input$plot_tabs=="Northeastern Category"){
      if(input$category_data_tabs=="Totals"){
        if(input$category!=""){
          ###Seagrass###
          selected_category_seagrass <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$Northeastern_category==input$category)
          validate(
            need(selected_category_seagrass$SeagrassAcreage!="", "NO SEAGRASS DATA AVAILABLE")
          )
          selected_category_seagrass <- aggregate(selected_category_seagrass$SeagrassAcreage, by=list(Category=selected_category_seagrass$Year), FUN=sum)
          colnames(selected_category_seagrass)[colnames(selected_category_seagrass) == "Category"] <- "Year"
          colnames(selected_category_seagrass)[colnames(selected_category_seagrass) == "x"] <- "TotalAcreage"
          for(i in 1:nrow(selected_category_seagrass)) {
            selected_category_seagrass$percent_remaining[i]<-(((selected_category_seagrass$TotalAcreage)[i])/((selected_category_seagrass$TotalAcreage)[1])*100)
          }
          selected_category_seagrass$Year <- as.numeric(as.character(selected_category_seagrass$Year))
          selected_category_seagrass$Habitat<-(NA)
          for(i in 1:nrow(selected_category_seagrass)) {
            selected_category_seagrass$Habitat[i]<-("Seagrass")
          }
          ###Salt Marsh###
          selected_category_marsh <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$Northeastern_category==input$category)
          validate(
            need(selected_category_marsh$MarshAcreage!="", "NO marsh DATA AVAILABLE")
          )
          selected_category_marsh <- aggregate(selected_category_marsh$MarshAcreage, by=list(Category=selected_category_marsh$Year), FUN=sum)
          colnames(selected_category_marsh)[colnames(selected_category_marsh) == "Category"] <- "Year"
          colnames(selected_category_marsh)[colnames(selected_category_marsh) == "x"] <- "TotalAcreage"
          for(i in 1:nrow(selected_category_marsh)) {
            selected_category_marsh$percent_remaining[i]<-(((selected_category_marsh$TotalAcreage)[i])/((selected_category_marsh$TotalAcreage)[1])*100)
          }
          selected_category_marsh$Year <- as.numeric(as.character(selected_category_marsh$Year))
          selected_category_marsh$Habitat<-(NA)
          for(i in 1:nrow(selected_category_marsh)) {
            selected_category_marsh$Habitat[i]<-("Salt Marsh")
          }
          ###Tidal Flats###
          selected_category_flats <- subset(Tidy_Flats_Data,Tidy_Flats_Data$Northeastern_category==input$category)
          validate(
            need(selected_category_flats$FlatsAcreage!="", "NO flats DATA AVAILABLE")
          )
          selected_category_flats <- aggregate(selected_category_flats$FlatsAcreage, by=list(Category=selected_category_flats$Year), FUN=sum)
          colnames(selected_category_flats)[colnames(selected_category_flats) == "Category"] <- "Year"
          colnames(selected_category_flats)[colnames(selected_category_flats) == "x"] <- "TotalAcreage"
          for(i in 1:nrow(selected_category_flats)) {
            selected_category_flats$percent_remaining[i]<-(((selected_category_flats$TotalAcreage)[i])/((selected_category_flats$TotalAcreage)[1])*100)
          }
          selected_category_flats$Year <- as.numeric(as.character(selected_category_flats$Year))
          selected_category_flats$Habitat<-(NA)
          for(i in 1:nrow(selected_category_flats)) {
            selected_category_flats$Habitat[i]<-("Tidal Flats")
          }   
          category_totals1 <- merge(selected_category_seagrass,selected_category_marsh, all = TRUE)
          category_totals <- merge(category_totals1,selected_category_flats, all = TRUE)
          
          if(input$data_category_totals_displayed=="Percent Remaining"){
            w <-ggplot(category_totals,aes(x = Year, y = percent_remaining, color = Habitat))+
              geom_line(aes(group=Habitat))+
              geom_point()+
              scale_y_log10()+
              scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
              #theme(legend.position="none")+
              ylab("Percent Remaining")
            ggplotly(w)
          }
          else{
            w <-ggplot(category_totals,aes(x = Year, y = TotalAcreage, color = Habitat))+
              geom_line(aes(group=Habitat))+
              geom_point()+
              scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
              #theme(legend.position="none")+
              ylab("Acres")
            ggplotly(w)
          }
        }
      }
    }
  })
  
  ######### Northeastern Category: Seagrass data profiles #################         
  
  output$category_seagrass_profile <- renderPlotly({
    if(input$plot_tabs=="Northeastern Category"){
      if(input$category!=""){
        selected_seagrass_category <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$Northeastern_category==input$category)
        validate(
          need(selected_seagrass_category$SeagrassAcreage!="", "NO SEAGRASS DATA AVAILABLE")
        )
        selected_seagrass_category$Year <- as.numeric(as.character(selected_seagrass_category$Year))
        seagrass_category_per_rem<-selected_seagrass_category %>%
          group_by(`EMBAYMENT NAME`) %>%
          select(SeagrassAcreage, Year, `EMBAYMENT NAME`) %>%
          mutate(percent_remaining = (SeagrassAcreage/(SeagrassAcreage[nrow=1]))*100)
        if(input$data_category_seagrass_displayed=="Percent Remaining"){
          v <-ggplot(seagrass_category_per_rem,aes(x = Year, y = percent_remaining, text = seagrass_category_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Percent Remaining")+
            ggtitle("Displaying all embayments within this category")
          ggplotly(v)
        }
        else{
          v <-ggplot(seagrass_category_per_rem,aes(x = Year, y = SeagrassAcreage, text = seagrass_category_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Acres")+
            ggtitle("Displaying all embayments within this category")
          ggplotly(v)
        }
      }
    }
  })
  
  ############# Northeastern Category: Salt Marsh data profiles #################       
  
  output$category_marsh_profile <- renderPlotly({
    if(input$plot_tabs=="Northeastern Category"){
      if(input$category!=""){
        selected_marsh_category <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$Northeastern_category==input$category)
        validate(
          need(selected_marsh_category$MarshAcreage!="", "NO SALT MARSH DATA AVAILABLE")
        )
        selected_marsh_category$Year <- as.numeric(as.character(selected_marsh_category$Year))
        marsh_category_per_rem<-selected_marsh_category %>%
          group_by(`EMBAYMENT NAME`) %>%
          select(MarshAcreage, Year, `EMBAYMENT NAME`) %>%
          mutate(percent_remaining = (MarshAcreage/(MarshAcreage[nrow=1]))*100)
        if(input$data_category_marsh_displayed=="Percent Remaining"){
          u <-ggplot(marsh_category_per_rem,aes(x = Year, y = percent_remaining, text = marsh_category_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Percent Remaining")+
            ggtitle("Displaying all embayments within this category")
          ggplotly(u)
        }
        else{
          u <-ggplot(marsh_category_per_rem,aes(x = Year, y = MarshAcreage, text = marsh_category_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Acres")+
            ggtitle("Displaying all embayments within this category")
          ggplotly(u)
        }
      }
    }
  }) 
  
  ############## Northeastern Category: Salt Marsh data profiles #################    
  
  output$category_flats_profile <- renderPlotly({
    if(input$plot_tabs=="Northeastern Category"){
      if(input$category!=""){
        selected_flats_category <- subset(Tidy_Flats_Data,Tidy_Flats_Data$Northeastern_category==input$category)
        validate(
          need(selected_flats_category$FlatsAcreage!="", "NO TIDAL FLAT DATA AVAILABLE")
        )
        selected_flats_category$Year <- as.numeric(as.character(selected_flats_category$Year))
        flats_category_per_rem<-selected_flats_category %>%
          group_by(`EMBAYMENT NAME`) %>%
          select(FlatsAcreage, Year, `EMBAYMENT NAME`) %>%
          mutate(percent_remaining = (FlatsAcreage/(FlatsAcreage[nrow=1]))*100)
        if(input$data_category_flats_displayed=="Percent Remaining"){
          t <-ggplot(flats_category_per_rem,aes(x = Year, y = percent_remaining, text = flats_category_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Percent Remaining")+
            ggtitle("Displaying all embayments within this category")
          ggplotly(t)
        }
        else{
          t <-ggplot(flats_category_per_rem,aes(x = Year, y = FlatsAcreage, text = flats_category_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Acres")+
            ggtitle("Displaying all embayments within this category")
          ggplotly(t)
        }
      }
    }
  })
  
  ################################################################################   
  ############ Generate MassBays Regions data profiles and tables ################
  
  ############ Generate MassBays Regions: Totals data profiles ###################
  
  output$region_totals_profile <- renderPlotly({
    if(input$plot_tabs=="MassBays Region"){
      if(input$region_data_tabs=="Totals"){
        if(input$region!=""){
          ###Seagrass###
          selected_region_seagrass <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$`MassBays Region`==input$region)
          validate(
            need(selected_region_seagrass$SeagrassAcreage!="", "NO SEAGRASS DATA AVAILABLE")
          )
          selected_region_seagrass <- aggregate(selected_region_seagrass$SeagrassAcreage, by=list(Category=selected_region_seagrass$Year), FUN=sum)
          colnames(selected_region_seagrass)[colnames(selected_region_seagrass) == "Category"] <- "Year"
          colnames(selected_region_seagrass)[colnames(selected_region_seagrass) == "x"] <- "TotalAcreage"
          for(i in 1:nrow(selected_region_seagrass)) {
            selected_region_seagrass$percent_remaining[i]<-(((selected_region_seagrass$TotalAcreage)[i])/((selected_region_seagrass$TotalAcreage)[1])*100)
          }
          selected_region_seagrass$Year <- as.numeric(as.character(selected_region_seagrass$Year))
          selected_region_seagrass$Habitat<-(NA)
          for(i in 1:nrow(selected_region_seagrass)) {
            selected_region_seagrass$Habitat[i]<-("Seagrass")
          }
          ###Salt Marsh###
          selected_region_marsh <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$`MassBays Region`==input$region)
          validate(
            need(selected_region_marsh$MarshAcreage!="", "NO marsh DATA AVAILABLE")
          )
          selected_region_marsh <- aggregate(selected_region_marsh$MarshAcreage, by=list(Category=selected_region_marsh$Year), FUN=sum)
          colnames(selected_region_marsh)[colnames(selected_region_marsh) == "Category"] <- "Year"
          colnames(selected_region_marsh)[colnames(selected_region_marsh) == "x"] <- "TotalAcreage"
          for(i in 1:nrow(selected_region_marsh)) {
            selected_region_marsh$percent_remaining[i]<-(((selected_region_marsh$TotalAcreage)[i])/((selected_region_marsh$TotalAcreage)[1])*100)
          }
          selected_region_marsh$Year <- as.numeric(as.character(selected_region_marsh$Year))
          selected_region_marsh$Habitat<-(NA)
          for(i in 1:nrow(selected_region_marsh)) {
            selected_region_marsh$Habitat[i]<-("Salt Marsh")
          }
          ###Tidal Flats###
          selected_region_flats <- subset(Tidy_Flats_Data,Tidy_Flats_Data$`MassBays Region`==input$region)
          validate(
            need(selected_region_flats$FlatsAcreage!="", "NO flats DATA AVAILABLE")
          )
          selected_region_flats <- aggregate(selected_region_flats$FlatsAcreage, by=list(Category=selected_region_flats$Year), FUN=sum)
          colnames(selected_region_flats)[colnames(selected_region_flats) == "Category"] <- "Year"
          colnames(selected_region_flats)[colnames(selected_region_flats) == "x"] <- "TotalAcreage"
          for(i in 1:nrow(selected_region_flats)) {
            selected_region_flats$percent_remaining[i]<-(((selected_region_flats$TotalAcreage)[i])/((selected_region_flats$TotalAcreage)[1])*100)
          }
          selected_region_flats$Year <- as.numeric(as.character(selected_region_flats$Year))
          selected_region_flats$Habitat<-(NA)
          for(i in 1:nrow(selected_region_flats)) {
            selected_region_flats$Habitat[i]<-("Tidal Flats")
          }   
          region_totals1 <- merge(selected_region_seagrass,selected_region_marsh, all = TRUE)
          region_totals <- merge(region_totals1,selected_region_flats, all = TRUE)
          
          if(input$data_region_totals_displayed=="Percent Remaining"){
            s <-ggplot(region_totals,aes(x = Year, y = percent_remaining, color = Habitat))+
              geom_line(aes(group=Habitat))+
              geom_point()+
              scale_y_log10()+
              scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
              #theme(legend.position="none")+
              ylab("Percent Remaining")
            ggplotly(s)
          }
          else{
            s <-ggplot(region_totals,aes(x = Year, y = TotalAcreage, color = Habitat))+
              geom_line(aes(group=Habitat))+
              geom_point()+
              scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
              #theme(legend.position="none")+
              ylab("Acres")
            ggplotly(s)
          }
        }
      }
    }
  })
  
  ######### MassBays Region: Seagrass data profiles #################         
  
  output$region_seagrass_profile <- renderPlotly({
    if(input$plot_tabs=="MassBays Region"){
      if(input$region!=""){
        selected_seagrass_region <- subset(Tidy_Seagrass_Data,Tidy_Seagrass_Data$`MassBays Region`==input$region)
        validate(
          need(selected_seagrass_region$SeagrassAcreage!="", "NO SEAGRASS DATA AVAILABLE")
        )
        selected_seagrass_region$Year <- as.numeric(as.character(selected_seagrass_region$Year))
        seagrass_region_per_rem<-selected_seagrass_region %>%
          group_by(`EMBAYMENT NAME`) %>%
          select(SeagrassAcreage, Year, `EMBAYMENT NAME`) %>%
          mutate(percent_remaining = (SeagrassAcreage/(SeagrassAcreage[nrow=1]))*100)
        if(input$data_region_seagrass_displayed=="Percent Remaining"){
          r <-ggplot(seagrass_region_per_rem,aes(x = Year, y = percent_remaining, text = seagrass_region_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Percent Remaining")+
            ggtitle("Displaying all embayments within this region")
          ggplotly(r)
        }
        else{
          r <-ggplot(seagrass_region_per_rem,aes(x = Year, y = SeagrassAcreage, text = seagrass_region_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Acres")+
            ggtitle("Displaying all embayments within this region")
          ggplotly(r)
        }
      }
    }
  })
  
  ############# MassBays Region: Salt Marsh data profiles #################       
  
  output$region_marsh_profile <- renderPlotly({
    if(input$plot_tabs=="MassBays Region"){
      if(input$region!=""){
        selected_marsh_region <- subset(Tidy_Marsh_Data,Tidy_Marsh_Data$`MassBays Region`==input$region)
        validate(
          need(selected_marsh_region$MarshAcreage!="", "NO SALT MARSH DATA AVAILABLE")
        )
        selected_marsh_region$Year <- as.numeric(as.character(selected_marsh_region$Year))
        marsh_region_per_rem<-selected_marsh_region %>%
          group_by(`EMBAYMENT NAME`) %>%
          select(MarshAcreage, Year, `EMBAYMENT NAME`) %>%
          mutate(percent_remaining = (MarshAcreage/(MarshAcreage[nrow=1]))*100)
        if(input$data_region_marsh_displayed=="Percent Remaining"){
          q <-ggplot(marsh_region_per_rem,aes(x = Year, y = percent_remaining, text = marsh_region_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Percent Remaining")+
            ggtitle("Displaying all embayments within this region")
          ggplotly(q)
        }
        else{
          q <-ggplot(marsh_region_per_rem,aes(x = Year, y = MarshAcreage, text = marsh_region_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Acres")+
            ggtitle("Displaying all embayments within this region")
          ggplotly(q)
        }
      }
    }
  }) 
  
  ############## MassBays Region: Tidal FLats data profiles #################    
  
  output$region_flats_profile <- renderPlotly({
    if(input$plot_tabs=="MassBays Region"){
      if(input$region!=""){
        selected_flats_region <- subset(Tidy_Flats_Data,Tidy_Flats_Data$`MassBays Region`==input$region)
        validate(
          need(selected_flats_region$FlatsAcreage!="", "NO TIDAL FLAT DATA AVAILABLE")
        )
        selected_flats_region$Year <- as.numeric(as.character(selected_flats_region$Year))
        flats_region_per_rem<-selected_flats_region %>%
          group_by(`EMBAYMENT NAME`) %>%
          select(FlatsAcreage, Year, `EMBAYMENT NAME`) %>%
          mutate(percent_remaining = (FlatsAcreage/(FlatsAcreage[nrow=1]))*100)
        if(input$data_region_flats_displayed=="Percent Remaining"){
          p <- ggplot(flats_region_per_rem,aes(x = Year, y = percent_remaining, text = flats_region_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_y_log10()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Percent Remaining")+
            ggtitle("Displaying all embayments within this region")
          ggplotly(p)
        }
        else{
          p <- ggplot(flats_region_per_rem,aes(x = Year, y = FlatsAcreage, text = flats_region_per_rem$`EMBAYMENT NAME`))+
            geom_line(aes(group=`EMBAYMENT NAME`, color = `EMBAYMENT NAME`))+
            geom_point()+
            scale_x_continuous(breaks = seq(1760, 2020, by = 20))+
            theme(legend.position="none")+
            ylab("Acres")+
            ggtitle("Displaying all embayments within this region")
          ggplotly(p)
        }
      }
    }
  }) 
}

shinyApp(ui, server)