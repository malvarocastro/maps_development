#UI file for demo 2

library(shiny)
library(shinydashboard)
library(leaflet)

##
source("maps_geo_functions.R")
source("maps_tigo_data.R")
source("cx_functions.R")

#Controls list
control_list_tab1 <- list(dateRangeInput("tigo_filter_fecha_t1","Fecha de respuesta",
                                         start = Sys.Date()-30, end = max(df_tigo_ver$fecha_respuesta),
                                         min = min(df_tigo_ver$fecha_respuesta),
                                         max = max(df_tigo_ver$fecha_respuesta),
                                         format = "dd-mm-yyyy", separator = "-"),
                          selectInput("tigo_filter_mercado_t1","Mercado",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$mercado)))),
                                      multiple = T,
                                      selected = "Todo"),
                          selectInput("tigo_filter_canal_t1","Canal",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$canal)))),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_ver$canal)))),
                          selectInput("tigo_filter_journey_t1","Journey",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$journey)))),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_ver$journey)))),
                          selectInput("tigo_filter_bu_t1","BU",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$bu)))),
                                      multiple = T,
                                      selected= sort(as.character(unique(df_tigo_ver$bu)))),
                          selectInput("tigo_filter_verified_t1","Verificado",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$ver)),decreasing = T)),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_ver$ver)),decreasing = T)),
                          selectInput("tigo_controls_mrc_metrics_t1",
                                      choices = tigoMetrics,selected = "LTR",
                                      multiple = F,label = "Metrica"),
                          selectInput("tigo_controls_mra_calcs_t1",
                                      choices=tigoCalcs, selected = "NPS" ,
                                      multiple = F, label="Cálculo")
)

control_list_tab2 <- list(dateRangeInput("tigo_filter_fecha_t2","Fecha de respuesta",
                                         start = Sys.Date()-30, end = max(df_tigo_ver$fecha_respuesta),
                                         min = min(df_tigo_ver$fecha_respuesta),
                                         max = max(df_tigo_ver$fecha_respuesta),
                                         format = "dd-mm-yyyy", separator = "-"),
                          selectInput("tigo_filter_mercado_t2","Mercado",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$mercado)))),
                                      multiple = T,
                                      selected = "Todo"),
                          selectInput("tigo_filter_canal_t2","Canal",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$canal)))),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_ver$canal)))),
                          selectInput("tigo_filter_journey_t2","Journey",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$journey)))),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_ver$journey)))),
                          selectInput("tigo_filter_bu_t2","BU",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$bu)))),
                                      multiple = T,
                                      selected= sort(as.character(unique(df_tigo_ver$bu)))),
                          selectInput("tigo_filter_verified_t2","Verificado",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$ver)),decreasing = T)),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_ver$ver)),decreasing = T)),
                          selectInput("tigo_controls_mrc_metrics_t2",
                                      choices = tigoMetrics,selected = "LTR",
                                      multiple = F,label = "Metrica"),
                          selectInput("tigo_controls_mra_calcs_t2",
                                      choices=tigoCalcs, selected = "NPS" ,
                                      multiple = F, label="Cálculo")
)

control_list_tab3 <- list(dateRangeInput("tigo_filter_fecha_t3","Fecha de respuesta",
                                         start = Sys.Date()-30, end = max(df_tigo_geo_pais$fecha_respuesta),
                                         min = min(df_tigo_geo_pais$fecha_respuesta),
                                         max = max(df_tigo_geo_pais$fecha_respuesta),
                                         format = "dd-mm-yyyy", separator = "-"),
                          selectInput("tigo_filter_mercado_t3","Mercado",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_geo_pais$mercado)))),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_geo_pais$mercado)))),
                          selectInput("tigo_filter_canal_t3","Canal",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_geo_pais$canal)))),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_geo_pais$canal)))),
                          selectInput("tigo_filter_journey_t3","Journey",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_geo_pais$journey)))),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_geo_pais$journey)))),
                          selectInput("tigo_filter_bu_t3","BU",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_geo_pais$bu)))),
                                      multiple = T,
                                      selected= sort(as.character(unique(df_tigo_geo_pais$bu)))),
                          selectInput("tigo_filter_verified_t3","Verificado",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_geo_pais$ver)),decreasing = T)),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_geo_pais$ver)),decreasing = T)),
                          selectInput("tigo_controls_mrc_metrics_t3",
                                      choices = tigoMetrics,selected = "LTR",
                                      multiple = F,label = "Metrica"),
                          selectInput("tigo_controls_mra_calcs_t3",
                                      choices=tigoCalcs, selected = "NPS" ,
                                      multiple = F, label="Cálculo")
)

## Sidebar content
dash_sidebar <-  dashboardSidebar(
  sidebarMenu(
    #Dash tabs
    menuItem("Regional - Detalle", tabName = "tigo_dash_regional_detalle", 
             icon = icon("dashboard")),
    menuItem("Regional - Agregado", tabName = "tigo_dash_regional_agregado",
             icon = icon("dashboard")),
    menuItem("Colombia - Agregado", tabName = "tigo_dash_colombia_agregado",
             icon = icon("dashboard"))
    
    #Other
    #menuItem("Ir a Medallia", icon = icon("browser"), 
    #         href = "https://tigo.medallia.com/tigo/")
  )
)


## Body content
dash_body <-   tabItems(
  # First tab content
  tabItem(tabName = "tigo_dash_regional_detalle",
          br(),
            ##Filters
            fluidRow(
              box(width = 12,collapsible = T, collapsed = T,background = "blue",
                  title="Filtros",solidHeader = T,
                  splitLayout(control_list_tab1[1:5],control_list_tab1[6:7]))
            ),
            
            ##Mapa 1
            fluidRow(
              box(width = 12,background = "light-blue",
                  title="Mapa regional - Circulos",solidHeader = T,
                  leafletOutput("tigo_map_circles"))
              ),
            
            ##Mapa 2
            fluidRow(
              box(width = 12,background = "light-blue",
                  title="Mapa regional - Pines",solidHeader = T,
                  leafletOutput("tigo_map_pines"))
              )

                 
          ),
  
#2nd tab content
  tabItem(tabName = "tigo_dash_regional_agregado",
            br(),
            ##Filters
            fluidRow(
              box(width = 12,collapsible = T, collapsed = T,background = "blue",
                  title="Filtros",solidHeader = T,
                  splitLayout(control_list_tab2[1:5],control_list_tab2[7:8]))
            ),
            ##Mapa 1
            fluidRow(
              box(width = 12,background = "light-blue",
                  title="Mapa regional - Agregado",solidHeader = T,
                  leafletOutput("tigo_map_agregado"))
            )
                 
          ),

#3rd tab content
  tabItem(tabName = "tigo_dash_colombia_agregado",
          br(),
          ##Filters
          fluidRow(
            box(width = 12,collapsible = T, collapsed = T,background = "blue",
                title="Filtros",solidHeader = T,
                splitLayout(control_list_tab3[c(1,3,4,5)],control_list_tab3[7:8]))
          ),
          ##Mapa 1
          fluidRow(
            box(width = 12,background = "light-blue",
                title="Nivel 1",solidHeader = T,
                leafletOutput("tigo_map_colombia_n1_agregado"))
          ),
          fluidRow(
            box(width = 12,background = "light-blue",
                title="Nivel 2",solidHeader = T,
                leafletOutput("tigo_map_colombia_n2_agregado"))
          )
          
        )  
          
          
  )
  









ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title= "CX Team Maps Demo"),
                    dashboardSidebar(dash_sidebar),
                    dashboardBody(dash_body)
)







