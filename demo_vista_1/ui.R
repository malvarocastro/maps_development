library(shiny)
library(shinydashboard)
library(leaflet)

##
source("maps_geo_functions.R")
source("maps_tigo_ver_df.R")
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
                                      multiple = F, label="Calculo")
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
                                         start = Sys.Date()-30, end = max(df_tigo_ver$fecha_respuesta),
                                         min = min(df_tigo_ver$fecha_respuesta),
                                         max = max(df_tigo_ver$fecha_respuesta),
                                         format = "dd-mm-yyyy", separator = "-"),
                          selectInput("tigo_filter_mercado_t3","Mercado",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$mercado)))),
                                      multiple = T,
                                      selected = "Todo"),
                          selectInput("tigo_filter_canal_t3","Canal",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$canal)))),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_ver$canal)))),
                          selectInput("tigo_filter_journey_t3","Journey",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$journey)))),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_ver$journey)))),
                          selectInput("tigo_filter_bu_t3","BU",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$bu)))),
                                      multiple = T,
                                      selected= sort(as.character(unique(df_tigo_ver$bu)))),
                          selectInput("tigo_filter_verified_t3","Verificado",
                                      choices = append("Todo",sort(as.character(unique(df_tigo_ver$ver)),decreasing = T)),
                                      multiple = T,
                                      selected = sort(as.character(unique(df_tigo_ver$ver)),decreasing = T)),
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
    menuItem("Vista Regional - Detalle", tabName = "tigo_dash_regional_detalle", 
             icon = icon("dashboard")),
    menuItem("Vista Regional - Agregado", tabName = "tigo_dash_regional_agregado",
             icon = icon("dashboard")),
    
    
    #Other
    menuItem("Ir a Medallia", icon = icon("browser"), 
             href = "https://tigo.medallia.com/tigo/")
  )
)


## Body content
dash_body <-   tabItems(
  # First tab content
  tabItem(tabName = "tigo_dash_regional_detalle",
          br(),
          #fluidRow(
          tabBox(width = 12,
                 ##Mapa 1
                 tabPanel(title="Mapa regional - Circulos",solidHeader = T,
                          status="primary",leafletOutput("tigo_map_circles")),
                 ##Mapa 2
                 tabPanel(title="Mapa regional - Pines",solidHeader = T,
                          status="primary",leafletOutput("tigo_map_pines")),
                 ##Controles
                 
                 tabPanel(title="Filtros",solidHeader = T,status="primary",
                          splitLayout(control_list_tab1[1:5],control_list_tab1[6:7])
                 )
                 
          )
          
          #)
          
  ),
  

  #         
  # )
  tabItem(tabName = "tigo_dash_regional_agregado",
          br(),
          #fluidRow(
          tabBox(width = 12,
                 ##Mapa 1
                 tabPanel(title="Mapa regional - Agregado",solidHeader = T,
                          status="primary",leafletOutput("tigo_map_agregado")),
                 ##Controles
                 tabPanel(title="Filtros",solidHeader = T,status="primary",
                          splitLayout(control_list_tab2[1:5],control_list_tab2[6:8])
                 )
                 
          )
          
          #)
          
  )
  
)








ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title= "CX Team Maps Demo"),
                    dashboardSidebar(dash_sidebar),
                    dashboardBody(dash_body)
)




