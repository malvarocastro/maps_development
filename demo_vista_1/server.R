library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(sp)

##
source("maps_geo_functions.R")
source("maps_tigo_ver_df.R")
source("cx_functions.R")

#google map
Gtemplate <- "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga"

server <- function(input, output,session) {
  
  ##Observe for each filter
  observe({
    #obs for mercado
    if("Todo" %in% input$tigo_filter_mercado_t1){
      ob_tigo_filter_mercado_t1<- as.character(unique(df_tigo_ver$mercado))}else{
        ob_tigo_filter_mercado_t1<-input$tigo_filter_mercado_t1
      }
    updateSelectInput(session,"tigo_filter_mercado_t1",
                      selected =ob_tigo_filter_mercado_t1 )
    #obs for canal
    if("Todo" %in% input$tigo_filter_canal_t1){
      ob_tigo_filter_canal_t1<- as.character(unique(df_tigo_ver$canal))}else{
        ob_tigo_filter_canal_t1<-input$tigo_filter_canal_t1
      }
    updateSelectInput(session,"tigo_filter_canal_t1",
                      selected =ob_tigo_filter_canal_t1 )
    #obs for journey
    if("Todo" %in% input$tigo_filter_journey_t1){
      ob_tigo_filter_journey_t1<- as.character(unique(df_tigo_ver$journey))}else{
        ob_tigo_filter_journey_t1<-input$tigo_filter_journey_t1
      }
    updateSelectInput(session,"tigo_filter_journey_t1",
                      selected =ob_tigo_filter_journey_t1 )
    #obs for bu
    if("Todo" %in% input$tigo_filter_bu_t1){
      ob_tigo_filter_bu_t1<- as.character(unique(df_tigo_ver$bu))}else{
        ob_tigo_filter_bu_t1<-input$tigo_filter_bu_t1
      }
    updateSelectInput(session,"tigo_filter_bu_t1",
                      selected =ob_tigo_filter_bu_t1 )
    #obs for verified
    if("Todo" %in% input$tigo_filter_verified_t1){
      ob_tigo_filter_verified_t1<- as.character(unique(df_tigo_ver$verified))}else{
        ob_tigo_filter_verified_t1<-input$tigo_filter_verified_t1
      }
    updateSelectInput(session,"tigo_filter_verified_t1",
                      selected =ob_tigo_filter_verified_t1 )
  })
  
  ##Regional View - Details
  df_reac_detail <- reactive({
    df_tigo_ver %>% 
      filter(fecha_respuesta %in% input$tigo_filter_fecha_t1) %>%
      filter(mercado %in% input$tigo_filter_mercado_t1) %>%
      filter(canal %in% input$tigo_filter_canal_t1) %>%
      filter(journey %in% input$tigo_filter_journey_t1) %>%
      filter(bu %in% input$tigo_filter_bu_t1) %>%
      filter(ver %in% input$tigo_filter_verified_t1)
  })
  
  ##REactive controls
  reac_mrc_metrics <- reactive({
    met <- df_reac_detail()
    met[,as.character(tolower(input$tigo_controls_mrc_metrics_t1))]
  })
  
  
  output$tigo_map_circles <- renderLeaflet({
    leaflet(df_reac_detail()) %>% 
      #addTiles() %>%
      addTiles(urlTemplate = Gtemplate, attribution = 'Google')%>%
      addCircleMarkers(
        radius = reac_mrc_metrics(),
        color = DriverColor(reac_mrc_metrics(),7,9),
        stroke = FALSE, fillOpacity = 0.5,
        label=~paste(paste("Fecha de respuesta:",fecha_respuesta_dt),
                     paste("Score:",reac_mrc_metrics()),
                     sep = " - ")
      )
  })
  
  output$tigo_map_pines <- renderLeaflet({
    leaflet() %>% 
      #setView(lng = -74, lat = 4.5, zoom = 6)%>% 
      addTiles(urlTemplate = Gtemplate, attribution = 'Google')%>%
      addAwesomeMarkers(data = df_reac_detail(),
                        label=~paste(paste("Fecha de respuesta:",fecha_respuesta_dt),
                                     paste("Score:",reac_mrc_metrics()),
                                     sep = " - "),
                        icon = ~icons(reac_mrc_metrics(),7,9),
                        group = ~mercado,
                        clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T),
                        labelOptions = labelOptions(noHide = F,
                                                    direction = 'auto',opacity = .6))  
  })
  ##################################################################################### 
  ##Regional View - Agregated
  
  ##Observe for each filter
  observe({
    #obs for mercado
    if("Todo" %in% input$tigo_filter_mercado_t2){
      ob_tigo_filter_mercado_t2<- as.character(unique(df_tigo_ver$mercado))}else{
        ob_tigo_filter_mercado_t2<-input$tigo_filter_mercado_t2
      }
    updateSelectInput(session,"tigo_filter_mercado_t2",
                      selected =ob_tigo_filter_mercado_t2 )
    #obs for canal
    if("Todo" %in% input$tigo_filter_canal_t2){
      ob_tigo_filter_canal_t2<- as.character(unique(df_tigo_ver$canal))}else{
        ob_tigo_filter_canal_t2<-input$tigo_filter_canal_t2
      }
    updateSelectInput(session,"tigo_filter_canal_t2",
                      selected =ob_tigo_filter_canal_t2 )
    #obs for journey
    if("Todo" %in% input$tigo_filter_journey_t2){
      ob_tigo_filter_journey_t2<- as.character(unique(df_tigo_ver$journey))}else{
        ob_tigo_filter_journey_t2<-input$tigo_filter_journey_t2
      }
    updateSelectInput(session,"tigo_filter_journey_t2",
                      selected =ob_tigo_filter_journey_t2 )
    #obs for bu
    if("Todo" %in% input$tigo_filter_bu_t2){
      ob_tigo_filter_bu_t2<- as.character(unique(df_tigo_ver$bu))}else{
        ob_tigo_filter_bu_t2<-input$tigo_filter_bu_t2
      }
    updateSelectInput(session,"tigo_filter_bu_t2",
                      selected =ob_tigo_filter_bu_t2 )
    #obs for verified
    if("Todo" %in% input$tigo_filter_verified_t2){
      ob_tigo_filter_verified_t2<- as.character(unique(df_tigo_ver$verified))}else{
        ob_tigo_filter_verified_t2<-input$tigo_filter_verified_t2
      }
    updateSelectInput(session,"tigo_filter_verified_t2",
                      selected =ob_tigo_filter_verified_t2 )
  })
  
  
  
  
  
  #DF to calculate metrics from the company side
  df_reac_agregated_company <- reactive({
    df_tigo_ver %>% 
      filter(fecha_respuesta %in% input$tigo_filter_fecha_t2) %>%
      filter(mercado %in% input$tigo_filter_mercado_t2) %>%
      filter(canal %in% input$tigo_filter_canal_t2) %>%
      filter(journey %in% input$tigo_filter_journey_t2) %>%
      filter(bu %in% input$tigo_filter_bu_t2) %>%
      filter(ver %in% input$tigo_filter_verified_t2)
  })
  
  
  ##
  tigo_metrics_list <- as.character(c("ltr","csat","ces"))
  tigo_metric_column <- reactive({
    df_column_tmp <- df_reac_agregated_company()
    
    if(input$tigo_controls_mrc_metrics_t2=="LTR"){
      metric <- df_column_tmp %>% select(ltr)
      metric <- as.character(metric$ltr)
      metric}
    else if(input$tigo_controls_mrc_metrics_t2 == "CSat"){
      metric <- df_column_tmp %>% select(csat)
      metric <- as.character(metric$csat)
      metric}
    else if(input$tigo_controls_mrc_metrics_t2 == "CES"){
      metric <- df_column_tmp %>% select(ces)
      metric <- as.character(metric$ces)
      metric}
    else{
      return(NULL)}
  })
  
  df_with_metric_column <- reactive({
    df_with_column_tmp <- df_reac_agregated_company()
    tmp_column <- tigo_metric_column()
    df_with_column_tmp <- df_with_column_tmp %>%
      select(-tigo_metrics_list)
    df_with_column_tmp <- cbind(df_with_column_tmp,metric=tmp_column)
    df_with_column_tmp <- df_with_column_tmp %>%
      group_by(country_id) %>%
      summarise(calculation=TIGO_CALCULATION(metric,
                                             as.character(input$tigo_controls_mra_calcs_t2)))
    df_with_column_tmp
  })
  
  #DF to add metrics to geo information
  geo_data_reac <- reactive({
    geo_data[which(geo_data$NAME %in% input$tigo_filter_mercado_t2 ),]
  })
  
  geo_data_reac_calc <-reactive({ 
    geo_data_tmp <- geo_data_reac()
    dg_tmp_calc <- df_with_metric_column()
    
    sp::merge(geo_data_tmp,dg_tmp_calc,by.x="UN",by.y="country_id")
  })
  
  
  
  
  output$tigo_map_agregado <- renderLeaflet({
    leaflet(data=geo_data_reac_calc()) %>%
      addTiles(urlTemplate = Gtemplate, attribution = 'Google')%>%
      addPolygons( fillColor = ~ DriverColor(calculation,0,30),
                   stroke = F,fillOpacity = 0.9,
                   highlight = highlightOptions(weight = 4,
                                                color = "red",
                                                fillOpacity = 0.7,
                                                bringToFront = TRUE),
                   label=~paste(paste0(input$tigo_controls_mra_calcs_t2,":")
                                ,calculation))
  })
  
  
  
}

