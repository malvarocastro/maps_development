GeoVer<-function(df,geo_data,coun_col,lng=lng,lat=lat,surveyid="surveyid"){
  countries <- as.character(unique(df[,coun_col]))
  
  surveyid_list <- as.character() 
  for(i in countries){
    geo_tmp <- geo_data[which(geo_data$NAME == i ),]
    df_tmp <- df[which(df[,coun_col] == i ),]
    coordinates(df_tmp) <- ~lng + lat
    proj4string(df_tmp) <- proj4string(geo_tmp)
    index_list <- row.names(over( df_tmp,geo_tmp) %>% drop_na())
    ids <- df_tmp[index_list,surveyid]
    surveyid_list <- append(surveyid_list,ids$surveyid)
    
  } 
  df <- df %>% mutate(ver = ifelse(df$surveyid %in% surveyid_list,1,0))
  return(df)
}

DriverColor <- function(col,val1,val2){
  case_when(col < val1 ~ "red",
            col >= val1 & col <val2 ~ "yellow",
            col >= val2 ~ "green")
}

icons <-function(col,v1,v2){ 
  awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = DriverColor(col,v1,v2)
  )
}


