library(dplyr)
library(lubridate)
library(rgdal)
library(tidyr)

#############################################
## Load the shape file to a Spatial Polygon Data Frame (SPDF) using the readOGR() function
geo_data = readOGR(dsn="data",
                   layer="TM_WORLD_BORDERS_SIMPL-0.3")

##run
# setwd("C:/Users/alvar/OneDrive/Escritorio/geoData/COL_adm/")
# geo_data_state_col  = readOGR(dsn=getwd(), layer="COL_adm1",encoding = "UTF-8")
# geo_data_mun_col  = readOGR(dsn=getwd(), layer="COL_adm1",encoding = "UTF-8")
# 
# setwd("C:/Users/alvar/OneDrive/Escritorio/geoData/BOL_adm/")
# geo_data_state_bol  = readOGR(dsn=getwd(), layer="BOL_adm1",encoding = "UTF-8")
# geo_data_mun_bol  = readOGR(dsn=getwd(), layer="BOL_adm1",encoding = "UTF-8")
# 
# setwd("C:/Users/alvar/OneDrive/Escritorio/geoData/CRI_adm/")
# geo_data_state_cri  = readOGR(dsn=getwd(), layer="CRI_adm1",encoding = "UTF-8")
# geo_data_mun_cri  = readOGR(dsn=getwd(), layer="CRI_adm1",encoding = "UTF-8")
# 
# setwd("C:/Users/alvar/OneDrive/Escritorio/geoData/PRY_adm/")
# geo_data_state_pry  = readOGR(dsn=getwd(), layer="PRY_adm1",encoding = "UTF-8")
# geo_data_mun_pry  = readOGR(dsn=getwd(), layer="PRY_adm1",encoding = "UTF-8")
# 
# setwd("C:/Users/alvar/OneDrive/Escritorio/geoData/PAN_adm/")
# geo_data_state_pan  = readOGR(dsn=getwd(), layer="PAN_adm1",encoding = "UTF-8")
# geo_data_mun_pan  = readOGR(dsn=getwd(), layer="PAN_adm1",encoding = "UTF-8")
# 
# setwd("C:/Users/alvar/OneDrive/Escritorio/geoData/SLV_adm/")
# geo_data_state_slv = readOGR(dsn=getwd(), layer="SLV_adm1",encoding = "UTF-8")
# geo_data_mun_slv  = readOGR(dsn=getwd(), layer="SLV_adm1",encoding = "UTF-8")

#################################################


## we load the sources we need
#setwd("C:/Users/alvar/OneDrive/Escritorio/maps/Apps/Test/")
source("maps_geo_functions.R")

## we read the csv of the company
df_tigo <- read.csv("data/export.csv",
                    sep = "|",
                    encoding = "UTF-8")
## we rename th column as needed
 names(df_tigo) <- c("mercado","ltr","csat","ces","lat","lng","canal",
                     "fecha_de_respuesta","fdc","comment","surveyid","journey",
                     "bu") 

##we generate a df with lng and lat of each contry
df_lng_lat <- data.frame(mercado=geo_data$NAME,
                          gral_lng=geo_data$LON,
                          gral_lat=geo_data$LAT)

##we generate a df with id from de geo object and the names we have from the company
country_codes <- data.frame(country_id=c(68,170,188,222,320,340,558,600),
                            mercado=c("Bolivia","Colombia","Costa Rica","El Salvador",
                                      "Guatemala","Honduras","Nicaragua","Paraguay"))

## we clean the df od the company
 df_tigo_c <- df_tigo %>% 
   ##################
            filter(mercado != "Nicaragua") %>%
            filter(! is.na(lat)  & !lat >90 & !lat < (-90)) %>%
            filter(! is.na(lng) & !lng >180 & !lng < (-180)) %>%
            filter(! is.na(mercado)) %>%
            filter(! is.na(canal)) %>%
            select(-fdc) %>%
            mutate(fecha_respuesta= as_date(fecha_de_respuesta),
                   fecha_respuesta_dt = as_datetime(fecha_de_respuesta),
                   bu = ifelse(bu=="No","B2C","B2B"))
 
## we make some joins to heve country codes and lng and lat with the company data
df_tigo_c <- merge(df_tigo_c,country_codes,by="mercado",all.x=T)
df_tigo_c <- merge(df_tigo_c,df_lng_lat,by="mercado",all.x=T)

## we verify wich data points are not in the correct country and we mark them
df_tigo_ver <- GeoVer(df_tigo_c,geo_data,"mercado") %>% 
                mutate(ver=ifelse(ver==1,"Si","No"))


###
tigoMetrics <- c("LTR","CSat","CES")
tigoCalcs <- c("NPS","Top Box","Top 2 Box","Bottom Box")