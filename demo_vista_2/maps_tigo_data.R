#df file for demo 2

library(dplyr)
library(lubridate)
library(rgdal)
library(tidyr)

######################## Reading data ########################
source("maps_tigo_ver_df.R")
source("maps_geo_functions.R")



######################## Reading geo data ########################
geo_data_sta_col  = readOGR(dsn="data", layer="COL_adm1",encoding = "ASCII")
geo_data_sta_bol  = readOGR(dsn="data", layer="BOL_adm1",encoding = "ASCII")
geo_data_sta_cri  = readOGR(dsn="data", layer="CRI_adm1",encoding = "ASCII")
geo_data_sta_gtm  = readOGR(dsn="data", layer="GTM_adm1",encoding = "ASCII")
geo_data_sta_hnd = readOGR(dsn="data", layer="HND_adm1",encoding = "ASCII")
geo_data_sta_pry  = readOGR(dsn="data", layer="PRY_adm1",encoding = "ASCII")
geo_data_sta_slv  = readOGR(dsn="data", layer="SLV_adm1",encoding = "ASCII")

geo_data_list_2 <- list()
geo_data_list_2[["Bolivia"]] <- geo_data_sta_bol
geo_data_list_2[["Colombia"]] <- geo_data_sta_col
geo_data_list_2[["Costa Rica"]] <- geo_data_sta_cri
geo_data_list_2[["El Salvador"]] <- geo_data_sta_slv
geo_data_list_2[["Guatemala"]] <- geo_data_sta_gtm
geo_data_list_2[["Honduras"]] <- geo_data_sta_hnd
geo_data_list_2[["Paraguay"]] <- geo_data_sta_pry

geo_data_mun_col  = readOGR(dsn="data", layer="COL_adm2",encoding = "ASCII")
geo_data_mun_bol  = readOGR(dsn="data", layer="BOL_adm2",encoding = "ASCII")
geo_data_mun_cri  = readOGR(dsn="data", layer="CRI_adm2",encoding = "ASCII")
geo_data_mun_gtm  = readOGR(dsn="data", layer="GTM_adm2",encoding = "ASCII")
geo_data_mun_hnd = readOGR(dsn="data", layer="HND_adm2",encoding = "ASCII")
geo_data_mun_pry  = readOGR(dsn="data", layer="PRY_adm2",encoding = "ASCII")
geo_data_mun_slv  = readOGR(dsn="data", layer="SLV_adm2",encoding = "ASCII")

geo_data_list <- list()
geo_data_list[["Bolivia"]] <- geo_data_mun_bol
geo_data_list[["Colombia"]] <- geo_data_mun_col
geo_data_list[["Costa Rica"]] <- geo_data_mun_cri
geo_data_list[["El Salvador"]] <- geo_data_mun_slv
geo_data_list[["Guatemala"]] <- geo_data_mun_gtm
geo_data_list[["Honduras"]] <- geo_data_mun_hnd
geo_data_list[["Paraguay"]] <- geo_data_mun_pry


########################################################################


paises <- as.character(unique(df_tigo_ver$mercado))
colsF <- c(names(df_tigo_ver),"ID_0","ID_1","NAME_1","ID_2","NAME_2")


df_tigo_geo_pais <- data.frame()
for(i in paises){
  df_tmp1 <- df_tigo_ver %>% filter(mercado==i)
  df_tmp2 <- df_tmp1
  coordinates(df_tmp2) <- ~lng + lat
  
  proj4string(df_tmp2) <- proj4string(geo_data_list[[i]])
  df_geo_info <- over( df_tmp2,geo_data_list[[i]]) 
  
  df_tmpf <- cbind(df_tmp1,df_geo_info) %>% 
              select(colsF) %>%
              mutate(NAME_1 = SubVoc(NAME_1),
                     NAME_2 = SubVoc(NAME_2))
  
  df_tigo_geo_pais <- rbind(df_tigo_geo_pais,df_tmpf)
  
  
}



names(df_tigo_geo_pais) <-  c(names(df_tigo_ver),"ID0","ID1","NAME1","ID2","NAME2")











