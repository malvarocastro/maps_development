######################################################
NPS <- function(x,scale="0-10"){
  x <- as.numeric(x) 
  x <- x[!is.na(x)]
  if(scale=="0-10" | scale == "1-10"){
    pro <- length(x[which(x>=9)])
    pas <- length(x[which(x>=7 & x<=8)])
    det <- length(x[which(x<=6)])
    return( round((pro-det)*100/length(x),2) )  
  } else if(scale=="1-5"){
    pro <- length(x[which(x==5)])
    pas <- length(x[which(x==4)])
    det <- length(x[which(x<=3)])
    return(round((pro-det)*100/length(x),2) )  
  } else {
    return(NULL)
  }
}
######################################################
TOP_BOX <- function(x,scale="0-10"){
  x <- as.numeric(x) 
  x <- x[!is.na(x)]
  if(scale=="0-10" | scale == "1-10"){
    top <- length(x[which(x>=9)])
    return(round(top*100/length(x),2) )  
  } else if(scale=="1-5"){
    top <- length(x[which(x==5)])
    return(round(top*100/length(x),2) )  
  } else {
    return(NULL)
  }
}
######################################################
TOP_2_BOX <- function(x,scale="0-10"){
  x <- as.numeric(x) 
  x <- x[!is.na(x)]
  if(scale=="0-10" | scale == "1-10"){
    top2 <- length(x[which(x>=7)])
    return(round(top2*100/length(x),2) )  
  } else if(scale=="1-5"){
    top2 <- length(x[which(x>=4)])
    return(round(top2*100/length(x),2) )  
  } else {
    return(NULL)
  }
}

######################################################
BOTTOM_BOX <- function(x,scale="0-10"){
  x <- as.numeric(x) 
  x <- x[!is.na(x)]
  if(scale=="0-10" | scale == "1-10"){
    bot <- length(x[which(x<=2)])
    return(round(bot*100/length(x),2) )  
  } else if(scale=="1-5"){
    bot <- length(x[which(x==1)])
    return(round(bot*100/length(x),2) )  
  } else {
    return(NULL)
  }
}

######################################################
TIGO_CALCULATION <- function(x,cal,scale="0-10"){
  if(cal=="NPS"){
    return(NPS(x,scale=scale))
  }else if(cal=="Top Box"){
    return(TOP_BOX(x,scale=scale))
  }else if(cal=="Top 2 Box"){
    return(TOP_2_BOX(x,scale=scale))
  }else if(cal=="Bottom Box"){
    return(BOTTOM_BOX(x,scale=scale))
  }else if(cal=="Promedio"){
    return(mean(x))
  }else{
    return(NULL)
  }
}
