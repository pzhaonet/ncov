# Functions ----

## Packages ----
remotes::install_github('pzhaonet/ncovr')
require(ncovr)
require(leafletCN)
require(htmlwidgets)
require(htmltools)
# Sys.setlocale('LC_CTYPE', 'Chinese')

## Create map post ----
post_map <- function(method, date){
  prefix <- switch(method, 'province' = '省', 'city' = '市')
  filename <- paste0(method, '-map-', date)
  pathname <- paste0('content/post/', filename, '.Rmd')
  if(!file.exists(pathname)){
    link <- paste0('https://pzhaonet.github.io/ncov/leaflet/leafmap-', method, '-', date, '.html')
    filetext <- readLines('static/template/post-map.Rmd', encoding = 'UTF-8')
    filetext <- gsub("<<method>>", method, filetext)
    filetext <- gsub("<<method-zh>>", prefix, filetext)
    filetext <- gsub("<<date>>", date, filetext)
    writeLines(filetext, pathname, useBytes = TRUE)
  }
}

post_predict <- function(date){
  filename <- paste0('predict-', date)
  pathname <- paste0('content/post/', filename, '.Rmd')
  if(!file.exists(pathname)){
    filetext <- readLines('static/template/post-predict.Rmd', encoding = 'UTF-8')
    filetext <- gsub("<<date>>", date, filetext)
    writeLines(filetext, pathname, useBytes = TRUE)
  }
}

plot_predict <- function(province, ncov = ncov, ifplot = TRUE, addtitle = NA){
  #Dataset For a specific area
  Area <- ncov$area
  Area$updateTime <- ncovr:::conv_time(Area$updateTime)#Correct the time
  
  Region_all <- unique(Area$provinceShortName)
  Region_name <- Region_all[match(province, Region_all)]#Match regional name
  Region <- subset(Area,provinceShortName==Region_name)
  
  RegionTime <- aggregate(confirmedCount~updateTime,data=Region,sum)
  
  RegionTime$Date <- format(RegionTime$updateTime,"%m-%d")
  
  RegionDat <- aggregate(confirmedCount~Date,data=RegionTime,max)
  
  #No data availalbe for specific date
  RegionDat$Day <- 1:nrow(RegionDat)
  RegionDat$New <- with(RegionDat,confirmedCount-c(0,confirmedCount[-nrow(RegionDat)]))
  RegionDat$Date <-  as.Date(RegionDat$Date,"%m-%d")
  Length <- round(2.2*nrow(RegionDat),0)
  Dseq <- format(seq(RegionDat$Date[1], by = "day", length.out = Length ),"%m-%d")
  
  END <- NA
  Predict <- NA
  
  #Model logistic regression
  if(class(try(nls(confirmedCount~SSlogis(Day, Asym, xmid, scal),data= RegionDat), silent = TRUE)) != "try-error"){
    md <- nls(confirmedCount~SSlogis(Day, Asym, xmid, scal),data= RegionDat)
    Coe <- summary(md)$coefficients
    a <- Coe[1,1]
    xmax <-  2*Coe[2,1]
    
    #End date
    END <- Dseq [round(xmax,0)]
    #Predict
    Input=nrow(RegionDat)+1
    Predict <- round(a/(1+exp((Coe[2,1]-Input)/Coe[3,1])),0)
    
    # if(ifplot){
      #Plot the results
        par(mgp = c(2.5, 1, 0))
        with(RegionDat,plot(y=confirmedCount,x=Day,xlim=c(0,1.8*xmax),ylim=c(0,1.3*a),ylab="Number of cases",xlab="",bty='n',xaxt = "n"));title(ifelse(is.na(addtitle), '', Region_name))
        
        with(RegionDat,points(y=New,x=Day,col="grey",pch=19))
        
        #Smooth predict line
        PredS <- seq(0, 1.3*xmax, length = 100)
        lines(PredS, predict(md, list(Day = PredS )),col="red")
        
        #The daily increase case
        Lth<- round(1.3*xmax,0)
        newdat <- data.frame(Pred=1:Lth)
        newdat <- within(newdat, ypred <- predict(md,  list(Day = Pred )))
        newdat$Prednew <- with(newdat,ypred-c(0,ypred[-nrow(newdat)]))
        lines(x=newdat$Pred,y=newdat$Prednew,col="blue")
        
        axis(1, at=1:Length , labels=Dseq,cex.axis = 0.6,las=2)
        
        points(2,0.8*a)
        text(3,0.8*a,"Confirmed",cex=0.8,adj=0)
        segments(1.5,0.7*a,2.5,0.7*a,col="red")
        text(3,0.7*a,"Model fit",cex=0.8,adj=0)
        points(2,0.6*a,col="grey",pch=19)
        text(3,0.6*a,"Daily increase",cex=0.8,adj=0)
        
        segments(0,a,xmax,a,lty="dotted")
        segments(xmax,0,xmax,a,lty="dotted")
      }
      
}

calc_predict <- function(province, ncov = ncov, ifplot = FALSE, addtitle = NA){
  #Dataset For a specific area
  Area <- ncov$area
  Area$updateTime <- ncovr:::conv_time(Area$updateTime)#Correct the time
  
  Region_all <- unique(Area$provinceShortName)
  Region_name <- Region_all[match(province, Region_all)]#Match regional name
  Region <- subset(Area,provinceShortName==Region_name)
  
  RegionTime <- aggregate(confirmedCount~updateTime,data=Region,sum)
  
  RegionTime$Date <- format(RegionTime$updateTime,"%m-%d")
  
  RegionDat <- aggregate(confirmedCount~Date,data=RegionTime,max)
  
  #No data availalbe for specific date
  RegionDat$Day <- 1:nrow(RegionDat)
  RegionDat$New <- with(RegionDat,confirmedCount-c(0,confirmedCount[-nrow(RegionDat)]))
  RegionDat$Date <-  as.Date(RegionDat$Date,"%m-%d")
  Length <- round(2.2*nrow(RegionDat),0)
  Dseq <- format(seq(RegionDat$Date[1], by = "day", length.out = Length ),"%m-%d")
  
  END <- NA
  Predict <- NA
  
  #Model logistic regression
  if(class(try(nls(confirmedCount~SSlogis(Day, Asym, xmid, scal),data= RegionDat), silent = TRUE)) != "try-error"){
    md <- nls(confirmedCount~SSlogis(Day, Asym, xmid, scal),data= RegionDat)
    Coe <- summary(md)$coefficients
    a <- Coe[1,1]
    xmax <-  2*Coe[2,1]
    
    #End date
    END <- Dseq [round(xmax,0)]
    #Predict
    Input=nrow(RegionDat)+1
    Predict <- round(a/(1+exp((Coe[2,1]-Input)/Coe[3,1])),0)
    
    if(ifplot){
      #Plot the results
      myplot <- function(){
        par(mgp = c(2.5, 1, 0))
        with(RegionDat,plot(y=confirmedCount,x=Day,xlim=c(0,1.8*xmax),ylim=c(0,1.3*a),ylab="Number of cases",xlab="",bty='n',xaxt = "n"));title(ifelse(is.na(addtitle), '', Region_name))
        
        with(RegionDat,points(y=New,x=Day,col="grey",pch=19))
        
        #Smooth predict line
        PredS <- seq(0, 1.3*xmax, length = 100)
        lines(PredS, predict(md, list(Day = PredS )),col="red")
        
        #The daily increase case
        Lth<- round(1.3*xmax,0)
        newdat <- data.frame(Pred=1:Lth)
        newdat <- within(newdat, ypred <- predict(md,  list(Day = Pred )))
        newdat$Prednew <- with(newdat,ypred-c(0,ypred[-nrow(newdat)]))
        lines(x=newdat$Pred,y=newdat$Prednew,col="blue")
        
        axis(1, at=1:Length , labels=Dseq,cex.axis = 0.6,las=2)
        
        points(2,0.8*a)
        text(3,0.8*a,"Confirmed",cex=0.8,adj=0)
        segments(1.5,0.7*a,2.5,0.7*a,col="red")
        text(3,0.7*a,"Model fit",cex=0.8,adj=0)
        points(2,0.6*a,col="grey",pch=19)
        text(3,0.6*a,"Daily increase",cex=0.8,adj=0)
        
        segments(0,a,xmax,a,lty="dotted")
        segments(xmax,0,xmax,a,lty="dotted")
      }
      
      return(list(area = province, enddate = END, tomorrow = Dseq[nrow(RegionDat)+1], tomorrowcount = Predict, fig = myplot()))
      
    }
  }
  return(list(area = province, enddate = END, tomorrow = Dseq[nrow(RegionDat)+1], tomorrowcount = Predict))
}

## Get data ----
ncov <- get_ncov()
ncov$area$date <- as.character(as.Date(ncovr:::conv_time(ncov$area$updateTime)))
ncov$area <- ncov$area[!duplicated(paste(ncov$area$provinceName, ncov$area$date)), ]
ncov_dates <- unique(ncov$area$date)

## create maps ----
oldwd <- getwd()
setwd('static/leaflet')

for(i in ncov_dates){
  # province 
  filename <- paste0("leafmap-province-", i, ".html")
  if(!file.exists(filename)){
    leafMap <- plot_map(
      x = ncov$area[ncov$area$date == i, ], 
      key = c("confirmedCount", "suspectedCount", "curedCount", "deadCount")[1], 
      scale = "log", 
      method = c("province", "city")[1], 
      legend_title = paste0("确诊病例(", i, ")"), 
      filter = '待明确地区'
    )
    saveWidget(leafMap, filename)
  }

  # city
  filename <- paste0("leafmap-city-", i, ".html")
  if(!file.exists(filename)){
    leafMap <- plot_map(
      x = ncov$area[ncov$area$date == i, ], 
      key = c("confirmedCount", "suspectedCount", "curedCount", "deadCount")[1], 
      scale = "log", 
      method = c("province", "city")[2], 
      legend_title = paste0("确诊病例(", i, ")"), 
      filter = '待明确地区'
    )
    saveWidget(leafMap, filename)
  }
}
setwd(oldwd)

## Create map posts ----
if(!dir.exists('content/post/')) dir.create('content/post/')
for(i in ncov_dates){
  post_map(method = 'province', date = i)
  post_map(method = 'city', date = i)
}

## Create predict posts ----
post_predict(date = Sys.Date())

## Build site ----
blogdown::install_hugo()
blogdown::build_site()