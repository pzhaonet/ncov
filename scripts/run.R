# Functions ----

## Packages ----
remotes::install_github('pzhaonet/ncovr')
require(ncovr)
require(leafletCN)
require(htmlwidgets)
require(htmltools)
Sys.setlocale('LC_CTYPE', 'Chinese')

## backup data
ncov <- get_ncov(port = c('area?latest=0', 'overall', 'provinceName', 'news', 'rumors'), method = 'api')
names(ncov)[1] <- 'area'
ncov_tidy <- ncovr:::conv_ncov(ncov)
saveRDS(ncov_tidy, 'data/ncov_tidy.RDS')
saveRDS(ncov, 'data/ncov.RDS')


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
  # if(!file.exists(pathname)){
    filetext <- readLines('static/template/post-predict.Rmd', encoding = 'UTF-8')
    filetext <- gsub("<<date>>", date, filetext)
    writeLines(filetext, pathname, useBytes = TRUE)
  # }
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