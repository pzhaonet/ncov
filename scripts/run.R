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
if(!dir.exists('static/data-download')) dir.create('static/data-download')
saveRDS(ncov_tidy, 'static/data-download/ncov_tidy.RDS')
saveRDS(ncov, 'static/data-download/ncov.RDS')


## Create map post ----
post_map <- function(method, date, language = c('en', 'zh')){
  
  prefix <- switch(method, 
                   'province' = if(language == 'zh') '省' else 'provinces', 
                   'city' = if(language == 'zh') '市' else 'cities')
  filename <- paste0(method, '-map-', date)
  pathname <- paste0('content/', language, '/post/', filename, '.Rmd')
  # if(!file.exists(pathname)){
    link <- paste0('https://pzhaonet.github.io/ncov/leaflet/leafmap-', method, '-', date, '.html')
    filetext <- readLines(paste0('static/template/post-map-', language, '.Rmd'), encoding = 'UTF-8')
    filetext <- gsub("<<method>>", method, filetext)
    filetext <- gsub("<<language>>", language, filetext)
    filetext <- gsub("<<method-zh>>", prefix, filetext)
    filetext <- gsub("<<date>>", date, filetext)
    writeLines(filetext, pathname, useBytes = TRUE)
  # }
}

post_predict <- function(date, language = c('en', 'zh')){
  filename <- paste0('predict-', date)
  pathname <- paste0('content/', language, '/post/', filename, '.Rmd')
  # if(!file.exists(pathname)){
    filetext <- readLines(paste0('static/template/post-predict-', language, '.Rmd'), encoding = 'UTF-8')
    filetext <- gsub("<<date>>", date, filetext)
    writeLines(filetext, pathname, useBytes = TRUE)
  # }
}

## Get data ----
# ncov <- get_ncov()
ncov$area$date <- as.Date(ncovr:::conv_time(ncov$area$updateTime))
ncov$area <- ncov$area[rev(order(ncov$area$date)), ]
ncov_tidy$area$date <- as.Date(ncov_tidy$area$updateTime)
ncov_tidy$area <- ncov_tidy$area[rev(order(ncov_tidy$area$date)), ]

ncov_dates <- as.character(unique(ncov$area$date))

## create maps ----
oldwd <- getwd()
setwd('static/leaflet')

for(i in ncov_dates){
  y <- ncov$area[ncov$area$date <= as.Date(i), ]
  y <- y[!duplicated(y$provinceName), ]
  x <- y[, c('provinceShortName', 'confirmedCount', 'curedCount', 'deadCount')]
  names(x)[1] <- 'provinceName'
  
  # province 
  filename <- paste0("leafmap-province-", i, ".html")
  # if(!file.exists(filename)){
    leafMap <- plot_map(
      x = y, 
      key = "confirmedCount", 
      scale = "log", 
      method = c("province", "city")[1], 
      legend_title = paste0("确诊病例(", i, ")"), 
      filter = '待明确地区'
    )
    saveWidget(leafMap, filename)
  # }

  # city
  filename <- paste0("leafmap-city-", i, ".html")
  
  x_city <- ncov_tidy$area[ncov_tidy$area$date <= as.Date(i), ]
  x_city <- x_city[as.Date(x_city$date) <= as.Date("2020-02-07"), ]
  x_city <- x_city[!duplicated(x_city$cityName), ]
  names(x_city)[1] <- 'provinceName'
  
  x <- x[x$provinceName %in% c("北京", "上海", "重庆", "天津"), ]
  if(nrow(x_city) > 0) x <- rbind(x_city[, c('provinceName', 'confirmedCount', 'curedCount', 'deadCount')], x)
  cities <- leafletCN::regionNames(mapName = "city")
  x_cities <- x[x$provinceName %in% cities, ]
  x_cities$key <- x_cities$confirmedCount
  
  # if(!file.exists(filename)){
    x_cities$key_log <- log10(x_cities$key)
    x_cities$key_log[x_cities$key == 0] <- NA
    leafMap <- geojsonMap_legendless(dat = as.data.frame(x_cities), 
                                   mapName = "city", palette = 'Reds', namevar = ~provinceName, 
                                   valuevar = ~key_log, popup = paste(x_cities$provinceName, 
                                                                      x_cities$key)) %>% 
      leaflet::addLegend("bottomright", 
                         bins = 4, pal = leaflet::colorNumeric(palette = 'Reds', 
                                                               domain = x_cities$key_log), values = x_cities$key_log, 
                         title = paste0("确诊病例(", i, ")"), labFormat = leaflet::labelFormat(digits = 0, 
                                                                                           transform = function(x) 10^x), opacity = 1)
    saveWidget(leafMap, filename)
  # }
}
setwd(oldwd)

## Create map posts ----
if(!dir.exists('content/en/')) dir.create('content/en/')
if(!dir.exists('content/en/post/')) dir.create('content/en/post/')
if(!dir.exists('content/zh/')) dir.create('content/zh/')
if(!dir.exists('content/zh/post/')) dir.create('content/zh/post/')
for(i in ncov_dates){
  for(j in c('zh', 'en')){
  post_map(method = 'province', date = i, language = j)
  post_map(method = 'city', date = i, language = j)
  }
}

## Create predict posts ----
for(i in as.character(seq.Date(Sys.Date() - 7, Sys.Date(), 1))) {
  for(j in c('zh', 'en')){
    post_predict(date = as.Date(i), language = j)
  }
}

## Build site ----
blogdown::install_hugo()
blogdown::build_site()
