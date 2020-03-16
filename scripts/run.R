# file.remove(list.files('content/en/post/', pattern = '.*.Rmd', full.names = TRUE))
# file.remove(list.files('content/zh/post/', pattern = '.*.Rmd', full.names = TRUE))

# Functions ----

## Packages ----
install.packages("static/rpkg/ncovr_0.0.10.tar.gz", repos = NULL, type = "source")
# remotes::install_github('pzhaonet/ncovr')
require(ncovr)
require(leafletCN)
require(htmlwidgets)
require(htmltools)
require(dplyr)
Sys.setlocale('LC_CTYPE', 'Chinese')

# update data
## download all data from api
# ncov <- get_ncov(port = c('area?latest=0', 'overall', 'provinceName', 'news?num=10000', 'rumors?num=10000'), method = 'api')
# names(ncov) <- c('area', 'overall', 'provinceName', 'news', 'rumors')
# range(ncovr:::conv_time(ncov$area$updateTime))
# if(!dir.exists('static/data-download')) dir.create('static/data-download')
# saveRDS(ncov, 'static/data-download/ncov.RDS')
# ncov_tidy <- ncovr:::conv_ncov(ncov)
# saveRDS(ncov_tidy, 'static/data-download/ncov_tidy.RDS')

## update with the latest data
ncov <- readRDS('static/data-download/ncov.RDS')
# ncov_tidy <- readRDS('static/data-download/ncov_tidy.RDS')
dim(ncov$area) # 14177    24

ncov_new <- get_ncov(method = "json")
if(ncov_new$area$updateTime[1] > ncov$area$updateTime[1]){
  ncov$area$province_en <- unlist(ncov$area$province_en)
  ncov_new$area$province_en <- unlist(ncov_new$area$province_en)
  ncov$area <- bind_rows(ncov$area, ncov_new$area)
}
dim(ncov$area) # 14177    24

ncov$overall <- ncov_new$overall

ncov$news <- bind_rows(ncov$news, ncov_new$news)
ncov$news <- ncov$news[!duplicated(ncov$news$title), ]

ncov$rumors <- bind_rows(ncov$rumors, ncov_new$rumors)
ncov$rumors <- ncov$rumors[!duplicated(ncov$rumors$title), ]

# ncov_tidy <- ncovr:::conv_ncov(ncov)

# save data
if(!dir.exists('static/data-download')) dir.create('static/data-download')
saveRDS(ncov, 'static/data-download/ncov.RDS')
# saveRDS(ncov_tidy, 'static/data-download/ncov_tidy.RDS')

## Create map post ----
post_map <- function(method, date, language = c('en', 'zh')){
  
  prefix <- switch(method, 
                   'province' = if(language == 'zh') '省' else 'provinces', 
                   'city' = if(language == 'zh') '市' else 'cities', 
                   'country' = if(language == 'zh') '国' else 'countries')
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

## process time column ----
ncov$area$updateTime2 <- ncovr:::conv_time(ncov$area$updateTime)
ncov$area$date <- as.Date(ncov$area$updateTime2)
ncov$area <- ncov$area[rev(order(ncov$area$updateTime2)), ]
ncov_dates <- as.character(unique(ncov$area$date))

## create maps ----
oldwd <- getwd()
setwd('static/leaflet')

# regionNames('world')[order(regionNames('world'))]
countryname <- data.frame(ncovr = c("United Kingdom", "United States of America", "New Zealand", "Kampuchea (Cambodia )"),
                          leafletNC = c("UnitedKingdom", "UnitedStates", "NewZealand", "Cambodia"), 
                          stringsAsFactors = FALSE)

# i <- ncov_dates[1]
for(i in ncov_dates[1:3]){
  y <- ncov$area[ncov$area$date <= as.Date(i), ]
  y <- y[!duplicated(y$provinceName), ]
  names(y)
  x <- y[, c('provinceName', c('confirmedCount', 'curedCount', 'deadCount'))]
  # names(x) <- c('provinceName', 'confirmedCount', 'curedCount', 'deadCount')
  
  # # province 
  # filename <- paste0("leafmap-province-", i, ".html")
  # # if(!file.exists(filename)){
  #   leafMap <- plot_map(
  #     x = as.data.frame(x), 
  #     key = "confirmedCount", 
  #     scale = "log", 
  #     method = c("province", "city")[1], 
  #     legend_title = paste0("确诊病例(", i, ")"), 
  #     filter = '待明确地区'
  #   )
  #   saveWidget(leafMap, filename)
  # # }
  # 
  #   # city
  # filename <- paste0("leafmap-city-", i, ".html")
  # 
  # # if(!file.exists(filename)){
  # x_city <- ncov_tidy$area[ncov_tidy$area$date <= as.Date(i) & !ncov_tidy$area$provinceShortName %in% c("北京", "上海", "重庆", "天津"), ]
  # # x_city <- x_city[as.Date(x_city$date) <= as.Date("2020-02-07"), ]
  # x_city <- x_city[!duplicated(x_city$cityName), c('cityName', 'confirmedCount', 'curedCount', 'deadCount')]
  # names(x_city)[1] <- 'provinceName'
  # 
  # x <- x[x$provinceName %in% c("北京", "上海", "重庆", "天津"), ]
  # if(nrow(x_city) > 0) x <- rbind(x_city[, c('provinceName', 'confirmedCount', 'curedCount', 'deadCount')], x)
  # cities <- leafletCN::regionNames(mapName = "city")
  # x_cities <- x[x$provinceName %in% cities, ]
  # x_cities$key <- x_cities$confirmedCount
  # 
  #   x_cities$key_log <- log10(x_cities$key)
  #   x_cities$key_log[x_cities$key == 0] <- NA
  #   leafMap <- geojsonMap_legendless(dat = as.data.frame(x_cities), 
  #                                  mapName = "city", palette = 'Reds', namevar = ~provinceName, 
  #                                  valuevar = ~key_log, popup = paste(x_cities$provinceName, 
  #                                                                     x_cities$key)) %>% 
  #     leaflet::addLegend("bottomright", 
  #                        bins = 4, pal = leaflet::colorNumeric(palette = 'Reds', 
  #                                                              domain = x_cities$key_log), values = x_cities$key_log, 
  #                        title = paste0("确诊病例(", i, ")"), labFormat = leaflet::labelFormat(digits = 0, 
  #                                                                                          transform = function(x) 10^x), opacity = 1)
  #   saveWidget(leafMap, filename)
  # # }
    
    # country 
    x <- data.frame(countryEnglishName = y$countryEnglishName,
                    countryName = y$countryName, 
                    confirmedCount = y$confirmedCount, 
                    stringsAsFactors = FALSE)
    loc <- which(x$countryEnglishName %in% countryname$ncovr)
    x$countryEnglishName[loc] <- countryname$leafletNC[match(x$countryEnglishName[loc], countryname$ncovr)]
    
    x$countryEnglishName2 = x$countryEnglishName # for taiwan
    
    # x_other <- x[!is.na(x$countryEnglishName) & x$countryEnglishName != 'China', ]
    # x_china <- data.frame(countryEnglishName = 'China',
    #                       countryName = unique(x[!is.na(x$countryEnglishName) & x$countryEnglishName == 'China', 'countryName']),
    #                       confirmedCount = sum(x[!is.na(x$countryEnglishName) & x$countryEnglishName == 'China', 'confirmedCount']),
    #                       countryEnglishName2 = 'China') 
    # x_taiwan <- x_china
    # x_taiwan$countryEnglishName2 = "Taiwan"
    # x <- rbind(x_other, x_china, x_taiwan)
    x <- x[!is.na(x$countryEnglishName),]
    filename <- paste0("leafmap-country-", i, ".html")
    # if(!file.exists(filename)){
    leafMap <- plot_map(
      x = x, 
      key = "confirmedCount", 
      scale = "log", 
      method = 'country', 
      legend_title = paste0("Cnfrm 确诊"), 
      filter = '待明确地区'
    )
    saveWidget(leafMap, filename)
    # }
}
setwd(oldwd)


## create ts ----
if(!dir.exists('static/ts')) dir.create('static/ts')
setwd('static/ts')
countryname <- data.frame(ncovr = c("United Kingdom", "United States of America", "New Zealand", "Kampuchea (Cambodia )"),
                          leafletNC = c("UnitedKingdom", "UnitedStates", "NewZealand", "Cambodia"), 
                          stringsAsFactors = FALSE)

x_ts <- ncov$area[, c('countryEnglishName', 'countryName', 'date', 'confirmedCount', 'curedCount', 'deadCount')] %>% 
  group_by(countryEnglishName, date) %>% 
  summarise(confirmed = max(confirmedCount), 
            cured = max(curedCount), 
            dead = max(deadCount)) %>% 
  ungroup() %>% 
  filter(!is.na(countryEnglishName)) %>% 
  # filter(!is.na(countryEnglishName) & !countryEnglishName == 'China') %>% 
  as.data.frame()
loc <- which(x_ts$countryEnglishName %in% countryname$ncovr)
x_ts$countryEnglishName[loc] <- countryname$leafletNC[match(x_ts$countryEnglishName[loc], countryname$ncovr)]

for(i in unique(x_ts$countryEnglishName)){
  ts_fig <- plot_ts(x_ts, area = i, area_col = "countryEnglishName", date_col = "date", ts_col = c("confirmed", "cured", "dead"))  
  filename <- paste0("ts-country-", i, ".html")
  saveWidget(ts_fig, filename)
}
setwd(oldwd)

## Create map posts ----
if(!dir.exists('content/en/')) dir.create('content/en/')
if(!dir.exists('content/en/post/')) dir.create('content/en/post/')
if(!dir.exists('content/zh/')) dir.create('content/zh/')
if(!dir.exists('content/zh/post/')) dir.create('content/zh/post/')
for(i in ncov_dates[1:3]){
  for(j in c('zh', 'en')){
  # post_map(method = 'province', date = i, language = j)
  # post_map(method = 'city', date = i, language = j)
  post_map(method = 'country', date = i, language = j)
  }
}

## Create predict posts ----
# for(i in as.character(seq.Date(Sys.Date() - 2, Sys.Date(), 1))) {
#   for(j in c('zh', 'en')){
#     post_predict(date = as.Date(i), language = j)
#   }
# }

## Build site ----
blogdown::install_hugo()
blogdown::build_site()

