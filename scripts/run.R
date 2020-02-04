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
    filetext <-
      c("---",
        paste0("title: 全国疫情地图(", prefix, "级)", date),
        paste0("date: ", date),
        paste0("slug: ", filename),
        "---",
        "",
        paste0('<iframe seamless src="', link, '" width="100%" height="500"></iframe>'), 
        "",
        paste0('[点击这里全屏显示。](', link, ')'),
        "",
        "```{r, echo=FALSE}",
        'require(ncovr)',
        'ncov <- get_ncov()',
        'ncov$area$date <- as.character(as.Date(ncovr:::conv_time(ncov$area$updateTime)))',
        'ncov$area <- ncov$area[!duplicated(paste(ncov$area$provinceName, ncov$area$date)), ]',
        paste0('x <- ncov$area[ncov$area$date == "', date, '", 2:6]'),
        'knitr::kable(x, format = "html", caption = paste(date, "疫情数据表（", method, ")"), row.names = FALSE, col.names = c("名称", "确诊", "疑似", "治愈", "死亡"))',
        "```"
      )
    writeLines(filetext, pathname, useBytes = TRUE)
  }
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

## Create posts ----
for(i in ncov_dates){
  post_map(method = 'province', date = i)
  post_map(method = 'city', date = i)
}

## Build site ----
blogdown::install_hugo()
blogdown::build_site()