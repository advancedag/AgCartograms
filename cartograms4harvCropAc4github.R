
setwd("~/cartograms") # this is your working directory 

rm(list = ls())

library(tidyverse)
library(httr)
library(jsonlite)
library(tmap)
library(rgdal)
library(cartogram)
  
crops<-c("CORN", "SOYBEANS", "RICE", "COTTON", "SORGHUM", "WHEAT", "PEANUTS", "BARLEY", "CANOLA", "HAY", "OATS", "POTATOES", "PUMPKINS", 'RYE', "SAFFLOWER", 'SUGARBEETS', "SUNFLOWER", "TOBACCO", "TOMATOES")


for(i in 1:length(crops)){
  crop<-crops[i]

### acqure your API key from https://quickstats.nass.usda.gov/api
api.key <- 'xxxxx-xxxx-xxxx-xxxxx' #'
api.url4acres <- paste(
  'http://quickstats.nass.usda.gov/api/api_GET/?key=', api.key,
  '&source_desc=SURVEY',
  '&agg_level_desc=STATE',
  '&commodity_desc=', crop,
  '&reference_period_desc=YEAR',
  '&year__GE=2017',
  '&short_desc__LIKE=ACRES HARVESTED',
  '&statisticcat_desc=AREA HARVESTED',
  sep=''
)
api.return4acres <- GET(api.url4acres)

acresHarvested <-
  fromJSON(content(api.return4acres, 'text'))$data 

dat<-subset(acresHarvested, year==2017) 
dat<-subset(dat, dat$util_practice_desc!="SILAGE")
dat<-subset(dat, dat$util_practice_desc!="PROCESSING")
dat<-subset(dat, dat$prodn_practice_desc=="ALL PRODUCTION PRACTICES" | dat$prodn_practice_desc =="IN THE OPEN")
dat<-subset(dat, dat$class_desc=="ALL CLASSES")
dat$value<-as.numeric(gsub(",", "", dat$Value))
dat$value<-as.numeric(dat$value)
dat$percent<-round(dat$value/sum(dat$value)*100,1)
dat2<-cbind(dat$state_name,dat$value, dat$percent, dat$state_fips_code)  
dat3<-as.data.frame(dat2)
colnames(dat3)<-c("State", "acres", "% Total", "STATE_FIPS")
dat4<-subset(dat3, State!= "OTHER STATES")
dat4$acres<-as.numeric(as.character(dat4$acres))

# shapefiles are availalbe from:  https://www.census.gov/geo/maps-data/data/tiger-line.html
states         <- readOGR(dsn='.', layer='lower48peanuts2') # name of shpe file
states@data$id <- rownames(states@data)
dat4$STATEFP <- dat4$STATE_FIPS

dat4cartogram <- merge(states, dat4, by='STATEFP', all.x=TRUE)

afr<-spTransform(dat4cartogram, CRS("+init=epsg:3395"))
corn<-cartogram(afr, weight="acres", itermax = 5)

m<-tm_shape(corn) + tm_fill("acres", style="jenks", title= paste("2017 harvested ", crop, "  acres", sep="")) + 
  tm_borders() + tm_layout(frame=F, legend.outside = TRUE)

m2<-tm_shape(corn) + tm_fill("acres", style="jenks", title= paste("2017 harvested ", crop, "  acres", sep="")) + 
  tm_borders() + tm_layout(frame=F, legend.show = FALSE)

save_tmap(m, paste("cartogram", crop, ".png", sep=""), height=7)
save_tmap(m2, paste("cartogram", crop, "noLengend.png", sep=""), height=7)

}
