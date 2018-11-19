mypath <- "Downloads/dataTEMP/"
setwd(mypath)

# read all ascii files and convert them into a raster stack
# activate relevant raster packages
library(sp)
library(raster)

lucas <- brick("DFD_LULC_DE_2014_v1_clip_Steigerwald_clip_ResearchStation.tif")
timescan <- brick("TimeScan_EAGLE_AOI_UTM_WGS84.tif")
guf <- raster("GUF_EAGLE_AOI_UTM_WGS84.tif")
srtm <- raster("SRTM.tif")
s2 <- brick("S2_2017_06_19.tif")

lucas.2 <- projectRaster(lucas,s2, method = "ngb")
lucas.3 <- crop(lucas.2,s2)
lucas.4 <- resample(lucas.3,s2)

timescan.2 <- projectRaster(timescan,s2)
timescan.3 <- crop(timescan.2,s2)
timescan.4 <- resample(timescan.3,s2)

guf.2 <- projectRaster(guf,s2)
guf.3 <- crop(guf.2,s2)
guf.4 <- resample(guf.3,s2)
guf.4[guf.4!=255] <- NA
guf.4[guf.4==255] <- 1

stackALL <- stack(lucas.4,s2,srtm,timescan.4,guf.4)

names(stackALL)



filenames <- grep("LC*|MOD*", list.files(path = ".", pattern="*.grd$"), value=T)
filenames


for (i in 0:length(filenames)){
  if (i == 0){
    # remove the raster in case it already exists to avoid duplicate entries
    print("remove old raster")
    rm(my_raster)
    # for the first run define our final raster file ...
    my_raster <- stackALL
    print("my raster defined")
  } else {
    # ... and fill it with each additional run with another layer
    print("start again")
    print(filenames[i])
    b_raster <- raster(filenames[i],1)
    summary(b_raster)
    b_raster <- projectRaster(b_raster,my_raster)
    b_raster <- crop(b_raster, my_raster)
    b_raster <- resample(b_raster, my_raster)
    b_raster <- b_raster/10000
    print("resample done")
    my_raster <- stack(my_raster, b_raster)
    print("stack done")
    # Delete all variables except for the raster stack "my_raster"
    rm(i, b_raster)
  }
}

writeRaster(my_raster, "stack_all_data_Steigerwald.grd", overwrite=T)

my_raster.2 <- my_raster[[c(1:27, 44:56)]]

names(my_raster.2)
 
namesData <- c("LUCAS_LC", "S2.1", "S2.2", "S2.3", "S2.4", "S2.5", "S2.6", "S2.7", "S2.8", "S2.9", "S2.10", "SRTM", "TimeScan.NDBImax", "TimeScan.NDBImin", "TimeScan.NDBIavg", "TimeScan.NDBIsd", "TimeScan.NDBIslope", "TimeScan.mNDWImax", "TimeScan.mNDWImin", "TimeScan.mNDWIavg", "TimeScan.mNDWIsd", "TimeScan.mNDWIslope", "TimeScan.NDVImax", "TimeScan.NDVImin", "TimeScan.NDVIavg", "TimeScan.NDVIsd", "TimeScan.NDVIslope", "GUF", "L8.evi", "L8.msavi", "L8.ndmi", "L8.ndvi", "L8.savi", "L7.evi", "L7.msavi", "L7.ndmi", "L7.ndvi", "L7.savi", "MOD.evi","MOD.ndvi")

names(my_raster.2) <- namesData

names(my_raster.2)

writeRaster(my_raster.2, "stack_all_data_Steigerwald_subset_withNames.grd", overwrite=T)

my_raster.2

my_raster.2 <- brick("stack_all_data_Steigerwald_subset_withNames.grd")

# poi <- as.data.frame(sampleRandom(my_raster.2,10000))
poi.spdf <- sampleStratified(my_raster.2$LUCAS_LC,300, sp=TRUE)

poi <- as.data.frame(extract(my_raster.2,poi.spdf))

poi <- cbind(poi,data.frame(poi.spdf@coords))

poi$LCname <- poi$LUCAS_LC 

poi$LCname[poi$LCname==1] <- c("urban")
poi$LCname[poi$LCname==2] <- c("cropland")
poi$LCname[poi$LCname==3] <- c("broadleaf_woodland")
poi$LCname[poi$LCname==4] <- c("conferious_woodland")
poi$LCname[poi$LCname==5] <- c("mixed_woodland")
poi$LCname[poi$LCname==6] <- c("shrubland")
poi$LCname[poi$LCname==7] <- c("grassland")
poi$LCname[poi$LCname==8] <- c("bare")
poi$LCname[poi$LCname==9] <- c("water")
poi$LCname[poi$LCname==10] <- c("wetland")

poi.spdf@data <- poi
library(rgdal)
writeOGR(poi.spdf,"Steigerwald_sample_point_with_data.shp", driver="ESRI Shapefile", "data")

plot(my_raster.2,1)
plot(poi.spdf,add=T)

unique(poi$LUCAS_LC)

summary(poi)

names(poi)


write.csv(poi,"Steigerwald_sample_points_all_data_subset_withNames.csv")

poi <- read.csv("Steigerwald_sample_points_all_data_subset_withNames.csv")

# alternative import methods
library(RCurl)
poi <- read.csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")

library(tidyverse)
poi <- read_csv("sample_points_all_data_subset_withNames.csv")
poi <- read_csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")


df <- poi

library(ggplot2)

a <- ggplot(df, aes(x=L8.ndvi, y=L8.savi)) + geom_point()

ggplot(df, aes(x=L8.ndvi, y=L8.savi, colour=SRTM)) + geom_point()

ggplot(df, aes(x=L8.ndvi, y=L8.savi, colour=SRTM)) + geom_point() + geom_smooth()

ggplot(df, aes(x=L8.ndvi, y=L8.savi, colour=SRTM)) + geom_point() + geom_smooth() + facet_wrap(~LCname)

pdf("landcover_vs_L8savi_ndvi.pdf",width=12,height=4)
ggplot(poi, aes(x = L8.ndvi, y = L8.savi)) +
  # geom_point(size=2)+
  geom_point(aes(color=LCname),size=2)+
  facet_grid(. ~ LCname)
dev.off()

ggplot(df, aes(x=LCname, y=L8.savi)) +  
  geom_boxplot(alpha=.5) + 
  geom_point(aes(color=SRTM), alpha=.7,size=1.5,position=position_jitter(width = .25,height=0)) 

  
ggplot() + geom_point(data=df, aes(LCname, L8.savi, colour=SRTM))

ggplot(df, aes(x=LCname, y=L8.savi)) + 
  geom_jitter()

ggplot(df, aes(x=LCname, y=L8.savi, colour=SRTM)) + 
  geom_jitter()

ggplot(df, aes(x=LCname, y=L8.savi)) + 
  geom_violin()

ggplot(df, aes(x=TimeScan.NDVIavg, fill=LCname)) + 
  geom_density(alpha=0.3)

ggplot(df, aes(x=LCname, y=L8.savi)) + 
   geom_jitter(aes(alpha=SRTM, size=TimeScan.NDVIsd, colour=L8.ndvi))+
  geom_boxplot(alpha=.5)

# df to spdf

spdf <- df
names(spdf)
library(sp)
coordinates(spdf) <- ~x+y

spdf

plot(spdf)
plot

library(rgdal)
test <- readOGR("sample_point_with_data.shp")
plot(test,add=T, col="blue")

# #############
# spatial domain



library(RStoolbox)

# plot of input data for overview

p1 <- ggR(my_raster.2$SRTM, geom_raster = T)+
  scale_fill_gradient(low="lightblue", high="darkblue", name ="elevation", na.value = NA)+
  labs(x="",y="")+
  ggtitle("SRTM")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=10))+
  theme(legend.title = element_text(size = 10, face = "bold"))+
  theme(legend.text = element_text(size = 6))+
  theme(axis.text.y = element_text(angle=45, size=6))+
  scale_y_continuous(breaks = seq(5527000,5538000,4000))+
  xlab("")+
  ylab("")

p2 <- ggR(my_raster.2$L8.ndvi, geom_raster = T)+
  scale_fill_gradient2(low="brown", mid="yellow", high="darkgreen", name ="NDVI", na.value = NA)+
  labs(x="",y="")+
  ggtitle("Landsat 8 - 2018")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=10))+
  theme(legend.title = element_text(size = 10, face = "bold"))+
  theme(legend.text = element_text(size = 6))+
  theme(axis.text.y = element_text(angle=45, size=6))+
  scale_y_continuous(breaks = seq(5527000,5538000,4000))+
  xlab("")+
  ylab("")

p3 <- ggR(my_raster.2$MOD.evi, geom_raster = T)+
  scale_fill_gradient2(low="brown", mid="yellow", high="darkgreen", name ="EVI", na.value = NA)+
  labs(x="",y="")+
  ggtitle("MODIS - 2016")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=10))+
  theme(legend.title = element_text(size = 10, face = "bold"))+
  theme(legend.text = element_text(size = 6))+
  theme(axis.text.y = element_text(angle=45, size=6))+
  scale_y_continuous(breaks = seq(5527000,5538000,4000))+
  xlab("")+
  ylab("")

p4 <- ggR(my_raster.2$S2.5, geom_ras
          ter = T, stretch = "lin")+
  scale_fill_gradient2(low="brown", mid='yellow', high="darkgreen", name ="band 5", na.value = NA)+
  labs(x="",y="")+
  ggtitle("Sentinel 2 - 19.06.2017")+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=10))+
  theme(legend.title = element_text(size = 10, face = "bold"))+
  theme(legend.text = element_text(size = 6))+
  theme(axis.text.y = element_text(angle=45, size=6))+
  scale_y_continuous(breaks = seq(5527000,5538000,4000))+
  xlab("")+
  ylab("")

library(gridExtra)
pdf("Steigerwald_data_overview.pdf", width = 25, height = 6)
grid.arrange(p1, p2, p3, p4, ncol=4)
dev.off()



