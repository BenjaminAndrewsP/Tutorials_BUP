rm(list = ls())
graphics.off()

install.packages("geodata")
library(geodata)
library(terra)
install.packages("predicts")
library(predicts)

occdata <- geodata::sp_occurrence("Phasianus", "colchicus", geo = T,
                                  removeZeros = T, start = 1, end = 10000)
dim(occdata)

wrld <- world(path = ".")
plot(wrld, xlim = c(-180,180), ylim = c(-80,80), col = "lightyellow", border = "lightgrey")
points(occdata$lon, occdata$lat, col = "darkorange", pch = 20)

dups <- duplicated(occdata[, c('lon','lat')])
sum(dups)
occ <- occdata[!dups,]

output_dir <- "/Users/benjaminandrews/Documents/MS_EEB/Biodiversity Under Presure/Tutorials_BUP/tutorial_3_sdm/data"
bio_glob <- worldclim_global(var="bio", res = 10, path = output_dir,
                             version="2.1")
dim(bio_glob)
summary(occ$lon)
summary(occ$lat)

e <- ext(-180, 180, -80, 80)
predictors <- crop(bio_glob, e)
names(predictors) <- substring(names(predictors), 11, 16)

plot(predictors, 1:9)
plot(predictors, 1)
points(occ$lon, occ$lat, col = "darkorange", pch = 16, cex = 0.2)

bg <- spatSample(predictors, 5000, "random", na.rm = T, as.points = T, ext = e)
plot(predictors, 1)
points(bg, cex=0.1)

occlatlon<-cbind(occ$lon,occ$lat)
presvals <- extract(predictors, occlatlon)
#presvals is the climate data for where the species is present
backvals <- values(bg)
#backvals is the climate data for the background data
bg_lonlat<-geom(bg)
lonlats<-rbind(occlatlon, bg_lonlat[,c("x","y")])
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(backvals)))
#The first column of pb  is a vector of 1s for presences and 0s for background data.
sdmdata <- data.frame(cbind(lonlats,pb, rbind(presvals, backvals)))

pairs(sdmdata[,4:7], cex=0.1)

sdmdata<-subset(sdmdata,is.na(bio_1)==F)
#here we're just removing a couple of rows where the climate data are NAs.
specdata<-as.data.frame(cbind(rep("Lagopus mutu",length(sdmdata[,1])),
                              sdmdata))
names(specdata)[1:4]<-c("species","longitude","latitude","presabs")
specdata<-subset(specdata,presabs==1)
backdata<-as.data.frame(cbind(rep("background",length(sdmdata[,1])),
                              sdmdata))
names(backdata)[1:4]<-c("","longitude","latitude","presabs")
backdata<-subset(backdata,presabs==0)

write.table(specdata[,-4],paste(output_dir,"/Lagopusmutu_swd.csv",sep=""),col.names=T,row.names=F,sep=",")
write.table(backdata[,-4],paste(output_dir,"/background.csv",sep=""),col.names=T,row.names=F,sep=",")

model <- MaxEnt(sdmdata[,-c(1:3)], sdmdata[,3], removeDuplicates = T)
plot(model)

predictedocc <- predict(model, predictors, args=c("outputformat=raw")) 
par(mfrow=c(2,1))
plot(predictedocc)
plot(predictedocc)
points(occlatlon,pch=".")

bio_fut<-cmip6_world(model='ACCESS-ESM1-5', ssp='245', time='2041-2060', var='bioc', res=10, path=output_dir)
fut_predictors<-crop(bio_fut,e)

plot(predictors,2)
plot(fut_predictors,2)

names(fut_predictors)<-names(predictors)
fut_predictedocc <- predict(model, fut_predictors, args=c("outputformat=raw")) 
par(mfrow=c(2,1))
plot(predictedocc,main="current")
plot(fut_predictedocc,main="2050")

