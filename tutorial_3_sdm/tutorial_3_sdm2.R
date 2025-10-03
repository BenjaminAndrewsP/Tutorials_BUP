rm(list = ls())
graphics.off()

library(geodata)
library(terra)
install.packages("predicts")
library(predicts)

occdata <- geodata::sp_occurrence("Phasianus", "colchicus", geo = T,
                                  removeZeros = T, start = 1, end = 10000)
dim(occdata)

wrld <- world(path = ".")
plot(wrld, clim = c(-180,180), yclim = c(-80,80), col = "lightyellow", border = "lightgrey")
points(occdata$lon, occdata$lat, col = "darkorange", pch = 20)

occdata <- subset(occdata, lat > 0)
dups <- duplicated(occdata[, c('lon','lat')])
sum(dups)
occ <- occdata[!dups,]

output_dir <- "/Users/benjaminandrews/Documents/MS_EEB/Biodiversity Under Presure/Tutorials_BUP/tutorial_3_sdm/data"
bio_glob <- worldclim_global(var="bio", res = 10, path = output_dir,
                             version="2.1")
dim(bio_glob)
summary(occ$lon)
summary(occ$lat)

e <- ext(-156.919, 135.101, 20.76, 66.64)
predictors <- crop(bio_glob, e)

plot(predictors, 1:9)
plot(predictors, 1)
points(occ$lon, occ$lat, col = "darkorange", pch = 16, cex = 0.2)










