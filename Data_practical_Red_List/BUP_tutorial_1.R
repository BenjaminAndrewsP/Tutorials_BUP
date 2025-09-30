rm(list = ls())
graphics.off()
getwd()

pol <- read.csv("Data_practical_Red_List/pollution/assessments.csv")
clim <- read.csv("Data_practical_Red_List/climate/assessments.csv")
inv <- read.csv("Data_practical_Red_List/invasion/assessments.csv")
land <- read.csv("Data_practical_Red_List/landuse/assessments.csv")
expl <- read.csv("Data_practical_Red_List/overexploitation/assessments.csv")

str(pol)
unlist(lapply(pol, class))
unlist(lapply(clim, class))

nrow(pol)
length(unique(pol$scientificName))

nrow(clim)
length(unique(clim$scientificName))

nrow(inv)
length(unique(inv$scientificName))
if(nrow(inv) != length(unique(inv$scientificName))) {
  inv <- inv[-which(duplicated(inv$scientificName)),]
}

nrow(land)
length(unique(land$scientificName))
if(nrow(land) != length(unique(land$scientificName))) {
  land <- land[-which(duplicated(land$scientificName)),]
}

nrow(expl)
length(unique(expl$scientificName))
if(nrow(expl) != length(unique(expl$scientificName))) {
  expl <- expl[-which(duplicated(expl$scientificName)),]
}

clim$pol <- 0
clim$land <- 0
clim$inv <- 0
clim$expl <- 0
clim$clim <- 1
clim

pol$pol <- 1
pol$land <- 0
pol$inv <- 0
pol$expl <- 0
pol$clim <- 0
pol

land$pol <- 0
land$land <- 1
land$inv <- 0
land$expl <- 0
land$clim <- 0
land

inv$pol <- 0
inv$land <- 0
inv$inv <- 1
inv$expl <- 0
inv$clim <- 0
inv

expl$pol <- 0
expl$land <- 0
expl$inv <- 0
expl$expl <- 1
expl$clim <- 0
expl

df <- rbind(expl, inv, land, pol, clim)

length(unique(df$scientificName))
dim(df)

?which
?duplicated

df.dup <- which(duplicated(df$scientificName))
df[df.dup, 1:23]

library(dplyr)

df.red <- distinct(df[df.dup, 1:23])
df.red
View(df.red)

for(i in 1:nrow(df.red)) {
  row.number <- which(df$scientificName == df.red$scientificName [i])
  row.selected <- df[row.number,]
  new.row <- data.frame(c(row.selected[1,1:23],colSums(row.selected[,24:28])))
  df <- df[-row.number,]
  df <- rbind(df,new.row)
  }
dim(df)

##General patterns
install.packages("devtools")
library("ggVennDiagram")

colSums(df[,24:28])
barplot(colSums(df[,24:28]))

ven.list <- list(
  invasions <- inv$scientificName,
  landuse <- land$scientificName,
  climate <- clim$scientificName,
  overexplotation <- expl$scientificName,
  pollution <- pol$scientificName
)

ggVennDiagram(ven.list,
              label_alpha = 0,
              set_color = c("red","green","blue","purple","orange"))

install.packages("UpSetR")
library(UpSetR)

upset(data=df[c(3,24:28)],nsets=5)

taxo.pol <- read.csv("Data_practical_Red_List/Pollution/taxonomy.csv")
taxo.clim <- read.csv("Data_practical_Red_List/Climate/taxonomy.csv")
taxo.inv <- read.csv("Data_practical_Red_List/Invasion/taxonomy.csv")
taxo.expl <- read.csv("Data_practical_Red_List/Overexploitation/taxonomy.csv")
taxo.land <- read.csv("Data_practical_Red_List/Landuse/taxonomy.csv")

taxo.df <- as.data.frame(rbind(taxo.pol,taxo.clim,taxo.inv,taxo.expl,taxo.land))
taxo.df <- distinct(taxo.df)
dim(taxo.df)
dim(df)

taxo.df <- taxo.df[order(taxo.df$scientificName),]
df <- df[order(df$scientificName),]

table(taxo.df$className)

threats.mammals <- df[which(taxo.df$className == "MAMMALIA"),]
threats.birds <- df[which(taxo.df$className=="AVES"),]
threats.fish <- df[which(taxo.df$className=="ACTINOPTERYGII"),]
threats.amphibian <- df[which(taxo.df$className=="AMPHIBIA"),]
threats.gastropode <- df[which(taxo.df$className=="GASTROPODA"),]
threats.flower <- df[which(taxo.df$className=="MAGNOLIOPSIDA"),]
threats.reptile <- df[which(taxo.df$className=="REPTILIA"),]

upset(data=df[c(3,24:28)],nsets=5)
upset(data=threats.mammals[c(3,24:28)],nsets=5)
upset(data=threats.birds[c(3,24:28)],nsets=5)
upset(data=threats.fish[c(3,24:28)],nsets=5)
upset(data=threats.amphibian[c(3,24:28)],nsets=5)
upset(data=threats.gastropode[c(3,24:28)],nsets=5)
upset(data=threats.flower[c(3,24:28)],nsets=5)
upset(data=threats.reptile[c(3,24:28)],nsets=5)

#Temporal variation
range(df$yearPublished)

threats.df <- df[which(df$yearPublished <= 1996),]
threats.temp.mammals <- df[which(df$yearPublished <= 1996),]
threats.temp.birds <- df[which(df$yearPublished <= 1996),]
threats.temp.fish <- df[which(df$yearPublished <= 1996),]
threats.temp.amphibian <- df[which(df$yearPublished <= 1996),]
threats.temp.gastropode <- df[which(df$yearPublished <= 1996),]
threats.temp.flower <- df[which(df$yearPublished <= 1996),]
threats.temp.reptile <- df[which(df$yearPublished <= 1996),]

upset(data=df[c(3,24:28)],nsets=5)
upset(data=threats.mammals[c(3,24:28)],nsets=5)
upset(data=threats.birds[c(3,24:28)],nsets=5)
upset(data=threats.fish[c(3,24:28)],nsets=5)
upset(data=threats.amphibian[c(3,24:28)],nsets=5)
upset(data=threats.gastropode[c(3,24:28)],nsets=5)
upset(data=threats.flower[c(3,24:28)],nsets=5)
upset(data=threats.reptile[c(3,24:28)],nsets=5)

threats.time <- colSums(threats.df[,24:28])
threats.time.mammal <- colSums(threats.temp.mammals[,24:28])
threats.time.birds <- colSums(threats.temp.birds[,24:28])
threats.time.fish <- colSums(threats.temp.fish[,24:28])
threats.time.amphibian <- colSums(threats.temp.amphibian[,24:28])
threats.time.gastropode <- colSums(threats.temp.gastropode[,24:28])
threats.time.flower <- colSums(threats.temp.flower[,24:28])
threats.time.reptile <- colSums(threats.temp.reptile[,24:28])
for(y in 1997:2025){
  threats.df <- df[which(df$yearPublished <= y),]
  threats.temp.mammal <- threats.mammals[which(threats.mammals$yearPublished <= y),]
  threats.temp.birds <- threats.birds[which(threats.birds$yearPublished <= y),]
  threats.temp.fish <- threats.fish[which(threats.fish$yearPublished <= y),]
  threats.temp.amphibian <- threats.amphibian[which(threats.amphibian$yearPublished <= y),]
  threats.temp.gastropode <- threats.gastropode[which(threats.gastropode$yearPublished <= y),]
  threats.temp.flower <- threats.flower[which(threats.flower$yearPublished <= y),]
  threats.temp.reptile <- threats.reptile[which(threats.reptile$yearPublished <= y),]
  threats.time <- rbind(threats.time,colSums(threats.dat.temp[,24:28]))
  threats.time.mammal <- rbind(threats.time.mammal,colSums(threats.dat.temp.mammal[,24:28]))
  threats.time.birds <- rbind(threats.time.birds,colSums(threats.dat.temp.birds[,24:28]))
  threats.time.fish <- rbind(threats.time.fish,colSums(threats.dat.temp.fish[,24:28]))
  threats.time.amphibian <- rbind(threats.time.amphibian,colSums(threats.dat.temp.amphibian[,24:28]))
  threats.time.gastropode <- rbind(threats.time.gastropode,colSums(threats.dat.temp.gastropode[,24:28]))
  threats.time.flower <- rbind(threats.time.flower,colSums(threats.dat.temp.flower[,24:28]))
  threats.time.reptile <- rbind(threats.time.reptile,colSums(threats.dat.temp.reptile[,24:28]))
}
