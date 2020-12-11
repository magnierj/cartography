#Carte BAYPASS commune pop ovins + pop bovins

#Récupération des jdd gps
gps_cattle<-read.table("~/Documents/Thèse/data/publi_assoc_clim/gps_assoc_clim_mod.txt", sep = " ", header = FALSE, row.names = 1)
gps_cattle<-gps_cattle[,c(2,1)]
colnames(gps_cattle)<-colnames(gps_ovin)

gps_ovin<-read.table(file = "~/Documents/Thèse/moutons/assoc_climat/list_gps_clim_v1.csv", sep="\t", header=TRUE, row.names = 1)
gps_ovin<-gps_ovin[,c(2,3)]

gps_pop<-rbind(gps_ovin, gps_cattle)
Type<-c(rep("ovin",9), rep("bovin",17))
gps_pop<-cbind(gps_pop, Type)

#Carto figure 1 _ publi Mayotte_Madagascar
library(osmdata)
library(sf)
library(ggmap)
library(reshape2)
### constitution du fichier

#### To loop through all 40 variables (Long puis Lat)
#latlong : coordonnées populations
#latlong <- read.table("gps_assoc_clim_mod.txt", header=F, sep=" ",row.names = 1,stringsAsFactors=F, quote="")
#inversion de longitude avec latitude
#latlong <- latlong[,c(2,1,3)]
#colonne MAY = -12... -> latitude
#colonne MAY = 45.15... -> longitude
#colnames(latlong)=c("Longitude","Latitude","Type")

latlong<-gps_pop

### Cartographie
#our background map
zone<-c(left = 0, bottom = -40, right = 125 , top = 50)
mad_map <-get_map(location = zone, source = "osm")


#final map

ggmap(mad_map)+
  geom_point(data=latlong, 
             aes(x=Longitude,y=Latitude, fill=Type),shape=21, size=2)+
  ggrepel::geom_label_repel(data = latlong, size=5, mapping = aes(x = Longitude, y = Latitude, label=row.names(latlong), fill=Type), color='black')+
  labs(x = "Longitude", y = "Latitude")+
  theme(legend.position = "bottom")


### Cartographie des variables climatiques

#Utilisation du package raster

# Download the ASCII files from https://www.climond.org/ClimateData.aspx

rm(list=ls())




#install.packages("raster")
#install.packages("plyr")
#install.packages("rgdal")

library(rgdal)
library(raster)
library(plyr)
library(ggspatial)
library(ggrepel)
library(gridExtra)
library(cowplot)
##ATTENTION!!!!! ne pas charger tidyverse car fonction extract en conflit avec raster.

#création d'un data frame avec long et lat
coord_long<-as.numeric(as.matrix(latlong[1]))
coord_lat<-as.numeric(as.matrix(latlong[2]))
coord2 <- cbind(coord_long, coord_lat)
coord2<-as.data.frame(coord2)
coordinates(coord2)<-c("coord_long","coord_lat")
pos_id=row.names(latlong)
##ATTENTION, mettre longitude puis latitude dans le fichier

folders <- list.files(path="~/Documents/Thèse/data/donnees_climatiques/CM10_1975H_Bio_V1.2", full.names = TRUE, recursive=FALSE)

bio_df <- c()

#Note: this will cause an error for the last "info" folder which does not have an .adf file
for (currentfile in folders) {
  #path <- paste0(currentfile)
  bio <- raster(currentfile)
  bioextracted <- extract(bio, coord2)
  bio_df <- cbind(bio_df, bioextracted)
  colnames(bio_df)[colnames(bio_df)=="bio_extracted"] <- currentfile
}

for (currentfile in folders) {
  print(currentfile)
}

#calcul du raster THI
Bio01=raster("~/Documents/Thèse/data/donnees_climatiques/CM10_1975H_Bio_V1.2/CM10_1975H_Bio01_V1.2.txt")
Bio28=raster("~/Documents/Thèse/data/donnees_climatiques/CM10_1975H_Bio_V1.2/CM10_1975H_Bio28_V1.2.txt")

thi <- overlay(Bio01, Bio28, fun=function(x,y){return((1.8*x+32)-(0.55-0.0055*y)*(1.8*x-26)
)})



#Pour chaque variable climatique, nous faisons tourner les lignes suivantes : 

###CARTES##
#Plot temperature Bio01
#délimitation de la zone
b <- as(extent(30, 120, -26, 27.5), 'SpatialPolygons')

#Plot temperature Bio01
bio=raster("~/Documents/Thèse/data/donnees_climatiques/CM10_1975H_Bio_V1.2/CM10_1975H_Bio01_V1.2.txt")
crs(b) <- crs(bio)

bio_crop <- crop(bio, b)

bio01<-ggplot(gps_pop, aes(Longitude, Latitude)) +
  layer_spatial(bio_crop, aes(fill = stat(band1))) +
  scale_fill_viridis_c(na.value = NA, option = "C")+
  geom_spatial_point(aes(col=Type)) +
#  geom_spatial_point() +
#  stat_spatial_identity(aes(label = rownames(gps_pop)), geom = "text_repel", box.padding = 1) +
  ggtitle("A. Bio01")+
  theme_void()+
  theme(legend.title=element_blank(),
        plot.title = element_text(size = 12, hjust = 0.15))
#  coord_sf(xlim=c(0,150),ylim=c(-50,75))



#Plot temperature annual range Bio07
bio=raster("~/Documents/Thèse/data/donnees_climatiques/CM10_1975H_Bio_V1.2/CM10_1975H_Bio07_V1.2.txt")
crs(b) <- crs(bio)
bio_crop <- crop(bio, b)
bio07<-ggplot(gps_pop, aes(Longitude, Latitude)) +
  layer_spatial(bio_crop, aes(fill = stat(band1))) +
  scale_fill_viridis_c(na.value = NA, option = "C")+
   geom_spatial_point(aes(col=Type)) +
 # geom_spatial_point() +
  #stat_spatial_identity(aes(label = rownames(gps_pop)), geom = "text_repel", box.padding = 1) +
  ggtitle("B. Bio07")+
  theme_void()+
  theme(legend.title=element_blank(),
        plot.title = element_text(size = 12, hjust = 0.15))
#  coord_sf(xlim=c(0,150),ylim=c(-50,75))

#Plot Annual Precipitation Bio12
bio=raster("~/Documents/Thèse/data/donnees_climatiques/CM10_1975H_Bio_V1.2/CM10_1975H_Bio12_V1.2.txt")
crs(b) <- crs(bio)
bio_crop <- crop(bio, b)
bio12<-ggplot(gps_pop, aes(Longitude, Latitude)) +
  layer_spatial(bio_crop, aes(fill = stat(band1))) +
  scale_fill_viridis_c(na.value = NA, option = "C", direction = -1)+
    geom_spatial_point(aes(col=Type)) +
  #geom_spatial_point() +
  #stat_spatial_identity(aes(label = rownames(gps_pop)), geom = "text_repel", box.padding = 1) +
  ggtitle("C. Bio12 ")+
  theme_void()+
  theme(legend.title=element_blank(),
        plot.title = element_text(size = 12, hjust = 0.15))
#  coord_sf(xlim=c(0,150),ylim=c(-50,75))



#Plot Annual mean radiation Bio20
bio=raster("~/Documents/Thèse/data/donnees_climatiques/CM10_1975H_Bio_V1.2/CM10_1975H_Bio20_V1.2.txt")
crs(b) <- crs(bio)
bio_crop <- crop(bio, b)
bio20<-ggplot(gps_pop, aes(Longitude, Latitude)) +
  layer_spatial(bio_crop, aes(fill = stat(band1))) +
  scale_fill_viridis_c(na.value = NA, option = "C")+
    geom_spatial_point(aes(col=Type)) +
  #geom_spatial_point() +
  #stat_spatial_identity(aes(label = rownames(gps_pop)), geom = "text_repel", box.padding = 1) +
  ggtitle("D. Bio20")+
  theme_void()+
  theme(legend.title=element_blank(),
        plot.title = element_text(size = 12, hjust = 0.15))
#  coord_sf(xlim=c(0,150),ylim=c(-50,75))


#Plot Annual mean moisture index Bio28
bio=raster("~/Documents/Thèse/data/donnees_climatiques/CM10_1975H_Bio_V1.2/CM10_1975H_Bio28_V1.2.txt")
crs(b) <- crs(bio)
bio_crop <- crop(bio, b)
bio28<-ggplot(gps_pop, aes(Longitude, Latitude)) +
  layer_spatial(bio_crop, aes(fill = stat(band1))) +
  scale_fill_viridis_c(na.value = NA, option = "C", direction = -1)+
    geom_spatial_point(aes(col=Type)) +
  #geom_spatial_point() +
  # stat_spatial_identity(aes(label = rownames(gps_pop)), geom = "text_repel", box.padding = 1) +
  ggtitle("F. Bio28")+
  theme_void()+
  theme(legend.title=element_blank(),
        plot.title = element_text(size = 12, hjust = 0.15))
# coord_sf(xlim=c(0,150),ylim=c(-50,75))



#pour le THI
crs(b) <- crs(thi)
bio_crop <- crop(thi, b)

biothi<-ggplot(gps_pop, aes(Longitude, Latitude)) +
  layer_spatial(bio_crop, aes(fill = stat(band1))) +
  scale_fill_viridis_c(na.value = NA, option = "C")+
    geom_spatial_point(aes(col=Type)) +
  #geom_spatial_point() +
  #stat_spatial_identity(aes(label = rownames(gps_pop)), geom = "text_repel", box.padding = 1) +
  ggtitle("E. THI ")+
  theme_void()+
  theme(legend.title=element_blank(),
        plot.title = element_text(size = 12, hjust = 0.15))



pdf(file="~/Documents/Thèse/Rédaction/présentations/hist_adapt/varclim_IO_accol.pdf",height=5,width=10,onefile = TRUE)

ggdraw()+
  draw_plot(bio01, x=0,y=0.5, width=0.33,height=0.5)+
  draw_plot(bio07, x=0.33,y=0.5, width=0.33,height=0.5)+
  draw_plot(bio12, x=0.66,y=0.5, width = 0.33, height=0.5)+
  draw_plot(bio20, x=0,y=0, width=0.33,height=0.5)+
  draw_plot(bio28, x=0.33,y=0, width = 0.33, height=0.5)+
  draw_plot(biothi, x=0.66,y=0, width = 0.33, height=0.5)

graphics.off()


