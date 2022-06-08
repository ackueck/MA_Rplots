#### libraries
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("vegan")
install.packages("dyplr")
install.packages("RColorBrewer")
install.packages("extrafont")

library("ggplot2")
library("tidyverse")
library("vegan")
library("dyplr")
library("RColorBrewer")
library("extrafont")

########################## Sampling location #######################################
# Set working directory
setwd("~/Documents/Master/lucinid MA/bioinformatics/R")

# Dataset

Sampling.table <- read.csv2("~/Documents/Master/lucinid MA/bioinformatics/R/Sampling.table.csv")
View(Sampling.table)

install.packages("maps")
install.packages("mapdata")
install.packages("sf")
install.packages("ggmap")
install.packages("ggspatial")
install.packages("googleway")
install.packages("cowplot")
install.packages("ggrepel")
install.packages("lwgeom")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos")
install.packages("rgdal")

library("maps")
library("mapdata")
library("sf")
library("ggmap")
library("ggspatial")
library("googleway")
library("cowplot")
library("ggrepel")
library("lwgeom")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")
library("rgdal")

isthmus.colors<-c("atlantic"="#62AA92",
                  "pacific"="#8676B8")

isthmus.shapes<-c("ClacoCahuit002"=21, "CteimGalet28"=22, "CteimSTRI091"=24, 
                  "CtegaSaHe038"=21, "CtemeSaHe044"=22, "CtemeSaHe050"=24,
                  "Guadeoupe"=21,
                  "Florida_FortPierce"=21, "Florida_Wildcat"=22)


world<-map_data("world")

##### Trans Isthmus
TransIsthmus<-ggplot() +
  geom_polygon(data=world, aes(x=long, y=lat, group=group) , fill="grey") +
  coord_sf(xlim = c(-100, -60), ylim = c(0, 35), crs=4326) +
  annotation_scale(location="bl", width_hint = 0.5) +
  theme_bw() +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=Sampling.table, aes(x=longitude, y=latitude, fill=Isthmusside, shape =ID), size=10) +
  scale_fill_manual(values=isthmus.colors, name="Isthmus side") +
  scale_shape_manual(values=isthmus.shapes) +
  theme(legend.position="none",
        text=element_text(size=25))
TransIsthmus
ggsave(TransIsthmus, file="./plots/map.TransIsthmus.eps", width=400, height=300, units="mm", limitsize=F)
```


####### Costa Rica
CostaRica<-ggplot() +
  geom_polygon(data=world, aes(x=long, y=lat, group=group) , fill="grey")+
  coord_sf(xlim = c(-83.5, -79.5), ylim = c(8, 10.5), crs=4326) +
  annotation_scale(location="bl", width_hint = 0.5) +
  theme_bw() +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=Sampling.table, aes(x=longitude, y=latitude, fill=Isthmusside, shape =ID), size=12) +
  scale_fill_manual(values=isthmus.colors, name="Isthmus side") +
  scale_shape_manual(values=isthmus.shapes) +
  theme(legend.position="none",
        text=element_text(size=25))
CostaRica
ggsave(CostaRica, file="./plots/map.CostaRica.eps", width=400, height=300, units="mm", limitsize=F)



####### Guadeloupe
Guadeloupe<-ggplot() +
  geom_polygon(data=world, aes(x=long, y=lat, group=group) , fill="grey")+
  coord_sf(xlim = c(-62, -61), ylim = c(15.9, 16.5), crs=4326) +
  annotation_scale(location="bl", width_hint = 0.5) +
  theme_bw() +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=Sampling.table, aes(x=longitude, y=latitude, fill=Isthmusside, shape =ID), size=12) +
  scale_fill_manual(values=isthmus.colors, name="Isthmus side") +
  scale_shape_manual(values=isthmus.shapes) +
  theme(legend.position="none",
        text=element_text(size=25))
Guadeloupe
ggsave(Guadeloupe, file="./plots/map.Guadeloupe.eps", width=400, height=300, units="mm", limitsize=F)


###### Santa Helena
SaHe<-ggplot() +
  geom_polygon(data=world, aes(x=long, y=lat, group=group) , fill="grey")+
  coord_sf(xlim = c(-87, -85), ylim = c(10, 12), crs=4326) +
  annotation_scale(location="bl", width_hint = 0.5) +
  theme_bw() +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=Sampling.table, aes(x=longitude, y=latitude, fill=Isthmusside, shape =ID), size=14) +
  scale_fill_manual(values=isthmus.colors, name="Isthmus side") +
  scale_shape_manual(values=isthmus.shapes) +
  theme(legend.position="none",
        text=element_text(size=25))
SaHe
ggsave(SaHe, file="./plots/map.SaHe.eps", width=400, height=300, units="mm", limitsize=F)


######### FLorida
###### Santa Helena
Florida<-ggplot() +
  geom_polygon(data=world, aes(x=long, y=lat, group=group) , fill="grey")+
  coord_sf(xlim = c(-81.5, -79.5), ylim = c(26, 28), crs=4326) +
  annotation_scale(location="bl", width_hint = 0.5) +
  theme_bw() +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=Sampling.table, aes(x=longitude, y=latitude, fill=Isthmusside, shape =ID), size=14) +
  scale_fill_manual(values=isthmus.colors, name="Isthmus side") +
  scale_shape_manual(values=isthmus.shapes) +
  theme(legend.position="none",
        text=element_text(size=25))
Florida
ggsave(Florida, file="./plots/map.Florida.eps", width=400, height=300, units="mm", limitsize=F)



########################## Deltas #####################################################

#### set wd
setwd("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV")

#### load dataset
deltas <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV/deltas.csv")
View(deltas)


### load dataset
all_natural_gills <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV/all_natural_gills.csv")
View(all_natural_gills)

#### plot delta 13C/15N

d_allnatab <- ggplot(data = all_natural_gills) +
  geom_point(aes(x = d15N, y = d13C, fill = factor(Identifier), shape = factor(Identifier)), size=3.5) +
  ylab(expression(paste(delta^{13},'C'~'[\u2030]'))) + 
  xlab(expression(paste(delta^{15},'N'~'[\u2030]'))) + 
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("Ctena1"="#FF7256", "Ctena2"="#FF7256", "Ctena3"="#FF7256", "Ctena4"="#FF7256", "Ctena5"="#FF7256",
                              "Codakia_1"="#7AC5CD", "Codakia_3"="#7AC5CD", "Codakia_4"="#7AC5CD", "Codakia_5"="#7AC5CD",
                              "Claco_1"="#CDC8B1", "Claco_2"="#CDC8B1", "Claco_3"="#CDC8B1", "Claco_4"="#CDC8B1", "Claco_5"="#CDC8B1",
                              "Phacoides_1_1"="#66CDAA", "Phacoides_1_2"="#66CDAA", "Phacoides_1_3"="#66CDAA", "Phacoides_1_4"="#66CDAA", "Phacoides_1_5"="#66CDAA",
                              "Phacoides_2_1"="#EEE0E5", "Phacoides_2_2"="#EEE0E5", "Phacoides_2_3"="#EEE0E5", "Phacoides_2_4"="#EEE0E5")) +
  scale_shape_manual(values=c("Ctena1"=15, "Ctena2"=15, "Ctena3"=15, "Ctena4"=15, "Ctena5"=15,
                              "Codakia_1"=15, "Codakia_3"=15, "Codakia_4"=15, "Codakia_5"=15,
                              "Claco_1"=15, "Claco_2"=15, "Claco_3"=15, "Claco_4"=15, "Claco_5"=15,
                              "Phacoides_1_1"=15, "Phacoides_1_2"=15, "Phacoides_1_3"=15, "Phacoides_1_4"=15, "Phacoides_1_5"=15,
                              "Phacoides_2_1"=16, "Phacoides_2_2"=16, "Phacoides_2_3"=16, "Phacoides_2_4"=16)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        text=element_text(size=20))

print(d_allnatab)
ggsave(d_allnatab, file="./d_allnatab.png", height=200, width=300, units="mm")

#######################
natural_delta <- ggplot(data = all_natural_gills)+
  geom_point(aes(x = d15N, y = d13C, fill = factor(Identifier), shape = factor(Identifier)), size=3.5, colour="black") +
  ylab(expression(paste(delta^{13},'C'~'[\u2030]'))) +
  xlab(expression(paste(delta^{15},'N'~'[\u2030]'))) +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("Ctena1"="#555599", "Ctena2"="#555599", "Ctena3"="#555599", "Ctena4"="#555599", "Ctena5"="#555599",
                              "Codakia_1"="#66BBBB", "Codakia_3"="#66BBBB", "Codakia_4"="#66BBBB", "Codakia_5"="#66BBBB",
                              "Claco_1"="#DD4444", "Claco_2"="#DD4444", "Claco_3"="#DD4444", "Claco_4"="#DD4444", "Claco_5"="#DD4444",
                              "Phacoides_1_1"="#E08104FC", "Phacoides_1_2"="#E08104FC", "Phacoides_1_3"="#E08104FC", "Phacoides_1_4"="#E08104FC", "Phacoides_1_5"="#E08104FC",
                              "Phacoides_2_1"="#E08104FC", "Phacoides_2_2"="#E08104FC", "Phacoides_2_3"="#E08104FC", "Phacoides_2_4"="#E08104FC")) +
  scale_shape_manual(values=c("Ctena1"=15, "Ctena2"=15, "Ctena3"=15, "Ctena4"=15, "Ctena5"=15,
                              "Codakia_1"=15, "Codakia_3"=15, "Codakia_4"=15, "Codakia_5"=15,
                              "Claco_1"=15, "Claco_2"=15, "Claco_3"=15, "Claco_4"=15, "Claco_5"=15,
                              "Phacoides_1_1"=15, "Phacoides_1_2"=15, "Phacoides_1_3"=15, "Phacoides_1_4"=15, "Phacoides_1_5"=15,
                              "Phacoides_2_1"=16, "Phacoides_2_2"=16, "Phacoides_2_3"=16, "Phacoides_2_4"=16)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        text=element_text(size=20))
print(natural_delta)
ggsave(natural_delta, file="./natural_delta.png", height=200, width=300, units="mm")                     


########################## new dataset

all_natural_gills_march <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV/all_natural_gills_march.csv")
View(all_natural_gills_march)

natural_delta_march <- ggplot(data = all_natural_gills_march)+
  geom_point(aes(x = d15N, y = d13C, fill = factor(Identifier), shape = factor(Identifier)), size=8, show.legend = TRUE) +
  ylab(expression(paste(delta^{13},'C'~'[\u2030]'))) +
  xlab(expression(paste(delta^{15},'N'~'[\u2030]'))) +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("Ctena1"="#FFC325", "Ctena2"="#FFC325", "Ctena3"="#FFC325", "Ctena4"="#FFC325", "Ctena5"="#FFC325",
                              "Codakia_1"="#048A81", "Codakia_3"="#048A81", "Codakia_4"="#048A81", "Codakia_5"="#048A81",
                              "Claco_1"="#222E50", "Claco_2"="#222E50", "Claco_3"="#222E50", "Claco_4"="#222E50", "Claco_5"="#222E50",
                              "Phacoides_1_1"="#EEB6B5", "Phacoides_1_2"="#EEB6B5", "Phacoides_1_3"="#EEB6B5", "Phacoides_1_4"="#EEB6B5", "Phacoides_1_5"="#EEB6B5",
                              "Phacoides_2_1"="#EEB6B5", "Phacoides_2_2"="#EEB6B5", "Phacoides_2_3"="#EEB6B5", "Phacoides_2_4"="#EEB6B5",
                              "PinnaG1"="#C0D6DF", "PinnaG2"="#C0D6DF", "PinnaG3"="#C0D6DF", "PinnaG4"="#C0D6DF", "PinnaG5"="#C0D6DF",
                              "Crass G1"="#A31444", "Crass G2"="#A31444", "Crass G3"="#A31444", "Crass G4"="#A31444")) +
  scale_shape_manual(values=c("Ctena1"=21, "Ctena2"=21, "Ctena3"=21, "Ctena4"=21, "Ctena5"=21,
                              "Codakia_1"=21, "Codakia_3"=21, "Codakia_4"=21, "Codakia_5"=21,
                              "Claco_1"=21, "Claco_2"=21, "Claco_3"=21, "Claco_4"=21, "Claco_5"=21,
                              "Phacoides_1_1"=21, "Phacoides_1_2"=21, "Phacoides_1_3"=21, "Phacoides_1_4"=21, "Phacoides_1_5"=21,
                              "Phacoides_2_1"=24, "Phacoides_2_2"=24, "Phacoides_2_3"=24, "Phacoides_2_4"=24,
                              "PinnaG1"=22, "PinnaG2"=22, "PinnaG3"=22, "PinnaG4"=22, "PinnaG5"=22,
                              "Crass G1"=22, "Crass G2"=22, "Crass G3"=22, "Crass G4"=22)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black')) +
  theme(axis.title.x = element_text(size =30)) +
  theme(axis.title.y = element_text(size =30)) +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 20))


print(natural_delta_march)
ggsave(natural_delta_march, file="./natural_delta_march.png", height=200, width=300, units="mm")                     



#######################
combi_delta1 <- ggplot(data = combi_gills)+
  geom_point(aes(x = d15N, y = d13C, fill = factor(Identifier.1), shape = factor(Identifier.1)), size=6) +
  ylab(expression(paste(delta^{13},'C'~'[\u2030]'))) +
  xlab(expression(paste(delta^{15},'N'~'[\u2030]'))) +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("A1"="#E47155", "A2"="#E47155", "A3"="#E47155", "A4"="#E47155", "A5"="#E47155", "A6"="#E47155",
                              "B1"="#9AC0CD","B2"="#9AC0CD", "B3"="#9AC0CD", "B4"="#9AC0CD", "B5"="#9AC0CD", "B6"="#9AC0CD",
                              "Ctena1"="#FFC325", "Ctena2"="#FFC325", "Ctena3"="#FFC325", "Ctena4"="#FFC325", "Ctena5"="#FFC325",
                              "Codakia_1"="#ADD8E6", "Codakia_3"="#ADD8E6", "Codakia_4"="#ADD8E6", "Codakia_5"="#ADD8E6",
                              "Claco_1"="#34585D", "Claco_2"="#34585D", "Claco_3"="#34585D", "Claco_4"="#34585D", "Claco_5"="#34585D",
                              "Phacoides_1_1"="#EEB6B5", "Phacoides_1_2"="#EEB6B5", "Phacoides_1_3"="#EEB6B5", "Phacoides_1_4"="#EEB6B5", "Phacoides_1_5"="#EEB6B5",
                              "Phacoides_2_1"="#EEB6B5", "Phacoides_2_2"="#EEB6B5", "Phacoides_2_3"="#EEB6B5", "Phacoides_2_4"="#EEB6B5")) +
  scale_shape_manual(values=c("A1"=22, "A2"=22, "A3"=22, "A4"=22, "A5"=22, "A6"=23,
                              "B1"=22,"B2"=22, "B3"=22, "B4"=22, "B5"=22, "B6"=23,
                              "Ctena1"=21, "Ctena2"=21, "Ctena3"=21, "Ctena4"=21, "Ctena5"=21,
                              "Codakia_1"=21, "Codakia_3"=21, "Codakia_4"=21, "Codakia_5"=21,
                              "Claco_1"=21, "Claco_2"=21, "Claco_3"=21, "Claco_4"=21, "Claco_5"=21,
                              "Phacoides_1_1"=21, "Phacoides_1_2"=21, "Phacoides_1_3"=21, "Phacoides_1_4"=21, "Phacoides_1_5"=21,
                              "Phacoides_2_1"=24, "Phacoides_2_2"=24, "Phacoides_2_3"=24, "Phacoides_2_4"=24)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        text=element_text(size=20))
print(combi_delta1)
ggsave(combi_delta1, file="./combi_delta1.png", height=200, width=300, units="mm") 

## incubated clams
isotope_delta <- ggplot(data = combi_gills)+
  geom_point(aes(x = d15N, y = d13C, fill = factor(Identifier.1), shape = factor(Identifier.1)), size=7) +
  ylab(expression(paste(delta^{13},'C'~'[\u2030]'))) +
  xlab(expression(paste(delta^{15},'N'~'[\u2030]'))) +
  xlim(0,6) +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("A1"="#E47155", "A2"="#E47155", "A3"="#E47155", "A4"="#E47155", "A5"="#E47155", "A6"="#E47155",
                              "B1"="#9AC0CD","B2"="#9AC0CD", "B3"="#9AC0CD", "B4"="#9AC0CD", "B5"="#9AC0CD", "B6"="#9AC0CD")) +
  scale_shape_manual(values=c("A1"=22, "A2"=22, "A3"=22, "A4"=22, "A5"=22, "A6"=23,
                              "B1"=22,"B2"=22, "B3"=22, "B4"=22, "B5"=22, "B6"=23)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        text=element_text(size=20))
print(isotope_delta)
ggsave(isotope_delta, file="./isotope_delta.png", height=200, width=300, units="mm") 

### incubated + natural abundance Ctena
isotope_delta1 <- ggplot(data = combi_gills)+
  geom_point(aes(x = d15N, y = d13C, fill = factor(Identifier.1), shape = factor(Identifier.1)), size=7) +
  ylab(expression(paste(delta^{13},'C'~'[\u2030]'))) +
  xlab(expression(paste(delta^{15},'N'~'[\u2030]'))) +
  xlim(0,6) +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("A1"="#E47155", "A2"="#E47155", "A3"="#E47155", "A4"="#E47155", "A5"="#E47155", "A6"="#E47155",
                              "B1"="#9AC0CD","B2"="#9AC0CD", "B3"="#9AC0CD", "B4"="#9AC0CD", "B5"="#9AC0CD", "B6"="#9AC0CD",
                              "Ctena1"="#FFC325", "Ctena2"="#FFC325", "Ctena3"="#FFC325", "Ctena4"="#FFC325", "Ctena5"="#FFC325")) +
  scale_shape_manual(values=c("A1"=22, "A2"=22, "A3"=22, "A4"=22, "A5"=22, "A6"=23,
                              "B1"=22,"B2"=22, "B3"=22, "B4"=22, "B5"=22, "B6"=23,
                              "Ctena1"=21, "Ctena2"=21, "Ctena3"=21, "Ctena4"=21, "Ctena5"=21)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        text=element_text(size=20))
print(isotope_delta1)
ggsave(isotope_delta1, file="./isotope_delta1.png", height=200, width=300, units="mm") 


#### only natural abundance
natural_delta <- ggplot(data = combi_gills)+
  geom_point(aes(x = d15N, y = d13C, fill = factor(Identifier.1), shape = factor(Identifier.1)), size=6) +
  ylab(expression(paste(delta^{13},'C'~'[\u2030]'))) +
  xlab(expression(paste(delta^{15},'N'~'[\u2030]'))) +
  ylim(-30, -15) +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("Ctena1"="#FFC325", "Ctena2"="#FFC325", "Ctena3"="#FFC325", "Ctena4"="#FFC325", "Ctena5"="#FFC325",
                              "Codakia_1"="#ADD8E6", "Codakia_3"="#ADD8E6", "Codakia_4"="#ADD8E6", "Codakia_5"="#ADD8E6",
                              "Claco_1"="#34585D", "Claco_2"="#34585D", "Claco_3"="#34585D", "Claco_4"="#34585D", "Claco_5"="#34585D",
                              "Phacoides_1_1"="#EEB6B5", "Phacoides_1_2"="#EEB6B5", "Phacoides_1_3"="#EEB6B5", "Phacoides_1_4"="#EEB6B5", "Phacoides_1_5"="#EEB6B5",
                              "Phacoides_2_1"="#EEB6B5", "Phacoides_2_2"="#EEB6B5", "Phacoides_2_3"="#EEB6B5", "Phacoides_2_4"="#EEB6B5")) +
  scale_shape_manual(values=c("Ctena1"=21, "Ctena2"=21, "Ctena3"=21, "Ctena4"=21, "Ctena5"=21,
                              "Codakia_1"=21, "Codakia_3"=21, "Codakia_4"=21, "Codakia_5"=21,
                              "Claco_1"=21, "Claco_2"=21, "Claco_3"=21, "Claco_4"=21, "Claco_5"=21,
                              "Phacoides_1_1"=21, "Phacoides_1_2"=21, "Phacoides_1_3"=21, "Phacoides_1_4"=21, "Phacoides_1_5"=21,
                              "Phacoides_2_1"=24, "Phacoides_2_2"=24, "Phacoides_2_3"=24, "Phacoides_2_4"=24)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        text=element_text(size=20))
print(natural_delta)
ggsave(natural_delta, file="./natural_delta.png", height=200, width=300, units="mm") 

##################### new dataset


### incubated SII Round 1 + natural abundance Ctena
isotope_delta1_march <- ggplot(data = combi_gills_march)+
  geom_point(aes(x = d15N, y = d13C, fill = factor(Identifier.1), shape = factor(Identifier.1)), size=8) +
  ylab(expression(paste(delta^{13},'C'~'[\u2030]'))) +
  xlab(expression(paste(delta^{15},'N'~'[\u2030]'))) +
  xlim(0,6) +
  ylim(-50,100)+
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("A1"="#E47155", "A2"="#E47155", "A3"="#E47155", "A4"="#E47155", "A5"="#E47155", "A6"="#E47155",
                              "B1"="#9AC0CD","B2"="#9AC0CD", "B3"="#9AC0CD", "B4"="#9AC0CD", "B5"="#9AC0CD", "B6"="#9AC0CD",
                              "Ctena1"="#FFC325", "Ctena2"="#FFC325", "Ctena3"="#FFC325", "Ctena4"="#FFC325", "Ctena5"="#FFC325")) +
  scale_shape_manual(values=c("A1"=22, "A2"=22, "A3"=22, "A4"=22, "A5"=22, "A6"=25,
                              "B1"=22,"B2"=22, "B3"=22, "B4"=22, "B5"=22, "B6"=25,
                              "Ctena1"=21, "Ctena2"=21, "Ctena3"=21, "Ctena4"=21, "Ctena5"=21)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'))+
  theme(axis.title.x = element_text(size =30)) +
  theme(axis.title.y = element_text(size =30)) +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 20))

print(isotope_delta1_march)
ggsave(isotope_delta1_march, file="./isotope_delta1_march.png", height=200, width=300, units="mm") 


### incubated SII Round 2 + natural abundance Ctena + Phacoides
isotope_delta2_march <- ggplot(data = combi_gills_march)+
  geom_point(aes(x = d15N, y = d13C, fill = factor(Identifier.1), shape = factor(Identifier.1)), size=8) +
  ylab(expression(paste(delta^{13},'C'~'[\u2030]'))) +
  xlab(expression(paste(delta^{15},'N'~'[\u2030]'))) +
 # xlim(0,6) +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("Ctena Ci1"="#FF9100", "Ctena Ci2"="#FF9100", "Ctena Ci3"="#FF9100", "Ctena Ci4"="#FF9100", "Ctena Ci5"="#FF9100",
                              "Ctena CCi1"="#FF9100","Ctena CCi2"="#FF9100", "Ctena CCi3"="#FF9100", "Ctena CCi4"="#FF9100",
                              "Phaco Pp1"="#8E5572", "Phaco Pp2"="#8E5572", "Phaco Pp3"="#8E5572", "Phaco Pp4"="#8E5572", "Phaco Pp5"="#8E5572",
                              "Phaco CPp1"="#8E5572","Phaco CPp2"="#8E5572", "Phaco CPp3"="#8E5572", "Phaco CPp4"="#8E5572",
                              "Ctena1"="#FFC325", "Ctena2"="#FFC325", "Ctena3"="#FFC325", "Ctena4"="#FFC325", "Ctena5"="#FFC325",
                              "Phacoides_1_1"="#EEB6B5", "Phacoides_1_2"="#EEB6B5", "Phacoides_1_3"="#EEB6B5", "Phacoides_1_4"="#EEB6B5", "Phacoides_1_5"="#EEB6B5")) +
  scale_shape_manual(values=c("Ctena Ci1"=22, "Ctena Ci2"=22, "Ctena Ci3"=22, "Ctena Ci4"=22, "Ctena Ci5"=22, 
                              "Ctena CCi1"=24, "Ctena CCi2"=24,
                              "Ctena CCi3"=25, "Ctena CCi4"=25,
                              "Phaco Pp1"=22,"Phaco Pp2"=22, "Phaco Pp3"=22, "Phaco Pp4"=22, "Phaco Pp5"=22, 
                              "Phaco CPp1"=24, "Phaco CPp2"=24,
                              "Phaco CPp3"=25, "Phaco CPp4"=25, 
                              "Ctena1"=21, "Ctena2"=21, "Ctena3"=21, "Ctena4"=21, "Ctena5"=21,
                              "Phacoides_1_1"=21, "Phacoides_1_2"=21, "Phacoides_1_3"=21, "Phacoides_1_4"=21, "Phacoides_1_5"=21)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black')) +
  theme(axis.title.x = element_text(size =30)) +
  theme(axis.title.y = element_text(size =30)) +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 20))

print(isotope_delta2_march)
ggsave(isotope_delta2_march, file="./isotope_delta2_march.png", height=200, width=300, units="mm") 




#################### visceral masses
#### load datasets
visc.masses <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV/visc.masses.csv")
View(visc.masses)

visc.gill.masses <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV/visc.gill.masses.csv")
View(visc.gill.masses)

visc_delta <- ggplot(data = visc.gill.masses)+
  geom_point(aes(x = d15N, y = d13C, fill = factor(Identifier), shape = factor(Identifier)), size=8, show.legend = FALSE) +
  ylab(expression(paste(delta^{13},'C'~'[\u2030]'))) +
  xlab(expression(paste(delta^{15},'N'~'[\u2030]'))) +
  ylim(-30, -15) +
  #theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("Phacoides1_visc"="#EEB6B5", "Phacoides2_visc"="#EEB6B5", "Phacoides3_visc"="#EEB6B5", "Phacoides4_visc"="#EEB6B5", "Phacoides5_visc"="#EEB6B5",
                              "Codakia1_visc"="#048A81", "Codakia2_visc"="#048A81", "Codakia3_visc"="#048A81", "Codakia4_visc"="#048A81", "Codakia5_visc"="#048A81",
                              "Ctena1_visc"="#FFC325", "Ctena2_visc"="#FFC325", "Ctena3_visc"="#FFC325", "Ctena4_visc"="#FFC325", "Ctena5_visc"="#FFC325",
                              "PhacF2_visc"="#EEB6B5", "PhacF3_visc"="#EEB6B5", "PhacF1_visc"="#EEB6B5",
                              "PinnaM1"="#C0D6DF", "PinnaM2"="#C0D6DF", "PinnaM3"="#C0D6DF", "PinnaM4"="#C0D6DF", "PinnaM5"="#C0D6DF",
                              "CrassM1"="#A31444", "CrassM2"="#A31444", "CrassM3"="#A31444", "CrassM4"="#A31444")) +
  scale_shape_manual(values=c("Phacoides1_visc"=22, "Phacoides2_visc"=22, "Phacoides3_visc"=22, "Phacoides4_visc"=22, "Phacoides5_visc"=22,
                              "Codakia1_visc"=22, "Codakia2_visc"=22, "Codakia3_visc"=22, "Codakia4_visc"=22, "Codakia5_visc"=22,
                              "Ctena1_visc"=22, "Ctena2_visc"=22, "Ctena3_visc"=22, "Ctena4_visc"=22, "Ctena5_visc"=22,
                              "PhacF2_visc"=25, "PhacF3_visc"=25, "PhacF1_visc"=25,
                              "PinnaM1"=22, "PinnaM2"=22, "PinnaM3"=22, "PinnaM4"=22, "PinnaM5"=22,
                              "CrassM1"=22, "CrassM2"=22, "CrassM3"=22, "CrassM4"=22)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        text=element_text(size=30))

print(visc_delta)
ggsave(visc_delta, file="./visc_delta.png", height=200, width=200, units="mm") 

################################## Delta deltas #####################################

delta.delta.isotopes <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV/delta.delta.isotopes.csv")
View(delta.delta.isotopes)

delta.deltas1 <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV/delta.deltas1.csv")
View(delta.deltas1)

delta.deltas <- ggplot(data = delta.deltas1)+
  geom_point(aes(x = delta_d15N, y = delta_d13C, fill = factor(Identifier), shape = factor(Identifier)), size=8, show.legend = TRUE) +
  ylab(expression(paste(Delta*delta^{13},'C'~'[\u2030]'))) +
  xlab(expression(paste(Delta*delta^{15},'N'~'[\u2030]'))) +
  ylim(0, 6) +
  xlim(0, 6) +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("Phacoides1"="#EEB6B5", "Phacoides2"="#EEB6B5", "Phacoides3"="#EEB6B5", "Phacoides4"="#EEB6B5", "Phacoides5"="#EEB6B5",
                              "Codakia1"="#048A81", "Codakia3"="#048A81", "Codakia4"="#048A81", "Codakia5"="#048A81",
                              "Ctena1"="#FFC325", "Ctena2"="#FFC325", "Ctena3"="#FFC325", "Ctena4"="#FFC325", "Ctena5"="#FFC325",
                              "PhacFlorida2"="#EEB6B5", "PhacFlorida3"="#EEB6B5", "PhacFlorida1"="#EEB6B5",
                              "Pinna1"="#C0D6DF", "Pinna2"="#C0D6DF", "Pinna3"="#C0D6DF", "Pinna4"="#C0D6DF", "Pinna5"="#C0D6DF",
                              "Crass1"="#A31444", "Crass2"="#A31444", "Crass3"="#A31444", "Crass4"="#A31444")) +
  scale_shape_manual(values=c("Phacoides1"=21, "Phacoides2"=21, "Phacoides3"=21, "Phacoides4"=21, "Phacoides5"=21,
                              "Codakia1"=21, "Codakia3"=21, "Codakia4"=21, "Codakia5"=21,
                              "Ctena1"=21, "Ctena2"=21, "Ctena3"=21, "Ctena4"=21, "Ctena5"=21,
                              "PhacFlorida2"=24, "PhacFlorida3"=24, "PhacFlorida1"=24,
                              "Pinna1"=22, "Pinna2"=22, "Pinna3"=22, "Pinna4"=22, "Pinna5"=22,
                              "Crass1"=22, "Crass2"=22, "Crass3"=22, "Crass4"=22)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black')) +
        theme(axis.title.x = element_text(size =30)) +
          theme(axis.title.y = element_text(size =30)) +
          theme(axis.text.x = element_text(size = 20)) +
          theme(axis.text.y = element_text(size = 20))

print(delta.deltas)
ggsave(delta.deltas, file="./delta.deltas1.png", height=200, width=300, units="mm") 


##### Delta vs Deltadeltas
allnatab_delta_vs_deltadeltas <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV/allnatab_delta_vs_deltadeltas.csv")
View(allnatab_delta_vs_deltadeltas)

d.C.dd <- ggplot(data = allnatab_delta_vs_deltadeltas)+
  geom_point(aes(x = d13C, y = delta_d13C, fill = factor(Identifier), shape = factor(Identifier)), size=8, show.legend = TRUE) +
  ylab(expression(paste(Delta*delta^{13},'C'~'[\u2030]'))) +
  xlab(expression(paste(delta^{13},'C'~'[\u2030]'))) +
  ylim(0, 4) +
  #xlim() +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("Phacoides_1_1"="#EEB6B5", "Phacoides_1_2"="#EEB6B5", "Phacoides_1_3"="#EEB6B5", "Phacoides_1_4"="#EEB6B5", "Phacoides_1_5"="#EEB6B5",
                              "Codakia_1"="#048A81", "Codakia_3"="#048A81", "Codakia_4"="#048A81", "Codakia_5"="#048A81",
                              "Ctena1"="#FFC325", "Ctena2"="#FFC325", "Ctena3"="#FFC325", "Ctena4"="#FFC325", "Ctena5"="#FFC325",
                              "Phacoides_2_1"="#EEB6B5", "Phacoides_2_2"="#EEB6B5", "Phacoides_2_3"="#EEB6B5",
                              "PinnaG1"="#C0D6DF", "PinnaG2"="#C0D6DF", "PinnaG3"="#C0D6DF", "PinnaG4"="#C0D6DF", "PinnaG5"="#C0D6DF",
                              "Crass G1"="#A31444", "Crass G2"="#A31444", "Crass G3"="#A31444", "Crass G4"="#A31444")) +
  scale_shape_manual(values=c("Phacoides_1_1"=21, "Phacoides_1_2"=21, "Phacoides_1_3"=21, "Phacoides_1_4"=21, "Phacoides_1_5"=21,
                              "Codakia_1"=21, "Codakia_3"=21, "Codakia_4"=21, "Codakia_5"=21,
                              "Ctena1"=21, "Ctena2"=21, "Ctena3"=21, "Ctena4"=21, "Ctena5"=21,
                              "Phacoides_2_1"=24, "Phacoides_2_2"=24, "Phacoides_2_3"=24,
                              "PinnaG1"=22, "PinnaG2"=22, "PinnaG3"=22, "PinnaG4"=22, "PinnaG5"=22,
                              "Crass G1"=22, "Crass G2"=22, "Crass G3"=22, "Crass G4"=22)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black')) +
  theme(axis.title.x = element_text(size =30)) +
  theme(axis.title.y = element_text(size =30)) +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 20))

print(d.C.dd)
ggsave(d.C.dd, file="./d.C.dd.png", height=200, width=300, units="mm") 




d.N.dd <- ggplot(data = allnatab_delta_vs_deltadeltas)+
  geom_point(aes(x = d15N, y = delta_d15N, fill = factor(Identifier), shape = factor(Identifier)), size=8, show.legend = TRUE) +
  ylab(expression(paste(Delta*delta^{15},'N'~'[\u2030]'))) +
  xlab(expression(paste(delta^{15},'N'~'[\u2030]'))) +
  ylim(0, 4) +
  #xlim() +
  theme(legend.position = "right", legend.title = element_blank()) +
  scale_fill_manual(values= c("Phacoides_1_1"="#EEB6B5", "Phacoides_1_2"="#EEB6B5", "Phacoides_1_3"="#EEB6B5", "Phacoides_1_4"="#EEB6B5", "Phacoides_1_5"="#EEB6B5",
                              "Codakia_1"="#048A81", "Codakia_3"="#048A81", "Codakia_4"="#048A81", "Codakia_5"="#048A81",
                              "Ctena1"="#FFC325", "Ctena2"="#FFC325", "Ctena3"="#FFC325", "Ctena4"="#FFC325", "Ctena5"="#FFC325",
                              "Phacoides_2_1"="#EEB6B5", "Phacoides_2_2"="#EEB6B5", "Phacoides_2_3"="#EEB6B5",
                              "PinnaG1"="#C0D6DF", "PinnaG2"="#C0D6DF", "PinnaG3"="#C0D6DF", "PinnaG4"="#C0D6DF", "PinnaG5"="#C0D6DF",
                              "Crass G1"="#A31444", "Crass G2"="#A31444", "Crass G3"="#A31444", "Crass G4"="#A31444")) +
  scale_shape_manual(values=c("Phacoides_1_1"=21, "Phacoides_1_2"=21, "Phacoides_1_3"=21, "Phacoides_1_4"=21, "Phacoides_1_5"=21,
                              "Codakia_1"=21, "Codakia_3"=21, "Codakia_4"=21, "Codakia_5"=21,
                              "Ctena1"=21, "Ctena2"=21, "Ctena3"=21, "Ctena4"=21, "Ctena5"=21,
                              "Phacoides_2_1"=24, "Phacoides_2_2"=24, "Phacoides_2_3"=24,
                              "PinnaG1"=22, "PinnaG2"=22, "PinnaG3"=22, "PinnaG4"=22, "PinnaG5"=22,
                              "Crass G1"=22, "Crass G2"=22, "Crass G3"=22, "Crass G4"=22)) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black')) +
  theme(axis.title.x = element_text(size =30)) +
  theme(axis.title.y = element_text(size =30)) +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 20))

print(d.N.dd)
ggsave(d.N.dd, file="./d.N.dd.png", height=200, width=300, units="mm") 

############################## Rates ####################################################

RatesBP <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV/RatesBP.csv")
View(RatesBP)

ratesboxplot <- ggplot(RatesBP, aes(x= Treatment, y= Rates, fill= Element)) +
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name=expression('Fixation rates'~"["~mu*'mol'~'mg'[dw]~''^-1~'d'^-1~"]"),
                     breaks=seq(0, 7, 0.5),
                     limits=c(0, 7)) +
  ggtitle("Fixation Rates") +
  theme_classic() +
  theme(plot.title= element_text(size= 14, family= "Helvetica", face= "plain"),
        text= element_text(size= 12, family= "Helvetica"),
        axis.title= element_text(face= "plain"),
        axis.text.x = element_text(size= 11)) +
  scale_fill_manual(values= c("C"="#6A93B0", "N"="#EE9A49")) +
  geom_jitter()

print(ratesboxplot)
ggsave(ratesboxplot, file="./rateBP.png", height=200, width=200, units="mm")


RatesBP <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV/RatesBP.csv")
View(RatesBP)

############ Round 1
Rates_R1 <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV/Rates_R1.csv")
View(Rates_R1)

ratesboxplot1 <- ggplot(Rates_R1, aes(x= Treatment, y= Rates, fill= Element)) +
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name=expression('Fixation rates'~"["~mu*'mol'~'mg'[dw]~''^-1~'d'^-1~"]")) +
  ggtitle("Stable isotope incubation 1") +
  theme_classic() +
  theme(plot.title= element_text(size= 20, family= "Helvetica", face= "plain"),
        text= element_text(size= 20, family= "Helvetica"),
        axis.title= element_text(face= "plain"),
        axis.text.x = element_text(size= 20)) +
  scale_fill_manual(values= c("C"="#6A93B0", "N"="#EE9A49")) +
  geom_jitter()

print(ratesboxplot1)
ggsave(ratesboxplot1, file="./rateBP_1.png", height=200, width=200, units="mm")


############ Round 2
Rates_R2 <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/CSV/Rates_R2.csv")
View(Rates_R2)

ratesboxplot2 <- ggplot(Rates_R2, aes(x= Species, y= Rates, fill= Element)) +
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(name="Species") +
  scale_y_continuous(name=expression('Fixation rates'~"["~mu*'mol'~'mg'[dw]~''^-1~'d'^-1~"]")) +
  ggtitle("Stable isotope incubation 2") +
  theme_classic() +
  theme(plot.title= element_text(size= 20, family= "Helvetica", face= "plain"),
        text= element_text(size= 20, family= "Helvetica"),
        axis.title= element_text(face= "plain"),
        axis.text.x = element_text(size= 20)) +
  scale_fill_manual(values= c("C"="#6A93B0", "N"="#EE9A49")) +
  geom_jitter()

print(ratesboxplot2)
ggsave(ratesboxplot2, file="./rateBP_2.png", height=200, width=200, units="mm")


###################### Ammonia #####################################################
### set WD
setwd("~/Documents/Master/lucinid MA/N2_fixation_experiment/ammonia measurements")

### load dataset
ammonia <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/ammonia measurements/ammonia.csv")
View(ammonia)

### 
nh4 <- ggplot(data = ammonia, aes(x=Sample, y=Average)) +
  geom_bar(stat="identity", fill = "dodgerblue4", color="black", width=0.5) +
  theme_bw() +
  theme(axis.title.x = element_text(size =20)) +
  theme(axis.title.y = element_text(size =20)) +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 20)) +
  ylab(expression(paste(NH[4]^{"+"}~'['~µM~']'))) + 
  xlab("Sediment Sample") +
  geom_errorbar(aes(ymin=Average-Standard..deviation, ymax=Average+Standard..deviation))

nh4
ggsave(nh4, file="./nh4.eps", height=200, width=200, units="mm")

####################### Oxygen consumption ##########################################
### set WD
setwd("~/Documents/Master/lucinid MA/N2_fixation_experiment")

### load dataset
o2consumption <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/o2consumption.csv")
View(o2consumption)

###
o2trial <- ggplot(data = o2consumption, aes(x=h, y=O2))+
  geom_point(stat="identity", shape=21, fill = "dodgerblue4", color="black", size=8) +
  ylim(0,250) +
  ylab(expression(paste(O[2]~'['~µM~']'))) + 
  xlab("Time [h]") +
  theme_bw() +
  theme(axis.title.x = element_text(size =20)) +
  theme(axis.title.y = element_text(size =20)) +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 20)) +
  geom_line(linetype = 8)

o2trial
ggsave(o2trial, file="./o2trial.eps", height=200, width=300, units="mm")


### set WD
setwd("~/Documents/Master/lucinid MA/N2_fixation_experiment")

### load dataset
O2_round2 <- read.csv2("~/Documents/Master/lucinid MA/N2_fixation_experiment/O2_round2.csv")
View(O2_round2)

### 
o22 <- ggplot() +
  geom_point(data = O2_round2, aes(x=time, y=Ctena), stat="identity", shape=21, fill = "#FFC325", color="black", size=8) +
  geom_line(data = O2_round2, aes(x=time, y=Ctena), linetype = 8) +
  geom_point(data = O2_round2, aes(x=time, y=Phacoides), stat="identity", shape=21, fill = "#EEB6B5", color="black", size=8) +
  geom_line(data = O2_round2, aes(x=time, y=Phacoides), linetype = 8) +
  theme_bw() +
  theme(axis.title.x = element_text(size =20)) +
  theme(axis.title.y = element_text(size =20)) +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 20)) +
  ylim(0,250) +
  ylab(expression(paste(O[2]~'['~µM~']'))) +
  xlab("Time [h]")


o22
ggsave(o22, file="./o22.eps", height=200, width=300, units="mm")


########################### Enrichment ###################################################
# Set working directory
setwd("~/Documents/Master/lucinid MA/high quality MAGs")

# Dataset
enrichment1 <- read.csv2("~/Documents/Master/lucinid MA/high quality MAGs/enrichment1.csv")
View(enrichment1)

isthmus.colors<-c("caribbean"="#62AA92",
                  "pacific"="#8676B8")

enrichment1$gene <- factor(enrichment1$gene, levels = c("ytfP", "ycf48",
                                                        "acd",
                                                        "arbV", "gnfK", "ybiB",
                                                        "lraI", "IV02_10640", "etfD", "etfB", "etfA", 
                                                        "nrtC", "nrtB", "nirB", "nasA",
                                                        "hesB", "fdxN", "fdxD", "fdxB", "fdx4", "draT", "draG", "nifY", "nifX", "nifW", "nifV", "nifU", "nifT", "nifS", "nifQ", "nifN", "nifM", "nifL", "nifK", "nifH", "nifE", "nifD", "nifB", "nifA"))

enrich1 <- ggplot() +
  geom_bar(data = enrichment1, aes(x=p.value, y=gene, fill=isthmus_side), position="stack", stat="identity", width=0.5) +
  theme(panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size= 0.2, color = 'black'),
        axis.ticks = element_line(size= 0.2, color = 'black'),
        text=element_text(size=20, "Helvetica")) +
  theme(legend.position = "right", legend.title = element_blank()) +
  ylab("genes") +
  xlab("abundance") +
  #scale_color_manual(values=isthmus.colors, name="isthmus_side") +
  scale_fill_manual(values=isthmus.colors, name="isthmus_side") 

enrich1
ggsave(enrich1, file="./plots/enrichment.eps", width=200, height=300, units="mm", limitsize=F)


