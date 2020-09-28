##Loading the data into RStudio
## load libraries
library(readr)
library(tidyverse)
library(grid)
library(gridExtra)

## importing the datasets
fulldata <- read_csv(file = "GPSDD Kabadiwalla Data.csv")
materials <- read_csv(file = "GPSDD Kabadiwalla data material subcategories.csv")

## Average buying price for each category (by subcategory)
## filter subcategories

Aluminium<-filter(materials, Material=="Aluminium")
Steel<-filter(materials, Material=="Steel")
Plastic<-filter(materials, Material=="Plastic")
Paper<-filter(materials, Material=="Paper")
Batteries<-filter(materials, Material=="Batteries")
Brass<-filter(materials, Material=="Brass")
Cardboard<-filter(materials,  Material=="Cardboard")
Copper<-filter(materials,  Material=="Copper")
E_waste<-filter(materials, Material=="E-waste")
Glass<-filter(materials, Material=="Glass")
Gun_metal<-filter(materials, Material=="Gun metal")
Iron<-filter(materials, Material=="Iron")
Lead<-filter(materials, Material=="Lead")
Metal<-filter(materials, Material=="Metal")
Silver<-filter(materials, Material=="Silver")
Wire<-filter(materials, Material=="Wire")

## plots for CP for each subcategory
plot1<-ggplot(Aluminium, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Aluminium")
plot2<-ggplot(Steel, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Steel")
plot3<-ggplot(Plastic, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Plastic")
plot4<-ggplot(Paper, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Paper")
plot5<-ggplot(Batteries, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Batteries")
plot6<-ggplot(Brass, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Brass")
plot7<-ggplot(Cardboard, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Cardboard")
plot8<-ggplot(Copper, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Copper")
plot9<-ggplot(E_waste, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("E_waste")
plot10<-ggplot(Glass, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Glass")
plot11<-ggplot(Gun_metal, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1), fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Gun metal")
plot12<-ggplot(Iron, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Iron")
plot13<-ggplot(Lead, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Lead")
plot14<-ggplot(Metal, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Metal")
plot15<-ggplot(Silver, aes(subcategory, CP)) + 
   geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Silver")plot15
plot16<-ggplot(Wire, aes(subcategory, CP)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Wire")plot16

## Plots for volume
plot1<-ggplot(Aluminium, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Aluminium")
plot2<-ggplot(Steel, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Steel")
plot2
plot3<-ggplot(Plastic, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Plastic")
plot3
plot4<-ggplot(Paper, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Paper")
plot5<-ggplot(Batteries, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Batteries")
plot6<-ggplot(Brass, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Brass")
plot7<-ggplot(Cardboard, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Cardboard")
plot7
plot8<-ggplot(Copper, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Copper")
plot9<-ggplot(E_waste, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("E_waste")
plot10<-ggplot(Glass, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Glass")
plot11<-ggplot(Gun_metal, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1), fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Gun metal")
plot12<-ggplot(Iron, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Iron")
plot13<-ggplot(Lead, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Lead")
plot14<-ggplot(Metal, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Metal")
plot15<-ggplot(Silver, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Silver")
plot16<-ggplot(Wire, aes(subcategory, Volume)) + 
  geom_point() + stat_summary(aes(group=1),fun.y = "mean", colour = "red", linetype="dashed",geom="line")+ggtitle("Wire")
plot16
