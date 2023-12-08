##Script used to analyze community composition data from scavenging and frugivory experiments. 

#Updated Oct 2023

# #clear environment
remove(list=ls())

# load packages
library(tidyverse)
library (dplyr)
library(vegan)



#Input scavenging data
dat_scavenge<-read.csv("June2022/FINAL_FOR_POST/Ecofunction/2023_Results/SciReports_McKee_Review/scav.com.matrix.csv")

##Format matrix 
dat_scavenge_temp<-dat_scavenge%>%dplyr::select(Reptile:LargeMammal)
scav_matrix<-as.matrix(dat_scavenge_temp)
scav_matrix[is.na(scav_matrix)]<-0


##Calculate distance 
scav_dist.log<-vegdist(decostand(scav_matrix, method="log"), method="jaccard")

#PERMANOVA Log.Abundance
scav.ruz.log<-adonis2(scav_dist.log~mesoPres,data = dat_scavenge,permutations = 99999)
scav.ruz.log


#Input frugivory data
dat_frugivory<-read.csv("June2022/FINAL_FOR_POST/Ecofunction/2023_Results/SciReports_McKee_Review/frug.com.matrix.csv")

#Format Matrix 
dat_frugivory_temp<-dat_frugivory%>%dplyr::select(Insect:Reptile)
frugivory_matrix<-as.matrix(dat_frugivory_temp)
frugivory_matrix[is.na(frugivory_matrix)]<-0

##Frugivory

#Calculate distance on abundance with log transformation 
frug_dist.log<-vegdist(decostand(frugivory_matrix,"log"),method='jaccard')

#PERMANOVA Log.Abundance

frug.ruz.log<-adonis2(frug_dist.log~mesoPres,data = dat_frugivory, permutations = 99999)
frug.ruz.log

