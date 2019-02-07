###### Query MaxN and Length/3D points from EventMeasure data tables downloaded from GlobalArchive ######
### Written by Tim Langlois, adpated and edited by Brooke Gibbons

### Please forward any updates and improvements to timothy.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au 
### or make a pull request on the GitHub repository "Format-EventMeasure-database-outputs"

### OBJECTIVE
# 1 Query datatables from EventMeasure database export function
# 2 Create a MaxN dataframe and combine Lengths and 3D measurements
# 3 Check and Format MaxN and Length

# Clear memory ----
rm(list=ls())

# Libraries required ----
library(tidyr)
library(dplyr)
library(readr)
library(data.table)
library(magrittr)
library(googlesheets)

# Study name---
study<-"Example" ## change for your project

# Set working directory ----
work.dir=("C:/GitHub/Format-EventMeasure-database-outputs") ## Change to your directory

# Set sub directories----
data.dir=paste(work.dir,"Database output",sep="/")
plots.dir=paste(work.dir,"Plots",sep="/")
tidy.dir=paste(work.dir,"Tidy data",sep="/")

# Load metadata ----
setwd(work.dir)
metadata<-read.csv(paste(study,"metadata.csv",sep="_"))%>%
  setNames(tolower(names(.)))

# Bring in points and calculate MaxN ----
maxn<-read.delim("2018-10_POC_RPS-consulting_Points.txt")%>%
  setNames(tolower(names(.)))%>%
  mutate(number=as.numeric(number))%>%
  group_by(opcode,frame,family,genus,species)%>%
  dplyr::summarise(maxn=sum(number))%>%
  group_by(opcode,family,genus,species)%>%
  slice(which.max(maxn))%>%
  ungroup()%>%
  filter(!maxn%in%c(NA,""))%>%
  tidyr::replace_na(list(family = "Unknown",genus = "Unknown", species = "spp"))%>% # Removes all NA's
  dplyr::mutate(family = ifelse(family%in%c("NA",""),"Unknown", as.character(family)))%>%
  dplyr::mutate(genus = ifelse(genus%in%c("genus","NA","NANA","sp","spp","unknown",""),"Unknown", as.character(genus)))%>%
  dplyr::mutate(species = ifelse(species%in%c(NA,"NA","sp",""),"spp", as.character(species)))%>%
  dplyr::rename(sample=opcode)%>%
  mutate(scientific=paste(family,genus,species,sep = " "))

# Load and format Length and 3D data from EventMeasure----
length<-read.delim("2018-10_POC_RPS-consulting_Lengths.txt")%>%setNames(tolower(names(.)))
threedpoints<-read.delim("2018-10_POC_RPS-consulting_3DPoints.txt")%>%setNames(tolower(names(.)))

length3dpoints<-threedpoints%>%
  plyr::rbind.fill(length)%>% #-location,-time,-site,-id
  tidyr::replace_na(list(family = "Unknown",genus = "Unknown", species = "spp"))%>% # Removes all NA's
  dplyr::mutate(family = ifelse(family%in%c("NA",""),"Unknown", as.character(family)))%>%
  dplyr::mutate(genus = ifelse(genus%in%c("genus","NA","NANA","sp","spp","unknown",""),"Unknown", as.character(genus)))%>%
  dplyr::mutate(species = ifelse(species%in%c(NA,"NA","sp",""),"spp", as.character(species)))%>%
  dplyr::mutate(length=as.numeric(length))%>%
  dplyr::mutate(number=as.numeric(number))%>%
  select(opcode,family,genus,species,number,length,stage,activity,periodtime,range)%>%
  mutate(scientific=paste(family,genus,species,sep = " "))%>%
  dplyr::rename(sample=opcode)%>%
  glimpse()
names(length3dpoints)

## Write data
setwd(data.dir)
dir()

write_csv(maxn, path=paste(study,"maxn.csv",sep = "."))
write_csv(length3dpoints, path=paste(study,"Len3DPoints.csv",sep = "."))

# GO TO SCRIPT 3

