### Query MaxN and Length/3D points from EventMeasure data tables downloaded from GlobalArchive ###
### Written by Tim Langlois, adpated and edited by Brooke Gibbons

### Please forward any updates and improvements to timothy.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au 
### or make a pull request on the GitHub repository "Format-EventMeasure-database-outputs"

### OBJECTIVES ###
# 1. Query datatables from EventMeasure database export function
# 2. Create a MaxN dataframe and combine Lengths and 3D measurements
# 3. Check and Format MaxN and Length

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
plots.dir=paste(work.dir,"Plots",sep="/")
data.dir=paste(work.dir,"Data",sep="/")
export.dir=paste(data.dir,"Database output",sep="/")
temp.dir=paste(data.dir,"Temporary data",sep="/")
tidy.dir=paste(data.dir,"Tidy data",sep="/")

# Load metadata ----
setwd(data.dir)
metadata<-read.csv(paste(study,"metadata.csv",sep="_"))%>%
  setNames(tolower(names(.)))

# Bring in points and calculate MaxN ----
setwd(export.dir)
maxn<-read.delim(paste(study,"Points.txt",sep="_"))%>%
  setNames(tolower(names(.)))%>%
  mutate(number=as.numeric(number))%>%
  group_by(opcode,frame,family,genus,species)%>%
  dplyr::summarise(maxn=sum(number))%>%
  group_by(opcode,family,genus,species)%>%
  slice(which.max(maxn))%>%
  ungroup()%>%
  filter(!maxn%in%c(NA,""))%>%
  tidyr::replace_na(list(family = "Unknown",genus = "Unknown", species = "spp"))%>% # Removes all NA's in family, genus and species
  dplyr::mutate(family = ifelse(family%in%c("NA",""),"Unknown", as.character(family)))%>%
  dplyr::mutate(genus = ifelse(genus%in%c("genus","NA","NANA","sp","spp","unknown","","Genus"),"Unknown", as.character(genus)))%>%
  dplyr::mutate(species = ifelse(species%in%c("NA","sp","","unknown"),"spp", as.character(species)))%>%
  dplyr::rename(sample=opcode)%>%
  mutate(scientific=paste(family,genus,species,sep = " "))%>%
  filter(!scientific=="Unknown Unknown spp")%>% # removes where completely unidentifiable
  glimpse()

# Load and format Length and 3D data from EventMeasure----
length<-read.delim(paste(study,"Lengths.txt",sep="_"))%>%
  setNames(tolower(names(.)))

threedpoints<-read.delim(paste(study,"3DPoints.txt",sep="_"))%>%
  setNames(tolower(names(.)))

length3dpoints<-threedpoints%>%
  plyr::rbind.fill(length)%>% 
  tidyr::replace_na(list(family = "Unknown",genus = "Unknown", species = "spp"))%>% # Removes all NA's
  dplyr::mutate(family = ifelse(family%in%c("NA",""),"Unknown", as.character(family)))%>%
  dplyr::mutate(genus = ifelse(genus%in%c("genus","NA","NANA","sp","spp","unknown","","Genus"),"Unknown", as.character(genus)))%>%
  dplyr::mutate(species = ifelse(species%in%c("NA","sp","","unknown"),"spp", as.character(species)))%>%
  dplyr::mutate(length=as.numeric(length))%>%
  dplyr::mutate(number=as.numeric(number))%>%
  select(opcode,family,genus,species,number,length,stage,activity,periodtime,range)%>%
  mutate(scientific=paste(family,genus,species,sep = " "))%>%
  dplyr::rename(sample=opcode)%>%
  glimpse()

## Write data
setwd(temp.dir)
dir()

write_csv(maxn, path=paste(study,"maxn.csv",sep = "_"))
write_csv(length3dpoints, path=paste(study,"Len3DPoints.csv",sep = "_"))

# GO TO SCRIPT 3

