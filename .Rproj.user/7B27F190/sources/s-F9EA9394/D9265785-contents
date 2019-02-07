### Check of MaxN and Length/3D points from EventMeasure data tables held in GlobalArchive ###
### Written by Tim Langlois, adpated and edited by Brooke Gibbons

### Please forward any updates and improvements to timothy.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au 
### or make a pull request on the GitHub repository "Format-EventMeasure-database-outputs"

### OBJECTIVES
# 1. Combine complete data with habitat 
# 2. Write long data sets for further analysis

# Clear memory ----
rm(list=ls())

# Libraries required
library(tidyr)
library(dplyr)
library(googlesheets)

# Study name---
study<-"Example" ## change for your project

# Set working directory ----
work.dir=("C:/GitHub/Format-EventMeasure-database-outputs") ## Change to your directory

# Set sub directories----
data.dir=paste(work.dir,"Data",sep="/")
temp.dir=paste(data.dir,"Temporary data",sep="/")
tidy.dir=paste(data.dir,"Tidy data",sep="/")

# Load metadata ----
setwd(data.dir)
metadata<-read.csv(paste(study,"metadata.csv",sep="_"))%>%
  setNames(tolower(names(.)))

maxn.metadata<-metadata%>%
  filter(successful.count=="Yes")

length.metadata<-metadata%>%
  filter(successful.length=="Yes")

# Read in data ----
setwd(tidy.dir)

# MaxN and Length ----
complete.maxn<-read.csv(file=paste(study,"complete.maxn.csv",sep = "_"))
complete.length.number<-read.csv(file=paste(study,"complete.length.number.csv",sep = "_"))
complete.length.number.mass<-read.csv(file=paste(study,"complete.length.number.mass.csv",sep = "_"))

setdiff(complete.maxn$sample,complete.length.number$sample) # Four drops have count but not length
setdiff(complete.length.number$sample,complete.maxn$sample) ## no differences

# Habitat ----
habitat<-read.csv(paste(study,"habitat.output.csv",sep="_"))%>%glimpse()

# WRITE FINAL complete + habitat data----
setwd(tidy.dir)
dir()

complete.maxn.hab<-complete.maxn%>%
  inner_join(maxn.metadata, by="sample")%>%
  inner_join(habitat, by="sample")%>%
  write.csv(file=paste(study,"complete.maxn.hab.csv",sep = "_"), row.names=FALSE)

complete.length.number.hab<-complete.length.number%>%
  inner_join(length.metadata, by="sample")%>%
  inner_join(habitat, by="sample")%>%
  write.csv(file=paste(study,"complete.length.number.hab.csv",sep = "_"), row.names=FALSE)

complete.length.number.mass.hab<-complete.length.number.mass%>%
  inner_join(length.metadata, by="sample")%>%
  inner_join(habitat, by="sample")%>%
  write.csv(file=paste(study,"complete.length.number.mass.hab.csv",sep = "_"), row.names=FALSE)

# DATA CLEANING FINSIHED :)
