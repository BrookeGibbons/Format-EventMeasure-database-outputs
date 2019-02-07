### objective is to 

# 1. Combine complete data with habitat 
# 6. Write long data sets for further analysis

## Milly
## 07/11/2018

# Naming conventions----
# data objects in lower case
# column names Capitalized


# Libraries required
library(tidyr)
library(dplyr)
library(googlesheets)


# Study name----
rm(list=ls()) #clear memory
study<-"POC"


# Set work directory----
#work.dir=("~/Google Drive/Analysis/Project_RPS_OldCoastLine/Analysis_RPS_OldCoastLine/") #TIM ??
#work.dir=("C://Users/Milly/Google Drive/Project_RPS_OldCoastLine/Analysis_RPS_OldCoastLine/") ## Milly PC
work.dir=("~/Google Drive/Project_RPS_OldCoastLine/Analysis_RPS_OldCoastLine/") ## Milly Mac
work.dir=("C:/Users/00097191/Google Drive/MEG/Projects/Project_RPS_OldCoastLine/Analysis_RPS_OldCoastLine") ## Brooke

# Set sub directories----
data.dir=paste(work.dir,"Data/Tidy data",sep="/")
plots.dir=paste(work.dir,"Plots",sep="/")
tidy.data.dir=paste(work.dir,"Data/Tidy data",sep="/")

# Read in the data----
setwd(tidy.data.dir)
dir()

metadata<-gs_title("MEG_Labsheets")%>%
  gs_read(ws = "RPS_2018")%>%
  setNames(tolower(names(.)))%>%
  select(sample,date,time,depth,site,location,status,latitude,longitude,successful.count,successful.length)
names(metadata)

maxn.metadata<-metadata%>%
  filter(successful.count=="Yes")

length.metadata<-metadata%>%
  filter(successful.length=="Yes")

x<-"complete.maxn"
complete.maxn<-read.csv(file=paste(study,x,"csv",sep = "."))
x<-"complete.length.number"
complete.length.number<-read.csv(file=paste(study,x,"csv",sep = "."))
x<-"complete.length.number.mass"
complete.length.number.mass<-read.csv(file=paste(study,x,"csv",sep = "."))


setdiff(complete.maxn$sample,complete.length.number$sample) # Four drops have count but not length
setdiff(complete.length.number$sample,complete.maxn$sample) ## no differences

habitat<-read.csv("POC.R_habitat.output.csv")%>%glimpse()
velocity<-read.csv("POC.velocity.csv")
visibility<-read.csv("POC.visibility.wide.csv")
covariates<-read.csv("POC.covariates.csv")
tide<-read.csv("POC.tide.conversion.csv")
status<-read.csv("POC.status.csv")

# WRITE FINAL complete + habitat data----
setwd(data.dir)
dir()

x<-"complete.maxn.hab"
complete.maxn.hab<-complete.maxn%>%
  inner_join(maxn.metadata, by="sample")%>%
  dplyr::rename(fished.status=status)%>%
  inner_join(habitat, by="sample")%>%
  inner_join(velocity, by="sample")%>%
  inner_join(visibility, by="sample")%>%
  inner_join(covariates)%>%
  inner_join(status,by="sample")%>%
  inner_join(tide,by="sample")%>%
  write.csv(file=paste(study,x,"csv",sep = "."), row.names=FALSE)

test<-anti_join(tide,maxn.metadata)

x<-"complete.length.number.hab"
complete.length.number.hab<-complete.length.number%>%
  inner_join(length.metadata, by="sample")%>%
  dplyr::rename(fished.status=status)%>%
  inner_join(habitat, by="sample")%>%
  inner_join(velocity, by="sample")%>%
  inner_join(visibility, by="sample")%>%
  inner_join(covariates)%>%
  inner_join(status,by="sample")%>%
  inner_join(tide,by="sample")%>%
  write.csv(file=paste(study,x,"csv",sep = "."), row.names=FALSE)

x<-"complete.length.number.mass.hab"
complete.length.number.mass.hab<-complete.length.number.mass%>%
  inner_join(length.metadata, by="sample")%>%
  dplyr::rename(fished.status=status)%>%
  inner_join(habitat, by="sample")%>%
  inner_join(velocity, by="sample")%>%
  inner_join(visibility, by="sample")%>%
  inner_join(covariates)%>%
  inner_join(status,by="sample")%>%
  inner_join(tide,by="sample")%>%
  write.csv(file=paste(study,x,"csv",sep = "."), row.names=FALSE)

# DATA CLEANING FINSIHED :)

## Write data sheet for abudnance and biomass with no first day samples 
setwd(data.dir)
x<-"complete.maxn.hab.filt"
complete.maxn.hab<-read.csv("POC.complete.maxn.hab.csv")%>%
  #filter(mass.g!="NA")%>%
  filter(!sample %in% c("POC11001","POC11002","POC11003","POC11004","POC11005","POC12001","POC12002","POC12003","POC12004","POC12005","POC13001","POC13002","POC13003","POC13004","POC13005"))%>%
  write.csv(file=paste(study,x,"csv",sep = "."), row.names=FALSE)

x<-"complete.length.number.mass.hab.filt"
complete.length.number.mass.hab<-read.csv("POC.complete.length.number.mass.hab.csv")%>%
  #filter(mass.g!="NA")%>%
  filter(!sample %in% c("POC11001","POC11002","POC11003","POC11004","POC11005","POC12001","POC12002","POC12003","POC12004","POC12005","POC13001","POC13002","POC13003","POC13004","POC13005"))%>%
  write.csv(file=paste(study,x,"csv",sep = "."), row.names=FALSE)


