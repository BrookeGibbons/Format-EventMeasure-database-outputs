### Importing Habitat Annotation points from TransectMeasure (Stereo) ###
### Written by Tim Langlois, adpated and edited by Brooke Gibbons

### Please forward any updates and improvements to timothy.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au 
### or make a pull request on the GitHub repository "Format-EventMeasure-database-outputs"

### OBJECTIVES ###
# 1. Import and combine data from .txt file data collected in a 4 x 5 grid of CATAMI and relief codes
# 2. Make % scores and levels for different groups

# Clear memory ----
rm(list=ls())

# Libraries required ----
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
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

# Load and format habitat annotation data from TransectMeasure----
setwd(export.dir)

hab<-read.delim(paste(study,"Dot Point Measurements.txt",sep="_"),header=T,skip=4,stringsAsFactors=FALSE)%>%
  setNames(tolower(names(.)))%>%
  separate(filename,into=c("sample","camera"))%>% # in this example the videos where analysed in TM, so this will remove the camera names at the end of the video name
  select(-c(camera,frame,time..mins.,date,location,site..,transect..,latitude,longitude,rugosity,depth,collector,fishing.status,spare,spare.1,code,radius..))%>% # remove unnecessary columns
  glimpse()

# Formatting checks ----
number.of.annotations<-hab%>%
  group_by(sample)%>%
  dplyr::summarise(n=n())

unique(number.of.annotations$n) # should be 20 (if using a 5 x 4 grid), if more or less will need to go back into TMObs and edit and re-export data
  
not.in.metadata<-anti_join(metadata,hab) # all these drops were unsuccessful for fish, but the habitat was still analysed

dont.match.metadata<-anti_join(hab,metadata) # zero

# Create fov----
fov<-hab%>%
  select(-c(broad,morphology,type,relief,bedforms,bedform.type))%>%
  filter(!fieldofview=="")%>%
  filter(!is.na(fieldofview))%>%
  mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
  mutate(count=1)%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  select(-c(image.row,image.col))%>%
  group_by(sample)%>%
  summarise_all(funs(sum))%>%
  group_by(sample)%>%
  mutate_all(funs(./20))%>%
  mutate_all(funs(replace(.,is.na(.),0)))%>%
  glimpse()

# Create relief----
relief<-hab%>%
  filter(!broad%in%c("Open Water","Unknown"))%>%
  filter(!relief%in%c(""))%>%
  select(-c(broad,morphology,type,fieldofview,image.row,image.col,bedforms,bedform.type))%>%
  mutate(relief.rank=ifelse(relief=="0. Flat substrate, sandy, rubble with few features. ~0 substrate slope.",0,ifelse(relief=="1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope.",1,ifelse(relief=="2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope.",2,ifelse(relief=="3. Good relief structure with some overhangs. >45 substrate slope.",3,ifelse(relief==".4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope.",4,ifelse(relief==".5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope.",5,relief)))))))%>%
  select(-c(relief))%>%
  mutate(relief.rank=as.numeric(relief.rank))%>%
  group_by(sample)%>%
  dplyr::summarise(mean.relief= mean (relief.rank), sd.relief= sd (relief.rank))%>%
  glimpse()

# CREATE catami_broad------
broad<-hab%>%
  select(-c(fieldofview,morphology,type,relief,bedforms,bedform.type))%>%
  filter(!broad%in%c("",NA,"Unknown","Open Water","Open.Water"))%>%
  mutate(broad=paste("broad",broad,sep = ". "))%>%
  mutate(count=1)%>%
  group_by(sample)%>%
  spread(key=broad,value=count,fill=0)%>%
  select(-c(image.row,image.col))%>%
  group_by(sample)%>%
  summarise_all(funs(sum))%>%
  mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  group_by(sample)%>%
  mutate_each(funs(./Total.Sum), matches("broad"))%>%  
  select(-Total.Sum)%>%
  glimpse

# CREATE catami_morphology------
morphology.only<-hab%>%
  filter(!morphology%in%c("",NA,"Unknown"))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  filter(type%in%c(NA,"","NA","Na"))%>%
  mutate(morphology=paste("detailed",broad,morphology,sep = ": "))

morphology.with.type<-hab%>%
  filter(!morphology%in%c("",NA,"Unknown"))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  filter(!type%in%c(NA,"","NA","Na"))%>%
  mutate(morphology=paste("detailed",broad,morphology,type,sep = ": "))

morphology<-bind_rows(morphology.only,morphology.with.type)%>%
  select(-c(fieldofview,broad,type,relief,bedforms,bedform.type))%>%
  mutate(count=1)%>%
  group_by(sample)%>%
  spread(key=morphology,value=count,fill=0)%>%
  select(-c(image.row,image.col))%>%
  group_by(sample)%>%
  # mutate_each(funs(replace(.,is.na(.),0)))%>%
  summarise_all(funs(sum))%>%
  mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  group_by(sample)%>%
  mutate_each(funs(./Total.Sum), matches("detailed"))%>%
  select(-Total.Sum)%>%
  glimpse()

# CREATE catami_bedforms------
bedforms<-hab%>%
  select(-c(fieldofview,broad,morphology,type,relief))%>%
  filter(!bedforms%in%c("",NA,"Unknown"))%>%
  mutate(bedforms=paste("bedform.",bedforms,bedform.type,sep = " "))%>%
  mutate(bedforms=str_replace_all(.$bedforms,c("None :"="None","Bioturbated :"="Bioturbated")))%>%
  select(-c(bedform.type))%>%
  mutate(count=1)%>%
  group_by(sample)%>%
  spread(key=bedforms,value=count,fill=0)%>%
  select(-c(image.row,image.col))%>%
  group_by(sample)%>%
  # mutate_each(funs(replace(.,is.na(.),0)))%>%
  summarise_all(funs(sum))%>%
  mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  group_by(sample)%>%
  mutate_each(funs(./Total.Sum), matches("bedform."))%>%
  select(-Total.Sum)

# Write final habitat data----
# join starting with relief - as this is most liekly to have the most samples with habitat data
setwd(tidy.dir)
dir()

habitat<-relief%>%
  left_join(fov,by="sample")%>%
  left_join(broad,by="sample")%>% # if you have analysed to detailed scale add a hashtag here
  left_join(morphology,by="sample")%>% # if you have only annotated to broad scale add a hashtag here
  left_join(bedforms,by="sample") # add a hashtag to start of line if not annotated

write.csv(habitat,file=paste(study,"habitat.output.csv",sep = "_"), row.names=FALSE)

# Habitat.plot----
setwd(plots.dir)

habitat.plot<-habitat%>%
  gather(key=habitat, value = value, 2:ncol(.))

habitat.ggplot<-ggplot(habitat.plot,aes(x=value))+
  geom_histogram()+
  facet_grid(habitat~.,scales="free")+
  ylab("Percent cover or Value")+
  theme(strip.text.y = element_text(angle=0))
habitat.ggplot

ggsave(habitat.ggplot,file=paste(study,"habitat.plot.png",sep = "_"), width = 20, height = 25,units = "cm")
