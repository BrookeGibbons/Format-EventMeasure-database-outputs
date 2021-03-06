### Make complete.maxn and complete.length.number.mass data from Checked.maxn and Checked.length data created from EventMeasure data ###
### Written by Tim Langlois, adpated and edited by Brooke Gibbons

### Please forward any updates and improvements to timothy.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au 
### or make a pull request on the GitHub repository "Format-EventMeasure-database-outputs"

### OBJECTIVES ###
# 1. Import checked data
# 2. Make factors
# 3. Make complete.maxn data -  PeriodTime will represent the first PeriodTime of MaxN if PeriodTime has been set to zero at Time os Seabed in EM.
    # a. useful for abundance metrics - that do not account for body size or range/sample unit size
# 4. Make complete.length.number.mass data:
    # a. useful for calculating abundance/mass based on length rules - and that account for range/sample unit size
    # b. useful for length analyses (e.g. mean length, KDE, histograms) - after expansion
# 5. Make mass estimates from Length using a and b from Master list
# 6. Write complete data sets for further analysis

# Clear memory ----
rm(list=ls())

# Libraries required ----
library(tidyr)
library(dplyr)
library(ggplot2)
library(googlesheets)
library(readr)
library(httpuv)
library(stringr)

# Study name---
study<-"Example" ## change for your project

# A function we should use throughout-----TJL------
clean_names <- function(dat){
  # Takes a data.frame, returns the same data frame with cleaned names
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("%", "percent", .) %>%
    make.names(.) %>%
    gsub("[.]+", ".", .) %>%
    tolower(.) %>%
    gsub("_$", "", .)
  setNames(dat, new_names)
}

# Set working directory ----
work.dir=("C:/GitHub/Format-EventMeasure-database-outputs") ## Change to your directory

# Set sub directories----
plots.dir=paste(work.dir,"Plots",sep="/")
data.dir=paste(work.dir,"Data",sep="/")
export.dir=paste(data.dir,"Database output",sep="/")
temp.dir=paste(data.dir,"Temporary data",sep="/")
tidy.dir=paste(data.dir,"Tidy data",sep="/")

# Read in the data----
setwd(tidy.dir)
dir()

# Make species families to merge back in after data is complete -----
maxn.families<-read_csv(file=paste(study,"checked.maxn.csv",sep = "_"),na = c("", " "))%>%
  filter(!(family=="Unknown"))%>%
  select(c(family,genus,species,scientific))%>%
  distinct() #to join back in after complete

# Make complete.maxn from maxn and complete.length.number.mass from length3D----
# Make complete.maxn: fill in 0, make Total and Species Richness and join in factors----
dat<-read_csv(file=paste(study,"checked.maxn.csv",sep = "_"),na = c("", " "))%>%
  select(c(sample,family,genus,species,maxn))%>%
  complete(sample,nesting(family,genus,species)) %>%
  replace_na(list(maxn = 0))%>%
  group_by(sample,family,genus,species)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>% #always a good idea to ungroup() after you have finished using the group_by()!
  mutate(scientific=paste(family,genus,species,sep=" "))%>%
  dplyr::select(sample,scientific,maxn)%>%
  spread(scientific,maxn, fill = 0)%>%
  glimpse()

# Presence.Absence <- dat[,2:(ncol(dat))-1]
# for (i in 1:dim(Presence.Absence)[2]){
#   Presence.Absence[,i] <- ifelse(Presence.Absence[,i]>0,1,0)
# }

complete.maxn<-dat%>%
  gather(key=scientific, value = maxn,-sample)%>%
  data.frame()%>%
  inner_join(maxn.families,by=c("scientific"))%>%
  glimpse()

# Make complete.length.number.mass: fill in 0 and join in factors----
# This data is useful for calculating abundance based on length rules--
length.families<-read_csv(file=paste(study,"checked.length.csv",sep = "_"),na = c("", " "))%>%
  filter(!(family=="Unknown"))%>%
  select(family,genus,species)%>%
  distinct()%>% #to join back in after complete
  glimpse()

complete.length.number<-read_csv(file=paste(study,"checked.length.csv",sep = "_"))%>% #na = c("", " "))
  filter(!(family=="Unknown"))%>%
  dplyr::select(sample,family,genus,species,length,number,range,activity)%>%
  complete(sample,nesting(family,genus,species)) %>%
  replace_na(list(number = 0))%>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
  ungroup()%>%
  filter(!is.na(number))%>% #this should not do anything
  mutate(length=as.numeric(length))%>%
  data.frame()%>%
  glimpse()

# MAKE mass data from number.length.complete----
# Import master from Life-history-
master<-gs_title("Australia.life.history")%>%
  gs_read_csv(ws = "australia.life.history")%>%clean_names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('NW', marine.region))%>%
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  select(family,genus,species,marine.region,length.measure,a,b,all,bll,fb.length_max,fb.ltypemaxm)%>% 
  distinct()%>%
  glimpse()

## Biomass ---
# Check for species missing length weight relationship ----
taxa.missing.lw <- complete.length.number%>%
  distinct(family,genus,species)%>%
  anti_join(filter(master,!is.na(a)), by=c("family","genus","species"))%>%
  glimpse() # 8 missing - all spp's

# Missing Genus length weight ----
genus.missing.lw <- complete.length.number%>%
  distinct(genus)%>%
  anti_join(filter(master,!is.na(a)), by="genus") # 1 (Unknown)

# Missing Family length weight ----
family.missing.lw <- complete.length.number%>%
  distinct(family)%>%
  anti_join(filter(master,!is.na(a)), by="family") # None

#4. Fill length data with relevant a and b and if blank use family?----
length.species.ab<-master%>% #done this way around to avoid duplicating Family coloum
  select(-family)%>%
  inner_join(complete.length.number,., by=c("genus","species")) # only keeps row if has a and b

family.lw <- master%>%
  dplyr::group_by(family,length.measure)%>%
  dplyr::mutate(log.a = log10(a))%>%     
  dplyr::summarise(a = 10^(mean(log.a, na.rm = T)),
                   b = mean(b, na.rm = T),
                   all = mean(all, na.rm = T),
                   bll = mean(bll, na.rm = T))%>%
  filter(!is.na(a))%>%
  dplyr::mutate(all=str_replace_all(all,"NaN","0"))%>%
  dplyr::mutate(bll=str_replace_all(bll,"NaN","1"))%>%
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(rank = ifelse(length.measure=="FL",1,ifelse(length.measure=="TL", 2, 3)))%>%
  dplyr::mutate(min.rank = rank - min(rank, na.rm = TRUE))%>%
  dplyr::filter(min.rank == 0)

length.family.ab<-complete.length.number%>%
  anti_join(master, by=c("genus","species"))%>%
  left_join(family.lw, by="family")

# 5. Fill length data with relevant a and b and if blank use family?----
complete.length.number.mass<-length.species.ab%>%
  bind_rows(length.family.ab)%>%
  dplyr::filter(!is.na(a))%>% #this gets rid of species with no lw
  mutate(length.cm = length/10)%>%
  mutate(all = ifelse(is.na(all)&length.measure%in%c("TL", "FL","SL"), 0, all))%>% # Temporary fix, remove later
  mutate(bll = ifelse(is.na(bll)&length.measure%in%c("TL", "FL","SL"), 1, bll))%>% # Temporary fix, remove later
  mutate(adjLength = ((length.cm*bll)+all)) %>% 
  mutate(mass.g = (adjLength^b)*a*number)%>%
  dplyr::select(c(sample,family,genus,species,length,range,number,mass.g,length.cm))%>%
  glimpse()


#5. Check the mass estimates across species - in kg's----
top.mass<- complete.length.number.mass %>%
  dplyr::group_by(family,genus,species)%>%
  filter(mass.g>0)%>%
  dplyr::mutate(mass.kg.individual = (mass.g/number)/1000)%>% # Work out the mass per individual fish
  dplyr::mutate(length=length/10)%>%
  mutate(length=round(length,digits=2))%>%
  dplyr::summarise(mean.kg = mean(mass.kg.individual, na.rm = TRUE),max.kg = max(mass.kg.individual, na.rm = TRUE),min.kg = min(mass.kg.individual, na.rm = TRUE),min.length = min(length, na.rm = TRUE),mean.length = mean(length, na.rm = TRUE),max.length = max(length, na.rm = TRUE))%>%
  arrange(-mean.kg)%>%
  glimpse()%>%
  mutate(mean.kg=round(mean.kg,digits=3))%>%
  mutate(max.kg=round(max.kg,digits=3))%>%
  mutate(min.kg=round(min.kg,digits=3))%>%
  mutate(mean.length=round(mean.length,digits=2))

# WRITE FINAL complete and expanded data----
setwd(tidy.dir)
dir()

write.csv(complete.maxn, file=paste(study,"complete.maxn.csv",sep = "_"), row.names=FALSE)

write.csv(complete.length.number.mass, file=paste(study,"complete.length.number.mass.csv",sep = "_"), row.names=FALSE)

write.csv(complete.length.number, file=paste(study,"complete.length.number.csv",sep = "_"), row.names=FALSE)

# GO TO SCRIPT 5