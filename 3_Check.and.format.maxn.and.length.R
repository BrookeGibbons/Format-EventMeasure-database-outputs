###### Check of MaxN and Length/3D points from EventMeasure data tables held in GlobalArchive ######
## Milly Piggott
## 06/11/2018

### Written by Tim Langlois 
### Any errors are due to Tim Langlois
### Please forward any updates and improvements to timothy.langlois@uwa.edu.au

# The following code forms an appendix to the manuscript:
#  "Langlois, T. J., L. M. Bellchambers, R. Fisher, G. R. Shiell, J. Goetze, L. Fullwood, S. N. Evans, N. Konzewitsch, E. S. Harvey, and M. B. Pember. 2016. Investigating ecosystem processes using targeted fisheries closures: can small-bodied invertivore fish be used as indicators for the effects of western rock lobster fishing? Marine and Freshwater Research."

# Please cite it if you like it

### Designed to: 
#   check data resulting in queries from GlobalArchive or EventMeasure. 
#   this script is designed to be used in an interative process to suggest corrections that can/should be made to original EventMeasure or GlobalArchive files.


### objective is to 
# 1. Import data and create Genus_species column
# 2. run BASIC data checks
# 3. Limit length data by range and precision rules
# 4. run SERIOUS checks against a master species list
# 5. Remove species that can never be ID'd
# 6. Visualise what MaxN are missing in the stereoMaxN
# 7. Write data for analysis that passes checks


# Naming conventions----
# data objects in lower case
# column names Capitalized


# Libraries required
library(tidyr)
library(dplyr)
library(magrittr)
library(data.table)
library(ggplot2)
library(googlesheets)
library(httpuv)
library(readr)
library(RCurl) #needed to download data from GitHub


# Study name----
rm(list=ls()) #clear memory
study<-"POC"

# Metadata ----
metadata<-gs_title("MEG_Labsheets")%>%
  gs_read(ws = "RPS_2018")%>%
  setNames(tolower(names(.)))%>%
  dplyr::rename(Sample=sample)

# Set work directory----
#work.dir=("~/Google Drive/Analysis/Project_RPS_OldCoastLine/Analysis_RPS_OldCoastLine/") #TIM ??
work.dir=("C://Users/Milly/Google Drive/Project_RPS_OldCoastLine/Analysis_RPS_OldCoastLine/") ## Milly PC
#work.dir=("~/Google Drive/Project_RPS_OldCoastLine/Analysis_RPS_OldCoastLine/") ## Milly Mac
work.dir=("C:/Users/00097191/Google Drive/MEG/Projects/Project_RPS_OldCoastLine/Analysis_RPS_OldCoastLine") ## Brooke

# Set sub directories----
data.dir=paste(work.dir,"Data/Data cleaning",sep="/")
plots.dir=paste(work.dir,"Plots",sep="/")
tidy.data.dir=paste(work.dir,"Data/Tidy data",sep="/")

# Load functions from GitHub----
gsr <- getURL("https://raw.githubusercontent.com/TimLanglois/Fuctions/master/gsr.r", ssl.verifypeer = FALSE)
eval(parse(text = gsr))
detach("package:RCurl", unload=TRUE)#will error - don't panic - need to make sure it is not loaded as will interfer with dplyr() #TJL

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


# Import MaxN and length/3d files----
setwd(data.dir)
dir()

# Import MaxN file---
maxn<-read_csv("POC.maxn.csv")%>%
  mutate(maxn=as.numeric(maxn))%>%
  filter(family!="Unknown")%>%
  data.frame()%>%
  glimpse()

# Import length/3d file----
length<-read_csv(file=paste(study,"Len3DPoints.csv",sep = "."),na = c("", " "))%>%
  mutate(number=as.numeric(number))%>%
  mutate(range=as.numeric(range))%>%
  mutate(length=as.numeric(length))%>%
  filter(!is.na(number)) %>% #find and remove sync points that are not fish
  data.frame()%>%
  glimpse()

# BASIC Checks----
setwd(data.dir)

# Check if we have 3d points (Number) in addition to length----
three.d.points<-length%>%
  filter(is.na(length))%>%
  filter(!is.na(number))
head(three.d.points,2) #Do we have 3d points? YES


# Check if we have schools associated with single length measures----
schools<-length%>%
  filter(number>1) 
head(schools,2) #Do we have schools? YES


#Standardise for RANGE and Error for Length----
# To standardise for RANGE and Error we can remove any length observations outside Range and Error rules
# i.e. the length data, and any abundnance calculated from it, will be restricted by range

summary(length$range)
out.of.range<-filter(length,range>10000);head(out.of.range,2) ## none out of range

# Check on the BIG fish length data----
fish.greater.than.1.meter<-filter(length,length>1000) # All sharks- checked
head(fish.greater.than.1.meter,2)
write.csv(fish.greater.than.1.meter,file=paste(study,"check","length.greater.than.1.meter.csv",sep = "."), row.names=FALSE)

# Plots to visually check length and range data----
# Plot to visualise length data---
setwd(plots.dir)
check.length<-ggplot(data=length, aes(as.numeric(length))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="blue",
                 alpha = .2)
check.length ## most fish less than 300mm
ggsave(check.length,file=paste(study,"check.length.png",sep = "."))

# Plot to visualise range data---
check.range<-ggplot(data=length, aes(as.numeric(range))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="green",
                 alpha = .2)
check.range ## most fish recorded under 2 m away
ggsave(check.range,file=paste(study,"check.range.png",sep = "."))

# Plot to visualise length/range data---
check.range.vs.length<-ggplot(data=length, aes(range,length)) +
  geom_point()+
  geom_smooth()
check.range.vs.length ## mostly linearly correlated with some outlies of big fish close in
ggsave(check.range.vs.length,file=paste(study,"check.range.vs.length.png",sep = "."))


# SERIOUS data checking to compare taxa to Master list and correct any names that may have changed----
# # Read in Species list to compare against----
master<-gs_title("Australia.life.history")%>%gs_read_csv(ws = "australia.life.history")%>%clean_names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('NW', marine.region))%>%
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  select(family,genus,species,marine.region,length.measure,a,b,all,bll,fb.length_max,fb.ltypemaxm)%>% # maybe use RLS length instead
  distinct()%>%
  glimpse()

# Update names of species that may have changed----
# life-history-sheet ---
synonyms <- gs_title("Synonyms_Australia")%>%
  gs_read_csv(ws = "Synonyms_Australia")%>%
  distinct()%>%clean_names()%>%select(-comment)

# Change synonyms
maxn<-left_join(maxn,synonyms,by=c("family","genus","species"))%>%
  mutate(genus=ifelse(!is.na(genus_correct),genus_correct,genus))%>%
  mutate(species=ifelse(!is.na(species_correct),species_correct,species))%>%
  mutate(family=ifelse(!is.na(family_correct),family_correct,family))%>%
  select(-c(family_correct,genus_correct,species_correct))

length<-left_join(length,synonyms,by=c("family","genus","species"))%>%
  mutate(genus=ifelse(!is.na(genus_correct),genus_correct,genus))%>%
  mutate(species=ifelse(!is.na(species_correct),species_correct,species))%>%
  mutate(family=ifelse(!is.na(family_correct),family_correct,family))%>%
  select(-c(family_correct,genus_correct,species_correct))

# Check for taxa.not.match----
setwd(data.dir)

maxn.taxa.not.match.life.history<-
  master%>%
  anti_join(maxn,.,by=c("family","genus","species"))%>%
  distinct(sample,family,genus,species)%>%
  filter(!species%in%c("spp","sp10","sp1"))

write.csv(maxn.taxa.not.match.life.history,file=paste(study,"check.maxn.taxa.not.match.life.history","csv",sep = "."), row.names=FALSE)

length.taxa.not.match<-
  master%>%
  anti_join(length,.,by=c("family","genus","species"))%>%
  dplyr::distinct(sample,family,genus,species)%>%
  filter(!species%in%c("spp","sp10","sp1"))

write.csv(length.taxa.not.match,file=paste(study,"check.length.taxa.not.match.life.history","csv",sep = "."), row.names=FALSE)

### SERIOUS Check for Min Max Length compared to Master list----
library(plyr)
names(master)

family.max.length<-master%>%
  replace_na(list(fb.length_max=0))%>%
  dplyr::group_by(family)%>%
  dplyr::summarise(famlength_max=mean(fb.length_max),na.rm = T)%>%
  dplyr::filter(!famlength_max==0)%>%select(-na.rm)

#add family max lengths when no max length to master
fam.master<-left_join(master,family.max.length,by=c("family"))%>%
  dplyr::mutate(fb.length_max=ifelse((is.na(fb.length_max)),famlength_max,fb.length_max))%>%
  dplyr::select(-c(famlength_max))%>%
  dplyr::select(family,fb.length_max)%>%
  dplyr::group_by(family)%>%
  dplyr::summarise(fb.length_max=mean(fb.length_max))

wrong.length.taxa<-left_join(length,master,by=c("family","genus","species"))%>%
  dplyr::filter(length >= fb.length_max)%>%
  dplyr::select(sample,family,genus,species,length,fb.length_max,fb.ltypemaxm)%>%
  dplyr::mutate(percent.error=(length-fb.length_max)/fb.length_max*100)%>%
  dplyr::arrange(desc(percent.error))

wrong.length.family<-left_join(length,fam.master,by=c("family"))%>%
  dplyr::filter(length >= fb.length_max)%>%
  dplyr::mutate(percent.error=(length-fb.length_max)/fb.length_max*100)%>%
  dplyr::arrange(desc(percent.error))

write.csv(wrong.length.taxa,file=paste(study,"check.wrong.length.taxa.vs.life.history","csv",sep = "."), row.names=FALSE)

write.csv(wrong.length.family,file=paste(study,"check.wrong.length.family.vs.life.history","csv",sep = "."), row.names=FALSE)

## Drop wrong lengths ----
drop.length.taxa.big<-wrong.length.taxa%>% #TO REMOVE LENGTHS OUTSIDE THE MIN/MAX OF MASTER LIST
  distinct(family,genus,species,length)%>%
  dplyr::select(family,genus,species,length)%>%
  dplyr::mutate(key = paste(family,genus,species,length, sep = '_'))

drop.length.family.big<-wrong.length.family%>% #TO REMOVE LENGTHS OUTSIDE THE MIN/MAX OF MASTER LIST
  distinct(family,genus,species,length)%>%
  dplyr::select(family,genus,species,length)%>%
  dplyr::mutate(key = paste(family,genus,species,length, sep = '_'))

drops.length.big<-bind_rows(drop.length.taxa.big,drop.length.family.big)%>%
  distinct()

length<-length%>%
  dplyr::mutate(key = paste(family,genus,species,length, sep = '_'))%>%
  #anti_join(drop.length.big,by="key")%>% # for dropping wrong.lengths
  dplyr::select(-c(key))%>%
  glimpse()

###########################################
# # # Check how many MaxN per Genus_species are missing from StereoMaxN----
# e.g. how many lengths are missing from the possible MaxN
#############################################

length.to.match.maxn<-master%>%
  select(family,genus,species)%>%
  semi_join(length,., by = c("family", "genus", "species"))%>%
  distinct(family,genus,species)%>% 
  select(family,genus,species)%>%
  semi_join(maxn,., by = c("family", "genus", "species"))%>%
  semi_join(length,., by = c("family", "genus", "species"))%>%
  select(family,genus,species,number,sample)%>%
  mutate(data = "stereomaxn")

length.sample <- length.to.match.maxn %>%
  distinct(sample)

maxn.match.length<-master%>%
  select(family,genus,species)%>%
  semi_join(maxn,.,by = c("family", "genus", "species"))%>%
  distinct(family,genus,species)%>% 
  select(family,genus,species)%>%
  semi_join(length,.,by = c("family", "genus", "species"))%>%
  semi_join(maxn,.,by = c("family", "genus", "species"))%>%
  semi_join(length.sample,by="sample")%>% # subset maxn to only those OpCode that match OpCodes from length
  select(family,genus,species,maxn,sample)%>%
  mutate(data = "maxn")%>%
  dplyr::rename(number = maxn)%>%
  bind_rows(length.to.match.maxn)
head(maxn.match.length)


# Summarise the matched data by taxa
x<-"taxa.maxn.vs.stereo.summary"
taxa.maxn.vs.stereo.summary <- maxn.match.length %>%
  group_by(family,genus,species,data,sample) %>%
  dplyr::summarise(maxn = sum(number))%>%
  spread(data,maxn)%>%
  mutate(percent.diff = (maxn-stereomaxn)/maxn)%>%
  replace_na(list(percent.diff=1))%>%
  filter(!percent.diff%in%c(0))%>%glimpse()
write.csv(taxa.maxn.vs.stereo.summary,file=paste( study,x,".csv",sep = "_"), row.names=FALSE)

# WRITE FINAL checked data----
setwd(tidy.data.dir)
dir()

# MaxN of species that also occur in master list
write.csv(maxn, file=paste(study,"checked.maxn.csv",sep = "."), row.names=FALSE)

## Remove juveniles from wrong.length.taxa sheet
write.csv(length, file=paste(study,"checked.length.csv",sep = "."), row.names=FALSE)

# GO TO SCRIPT 4
