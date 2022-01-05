
rm(list = ls())
library(tidyverse)
library(here)

options(digits = 10)
# package_path <- "/Users/martakaras/Downloads/GPSmobility_1.5.tar.gz"
# install.packages(package_path, repos = NULL, type = "source")
library(GPSmobility)


# mout = MobilityFeatures(filename,fildir)

# by myself 
# filename
# fildir
ACCURACY_LIM=51 ### meters GPS accuracy
ITRVL=10 ### seconds (data concatenation)
nreps=1 ### simulate missing data numer of times
tz="" ### time zone of data, defaults to current time zone
CENTERRAD=200 ### meters radius from significant locations considered
wtype="GLR"
spread_pars=c(10,1)
minpausedur=300
minpausedist=60
rad_fp=NULL
wid_fp=NULL

filelist <- file.path(fildir, filename)

# Function: GPS2MobMat
# #### Arguments
# filelist:  vector of cvs files containing GPS trace
# itrvl:     Interval width (in seconds) that observations are averaged over
# r:         Minimum distance between consecutive locations needed to be
#            covered to be considered movement (instead of a pause)
# w:         Maximum allowed distance from the flight path for intermediate points
# #### Value
# column 1: codes.
#           1=flight
#           2=pause
#           3=unclassified
#           4=missing data


filename <- "2015-10-09 09_00_00.csv"
fildir <- "/Users/martakaras/Downloads/GPSExample"


good_i <- numeric()
# all files
files_set <- list.files(fildir, pattern = "\\.csv$")
for (i in 1 : length(files_set)){ # i <- 39
  filelist <- files_set[i]
  tryCatch(
    {
      mobmatmiss <- GPS2MobMat(filelist,itrvl=ITRVL,accuracylim=ACCURACY_LIM,r=rad_fp,w=wid_fp)
      mobmat = GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)
      obj = InitializeParams(mobmat)
      qOKmsg = MobmatQualityOK(mobmat,obj)
      print(paste0("i = ", i, " -- qOKmsg = ", qOKmsg))
      # if data is anyhow OK 
      if (qOKmsg == ""){
        lsmf = list()
        lssigloc = list()
        out3=SimulateMobilityGaps(mobmat,obj,wtype,spread_pars)
        IDundef=which(out3[,1]==3)
        if(length(IDundef)>0){
          out3=out3[-IDundef,]      
        }
        obj3=InitializeParams(out3)
        out_GMFM=GetMobilityFeaturesMat(out3,obj3,mobmatmiss,tz,CENTERRAD,ITRVL)
        lsmf=out_GMFM[[1]]
        lssigloc=out_GMFM[[2]]
        # if data is anyhow OK 
        # if(length(lsmf)!=0){
        if(!is.na(lsmf[[2]])){
          good_i <- c(good_i, i)
          print(lsmf)
        } else {
          message("Sorry, length(lsmf)==0")
        }
      }
    },
    error=function(cond) {
      message(cond)
    })
}


# SINGLE

i <- 39
filelist <- files_set[i]
filelist
mobmatmiss <- GPS2MobMat(filelist,itrvl=ITRVL,accuracylim=ACCURACY_LIM,r=rad_fp,w=wid_fp)
mobmat = GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)
obj = InitializeParams(mobmat)
qOKmsg = MobmatQualityOK(mobmat,obj)
lsmf = list()
lssigloc = list()

# this part varies
out3=SimulateMobilityGaps(mobmat,obj,wtype,spread_pars)
IDundef=which(out3[,1]==3)
if(length(IDundef)>0){
  out3=out3[-IDundef,]      
}
obj3=InitializeParams(out3)
GetMobilityFeaturesMat(out3,obj3,mobmatmiss,tz,CENTERRAD,ITRVL)



 