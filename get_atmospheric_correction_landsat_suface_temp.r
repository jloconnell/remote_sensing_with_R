###A script to get atmospheric correction parameters from Julia Barsi's NASA web tool
###These parameters help convert top of atmosphere brightness temperature (B10 in Landsat 8, and B6 in Landsat 5)
###into surface temperature; USGS has released a provisional surface temp product 
###that will help us skip this step in the future
###This script is slow to run and, important, !CANNOT! be speed up
###Julia Barsi has asked users not to retrieve info more quickly than once every 2 mins, 
###a wait time is built into the download function and handles this for you, 
###so feel free to set your usage parameters, run it and walk away
###
###USAGE:Input file should be an Earth Explorer Landsat !cloud-free! data file with the .geo (for lat/long) and date fields retained
###      Change base directory and input file name to your file, 
###      change append.out to True or False as needed: 
###           TRUE means your running this after your first try and have old results from a previous try that you want to append to
###           Feel free to combine training, validation, etc observations from each 
###           landsat mission into 1 atmospheric parameter file, but you should separate the missions into different files
###           TRUE writes over your old file with a new combined file, 
###           it's a good idea to back-up your old file
###      change mission to 8 or 5 for Landsat 8 or 5
###      Change email to your email address, but don't give this script to others with your email address saved, 
###           Dr. Barsi will contact you via email if you misuse this tool
###      All other parameters are calcaulted automatically from the Landsat Earth Engine input file
###      After setting everything in the USE INPUT REQUIRED section below, save and run the script
###      Assumes that there is a project folder to hold everything (the "base directory") with the subfolders:
###                   "data", where the earth engine landsat data is held, and 
###                   "output", where the results of this script will go

library(httr); library(tidyverse); library(chron)

###USER INPUT REQUIRED######
##what email would like to use to a) receive results, and b) be held responsible for your use of the webtool
##example: email<- "someone@email.com", quotes are needed
email<-

##which Landsat mission? 8 or 5? just the number, no quotes
mission<-5

##add new results to an old file that was previously created from this script? TRUE/FALSE without quotes (T and F also work)
append.out<-T

###file paths, files to read in and write out####
 ##what is the path to this project folder?
 basedir<-"path/to/project/folder"

##input file: landsat, csv pts from google earth engine
lsat<-read_csv(paste0(basedir, "data/landsat_file_from_google_earth_engine.csv"))

##output file: what should we call the result?
out_file<-paste0(basedir,"output/atmospheric_correction_vars_L", mission,"_great_plains.csv")

#####END OF USER INPUT#######
lsat$time<-substr(lsat$date,12,19)
lsat$date<-as.Date(lsat$date)
lsat$year<-as.numeric(format(lsat$date, "%Y"))
lsat$doy<-as.numeric(format(lsat$date, "%j"))
lsat$mo<-as.numeric(format(lsat$date, "%m"))
lsat<-lsat[as.numeric(lsat$mo)>2,]
lsat<-lsat[as.numeric(lsat$mo)<11,]
time<-substr(as.character(mean(chron(times=lsat$time))),1,5)
##old data file we want to append new results to
if (append.out==T){
  old<-read_csv(out_file)
  old<-old[complete.cases(old)==T,]
  }

##create function to get atmospheric correction parameters from web
atmos<-function(date,lat, long, time, email, mission, profile="2"){
    ##download atmospheric variables as calculated by web tool: atmcorr.gsfc.nasa.gov (see Barsi et al. 2003; Barsi et al. 2005 )
    ##In a for loop of dates, this takes AWHILE to run, there also is a deliberate 2 minute wait at the end of the function to prevent overloading the webtool with requests, be patient
    ##The variables, unless local data are entered, were calculated as an average of 1 deg lat and long (31-32 degrees lat for example) 
    ##results were very close to measured conditions at the sapelo flux tower for days I tested
    ##profile_option = 1  Use atmospheric profile for closest integer lat/long, 2 = Use interpolated atmospheric profile for given lat/long, I used 2 (see previous sentence)
    ##stdatm_option value="1" Use mid-latitude summer standard atmosphere for upper atmospheric profile , 2 =Use mid-latitude winter standard atmosphere for upper atmospheric profile
    ##  this function uses the date to decide which stdatm to use, doy<91 &doy > 273 =winter
    ##L57_option = which landsat, put value= 8, 7,  5, or 11  where the number is the landsat mission and 11 means Output only atmospheric profile, do not calculate effective radiances
    ##time "16:00" is approx landsat passover time on the US East coast, which varies between 15:55 and 16:05 GMT, you can verify this in the landsat meta-data
    
    ##transform the variables into the format expected by the web form
    date<-as.Date(date)
    yr<-as.character(format(date, "%Y"))
    mo<-as.character(format(date, "%m"))
    day<-as.character(format(date, "%d"))
    hr<- substr(time, 1,2)
    min<- substr(time, 4,5)
    
    ##use date to decide which atomsphereic profile to use, winter or summer
    stdatm<-ifelse(as.numeric(format(date, "%j"))<91, "1","2")
    stdatm<-ifelse(as.numeric(format(date, "%j"))>273, "1",stdatm)
    
    ##check first that at least mission was entered correctly
    if(!(as.numeric(mission) %in% c(8,7,5,11))){
        print("mission must be 8,7,5 or 11")
    } else{
        ##gather form variables needed on the web form into a list
        formvars<-list(
            "year" = yr,
            "month" = mo,
            "day" = day,
            "hour" = as.character(hr),
            "minute" = as.character(min),
            "L57_option" = as.character(mission),
            "thelat" = as.character(round(lat,4)),
            "thelong" = as.character(round(long,4)),
            "profile_option" = as.character(profile),
            "user_email" = as.character(email),
            "stdatm_option" = "1", 
            "submit" ="Calculate"
        )
        ##use POST to submit the form and retrieve the result from the web
        res <- POST("https://atmcorr.gsfc.nasa.gov/cgi-bin/atm_corr.pl", body = formvars, verbose(), timeout=30001)
        x<-content(res, "text")
        
        ##process the result into a data frame of atomospheric variables that this function will return
        y<-str_split(x, "<br>")
        y<-y[[1]]
        Iu<-y[grep("upwelling radiance",y)];  Iu<-str_split(Iu, ":")[[1]][2];Iu<-as.numeric(str_split(Iu, " W")[[1]][1])
        Id<-y[grep("downwelling radiance",y)];Id<-str_split(Id, ":")[[1]][2];Id<-as.numeric(str_split(Id, " W")[[1]][1])
        trans<-y[grep("atmospheric transmission",y)]; trans<-str_split(trans, ":")[[1]][2];trans<-as.numeric(str_split(trans, " W")[[1]][1])
        out<-data.frame("date"=date, "Iu"=Iu, "Id"=Id, "trans"=trans)
        
        ##add in an automatic wait, requested by the creater of the web tool to prevent server overload
        Sys.sleep(2*60)
        return(out)
    }
}

##get location info from google earths .geo character string
x<-strsplit(lsat$".geo", "\\[")
x<-sapply(x, "[[", 2)
x<-strsplit(x, "\\]")
x<-sapply(x, "[[", 1)
x<-strsplit(x, ",")
lsat$long<-sapply(x, "[[", 1)
lsat$lat<-sapply(x, "[[", 2)
head(lsat)

##set up date, lat, and long to give to webtool, when using profile = "2" it returns an 
##average over 1 degree lat and long, so we only need one instance of each lat/long at that scale 
##and should remove duplicates after rounding
locs<-lsat[,c("date","lat", "long")]
locs$lat<-round(as.numeric(locs$lat),0)
locs$long<-round(as.numeric(locs$long),0)
locs<-unique(locs)

##remove the satellite data because they can be huge/memory hogs and we don't need them now
remove(lsat)
remove(x)


##remove dates we already analyzed, if appending to a previous file
if (append.out==T){
  old$date<-as.Date(old$date)
  x<-old[complete.cases(old)==T,c("date","lat", "long")]
  ##remove the rows from locs that we already retrieved in the "old" data.frame
  locs<-setdiff(locs,x)
}


##a dataframe to hold the results, with the right number of rows pre allocated
out_atmos<-data.frame("date"=locs$date, 
                      "lat"=rep(NA, nrow(locs)), 
                      "long"=rep(NA, nrow(locs)),
                      "Iu"=rep(NA, nrow(locs)), 
                      "Id"=rep(NA, nrow(locs)), 
                      "trans"=rep(NA, nrow(locs))
                      )

for (i in 1:nrow(locs)){
      x<- atmos(date=locs$date[i], lat=locs$lat[i], long=locs$long[i], 
                time=time, mission=mission, email=email)
      out_atmos[i,"date"]<-locs$date[i]    
      out_atmos[i,"lat"]<-locs$lat[i]    
      out_atmos[i,"long"]<-locs$long[i]    
      out_atmos[i,"trans"]<-x$trans    
      out_atmos[i,"Id"]<-x$Id
      out_atmos[i,"Iu"]<-x$Iu
}

##optionally add in the data from previous files before writing out, append.out option was set above
if(append.out==T){
  out_atmos<-rbind(old, out_atmos) }

##clean up the results (remove NAs and row duplicates if they exist), 
##order the rows by date and write out
out_atmos<-out_atmos[complete.cases(out_atmos)==T,]
out_atmos<-unique(out_atmos)
out_atmos<-arrange(out_atmos, lat,long,date)
write.csv(out_atmos, out_file, row.names=F)
