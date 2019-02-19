##MODIS compositing function, creates a composite (eg, moving average of the best available low view zenith angle data) 
##of vegetation indices such as the normalized difference vegetation index (NDVI) from a time series of daily MODIS data

##This function was used to create tide-free, cloud-free vegetation composites in our paper:
##O’Connell et al. 2017. The Tidal Marsh Inundation Index (TMII)... Remote Sensing of Environment 201:34–46.
##     http://dx.doi.org/10.1016/j.rse.2017.08.008

##This function is intended to be applied to a timeseries from a single pixel and requires a row for each day;
##if the timeseries has been filtered (for clouds, tides, etc), leave NA in the vegetation index column 
##for the filtered day, which will omit it from the composite
##to apply this over multiple pixels, use for loops, apply functions, data.table, or tidyverse functions

##This function creates an average of vegetation index values from low view angles within the specified window 
##through the following rules: 
##  -if available, create an average of up to 5 index observations with view zenith angles <40 degrees, 
##  -if obs with viewzen <40 don't exist but viewzen <55 exists, use the single minimum view zen index value, 
##  -insert NA for windows with no view zenith angles <55 degrees
##Usage, date is a date vector (from as.Date()) 
##       viewzen is the cooresponding vector of view zenith angles (numeric) from the daily MODIS data product MOD11A1
##       index is the spectral reflectance index (numeric) to be composited (ex: NDVI)
##       window is the desired length in days of the compositing window

slidefx <- function(date, index, viewzen, window){
    ##set up dates dataframe with consistent dates for each window for each year
    ##the last window of the year is unequal from the others
    years<-unique(as.numeric(format(date, "%Y")))
    data<-data.frame(date=date, viewzen=viewzen, index=index)
    days<-seq(1, to=365-(window-1),by=window)
    dates<-data.frame(days=days, year=rep(years[1],length(days)))
    for (i in 2:length(years)){
        dates<-rbind(dates, 
                    data.frame(days=days, year=rep(years[i],length(days))))
        }
    dates$date<-as.Date(paste(dates$year,dates$days),format="%Y%j") 
    dates$survey<-findInterval(dates$date, as.Date(dates$date))
    data$survey<-findInterval(as.Date(data$date), as.Date(dates$date))
    dates<-dates[dates$survey>=data$survey[1],]
    dates<-dates[dates$survey<=(data$survey[nrow(data)]+1),]
    y<-data.frame(date=seq.Date(from=dates$date[1], to=dates$date[nrow(dates)], by=1)
                  )
    y$index<-seq(from=1, to=nrow(y))              
    dates<-merge(dates,y,by="date")
    total <- nrow(data)
    result <- matrix(nrow = nrow(dates),ncol=4,
                     dimnames=list(seq(1:nrow(dates)), c("date", "doy","viewzen", "index")))
    for(i in 1:nrow(dates)){
        h<-data[data$date>=dates$date[i]&data$date<dates$date[i+1],]
        or<-order(h$viewzen,na.last=T)
        hordered<-h[or,]
        ##proceed until all the data in the window aren't NA
        if(sum(complete.cases(h))>0){
            ##if the first five observations have a low view zen, average them
            ##na.rm=T allows for an average of 5,4,3,2,or 1
            if(length(good<-which(hordered$viewzen<40))>0){
                good<-which(hordered$viewzen<40)
                result[i,c(1,2,3)] <-c(dates$date[i],dates$days[i],
                                       mean(hordered$viewzen[good],na.rm=T))
                result[i,4]<-mean(hordered$index[good][1:5],na.rm=T)
            }else{
                if(min(h$viewzen, na.rm=T)<55){
                   ##grab the one minimum view zen and index value, if viewzen <40 does not exist but viewzen <55 does
                    result[i,c(1,2,3)] <-c(dates$date[i],dates$days[i], 
                                           hordered$viewzen[1])
                    result[i,4]<-hordered$index[1]}
            }
        }else{
            ##if all view zens are high, than insert NA
            result[i,]<-c(dates$date[i],dates$days[i], NA,NA)}
    }
    result<-as.data.frame(result)
    result$date<-as.Date(result$date, origin="1970-01-01")
    return(result)
}
