##function to convert MODIS viewtime (solar time) to local time
#format: "doy" : integer, Julian day of year, solarTime: numeric, 10.9; long: numeric, 112.1 (for western)
Solar2Local <- function(doy,solarTime,long){
    ##below assumes date is in form YYYYdoy
    N = as.integer(doy)
    D = 2*pi/365*(N-81)
    ET = 9.87*sin(2*D)-7.53*cos(D)-1.5*sin(D)
    LSTM = 15*floor(long/15)
    diff = round(4*(LSTM-long) + ET)
    solarTime = paste(floor(solarTime),":",60*(solarTime-floor(solarTime)),sep="")
    solarTime = as.POSIXlt(strptime(solarTime,"%H:%M"))
    localtime = solarTime - diff*60
    localtime = strftime(localtime, format="%H:%M")
    return (localtime)
}

##example of usage
##assign values for local time
#modis$localtime <- ifelse(is.na(modis$viewtime), NA, 
#                          Solar2Local(modis$doy,modis$viewtime,81.28) )
