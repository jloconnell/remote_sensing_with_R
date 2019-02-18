###find extrema in a sinusoidal time series
findpeaks <- function(vec,bw=1,x.coo=c(1:length(vec)))
{
    pos.x.max <- NULL
    pos.y.max <- NULL
    pos.x.min <- NULL
    pos.y.min <- NULL
    for(i in 1:(length(vec)-1)){
        if((i+1+bw)>length(vec)){
            sup.stop <- length(vec)}
        else{sup.stop <- i+1+bw}
        if((i-bw)<1){inf.stop <- 1}
        else{inf.stop <- i-bw}
        subset.sup <- vec[(i+1):sup.stop]
        subset.inf <- vec[inf.stop:(i-1)]
        
        is.max  <- sum(subset.inf > vec[i]) == 0
        is.nomin<- sum(subset.sup > vec[i]) == 0
        
        no.max   <- sum(subset.inf > vec[i]) == length(subset.inf)
        no.nomin <- sum(subset.sup > vec[i]) == length(subset.sup)
        
        if(is.max & is.nomin){
            pos.x.max <- c(pos.x.max,x.coo[i])
            pos.y.max <- c(pos.y.max,vec[i])
        }
        if(no.max & no.nomin){
            pos.x.min <- c(pos.x.min,x.coo[i])
            pos.y.min <- c(pos.y.min,vec[i])
        }
    }
    return(list(xmax=pos.x.max,ymax=pos.y.max,xmin=pos.x.min,ymin=pos.y.min))
}

##example of usage (not run)
##create some noisy sinusoidal time series data
#n <- 200 # number of data points
#t <- seq(from=0,to=5.75*3.14,length=n)
#a <- 3
#b <- 2
#err <- rnorm(n, sd=0.5)
#amp <- 2
#y <- a*sin(b*t)+err*amp 
#x<-seq.Date(from=as.Date("2010-01-01"), to=as.Date("2016-01-01"), length.out=n)
#plot(x, y, type="l")
#
##smooth the data and find the peaks in the smooth
#smoothingSpline<-smooth.spline(x, y)
#peaks<-findpeaks(smoothingSpline$y)
#
##which dates are the maxes?
as.Date(smoothingSpline$x[peaks$xmax], origin="1970-01-01")
#
##which dates are the mins?
as.Date(smoothingSpline$x[peaks$xmin], origin="1970-01-01")
#
##get extrema
#peaks<-findpeaks(data$y)
#smoothingSpline$x[peaks$xmax]
##maxes
#as.Date(smoothingSpline$x[peaks$xmax])
##mins
#as.Date(smoothingSpline$x[peaks$xmin])
