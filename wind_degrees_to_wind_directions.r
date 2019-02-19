###convert numeric wind degrees to degree categories (SW, etc)

degtocompass<-function(num){ 
    val= ceiling( (num -22.5 ) / 45 )
    val<-ifelse(val>7,val==1,val)
    arr=c("N","NE","E", "SE", 
          "S","SW","W","NW")
    return( arr[ abs(val)+1 ] )
}

##example
#x<-0:365
#winddir<-rep("A", length(x))
#for (i in 1:length(winddir)){
#    winddir[i]<-degtocompass(x[i])}
