
##Classification optimizer: which probability can be used as a cut-off to divide two binary categories?
    ##a function where probability is a vector of predicted probabilties (0-1), 
    ##for example from a glm or lm; probability is created by 
    ##probability<-prediction(model, type="response", newdata=newdata)
    ##observed is the observed data (ie. glm(observed~some vars, data=data)), and 
    ##event is criteria for converting observed to binary, if observed is 
    ##greater than event, than the outcome happened
    ##This function trys all probability cut offs and returns a dataframe 
    ##with sensitivity and specificity and plots roc curve.
    
cut.off<-function(observed,event, probability){
    x<-seq(from=0, to=1,by= 0.01)
    observed<-ifelse(observed>=event, "event","no event")
    out<-data.frame(cutoff=rep(999, length(x)), accuracy=rep(999, length(x)),
                    sensitivity = rep(999, length(x)), specificity=rep(999, length(x)))
    for (i in 1:length(x)){
        test<-ifelse(probability> x[i],"event","no event")
        t<-table(factor(test,c("event", "no event")), factor(observed,c("event","no event")))
        c<-confusionMatrix(t, positive="event")
        out$cutoff[i]<-x[i]
        out$accuracy[i]<-c$overall[1]
        out$sensitivity[i]<-c$byClass[1]
        out$specificity[i]<- c$byClass[2]
    }
    plot((1-out$specificity),out$sensitivity,
         col=rainbow(n=length(x)), pch=19, cex=0.7)
    lines((1-out$specificity),out$sensitivity,
          lwd=0.5)
    y<-seq(from=1, to=length(out$specificity), by=10)
    text(1-(out$specificity[y]-0.02),out$sensitivity[y]-0.02, labels=round(out$cutoff[y],2), cex=0.3)
    abline(0,1, lwd=3, xlim=c(0,1), ylim=c(0,1))
    a<-round(pROC::auc( observed, probability),2)
    mtext(paste0("auc = ",a), side=1, line=-1.5,adj=0.75)
    legend("bottomright", "cut-off", pch=19, col="red", bty="n")
    return(out)
    return(auc)
}
