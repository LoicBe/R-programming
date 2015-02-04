corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        correlation<-c(NULL)
        flag <-FALSE
        for (i in 1:332){
                #find the match filename        
                if(i<10){
                        namefile<-paste(directory,"/","00",i,".csv",sep="")
                }else if (i<100){
                        namefile<-paste(directory,"/","0",i,".csv",sep="")
                }else{
                        namefile<-paste(directory,"/",i,".csv",sep="") 
                }
                
                airQuality<-read.table(namefile,sep=",",header=TRUE)
                #Only no NA value are check
                good<-complete.cases(airQuality)
                airQualityComplete<-airQuality[good,]
                
                if (dim(airQualityComplete)[1]>threshold){
                        sulfate<-c(airQualityComplete[,"sulfate"])
                        nitrate<-c(airQualityComplete[,"nitrate"])
                        flag<-TRUE
                        correlation<-c(correlation,round(cor(sulfate,nitrate),5))
                }
        }
        
        if (flag==TRUE){
                response<-correlation
        }else{
                response<-c(NULL)
        }
        
        return(response)
        
}

#source("corr.R")
#source("complete.R")
cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)
length(cr)