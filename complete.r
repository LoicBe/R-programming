complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        #each monitor are going to be open
        result <- data.frame(id=NULL,nods=NULL)
        for (i in id){
                
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

                #We add a value at our data.frame
                goodData<-c(i,dim(airQualityComplete)[1])
                result<-rbind(result,goodData)
        }
        
        #We give name at each data.frame Columns
        colnames(result)<-c("id","nods")
        return(result)
}

complete("specdata",23:25)