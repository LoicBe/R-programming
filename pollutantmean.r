pollutantmean<-function(directory, pollutant, id = 1:332){
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        #initialisation
        sum <- 0
        nbr <- 0
        
        #each monitor are going to be open
        for (i in id){

                #find the match filename        
                if(i<10){
                        namefile<-paste(directory,"00",i,".csv",sep="")
                }else if (i<100){
                        namefile<-paste(directory,"0",i,".csv",sep="")
                }else{
                        namefile<-paste(directory,i,".csv",sep="") 
                }
                
                airQuality<-read.table(namefile,sep=",",header=TRUE)

                #Only no NA data of the polluant are track
                pollutantData<-airQuality[,pollutant]
                pollutantDataSNA<-pollutantData[!is.na(pollutantData)]
                
                #record of the sum and the number of data
                sum <- sum + sum(pollutantDataSNA)
                nbr <- nbr + dim(array(pollutantDataSNA))

        }
        
        return(round(sum/nbr,3))
        
}
