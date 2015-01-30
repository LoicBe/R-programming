pollutantmean<-function(directory="specdata",pollutant="sulfate",id=100:101) {
        #ouverture d'un fichier CSV
        sum<-c(0)
        nbr<-c(0)
        for (i in id){
                #on définit le nom du fichier
                chemin<-cheminPollutant(directory,i)
                dataMonitor<-read.table(chemin,header=TRUE,sep=",")
                #on récupère les données du polluant sans les NA
                datapollutant<-dataMonitor[,pollutant]
                datapollutantlessNA<-datapollutant[!is.na(datapollutant)]
                
                #on prend la somme et le numbre de données
                
                nbr<-nbr+c(dim(array(datapollutantlessNA)))
                sum<-sum+c(sum(datapollutantlessNA))
        }
        return(sum/nbr)
}

cheminPollutant<-function(directory,i){
        if (i<10){
                namefile <- paste("00",as.character(i),sep="")
        }else if (i<100){
                namefile <- paste("0",as.character(i),sep="")
        }else{
                namefile <- as.character(i)   
        }
        chemin <-paste(directory,"/",namefile,".csv",sep="")
        return(chemin)
}
