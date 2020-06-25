location<-read.csv('location.csv')
location

processLocation <- function(raw_location){
    library(geosphere)
    return(data.frame(distm(raw_location[,c('Longitude','Latitude')],fun = distVincentyEllipsoid)))
}

processedLocation<-processLocation(location)
processedLocation

locationMarker<-data.frame(names(processedLocation),location[,'Location.Address'])
names(locationMarker)<-c('marker', 'Location.Address')
locationMarker

patient<-read.csv('patient.csv')
patientM<-merge(patient, locationMarker, by.x='Location', by.y='Location.Address')
patientM

human<-read.csv('human2.csv')
humanM<-merge(human, locationMarker, by.x='Location', by.y='Location.Address')
humanM

generateLinkMatrix<-function(locations, Data, type){
    n_location = length(unique(locations[,'marker']))
    if (toupper(type) == "H"){
        n_dat = length(unique(Data[,'Human']))
    }else{
        n_dat = length(unique(Data[,'Patient']))
    }
    dm = matrix(ncol=n_dat, nrow=n_location)
    for (i in 1:nrow(Data)) {
        if (toupper(type) == "H"){
            pat = as.numeric(unlist(strsplit(as.character(Data[i,'Human']),'Human'))[[2]])
        }else{
            pat = as.numeric(unlist(strsplit(as.character(Data[i,'Patient']),'Patient'))[[2]])
        }
        loc = as.numeric(unlist(strsplit(as.character(Data[i,'marker']),'X'))[[2]])
        dm[loc,pat] = 1
    }
    result = as.data.frame(dm)
    result[is.na(result)] = 0
    row.names(result) <- c(paste("L",1:nrow(result), sep=''))
    names(result) <- c(paste(type,1:length(names(result)), sep=''))
    return(result)
}

lnkMtrx <- generateLinkMatrix(locationMarker, patientM, "P")
lnkMtrx

generateFL<-function(locations, patientData, type, vector){
    if (toupper(type)=='P'){
        ty = 'Patient'
    }else{
        ty = 'Human'
    }
    n_location = length(unique(locations[,'marker']))
    n_patient = length(unique(patientData[,ty]))
    dm = matrix(ncol=n_patient, nrow=n_location)
    for (i in 1:nrow(patientData)) {
        pat = as.numeric(unlist(strsplit(as.character(patientData[i,ty]),ty))[[2]])
        loc = as.numeric(unlist(strsplit(as.character(patientData[i,'marker']),'X'))[[2]])
        dm[loc,pat] = as.numeric(patientData[i,'Visit_Frequency'])
    }
    result = as.data.frame(dm)
    result[is.na(result)] = 0
    if(vector == TRUE){
        FLVector = rowSums(result)
        return(FLVector)
    }else{
        row.names(result) <- c(paste("L",1:nrow(result), sep=''))
        names(result) <- c(paste(toupper(type),1:length(names(result)), sep=''))
        return(result)
    }
}

FL <- generateFL(locationMarker, patientM, 'p', TRUE)
FL

generateDP <- function(locations, patientData){
    n_location = length(unique(locations[,'marker']))
    dm = matrix(ncol=1, nrow=n_location, data=0)
    for (i in 1:nrow(patientData)) {
        loc = as.numeric(unlist(strsplit(as.character(patientData[i,'marker']),'X'))[[2]])
        dm[loc,1] = dm[loc,1] + patientData[i, 'Visit_Duration.s.']
    }
    return(as.vector(dm))
}

DP <- generateDP(locationMarker, patientM)
DP

generateSL <- function(locations, humanData){
    n_location = length(unique(locations[,'marker']))
    dm = matrix(ncol=1, nrow=n_location, data=0)
    for (i in 1:nrow(humanData)) {
        loc = as.numeric(unlist(strsplit(as.character(humanData[i,'marker']),'X'))[[2]])
        dm[loc,1] = dm[loc,1] + 1
    }
    return(as.vector(dm))
}

SL <- generateSL(locationMarker, humanM)
SL

generateLocationNodeParameters <- function(locations, FL, DP, SL){
    temp = FL + DP + SL 
    locations['nodeParameter'] <- temp
    return(locations)
}

generateLocationNodeParameters(locationMarker, FL, DP, SL)

generateDU <- function (locations, humanData){
    n_location = length(unique(locations[,'marker']))
    n_humans = length(unique(humanData[,'Human']))
    dm = matrix(ncol=n_location, nrow=n_humans, data=0)
    for (i in 1:nrow(humanData)) {
        loc = as.numeric(unlist(strsplit(as.character(humanData[i,'marker']),'X'))[[2]])
        hum = as.numeric(unlist(strsplit(as.character(humanData[i,'Human']),'Human'))[[2]])
        dm[hum,loc] = dm[hum,loc] + humanData[i, 'Visit_Duration.s.']
    }
    res <- as.data.frame(dm)
    row.names(res) <- c(paste("H",1:nrow(res), sep=''))
    names(res) <- c(paste("L",1:length(names(res)), sep=''))
    return(res)
}

DU <- generateDU(locationMarker, humanM)
DU

lnkMtrxH <- generateLinkMatrix(locationMarker, humanM, "H")
lnkMtrxH

FH <- generateFL(locationMarker, humanM, 'h', TRUE)
FH

FHM <- generateFL(locationMarker, humanM, 'h', FALSE)
FHM

generateVp <- function(locations, humanData){
    n_humans = length(unique(humanData[,'Human']))
    n_location = length(unique(locations[,'marker']))
    dm = matrix(ncol=n_location, nrow=n_humans, data=0)
    for (i in 1:nrow(humanData)) {
        vac = humanData[i, 'Vaccination']
        if(vac>=2){
            vacResult=0
        }else if (vac>0){
            vacResult=1
        }else{
            vacResult=2
        }
        hum = as.numeric(unlist(strsplit(as.character(humanData[i,"Human"]),"Human"))[[2]])
        loc = as.numeric(unlist(strsplit(as.character(humanData[i,'marker']),'X'))[[2]])
        dm[hum, loc] = vacResult
    }
    result <- as.data.frame(dm)
    row.names(result) <- c(paste("H",1:nrow(result), sep=''))
    names(result) <- c(paste("L",1:length(names(result)), sep=''))
    return(result)
}

Vp <- generateVp(locationMarker, humanM)
Vp

generateAs <- function(locations, humanData){
    n_humans = length(unique(humanData[,'Human']))
    n_location = length(unique(locations[,'marker']))
    dm = matrix(ncol=n_location, nrow=n_humans, data=0)
    for (i in 1:nrow(humanData)) {
        sus = humanData[i, 'Age']
        if(sus>=6 && sus <= 20){
            susResult=0
        }else{
            susResult=1
        }
        hum = as.numeric(unlist(strsplit(as.character(humanData[i,"Human"]),"Human"))[[2]])
        loc = as.numeric(unlist(strsplit(as.character(humanData[i,'marker']),'X'))[[2]])
        dm[hum, loc] = susResult
    }
    result <- as.data.frame(dm)
    row.names(result) <- c(paste("H",1:nrow(result), sep=''))
    names(result) <- c(paste("L",1:length(names(result)), sep=''))
    return(result)
}

As <- generateAs(locationMarker, humanM)
As

generatePr <- function(locations, humanData){
    n_humans = length(unique(humanData[,'Human']))
    n_location = length(unique(locations[,'marker']))
    dm = matrix(ncol=n_location, nrow=n_humans, data=0)
    for (i in 1:nrow(humanData)) {
        sus = humanData[i, 'Pregnancy']
        hum = as.numeric(unlist(strsplit(as.character(humanData[i,"Human"]),"Human"))[[2]])
        loc = as.numeric(unlist(strsplit(as.character(humanData[i,'marker']),'X'))[[2]])
        dm[hum, loc] = sus
    }
    result <- as.data.frame(dm)
    row.names(result) <- c(paste("H",1:nrow(result), sep=''))
    names(result) <- c(paste("L",1:length(names(result)), sep=''))
    return(result)
}

Pr <- generatePr(locationMarker, humanM)
Pr

generateLinkWeight <- function(locationParameters, humanParameters){
    
}
