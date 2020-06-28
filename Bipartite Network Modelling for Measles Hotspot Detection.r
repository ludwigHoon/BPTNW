location<-read.csv('location.csv')
names(location)<-c('Location.Address', 'Latitude', 'Longitude', 'Population')
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
names(patient)<-c('Patient', 'Location', 'Visit_Frequency', 'Visit_Duration.s.')
patient
patientM<-merge(patient, locationMarker, by.x='Location', by.y='Location.Address')
patientM

human<-read.csv('human2.csv')
human
names(human)<-c('Human', 'Location', 'Visit_Frequency', 'Visit_Duration.s.', 'Vaccination', 'Age', 'Pregnancy')
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
class(DP)

generateSL <- function(locationMarker, locationData){
    temp <- merge(locationMarker, locationData, by.x='Location.Address', by.y='Location.Address')
    vals <- as.numeric(gsub("X","", temp$marker))
    temp<-temp[order(vals),]
    return(as.vector(temp$Population))
}

SL <- generateSL(locationMarker, location)
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

FHM <- t(generateFL(locationMarker, humanM, 'h', FALSE))
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

locParameters <- c()
locParameters$n_loc <- length(unique(locationMarker$marker))
locParameters$Fl <- FL
locParameters$Dp <- DP
locParameters$Sl <- SL
locParameters

humParameters <- c()
humParameters$n_hum <- length(unique(humanM$Human))
humParameters$Fh <- FHM
humParameters$Du <- DU
humParameters$V <- Vp
humParameters$As <- As
humParameters$P <- Pr
humParameters

generateLinkWeight <- function(locationParameters, humanParameters, linkMatrix){
    n_location = locationParameters$n_loc
    n_human = humanParameters$n_hum
    for (c in names(locationParameters)){
        if (c!= 'n_loc'){
            locationParameters[[`c`]] <- round(((locationParameters[[`c`]]/max(locationParameters[[`c`]]))*0.9),digits = 4)
        }
    }
    for (c in names(humanParameters)){
        if (c!= 'n_hum'){
            humanParameters[[`c`]] <- round(((humanParameters[[`c`]]/max(humanParameters[[`c`]]))*0.9),digits = 4)
        }
    }
    dm = matrix(nrow = n_location, ncol = n_human, data = 0)
    for (i in 1:n_location){
        locationSum = locationParameters$Fl[i] + locationParameters$Dp[i] + locationParameters$Sl[i]
        for (j in 1:n_human){
            humanSum = humanParameters$Fh[j,i] + humanParameters$Du[j,i] + humanParameters$V[j,i] + humanParameters$As[j,i] + humanParameters$P[j,i]
            dm[i,j] = locationSum + humanSum
        }
    }
    result <- as.data.frame(dm)
    row.names(result) <- c(paste("L",1:nrow(result), sep=''))
    names(result) <- c(paste("H",1:length(names(result)), sep=''))
    return(result*linkMatrix)
}

linkWeight <- generateLinkWeight(locParameters, humParameters, lnkMtrxH)
linkWeight

Hub = as.matrix(linkWeight) %*% t(linkWeight)
Hub

Authority = t(linkWeight)%*%as.matrix(linkWeight)
Authority

is_square_matrix <- function(A){
    return(nrow(A)==ncol(A))
}

eucNorm <- function(A){
    A <- matrix(A, ncol=1)
    return(sqrt(colSums(A^2)))
}

powerMethod <- function(matrix, startVector = NULL, threshold = 1e-6, maxIteration = 100){
    if (!is_square_matrix(matrix)){
        stop("'powerMethod()' requires a square numeric matrix")
    }
    if (is.null(startVector)){
        startVector = rep(1, nrow(matrix))
    }
    cachedVector = startVector
    steps = 1
    vectors <- list(startVector)
    while(TRUE){
        v_new = matrix %*% cachedVector
        # 4.21
        v_new = v_new/eucNorm(v_new)
        if (eucNorm(abs(v_new) - abs(cachedVector)) <= threshold){
            break}
        cachedVector = v_new
        steps = steps + 1
        vectors[[steps]] <- c(v_new)
        if (steps == maxIteration){
            break}
    }
    lambda = sum((matrix %*% v_new) * v_new)
    res <- list(iter = steps, vector = v_new, value = lambda)
    vectors <- do.call(cbind, vectors)
    colnames(vectors) <- paste0("v", 1:ncol(vectors))
    res <- c(vector_iterations=list(vectors), res)
    return(res)
}

k = 1000
auth <- c(rep(1,nrow(Authority))) 
hub <- c(rep(1,nrow(Hub))) 
ip <- powerMethod(Hub,hub,10^-2,k)
ih <- powerMethod(Authority,auth,10^-2,k)

vp <- ip$vector/max(ip$vector) 
vh <- ih$vector/max(ih$vector) 

as.data.frame(vh[order(vh, decreasing = TRUE),])
as.data.frame(vp[order(vp, decreasing = TRUE),])
