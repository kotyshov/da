best <- function(state, outcome) {
    data1 <- read.csv ("outcome-of-care-measures.csv",colClasses="character")
    if (any(data1$State==state) == FALSE) {
       stop ("invalid state")
    }
    data2 <- subset (data1, data1$State==state)
    if (outcome=="heart failure") {
        data2 <- subset(data2,data2[,17]!="Not Available")
        data2[,17] <- as.numeric(data2[,17])
        data3 <- subset(data2,data2[,17]==min(data2[,17],na.rm=T))
        return (min(data3[,2]))
    } else if (outcome=="heart attack") {
        data2 <- subset(data2,data2[,11]!="Not Available")
        data2[,11] <- as.numeric(data2[,11])
        data3 <- subset(data2,data2[,11]==min(data2[,11],na.rm=T))
        return (min(data3[,2]))        
    } else if (outcome=="pneumonia") {
        data2 <- subset(data2,data2[,23]!="Not Available")
        data2[,23] <- as.numeric(data2[,23])
        data3 <- subset(data2,data2[,23]==min(data2[,23],na.rm=T))
        return(min(data3[,2]))
    } else {
        stop("invalid outcome")
    }
}
best("TX","heart failure")
