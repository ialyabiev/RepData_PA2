fn <- "data/repdata-data-StormData.csv.bz2"
if (!file.exists(fn)) {
    message("Downloading dataset.")
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                  destfile = fn)
}

fnbz <- bzfile(fn,"r") 
data <- read.csv(fnbz, header = TRUE, nrows = 1 ) 
data[,1:4]
close(fnbz )

allNames  <-names(data)
featureNames <- c("EVTYPE","FATALITIES","INJURIES",
                  "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP") 
featureInd <- sapply(featureNames, function(x){which(allNames == x)})

dcols <- rep("NULL", ncol(data))
names(dcols) <- names(data)
dcols[featureInd] <- NA
dcols


fnbz <- bzfile(fn,"r") 
data <- read.csv(fnbz, header = T, colClasses = dcols) 
close(fnbz )
data[1:4,]

#translate damage units
data <- transform(data, EVTYPE = as.factor(toupper(EVTYPE)))
data <- transform(data, PROPDMGEXP = toupper(PROPDMGEXP), CROPDMGEXP = toupper(CROPDMGEXP))
data <- transform(data,  PROPDMGEXP =sapply(PROPDMGEXP, 
                            function(x){if (x %in% c("K","M","B")) x else NA}))
data <- transform(data,  CROPDMGEXP =sapply(CROPDMGEXP, 
                         function(x){if (x %in% c("K","M","B")) x else NA}))
                                                                        
#cross table for people and money
data <-transform(data, FATALITIES = as.numeric(FATALITIES))
tabF <- xtabs(FATALITIES ~ EVTYPE, data = data)
barplot(sort(tabF, decreasing=T)[1:10])
tabI <- xtabs(INJURIES ~ EVTYPE, data = data)
tabI <- sort(tabI, decreasing=T)
tabI[1:20]
m <- data.frame(TYPE_EVENT = names(tabI[1:10]), INJ = tabI[1:10], row.names= 1:10  )
#dimnames(m) <-list(rep("", dim(m)[1]), rep("", dim(m)[2]))
m
barplot(tabI[1:10])

##barplot(height = fatalities$Fatalities[1:20], names.arg = fatalities$Event.Type[1:20], 
##        las = 2, cex.axis = 0.8, cex.names = 0.7, col = rainbow(20, start = 0, end = 0.35), 
##        ylab = "Number of Fatalities")
##title("Top Events \n causing Fatalities", line = -2)

#economy
dnum <- data.frame(K=1e+3,M=1e+6,B=1e+9)
fnum <- function(x){
    if (x %in% c("K","M","B")) dnum[1,x] else 0 }
data <- transform(data,  PROPDMGEXP =sapply(PROPDMGEXP, fnum))
data <- transform(data,  CROPDMGEXP =sapply(CROPDMGEXP, fnum) )
tabP <- data.frame(EVTYPE = data$EVTYPE, PROPDMG = data$PROPDMG*data$PROPDMGEXP)                                            
tabC <- data.frame(EVTYPE = data$EVTYPE, CROPDMG = data$CROPDMG*data$CROPDMGEXP)                                            
tabP <- xtabs(PROPDMG ~ EVTYPE, data = tabP)
tabC <- xtabs(CROPDMG ~ EVTYPE, data = tabC)
tabP <- sort(tabP, decreasing=T)
tabC <- sort(tabC, decreasing=T)

barplot(tabP[1:10])
barplot(tabC[1:10])


#Estimates should be rounded to
#three significant digits, followed by an alphabetical character
#“K” for thousands, “M” for millions, and “B” for billions.
