library("dplyr")
library("zoo")
library("xts")
library("xlsx")
library("lubridate")

#       grab the metadata (this is currently unused, the metadata is also saved in the working dir.)
TMYlocationfile <- "http://rredc.nrel.gov/solar/old_data/nsrdb/1991-2005/tmy3/TMY3_StationsMeta.csv"
location_table <- read.csv(TMYlocationfile, header=TRUE)
location_table$location <- paste(location_table$State,location_table$Site.Name)

#       provide location code for desired station, copy weather file for said station into working dir.
location_USAF <- readline("please input a station USAF code ")
file <- paste("http://rredc.nrel.gov/solar/old_data/nsrdb/1991-2005/data/tmy3/",location_USAF,"TYA.CSV",sep="")

#       open weather file, make DF of desired columns, convert date time stamp to POSIX 
wtable <- read.csv(file, header = TRUE, skip = 1)
cols <- c("Date..MM.DD.YYYY.","Time..HH.MM.","Dry.bulb..C.","Dew.point..C.","RHum....")
T.dat <- wtable[cols]
T.dat$Date <- as.POSIXct(T.dat$Date..MM.DD.YYYY., format = "%m/%d/%Y")
T.dat$Time <- as.POSIXct(T.dat$Time..HH.MM., format = "%H:%M")

#       Celcius to Fahrenheit temps function
ctof <- function(var){9/5*var+32}

#       make Fahrenheit temps columns, re-build date time stamp to be 1 contiguous year (2000)
T.dat$DB_Deg.F <- sapply(T.dat$Dry.bulb..C.,ctof)
T.dat$Dew.point_Deg.F <- sapply(T.dat$Dew.point..C.,ctof)
T.dat$month <- as.numeric(strftime(T.dat$Date,"%m"))
T.dat$day <- as.numeric(strftime(T.dat$Date, "%d"))
T.dat$hour <- as.numeric(strftime(T.dat$Time, "%H"))
T.dat$Date <- ISOdatetime(2000,T.dat$month,T.dat$day,T.dat$hour,0,0, tz = "")

#       wetbulb temp calc function
#       source = http://journals.ametsoc.org/doi/pdf/10.1175/JAMC-D-11-0143.1 
#       mean abs. error = 0.28 deg C
empiricalWB <- function(DB,Rh){DB*atan(0.151977*(Rh+8.313659)^.5)+atan(DB+Rh)-atan(Rh-1.676331)+0.00391838*(Rh)^(3/2)*atan(0.023101*Rh)-4.686035}

#       make wet bulb columns C and F
T.dat$WB_Deg.C <- mapply(empiricalWB,T.dat$Dry.bulb..C.,T.dat$RHum....)
T.dat$WB_Deg.F <- sapply(T.dat$WB_Deg.C,ctof)

#       read in schedule, prepare weather file for schedule application
bs <- read.csv("./Schedule.csv", header = TRUE, sep = ",", dec = ".",stringsAsFactors=FALSE)
bs$Start.Date <- as.POSIXct(bs$Start.Date,format="%m/%d/%Y")
bs$End.Date <- as.POSIXct(bs$End.Date,format="%m/%d/%Y")
T.dat <- cbind(T.dat,list(Occ=NA,Schedule=NA))
str(bs)
#       schedule function
apply.schedule <- function(data,sched) {       
        data$Day <- strftime(data$Date,'%A')
        dn <- strftime(seq(1,7,1)*60*60*24 + Sys.time(),'%A')
        for(i in dn) {
                for(j in seq(1,length(index(bs)))) {
                        
                        start_hour <- bs[paste(i,".Start",sep="")][j,]
                        end_hour   <- bs[paste(i,".End",sep="")][j,]
                        start_date <- bs$Start.Date[j]
                        end_date   <- bs$End.Date[j] + 24 * 60 * 59 
                        window     <- bs[paste(i,".Window",sep="")][j,]
                        name 	   <- as.character(bs["Schedule.Name"][j,])
                        
                        data$Schedule[with(data,Date >= start_date & Date <= end_date)] <- name 
                        if(window == 1) {
                                data$Occ[with(data,Date >= start_date & Date <= end_date & Day == i & hour >= start_hour & hour < end_hour)] <- 1 
                        } else if(window == 0) {
                                if (start_hour == 0) {
                                        data$Occ[with(data,Date >= start_date & Date <= end_date & Day == i & hour < start_hour)] <- 1 
                                } else {
                                        data$Occ[with(data,Date >= start_date & Date <= end_date & Day == i & hour <= start_hour)] <- 1 
                                }
                                data$Occ[with(data,Date >= start_date & Date <= end_date & Day == i & hour > end_hour)] <- 1 
                        } #if
                } #for j
        } # for i
        
        data$Occ[is.na(data$Occ)] <- 0
        data$Occ <- as.factor(data$Occ)
        data$Schedule <- as.factor(data$Schedule)
        data$Day <- as.factor(data$Day)
        
        return(data) # return
}

#       apply schedule to data
T.dat <- apply.schedule(T.dat,bs)

#       bin data
bin_size <- readline("please specify a bin size as an integer number ")
bin_size <- as.numeric(unlist(strsplit(bin_size,",")))
range <- seq(from = 120, to = -20,by = -bin_size)
counter = (max(range)-min(range))/bin_size+1
binsum <- matrix(ncol = 5, nrow = counter)
binsum[,1] <- range
binname <- c("bin","Occ hours","Unocc hours","avg.DB","avg.WB")
for(i in 1:counter){
        binsum[i,2] <- sum(with(T.dat,DB_Deg.F <= range[i] & DB_Deg.F > (range[i]-bin_size) & Occ == 1 ))
        binsum[i,3] <- sum(with(T.dat,DB_Deg.F <= range[i] & DB_Deg.F > (range[i]-bin_size) & Occ == 0 ))
        DB <- filter(T.dat,DB_Deg.F < range[i] & DB_Deg.F >= range[i]-bin_size)
        DB <- (DB$DB_Deg.F)
        binsum[i,4] <- mean(as.numeric(DB))
        WB <- filter(T.dat,WB_Deg.F < range[i] & DB_Deg.F >= range[i]-bin_size)
        WB <- (WB$WB_Deg.F)
        binsum[i,5] <- mean(as.numeric(WB))
}
binsum <- data.frame(binsum)
names(binsum) <- binname

occdat <- filter(T.dat, Occ == 1)
dailymeanT <- aggregate(occdat$DB_Deg.F, by = list(yday(occdat$Date)), mean)
names(dailymeanT) <- c("day","mean_temp")
dailymeanT$date <- as.Date(unique(occdat$Date..MM.DD.YYYY.),format = "%m/%d/%Y") 
dailymeanT$month <- strftime(dailymeanT$date, "%m")
dailymeanT$cp <- 65
dailymeanT$dd <- dailymeanT$cp-dailymeanT$mean_temp

occhdd <- filter(dailymeanT, dd > 0)
occcdd <- filter(dailymeanT, dd < 0)
occcdd$dd <- -occcdd$dd

degsum <- data.frame(matrix(nrow = 12, ncol = 5))
names(degsum) <- c("month", "Occ_HDD", "Occ_CDD", "Unocc_HDD", "Unocc_CDD")
degsum$month <- unique(dailymeanT$month)
degsum[is.na(degsum)] <-  0

occ_HDD <- aggregate(occhdd$dd, by = list(occhdd$month), sum)
names(occ_HDD) <- c("month","Occ_HDD")
occ_CDD <- aggregate(occcdd$dd, by = list(occcdd$month), sum)
names(occ_CDD) <- c("month","Occ_CDD")
occ_deg <- merge(occ_HDD, occ_CDD, all = TRUE)

unoccdat <- filter(T.dat, Occ == 0)
dailymeanT <- aggregate(unoccdat$DB_Deg.F, by = list(yday(unoccdat$Date)), mean)
names(dailymeanT) <- c("day","mean_temp")
dailymeanT$date <- as.Date(unique(unoccdat$Date..MM.DD.YYYY.),format = "%m/%d/%Y") 
dailymeanT$month <- strftime(dailymeanT$date, "%m")
dailymeanT$cp <- 65
dailymeanT$dd <- dailymeanT$cp-dailymeanT$mean_temp

unocchdd <- filter(dailymeanT, dd > 0)
unocccdd <- filter(dailymeanT, dd < 0)
unocccdd$dd <- -unocccdd$dd

unocc_HDD <- aggregate(unocchdd$dd, by = list(unocchdd$month), sum)
names(unocc_HDD) <- c("month","Unocc_HDD")
unocc_CDD <- aggregate(unocccdd$dd, by = list(unocccdd$month), sum)
names(unocc_CDD) <- c("month","Unocc_CDD")
unocc_deg <- merge(unocc_HDD, unocc_CDD, all = TRUE)

degsum$Occ_HDD <- occ_deg$Occ_HDD
degsum$Occ_CDD <- occ_deg$Occ_CDD
degsum$Unocc_HDD <- unocc_deg$Unocc_HDD
degsum$Unocc_CDD <- unocc_deg$Unocc_CDD

#       save binned data in useful formats
write.xlsx(binsum, "binsum.xlsx")
write.csv(binsum, "binsum.csv")
