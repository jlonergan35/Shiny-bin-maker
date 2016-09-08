

library(shiny)
library(rhandsontable)
library(lubridate)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(zoo)
library(xts)
library(xlsx)

sch <- data.frame()
temp <- data.frame()

# CMH - added after splitting app.R into separate ui.R/server.R 
source('ui.R')

server <- function(input,output) {
        
        values = reactiveValues()
        setHot = function(x) values[["hot"]] <- x
        
        output$mySite <- renderUI({
                tags$a(href = "http://rredc.nrel.gov/solar/old_data/nsrdb/1991-2005/tmy3/by_state_and_city.html", "link to station IDs")
        })
        
        output$inst <- renderText("This tool is used to bin TMY 3 data with complex scheduling,
                                  It provides as output DB, WB, occupied hours, un-occupied hours, HDD and CDD.
                                  To use the tool: Select from the meta data link a station ID,
                                  select a bin size, and fill out the schedule (0 hour is 12 am, 
                                  23rd hour is 11 pm) the window indicates an inclusive or 
                                  exclusive schedule, 1 is inclusive, which is typical. For data 
                                  with multiple scheduled periods, use the scheduled periods counter
                                  to specify, there is no limit on the number of scheduled periods.
                                  The TMY 3 data set is averaged over a number of years, however for
                                  simplicity's sake, the year is set to 2000, all date entries should be
                                  between 1/1/2000 and 12/31/2000.To indicate no run hours, set start and 
                                  end to 0 and window to 1,
                                  to indicate 24 hour operation, set start to 0 and end to 24 and window to 1.
                                  Click the run button, data will be displayed as an html table
                                  a line plot, and available for download as a CSV, by clicking the
                                  download button.")
        
        DF <- reactive(data.frame(Schedule.Name = 1:input$n, Start.Date = "1/1/2000",
                                  End.Date = "12/31/2000",
                                  Monday.Start = 0, Monday.End = 0, Monday.Window = as.integer(1),
                                  Tuesday.Start = 0, Tuesday.End = 0, Tuesday.Window = as.integer(1),
                                  Wednesday.Start = 0, Wednesday.End = 0, Wednesday.Window = as.integer(1),
                                  Thursday.Start = 0, Thursday.End = 0, Thursday.Window = as.integer(1),
                                  Friday.Start = 0, Friday.End = 0, Friday.Window = as.integer(1),
                                  Saturday.Start = 0, Saturday.End = 0, Saturday.Window = as.integer(1),
                                  Sunday.Start =0, Sunday.End = 0, Sunday.Window = as.integer(1),
                                  stringsAsFactors = F))
        
        output$hot <- renderRHandsontable({
                
                rhandsontable(DF(), selectCallback = TRUE) %>%
                        hot_validate_numeric(col= list(4:24), min = 0, max = 24) 
        })
        observe({
                if(is.null(input$hot)) {
                        hotdf <-DF()
                        setHot(hotdf)
                } else if(!is.null(input$hot)) { 
                        hotdf = hot_to_r(input$hot)
                        setHot(hotdf)
                }
        })
        
        observeEvent(input$clicks, {
                
                
                
                sch <- values[["hot"]]
                
                #       grab the metadata (this is currently unused, the metadata is also saved in the working dir.)
                TMYlocationfile <- "http://rredc.nrel.gov/solar/old_data/nsrdb/1991-2005/tmy3/TMY3_StationsMeta.csv"
                location_table <- read.csv(TMYlocationfile, header=TRUE)
                location_table$location <- paste(location_table$State,location_table$Site.Name)
                
                #       provide location code for desired station, copy weather file for said station into working dir.
                #location_USAF <- readline("please input a station USAF code "),
                file <- paste("http://rredc.nrel.gov/solar/old_data/nsrdb/1991-2005/data/tmy3/",input$station,"TYA.CSV",sep="")                      #       open weather file, make DF of desired columns, convert date time stamp to POSIX 
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
                T.dat$Date[2186] <- as.POSIXct("4/2/2000", format = "%m/%d/%Y")
                
                #       wetbulb temp calc function
                #       source = http://journals.ametsoc.org/doi/pdf/10.1175/JAMC-D-11-0143.1 
                #       mean abs. error = 0.28 deg C
                empiricalWB <- function(DB,Rh){DB*atan(0.151977*(Rh+8.313659)^.5)+atan(DB+Rh)-atan(Rh-1.676331)+0.00391838*(Rh)^(3/2)*atan(0.023101*Rh)-4.686035}
                
                #       make wet bulb columns C and F
                T.dat$WB_Deg.C <- mapply(empiricalWB,T.dat$Dry.bulb..C.,T.dat$RHum....)
                T.dat$WB_Deg.F <- sapply(T.dat$WB_Deg.C,ctof)
                
                #       read in schedule, prepare weather file for schedule application
                bs <- sch
                #bs <- read.csv(fname, header = TRUE, sep = ",", dec = ".",stringsAsFactors=FALSE)
                bs$Start.Date <- as.POSIXct(bs$Start.Date,format="%m/%d/%Y")
                bs$End.Date <- as.POSIXct(bs$End.Date,format="%m/%d/%Y")
                T.dat <- cbind(T.dat,list(Occ=NA,Schedule=NA))
                
                #       schedule function
                apply.schedule <- function(dat,sch) {       
                        dat$Day <- strftime(dat$Date,'%A')
                        dn <- strftime(seq(1,7,1)*60*60*24 + Sys.time(),'%A')
                        for(i in dn) {
                                for(j in seq(1,length(index(sch)))) {
                                        
                                        start_hour <- sch[paste(i,".Start",sep="")][j,]
                                        end_hour   <- sch[paste(i,".End",sep="")][j,]
                                        start_date <- sch$Start.Date[j]
                                        end_date   <- sch$End.Date[j] + 24 * 60 * 59
                                        window     <- sch[paste(i,".Window",sep="")][j,]
                                        name 	   <- as.integer(sch["Schedule.Name"][j,])
                                        
                                        dat$Schedule[with(dat,Date >= start_date & Date <= end_date)] <- name 
                                        if(window == 1) {
                                                dat$Occ[with(dat,Date >= start_date & Date <= end_date & Day == i & hour >= start_hour & hour < end_hour)] <- 1 
                                        } else if(window == 0) {
                                                if (start_hour == 0) {
                                                        dat$Occ[with(dat,Date >= start_date & Date <= end_date & Day == i & hour < start_hour)] <- 1 
                                                } else {
                                                        dat$Occ[with(dat,Date >= start_date & Date <= end_date & Day == i & hour <= start_hour)] <- 1 
                                                }
                                                dat$Occ[with(dat,Date >= start_date & Date <= end_date & Day == i & hour > end_hour)] <- 1 
                                        } #if
                                } #for j
                        } # for i
                        
                        dat$Occ[is.na(dat$Occ)] <- 0
                        dat$Occ <- as.factor(dat$Occ)
                        dat$Schedule <- as.factor(dat$Schedule)
                        dat$Day <- as.factor(dat$Day)
                        return(dat) # return
                }
                
                #       apply schedule to data
                T.dat <- apply.schedule(T.dat,bs)
                
                #       bin data
                #bin_size <- readline("please specify a bin size as an integer number "),
                #input$bin_size <- as.numeric(unlist(strsplit(input$bin_size,",")))
                range <- seq(from = 120, to = -20,by = -input$bin_size)
                counter = (max(range)-min(range))/input$bin_size+1
                binsum <- matrix(ncol = 6, nrow = counter)
                binsum[,1] <- range
                binname <- c("bin","Occ_hours","Unocc_hours","Total_hours","avg.DB","avg.WB")
                for(i in 1:counter){
                        binsum[i,2] <- sum(with(T.dat,DB_Deg.F <= range[i] & DB_Deg.F > (range[i]-input$bin_size) & Occ == 1 ))
                        binsum[i,3] <- sum(with(T.dat,DB_Deg.F <= range[i] & DB_Deg.F > (range[i]-input$bin_size) & Occ == 0 ))
                        binsum[i,4] <- sum(with(T.dat,DB_Deg.F <= range[i] & DB_Deg.F > (range[i]-input$bin_size)))
                        DB <- filter(T.dat,DB_Deg.F < range[i] & DB_Deg.F >= range[i]-input$bin_size)
                        DB <- (DB$DB_Deg.F)
                        binsum[i,5] <- mean(as.numeric(DB))
                        WB <- filter(T.dat,WB_Deg.F < range[i] & DB_Deg.F >= range[i]-input$bin_size)
                        WB <- (WB$WB_Deg.F)
                        binsum[i,6] <- mean(as.numeric(WB))
                }
                
                binsum <- data.frame(binsum)
                names(binsum) <- binname
                binsum[is.na(binsum)] <-  0
                
                #create summary tables of degree days by schedule
                degsum <- data.frame(matrix(nrow = 12))#,ncol = 5))
                names(degsum) <- c("month")#, "Occ_HDD", "Occ_CDD", "Unocc_HDD", "Unocc_CDD")
                degsum$month <- unique(T.dat$month)
                
                dd <- data.frame(matrix(nrow = 365))
                names(dd) <- c("date")
                dd$date <- as.Date(unique(T.dat$Date..MM.DD.YYYY.), format = "%m/%d/%Y")
                
                occdat <- filter(T.dat, Occ == 1)
                if(length(index(occdat)) > 0){
                        dailymeanT <- aggregate(occdat$DB_Deg.F, by = list(yday(occdat$Date)), mean)
                        names(dailymeanT) <- c("day","mean_temp")
                        dailymeanT$date <- as.Date(unique(occdat$Date..MM.DD.YYYY.),format = "%m/%d/%Y") 
                        dailymeanT$month <- as.numeric(strftime(dailymeanT$date, "%m"))
                        dailymeanT$cp <- input$cp
                        dailymeanT$dd <- dailymeanT$cp-dailymeanT$mean_temp
                        #head(dailymeanT)
                        
                        occhdd <- filter(dailymeanT, dd > 0)
                        occcdd <- filter(dailymeanT, dd < 0)
                        occcdd$dd <- -occcdd$dd
                        occdd <- merge(occhdd,occcdd, by = "date", all = TRUE)
                        occdd <- occdd[,c(1,6,11)]
                        names(occdd) <- c("date","occ_hdd","occ_cdd")
                        
                        if(length(index(occhdd)) > 0){
                                occ_HDD <- aggregate(occhdd$dd, by = list(occhdd$month), sum)
                        }
                        names(occ_HDD) <- c("month","Occ_HDD")
                        if(length(index(occcdd)) > 0){
                                occ_CDD <- aggregate(occcdd$dd, by = list(occcdd$month), sum)
                        }
                        names(occ_CDD) <- c("month","Occ_CDD")
                        occ_deg <- merge(occ_HDD, occ_CDD, by = "month", all = TRUE)
                        #print(occ_deg)
                        
                        degsum  <- merge(degsum,occ_deg, by = "month", all.x = T)
                        #print(degsum)
                        #degsum$Occ_HDD <- occ_deg$Occ_HDD
                        #degsum$Occ_CDD <- occ_deg$Occ_CDD
                }
                unoccdat <- filter(T.dat, Occ == 0)
                if(length(index(unoccdat)) > 0){
                        dailymeanT <- aggregate(unoccdat$DB_Deg.F, by = list(yday(unoccdat$Date)), mean)
                        names(dailymeanT) <- c("day","mean_temp")
                        dailymeanT$date <- as.Date(unique(unoccdat$Date..MM.DD.YYYY.),format = "%m/%d/%Y") 
                        
                        #fixed date bug, now doesn't read any values beyond this point
                        # need to trouble shoot next block with print statements!
                        
                        dailymeanT$month <- as.numeric(strftime(dailymeanT$date, "%m"))
                        dailymeanT$cp <- input$cp
                        dailymeanT$dd <- dailymeanT$cp-dailymeanT$mean_temp
                        
                        unocchdd <- filter(dailymeanT, dd > 0)
                        unocccdd <- filter(dailymeanT, dd < 0)
                        unocccdd$dd <- -unocccdd$dd
                        unoccdd <- merge(unocchdd,unocccdd, by = "date", all = TRUE)
                        unoccdd <- unoccdd[,c(1,6,11)]
                        names(unoccdd) <- c("date","unocc_hdd","unocc_cdd")
                        
                        if(length(index(unocchdd)) > 0){
                                unocc_HDD <- aggregate(unocchdd$dd, by = list(unocchdd$month), sum)
                                names(unocc_HDD) <- c("month","Unocc_HDD")
                        }
                        
                        if(length(index(unocccdd)) > 0){
                                unocc_CDD <- aggregate(unocccdd$dd, by = list(unocccdd$month), sum)
                                names(unocc_CDD) <- c("month","Unocc_CDD")
                        }
                        if(length(index(unocchdd)) > 0 & length(index(unocccdd)) > 0){
                                unocc_deg <- merge(unocc_HDD, unocc_CDD, by = "month", all = TRUE)
                        } else if(length(index(unocchdd)) <= 0 ){
                                unocc_deg <- unocc_CDD
                        } else if(length(index(unocccdd)) <= 0){
                                unocc_deg <- unocc_HDD
                        }
                        degsum  <- merge(degsum,unocc_deg, by = "month", all.x = T)
                        #degsum$Unocc_HDD <- unocc_deg$Unocc_HDD
                        #degsum$Unocc_CDD <- unocc_deg$Unocc_CDD
                }
                
                if(exists("occdd") == TRUE & exists("unoccdd") == TRUE){
                        dd <- merge(dd,occdd, by = "date", all.x = TRUE)
                        dd <- merge(dd,unoccdd, by = "date", all.x = TRUE)
                } else if(exists("unoccdd") == FALSE){
                        dd <- occdd
                } else if(exists("occdd") == FALSE) {
                        dd <- unoccdd
                }
                
                dd[is.na(dd)] <- 0
                dd$day <- yday(dd$date)
                dd <-dd[order(dd$day),]
                drops <- "date" 
                dd <- dd[,!names(dd) %in% drops]
                col_idx <- grep("day", names(dd))
                dd <- dd[, c(col_idx, (1:ncol(dd))[-col_idx])]
                dailydd <- melt(dd, id = "day")
                degsum[is.na(degsum)] <-  0
                mydata <- melt(degsum, id = "month")
                
                plotdat <- binsum[,c(2,3,5)]
                dddat <- melt(plotdat, id = "avg.DB")
                
                output$table <- renderTable(binsum)
                output$table2 <- renderTable(degsum)
                output$data <- renderPlot(ggplot(data = dddat, aes(x = avg.DB, y = value, group = variable, colour = variable, fill = variable))  + geom_bar(position = "dodge", stat = "identity") +ylab("hours"))
                output$DDdata <- renderPlot(ggplot(data = mydata, aes(x = month, y = value, group = variable, colour = variable, fill = variable )) + geom_bar(position = "dodge", stat = "identity") + ylab("Degree Days"))
                output$DailyDDdata <- renderPlot(ggplot(data = dailydd, aes(x = day, y = value, group = variable, colour = variable, fill = variable )) + geom_bar(position = "dodge", stat = "identity") + ylab("Degree Days"))
                output$downloadData <- downloadHandler(
                        filename = function() { 
                                paste("binsum", '.csv', sep='')
                        },
                        content = function(file) {
                                write.csv(binsum, file)
                        }
                )
                output$downloadData2 <- downloadHandler(
                        filename = function() { 
                                paste("degsum", '.csv', sep='')
                        },
                        content = function(file) {
                                write.csv(binsum, file)
                        }
                )
                output$downloadData3 <- downloadHandler(
                        filename = function() { 
                                paste("daily_dd", '.csv', sep='')
                        },
                        content = function(file) {
                                write.csv(dd, file)
                        }
                )
                #       save binned data in useful formats
                #write.xlsx(binsum, "binsum.xlsx")
                #        write.csv(binsum, "binsum.csv"))
        })
        
}
#shinyApp(ui, server = server)