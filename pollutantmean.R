pollutantmean <- function(directory, pollutant, id = 1:332){
	pollutantCount <- 0
	numMeasurements <- 0
	p <- as.character(pollutant[1])
	
	for(monitor in id){
		#get data
		monitorData <- readMonitorFile(directory, monitor)				
		#remove NA
		measurementsWithNA <- monitorData[p]
		na <- is.na(monitorData[p])
		measurements <- measurementsWithNA[!na]
		
		#aggregate pollution metrics
		pollutantCount <- pollutantCount + sum(measurements) 
		numMeasurements <- numMeasurements + length(measurements)
	}
	#return average
	round(pollutantCount/numMeasurements, digits = 3)
}


readMonitorFile <- function(dir, monitorNumber){
	#format montior number properly
	monitorFile <- "0"	

	if(monitorNumber < 10){
		monitorFile <- paste("00", as.character(monitorNumber), sep = "")
	}
	else if(monitorNumber >= 10 & monitorNumber < 100){
		monitorFile <- paste("0", as.character(monitorNumber), sep = "")		
	}
	else{
		#no change
		monitorFile <- as.character(monitorNumber)
	}

	#generate fully qualified name
	fileName <- paste(dir, "\\", monitorFile, ".csv", sep = "")
	read.table(fileName, sep = ",", header = TRUE)
}