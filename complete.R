complete <- function(directory, id = 1:332){
	#initialize nobs
	nobs <- seq(0, by = 0, length.out = length(id))
	
	#intialize data frame
	completeRecords <- data.frame(id, nobs)
	
	#loop each file and count number of complete records
	index <- 1
	for(monitor in id){
		measurements <- readMonitorFile(directory, monitor)
		isComplete <- complete.cases(measurements)
		numComplete <-  sum(isComplete)	
		completeRecords[index, 1] <- monitor
		completeRecords[index, 2] <- numComplete
		index <- index + 1
	}
	completeRecords
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