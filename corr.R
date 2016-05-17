corr <- function(dir, threshold = 0){
	#get all files in directory
	monitorFiles <- list.files(dir)	
	correlation <- c();
	
	#get complete cases > threshold
	for(file in monitorFiles){
		
		#get data		
		monitorData <- readMonitorFileName(dir, file)
		monitroDataComplete <- complete.cases(monitorData)
		monitorDataClean <- monitorData[monitroDataComplete,]
		numComplete <- nrow(monitorDataClean)
		
		#calculate correlation if threshold satisfied
		if(numComplete > threshold){
			Corr <- cor(monitorDataClean[["sulfate"]], monitorDataClean[["nitrate"]])
			#add correlation to return vector
			correlation <- c(correlation, Corr)
		}
	}	
	correlation
}

readMonitorFileName <- function(dir, monitorFilename){
	#generate fully qualified name
	fileName <- paste(dir, "\\", monitorFilename, sep = "")
	read.table(fileName, sep = ",", header = TRUE)
}