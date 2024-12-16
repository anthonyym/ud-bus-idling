# This script is gets stats for idling and run times of each bus, route, and 
# per-day totals, and writes them to the result_file_name.

result_file_name = 'ud_bus_idle_time_stats_combined.csv'

# for each file in rawDataDir, set the filenameHashmap key as the full path to
# that file, value as the full path to the corresponding idle times file. 
rawDataDir = "C:\\Users\\Anthony Chan\\Documents\\UD Idling DoubleMap\\transloc\\transloc_r_project\\ud_bus_transloc_output\\"
# example file in rawDataDir: ud_bus_transloc_output_2024-12-06_035400_AM_EST.csv
idleTimesDir = "C:\\Users\\Anthony Chan\\Documents\\UD Idling DoubleMap\\transloc\\transloc_r_project\\ud_bus_idle_times\\"
filenameHashmap = new.env()
for (rawDataFilename in list.files(rawDataDir, pattern="*.csv", full.names = FALSE)) {
  rawDataFullPath = paste0(rawDataDir,rawDataFilename)
  date = gsub("ud_bus_transloc_output_","",rawDataFilename)
  idleTimesFilename = paste0("ud_bus_idle_times_",date)
  filenameHashmap[[rawDataFullPath]] = paste0(idleTimesDir,idleTimesFilename)
}

getStats = function(rawDataFullPath, idleTimesFullPath) {
  print(paste("rawDataFilename=", rawDataFullPath))
  dfRawData <- read.table(file=rawDataFullPath, header=TRUE, sep=";")
  dfIdleTimes <- read.table(file=idleTimesFullPath, header=TRUE, sep=",")
  
  # convert the transloc TimeStamp column from format /Date(1733248111000-0700)/ to 1733248111000
  # and convert milliseconds to seconds
  dfRawData$TimeStamp <- as.numeric(gsub("/Date\\((\\d+).*\\)/", "\\1", dfRawData$TimeStamp)) / 1000
  # convert the script-generated timestamp column format '2024-12-03_105121_AM_UTC' 
  # to '2024-12-03 05:51:21' (Eastern Time)
  dfRawData$timestamp <- as.POSIXct(as.character(strptime(dfRawData$timestamp, "%Y-%m-%d_%H%M%S", tz = "UTC")), tz="UTC")
  dfRawData$timestamp <- format(dfRawData$timestamp, tz="America/New_York")
  # rename timestamp to timestampFromSampling to indicate the time when the 
  # Transloc API call was made when sampling the data vs the the TimeStamp received
  # from the Transloc API call.
  colnames(dfRawData)[colnames(dfRawData) == 'timestamp'] <- 'timestampFromSampling'
  
  dfIdleTimeStats = data.frame()
  buses = unique(dfRawData$Name)
  for (bus in buses) {
    snapshot_bus = dfRawData[dfRawData$Name == bus, ]
    
    date = snapshot_bus$timestampFromSampling[1]
    routes = paste(sort(unique(snapshot_bus$RouteID)),collapse=", ")
    runTime = as.numeric(difftime(max(snapshot_bus$timestampFromSampling), min(snapshot_bus$timestampFromSampling), units="secs"))
    idleTime = sum(dfIdleTimes[dfIdleTimes$Name == bus, ]$elapseTime)
    ratioIdle = idleTime / runTime
    dfIdleTimeStats = rbind(dfIdleTimeStats, list(date, bus, routes, idleTime, runTime, ratioIdle))
  }
  
  colnames(dfIdleTimeStats) = c("date","busOrRouteName","busesOrRoutes","idleTime","runTime","idleRatio")
  
  totalDate = min(dfIdleTimeStats$date)
  totalIdleTime = sum(dfIdleTimeStats$idleTime)
  totalRunTime = sum(dfIdleTimeStats$runTime)
  totalIdleRatio = totalIdleTime / totalRunTime
  dfIdleTimeStats = rbind(dfIdleTimeStats, list(totalDate,"total","allBuses",totalIdleTime,totalRunTime,totalIdleRatio))
  
  routes = unique(dfRawData$RouteID)
  for (route in routes) {
    snapshot_route = dfRawData[dfRawData$RouteID == route, ]
    
    date = snapshot_route$timestampFromSampling[1]
    buses = paste(sort(unique(snapshot_route$Name)),collapse=", ")
    runTime = as.numeric(difftime(max(snapshot_route$timestampFromSampling), min(snapshot_route$timestampFromSampling), units="secs"))
    idleTime = sum(dfIdleTimes[dfIdleTimes$RouteID == route, ]$elapseTime)
    ratioIdle = idleTime / runTime
    dfIdleTimeStats = rbind(dfIdleTimeStats, list(date, route, buses, idleTime, runTime, ratioIdle))
  }
  return(dfIdleTimeStats)
}

dfIdleTimeStatsCombined = data.frame()
for (rawDataFilename in ls(filenameHashmap)) {
  idleTimesFilename = filenameHashmap[[rawDataFilename]]
  print(paste("rawDataFilename:", rawDataFilename))
  print(paste("idleTimesFilename:", idleTimesFilename))
  dfIdleTimeStatsCombined = rbind(dfIdleTimeStatsCombined, getStats(rawDataFilename,idleTimesFilename))
}

write.csv(dfIdleTimeStatsCombined, result_file_name, row.names = FALSE)
