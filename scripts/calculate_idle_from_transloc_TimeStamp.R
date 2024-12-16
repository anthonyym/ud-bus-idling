library(raster)

# According to https://www.gps.gov/systems/gps/performance/accuracy/
# Single frequency GPS with high accuracy has accuracy of <=1.891M 95% of the time.
# so, a bus must move more than this variance to be treated as actually moved
move_variance=1.891

min_idle_time=10  #minimum idle time (seconds) to be included for studies.

input_directory="C:\\Users\\Anthony Chan\\Documents\\UD Idling DoubleMap\\transloc\\transloc_r_project\\ud_bus_transloc_output\\"

output_dir="C:\\Users\\Anthony Chan\\Documents\\UD Idling DoubleMap\\transloc\\transloc_r_project\\ud_bus_idle_times\\"
if (!dir.exists(output_dir)) {dir.create(output_dir)}
# get the output_filename using the date from the input_filename, omitting the timestamp

getIdleTimes = function(input_filepath, output_filepath) {
  snapshot <- read.table(file=input_filepath, header=TRUE, sep=";")
  
  # convert the transloc TimeStamp column from format /Date(1733248111000-0700)/ to 1733248111000
  # and convert milliseconds to seconds
  snapshot$TimeStamp <- as.numeric(gsub("/Date\\((\\d+).*\\)/", "\\1", snapshot$TimeStamp)) / 1000
  
  # convert the script-generated timestamp column format '2024-12-03_105121_AM_UTC' 
  # to '2024-12-03 05:51:21' (Eastern Time)
  snapshot$timestamp <- as.POSIXct(strptime(snapshot$timestamp, "%Y-%m-%d_%H%M%S"), tz="UTC")
  snapshot$timestamp <- format(snapshot$timestamp, tz="America/New_York")
  # rename timestamp to timestampFromSampling to indicate the time when the 
  # Transloc API call was made when sampling the data vs the the TimeStamp received
  # from the Transloc API call.
  colnames(snapshot)[colnames(snapshot) == 'timestamp'] <- 'timestampFromSampling'
  
  hashmap <- new.env()
  df_idleTimes <- data.frame()
  output_count=0
  
  for (r_idx in 1:nrow(snapshot)) {
    bus_id <- toString(snapshot[r_idx,"Name"])
    if (is.null(hashmap[[bus_id]])) {
      
      # save this location in hashmap
      hashmap[[bus_id]] <- merge(
        snapshot[r_idx,c("Name","Latitude","Longitude", "TimeStamp","RouteID","IsDelayed","IsOnRoute","timestampFromSampling")], data.frame(elapseTime = 0, distance = 0))
      
    } else {
      
      # get distance between latest snapshot and last saved location
      distance <- pointDistance(
        c(snapshot[r_idx,"Longitude"], snapshot[r_idx,"Latitude"]),
        c(hashmap[[bus_id]][,"Longitude"], hashmap[[bus_id]][,"Latitude"]),
        lonlat=TRUE)
      
      # print(paste(bus_route, distance))
      # check if bus has actually moved more than the predefined variance
      if (distance > move_variance) {
        
        # if idle for longer than min_idle_time.  Then record it for plotting (from last snapshot,
        # not current, because it is not certain when the bus started to move from idle)
        if (hashmap[[bus_id]][,"elapseTime"] >= min_idle_time) {
          
          # add it to dataframe
          
          df_idleTimes <- rbind(df_idleTimes, hashmap[[bus_id]])
          output_count = output_count + 1
          
        }
        
        #reset the hashmap record for this bus
        hashmap[[bus_id]] <- merge(
          snapshot[r_idx,c("Name","Latitude","Longitude", "TimeStamp","RouteID","IsDelayed","IsOnRoute","timestampFromSampling")], data.frame(elapseTime = 0, distance = 0))
      } else {
        # bus has not moved, update the elapse time
        hashmap[[bus_id]][,"elapseTime"] <- snapshot[r_idx,"TimeStamp"] - hashmap[[bus_id]][,"TimeStamp"]
      }
    }
  }
  
  write.csv(df_idleTimes, output_filepath, row.names=FALSE)
  print(paste("created file",output_filepath))
}

files=list.files(input_directory, pattern="*.csv", full.names = FALSE)
for (input_filename in files) {
  input_filepath = paste0(input_directory, input_filename)
  output_filename = paste0("ud_bus_idle_times_",gsub("ud_bus_transloc_output_","",input_filename))
  output_filepath = paste0(output_dir,output_filename)
  print(paste("input_filepath:",input_filepath))
  print(paste("output_filepath:",output_filepath))
  getIdleTimes(input_filepath, output_filepath)
}
