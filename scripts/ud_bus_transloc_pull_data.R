library(jsonlite)
library(httr)

print(paste("script started at ", format(Sys.time(), "%Y-%m-%d_%H%M%S_%p_%Z")))

getSnapshot <- function() {
  response <- GET("https://udshuttle.transloc.com/Services/JSONPRelay.svc/GetMapVehiclePoints?apiKey=8882812681&isPublicMap=true")
  return(fromJSON(content(response,as="text", encoding="UTF-8")))
}

current_dir = getwd()

get_current_date <- function() {
  return(format(Sys.time(), "%Y-%m-%d", tz = "America/New_York"))
}

get_current_time <- function() {
  return(format(Sys.time(), "%Y-%m-%d_%H%M%S_%p_%Z", tz = "America/New_York"))
}

# get the current date in EST
current_date = get_current_date()
last_date = current_date

# Initialize filename with current datetime
filename = paste0(current_dir,"/ud_bus_transloc_output_", get_current_time(), ".csv")
print(paste("Initial output filename=", filename))

# is_new = TRUE will create a new file and write column name for the first time
is_new = TRUE

while (TRUE) {
  # Check if date has changed
  current_date = get_current_date()
  
  # If date changed, create new file
  if (current_date != last_date) {
    filename = paste0(current_dir,"/ud_bus_transloc_output_", get_current_time(), ".csv")
    print(paste("New day, new output filename=", filename))
    is_new = TRUE
    last_date = current_date
  }
  
  snapshot <- getSnapshot()
  if (length(snapshot) != 0) { # only write new row when data is returned
    snapshot$timestamp = format(Sys.time(), "%Y-%m-%d_%H%M%S_%p_%Z")
    write.table(snapshot,file=filename,sep=";", fileEncoding = "UTF-8",
                row.names=FALSE, col.names = is_new, append = !is_new)
    is_new=FALSE
  }
  Sys.sleep(5)  #pull data every 5 seconds
}

print(paste("script completed at ", format(Sys.time(), "%Y-%m-%d_%H%M%S_%p_%Z")))
