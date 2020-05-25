# Calculates the time windows which will be used for efficiency calculations
#
# The time windows are aligned to windowGap intervals going backward from today
# till the date closest to specified startDate.
#
# This function returns a list of lists, where each list contains a start date
# and an end date. sub lists are arranged in aspcending order of their dates
getTimeWindows <- function( startDate=today() - months(6), 
                            endDate=today(), 
                            windowGapInDays=7, 
                            windowSizeInDays=15 ) {
  
  print( "Creating time windows with parameters - " ) 
  print( paste( "   startDate        = ", startDate ) ) 
  print( paste( "   endDate          = ", endDate ) ) 
  print( paste( "   windowGapInDays  = ", windowGapInDays ) ) 
  print( paste( "   windowSizeInDays = ", windowSizeInDays ) ) 
  
  week_markers <- rev( seq.Date( endDate, startDate, "-1 week") )
  time_windows <- list()
  
  for( marker in week_markers ) {
    end_date <- as.Date( marker, origin="1970-01-01" )
    start_date <- end_date - days( 15 )
    time_windows[[length(time_windows) + 1]] <- list( start_date, end_date )
  }
  return (time_windows)
}

# Reads the specified CSV file from the wd$data directory and returns a data frame
loadDataFrame <- function( fileName, folder=WD$data ) {
  df <- read.csv( paste0( folder, "/", fileName ), header = TRUE )
  return (df)
}

