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
  
  week_markers <- seq.Date( startDate, endDate, "1 week")
  if( tail( week_markers, n=1 ) != today() ) {
    week_markers <- append( week_markers, today() )
  }
  
  time_windows <- list()
  
  for( marker in week_markers ) {
    end_date <- as.Date( marker, origin="1970-01-01" )
    start_date <- end_date - days( 15 )
    time_windows[[length(time_windows) + 1]] <- list( start_date, end_date )
  }
  return (time_windows)
}

# Reads the specified CSV file from the wd$data directory and returns a data frame
loadDataFrameFromFile <- function( fileName, folder=WD$data ) {
  df <- read.csv( paste0( folder, "/", fileName ), header = TRUE )
  return (df)
}

# Loads test attempt dataset
loadTestAttemptDataset <- function( src="file", 
                                    hostname="localhost",
                                    saveAsCSV=FALSE ) {
  
  if( src == "file" ) {
    print( "Loading test attempt data from local file." )
    df <- loadDataFrameFromFile( "test_attempt_dataset.csv")
    return ( df ) 
  }
  else if( src == "db" ) {
    print( paste0( "Loading test attempt data from database at ", hostname ) )
    dbConn = dbConnect( MySQL(), 
                        user = 'root', password = Sys.getenv( "DB_PASSWORD"), 
                        dbname = 'sconsole', host = hostname )
    
    rs = dbSendQuery( dbConn, 
                      "SELECT 
                        ta.date_attempted,
                        mqm.subject_name,
                        mqm.question_type,
                        tm.topic_name,
                        bm.book_short_name,
                        tqa.is_correct,
                        tqa.root_cause,
                        tqa.time_spent
                      FROM 
                        test_question_attempt tqa,
                        mocktest_question_master mqm,
                        test_attempt ta,
                        topic_master tm,
                        book_master bm
                      WHERE
                        tqa.test_question_id = mqm.id AND 
                        tqa.test_attempt_id = ta.id AND 
                        mqm.book_id = bm.id AND 
                        mqm.topic_id = tm.id" )
    
    df <- fetch( rs, n = -1 )
    dbDisconnect( dbConn )
    
    if( saveAsCSV ) {
      print( "Saving dataframe as local csv" )
      write.csv( df, paste0( WD$data, "/test_attempt_dataset.csv" ), row.names = FALSE )
    }
    
    return ( df )
  }
  else {
    print( "Unknown test attempt dataset source. Should be file | db" )
  }
}




