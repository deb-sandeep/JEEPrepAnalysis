library( dplyr )
library( lubridate )
library( tibble )
library( ggplot2 )

setwd( "~/projects/source/JEEPrepAnalysis" )

source( paste0( getwd(), "/R/05_ref_data.R" ) ) 
source( paste0( getwd(), "/R/06_utility_functions.R" ) )

# Define some configuration data for this analysis
# START_DATE - The date from which we analyze the dataset. Any data before this
# date will be ignored for this analysis.
START_DATE = today() - months(6)

# Load data frames
topics <- loadDataFrame( "topic_master.csv" )
attempt_details <- loadDataFrame( "test_attempt_dataset.csv")

# Calculate the time windows for which topic efficiencies need to be calculated
time_windows <- getTimeWindows( START_DATE )

# Create an empty dataframe into which we will dump all our window efficiency calculations
window_efficiencies <- data.frame( marker=Date(),
                                   subject_name=character(),
                                   topic_name=character(),
                                   num_questions=integer(),
                                   num_correct=integer(),
                                   num_wrong=integer(),
                                   num_rc_c=integer(),
                                   num_rc_b=integer(),
                                   efficiency=double(),
                                   stringsAsFactors=FALSE ) 

# Analyzes efficiencies for all topics of the questions attempted between the
# provided start and end dates. The analyzed dataframe that is returned has the
# same structure as the 'window_efficiencies' data frame. 
#
# This function is called repeatedly for all the time windows.
analyzeDataInRange <- function( startDate, endDate ) {
  print( paste( "Analyzing data for time range -", startDate, "till", endDate ) )
  
  df = attempt_details %>%
    # Filter only those rows within the date range
    filter( between( as.Date( date_attempted ), startDate, endDate ) ) %>%
    
    # Select a subset of attributes relevant for analysis
    select( date_attempted, subject_name, topic_name, is_correct, root_cause, time_spent ) %>%
    
    # Sort the tibble 
    arrange( subject_name, topic_name, is_correct, root_cause ) %>%
    
    # Classify the error rrot cause as competence or behavior oriented
    mutate( rc_attribution = ifelse( !is_correct && root_cause %in% RC$competence, "RC_C", 
                                     ifelse( !is_correct && root_cause %in% RC$behavioral, "RC_B", "" ) ) ) %>%
    
    # Group data by subject and topic - we want to keep analysis at a topic level
    group_by( subject_name, topic_name ) %>%
    
    # Create some aggreated group summaries
    summarise( num_questions = n(), 
               num_correct = sum( is_correct == 1 ),
               num_wrong = sum( is_correct == 0 ),
               num_rc_c = sum( rc_attribution == "RC_C" ),
               num_rc_b = sum( rc_attribution == "RC_B" ) ) %>%

    # Filter out any rows where all errors are behavioral errors and there
    # are no correct answers. This is to prevent a division by zero during
    # later analysis
    filter( (num_questions - num_rc_b) > 0 ) %>%
    
    # Compute efficiency - note that we are ignoring    
    mutate( efficiency = (num_correct / (num_questions - num_rc_b))*100,
            marker = endDate ) %>%
    
    # Ungroup the data frame
    ungroup() %>%
    
    # Sort the data frame
    arrange( subject_name, efficiency )
    
  return( df[, c(9,1,2,3,4,5,6,7,8)] )
}

# Extracts a tibble from the windows_efficiencies data frame for the given
# subject and topic, sorted by the marker in ascending order.
#
# Note that the returned data frame does not contain the subject or topic name
getTopicTimeSeries <- function( subject, topic ) {
  print( paste( "Extracting sub df for sub=", subject, ", topic=", topic ) )
  topic_ts_df <- window_efficiencies %>%
    filter( subject_name == subject, topic_name == topic ) %>%
    select( marker, num_questions, num_correct, num_wrong, num_rc_c, num_rc_b, efficiency ) %>%
    arrange( marker )

    return (topic_ts_df)
}

# Plots the topic efficiency timeseries 
plotTopicEfficiencyTimeSeries <- function( topicTimeSeries, subjectName, topicName ) {
  
  numQScaleFactor <- 100/(max( topicTimeSeries$num_questions )*2)
  
  ggplot( topicTimeSeries, aes( x=marker ) )  + 
    scale_x_date( date_breaks = "1 month", 
                  date_labels = "%b" ) +
    geom_bar( aes( y=num_questions*numQScaleFactor ), 
              stat="identity", 
              fill="white", 
              colour="grey" ) +
    geom_line( aes( y=efficiency ), 
               stat="identity", 
               color="blue" ) +
    geom_point( aes( y=efficiency ), 
                shape=21, 
                size=3, 
                fill="lightblue" ) +
    scale_y_continuous( sec.axis = sec_axis( ~./numQScaleFactor, 
                                             name="Num questions" ) ) +
    labs( title = topic,
          subtitle = subject,
          x = "Date", 
          y = "Efficiency" ) +
    theme( plot.title = element_text( hjust = 0.5 ),
           plot.subtitle = element_text( hjust = 0.5 ) )
}

# For each window calculate efficiencies of all topics and append the analysis
# data into the master dataframe
for( window in time_windows ) {
  startDate = as.Date( window[[1]] )
  endDate = as.Date( window[[2]] )
  window_df <- analyzeDataInRange( startDate, endDate )
  window_efficiencies <- rbind( window_efficiencies, window_df )
}

subject <- "IIT - Maths"
topic <- "Trigonometry"

topic_timeseries <- getTopicTimeSeries( subject, topic )
plotTopicEfficiencyTimeSeries( topic_timeseries, subject, topic )

