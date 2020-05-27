library( dplyr )
library( lubridate )
library( tibble )
library( ggplot2 )
library( RMySQL )

# Set the working directory to the project folder.
#setwd( "~/projects/source/JEEPrepAnalysis" )

source( paste0( getwd(), "/R/include/ref_data.R" ) ) 
source( paste0( getwd(), "/R/include/utility_functions.R" ) )

# Load data frames
df <- loadTestAttemptDataset( src="file" )

df$date_attempted <- as.Date  ( df$date_attempted )
df$subject_name   <- as.factor( df$subject_name   )
df$question_type  <- as.factor( df$question_type  )
df$topic_name     <- as.factor( df$topic_name     )

plot.df <- df %>%
  select( date_attempted, subject_name, question_type, topic_name, 
          is_correct, root_cause, time_spent ) %>%
  filter( as.integer( is_correct ) == 0 ) %>%
  mutate( root_cause = ifelse( is.na( root_cause ), "SUCCESS", root_cause ) ) %>%
  mutate( rc_attribution = ifelse( root_cause %in% RC$competence & is_correct==0, "RC_C", 
                                   ifelse( root_cause %in% RC$behavioral & is_correct==0, "RC_B", "" ) ) )

ggplot( plot.df, aes( x=question_type, group=question_type, fill=question_type ) ) +
  geom_histogram( stat="count", alpha=0.5 ) 
