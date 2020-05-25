PROJECT_DIR <- getwd()

WD <- list()
WD$R <- paste0( PROJECT_DIR, "/R" )
WD$data <- paste0( PROJECT_DIR, "/data" )
WD$output <- paste0( PROJECT_DIR, "/output" ) 

SUBJECTS <- c(
  "IIT - Physics",
  "IIT - Chemistry",
  "IIT - Maths"
)

RC <- list()
RC$all <- c( 
  "RECOLLECTION", 
  "CALCULATION", 
  "INTERPRETATION", 
  "CONCEPT", 
  "LATERAL", 
  "ALIEN_CONCEPT", 
  "WTF", 
  "LENGTHY", 
  "STUPID", 
  "JUDGEMENT_ERROR", 
  "UNWARRANTED_RISK", 
  "WTF_LATERAL" 
)

RC$avoidable <- c( 
  "RECOLLECTION", 
  "CALCULATION", 
  "INTERPRETATION", 
  "CONCEPT", 
  "LATERAL", 
  "STUPID", 
  "JUDGEMENT_ERROR", 
  "UNWARRANTED_RISK"
)

RC$unavoidable <- c(
  "ALIEN_CONCEPT", 
  "WTF", 
  "LENGTHY", 
  "WTF_LATERAL" 
)

RC$competence <- c(
  "RECOLLECTION", 
  "CONCEPT", 
  "LATERAL", 
  "ALIEN_CONCEPT", 
  "WTF", 
  "LENGTHY", 
  "WTF_LATERAL" 
)

RC$behavioral <- c(
  "CALCULATION", 
  "INTERPRETATION", 
  "STUPID", 
  "JUDGEMENT_ERROR", 
  "UNWARRANTED_RISK"
)