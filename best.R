# Logic & functions

best <- function(state="AL", outcome="heart attack") {
  # Inputs default to "AL" for state and "heart attack" for outcome
  
  # Vector mapping
  allowedOutcomes <- c('heart attack'=1,'heart failure'=1,'pneumonia'=1)
  outcomeColumn=c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  # Exit program if outcome not allowed
  if (is.na(allowedOutcomes[outcome])){
      errorMessage = paste("Outcome",outcome, "not allowed; use one of the following: 'heart attack','heart failure','pneumonia'")
      stop(errorMessage)
  }

  # Pre-run details
  
  ## Read outcome data
  hospitals <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  statesMatch <- hospitals$State==state
  dataByState <- hospitals[statesMatch,]
  
  nres = nrow(dataByState)
  if (nres>0){
    # We have records!
  } else {
    # We don't have records. Output error message and quit
    stop(paste("No data for state ",state))
  }
  print(paste("Found",nres,"records for",state,"state"))
  
  ## Return hospital name in that state with lowest 30-day death
  
  # Select outcome column
  selectedOutcome <- outcomeColumn[outcome]
  
  ## rate
  sortedHospitals <- dataByState[order(dataByState[,outcomeColumn[outcome]]),]
  sortedHospitals <- data.frame(sortedHospitals$Hospital.Name,sortedHospitals[selectedOutcome])
  
  # Rename columns
  names(sortedHospitals) <- c('Hospital name', "Deaths")
  
  # Print top result
  print (paste("Hospital(s) in",state,"with lowest 30-day deaths for",outcome))
  head(sortedHospitals,1)
}
