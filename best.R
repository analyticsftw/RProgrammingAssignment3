# Logic & functions

best <- function(state="AL", outcome="heart attack") {
  # Inputs default to "AL" for state and "heart attack" for outcome
  
  # If state.abb is available, check for state validity
  if (!is.null(state.abb)){
    # we have state.abb support
    if(!state %in% state.abb == TRUE)
      stop("Invalid state.")
  }
  
  # Vector mapping
  allowedOutcomes <- c('heart attack'=1,'heart failure'=1,'pneumonia'=1)
  #outcomeColumn=c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  outcomeColumn=c(
    "heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
    "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
    "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  # Exit program if outcome not allowed
  if (is.na(allowedOutcomes[outcome])){
      #errorMessage = paste("Outcome",outcome, "not allowed; use one of the following: 'heart attack','heart failure','pneumonia'")
      stop("Invalid outcome.")
  }

  ## Read outcome data
  hospitals <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  statesMatch <- hospitals$State==state
  stateData <- hospitals[statesMatch,]
  
  nres = nrow(stateData)
  if (nres>0){
    # We have records!
  } else {
    # We don't have records. Output error message and quit
    stop(paste("No data for",state))
  }
  
  ## Return hospital name in that state with lowest 30-day death
  
  # Select outcome column
  selectedOutcome <- outcomeColumn[outcome]
  
  ## Clean & sort
  # Remove NAs
  #stateOutcomeData <- stateData[]
  stateData <- stateData[selectedOutcome]!='Not Available'

  
  # Sort
  sortedHospitals <- stateData[order(round(as.numeric(stateData[selectedOutcome])))]
  #sortedHospitals <- data.frame(sortedHospitals$Hospital.Name,sortedHospitals[selectedOutcome])
  
  # Rename columns (optional)
  #names(sortedHospitals) <- c('Hospital name', "Deaths")
  
  ## Print top result 
  #print (paste("Hospital(s) in",state,"with lowest 30-day deaths for",outcome))
  #head(sortedHospitals,1)
  head(sortedHospitals,1)
}
