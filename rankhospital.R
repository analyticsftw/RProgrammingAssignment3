## R Programming
#   Programming assignment 3 - Week 4
#   Julien Coquet

# This function takes three arguments (state, type of outcome and rank) 
# then sorts through a data file to return the name of the hospital 
# with the desired rank (nth best/lowest number of deaths) for a given outcome

rankhospital <- function(state="AL", outcome="heart attack", rank="best") {
  # Inputs default to "AL" for state, "heart attack" for outcome and "best" for rank
  
  # If state.abb is available, check for state validity
  if (!is.null(state.abb)){
    if(!state %in% state.abb == TRUE)
      stop("Invalid state.")
  }
  
  
  if (!rank %in% c("best","worst")){
    if (!is.numeric(rank)){
        stop("Invalid rank.")
    }
  } 
  
  #######
  # Vector mapping
  allowedOutcomes <- c('heart attack'=1,'heart failure'=1,'pneumonia'=1)
  outcomeColumn=c("heart attack"=4, "heart failure"=5, "pneumonia"=6)

  # Exit program if outcome not allowed or set the index for the selected outcome
  if (is.na(allowedOutcomes[outcome])){
      stop("Invalid outcome.")
  } else {
    deathCol = outcomeColumn[outcome]  
  }
  
  ## Read outcome data
  hospitals <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Subset data for selected state
  statesMatch <- hospitals$State==state
  stateData <- hospitals[statesMatch,]
  
  # Plan for possibility there is no data for selected state (extra credit?)
  nres <- nrow(stateData)
  if (nres>0){
    # We have records!
  } else {
    # We don't have records. Output error message and quit
    stop(paste("No data for",state))
  }
  
  # for simplicity, only conserve relevant columns
  stateData <- data.frame(stateData[,c(1,2,7,11,17,23)])
  
  ## Return hospital name in that state with lowest 30-day death

  # Clean & sort, remove NAs
  noNAs <- stateData[deathCol]!='Not Available'
  stateData <- stateData[noNAs,]
  
  # Convert text fields to numeric
  stateData[deathCol] <- lapply(stateData[deathCol],FUN = as.numeric)
  
  # Update number of rows in dataframe
  nres <- nrow(stateData)
  
  # Sort by rate then by alphabetical order
  df <-stateData[order(stateData[deathCol],stateData$Hospital.Name),]
  
  # Find rank
  lnum <- rank
  if (rank > nres) stop("Rank out of range.")
  if (rank == "best") lnum <- 1
  if (rank == "worst") lnum <- nres
  
  # Output
  #names(df) <- c("Hospital name")
  head(df[lnum,]$Hospital.Name,1)
  
}
