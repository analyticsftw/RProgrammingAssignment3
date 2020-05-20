## R Programming
#   Programming assignment 3 - Week 4
#   Julien Coquet

# This function takes two arguments (type of outcome and rank) 
# then sorts through a data file to return the name of the hospitals 
# with the desired rank (nth best/lowest number of deaths) for a given outcome in each state

source("rankhospital.R")

rankall <- function(outcome="heart attack", rank="best"){
  # Inputs default to "heart attack" for outcome and "best" for rank
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
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
  allStateData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # for simplicity, only conserve relevant columns
  allStateData <- data.frame(allStateData[,c(1,2,7,11,17,23)])
  
  ## Return hospital name in that state with lowest 30-day death
  
  # Clean & sort, remove NAs
  noNAs <- allStateData[deathCol]!='Not Available'
  allStateData <- allStateData[noNAs,]
  
  # Convert text fields to numeric
  allStateData[deathCol] <- lapply(allStateData[deathCol],FUN = as.numeric)
  
  # Update number of rows in dataframe
  nres <- nrow(allStateData)
  states <-unique(allStateData$State)

  # Find rank
  lnum <- rank
  if (is.numeric(rank) && rank > nres) stop("Rank out of range.")
  if (rank == "best") lnum <- 1
  if (rank == "worst") lnum <- nres
    
  # Go through all states and extract state data matching the function then consolidate into one data frame
  rDF <- allStateData[0,]
  return (rDF)
  for (iState in states){
    sDF <- subset(allStateData, State == iState)
    sDF <- sDF[order(sDF[deathCol],sDF$Hospital.Name ),]
    # Does requested rank exist? If not replace with just the state name
    if (is.na(sDF[lnum,]$Provider.Number)){
      iDF <-  allStateData[0,]
      iDF <- rbind(iDF, "State"=iState,"NA","NA","NA" )
      print(iDF)
      iDF <- iDF[1,]
    } else {
      iDF <- sDF[lnum,]
    }
    rDF <- rbind(rDF, iDF)
  }
  # Sort by rate then by alphabetical order
  #df <- allStateData[order(allStateData[deathCol],allStateData$Hospital.Name ),]
  

  
  # Output
  #names(df) <- c("Hospital name")
  #return(head(df[lnum,]$Hospital.Name,1))
  
  return (rDF)
  
  
  
}