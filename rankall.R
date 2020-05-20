## R Programming
#   Programming assignment 3 - Week 4
#   Julien Coquet

# This function takes two arguments (type of outcome and rank) 
# then sorts through a data file to return the name of the hospitals 
# with the desired rank (nth best/lowest number of deaths) for a given outcome in each state

rankall <- function(outcome="heart attack", rank="best"){
  # Inputs default to "heart attack" for outcome and "best" for rank
  
  if (!rank %in% c("best","worst")){
    if (!is.numeric(rank)){
      stop("Invalid rank.")
    }
  } 
  
  # Vector mapping
  allowedOutcomes <- c('heart attack'=1,'heart failure'=1,'pneumonia'=1)
  outcomeColumn = c("heart attack"=4, "heart failure"=5, "pneumonia"=6)
  
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
  
  # Find unique states and sort them alphabetically
  states <- unique(allStateData$State)
  states <- states[sort.list(states, na.last = NA)]

  # Copy dataframe structure for dataframe to be returned
  rDF <- allStateData[0,]
  
  ## Go through all states and extract state data matching the function then consolidate into one data frame

  for (iState in states){
    sDF <- subset(allStateData, State == iState)
    # Update number of rows in state dataframe
    nres <- nrow(sDF)
    
    # Order state data frame based on outcome rating (ascending) and name (alphabetical)
    sDF <- sDF[order(sDF[deathCol],sDF$Hospital.Name ),]
    
    # Find rank
    lnum <- rank
    if (rank == "best") lnum <- 1
    if (rank == "worst") lnum <- nres
    
    
    # Does requested rank exist? If not replace with just the state name and NAs
    if (is.na(sDF[lnum,]$Provider.Number)){
      iDF <- data.frame()
      iDF <- rbind(iDF, c(NA,NA,iState,NA,NA,NA))
      names(iDF) <- names(sDF)
      iDF <- iDF[1,]
    } else {
      iDF <- sDF[lnum,]
    }
    # Use states for rows
    row.names(iDF) <- iState
    
    # Clean up and insert state data into DF
    rDF <- suppressWarnings(rbind(rDF, iDF))
  }
  
  ## Output / return
  # Only conserve hospital name and state, rename columns
  rDF <- rDF[,c(2,3)]
  names(rDF) <- c("hospital","state")
  
  return (rDF)
  
}