# Programming Assignment #3
# Plot the 30-day mortality rate for heart attack

# Load data - outcomes
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# Get all figures for column #11
outcome[, 11] <- as.numeric(outcome[, 11])

# Plot histogram
label <- "30-day death rates from heart attack"
hist(outcome[, 11], main = label)