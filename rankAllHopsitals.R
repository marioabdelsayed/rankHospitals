
#read hospital statistics file
outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  
rankall <- function(wantedRate = "heart attack", theRank){
  #list to store initial ranked list retuned by lapply
  #which will need to be unlisted
  rankedList <- list()
  #frame to store unlisted rankedList
  unlistedRank <- data.frame()
  #forms the name of the column to use for getting the rates 
  causeName <- causeString(wantedRate)
  #gets valid sates
  validStates <- state.abb
  checkRate(wantedRate)
  #gets the death rates column number for the given condition
  rateColumnNum <- match(causeName,names(outcomeFile))
  #stores all rates, states, and hopsital info in a seperate data frame
  #for iteration 
  outcomeData <- data.frame(as.numeric(outcomeFile[,rateColumnNum]),
                            outcomeFile[['Hospital.Name']], outcomeFile[['State']])
  #renames columns for ease of use
  colnames(outcomeData) <- c("Outcome","Hospital", "State" )
  #removes incomplete rows
  outcomeData <- outcomeData[complete.cases(outcomeData),]
  outcomeData<- outcomeData[order(outcomeData$State, outcomeData$Outcome,
                                  outcomeData$Hospital),]
  #split data by state
  splitData <- split(outcomeData, outcomeData$State)
  #get the best ranked hospital in each state
  rankedList <- lapply(splitData, nthRankedHosp, theRank)
  #unlist elements if rankedList and store them in unlistedRank
  for (i in 1:length(rankedList)) {
    unlistedRank <- rbind(unlistedRank, unlist(rankedList[i]))
  }
  return (unlistedRank)
}
#gets the nth ranked hospital in each state
nthRankedHosp <- function(theData, theRank){
  #if the requested rank is less than the number of 
  #hospitals in the data
  if(theRank <= nrow(theData)){
    #return the hospital name and the state abbreviation
    return(c(theData[theRank,][["Hospital"]],theData[5,][["State"]]))
  }
  #if not, return the NA and the state
  else
    return(c(NA,theData[1,'State']))
}
#evaluate the rank to make sure it's valid and if it's best or worst, 
#it gets assigned the right value
evalRank <- function(wantedRank, listSize){
  if (wantedRank == 'best'){
    return (1)
  }
  else if(wantedRank == 'worst'){
    return(listSize)
  }
  else if(wantedRank > listSize){
    return(NA)
  }
  else
    return(wantedRank)
}


#forms the name of the column condition to use for getting the 
#condition rate column from the original file
causeString <- function(cause){
  columnName <- 'Hospital.30.Day.Death..Mortality..Rates.from.'
  if (cause == 'heart attack'){
    columnName <- paste0(columnName, 'Heart.Attack')
  } else if (cause == 'heart failure'){
    columnName <- paste0(columnName, 'Heart.Failure')
  } else if (cause == 'pneumonia') {
    columnName <- paste0(columnName, 'Pneumonia')
  }
  return(columnName)
}

#checks if the provided condition is a valid condition
checkRate <- function (theRate){
  if (!(theRate == 'heart attack') && !(theRate == 'heart failure') 
      && !(theRate == 'pneumonia'))  {
    stop('Invalid outcome')
  }
}