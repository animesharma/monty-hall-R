getNonPrizeDoor <- function (prizeDoor, numDoors, chosenDoor) {
  i <- 1
  while (i == prizeDoor || i == chosenDoor) {
    i <- (i + 1) %% numDoors
  }
    
  return(i)
}

switchDoor <- function (revealedDoor, numDoors, chosenDoor) {
  i <- 1
  while (i == revealedDoor || i == chosenDoor) 
    i <- (i + 1) %% numDoors
  return(i)
}

montyHallSimulator <- function (doors, doorSwitched, runs) {
  counts <- list(
    winSwitch = 0,
    winNoSwitch = 0,
    loseSwitch = 0,
    loseNoSwitch = 0
  )
  numDoors <- length(doors)
  
  for (run in 1:runs) {
    prizeDoor <- sample(doors, 1)[[1]]
    chosenDoor <- sample(doors, 1)[[1]]
    originalChoice <- chosenDoor
    revealedDoor <- getNonPrizeDoor(prizeDoor, numDoors, chosenDoor)
    
    if (doorSwitched == TRUE) {
      chosenDoor <- switchDoor(revealedDoor, numDoors, chosenDoor)
    }
    
    if (chosenDoor == prizeDoor && doorSwitched == FALSE) {
      counts$winNoSwitch <- counts$winNoSwitch + 1
    } else if (chosenDoor == prizeDoor && doorSwitched == TRUE) {
      counts$winSwitch <- counts$winSwitch + 1
    } else if (chosenDoor != prizeDoor && doorSwitched == FALSE) {
      counts$loseNoSwitch <- counts$loseNoSwitch + 1
    } else if (chosenDoor == prizeDoor && doorSwitched == TRUE) {
      counts$loseSwitch <- counts$loseSwitch + 1
    }
  }
  return(counts)
}

doors <- list(0, 1, 2)
x <- montyHallSimulator(doors, FALSE, 100000)
print(x)

