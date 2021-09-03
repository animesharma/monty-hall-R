# Function to return a door that is neither the value passed to 
# tempDoor nor the door chosen by the player
getDoor <- function (tempDoor, chosenDoor, numDoors) {
  i <- 1
  while (i == tempDoor || i == chosenDoor) 
    i <- (i + 1) %% numDoors
  return(i)
}

montyHallSimulator <- function (doors, doorSwitched, runs) {
  # Initialize a list of counts of possibilities
  counts <- list(
    # Player switches their choice after first reveal and wins
    winSwitch = 0,
    # Player does not switch their choice after first reveal and wins
    winNoSwitch = 0,
    # Player switches their choice after first reveal and loses
    loseSwitch = 0,
    # Player does not switch their choice after first reveal and loses
    loseNoSwitch = 0
  )
  numDoors <- length(doors)
  
  # Repeat simulation for multiple iterations
  for (run in 1:runs) {
    # Randomly choose a prize door and a player chosen door
    prizeDoor <- sample(doors, 1)[[1]]
    chosenDoor <- sample(doors, 1)[[1]]
    # Select a non prize door as the revealed door
    revealedDoor <- getDoor(prizeDoor, chosenDoor, numDoors)
    # Simulate player switching chosen door based on the value of
    # `doorSwitched` after the first non-prize door is revealed
    if (doorSwitched == TRUE) {
      chosenDoor <- getDoor(revealedDoor, chosenDoor, numDoors)
    }
    # Increment the respective counts of the cases based on whether or 
    # not the chosen door is the prize door and if the door was switched 
    # by the player after the first (non-prize) door reveal
    if (chosenDoor == prizeDoor && doorSwitched == FALSE) {
      counts$winNoSwitch <- counts$winNoSwitch + 1
    } else if (chosenDoor == prizeDoor && doorSwitched == TRUE) {
      counts$winSwitch <- counts$winSwitch + 1
    } else if (chosenDoor != prizeDoor && doorSwitched == FALSE) {
      counts$loseNoSwitch <- counts$loseNoSwitch + 1
    } else if (chosenDoor != prizeDoor && doorSwitched == TRUE) {
      counts$loseSwitch <- counts$loseSwitch + 1
    }
  }
  # Return the updated value of the counts list
  return(counts)
}

# Initialize list of doors and number of runs for the simulation
doors <- list(0, 1, 2)
runs = 100000
# Run Monty Hall simulation where player switches choice midway
switchResult <- montyHallSimulator(doors, TRUE, runs)
# Run Monty Hall simulation where player stays with initial choice
noSwitchResult <- montyHallSimulator(doors, FALSE, runs)

switchResultWinPercentage <- switchResult$winSwitch * 100 / runs
switchResultLosePercentage <- switchResult$loseSwitch * 100 / runs

noSwitchResultWinPercentage <- noSwitchResult$winNoSwitch * 100 / runs
noSwitchResultLosePercentage <- noSwitchResult$loseNoSwitch * 100 / runs

print(switchResultWinPercentage)
print(switchResultLosePercentage)
print(noSwitchResultWinPercentage)
print(noSwitchResultLosePercentage)

