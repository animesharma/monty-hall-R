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
  probabilities <- list(
    counts$winSwitch / runs,
    counts$winNoSwitch / runs,
    counts$loseSwitch / runs,
    counts$loseNoSwitch / runs
  )
  return(probabilities)
}

# Initialize list of doors and number of runs for the simulation
doors <- list(0, 1, 2)
runs = 10000
iterations = 1000

# Run Monty Hall simulation.
switchRes <- vector()
noSwitchRes <- vector()
for (i in 1:iterations) {
  # switchRes holds the result for the case when the player switches midway
  switchRes <- append(switchRes,
                      montyHallSimulator(doors, TRUE, runs))
  # noSwitchRes holds the result for the case when the player doesn't switch
  noSwitchRes <- append(noSwitchRes,
                        montyHallSimulator(doors, FALSE, runs))
}

# Create vectors to aggregate the data
tempWinSwitch <- vector()
tempLoseSwitch <- vector()
i <- 1
while (i <= length(switchRes)) {
  tempWinSwitch <- append(tempWinSwitch, switchRes[i])
  tempLoseSwitch <- append(tempLoseSwitch, switchRes[i + 2])
  i = i + 4
}
tempWinNoSwitch <- vector()
tempLoseNoSwitch <- vector()
i <- 1
while (i <= length(noSwitchRes)) {
  tempWinNoSwitch <- append(tempWinNoSwitch, noSwitchRes[i + 1])
  tempLoseNoSwitch <- append(tempLoseNoSwitch, noSwitchRes[i + 3])
  i = i + 4
}

# Create dataframes containing outcomes for cases when the player
# switches the door and does not switch the door
switch <- data.frame("Winning" = unlist(tempWinSwitch),
                     "Losing" = unlist(tempLoseSwitch))
noSwitch <- data.frame("Winning" = unlist(tempWinNoSwitch),
                       "Losing" = unlist(tempLoseNoSwitch))
# Plot the Probability of winning after switching doors
boxplot(
  switch$Winning,
  main = "Probability of winning after switching doors",
  xlab = "Winning with switching (1000 iterations)",
  ylab = "Probability",
  col = "orange"
)
# Plot the Probability of losing after switching the door mid-way
boxplot(
  switch$Losing,
  main = "Probability of losing after switching doors",
  xlab = "Losing with switching (1000 iterations)",
  ylab = "Probability",
  col = "orange"
)
# Plot the Probability of winning without switching the door
boxplot(
  noSwitch$Winning,
  main = "Probability of winning without switching doors",
  xlab = "Winning without switching (1000 iterations)",
  ylab = "Probability",
  col = "orange"
)
# Plot the Probability of losing without switching the door
boxplot(
  noSwitch$Losing,
  main = "Probability of losing without switching doors",
  xlab = "Losing without switching (1000 iterations)",
  ylab = "Probability",
  col = "orange"
)
