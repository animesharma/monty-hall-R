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
  probabilities <- data.frame(
    counts$winSwitch / runs,
    counts$winNoSwitch / runs,
    counts$loseSwitch / runs,
    counts$loseNoSwitch / runs
  )
  return(probabilities)
}

# Initialize list of doors and number of runs for the simulation
doors <- list(0, 1, 2)
runs = 1000
iterations = 1000

# Run Monty Hall simulation
switchRes <- vector()
noSwitchRes <- vector()
for (i in 1:runs) {
  # switchRes holds the result for the case when the player switches midway
  switchRes <- append(switchRes,
                      montyHallSimulator(doors, TRUE, i)$counts.winSwitch)
  # noSwitchRes holds the result for the case when the player doesn't switch
  noSwitchRes <- append(noSwitchRes,
                        montyHallSimulator(doors, FALSE, i)$counts.winNoSwitch)
}

# Plot the Probability of Winning After Switching Doors
plot(
  1:runs,
  switchRes,
  type = 'l',
  main = 'Probability of Winning After Switching Doors',
  xlab = 'Runs',
  ylab = 'Probability'
)
abline(h = 2 / 3,
       col = 'red',
       lw = '2',
       lty = 2)

# Plot the Probability of Winning Without Switching Doors
plot(
  1:runs,
  noSwitchRes,
  type = 'l',
  main = 'Probability of Winning Without Switching Doors',
  xlab = 'Runs',
  ylab = 'Probability'
)
abline(h = 1 / 3,
       col = 'red',
       lw = '2',
       lty = 2)

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
# Plot the Probability of Winning After Switching Doors
boxplot(
  switch$Winning,
  main = "Probability of Winning After Switching Doors",
  xlab = "Winning With Switching (1000 runs, 1000 iterations)",
  ylab = "Probability",
  col = "orange"
)
# Plot the Probability of Losing After Switching Doors
boxplot(
  switch$Losing,
  main = "Probability of Losing After Switching Doors",
  xlab = "Losing With Switching (1000 runs, 1000 iterations)",
  ylab = "Probability",
  col = "orange"
)
# Plot the Probability of Winning Without Switching Doors
boxplot(
  noSwitch$Winning,
  main = "Probability of Winning Without Switching Doors",
  xlab = "Winning Without Switching (1000 runs, 1000 iterations)",
  ylab = "Probability",
  col = "orange"
)
# Plot the Probability of Losing Without Switching Doors
boxplot(
  noSwitch$Losing,
  main = "Probability of Losing Without Switching Doors",
  xlab = "Losing Without Switching (1000 runs, 1000 iterations)",
  ylab = "Probability",
  col = "orange"
)

