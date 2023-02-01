# Problem 1
#
#      Occupants |  1  2  3  4  5
# --------------------------------
# Number of Cars | 70 15 10  3  2
#

# a. Find the sample mean number of occupatns.

dataFrameP1 <- setNames(data.frame(Occupants <- c(1,2,3,4,5), NumberOfCars <- c(70,15,10,3,2)), c("Occupants","NumberOfCars"))
View(dataFrameP1)

CreateSampleVector <- function(df) {
  sampleOccupants <- integer()
  for(i in 1:length(df$Occupants)) {
    for(j in 1:df$NumberOfCars[i]) {
      sampleOccupants <- append(sampleOccupants, df$Occupants[i])
    }
  }
  return(sampleOccupants)
}

FindMean <- function(vec) {
  sum <-0
  for (i in 1:length(vec)) {
    sum <- sum + vec[i]
  }
  sum <- sum / length(vec)
  return(sum)
}


sample <- CreateSampleVector(dataFrameP1)
mean1 <- FindMean(sample)
cat("1.a Answer: ", mean1, "\n\n")



# b. Find the sample standard deviation of the number of occupants.

FindSD <- function(vec){
  mean <- FindMean(vec)
  sdSum <- 0
  for (i in 1:length(vec)) {
    sdSum <- sdSum + (vec[i] - mean)^2
  }
  result <- sqrt(sdSum / (length(vec) - 1))
  return(result)
}

stDev1 <- FindSD(sample)
cat("1.b Answer: ", stDev1, "\n\n")


# c. Find the sample median number of occupants.
FindMedian <- function(vec) {
  if (length(vec) %% 2 == 0) {
    return((vec[length(vec)/2] + vec[(length(vec)/2) + 1])/2)
  }
  else {
    return(vec[(length(vec)/2) + 1])
  }
}

median1 <- FindMedian(sample)
cat("1.c Answer: ", median1, "\n\n")

# d. Compute the first and third quartiles of the number of occupants.
FindQuartiles <- function(vec, quartile) {
  returnVec <- sort(vec)
  if (quartile == 1) {
    if (is.integer((length(returnVec) + 1) / 4)) {
      return(returnVec[(length(returnVec) + 1) / 4])
    }
    else {
      return((returnVec[(length(returnVec) + 1) %/% 4] + returnVec[((length(returnVec) + 1) %/% 4) + 1]) / 2)
    }
  }
  else if (quartile == 2) {
    return(FindMedian(returnVec))
  }
  else if (quartile == 3) {
    if (is.integer(3*(length(returnVec) + 1) / 4)) {
      return(returnVec[(3*length(returnVec) + 1) / 4])
    }
    else {
      return((returnVec[(3*length(returnVec) + 1) %/% 4] + returnVec[((3*length(returnVec) + 1) %/% 4) + 1]) / 2)
    }
  }
  else return(NULL)
}

q11 <- FindQuartiles(sample, 1)
q31 <- FindQuartiles(sample, 3)
cat("1.d Answer\nQ1: ", q11, "\nQ3: ", q31, "\n\n")

# e. What proportion of cars had more than the mean number of occupants?

FindMoreThanProportion <- function(vec, standard) {
  proportionCalc <- integer()
  for (i in vec) {
    if (i > standard) {
      proportionCalc <- append(proportionCalc, i)
    }
  }
  result <- length(proportionCalc) / length(vec)
  return(result)
}


proportion1e <- FindMoreThanProportion(sample, FindMean(sample))
cat("1.e Answer: ", proportion1e, "\n\n")

# f. For what proportion of cars was the number of occupants
#    more than one standard deviation greater than the mean?

proportion1f <- FindMoreThanProportion(sample, FindMean(sample) + FindSD(sample))
cat("1.f Answer: ", proportion1f, "\n\n")

# g. For what proportion of cars was the number of occupants
#    within one standard deviation of the mean?

FindWithinProportion <- function(vec, standard, range) {
  proportionCalc <- integer()
  for (i in vec) {
    if (i > standard - range && i < standard + range) {
      proportionCalc <- append(proportionCalc, i)
    }
  }
  result <- length(proportionCalc) / length(vec)
  return(result)
}

proportion1g <- FindWithinProportion(sample, FindMean(sample), FindSD(sample))
cat("1.g Answer: ", proportion1g, "\n\n\n\n")


# Problem 2
#
# There are 10 employees in a particular division of a company.
# Their salaries have a mean of $70,000, a median of $55,000,
# and a standard deviation of $20,000.
# The largest number on the list is $100,000.
# By accident, this number is changed to $1,000,000.

originalSum2 <- integer()
# a. What is the value of the mean after the change?

originalSum2 <- 10 * 70000
changedSum2 <- originalSum2 - 100000 + 1000000
changedMean2 <- changedSum2 / 10
cat("2.a Answer: ", changedMean2, "\n\n")

# b. What is the value of the median after the change?
median <- 55000
cat("2.b Answer: ", median, "\nMedian does not changed because number of the employees remain same\n","\n\n")

# c. What is the value of the standard deviation after the change?
originalStDev2 <- 20000
originalVar2 <- originalStDev2^2
originalSumOfDiff <- originalVar2 * 10
originalSquareOfX <- (10-1) * originalSumOfDiff + 10 * 70000^2
modSumofDiff <- originalSquareOfX - 100000^2 + (1000000)^2
modStvDevSquare <- (1/9) * (modSumofDiff - 10*(changedMean2)^2)
cat("2.c Answer: ", modStvDevSquare, "\n\n\n")


# Problem 3
#
# Forty-five specimens of a certain type of power were analyzed for sulfur trioxide content.
# Following are the result, in percent. The list has been sorted into numerical order.
# 14.1  14.4  14.7  14.8  15.3  15.6  16.1  16.6  17.3
# 14.2  14.4  14.7  14.9  15.3  15.7  16.2  17.2  17.3
# 14.3  14.4  14.8  15.0  15.4  15.7  16.4  17.2  17.8
# 14.3  14.4  14.8  15.0  15.4  15.9  16.4  17.2  21.9
# 14.3  14.6  14.8  15.2  15.5  15.9  16.5  17.2  22.4

specimens <- c(14.1,  14.4,  14.7,  14.8,  15.3,  15.6,  16.1,  16.6,  17.3,
  14.2,  14.4,  14.7,  14.9,  15.3,  15.7,  16.2,  17.2,  17.3,
  14.3,  14.4,  14.8,  15.0,  15.4,  15.7,  16.4,  17.2,  17.8,
  14.3,  14.4,  14.8,  15.0,  15.4,  15.9,  16.4,  17.2,  21.9,
  14.3,  14.6,  14.8,  15.2,  15.5,  15.9,  16.5,  17.2,  22.4)

par(mfrow=c(1,2))

# a. Construct a stem-and-leaf plot for these data
stem(specimens)

# b. Construct a histogram for these data.
hist(specimens)

# c. Construct a boxplot for these data. Does the plot show any outliers?
boxplot(specimens)

cat("The plot shows two outliers, 22.4 & 21.9\n\n\n\n")


# Problem 4
#
# The histogram below presents the compressive strengths of a sample of concrete blocks
# hardened for 28 days. One rectangle from the histogram is missing. What is its height?

missedRF <- 1 - (0.05 + 0.1 + 0.15 + 0.25 + 0.2 + 0.1)
cat("Problem 4 Answer. The missed rectangle's height is : ", missedRF, "\n\n\n\n")



# Problem 5
#
# Which of the following statistics cannot be determined from a boxplot?
# i. The median
# ii. The mean
# iii. The first quartile
# iv. The third quartile
# v, The interquartile range

cat("Problem 5 Answer: The mean\n", "\nBox-and-Whisker plot uses five number summary\n1. Lower fence.\n2. Q1\n3. Q2\n4. Q3\n5. Upper Fence\nThe mean is not used in box plot.\n", "\n\n\n\n")