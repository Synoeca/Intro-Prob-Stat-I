#------------------------------------------------------------------------------#
# Section: STAT 510 A (Tuesday 9:30 A.M. - 10:34 A.M.)
# Assignment: Homework 5
#------------------------------------------------------------------------------#


#---------------------------User Defined Functions-----------------------------#


# -----------------------------------------------#
# Returns the mean of the number in given vector
#
# vec: a given vector
# mean: a calculated mean to be returned
# -----------------------------------------------#
FindMean <- function(vec) {
  sum <- 0
  for (i in 1:length(vec)) {
    sum <- sum + vec[i]
  }
  mean <- sum / length(vec)
  return(mean)
}

#----------------------------------------#
# Returns the standard deviation of the given vector
#
# vec: a given vector
# mean: a mean of given vector
# sdSum: a sum of Sigma(x_i - mu)^2
# SD: a calculated standard deviation
# Returns the standard deviation of the vec
#----------------------------------------#
FindSD <- function(vec){
  mean <- FindMean(vec)
  sdSum <- 0
  for (i in 1:length(vec)) {
    sdSum <- sdSum + (vec[i] - mean)^2
  }
  SD <- sqrt(sdSum / (length(vec) - 1))
  return(SD)
}


#----------------------------------------#
# Returns the median of the given vector
#
# vec: a given vector
# sorted: a sorted given vector
# Returns the median of the vec
#----------------------------------------#
FindMedian <- function(vec) {
  sorted <- sort(vec)
  if (length(sorted) %% 2 == 0) {
    return((sorted[length(sorted)/2] + sorted[(length(sorted)/2) + 1])/2)
  }
  else {
    return(sorted[(length(sorted)/2) + 1])
  }
}

#--------------------------------------------------------------------------------#
# Returns the mode of the given vector
#
# vec: a given vector
# modeList: a sorted given vector
# count: store the count of most frequently occurring value
# mode: stores the most frequently appearing value as mode
# modeFinal: stores all values equal to the mode in case there are multiple modes
# Returns the mode of the vec
#--------------------------------------------------------------------------------#
FindMode <- function(vec) {
  modeList <- list()
  count <- 0
  mode <- 0
  
  for (i in 1:length(vec)) {
    int <- vec[i]
    if (!any(sapply(modeList, function(x) int %in% x))) {
      modeList[i] <- int
    }
    else {
      modeList[i] <- int
      temp <- length(which(sapply(modeList, function(x) int %in% x)))
      if (temp > count) {
        count <- temp
        mode <- int
      }
    }
  }
  
  modeFinal <- integer()
  for (i in 1:length(vec)) {
    int <- vec[i]
    temp <- length(which(sapply(modeList, function(x) int %in% x)))
    if (temp >= count) {
      if (!any(sapply(modeFinal, function(x) int %in% x))) {
        modeFinal <- append(modeFinal, int)
      }
    }
  }
  modeFinal <- sort(modeFinal)
  return(modeFinal)
}

#----------------------------------------#
# Returns the minimum of the data set
#
# vec: a given vector
# Returns the minimum of the vec
#----------------------------------------#
FindMinimum <- function(vec) {
  min <- 9999
  for (i in 1:length(vec)) {
    if (vec[i] < min) {
      min <- vec[i]
    }
  }
  return(min)
}

#----------------------------------------#
# Returns the maximum of the data set
#
# vec: a given vector
# Returns the maximum of the vec
#----------------------------------------#
FindMaximum <- function(vec) {
  max <- -9999
  for (i in 1:length(vec)) {
    if (vec[i] > max) {
      max <- vec[i]
    }
  }
  return(max)
}

#----------------------------------------#
# Returns the percentile of the data set
#
# vec: a given vector
# Returns the percentile element of the vec
#----------------------------------------#
FindPercentile <- function(vec, percentile) {
  sorted <- sort(vec)
  locate <- floor(length(sorted) * percentile)
  resultPercentile <- sorted[locate]
  return(resultPercentile)
}

#------------------------------------------------------------------------------#



#-----------------Q1---------------------#

#---(a)---#
dat1=rbinom(100, size=30, 0.5)
print(dat1)

#---(b)---#
mean <- FindMean(dat1)
stDev <- FindSD(dat1)
print(mean)
print(stDev^2)
print(stDev)


#---(c)---#
mode <- FindMode(dat1)
median <- FindMedian(dat1)
print(mode)
print(median)

#---(d)---#
boxplot(dat1,
        main = "X ~ Bin(30, 0.5)",
        xlab = "Value",
        col = "orange",
        border = "brown",
        horizontal = TRUE
)



#-----------------Q2---------------------#

#---(a)---#
dat2 <- round(rnorm(100, mean=10,sd=5),2)
print(dat2)

#---(b)---#
min <- FindMinimum(dat2)
max <- FindMaximum(dat2)
range <- max - min
print(min)
print(max)
print(range)

#---(c)---#
library(lattice)
histogram(dat2, nint = 9)

#---(d)---#
qqnorm(dat2, pch = 1, frame = FALSE)
qqline(dat2, col = "steelblue", lwd = 2)

#---(e)---#
percentile <- FindPercentile(dat2, 0.30)
percentile