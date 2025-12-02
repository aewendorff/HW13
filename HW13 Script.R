#################
###HOMEWORK 13###
#################

#Packages: readr
#Author: Aubrey Wendorff

#Objective 1: Analytical Solution-------------------------------------------------------------
#Load Packages
library(readr)

#Read in Dragon Dataset
dragon_df <- read_csv("data/dragon_data.csv")

#Define x and y and length
x <- dragon_df$acres_on_fire
y <- dragon_df$size
n <- length(y)

#Create matrix of x-values and 1's
x_matrix <- as.matrix(cbind(1, x))
    #cbind combines vectors into 1 matrix
    #in this case, a column of 1's and a column of x values

#Solve for slope and intercept
b <- solve(t(x_matrix) %*% x_matrix) %*% (t(x_matrix) %*% y)
analytical_b0 <- b[1] #Indexes to first row, which is the intercept
analytical_b1 <- b[2] #Indexes to first row, which is the slope



#Objective 2: Ordinary Least Squares-------------------------------------------------------------
###PART A###---
#Create slope and intercept vectors
intercepts_1 <- seq(4, 5, by = 0.1)
    #creates vector from 0 to 10 of y with 0.1 between each value

slopes_1 <- seq(0, 1, by = 0.1)
    #creates vector from -10 to 10 with 0.1 between each value

#Create SSE function
SSE_function <- function(b0, b1, x, y) {
  sum((y - (b0 + b1 * x))^2)
}

#Create empty matrix to store the results of the for loop
SSE_matrix <- matrix(data = NA, #dataframe is null
                     nrow = length(intercepts_1), #number of rows = length of intercept vector
                     ncol = length(slopes_1)) #number of cols = length of slope vector

#Loop over SSE function
for(i in seq_along(intercepts_1)) { #loops over intercept vector, indexes which value
  for(j in seq_along(slopes_1)) { #loops over slope vector, indexes which value
    SSE_matrix[i, j] <- SSE_function(intercepts_1[i], slopes_1[j], x, y) 
    #complete grid search for every ith intercept, jth slope, and store in those rows/columns
  }
}


#Find minimum value in matrix
min_SSE <- which(SSE_matrix == min(SSE_matrix), arr.ind = TRUE)
    #finds which cell in SSE grid has minimum
SSE_b0_grid <- intercepts_1[min_SSE[1, "row"]] #Indexes to row
SSE_b1_grid <- slopes_1[min_SSE[1, "col"]]    #Indexes to column


###PART B###---
#Optimization function (minimize RSS for a linear model)
RSS_function <- function(par, x, y) {
  with(dragon_df, sum((par[1] + par[2] * x - y)^2)) 
  #par[1] = intercept, par[2] = slope
}

initial <- c(0,1)  #starting guess

result <- optim(par = initial, fn = RSS_function, x = x, y = y)
    #par = initial values to be optimized over
    #fn = function
    #method = is optional

RSS_b0_optim <- result$par[1]
RSS_b1_optim <- result$par[2]


###PART C###---
#Check convergence and sensitivity to starting values
result$convergence == 0  #0 indicates successful completion

#Test multiple starting values
start_list_1 <- list(c(0,0), c(5,5), c(10,1), c(3,0.5))

lapply(start_list_1, function(start) {
  fit_test <- optim(par = start, fn = RSS_function, x = x, y = y)
  cat("Start:", start, #Prints out the following with each test
      " -> b0:", fit_test$par[1],
      " b1:", fit_test$par[2],
      " Converged:", fit_test$convergence, "\n")
})
    #No sensitivity to starting values, convergence was successful



#Objective 3: Maximum Likelihood Estimation -------------------------------------------------------------
###PART A###
#Create Negative log-likelihood function
NLL_function <- function(b0, b1, x, y) {
  residual <- y - (b0 + b1*x)
  sigma <- sqrt(mean(residual^2)) #residual standard deviation
  -(-n/2 * log(2*pi) - length(y)*log(sigma) - sum(residual^2)/(2*sigma^2))
}

#Create slope and intercept vectors
intercepts_2 <- seq(4, 5, by = 0.1)  #around expected intercept
slopes_2 <- seq(0, 1, by = 0.1)  #around expected slope

#Create empty matrix to store calculated values
NLL_matrix <- matrix(NA, nrow = length(intercepts_2), ncol = length(slopes_2))

#Calculate NLL for each combination
for(i in seq_along(intercepts_2)) {
  for(j in seq_along(slopes_2)) {
    NLL_matrix[i,j] <- NLL_function(intercepts_2[i], slopes_2[j], x, y)
  }
}

#Find minimum value in matrix
min_NLL <- which(NLL_matrix == min(NLL_matrix), arr.ind = TRUE)
NLL_b0_grid <- intercepts_2[min_NLL[1, "row"]]
NLL_b1_grid <- slopes_2[min_NLL[1, "col"]]


###PART B###---
#Create NLL optim function
NLL_optim <- function(par) {
  NLL_function(par[1], par[2], x, y)
}

initial <- c(0, 1)  #starting guess near expected values
result <- optim(par = initial, fn = NLL_optim)

NLL_b0_optim <- result$par[1]
NLL_b1_optim <- result$par[2]


###PART C###
#Check convergence and sensitivity to starting values
result$convergence == 0  #0 indicates successful completion

#Test multiple starting values
start_list_2 <- list(c(0,0), c(5,5), c(10,1), c(3,0.5))

lapply(start_list_2, function(start) {
  fit_test <- optim(par = start, fn = NLL_optim)  
  cat("Start:", start, #Prints out the following with each test
      " -> b0:", fit_test$par[1],
      " b1:", fit_test$par[2],
      " Converged:", fit_test$convergence, "\n")
})
  #No sensitivity to starting values, convergence was successful



# Objective 4 -------------------------------------------------------------

#Analytical: intercept = 4.34, slope = 0.67

#SSE:
    #Grid search: intercept = 4, slope = 0.7
    #Optimization: intercept = 4.34, slope = 0.67

#NLL:
    #Grid search: intercept = 4, slope = 0.7
    #Optimization: intercept = 4.34, slope = 0.67

#The values are all very similar to one another. The largest difference is between grid search values and the rest.
#This is consistent with what we expect from grid searches, as they are less precise than optimization methods. 


