#Set your own working directory
setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Optimization/Assignment Problem")

# Import lpSolve package
library(lpSolve)

#Import required packages
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)

#Set assignment cost
cost <- matrix(
  c(90, 80, 75, 70,
  35, 85, 55, 65,
  125, 95, 90, 95,
  45, 110, 95, 115,
  50, 100, 90, 100), nrow = 5, byrow = TRUE)

#Set number of task
m <- ncol(cost)

#Set number of workers
n<- nrow(cost)

#Build model
Model <- MIPModel() %>%
  add_variable(x[i, j], i = 1:n, j = 1:m , type = "binary") %>% #define variables
  set_objective(sum_expr(x[i, j]*cost[i,j],i = 1:n, j = 1:m), "min") %>%   #define objective function
  add_constraint(sum_expr(x[i, j], j = 1:m) <= 1, i = 1:n) %>% #define constraints
  add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:m) %>% #define constraints
  solve_model(with_ROI(solver = "symphony", verbosity = 1))

#Model summary

##Objective Function
print(paste("Objective value: ", objective_value(Model)))

## X variables
for (r in 1:n) {
  for (c in 1:m) {
    tmp <- get_solution(Model, x[i, j]) %>%
      filter(variable == "x", i == r , j == c) %>%
      select(value)
    
    if (tmp > 0) {
      print(paste("Worker",r,"assigned to task",c,"(cost:",cost[r,c],")"))
    }
  }
}
