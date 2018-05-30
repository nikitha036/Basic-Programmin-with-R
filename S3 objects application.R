# Function to create a user defined class my_lm for a linear model 

my_lm_create <- function(x)
{
  structure(x,class = c("my_lm","lm"))
}

# Creating linear model for user defined sub class my_lm

l <- my_lm_create(lm(eruptions~waiting,data=faithful))

class(l)

# Creating the generic function for summary

summary <- function(a)
{
  UseMethod("summary")
}

# Summary function for the class my_lm

summary.my_lm <- function(b)
{
  # Gives the Coefficient of the linear model
  
  cat("============================================================",
      "My Summary Function for the Linear Model Class my_lm",sep = "\n")
  cat("\nCoefficients\n")
  cat(names(b$coefficients) <- c("(Intercept)","waiting"))
  cat("\n",b$coefficients,"\n")
  
  # Gives the residuals of the linear model
  
  cat("\nResiduals\n" )
  mini <- quantile(b$residuals,0)
  q1 <- quantile(b$residuals,0.25)
  med <- quantile(b$residuals,0.5)
  q3 <- quantile(b$residuals,0.75)
  mean <- mean(b$residuals)
  maxi <- quantile(b$residuals,1)
  residu <- c(round(mini,digits = 3),round(q1,digits = 4),
              round(med,digits = 5),formatC(mean,format = "e",digits = 2),
              round(q3,digits = 4),round(maxi, digits = 3))
  cat(names(residu) <- c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max."),"\n")
  cat(residu,"\n")
  
  # Number of observations for waiting and eruption is being calculated and printed
  
  cat("\nNumber of observations for eruptions is ",length(b$residuals))
  cat("\nNumber of observations for waiting is ",length(b$effects))
  
  # Gives the R square and Adjusted R square values for the linear model
  
  cat("\n\nR Squared Value ",summary.lm(b)$r.squared)
  cat("\nAdjusted R Squared Value ",summary.lm(b)$adj.r.squared)
  cat("\n============================================================")
}

# Calling the summary function for the class my_lm

summary(l)

# Reverting back to default summary of the linear model

class(l) <- "lm"
summary(l)
  