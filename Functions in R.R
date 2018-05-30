# Using Functions as a part of the task

#Problem 1

install.packages("ggplot2")  # Installing the ggplot package
library(ggplot2)
ggplot(faithful,aes(x=waiting,y=eruptions)) + geom_point() + stat_smooth(method = "lm", se = F) 
+ xlab("Waiting Time (minutes)") + ylab("Eruption Time (minutes)")  # Plotting the data
er_mod <- lm(eruptions ~ waiting, data=faithful) # Extracting the coefficients
er_mod

#Problem 2

f <- function(x)
{
  x
  if (!is.numeric(x))  # Checking for Non Numeric data
    {
     stop("Error, type should be numeric")
     geterrmessage()
    }
  else if (length(x[is.na(x)])!= 0)  # Checking for NA values
    {
     stop("Error, cannot have NAs")
     geterrmessage()
    }
  else  # Prints the required output if the above conditions doesn't satisfy
   {
    list("Data" =x, "Min" = min(x), "Max" = max(x), "Mean"= mean(x)) 
   }
}
f(c(1:9,"Test"))  # Sample 1
f(c(1:9,NA))      # Sample 2
f(c(1:10))        # Sample 3