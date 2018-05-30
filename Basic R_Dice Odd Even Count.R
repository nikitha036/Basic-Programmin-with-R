# Determining the count of odd and even numbers displayed when a dice is thrown

#Problem 1

D1 <- sample(1:6,1000,replace = T)  # Dice 1
D2 <- sample(1:6,1000,replace = T)  # Dice 2

set.seed(99)

x <- D1+D2  # Combination of both dices
y <- ifelse (x %% 2 == 0, "even", "odd")  # Checking for odd or even values
a <- c(length(y[y == "odd"]), length(y[y == "even"]))  # Getting the toatl number of odd and even values
names(a) <- c("Number Odd" , "Number Even")  # Printing the count of odd and even values
a

#Problem 2

x
b <- rep(0,11)  # Creating a empty vector to get the required values
z <- 2:12
names(b) <- z  # Assigning names for each value from 2 to 12 
for (i in z)
{
  b[paste(i)] <- length(x[x == i])  # Gettiong the count of each value from 2 to 12
}
b

#Problem 3

Test <- c(letters,LETTERS,0:9)  # Creating the input
x <- 1:5
y <- rep(0,5)
set.seed(99)
for(i in x)
{
  a <- sample(Test,10,replace = T)  # Random Password generation
  password <- paste(a, collapse = "")  #Collapsing the indvidual characters
  y[i] <- password
}
y  # Printing the 5 Passwords


