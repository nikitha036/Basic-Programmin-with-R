# Closure named "timer" to create and implement a simple timer

timer <- function()
{
  # Initializing the time
  Init_time <- Sys.time() 
  
  # Initializing the start, stop and split time
  start_time <- stop_time <- split_time <- NA 
  
  # Function to start the timer
  
  start <- function()
  {
    start_time <<- Sys.time()
  }
  
  # Function to split the timer after the timer is being started
  
  split <- function()
  {
    if(is.na(start_time))
    base::stop("Error, Cannot split as timer was not started...") 
    split_time <<- Sys.time()
  }
  
  # Function to stop the timer after the timer is being started
 
  stop <- function()
  {
    if(is.na(start_time))
    base::stop("Error, Cannot stop as timer was not started...")  
    stop_time <<- Sys.time()
  }
  
  # Function that gives the state of each of the above defined fucntions
  
  get_state <- function()
  {
    list(Init = Init_time, Start = start_time, Finish = stop_time, Split = split_time)
  }
  
  # Function to get the split time after the split time function is being called
  
  get_split <- function()
  {
    if(is.na(split_time))
      base::stop("Error, Cannot get split as split was not called...")  
    difftime(split_time,start_time,units = "secs")
  }
  
  # Function to get the time elapsed after the timer has beiung stopped
  
  get_time <- function()
  {
    if(is.na(stop_time))
      base::stop("Error in t$get_time() : Error, Cannot get time as stop was not called...")
    difftime(stop_time,start_time,units = "secs")
  }
  
  list(start = start, split = split, stop = stop, get_state = get_state, get_split = get_split, get_time = get_time)
}

# Assigning the function to a varibale t and get its intial state

t <- timer()
str(t$get_state())

# Checking for error  types 1 and 2 as specified in the question

t$split()
t$stop()

# Starting the timer and getting its current state

t$start()
str(t$get_state())

# Checking for error types 3 and 4 as specified in the question

t$get_split()
t$get_time()

# Calling the split function and getting the current state

t$split()
str(t$get_state())

# Returns the split time after the split function is being called

t$get_split()

# Calling the stop function and getting the current state

t$stop()
str(t$get_state())

# Returns the time elapsed after the stop function is being called

t$get_time()
