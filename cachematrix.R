## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

###########################################################
makeCacheMatrix <- function(x = matrix()) {
  ##
  ## Simply set the input x as a matrix
  ## and then set the solved value "i" as a null
  ## then changed every reference to "mean" to "solve"
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

###########################################################


## Write a short comment describing this function
## Changed "mean" to "solve" and "m" to "i"

###########################################################
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}
###########################################################
