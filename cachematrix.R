## These take a matrix 'x' and return its inverse
## The inverse is computed the first time cacheSolve is called and stored in 'inv'
## Otherwise the inverse is returned straight from 'inv' without computation

## makeCacheMatrix function sets up a list of sub-functions that operate on
## the matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## Function to set the initial values of the matrix and its inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Function to get the matrix
  get <- function(){
    x
  }                   
  
  ## Function to calculate the inverse using the solve() function
  setinv <- function(solve){
    inv <<- solve
  }  
  
  ## Function to fetch the value of the inverse
  getinv <- function(){
    inv
  } 
  
  ## Creates a list containing the four functions defined above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function calls on the sub-functions of makeCacheMatrix to:
## check if the inverse of 'x' has already been computed
## If it has, the inverse is recovered using the getinv() sub-function
## If it has not, the inverse is computed using the setinv sub-function

cacheSolve <- function(x, ...) {
  
  ##Return a matrix that is the inverse of 'x'
  
  ## Get the inverse. first time cacheSolve is called this will be set to NULL
  m <- x$getinv()  
  
  ## Test to check whether inverse has been computed. If it has return it with comms
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if the function hasn't already returned, get the matrix
  data <- x$get()
  
  ## Calculate the inverse of the vector
  m <- solve(data, ...)
  
  ## Set the value of the inverse in the cache, so next time this function called
  ## it will just spew out this value rather than calculating the inverse
  x$setinv(m)
  
  m
}
