## Function to create a special "matrix" object that can cache its inverse

makeCasheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Set the value of the cached inverse
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
  
  ## Get the value of the cached inverse
  getInverse <- function() inverse
  
  ## Return a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function to calculate the inverse of a "matrix" object created by makeCasheMatrix above

cacheSolve <- function(x, ...) {
  
  ## Get the cached value if it exists
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## If the cached value doesn't exist, calculate the inverse and cache it
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  
  ## Return the inverse
  inverse
}

# Test Code

## Create a matrix
m <- matrix(c(1,2,3,4), nrow=2)

## Create a cache matrix object
cm <- makeCasheMatrix(m)

## Calculate the inverse of the matrix using the cacheSolve function
cacheSolve(cm)
