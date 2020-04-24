## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Created by: Samyak Shah, IIT Bombay
# The function below creates special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  # sets the matrix value to x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the matrix value
  get <- function() x
  # set the inverse value
  setinv <- function(inverse) m <<- inverse
  # get the inverse value
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
#calculates the inverse of the special matrix created above
# It checksto see if the inverse has been calculated or not
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  # In case of longer computations, incomplete m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
