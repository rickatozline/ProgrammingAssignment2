## The following functions Create and modify and store in cache a square matrix 
## The second function returns the inverse of the square matrix 

## This function perform the task of storing and retrieving a square matrix

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- matrix(y,nrow=2,ncol = 2)
    m <<- NULL
  }
  get <- function() matrix(x , nrow = 2,ncol = 2)
  setmatrix <- function(m) m <<- matrix(m , nrow = 2,ncol = 2)
  getmatrix <- function() m
  list(set = set, get = get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Returns a matrix that is the inverse of 'x' and stores in cache for later retrieval 

cacheSolve <- function(x, ...){  
 
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmatrix(m)
  m
}


