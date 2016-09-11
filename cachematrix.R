## The pair of functions written below will cache the inverse of a matrix. 
## This will help in saving compution cost for repeated inverse calculation of a matrix

##The makeCacheMatrix function creates a special matrix which is a list containing function to
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of inverse
## 4. get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
  
}


## The cacheSolve function checks if there is a previously calculated inverse for the special matrix generated above in the cache memory and returns it
##If there is no inverse in cache memory it will compute one 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
