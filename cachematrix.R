## Put comments here that give an overall description of what your
## functions do

##  makeMatrix() builds a set of functions and returns the functions within
## a list to the parent environment. That is, results in an object
## that contains four functions

makeMatrix <- function(x = matrix()) {

    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() is required to populate or retrieve the inverse 
## from an object of type makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  
  if(det(data)==0){
    message("inverse it's not possible")
  } else {
    s <- solve(data, ...)
    x$setinverse(s)
    s
  } 
  
}
