## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  
  list(set = set, get = get, 
       setSolve = setSolve, 
       getSolve = getSolve)

}


## Returns the inverse of a given matrix using solve()
## if solve already exists it returns it, otherwise it calculates
## and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ## We did not find a stored value. we are calculating it
  ## storing and returning it.
  data <- x$get()
  s <- solve(data,...)
  x$setSolve(s)
  s 
}
