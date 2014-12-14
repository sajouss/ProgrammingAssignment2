## Given a Matrix this function will store it
## it will also store the solve() value of the matrix
## for later retrieval

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  
  ##return the list of operations that can be performed on the 
  ##chached value
  
  list(set = set, get = get, 
       setSolve = setSolve, 
       getSolve = getSolve)

}


## Returns the compputed inverse of a given square matrix using solve()
## If the solve value already exists it returns it, otherwise it calculates
## and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  
  ##Found cached value, so we will not recalculate it
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
