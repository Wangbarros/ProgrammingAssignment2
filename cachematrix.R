## The function makeCacheMatrix takes an argument x of type matrix
## and returns a list with 4 items (which are actually 4 functions)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse )
}


## This function uses the function make CacheMatrix
## It is expecting the special matrix created by the makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse() #query the x matrix's cache
  if(!is.null(m)) {   #If there is a cache
    message("getting cached data")
    return(m) #just return the code, do not compute anything
  }
  data <- x$get() #if there is no cache
  m <- solve(data, ...) #compute the inverse
  x$setinverse(m) #put it in the cache
  m #return the cache
}
