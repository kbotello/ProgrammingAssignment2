## These two functions allow you to calculate the inverse of a matrix
##makeCacheMatrix allows you to create a matrix, while also caching the inverse of the created matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inver <<- inverse
  getInv <- function() inver
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


##cacheSolve returns the inverse of your Matrix created in makeCacheMatrix. If the inverse is not cached, it will calculate the inverse for you. 
##If the inverse is already in your cache, it will return the inverse from the cache. 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inver)
  inver
}
