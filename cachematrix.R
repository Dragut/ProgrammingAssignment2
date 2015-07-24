## Programming Assignment you will take advantage of the scoping rules of
##the R language and how they can be manipulated to preserve state inside
##of an R object.
## The following functions will take advantage of the scoping rules of
##the R language and preserve state of a complex matirx inverse operation inside
##of an R object.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solved) m <<- solved
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}
## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve`  retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  datas <- x$get()
  m <- solve(datas)
  x$setinverse(m)
  m
}


