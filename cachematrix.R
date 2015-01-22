  ## this function creates a "special" (i.e. square) matrix object that can cache 
  ## its inverse
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
  ## this function computes the inverse of the special matrix returned by 
  ## makeCacheMatrix above. if the inverse has already been calculated 
  ## (and the matrix has not changed) then cacheSolve should return the 
  ## inverse from the cache
  cacheSolve <- function(x, ...) {
    inputs <- list(...)
    matrixUnchanged <- TRUE
    if (length(inputs) > 0) {
      newMatrix <- inputs[[1]]
      matrixUnchanged <- identical(newMatrix, x$get())
    }
    inverse <- x$getinverse()
    if(!is.null(inverse) && matrixUnchanged) {
      message("getting cached data")
      return(inverse)
    }  
    if (matrixUnchanged) 
      data <- x$get() 
    else 
      data <- newMatrix  
    x$set(data)
    inverse <- try(solve(data), silent = TRUE )
    x$setinverse(inverse)
    x$getinverse()
  }
