## Functions create the inverse of a matrix x. If the inverse has already
## been calculated, it returns the cached value

makeCacheMatrix <- function(x=matrix()){
  i <- NULL ## first two lines initialise the value of numeric matrix x and the inverse i. 
  setmatrix <- function(y){
    x <<- y
    i <<- NULL
    
  }
  getmatrix <- function() x ## get retrieves the value of the matrix
  setinverse <- function(inverse) i <<- inverse ## sets the value of the makeCacheMatrix variable i to the inverse calculated in CacheSolve
  getinverse <- function() i ## Returns the cached value of i when called
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse,
       getinverse = getinverse) ## returns the functions as a list that can be called in the parent environment
}

cacheSolve <- function(x, ...){ ##function that can only be called using a makeCacheMatrix object
  i <- x$getinverse() ## gets the inverse stored in makeCacheMatrix
  if(!is.null(i)){ ##If the inverse has previously been calculated, this returns the value from the cache
    message("getting cached data")
    return(i)
  }
  value <- x$getmatrix() ##if not, calculates the inverse
  i <- solve(value, ...)
  x$setinverse(i) ## sets the cached valued of the inverse
  i
}
