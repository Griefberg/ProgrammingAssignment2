#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( m = matrix() ) {
  
  # init variable for cashing inverse matrix
  i <- NULL
  
  # set the matrix
  setm <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  # get the matrix
  get <- function() m
  
  # set the inverse of the matrix
  setInv <- function(inv) i <<- inv
  
  # inverse of the matrix
  getInv <- function() i
  
  list(set = setm, get = get, setInv = setInv,
       getInv = getInv)
}


#This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  i <- x$getInv()
  
  # return the inverse if it is already set
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  # Otherwise, calculate the inverse
  data <- x$get()
  i <- solve(data) 
  x$setInv(i)
  i
}