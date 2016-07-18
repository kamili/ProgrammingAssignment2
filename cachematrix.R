
## The makeCacheMatrix function creates an invertible matrix
makeCacheMatrix <- function( m = matrix() ) {
  theInverse = NULL # set inverse variable to null
  setTheMatrix = function( a ) { # set the matrix
    # assign a value to this object in a different environment than current one.
    m <<- a 
    theInverse <<- NULL 
  }
  getTheMatrix = function() m # get the matrix
  setTheInverse = function( inverse ) theInverse <<- inverse # set the inverse
  getTheInverse = function() theInverse # get the inverse
  # list below goes into the cacheSolve() function
  list( setTheMatrix = setTheMatrix, getTheMatrix = getTheMatrix, setTheInverse = setTheInverse, getTheInverse = getTheInverse )
}


# The cacheSolve function takes input from the makeCacheMatrix function and gets the inverse from cache.
cacheSolve <- function( m, ... ) { ## Return the inverse matrix of 'm'
  theInverse = m$getTheInverse()
  if( !is.null( theInverse ) ) { ## If m's inverse already calculated...
      message("get cached list") ## Get inverse from cache and don't compute
      return ( theInverse )
  }
  ## else, calculate m's inverse
  mat.data = m$getTheMatrix() 
  theInverse = solve( mat.data, ... ) 
  m$setTheInverse( theInverse ) ## set value of inverse in cache
  return( theInverse ) 
}
