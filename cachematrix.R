## Programming Assignment 2 -- This function creates a special "matrix" that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse
  
    inv <- NULL
    
  ## Set the value of the matrix
    
      set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
  ## Get the matrix
      
    get <- function() { 
      x 
      }
    
  ## Set the inverse of the matrix
    
    setInverse <- function(inverse) {
      
     inv <<- inverse
    
     }

  ## Get the inverse of the matrix
    
      getInverse <- function() {
          
          inv
      }
    
  ## The list of methods
      
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
  }




## The cacheSolve function takes an inputted matrix, checks to see that an
## inverse has not already been cached.  If there's a cached inverse it 
## returns the cached matrix. If there isn't one, it creates an inverse matrix
## using the solve() function, stores that into the inv cache, and returns the 
## inversed matrix.

## This function is not highly useful in command line work, since we don't know
## for certain that the cached inverse is the right one -- but is useful within
## longer programs where we want to avoid doing the same calculations 
## repeatedly on the same input.

cacheSolve <- function(x, ...) {
       
   ## Return a matrix that is the inverse of 'x'
  
  ## Grabs content of cache
  
    inv <- x$getInverse()
  
  ## Checks to see if the cache is empty.  If it isn't returns cached data
    
      if (!is.null(inv)) {
      message("getting cached inverse matrix")
      return(inv)
      }
    
  ## if cache is empty, creates inverse, caches it and returns that value
    
    data <- x$get()
    
    ## creates inverse,
    
    inv <- solve(data,...)
    
    ## caches it 
    x$setInverse(inv)
    
    ## returns that value
    inv
  }
  
  
  

