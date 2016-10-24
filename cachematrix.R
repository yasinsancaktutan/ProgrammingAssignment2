
#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  setMatrix <- function(y){
    x <<- y
    inverseMatrix <- NULL
  }
  
  getMatrix <- function() x
  
  getInverse <- function() {
    if(is.null(inverseMatrix)){      
      inverseMatrix <<- solve(x)
    }else{
      message("Fetching")  
    }
    
    inverseMatrix
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse)
  
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then the cachesolve retrieves the inverse
# from the cache.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  inverseMatrix
  ## Return a matrix that is the inverse of 'x'
}
