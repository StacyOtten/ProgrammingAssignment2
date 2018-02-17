## The following explanation regarding the example provided in the exercise helped me understand this assignment:
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md


## The makeCacheMatrix function creates a special "matrix" to be used in the cacheSolve function.
## This function returns a set of functions as a list where the data can be accessed in the global environment.
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL 
        set <- function(y){
                X <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        getInverse <- function() {
                inv 
        }
        list (set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## The cacheSolve function returns the inverse of the matrix from the cache.
## This function will produce an error if the matrix is not a square.
## Non-square matrices do not have an inverse. It is assumed the matrix can be inverted. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx, ...)
        x$setInverse(inv)
        inv
}
