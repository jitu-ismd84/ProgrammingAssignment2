## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( m = matrix() ) { 
    
    i <- NULL                    ## Initialize the value of the matrix
    
    set <- function( matrix ) {  ## Set the values of matrix
        m <<- matrix
        i <<- NULL
    }
    
    get <- function() {                ## get the values of matrix
        m                              ## Return the matrix
    }
    
    setInverse <- function(inverse) { ## Set the inverse of the matrix
        i <<- inverse
    }
    
    
    getInverse <- function() {       ## get the inverse of the matrix
        i                            ## Return the inverse property
    }
    
    list(set = set, get = get,       ## Return a list of the methods
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function computes the inverse of the special "matrix" 
## created with the above function

cacheInverse <- function(x, ...) {
    m <- x$getInverse()            ## Return a matrix that is the inverse of 'x'
    if( !is.null(m) ) {            ## Return the inverse if its already set
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()               ## Get the matrix from our object
    m <- solve(data) %*% data     ## Calculate the inverse using matrix multiplication
    x$setInverse(m)               ## Set the inverse to the object via the setInverse function
    m                             ## Return the matrix
}