## This pair of functions caches the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object (which is really a list of
## functions) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Sets value of i (which stores the inverse of the matrix) to NULL, as
        ## a new matrix is being used.
        i <- NULL
        
        set <- function(y) {
                
                ## Substitutes matrix x with y, the input.
                x <<- y
                
                ## Resets i to NULL, as any previously calculated inverses are no
                ## longer needed.
                i <<- NULL
        }
        
        ## Returns value of x
        get <- function() x
        
        ## Stores value of "inverse" to i
        setinverse <- function(inverse) i <<- inverse
        
        ## Returns value of i
        getinverse <- function() i
        
        ## Stores the four functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the above
## function. It first checks to see if the inverse has already been calculated.
## If so, the inverse is retrieved from the cache. Otherwise, the inverse is calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Assigns value of inverse to i
        i <- x$getinverse()
        
        ## Checks to see if the inverse has been previously calculated
        if(!is.null(i)) {
                
                message("getting cached data")
                
                ## Returns the inverse of the matrix, if previously calculated.
                return(i)
        }
        
        ## Stores original matrix to "data"
        data <- x$get()
        
        ## Calculates inverse of matrix and stores it to i
        i <- solve(data, ...)
        
        ## Sets inverse
        x$setinverse(i)
        
        ## Returns inverse of matrix
        i
}