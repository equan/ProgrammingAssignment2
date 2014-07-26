## Assignment 2:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.  There are
## four functions that are placed in a list.  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## this function assigns the matrix to x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## this function returns the matrix x that was passed in the argument above
    get <- function() {
        x
    }    
    setInverse <- function(inverse) { 
        ## '<<-' This is a special operator that references an assignment outside of the local scope.
        ## The operator looks back in enclosing environment which is the 'm' that was originally NULL.
        ## 'inverse' is now 'm'
        m <<- inverse 
    }
    getInverse <- function() {
        ## this function returns m
        m
    }    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)    
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix 'm' that is the inverse of 'x'
    m <- x$getInverse()
    ## if 'm' is not NULL then prints the message and return the inverse 'm'
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if 'm' is NULL, function 'get' is called and assigned to 'data'
    data <- x$get()
    ## assume x is an invertible square matrix and solve() gets the inverse matrix of matrix x and assigns to 'm'
    m <- solve(data, ...)
    x$setInverse(m)
    m  
}

##  Below is the test code I used for the two functions.

##   testDat <- c(2, 0, 0, 0, 2, 0, 0, 0, 2)
##   xx <- matrix(testDat, nrow=3)
##   xx
##   cachedXX <- makeCacheMatrix(xx)
##   invMat <- cacheSolve(cachedXX)
##   invMat