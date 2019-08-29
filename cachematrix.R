## The two functions cache the inverse of a matrix

## 1. makecacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y){
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) invr <<- solveMatrix
        getInverse <- function() invr
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## 2.cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invr <- x$getInverse()
        if(!is.null(invr)){
                message("getting cached data")
                return(invr)
        }
        data <- x$get()
        invr <- solve(data)
        x$setInverse(invr)
        invr      
}
