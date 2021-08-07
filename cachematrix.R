## This file contains 
## 1. a function which create a matrix object which can 
## cache its inverse and 
## 2. A function which uses the matrix object created with function above to compute 
##  inverse of a matrix



## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            matInverse <- NULL
            set<- function(y){
                x<<- y 
                matInverse <<-NULL
            }
            get <- function() x
            setInverse <- function(matInv) matInverse <<- matInv
            getInverse <- function() matInverse
            list(set = set, get=get, setInverse = setInverse, getInverse=getInverse)
            }


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        matInverse <- x$getInverse()
        if (!is.null(matInverse)){
            message("getting cached data")
            return(matInverse)
        }
        matX <- x$get()
        matInv <- solve(matX)
        x$setInverse(matInv)
}
