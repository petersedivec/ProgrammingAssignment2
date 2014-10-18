## This file contains a matrix object that allows for a cached version of the 
## inverse to be computed and stored so the inverse doesn't need to be computed
## if the matrix has not changed. Underlying assumption is that all matrices
## of this type are square and invertable

## MAKECACHEMATRIX - creates a matrix that contains a cacheable inverse of the
## matrix so the inverse does not always need to be computed

makeCacheMatrix <- function(x = matrix()) {
    # inv is internal representation of the inverse
    inv <- NULL # when object is created inverse is set to NULL till computed
    set <- function(y) { # set/change matrix, as part of this we erase inv value
        x <<- y
        inv <<- NULL
    }
    get <- function() x # get the matrix
    setinv <- function(inverse) inv <<- inverse # assign inverse
    getinv <- function() inv # return inverse
    
    # return a list object with the four functions two gets & two sets
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}

## CACHESOLVE - computes the inverse of a CacheMatrix object and commits it to 
## cache. CACHESOLVE uses SOLVE function to compute the inverse

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    if (!is.null(inv)) { # test if inv is cached, if so return it
        message("Returning cached invervse!")
        return(inv)
    } else { # inverse is not cached so we need to compute it, cache it, return
        inv <-solve(x$get())
        x$setinv(inv)
        return(inv)
    }
        
}
