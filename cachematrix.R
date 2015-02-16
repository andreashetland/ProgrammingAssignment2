# This program implements and demonstrates how to cache the inverse of a matrix.
# This is particularly useful when working with large matrices that are
# expensive to invert, and which we wish to invert more than once.


# Define a regular, invertible matrix for testing the functions.
myMatrix <- rbind(c(4, 2), c(3, 2))


# This function converts a matrix object into a 'special matrix', which consists
# of a list object containing functions to respectively get and set the matrix
# and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


# Make my cached matrix (the inverse will not have been cached yet).
myCacheMatrix <- makeCacheMatrix(myMatrix)


# Evaluate the cached matrix to inspect the object. We note that myCacheMatrix
# is not a matrix object like myMatrix. Rather, it is a list of functions, see
# the description of the makeCacheMatrix() function above.
myCacheMatrix
# $set
# function (y) 
# {
#     x <<- y
#     inv <<- NULL
# }
# <environment: 0x000000002fce4bc8>
#     
#     $get
# function () 
#     x
# <environment: 0x000000002fce4bc8>
#     
#     $setinverse
# function (inverse) 
#     inv <<- inverse
# <environment: 0x000000002fce4bc8>
#     
#     $getinverse
# function () 
#     inv
# <environment: 0x000000002fce4bc8>


# This function computes the inverse of the cached matrix, which we created
# by calling the makeCacheMatrix() function above. It first checks to see if
# the inverse has already been computed and cached, in which case the function 
# returns the cached inverse. If the inverse has not been cached, then the
# function will compute and cache the inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}


# Invert myCacheMatrix for the first time, storing to cache.
cacheSolve(myCacheMatrix)
#      [,1] [,2]
# [1,]  1.0   -1
# [2,] -1.5    2


# Invert myCacheMatrix for the second time, getting inverse from cache.
cacheSolve(myCacheMatrix)
# Getting cached data.
#      [,1] [,2]
# [1,]  1.0   -1
# [2,] -1.5    2
