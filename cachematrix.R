## Bellow are functions that are used to create a special object
## that stores a matrix and cache's its inverse


## This first function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function calculates the inverse of the special matrix produced
## by the function above (makeCacheMartrix). If the inverse has already been 
## computed, this function will get the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }else{
        inv <- solve(x$get())
        x$setinverse(inv)
        return(inv)
    }
}
