## Put comments here that give an overall description of what your
## functions do
#The functions provide a method to cache the inverse of an invertable matrix 
#through storing it in an object

## Write a short comment describing this function
#The function setters and getters for a matrix in cache and its inverse  
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) im <<- solve
    getInverse <- function() im
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
#The function uses makeCacheMatrix to return the cached inversed matrix that is already exists in cache skipping its calculation, or if it does not exist it calculates the matrix inverse and stores it in the cache. 
cacheSolve <- function(x, ...) {
    im <- x$getInverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setInverse(im)
    im
}
