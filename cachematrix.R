## Assignment 2 solution - inversion of the matrix with result cacheing

## function which is working as a container for the data - the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- NULL
    ##      getters and setters for the matrix
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    ##      getters and setters for the inversion of matrix
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    
    ##      list of available functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function is used to get the inverse of the matrix, 
## to read it from cache or calculate and save in cache if needed

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
    ## first step is to check if inverse was already calculated and cached, if so return it
    inverse <- x$getInverse()
    if (!is.null(inverse))
    {
        message("getting cached data")
        return(inverse)
    }
    
    ## otherwise calculate inverse of the matrix, store it in cache and finally return it as a result
    message("cache in out of sync with data")
    inverse <- x$get()
    data <- solve(inverse)
    message("caching data")
    x$setInverse(data)
    data
}
