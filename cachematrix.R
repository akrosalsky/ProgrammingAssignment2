# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# cacheSolve computes the inverse of the matrix returned by makeCacheMatrix 
# If the inverse has already been computed and the matrix is unchanged,
# cachesolve retrieves the inverse from the cache.


## Creates a matrix object that can cache its own inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        ## Modifies existing matrix and resets inverse
        set <- function(y = matrix) {
                x <<- y
                inverse <<- NULL
        }
        
        ## Returns matrix
        get <- function() 
                x
        
        ## Caches matrix inverse
        setinverse <- function(user_inverse) inverse <<- user_inverse
        
        ## Returns matrix inverse
        getinverse <- function() inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the matrix and stores it in the cache. 
## If already computed, cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x) {
        
        ## Returns the cached inverse if it has already been computed
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        
        ## Computes, caches, and returns matrix inverse
        else {
                user_matrix <- x$get()
                inverse <- solve(user_matrix)
                x$setinverse(inverse)
                inverse
        }
}
