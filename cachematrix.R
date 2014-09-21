## This function creates a special "matrix" object that can cache its inverse. 
## For a very big matrix, it may take too long to compute the inverse, 
## especially if it has to be computed repeatedly (e.g. in a loop). 
## If the contents of a matrix are not changing, it may make sense to cache the value of the inverse 
## so that when we need it again, it can be looked up in the cache rather than recomputed.


## Return a matrix that is the inverse of 'x' 

makeCacheMatrix <- function(x = matrix()) { 

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
} 



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
	
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
