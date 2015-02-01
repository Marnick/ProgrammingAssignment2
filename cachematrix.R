## The function makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to
## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the mean
## 4.  get the value of the mean

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)}


## The function cacheSolve calculates the inverse of the special "matrix"
## created with makeCacheMatrix. It first checks if the inverse is already
## calculated. If so, it returns the cached value. Otherwise, it calculates
## the invere of the data and sets the value of the inverse in the cache via 
## the `setinverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
