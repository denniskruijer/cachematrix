# If a matrix does not need to be inverted again, 
# it may make sense to cache the inverted matrix so that when we need it again, 
# it can be looked up in the cache rather than recomputed.
# The first function, makeCacheMatrix, creates a special "matrix":

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# The following function inverses the special "matrix" created with the above function. 
# However, it first checks to see if the matrix has already been inverted before. 
# If so, it gets the inverted matrix from the cache and skips the above function. 
# Otherwise, it inverses the matrix and sets this inverted matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("retrieving cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}