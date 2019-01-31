# These functions allow us to compute the inverse of a matrix 
# more efficiently by caching computed inverses rather than
# recomputing at each element


# Creates a matrix object who's inverse can be cached.
makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y){
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m_inv <<- inv
    getinv <- function() {m_inv}
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}



# Calculates the inverse of the matrix returned by makeCacheMatrix only
# if the inverse has not already been calculated and cached
cacheSolve <- function(x, ...) {
    m_inv <- x$getinv()
    if(!is.null(m_inv)) {
        message("getting cached inverse")
        return(m_inv)
    }
    mdata <- x$get()
    m_inv <- solve(mdata, ...)
    x$setinv(m_inv)
    m_inv
}
