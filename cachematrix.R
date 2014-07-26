# Below are two functions, `makeCacheMatrix` and `cacheSolve`, which together 
# can be used to cache the inverse of a matrix. Recall that the the inverse of 
# a square matrix can be computed with the `solve` function in R. For example, 
# if `X` is a square invertible matrix, then
# `solve(X)` returns its inverse.


# `makeCacheMatrix`: This function creates a special "matrix" object that can 
# cache its inverse.  This special "matrix" object is essentially a list 
# containing a function that:
#         1. sets the value of the matrix
#         2. gets the value of the matrix
#         3. sets the value of the inverse of the matrix
#         4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s<<- NULLL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)
}

# `cacheSolve`: This function computes the inverse of the special "matrix" 
# returned by `makeCacheMatrix` above. If the inverse has already been 
# calculated (and the matrix has not changed), then `cacheSolve` should 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) 
                {message("getting cached data")
                return(s)         
        }
        data <- x$get()
        s <- solve(data,...)
        x$setsolve(s)
        s
}

# Note to future self: When I wrote this in July 2014, I did not entirely
# understand how the code works.  I simply mirrored the example code provided
# for caching the mean of a vector, which I copy-and-pasted below for future
# reference.
 # 
# makeVector <- function(x = numeric()) {
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
# }
# cachemean <- function(x, ...) {
#         m <- x$getmean()
#         if(!is.null(m)) {
#                 message("getting cached data")
#                 return(m)
#         }
#         data <- x$get()
#         m <- mean(data, ...)
#         x$setmean(m)
#         m

