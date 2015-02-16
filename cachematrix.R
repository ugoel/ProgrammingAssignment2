## The following code contains two functions.
##The first function "makeCacheMatrix" generates a square matrix.
## The second function caches the inverse of the matrix.


## The following function generates an invertible matrix.
## This function also sets and gets the value of the matrix.
## This funtion also sets and gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function "cacheSolve" checks if the inverse of the matrix is available in the cache.
## If the matrix inverse is already available in the cache, the result is retunred from the cache.
## If the result is not available in the cache, the inverse is computed and then cached in the memory and finally retuned to the console.
## The function also assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("Getting the matric inverse from the cache.")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


#Sample program run and the output

# > x = rbind(c(5, -5), c(-5, 5))
# > m = makeCacheMatrix(x)
# > m$get()
#       [,1] [,2]
# [1,]    5   -5
# [2,]   -5    5
# > x = rbind(c(5, -10), c(-10, 5))
# > m = makeCacheMatrix(x)
# > m$get()
#       [,1] [,2]
# [1,]    5  -10
# [2,]  -10    5
# > cacheSolve(m)
#             [,1]        [,2]
# [1,] -0.06666667 -0.13333333
# [2,] -0.13333333 -0.06666667
# > cacheSolve(m)
# Getting the matric inverse from the cache.
#             [,1]        [,2]
# [1,] -0.06666667 -0.13333333
# [2,] -0.13333333 -0.06666667