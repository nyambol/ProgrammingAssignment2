#' These functions demonstrate the use of caching to prevent
#' repetition of costly function calls.
#' Michael Powe
#' 20210926T062641


#' Create a cached value in the parent environment. This overcomes the
#' lexical scope by keeping the cached value in a separate
#' environment. This function also sets attributes on the cached
#' matrix that are used by the `cacheSolve' function.
#'
#'
#' @param x A square matrix
#' @examples
#' m <- matrix(sample(1:20,100,replace=TRUE),ncol=10)
#' makeCacheMatrix(m)
#'

makeCacheMatrix <- function(x = matrix()) {

    ifelse(dim(x)[1] != dim(x)[2], return(message("Must be a square matrix")), "")

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) inv <<- inverseMatrix
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#' Retrieve or set a cached value for the inverse of a given matrix.
#' If a cached value is found, return it. Otherwise, calculate the
#' inverse, store it in the cache and return it. This function only
#' works with matrices created through the `makeCacheMatrix'
#' function. These have the attributes necessary to access the cache.
#'
#' @param x A square matrix
#' @return inv The inverse of x
#' @examples
#' m <- makeCacheMatrix(matrix(sample(1:20,100,replace=TRUE),ncol=10))
#' cacheSolve(m)
#'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
