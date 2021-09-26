#' These functions demonstrate the use of caching to prevent
#' repetition of possibly costly function calls.
#' Michael Powe
#' 20210926T062641


#' Create a cached value in the parent environment. This overcomes the
#' lexical scope by keeping the cached value in a separate
#' environment. This function also sets attributes on the cached
#' matrix that are used by the `cacheSolve' function.
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
#' @param x A square matrix created through `makeCacheMatrix'
#' @return inv The inverse of x
#' @examples
#' cm <- makeCacheMatrix(matrix(sample(1:20,100,replace=TRUE),ncol=10))
#' cacheSolve(cm)
#'
#' m <- matrix(sample(1:20, 100, replace=TRUE), ncol=10)
#' cacheSolve(m)
#' Error: $ operator is invalid for atomic vectors
#'
#' solve(m) == cacheSolve(cm)
#' Using cached data
#'      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#'  [1,] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE  TRUE
#'  [...]

cacheSolve <- function(x) {

    # check for a cached inverse
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Using cached data")
        return(inv)
    }
    # no cache, create it and return the inverse
    mat <- x$get()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
}
