#' These functions demonstrate the use of caching to prevent
#' repetition of costly function calls.


#' Create a cached value in an alternative environment. This overcomes
#' the lexical scope, for otherwise the value  By keeping the cached value in a separate
#' enviroment, accidental modification of the value is prevented. This
#' function sets attributes on the cached matrix that are used by the
#' cacheSolve function.
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
#' works with matrices created through the makeCacheMatrix function.
#'
#' @param x A square matrix
#' @return inv The inverse of x
#' @examples
#' m <- matrix(sample(1:20,100,replace=TRUE),ncol=10)
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
