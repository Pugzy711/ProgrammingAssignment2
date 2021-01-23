## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves the inverse from the cache.

## This function creates special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    setmatrix <- function(y){
        x <<- y
        x_inverse <<- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(inverse) x_inverse <<- inverse
    get_inverse <- function() x_inverse
    list(setmatrix = setmatrix, get_matrix = get_matrix, set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function computes the inverse, or retrieves it from the cache.

cacheSolve <- function(x,...) {
    matrix_inverse <- x$get_inverse()
    print(matrix_inverse)
    # check if inverse exists, if it does return inverse
    if (!is.null(matrix_inverse)){
        message("getting cached data")
        return(matrix_inverse)
    }
    data <- x$get_matrix()
    print(data)
    matrix_inverse <- solve(data)
    x$set_inverse(matrix_inverse)
    matrix_inverse
}
