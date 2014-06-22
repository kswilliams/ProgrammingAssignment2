## Function makeCacheMatrix creates a special "matrix"
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix using solve()
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    Set <- function(y){
        x <<- y
        m <<- NULL
    }
    SetMatrix <- function(solve) m <<- solve
    
    Get <- function() x
    GetMatrix <- function() m

    list (set = Set, get = Get, setmatrix = SetMatrix, getmatrix = GetMatrix)
}


## Function cacheSolve calculates the inverse of the special "matrix" created in makeCacheMatrix.
## Checks first if inverse of the matrix has been calculated, if so it uses the cache value and skips the calculation.
## Otherwis, it calculates the inverse of the matrix and sets the value in the cache with the setmatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if (!is.null(m)){
        message("getting cache data")
        return (m)
    }
    matrix_data <- x$get()
    m <- solve(matrix_data, ...)
    x$setmatrix(m)
    m
}
