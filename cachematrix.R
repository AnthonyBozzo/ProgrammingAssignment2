## These functions can be used together to cache the results of a matrix to decrease computational load when looping over a large matrix

## Function 1: square invertible matrix --> returns a list

makeCacheMatrix <- function(x = matrix()) {

        inv = NULL
        set = function(y) {
        x <<- y
        inx = NULL
        }
        
        get = function() x
        setinv = function(inv) inv <<- inverse
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Function 2: Will return the inverse of the matrix input to makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        return(inv)


}
