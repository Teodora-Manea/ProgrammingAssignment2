## These functions cache the inverse of a matrix.

## The makeCacheMatrix function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) m<<- solve
    getinverse<-function() m
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}

## Computes the inverse of the “matrix” returned by makeCacheMatrix().
## If the inverse has already been calculated and the matrix has not changed, 
## it’ll retrieves the inverse from the cache directly.

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matr<-x$get()
    m<-solve(matr, ...)
    x$setinverse(m)
    return(m)
}
