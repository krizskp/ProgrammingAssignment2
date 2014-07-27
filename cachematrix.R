## Functions calculate and store inverse of a matrix in cache

## Function creates a special vector 'matrix'
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Function checks to see if an inverse of the matrix exists in the cache
## Else, calculates and stores the inverse of the matrix
cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
