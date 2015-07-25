## Allows someone to create a matrix that will store it's inverse 
## and when asked to get inverse will return that cached copy instead 
## of recalculating


## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y){
                x<<-y
                s<<-NULL
        }
        get<-function() x
        setsolution<-function(solve) s<<- solve
        getsolution<-function() s
        list(set=set, get=get,
             setsolution=setsolution,
             getsolution=getsolution)
}

## computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
        s<-x$getsolution()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        matrix<-x$get()
        s<-solve(matrix, ...)
        x$setsolution(s)
        s
}