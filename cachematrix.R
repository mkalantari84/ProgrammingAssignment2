##The first function creates a special "matrix" object that can cache its inverse.Then,
##the second function computes the inverse of the special "matrix" returned by the first function. 

## makeCacheMatrix  is a function that creates a special "matrix" object which is really a list containing a 
## function to 1) set the value of the matrix, 2) get the value of the matrix, 3)set the inverse of the matrix, and 
## 4) get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    Inv<-NULL
    set<-function(y){
        x<<-y
        Inv<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) Inv<<-inverse
    getinverse<-function() Inv
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)

}

## cachesolve is a function  that calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inv<-x$getinverse()
    if(!is.null(Inv)){
        message("getting cached data")
        return(Inv)
    }
    data<-x$get()
    Inv<-solve(data,...)
    x$setinverse(Inv)
    Inv
}