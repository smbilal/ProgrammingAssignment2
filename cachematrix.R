## The functions are creating a matrix, calculating its inverse and then storing it into
## the cache for future retrievel requests. The second function first checks if the inverse already exists
## in the cache and if it does, it skips the computation and simply pulls the output from cache provided
## the contents of the matrix are the same


## In "makeCacheMatrix" we are creating a matrix by either passing arguments along with the function call or by calling the "set"
## method.It further saves the value of the inverse into a variable/cache using "setinverse" and "getinverse" is used to retrieve the value 
## of inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) inv<<- solve
  getinverse<-function() inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)

}


## In "cacheSolve" we are first checking if the inverse of the matrix created by "makeCacheMatrix" already exists in cahce and if it does,
## the value is pulled from cache for output. Otherwise, it computes the inverse and returns the inversed matrix

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix, ...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
