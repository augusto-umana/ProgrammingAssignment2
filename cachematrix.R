## These couple of functions calculate de inverse of a matrix, 
## in case that the inverse had been calculated, instead of 
## recalculating it returns the previous value from a cache
## 
## Example of use:

##> mat<-matrix(1:4,2,2)
##> prueba<-makeCacheMatrix(mat)
##> cacheSolve(prueba)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##Call again cacheSolve:
##> cacheSolve(prueba)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## makeCahceMatrix constructs an object that stores the original matrix and
## its inverse  in a cache

## cacheSolve is the function that actually calculates the inverse or call the cache 
## if it has been previously calculated

cacheSolve<-function(mat,...){
  ## Return a matrix that is the inverse of 'mat'  
  inv<-mat$getInverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data<-mat$get()
  inv<-solve(data)
  mat$setInverse(inv)
  inv
}

makeCacheMatrix<-function(matriz){
  inv<-NULL
  
  set<-function(y) {
    matriz<<-y
    inv<<-NULL
  }
  
  get <- function(){
    matriz
  }
  
  setInverse<-function(inversa) {
    inv<<-inversa
  }
  
  getInverse<-function() {
    inv
  }
  
  list (set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}
