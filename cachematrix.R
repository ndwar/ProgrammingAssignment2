## The first function, makeCacheMatrix creates a special "MATRIX"
## The function below will 
## -set the value of the Matrix
## -get the value of the Matrix
## -set the value of the Inverse Matrix
## -get the value of the Inverse Matrix

##Example of how this works
##b<- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
##b$get()
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
    set<-function(y){
      x<<-y
      m<<-NULL
    }
  get<-function() x
  setinverse<-function(solve){
    m <<- solve
  }
  getinverse<-function() {
    m
  }
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function calculates the inverse of the matrix created using the
## makecacheMatrix function above
## if the result is available in cache, the inverse of the matrix is diplayed
## along with a message 'getting cached data'

cacheSolve<-function(x,...){
  m<-x$getinverse()
  if(!is.null(m)){
    message ("getting cached data")
    return (m)
  }
  matrix<-x$get()
  m<-solve(matrix,...)
  x$setinverse(m)
  m
}

