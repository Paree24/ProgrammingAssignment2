## This contains two functions to cache the inverse of a matrix


## This creates a  matrix object that is able cache its inverse
makeCacheMatrix<-function(m=matrix()){
  i <- NULL
  init<-function(matrix){
    m<<-matrix
    i<<-NULL
  }
  recv <- function() {
    m
  }
  initInverse <- function(inverse) {
    i <<- inverse
  }
  recvInverse <- function() {
    i
  }
  list(init = init, recv = recv,
       initInverse = initInverse,
       recvInverse = recvInverse)
}

#Function to retrieve inverse from cache
cacheSolve <- function(x, ...) {
  m <- x$recvInverse()
  if( !is.null(m) ) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$recv()
  m <- solve(data) %*% data
  x$initInverse(m)
  m
}

