## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix
##rather than compute it repeatedly 

## Function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m_var<-NULL
  set<-function(inp){
    x<<-inp
    m_var<<-NULL
  }
  get<-function() x
  setmatrix<-function(set_mat) m_var<<- set_mat
  getmatrix<-function() m_var
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x=matrix(), ...) {
  m_mat<-x$getmatrix()
  if(!is.null(m_mat)){
    message("getting cached data")
    return(m_mat)
  }
  matrix<-x$get()
  m_mat<-solve(matrix, ...)
  x$setmatrix(m_mat)
  m_mat
}
