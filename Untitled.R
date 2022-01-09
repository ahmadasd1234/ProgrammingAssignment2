
makeMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x<<-y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cachematrix <- function(y, ...) {
  i <- y$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- y$get()
  i <- solve(data, ...)
  y$setInverse(i)
  i
}



