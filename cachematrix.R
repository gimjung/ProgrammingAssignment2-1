```{r}
## This function computes the special "matrix" 
makeCacheMatrix <- function(x = matrix()) { 

  i <- NULL 

 set <- function(y) { 

   x <<- y 

   i <<- NULL 

  } 

  get <- function() x 

  setinverse <- function(solve) i <<- solve 
  getinverse <- function() i 

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 

 } 
```

```{r}
## This function computes the inverse of the special "matrix" 


cachesolve <- function(x, ...) { 

  j <- x$getinverse() 
  if(!is.null(j)) { message("getting cached data") 
  return(j) 
  } 

  matrice <- x$get() 
  j <- solve(matrice, ...) 
  x$setinverse(j) 
  return(j) 
 } 
```

```{r}
## Test Matrix 

m <- makeCacheMatrix(matrix(c(3, 5, 7, 9,1,5,7,4,2), 3, 3)) 
m$get() 
m$getinverse() 
cachesolve(m) 
cachesolve(m) 
```
