## Put comments here that give an overall description of what your
## functions do

## this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(inverse){
    inv <<- inverse
  }
  getinverse <- function() inv
  list(set = set,get = get,setinverse = setinverse, getinverse= getinverse)
}


## this function checks the "special" matrix if the inverse is already cached, 
## and otherwise calculate the inverse and cache it before returning
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  message("No cache, computing")
  m <- x$get()
  inv = solve(m)
  x$setinverse(inv)
  return(inv)
}



# Test below 

# Create 3 different vectors 
# using combine method. 
a1 <- c(3, 2, 5) 
a2 <- c(2, 3, 2) 
a3 <- c(5, 2, 4) 

# bind the three vectors into a matrix  
# using rbind() which is basically 
# row-wise binding. 
A <- rbind(a1, a2, a3) 
inverse <- solve(A)

#Create special matrix
special <- makeCacheMatrix(A)

#Check output for message "No cache, computing"
s_inverse <- cacheSolve(special)
# check if this is a matrix of zeros
s_inverse - inverse

#Check out "Getting cached data"
s_inverse <- cacheSolve(special)
# check if this is a matrix of zeros
s_inverse - inverse



