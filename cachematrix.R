## The results of certain Time consuming computations on data that does not change often 
## or that remain static for a period of time (not very dynamic), can be stored in cache and
## retrieved when needed from the cache. When the data changes the results should be recalculated and 
## stored again in cache.
## Since, Matrix inversion is such a type of time consuming and costly computation, 
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following two functions makeCacheMatrix() and cacheSolve() are used to create a special object that stores a matrix and cache's its inverse.
## Please note that the functions illustrate the scoping rules of the R language and 
## their manipulation to preserve state inside of an R object.
## Also note the usage of the <<- operator which is used to assign a value to an object in an environment 
## that is different from the current environment.

## makeCacheMatrix function defines and stores four functions namely set, get, setinverse and getinverse in a list.
## Assumption - An invertible matrix is assumed as the input
makeCacheMatrix <- function(x = matrix()) 
{
	## Stores NULL in the variable i (i is the variable for storing the inverse of the matrix)
	## i is restored to NULL, everytime a new Matrix is created
	i <- NULL
	  
	## Definition of the set function
	## set() function changes the matrix stored in the main function makeCacheMatrix.
	set <- function(y) 
	{
		x <<- y
		i <<- NULL
	}

	## Definition of the get function
	## get() function returns the matrix x stored in the main function makeCacheMatrix. Doesn't require any input.
      get <- function() x

	## Definition of the setinverse function
	## Simply stores the value of the parameter inverse in a variable i into the main function makeCacheMatrix. 
      setinverse <- function(inverse) i <<- inverse

	## Definition of the getinverse function 
	## Simply gets the value of the variable i into the main function makeCacheMatrix. 
	getinverse <- function() i

	## The following line stores the four functions - set, get, setinverse and getinverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve function Computes, caches, the inverse of matrix
cacheSolve <- function(x, ...) 
{
	## Return a matrix that is the inverse of 'x'

	## getinverse() function value is stored in i
	i <- x$getinverse()

	## Checks the value of i, stored previously with getinverse(), whether it exists and is not NULL.
	## If it exists in memory, it simply returns a message and the value i, that is supposed to be the inverse of the matrix, but not necessarily. 
	if(!is.null(i)) 
	{
		message("getting cached data")
		return(i)
	}

	## If it does not exists in memory, calculate the inverse of the matrix.
	## data gets the matrix stored with makeCacheMatrix
	data <- x$get()

	## solve() is used below to calculate the inverse of the matrix.
	## The inverse of the matrix is stored in i
	i <- solve(data, ...)

	## The new calculated inverse of the matrix is passed and stored in the object generated/assigned with makeCacheMatrix.
	x$setinverse(i)
	i
}
