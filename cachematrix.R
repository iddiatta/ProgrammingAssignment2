## This code has two functions- makeCacheMatrix and cacheSolve. The two functions together find the inverse of a matrix and stores the invers in cache memory. 
## If the inverse of the same matrix is called for then the program returns the inverse after findding from cache
## This reduced computation time



makeCacheMatrix<-function(x=matrix())
{
	## x xhould be an invertible matrix
	## This function sets the matrix, gets the matrix, set the inverse and gets the inverse (similar to the mean example)
	
	inverse<-NULL
	## set function sets the matrix in cache
	set<- function (y)
	{
		x<<-y
		inverse<<-NULL
	}
	## If the matrix exists in cache then get function gets the matrix
	get<-function () x
	## setinverse sets the inverse of the matrix in cache
	setinverse<-function(inverse2) inverse<<- inverse2
	## if the matrix was earlier computed then the getinverse function gets the matrix from cache
	getinverse<- function ()  inverse
	list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <-function (x)
{
	## This function returns the inverse of the invertible matrix
	## Input to this function has to be the output of makeCacheMatrix
	
	## First we search for the matrix in cache
	inverse<-x$getinverse()
	
	## If the matrix is present in cache
	
	if (!is.null(inverse)){
		message("Present in cache")
		return (inverse)
		}

	matrix<-x$get()
	inverse<- solve(matrix)
	## above function calculates inverse

	## Set the inverse in cache
	x$setinverse(inverse) 

	return (inverse)
}
