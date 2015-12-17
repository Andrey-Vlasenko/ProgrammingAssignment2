submit_script <- function(WorkingDirectory = "C:/Users/vlasenko/YandexDisk/R_Programming/ProgrammingAssignment2", SourceName = "cachematrix.R") {

rm(list = ls())
## WorkingDirectory <- "C:/Users/vlasenko/YandexDisk/R_Programming/2_ProgrammingAssignment2"
## SourceName <- "cachematrix.R"

setwd(WorkingDirectory)

## Set test example
k <- sample(4:8, 1)
m <- matrix(sample(-100:100,k^2),nrow = k, ncol = k) / 200
while (det(m) == 0) { 
	k <- sample(4:8, 1)
	m <- matrix(sample(-100:100,k^2),nrow = k, ncol = k) 
}
print(paste("(Determinant of test matrix <> 0) is", det(m) != 0))
m1 <- solve(m)

## Testing
source(SourceName)
M <- makeCacheMatrix(M)
# Empty
M$get()
M$set(m)

# Not empty and equal m
print(paste("(M = test matrix) is", identical(M$get(), m)))

# Empty
print(paste("(Inverted matrix is NULL) is ",is.null(M$getsolve())))

# Not empty and equal m1

m2 <- cacheSolve(M)
print(paste("(Returned inverted matrix is inverted matrix) is ",identical(m2,m1)))

print(paste("(Inverted matrix in M is inverted matrix) is ",identical(M$getsolve(),m1)))

M$setsolve(m)
print(paste("(setsolve is working) is ",identical(M$getsolve(),m)))

}