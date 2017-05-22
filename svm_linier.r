s1 <- c(4,5,1)
s2 <- c(2,3,1)
s3 <- c(1,4,1)

D <- list(as.vector(s1),
          as.vector(s2),
          as.vector(s3))

A <- matrix(, 
            nrow = length(D),
            ncol = length(D))

for(i in 1:nrow(A)){
  for(j in 1:ncol(A)){
    A[i,j] = sum(D[[i]]*D[[j]])
  }
}

b <- matrix(data = c(1,1,1),
            nrow = 3,
            ncol = 1,
            byrow = FALSE)
params <- solve(A,b)
w <- params[1]*s1+params[2]*s2+params[3]*s3
hyperplane <- function(x1, x2, w){
  sign(w[1]*x1+w[2]*x2+w[3])
}

coba <- data.frame(matrix(data = c(5,7,1,1,3,6),
                   nrow = 3,
                   ncol = 2,
                   byrow = TRUE)
                  )
hyperplane(coba[,1],
           coba[,2],
           w)
