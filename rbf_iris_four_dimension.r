# datasets
datairis <- iris

#data 4 var
data1 <- datairis[,-5]
#label
data2 <- as.vector(datairis[,5])
data2[data2=="virginica"] = 2
data2[data2=="versicolor"] = 3
data2[data2=="setosa"] = 1
data2 <- as.numeric(data2)
# virginica = 1
# versicolor = 2
# setosa 3

#1. predetermine the number the centers
n_center <- 3
nilai_seed <- c(2,3,4)
set.seed(2)
idx_center <- sample(c(1:length(data2)), n_center, replace = FALSE)
data_center <- datairis[idx_center,]

#2. use the k-means clustering algorithm to find centers ci

  #function to calculate distance between data to center
  distance_ab <-function(var_a, var_b){
    d_ab = 0
    for(i in 1:length(var_a)){
      d_ab = d_ab + (abs(var_a[,i]-var_b[,i])^2)
    }
    return(sqrt(d_ab))
  }
  
  matrix_distance <- matrix(data = NA,
                            nrow = nrow(datairis),
                            ncol = n_center)
  
  
  for(i in 1:nrow(datairis)){
    for(j in 1:n_center){
      matrix_distance[i,j] = distance_ab(data1[i,], data_center[j,-5])
    }
  }

 #adding one column
 matrix_distance <- cbind(matrix_distance, c(1:nrow(matrix_distance)))
 
 max_idx_ab <-function(var_a){
   idx = 1
   for(i in 2:length(var_a)){
     if(var_a[idx] > var_a[i]){
       idx = i
     }
   }
   return(idx)
 } 

 #we got the result clustering vector in last column of matrix_distance
 for(j in 1:nrow(matrix_distance)){
   matrix_distance[j,n_center+1] = max_idx_ab(matrix_distance[j,1:n_center])
 }
 clustering_vector <- matrix_distance[,n_center+1]
 
 #lets see to join with the actual label, is right there are schooling at the same label
 nama <- c("setosa", "virginica", "versicolor")
 cluster_means <- as.data.frame(cbind(clustering_vector, data2))
 result_cluster_means <- matrix(data = NA,
                                nrow = length(nama),
                                ncol = length(nama))
 for(i in 1:length(nama)){
   for(j in 1:length(nama)){
     result_cluster_means[i,j] = nrow(cluster_means[cluster_means$clustering_vector==i & cluster_means$data2==j,])
   }
 }
 result_cluster_means
 
 # get the center ci : there are three center. get the minimum J
 the_ci <- vector(length = 3)
 for(i in 1:n_center){
  idx1 = 1
  for(j in 1:nrow(matrix_distance)){
    if(j != idx_center[i] && (matrix_distance[idx1,i] > matrix_distance[j,i])){
      idx1 = j
    }
  }
  the_ci[i] = idx1
 }
 
 points1 <- rbind(datairis[the_ci[1],],
                  datairis[the_ci[2],],
                  datairis[the_ci[3],])

#3. calculate phi(x) for all training data samples
gamma <- 1
N     <- nrow(datairis)
ncols <- ncol(datairis)-1
Phi   <- matrix(rep(NA, (n_center)*N), ncol = n_center)

#form matrix and t
for(lin in 1:N){
  for(col in 1:n_center){
    Phi[lin, col] <- exp(-gamma*norm(as.matrix(datairis[lin,1:4]-points1[col,1:4]))^2)
  }
}

# calculate w

w <- (solve(t(Phi) %*% Phi) %*% t(Phi)) %*% as.vector(data2)


#4. use the resultant classifier g(x) for the classification
pred <- rep(0,N)
for(j in 1:N){
  for(k in 1:n_center){
    pred[j] <- pred[j] + w[k]*exp(-gamma*norm(as.matrix(datairis[j,1:4]-points1[k,1:4]))^2)
  }
}

pred <- unlist(lapply(pred, sign))

#RESULT, 100%, wowowow
#  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [54] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#[107] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
