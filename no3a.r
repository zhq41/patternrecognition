# Buat suatu program sederhana menggunakan R 
# untuk membandingkan dua algoritme pembelajaran
# menggunakan bayes classifier dan k-nn classifier 
# untuk mengklasifikasikan data wajah. Perlihatkan Hasilnya dan berikan analisis anda

# 1. Prepare Your Work Directory and Get Data
#soal nomor 4
library(EBImage)
location <- '~/Thesis/Neural Network/Pengenalan Pola/anak_anak_polah/uts/foto/'

panjang <- 0
N = 100

label_nama <- rep(c("Yaul","Ariq","Rio","Feliks"), each= 25)

for(i in 1:N){
  y <- readImage(files = paste(location,i,".jpg",sep = ""))
  y1 <- channel(y, 'luminance')
  y2 <-  resize(y1, 128) 
  y3 <- as.vector(y2)
  if(panjang < length(y3)){
    panjang = length(y3)
  }
  cat("hitung panjang gambar ke ", i, "\n")
}
m1 <- data.frame(matrix(ncol = panjang, nrow = 0))

for(i in 1:N){
  y <- readImage(files = paste(location,i,".jpg",sep = ""))
  y1 <- channel(y, 'luminance')
  y2 <-  resize(y1, 128)
  y3 <- as.vector(y2)
  m1 <- rbind(m1, y3)  
  cat("simpan panjang gambar ke ", i, "\n")
}
# 2. Know Your Data (optional) 
#normalisasi

#fungsi normalisasi
prepare <-function(x){
  a = (x-min(x))
  b = max(x) -min(x)
  return (a/b)
}

m1[,1:panjang] <- as.data.frame(lapply(m1[1:panjang],prepare))
m1 <- cbind(m1,label_nama[1:N])
colnames(m1) <-c(1:panjang+1)

# 3. Prepare Your Data
#Lakukan Klasifikasi KNN (Menggunakan Library Class)
any(grepl("class", installed.packages()))
library(class)
'?' (knn)
library (gmodels)


# 2. Gunakan teknik perulangan (loop) untuk menampilkan nilai Error disetiap kombinasi nilai Variabel N 
# dan juga nilai parameter K pada KNN ?

result <- data.frame(c(0),c(0),c(0))
colnames(result) <- c("N", "k", "error_rate")

# jumlah N dimulai dari 2, karena minimal dua fitur
for (i in 2:panjang){
  N = i
  set.seed(1234)
  ind<-sample(2, nrow(m1), replace = TRUE, prob=c(0.67,0.33))
  m1.trainset <- m1[ind==1,(1:i)]
  m1.testset <- m1[ind!=1,(1:i)]
  m1.trainlabel <- m1[ind==1,panjang+1]
  m1.testlabel <- m1[ind!=1,panjang+1]
  for (j in 1:50) {
    m1.pred <- knn(train = m1.trainset, test = m1.testset, cl = m1.trainlabel, k=3)
    tbl_test <- table(m1.pred, m1.testlabel)
    err_rate <- (tbl_test[1, 2] + tbl_test[2, 1])/length(m1.pred)
    result <- rbind(result, c(i,j,err_rate))
    cat("N=", i,", k=",j,", err_rate :",err_rate,"\n")
  }
}

result <- result[2:length(result$error_rate),1:3]
plot(result$error_rate, type = "l")

min(result$error_rate)
#hasil kombinasi N dan k yang menunjukkan error_rate paling minimum
result2 <- result[result$error_rate == min(result$error_rate),1:3]

plot(result2$error_rate, type = "l")