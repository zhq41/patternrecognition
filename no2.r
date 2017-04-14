# Gunakan teknik perulangan (loop) untuk menampilkan 
# nilai Error disetiap kombinasi nilai Variabel N 
# dan juga nilai parameter K pada KNN (Studi kasus breast cancer)?

datasets <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"),
                     header = FALSE)

a<-c("Id", "diagnosis", "rad_mean", "texture_mean", "perimeter_mean", 
  "area_mean", "smoothness_mean", "compactness_mean", "concavity_mean","points_mean", 
  "sym_mean", "dimn_mean", "rad_se", "texture_se", "perimeter_se", "area_se", 
  "smoothness_se", "compactness_se", "concavity_se", "points_se", "sym_se", 
  "dimn_se", "rad_worst", "texture_worst", "perimeter_worst", "area_worst", 
  "smoothness_worst", "compactness_worst", "concavity_worst", "points_worst", 
  "sym_worst", "dimn_worst")

colnames(datasets) <- a

#fungsi normalisasi
prepare <-function(x){
  a = (x-min(x))
  b = max(x) -min(x)
  return (a/b)
}

#cek diagnonis
table(datasets$diagnosis)

#cek untuk data missing value
sum(is.na(datasets$diagnosis))

# ubah diagnosis (B, dan M)  ke dalam bentuk factor  labels ("benign", "malignant")
levels(datasets$diagnosis) <- c("benign", "malignant")

#normalisasi data
datasets[,3:32] <- as.data.frame(lapply(datasets[3:32],prepare))

#Lakukan Pembagian Data (Data untuk training dan validasi) gunakan “sample”

set.seed(1234)
ind<-sample(2, nrow(datasets), replace = TRUE, prob=c(0.67,0.33))
datasets.trainset <- datasets[ind==1,(3:32)]
datasets.testset <- datasets[ind!=1,(3:32)]
datasets.trainlabel <- datasets[ind==1,2]
datasets.testlabel <- datasets[ind!=1,2]

#Lakukan Klasifikasi KNN (Menggunakan Library Class)
any(grepl("class", installed.packages()))
library(class)
'?' (knn)
library (gmodels)

#Analisis Hasil Prediksi
#Cobakan untuk beberapa nilai k= 3, 7, 11, 31
# K = 3
datasets.pred <- knn(train = datasets.trainset, test = datasets.testset, cl = datasets.trainlabel, k=3)
table(datasets.pred, datasets.testlabel)
CrossTable(x=datasets.testlabel, y=datasets.pred, prop.chisq=FALSE)

# K = 7
datasets.pred <- knn(train = datasets.trainset, test = datasets.testset, cl = datasets.trainlabel, k=7)
table(datasets.pred, datasets.testlabel)
CrossTable(x=datasets.testlabel, y=datasets.pred, prop.chisq=FALSE)

# K = 11
datasets.pred <- knn(train = datasets.trainset, test = datasets.testset, cl = datasets.trainlabel, k=11)
table(datasets.pred, datasets.testlabel)
CrossTable(x=datasets.testlabel, y=datasets.pred, prop.chisq=FALSE)

# K = 31
datasets.pred <- knn(train = datasets.trainset, test = datasets.testset, cl = datasets.trainlabel, k=31)
tbl_test <- table(datasets.pred, datasets.testlabel)
CrossTable(x=datasets.testlabel, y=datasets.pred, prop.chisq=FALSE)
err_rate <- (tbl_test[1, 2] + tbl_test[2, 1])/length(datasets.pred)

# 2. Gunakan teknik perulangan (loop) untuk menampilkan nilai Error disetiap kombinasi nilai Variabel N 
# dan juga nilai parameter K pada KNN (Studi kasus breast cancer)?

result <- data.frame(c(0),c(0),c(0))
colnames(result) <- c("N", "k", "error_rate")

# jumlah N dimulai dari 2, karena minimal dua fitur
for (i in 2:30){
  N = i
  set.seed(1234)
  ind<-sample(2, nrow(datasets), replace = TRUE, prob=c(0.67,0.33))
  datasets.trainset <- datasets[ind==1,(3:(2+N))]
  datasets.testset <- datasets[ind!=1,(3:(2+N))]
  datasets.trainlabel <- datasets[ind==1,2]
  datasets.testlabel <- datasets[ind!=1,2]
  for (j in 1:50) {
    datasets.pred <- knn(train = datasets.trainset, test = datasets.testset, cl = datasets.trainlabel, k=31)
    tbl_test <- table(datasets.pred, datasets.testlabel)
    #tbl_test
    err_rate <- (tbl_test[1, 2] + tbl_test[2, 1])/length(datasets.pred)
    #err_rate
    result <- rbind(result, c(i,j,err_rate))
  }
}

result <- result[2:length(result$error_rate),1:3]
plot(result$error_rate, type = "l")

min(result$error_rate)
#hasil kombinasi N dan k yang menunjukkan error_rate paling minimum
result2 <- result[result$error_rate == min(result$error_rate),1:3]

plot(result2$error_rate, type = "l")
