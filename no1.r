# Tuliskan kembali poin-poin pertanyaan pada modul praktikum berikut
# dengan jawaban berupa analisis dan penjelasan yang anda pahami. 
# (dikerjakan dan dikumpulkan dalam format word file)

library(help="datasets")
iris<-read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = TRUE)

# 1. Prepare Your Work Directory and Get Data
# Bagaimana jika header di set menjadi False, dan bagaimana jika diset menjadi True? 
# jawab : data pada baris pertama menjadi colnames dari dataframe iris

# Jika tanpa ada header, bagaimana memunculkan header yang sesuai dengan kita inginkan.
# jawab : membuat colnames sendiri, contoh :

colnames(iris) <- c("sepal.length", "sepal.width", "petal.length", "petal.width", "class")


library(ggvis)
# 2. Know Your Data (optional) 

iris %>% ggvis(~sepal.length, ~sepal.width, fill =~class) %>%
layer_points()
# Lakukan analisis terhadap dua variabel sepal. Length, dan sepal.width untuk ketiga setiap ketiga kelas yang diperlihatkan oleh grafik?
# jawab : 
# berdasarkan hasil plot diatas, terlihat bahwa untuk kelas iris-setosa hanya satu yang tidak terklasifikasi dengan benar,
# sedangkan untuk irsh-versicolor dan iris-virginica masih bercampur sehingga belum bisa dibedakan dengan jelas.


iris %>% ggvis(~petal.length, ~petal.width, fill =~class) %>%
layer_points()
# Lakukan analisis terhadap dua variabel petal. length, dan petal.width untuk ketiga setiap ketiga kelas yang diperlihatkan oleh grafik. Apa hasilnya?
# jawab : 
# untuk kelas iris-setosa di klasifikasikan dengan baik untuk semua data
# sedangkan untuk iris-virginica dan iris-versicolor masih terdapat beberapa data yang tercampur, namun sebagian besar sudah terklasfikasi dengan baik.


# 3. Prepare Your Data
prepare<-function(x){
  a=(x-min(x))
  b= max(x)-min(x)	
  return(a/b)
}

# Fungsi apakah prepare(x) itu? 
# jawab :
# fungsi prepare adalah fungsi untuk membuat data lebih konsisten, yakni dengan cara melakukan normalisasi
# contoh untuk normalisasi data irish pada kolom sepal.width

d1 <- iris$sepal.length
d2 <- prepare(d1)
y1 <- max(d1)

# bandingkan hasil kedua plot
# data sebelum di normalisasi
barplot(d1[0:50])
# data setelah di normalisasi
barplot(d2[0:50])


# Kenapa diperlukan untuk memastikan bahwa data yang disiapkan dan digunakan tidak over-emphasise?
# jawab : agar sebaran data memiliki perbedaan nilai yang jauh, sehingga memudahkan dalam klasifikasi

# Bagaimana menerapkan alforitme untuk fungsi  tersebut? 
# jawab : seperti diatas, menggunakan algoritma normalisasi, data yang baru adalah data yang telah melalui proses normalisasi

set.seed(1234)
ind<-sample(2, nrow(iris), replace = TRUE, prob=c(0.67,0.33))
iris.trainset <- iris[ind==1,(1:4)]
iris.testset <- iris[ind!=1,(1:4)]
iris.trainlabel <- iris[ind==1,5]
iris.testlabel <- iris[ind!=1,5]

# 4. Building the Clasifier

any(grepl("class", installed.packages()))

library(class) #install.packages("class")
'?' (knn)
install.packages("gmodels")
library (gmodels)

# 5. Evaluation your model
# untuk k = 11
iris.pred <- knn(train = iris.trainset, test = iris.testset, cl = iris.trainlabel, k=13)
table(iris.pred, iris.testlabel)
CrossTable(x=iris.testlabel, y=iris.pred, prop.chisq=FALSE)

# Cobakan untuk k yang berbeda, k=5,7,9,11,13
# Perlihatkan dengan matriks konfusi untuk nilai k tersebut

