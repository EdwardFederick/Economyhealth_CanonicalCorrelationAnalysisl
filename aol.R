library(readxl)
aol <- read_excel("C:/Users/Edward/Documents/Multivariate/aol.xlsx", sheet = "Sheet1")
View(aol)

library(expm)

#Uji Asumsi

#Distirbusi normal multivariate
library(MVN)
result <- mvn(data = aol[2:7], mvnTest = "mardia")
print(result) #Tidak distribusi normal multivariate

qqPlot <- mvn(data=aol[2:7], multivariatePlot = "qq")

#Log transformasi
aol_new = log(aol[2:7] + 1)
result_new <- mvn(data = aol_new, mvnTest = "mardia")
print(result_new)

qqPlot <- mvn(data=aol_new, multivariatePlot = "qq")


#Multikolinearitas
model = lm(Provinsi ~., data = aol_new)
summary(model)

library(car)

vif(model) #Tidak terjadi multikolinearitas karena VIF nya tidak ada yang lebih dari 10

#Linearitas
library(lmtest)
reset_result <- resettest(model, power = 2:3, type = "fitted")
print(reset_result)


#Koefisien korelasi kanonik
install.packages("CCA")
library(CCA)

# Pisahkan data menjadi dua set
X <- data[, 2:4]
Y <- data[, 5:7]

# Hitung koefisien korelasi kanonik
cancor_result <- cancor(X, Y)

# Tampilkan hasil
print(cancor_result)

# Tampilkan koefisien korelasi kanonik
print(cancor_result$cor)

# Tampilkan koefisien vektor kanonik untuk set X
print(cancor_result$xcoef)

# Tampilkan koefisien vektor kanonik untuk set Y
print(cancor_result$ycoef)



matrix_corr <- cor(data[, 2:7], method = c("pearson"))
matrix_corr

rho11 <- matrix_corr[1:3, 1:3]
rho11

rho12 <- matrix_corr[1:3, 4:6]
rho12

rho21 <- matrix_corr[4:6, 1:3]
rho21

rho22 <- matrix_corr[4:6, 4:6]
rho22

#1. hitung rho11 ^ -1/2
rho11_sqrtm <- solve(sqrtm(rho11))
rho11_sqrtm

#2. hitung rho22 ^ -1
rho22_inv <- solve(rho22)
rho22_inv

#3. hitung rho22 ^ -1/2
rho22_sqrtm <- solve(sqrtm(rho22))
rho22_sqrtm

#4. hitung 11 ^ -1
rho11_inv <- solve(rho11)
rho11_inv

matA <- rho11_sqrtm %*% rho12 %*% rho22_inv %*% rho21 %*% rho11_sqrtm
matB <- rho22_sqrtm %*% rho21 %*% rho11_inv %*% rho12 %*% rho22_sqrtm

matA
matB

eigenA <- eigen(matA)
eigenA

eigenB <- eigen(matB)
eigenB

sqrt_kanonik <- sqrt(eigenA$values)
sqrt_kanonik

U1_1 <- eigenA$vectors[1:3, 1] %*% rho11_sqrtm
U1_1

U1_2 <- eigenA$vectors[1:3, 2] %*% rho11_sqrtm
U1_2

U1_3 <- eigenA$vectors[1:3, 3] %*% rho11_sqrtm
U1_3

V1_1 <- eigenB$vectors[1:3, 1] %*% rho22_sqrtm
V1_1

V1_2 <- eigenB$vectors[1:3, 2] %*% rho22_sqrtm
V1_2

V1_3 <- eigenB$vectors[1:3, 3] %*% rho22_sqrtm
V1_3


cor(as.vector(U1_1), as.vector(V1_1))

cancor_result = cancor(data[, 2:4], data[, 5:7])
cancor_result

weight_x = cancor_result$xcoef
weight_y = cancor_result$ycoef

weight_x
weight_y

can_loads = comput(X,Y,cc(X,Y))

can_loads$corr.X.xscores[,1]
can_loads$corr.Y.yscores[,1]
can_loads$corr.X.yscores[,1]
can_loads$corr.Y.xscores[,1]

library(CCP)

canonical_correlations <- cancor_result$cor

# Perform Wilks' Lambda test using CCP package
n <- nrow(aol)
p <- 3
q <- 3
wilks_result <- p.asym(canonical_correlations, n, p, q, tstat = "Wilks")

# Print the results
wilks_result



