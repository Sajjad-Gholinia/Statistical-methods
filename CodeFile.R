
#1 
#1.1  

n1 = 20
parametr = 2
par(mfrow = c(1, 3))  
#-------------------------------------------------------------------------------
rep11 = 200
means11 = numeric(rep11)
for (i in 1:rep11) {
  sample11 = rchisq(n1, parametr)
  means11[i] = mean(sample11)}
hist(means11, col = "skyblue", probability = TRUE,
     main = "rep = 200", xlab = "Average of samples")
curve(dnorm(x, mean = mean(means11), sd = sd(means11)),
      col = "red", lwd = 2, add = TRUE)

#-------------------------------------------------------------------------------

rep12 = 2000
means12 = numeric(rep12)
for (i in 1:rep12) {
  sample12 = rchisq(n1, parametr)
  means12[i] = mean(sample12)}
hist(means12, col = "skyblue", probability = TRUE,
     main = "rep = 2000", xlab = "Average of samples")
curve(dnorm(x, mean = mean(means12), sd = sd(means12)),
      col = "red", lwd = 2, add = TRUE)



#-------------------------------------------------------------------------------
rep13 = 20000
means13 = numeric(rep13)
for (i in 1:rep13) {
  sample13 = rchisq(n1, parametr)
  means13[i] = mean(sample13)}
hist(means13, col = "skyblue", probability = TRUE,
     main = "rep = 20000", xlab = "Average of samples")
curve(dnorm(x, mean = mean(means13), sd = sd(means13)),
      col = "red", lwd = 2, add = TRUE)




#1.2

n2 = 20
df = 2
par(mfrow = c(1, 3))  
#-------------------------------------------------------------------------------
rep21 = 200
means21 = numeric(rep21)

for (i in 1:rep21) {
  sample21 = rchisq(n2, df)
  means21[i] = mean(sample21)
}
hist(means21, col = "lightgreen", probability = TRUE,
     main = "rep = 200", xlab = "Average of samples")
curve(dnorm(x, mean = mean(means21), sd = sd(means21)),
      col = "red", lwd = 2, add = TRUE)

#-------------------------------------------------------------------------------
rep22 = 2000
means22 = numeric(rep22)

for (i in 1:rep22) {
  sample22 = rchisq(n2, df)
  means22[i] = mean(sample22)
}
hist(means22, col = "lightgreen", probability = TRUE,
     main = "rep = 2000", xlab = "Average of samples")
curve(dnorm(x, mean = mean(means22), sd = sd(means22)),
      col = "red", lwd = 2, add = TRUE)



#-------------------------------------------------------------------------------
rep23 = 20000
means23 = numeric(rep23)

for (i in 1:rep23) {
  sample23 = rchisq(n2, df)
  means23[i] = mean(sample23)
}
hist(means23, col = "lightgreen", probability = TRUE,
     main = "rep = 20000", xlab = "Average of samples")
curve(dnorm(x, mean = mean(means23), sd = sd(means23)),
      col = "red", lwd = 2, add = TRUE)




#2
  
CI = function(data) {
  n_CI = length(data)
  x_bar = mean(data)
  sigma = 25
  z = 1.96  
  se = sigma / sqrt(n_CI)
  lower = x_bar - z * se
  upper = x_bar + z * se
  return(c(lower, upper))}
sample_data = c(70,78,80,69,81,75,71,83,90,77,88,74,89,67,85)
CI(sample_data)



#3

weights4 = c(52, 48, 47, 53, 58, 56, 53, 49, 48, 50)
n4 = 10
x_bar4 = mean(weights4)
s = sd(weights4)
mu = 52 
(t = (x_bar4 - mu) / (s / sqrt(n4)))
(t_critical = qt(0.05, df = n4 - 1))




#4

data5 = c(301, 300, 298, 296, 303, 305, 301, 299, 297, 298)
n5 = 10
s2 = var(data5)  
sigma_sq = 8
(chi_sq = (n5 - 1) * s2 / sigma_sq)
(chi_critical = qchisq(0.05, df = n5 - 1))
Y = c(641, 633, 651, 666, 688, 680)
X = c(214, 215, 216, 217, 219, 221)
plot(X,Y,col = "black" , pch =16)
cor(X,Y)



  

#5

n3 = 30
lambda = 3
par(mfrow = c(2, 3)) 
#-------------------------------------------------------------------------------
rep31 = 100
means31 = numeric(rep31)
medians31 = numeric(rep31)
for (i in 1:rep31) {
  sample31 = rexp(n3, rate = lambda)
  means31[i] = mean(sample31)
  medians31[i] = median(sample31)}
hist(means31, col = "lightblue", main = "Mean , rep = 100")
hist(medians31, col = "lightblue", main = "Median , rep = 100")
#-------------------------------------------------------------------------------
rep32 = 1000
means32 = numeric(rep32)
medians32 = numeric(rep32)
for (i in 1:rep32) {
  sample32 = rexp(n3, rate = lambda)
  means32[i] = mean(sample32)
  medians32[i] = median(sample32)}
hist(means32, col = "lightgreen", main = "Mean , rep = 1000")
hist(medians32, col = "lightgreen", main = "Median , rep = 1000")
#-------------------------------------------------------------------------------
rep33 = 10000
means33 = numeric(rep33)
medians33 = numeric(rep33)
for (i in 1:rep33) {
  sample33 = rexp(n3, rate = lambda)
  means33[i] = mean(sample33)
  medians33[i] = median(sample33)}
hist(means33, col = "lightyellow", main = "Mean , rep = 10000")
hist(medians33, col = "lightyellow", main = "Median , rep = 10000")









