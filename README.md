# Statistical-methods

## Examining the Central Limit Theorem in Poisson and Chi-Square Distributions

## 1) Poisson distribution with parameter 2

As shown in the graphs below, in the case of 200 repetitions, the histogram is relatively spread out because the number of repetitions is low, and the distribution of the means is not yet fully normal.

In the second case, with 2000 repetitions, the histogram becomes more bell-shaped compared to the first case. The fluctuations are smaller, and the overall shape is closer to a normal distribution.

In the third case, with 20000 repetitions, the histogram becomes completely symmetric and bell-shaped, clearly resembling a normal distribution—exactly what the Central Limit Theorem predicts.

---
```R
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
```

