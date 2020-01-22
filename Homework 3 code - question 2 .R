# get the data
library(readr)
# removed a row with label units and transformed to csv file format

data2 <- read.delim("C:/Users/kiqbal2/Desktop/canon.txt")


# create two groups for attitudnal and health 

attitudnal <- data2 [, 6:8,10]
health <- data2 [, 2:5, 9]

#CCA summart of data
library(psych)
a = set.cor(x=6:8,10 , y =2:5,9 , data = data2)

print(a)


# the test statistic (Wilks)
(1 - a$cancor2[1]) * (1 - a$cancor2[2]) * (1 - a$cancor2[3])
## [1] 0.6963021
# df (n - 1) where n = 465
a$df[1] + a$df[2]
## [1] 465
print("p-value from txt, Sig. F = 0.000")
## [1] "p-value from txt, Sig. F = 0.000"
# ct = corr.test(attitudnal, health)
# ct$p



# the test statistic - part b
(1 - a$cancor2[2]) * (1 - a$cancor2[3])
a$df[1] + a$df[2]
## [1] 465
print("p-value from txt, Sig. F = 0.000")
## [1] "p-value from txt, Sig. F = 0.000"






library(CCA)
library(ggplot2)
library(GGally)
ggpairs(attitudnal)
ggpairs(health)

# correlations between the two groups of variables
matcor(attitudnal,health)


# display the canonical correlations
cc1 <- cc(attitudnal, health)
cc1$cor

cc1[3:4]




# question 2 

# compute canonical loadings
cc2 <- comput(attitudnal, health, cc1)

# display canonical loadings/latent variables
cc2[3:6]

# tests of canonical dimensions
ev <- (1 - cc1$cor^2)

n <- dim(attitudnal)[1]
p <- length(attitudnal)
q <- length(health)
k <- min(p, q)
m <- n - 3/2 - (p + q)/2

w <- rev(cumprod(rev(ev)))

# initialize
d1 <- d2 <- f <- vector("numeric", k)

for (i in 1:k) {
  s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
  si <- 1/s
  d1[i] <- p * q
  d2[i] <- m * s - p * q/2 + 1
  r <- (1 - w[i]^si)/w[i]^si
  f[i] <- r * d2[i]/d1[i]
  p <- p - 1
  q <- q - 1
}

pv <- pf(f, d1, d2, lower.tail = FALSE)
(dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))

