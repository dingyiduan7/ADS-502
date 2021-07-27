library(e1071)
ma <- data.frame(matrix(c(1,1,0,0,1,0,1,0), nrow = 4))
colnames(ma) <- c("factor1", "factor2")

x <- c("x1", "x2", "x3", "x4", "x5", "x6")
df <- data.frame(matrix(ncol = 6, nrow = 4))
colnames(df) <- x
df$x1 <- 1
df$x2 <- sqrt(2)*ma$factor1
df$x3 <- sqrt(2)*ma$factor2
df$x4 <- sqrt(2)*ma$factor1 * ma$factor2
df$x5 <- ma$factor1 ^ 2
df$x6 <- ma$factor2 ^ 2
df$label <- c(-1, 1, 1, -1)

svm_model <- svm(label~x1+x2+x3+x4+x5+x6, 
                 df, type='C-classification', 
                 kernal='linear', scale=F)

# label=w1*x1+...+w6*x6+b=0
# w1...w6
w <- t(svm_model$coefs) %*% svm_model$SV
# b
svm_model$rho

