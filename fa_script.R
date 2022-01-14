### FACTOR ANALYSIS


rm <- matrix(red, nrow = nrow(red)*ncol(red), ncol =1, byrow = TRUE)
gm <- matrix(green, nrow = nrow(red)*ncol(red), ncol =1, byrow = TRUE)
bm <- matrix(blue, nrow = nrow(red)*ncol(red), ncol =1, byrow = TRUE)

matrix_f <- cbind(rm,gm,bm)


fa <- factanal(matrix_f,factors=1, method = 'mle')

