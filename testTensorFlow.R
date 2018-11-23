library(tensorflow)

time <- Sys.time()
floatType <- "float32"
p <- 2L
X <- tf$placeholder(floatType, shape = shape(NULL, p), name = "x-data")
Y <- tf$placeholder(floatType, name = "y-data")
W <- tf$Variable(tf$zeros(list(p, 1L), dtype=floatType))
b <- tf$Variable(tf$zeros(list(1L), dtype=floatType))
Y_hat <- tf$add(tf$matmul(X, W), b)
cost <- tf$reduce_mean(tf$square(Y_hat - Y))
generator <- tf$train$GradientDescentOptimizer(learning_rate=0.01)
optimizer <- generator$minimize(cost)

session <- tf$Session()
session$run(tf$global_variables_initializer())

set.seed(1443)
n <- 10^5
x <- matrix( replicate(p, sort(scale((rpois(n, 10^6))))) , nrow = n )
y <- matrix((x[,1]+x[,2])/2)

#shuffledIndex<-sample(1:(nrow(x)))
#x <- x[shuffledIndex,]
#y <- y[shuffledIndex,,drop=FALSE]

i<-1
batch_size <- 2048
epoch_number  <- 300
iterationNumber <- n*epoch_number / batch_size

while (iterationNumber > 0) {
  feed_dict <- dict(X = x[i:(i+batch_size-1),, drop = F], Y = y[i:(i+batch_size-1),, drop = F])
  session$run(optimizer, feed_dict = feed_dict)
  
  i <- i+batch_size
  if( i > n-batch_size)
    i <- i %% batch_size 

  iterationNumber <- iterationNumber - 1
}
r_model <- lm(y ~ x)
tf_coef <- c(session$run(b), session$run(W))
r_coef  <- r_model$coefficients
print(rbind(tf_coef, r_coef))

Sys.time() - time
