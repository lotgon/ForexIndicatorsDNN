library(keras)
time <- Sys.time()
set.seed(1443)
n <- 10^5
x <- data.frame(x1=sort(scale(rpois(n, 1000000))), x2 =sort(scale(rpois(n, 1000000))))
y <- data.frame(y1=(x$x1+x$x2)/2)

shuffledIndex<-sample(1:n)
x <- x[shuffledIndex,]
y <- y[shuffledIndex,,drop=F]

model <- keras_model_sequential() 
model %>% layer_dense(units = 1, input_shape = 2)
model %>% compile(loss = 'mean_squared_error', optimizer = optimizer_sgd())

summary(model)
model %>% fit(as.matrix(x), as.matrix(y), epochs = 1000, batch_size=10000)
model$get_weights()
Sys.time() - time

############################
set.seed(1443)
x <- array(0, dim=c(10^5, 1, 3))
y <- array(0, dim=c(10^5, 1))
x[,,1] <- scale(sort(rpois(10^5, 10^6)))
x[,,2] <- scale(sort(rpois(10^5, 10^6)))
x[,,3] <- scale(sort(rpois(10^5, 10^6)))

y[,1] <- (x[,1,1]+x[,1,2])/2

input_layer <- layer_input(shape=c(NULL, 1, 3))
output_layer <- input_layer %>% layer_conv_1d(1, 1, use_bias = F) %>% layer_flatten()
model <- keras_model(input_layer, output_layer)

model %>% compile(loss = 'mean_squared_error', optimizer = optimizer_sgd())

summary(model)
model %>% fit(x, y, epochs = 100, batch_size=16)
model$get_weights()

#################################################
library(tensorflow)

# 'X' and 'Y' are placeholders for input data, output data.
# We'll fit a regression model using 3 predictors, so our
# input matrix 'X' will have three columns.
p <- 2L
X <- tf$placeholder("float64", shape = shape(NULL, p), name = "x-data")
Y <- tf$placeholder("float64", name = "y-data")

# Define the weights for each column in X. Since we will
# have 3 predictors, our 'W' matrix of coefficients will
# have 3 elements. We use a 3 x 1 matrix here, rather than
# a vector, to ensure TensorFlow understands how to perform
# matrix multiplication on 'W' and 'X'.
W <- tf$Variable(tf$zeros(list(p, 1L), dtype="float64"))

# Define the model (how estimates of 'Y' are produced)
Y_hat <- tf$matmul(X, W)

# Define the cost function.
# We seek to minimize the mean-squared error.
cost <- tf$reduce_mean(tf$square(Y_hat - Y))

# Define the mechanism used to optimize the loss function.
# Although normally we'd just use ordinary least squares,
# we'll instead use a gradient descent optimizer (since, in
# a more typical learning situation, we won't have an easy
# mechanism for directly computing the values of coefficients)
generator <- tf$train$GradientDescentOptimizer(learning_rate = 0.01)
optimizer <- generator$minimize(cost)

# Initialize a TensorFlow session for our regression.
init <- tf$global_variables_initializer()
session <- tf$Session()
session$run(init)

n <- 2500
set.seed(1443)
x <- matrix( c((sort(rpois(n, 1000000))), (sort(rpois(n, 1000000)))), ncol = 2 )
y <- matrix((x[,1]+x[,2])/2)


# Repeatedly run optimizer until the cost is no longer changing.
# (We can take advantage of this since we're using gradient descent
# as our optimizer)
feed_dict <- dict(X = x, Y = y)
epsilon <- .Machine$double.eps
last_cost <- Inf
while (TRUE) {
  session$run(optimizer, feed_dict = feed_dict)
  current_cost <- session$run(cost, feed_dict = feed_dict)
  if (last_cost - current_cost < epsilon) break
  last_cost <- current_cost
}

# Generate an R model and compare coefficients from each fit
r_model <- lm(y ~ x)

# Collect coefficients from TensorFlow model fit
tf_coef <- c( 0, session$run(W))
r_coef  <- r_model$coefficients

print(rbind(tf_coef, r_coef))










#grads <- k_gradients(model$output, model$input)
#get_grads <- k_function(list(model$input), grads)
#get_grads(list(x[1,,]))

K  <- backend()
wt <- model$trainable_weights
grad <- K$gradients( model$output, model$input)[[1]]
#lp <- K$learning_phase()

sess=K$get_session()
sess$run(wt)
sess$run( grad, feed_dict = dict(input_layer=x[2,,,drop=F]))

          