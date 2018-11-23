require(keras)
source("parameters.R")

CreateModelFunctional <- function () {
  longInput <- layer_input(shape = c(NULL, Tx, Nx))
  longOutput <- longInput %>%
    #layer_conv_1d(1, 1, padding = "valid", strides = 1, use_bias = F) %>%
    layer_flatten() %>%
    layer_dense(units = Ty, use_bias = FALSE)#, activation = 'sigmoid')

  model <- keras_model(longInput, longOutput)
  summary(model)
  model %>% compile(
    loss = 'mean_absolute_error',
    optimizer = optimizer_adamax(lr = 0.001)
  )
  model
}

