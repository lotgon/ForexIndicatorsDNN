
Tx <- 1 # NUmber of input ticks
Nx <- 3 # NUmber of features (ask, bid, duration last tick)
Ty <- Tx #output number
minTickDuration <- 0.01 #ticks with shorter duration will be ignored
maxTickDuration <- 100 #reduce duration all long ticks to 100

sdForexRates <- 0.007642418
batch_size <- 5000
