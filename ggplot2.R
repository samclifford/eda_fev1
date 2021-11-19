#       _/_/      _/_/    _/_/_/      _/   
#    _/    _/  _/    _/        _/  _/_/    
#       _/    _/    _/    _/_/      _/     
#    _/      _/    _/        _/    _/      
# _/_/_/_/    _/_/    _/_/_/    _/_/_/   

library(tidyverse)
library(data.table)

# Activity 2
# load in the data set, ensuring that `id` is read as a factor
fev1 <- fread("fev1.csv", colClasses = list(factor = 'id'))
fev1 <- fev1[ , id := fct_inorder(id)]

fev1_sampled <- fev1[ , .N, by = id
                     ][N > 6
                       ][id %in% sample(id, 20)
                         ][ , id := fct_drop(id)
                           ][ , .(id, N)]

... <- merge(fev1, fev1_sampled)

# Activity 3

# 3a
ggplot(data = ...,
       aes(x = age, y = FEV1)) + 
    geom_...()

fev1_plot3a

# 3b

... + 
    xlab(...) + 
    ... +
    theme_...()

fev1_plot3b

# 3c

...

# Activity 4 

...

# Activity 5

... # count number of times id appears

# Activity 6

ggplot(data = ...,
       aes(x = ..., y = ...)) +
    geom_...(aes(... = ...)) +
    ...

# Activity 7

library(mgcv)
model <- gamm(data = ..., FEV1 ~ ...)

pred  <- data.table(age = seq(min(...$age),
                              max(...$age),
                              length.out = 101))

pred[, FEV1 := predict(model$gam, pred)]
