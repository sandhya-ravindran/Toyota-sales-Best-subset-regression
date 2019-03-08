
# Install Packages

install.packages('corrgram')
install.packages('forecast')
install.packages('leaps')
install.packages('ggplot2')

# Calling Packages

library('corrgram')
library('forecast')
library('leaps')
library('ggplot2')

# Read and find correlation

car.df <- read.csv("c:/Users/Sandhya Ravindran/Desktop/r_wd/ToyotaCorolla.csv")
correl <- cor(car.df[sapply(car.df, is.numeric)])

corrgram(correl)

selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
car.df.h <- car.df[,c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)]
correl2 <- cor(car.df.h[sapply(car.df.h, is.numeric)])
corrgram(correl2)

# set seed for reproducing the partition

set.seed(1)

# partition data

train.index <- sample(c(1:1000), 600)  
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]


car.lm <- lm(Price ~., data = train.df)
options(scipen = 999)
summary(car.lm)

car.lm.pred <- predict(car.lm, valid.df)
all.residuals <- valid.df$Price - car.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
df <- augment(car.lm)

#all.residuals
ggplot(df, aes(x = .fitted, y = .resid, color = 'red')) + geom_point()+geom_smooth(aes(colour = fitted(car.lm), fill = fitted(car.lm)))+geom_hline(yintercept=0)

car.lm.pred <- predict(car.lm, valid.df)
options(scipen=999, digits = 0)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)

accuracy(car.lm.pred, valid.df$Price)

# run exhaustive serach

search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)

# show models

sum$which

# show metrics

sum$rsq
sum$adjr2
sum$Cp

#backward step()

car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

# null model
car.lm.null <- lm(Price~1, data = train.df)

# run forward regression

car.lm.step <- step(car.lm.null, scope=list(lower=car.lm.null, upper=car.lm), direction = "forward")
summary(car.lm.step)
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)  
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)
