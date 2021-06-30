if(!require(gdata)) install.packages("gdata", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(dplyr)
library(gdata)  
temptable <- read.table("rating_complete.csv", 
                        header = TRUE, sep = ",")

library(caret)
set.seed(1)
test_index <- createDataPartition(y = temptable$Rating, times = 1, p = 0.1, 
                                  list = FALSE)
train_set <- temptable[-test_index,]
test_set <- temptable[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "ProductId") %>%
  semi_join(train_set, by = "username")
# Analysis 
head(temptable)
str(temptable)
mean(temptable$Rating)
temptable %>% group_by(username) %>% summarize(count=n()) %>% arrange(desc(count))   
summary(temptable)
mean(temptable$Rating)
temptable %>% count(username) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "yellow", fill="black") + 
  scale_x_log10() + 
  ggtitle("Histogram - Number of Users")
temptable %>% count(ProductId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "yellow", fill="black") + 
  scale_x_log10() + 
  ggtitle("Histogram - Anime")
ggplot(temptable, aes(Rating)) + geom_histogram(binwidth=0.5, color="black", fill="yellow") + ggtitle("Histogram of Ratings by Count of Votes")
#Define function for RSME
RMSE <- function(Realratings, Predictedratings){
  sqrt(mean((Realratings - Predictedratings)^2))
}


#Ratings average
murating <- mean(temptable$Rating)
murating

#First Model
FirstRMSE <- RMSE(test_set$Rating, murating)
FirstRMSE
Comparativetable <- tibble(Method = "First Model = Mu Rating", RMSE = FirstRMSE)
Comparativetable %>% knitr::kable()

Second Model Anime Interaction Bias
newmu <- mean(train_set$Rating) 
newmu
prodavgs <- train_set %>% 
  group_by(ProductId) %>% 
  summarize(b_i = mean(Rating - newmu))


str(prodavgs)
head(prodavgs)
qplot(b_i, data = prodavgs, bins = 20, color = I("yellow"))


Predratings <- newmu + test_set %>% 
  left_join(prodavgs, by = 'ProductId') %>%
  pull(b_i)

head(Predratings)
SecondRMSE <- RMSE(test_set$Rating, Predratings)
SecondRMSE
Comparativetable <- bind_rows(Comparativetable, tibble(Method = "Second Model = LEAST SQUARE Product (Anime) Bias", RMSE = SecondRMSE))
Comparativetable %>% knitr::kable()

#Third Model User Interaction Bias
train_set %>% 
  group_by(username) %>% 
  filter(n()>=10) %>%
  summarize(biasuser = mean(Rating)) %>%
  ggplot(aes(biasuser)) + 
  geom_histogram(bins = 50)

userinteraction <- train_set %>% 
  left_join(prodavgs, by='ProductId') %>%
  group_by(username) %>%
  summarize(biasuser = mean(Rating - newmu - b_i))

head(userinteraction)

Predictedratings <- test_set %>% 
  left_join(prodavgs, by='ProductId') %>%
  left_join(userinteraction, by = 'username') %>%
  mutate(predictor = newmu + b_i + biasuser) %>%
  pull(predictor)

head(Predictedratings)

ThirdRMSE <- RMSE(Predictedratings, test_set$Rating)
ThirdRMSE
Comparativetable <- bind_rows(Comparativetable, tibble(Method = "Third Model  = LEAST SQUARE Anime + Username bias", RMSE = ThirdRMSE))
Comparativetable %>% knitr::kable()

#4th model
lambda <- 2
mu <- mean(train_set$Rating)
prod_reg_avgs <- train_set %>% 
  group_by(ProductId) %>% 
  summarize(b_i = sum(Rating - mu)/(n()+lambda), n_i = n()) 

predicted_ratings <- test_set %>% 
  left_join(prod_reg_avgs, by = "ProductId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$Rating)

lambdas <- seq(0, 10, 1)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$Rating)
  
  b_i <- train_set %>% 
    group_by(ProductId) %>%
    summarize(b_i = sum(Rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="ProductId") %>%
    group_by(username) %>%
    summarize(b_u = sum(Rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "ProductId") %>%
    left_join(b_u, by = "username") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$Rating))
})

qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
FourthRMSE <- min(rmses)
FourthRMSE
Comparativetable <- bind_rows(Comparativetable, tibble(Method = "Fourth Model = PENALTY ESTIMATE Regularized Anime + Username", RMSE = FourthRMSE))
Comparativetable %>% knitr::kable()

#Validation on Validation Set
library(recosystem)
set.seed(1, sample.kind="Rounding")
tr_reco <- with(train_set, data_memory(user_index = username, item_index = ProductId, rating = Rating))
validation_reco <- with(test_set, data_memory(user_index = username, item_index = ProductId, rating = Rating))
r <- Reco()

r$train(tr_reco)

final_reco <- r$predict(validation_reco, out_memory())
Validated_RMSE <- RMSE(final_reco, test_set$Rating)
Validated_RMSE
Comparativetable <- bind_rows(Comparativetable, tibble(Method = "Fifth Model = OPTIMIZED MODEL on Validation Set", RMSE = Validated_RMSE))
Comparativetable %>% knitr::kable()
