shell("cls")

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

# if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
# if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

library(tidyverse)
library(caret)
library(data.table)
library(readxl)
library(knitr)
library(htmlTable)


#Download Movielens dataset
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


#Inspect the Movielens data
movielens %>% as_tibble()
# movielens %>% head() %>% htmlTable()


movielens %>% 
  summarise(n_users=n_distinct(userId),
            n_movies=n_distinct(movieId))

movielens %>% 
  group_by(movieId) %>% 
  summarise( n=n()) %>% 
  ggplot(aes(x=n))+
  geom_histogram(color="black",fill="grey")+
  scale_x_continuous(trans='log10')+
  ggtitle("Movies")


movielens %>% 
  group_by(userId) %>% 
  summarise( n=n()) %>% 
  ggplot(aes(x=n,title="Users"))+
  geom_histogram(color="black",fill="light blue")+
  scale_x_continuous(trans='log10')+
  ggtitle("Users")

# Validation set will be 10% of MovieLens data

 set.seed(999)

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.10, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

  rm(dl, ratings, movies, test_index, temp, movielens, removed)
 
 RMSE <- function(true_ratings,predicted_ratings){
   sqrt(mean((true_ratings-predicted_ratings)^2))
 }


#----------------
head(edx)
head(validation)

# write.csv(edx,file="EDX - Train.csv",col.names=TRUE)
# write.csv(validation,file="Validation - Test.csv",col.names=TRUE)

#-------
# Movie effect Model: Mu plus Movie factor

train_set <- edx
test_set <- validation

#Ave across all movies: mu_hat
mu <- mean(train_set$rating)
mu 

RMSE_overall <- RMSE(mu,test_set$rating)

#Ave by movie ID
m_ave <- train_set %>% 
  group_by(movieId) %>% 
  summarise(b_i=mean(rating - mu))

m_ave %>% 
  ggplot(aes(x=b_i))+
  geom_histogram(bins=10,color="black",fill="blue")+
  ggtitle(expression(paste("Movie Factor (Ave Rating -" , hat(mu),")")))


pred_rat <- mu + test_set %>% 
  left_join(m_ave,by='movieId') %>% 
  pull(b_i)

#RMSE by Movie ID
mod_1_rmse <- RMSE(pred_rat,test_set$rating)
mod_1_rmse



#--------
#Results
rmse_res <- tibble(method = "Overall Average",
                   RMSE=RMSE_overall)
rmse_res <- bind_rows(rmse_res,
                      tibble(method = "Movie effect model",
                             RMSE=mod_1_rmse))

rmse_res



#-------
# User and Movie effect Model: Mu plus Movie and User factors

#Avg ratings by user with more than 100 ratings
train_set %>% 
  group_by(userId) %>% 
  summarise(b_u=mean(rating)) %>% 
  filter(n()>=100) %>% 
  ggplot(aes(b_u))+
  geom_histogram(color="black",fill="light blue")+
  ggtitle("UserId Factor")


#Ave by User ID
u_ave <- train_set %>% 
  left_join(m_ave, by="movieId") %>% 
  group_by(userId) %>% 
  summarise(b_u=mean(rating - mu - b_i))

pred_rat2 <-test_set %>% 
  left_join(m_ave,by='movieId') %>% 
  left_join(u_ave,by='userId') %>% 
  mutate(pred=mu + b_i + b_u) %>% 
  pull(pred)
  

mod_2_rmse <- RMSE(pred_rat2,test_set$rating)
mod_2_rmse
  

rmse_res <- bind_rows(rmse_res,
                      tibble(method = "User and Movie effect model",
                             RMSE=mod_2_rmse))
  
rmse_res
