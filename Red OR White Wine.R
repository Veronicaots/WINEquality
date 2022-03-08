## Made Ots
## 09.March 2022
## Predicting the Wine Quality
## HarvardX: PH125.9x - Capstone Project
## https://github.com/Veronicaots/WINEquality

if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dslabs)) install.packages("dslabs")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(scales)) install.packages("scales")
if(!require(DescTools)) install.packages("DescTools")
if(!require (kableExtra)) install.packages("kableExtra")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(data.table)) install.packages("data.table")
if(!require(ROSE)) install.packages("ROSE")

# Load Library
library(knitr)
library(ggrepel)
library(kableExtra)
library(dslabs)
library(caret)
library(tidyverse)
library(Rborist)
library(readr)
library(grid)
library(gridExtra)
library(GGally)
library(rpart)
library(ROSE)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(tidyverse)
library(caret)
library(data.table)
library(ROSE)

##################################################################################
# Load the data set, create the validation set (the final hold-out test set)
##################################################################################

# Load the White Wine

winequality_white <- read_delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", ";", 
                                escape_double = FALSE, col_types = cols(alcohol = col_number(), 
                                                                        chlorides = col_number(), `citric acid` = col_number(), 
                                                                        density = col_number(), `fixed acidity` = col_number(), 
                                                                        `free sulfur dioxide` = col_number(), 
                                                                        pH = col_number(), quality = col_character(), 
                                                                        `residual sugar` = col_number(), 
                                                                        sulphates = col_number(), `total sulfur dioxide` = col_number(), 
                                                                        `volatile acidity` = col_number()), trim_ws = TRUE)

names(winequality_white)<-make.names(names(winequality_white),unique = TRUE)

# Load the Red Wine

winequality_red <- read_delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", ";", 
                              escape_double = FALSE, col_types = cols(alcohol = col_number(), 
                                                                      chlorides = col_number(), `citric acid` = col_number(), 
                                                                      density = col_number(), `fixed acidity` = col_number(), 
                                                                      `free sulfur dioxide` = col_number(), 
                                                                      pH = col_number(), quality = col_character(), 
                                                                      `residual sugar` = col_number(), 
                                                                      sulphates = col_number(), `total sulfur dioxide` = col_number(), 
                                                                      `volatile acidity` = col_number()), trim_ws = TRUE)

names(winequality_red)<-make.names(names(winequality_red),unique = TRUE)



########################### ANALYSE the RATINGS ###########################


# Create train set, test set. In the files the quality of red and the white wine are rated from 0 to 10
test_index_rating <- createDataPartition(winequality_white$quality, times = 1, p = 0.5, list = FALSE)
test_set_rating <- winequality_white[test_index_rating, ]
train_set_rating <- winequality_white[-test_index_rating, ]


# Predict the quality of wine using Rborist. Create train and prediction sets. Validate
train_Rborist_rating <- train(quality ~ .,
                              method = "Rborist",
                              tuneGrid = data.frame(predFixed = 0, minNode = seq(1, 5, 1)),
                              data = train_set_rating)

train_Rborist_rating$bestTune %>% kable("latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "condensed"), position = "center", font_size = 9, full_width = FALSE) %>% footnote(general = "best tune")

plot(train_Rborist_rating)
varImp(train_Rborist_rating, scale = FALSE)

y_hat_Rborist_rating <- predict(train_Rborist_rating, test_set_rating) %>% factor()

# Calculating the accuracy
Accuracy_Rborist_rating <- mean(y_hat_Rborist_rating == test_set_rating$quality)
Accuracy_Rborist_rating 


# Create confusion matrix
test_set_quality_Rborist_rating <- test_set_rating$quality %>% factor()

confusionMatrix(data = y_hat_Rborist_rating, reference = test_set_quality_Rborist_rating)


############################ GOOD, AVERAGE, BAD ##########################

# To simplify the task, we are changing the quality rating to "good" (7-10), "average" (4-6) and "bad" (rating 0-3)
winequality_white_GoodAverageBad <- winequality_white %>% mutate(quality_GoodAverageBad = 
            ifelse(winequality_white$quality == 0, "bad", 
            ifelse(winequality_white$quality == 1, "bad", 
            ifelse(winequality_white$quality == 2, "bad", 
            ifelse(winequality_white$quality == 3, "bad", 
            ifelse(winequality_white$quality == 4, "average", 
            ifelse(winequality_white$quality == 5, "average", 
            ifelse(winequality_white$quality == 6, "average", "good")))))))) %>% dplyr::select(-quality)


# Create train set, test set - "good", "average" and "bad" rating
test_index_GoodAverageBad <- createDataPartition(winequality_white_GoodAverageBad$quality_GoodAverageBad, times = 1, p = 0.5, list = FALSE)
test_set_GoodAverageBad <- winequality_white_GoodAverageBad[test_index_GoodAverageBad, ]
train_set_GoodAverageBad <- winequality_white_GoodAverageBad[-test_index_GoodAverageBad, ]


# Predicting the quality with Rborist on "good", "average" and "bad" rating Create train and prediction
train_Rborist_GoodAverageBad <- train(quality_GoodAverageBad ~ .,
                                      method = "Rborist",
                                      tuneGrid = data.frame(predFixed = 0, minNode = seq(1, 5, 1)),
                                      data = train_set_GoodAverageBad)

train_Rborist_GoodAverageBad$bestTune %>% kable("latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "condensed"), position = "center", font_size = 9, full_width = FALSE) %>% footnote(general = "best tune")

plot(train_Rborist_GoodAverageBad)

varImp(train_Rborist_GoodAverageBad, scale = FALSE)

y_hat_Rborist_GoodAverageBad <- predict(train_Rborist_GoodAverageBad, test_set_GoodAverageBad) %>% factor()

Accuracy_Rborist_GoodAverageBad <- mean(y_hat_Rborist_GoodAverageBad == test_set_GoodAverageBad$quality_GoodAverageBad)

Accuracy_Rborist_GoodAverageBad # Calculate Accuracy

# Create confusion matrix
test_set_quality_Rborist_GoodAverageBad <- test_set_GoodAverageBad$quality_GoodAverageBad %>% factor()

confusionMatrix(data = y_hat_Rborist_GoodAverageBad, reference = test_set_quality_Rborist_GoodAverageBad)


################################### RATING GOOD, BAD ######################


# Changing the quality ratings "good" (6-10) and "bad" (rating 0-5)
winequality_white_GoodBad <- winequality_white %>% mutate(quality_GoodBad = 
                            ifelse(winequality_white$quality == 0, "bad",   
                            ifelse(winequality_white$quality == 1, "bad",
                            ifelse(winequality_white$quality == 2, "bad",
                            ifelse(winequality_white$quality == 3, "bad", 
                            ifelse(winequality_white$quality == 4, "bad", 
                            ifelse(winequality_white$quality == 5, "bad", "good"))))))) %>% dplyr::select(-quality)

# Creating train set & test set for "good" and "bad" ratings
test_index_GoodBad <- createDataPartition(winequality_white_GoodBad$quality_GoodBad, times = 1, p = 0.5, list = FALSE)

test_set_GoodBad <- winequality_white_GoodBad[test_index_GoodBad, ]

train_set_GoodBad <- winequality_white_GoodBad[-test_index_GoodBad, ]


# Predicting the wine quality with Rborist against "good" and "bad" ratings with new values
train_Rborist_GoodBad <- train(quality_GoodBad ~ .,
                               method = "Rborist",
                               tuneGrid = data.frame(predFixed = 0, minNode = seq(1, 5, 1)),
                               data = train_set_GoodBad)

train_Rborist_GoodBad$bestTune %>% kable("latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "condensed"), position = "center", font_size = 9, full_width = FALSE) %>% footnote(general = "best tune")

plot(train_Rborist_GoodBad)

varImp(train_Rborist_GoodBad, scale = FALSE)

y_hat_Rborist_GoodBad <- predict(train_Rborist_GoodBad, test_set_GoodBad) %>% factor()

Accuracy_Rborist_GoodBad <- mean(y_hat_Rborist_GoodBad == test_set_GoodBad$quality_GoodBad)

Accuracy_Rborist_GoodBad # Calculate Accuracy

# Creating confusion matrix
test_set_quality_Rborist_GoodBad <- test_set_GoodBad$quality_GoodBad %>% factor()

confusionMatrix(data = y_hat_Rborist_GoodBad, reference = test_set_quality_Rborist_GoodBad)


######################## RATING GOOD & BAD to BALANCE  ####################

# Avoiding oversampling and creating balanced train set
N <- (train_set_GoodBad %>% filter(quality_GoodBad == "good") %>% nrow())*2
train_set_GoodBad_balanced <- ovun.sample(quality_GoodBad ~ ., data = train_set_GoodBad, method = "over", N = N)$data

# Balancing out with Rborist and creating a train set
train_Rborist_GoodBad_balanced <- train(quality_GoodBad ~ .,
                                        method = "Rborist",
                                        tuneGrid = data.frame(predFixed = 0, minNode = seq(1, 5, 1)),
                                        data = train_set_GoodBad_balanced)

train_Rborist_GoodBad_balanced$bestTune %>% kable("latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "condensed"), position = "center", font_size = 9, full_width = FALSE) %>% footnote(general = "best tune")

plot(train_Rborist_GoodBad_balanced)

varImp(train_Rborist_GoodBad_balanced, scale = FALSE)

y_hat_Rborist_GoodBad_balanced <- predict(train_Rborist_GoodBad_balanced, test_set_GoodBad) %>% factor()

Accuracy_Rborist_GoodBad_balanced <- mean(y_hat_Rborist_GoodBad_balanced == test_set_GoodBad$quality_GoodBad)
Accuracy_Rborist_GoodBad_balanced # Calculating the accuracy

# Creating confusion matrix
confusionMatrix(data = y_hat_Rborist_GoodBad_balanced, reference = test_set_quality_Rborist_GoodBad)



############################### RED OR WHITE? ###########################

# Predicting the type of the wine - joining 2 databases, red and white
winequality_white_winetype <- winequality_white %>% mutate(wine = "white")
winequality_red_winetype <- winequality_red %>% mutate(wine = "red")
winequality_whiteANDred <- winequality_white_winetype %>% full_join(winequality_red_winetype)


# Creating the train set & test set for the wine type, red or white
test_index_winetype <- createDataPartition(winequality_whiteANDred$wine, times = 1, p = 0.5, list = FALSE)
test_set_winetype <- winequality_whiteANDred[test_index_winetype, ] %>% dplyr::select(-quality)
train_set_winetype <- winequality_whiteANDred[-test_index_winetype, ] %>% dplyr::select(-quality)

# Predicting the Red or White with Rborist, creating train and prediction sets
train_Rborist_winetype <- train(wine ~ .,
                                method = "Rborist",
                                tuneGrid = data.frame(predFixed = 0, minNode = seq(1, 5, 1)),
                                data = train_set_winetype)

train_Rborist_winetype$bestTune %>% kable("latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "condensed"), position = "center", font_size = 9, full_width = FALSE) %>% footnote(general = "best tune")

plot(train_Rborist_winetype)

varImp(train_Rborist_winetype, scale = FALSE)

y_hat_Rborist_winetype <- predict(train_Rborist_winetype, test_set_winetype) %>% factor()

Accuracy_Rborist_winetype <- mean(y_hat_Rborist_winetype == test_set_winetype$wine)
Accuracy_Rborist_winetype # Calculating the accuracy


# Creating confusion matrix
test_set_quality_Rborist_winetype <- test_set_winetype$wine %>% factor()

confusionMatrix(data = y_hat_Rborist_winetype, reference = test_set_quality_Rborist_winetype)





