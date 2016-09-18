#csc570al Machine Learning 
#fall 2016
#Matthew d murphy

#import the data into the wbcd data frame
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = + FALSE)

#summary of data
str(wbcd)

#exclude patient ID (column 1)
wbcd <- wbcd[-1] # since the first column has a minus sign # it is excluded from the copy

#labels the two levels of the diagnosis variable, "B" and "M", as "Benign" and "Malignant"
wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

#table form of outcomes
table(wbcd$diagnosis)

#prop table of outcomes
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

#comparison of numeric scales for key variables
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#normalize features using min-max function
normalize <- function(x) { 
  return ((x - min(x)) / (max(x) - min(x)))  
  }

#globally normalize using lappy
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

#separate into a training and test set
wbcd_train <- wbcd_n[1:469, ] # selects all columns of the # first 469 rows
wbcd_test <- wbcd_n[470:569, ] #selects all columns of the # last 100 rows

#store labels
wbcd_train_labels <- wbcd[1:469, 1] # selects the first # column from rows 1:468
wbcd_test_labels <- wbcd[470:569, 1] #selects the first # column from rows 470:569

#load package
library(class)

#runn knn function and store outputs
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

#load gmodels package
#install.packages("gmodels") #if not already installed
library(gmodels) # loads the gmodels package

#create crosstab of predicted and actuals
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)

#rescale values using the z-score standardization
wbcd_z <- as.data.frame(scale(wbcd[-1]))

#separate into a training and test set
wbcd_train <- wbcd_z[1:469, ] # training set is extracted
wbcd_test <- wbcd_z[470:569, ] # validation set is # extracted

#store labels
wbcd_train_labels <- wbcd[1:469, 1] # training labels
wbcd_test_labels <- wbcd[470:569, 1] # test labels

#runn knn function and store outputs
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21) # runs the k-NN algorithm

#create crosstab of predicted and actuals
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE) # builds a crosstab
