library(caret)
Bank_Full<- read.csv("bank-additional-full.csv", sep=";")
str(Bank_Full) #41188obs 21 variables
# Remove duration column
Bank_Full<- Bank_Full[Bank_Full$default != 'yes',-11]

# Create pdays
pdays <- Bank_Full$pdays

#Create height categories of A = 0-7, B=7-14, C=14-998, D=998+
cutpdays <- cut(pdays, breaks=c(-1,7,14,998,999), labels=c("A", "B", "C", "D"), right=TRUE)
Bank_Full$cutpdays <- cutpdays

# Split data into training and test set
set.seed(123)
train.index <- createDataPartition(Bank_Full$y, p = .80, list = FALSE)
train <- Bank_Full[ train.index,]
test  <- Bank_Full[-train.index,]

# Check to make sure class proportions have been maintained
mean(Bank_Full$y == 'yes')
mean(train$y == 'yes')
mean(test$y == 'yes')


Principal_Component <- function(dataframe){
  "Subset of raw data with only the economic features"
  dataframe_features_subset <- data.frame("emp.var.rate"=dataframe$emp.var.rate,"cons.price.idx"=dataframe$cons.conf.idx,"cons.conf.idx"=dataframe$cons.conf.idx,"euribor3m"=dataframe$euribor3m,"nr.employed"=dataframe$nr.employed)
  
  "Create Principal Components of the economic features"
  dataframe_subset_princomp <- prcomp(dataframe_features_subset,center=TRUE,scale.=TRUE)
  
  "Create a new dataframe by removing the original economic features and replacing them with the principal components  "
  dataframe_new <- dataframe[, !(colnames(dataframe) %in% c("emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed"))]
  dataframe_new$economic1 <- dataframe_subset_princomp$x[,1]
  dataframe_new$economic2 <- dataframe_subset_princomp$x[,2]
  
  "Return the new dataframe"
  return (dataframe_new)
}

train.pca <- Principal_Component(train)
test.pca <- Principal_Component(test)

# Output basic train/test
write.csv(train[,!(colnames(train) %in% c("cutpdays"))], "simple_train.csv", row.names = FALSE)
write.csv(test[,!(colnames(test) %in% c("cutpdays"))], "simple_test.csv", row.names = FALSE)

# Output updated train/test
write.csv(train.pca[,!(colnames(train.pca) %in% c("pdays"))], "updated_train.csv", row.names = FALSE)
write.csv(test.pca[,!(colnames(test.pca) %in% c("pdays"))], "updated_test.csv", row.names = FALSE)
