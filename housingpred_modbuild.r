#### TEST.CSV ####
# pmm tries to impute values that are simiar to the data (refer to citation)
imput_data <- mice(test, m = 5, maxit = 30, method = ’pmm’,seed = 500)
summary(imput_data)
complete <- complete(imput_data)
write.csv(completeData, file = "imputed_test.csv", row.names = FALSE)

#### TRAIN.CSV ####
imput_train <- mice(train, m =5, maxit = 30, method = ’pmm’, seed = 500)
summary(imput_train)
completeTrain <- complete(imput_train)
View(completeTrain)
write.csv(completeTrain, file = "imputed_train.csv",row.names= FALSE)

imputed_test <- read.csv("C:/Users/Owner/Downloads/imputed_test.csv")
View(imputed_test)

imputed_train <- read.csv("C:/Users/Owner/Downloads/imputed_train.csv")
View(imputed_train)
attach(imputed_train)

ndmodel <- lm(SalePrice ~ MSSubClass+LotFrontage+LotArea+Street+ Utilities+OverallQual+OverallCond+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+CentralAir+X1stFlrSF+X2ndFlrSF+LowQualFinSF+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+Fireplaces+GarageCars+GarageArea+WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+MiscVal+MoSold+YrSold)
nullmodelnd <- lm(SalePrice~1)
scopend = list(lower=formula(nullmodelnd), upper=formula(ndmodel))
summary(ndmodel)

selectnd = SignifReg(nullmodelnd, scope = scopend, direction = "both", trace = FALSE, criterion = "AIC", alpha = 1)
summary(selectnd)
plot(selectnd)
colnames(imputed_train)[1] <- "Id"
y_hat3 = as.numeric(predict(selectnd,imputed_test))
outputnd = data.frame(Id = imputed_test$Id, SalePrice = y_hat3)
write.csv(outputnd, file = "my_submission(3).csv", row.names = FALSE)
