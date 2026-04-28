library(randomForest) |> suppressMessages()

source("clean_data.R")

attach_predictions <- function(x) {
  data.frame(
    id = test$id,
    Churn = x
  )
}


#######################
# Logistic Regression #
#######################

glm(Churn ~ . +
    SeniorCitizen:InternetService +
    SeniorCitizen:Contract +
    SeniorCitizen:PaymentMethod +
    Partner:InternetService +
    Partner:PaymentMethod +
    Dependents:InternetService +
    Dependents:Contract +
    PhoneService:Contract +
    PhoneService:PaymentMethod +
    MultipleLines:InternetService +
    MultipleLines:Contract +
    InternetService:Contract +
    InternetService:PaymentMethod +
    OnlineSecurity:Contract +
    OnlineSecurity:PaymentMethod +
    DeviceProtection:PaymentMethod +
    TechSupport:Contract +
    TechSupport:PaymentMethod +
    StreamingTV:Contract +
    StreamingTV:PaymentMethod +
    StreamingMovies:Contract +
    StreamingMovies:PaymentMethod +
    Contract:PaymentMethod +
    MonthlyCharges:Contract +
    MonthlyCharges:InternetService +
    MonthlyCharges:MultipleLines +
    MonthlyCharges:PaperlessBilling +
    MonthlyCharges:PaymentMethod +
    MonthlyCharges:SeniorCitizen +
    MonthlyCharges:StreamingMovies +
    MonthlyCharges:StreamingTV +
    TotalCharges:DeviceProtection +
    TotalCharges:InternetService +
    TotalCharges:MultipleLines +
    TotalCharges:OnlineBackup +
    TotalCharges:OnlineSecurity +
    TotalCharges:OnlineSecurity +
    TotalCharges:Partner +
    TotalCharges:PaymentMethod +
    TotalCharges:StreamingMovies +
    TotalCharges:StreamingTV +
    TotalCharges:TechSupport +
    tenure:Contract +
    tenure:DeviceProtection +
    tenure:MultipleLines +
    tenure:OnlineBackup +
    tenure:OnlineSecurity +
    tenure:Partner +
    tenure:StreamingMovies +
    tenure:StreamingTV +
    tenure:TechSupport +
    tenure*MonthlyCharges*TotalCharges,
  data = train,
  family = binomial()
) |>
  predict(newdata = test, type = "resp") |>
  attach_predictions() |>
  write.csv("logistic_regression.csv", quote = FALSE, row.names = FALSE)


#################
# Random Forest #
#################

randomForest(Churn ~ ., data = train) |>
  predict(newdata = test) |>
  (\(.) {
     x <- rep(0, length(.))
     x[. == "Yes"] <- 1
     x
  })() |>
  attach_predictions() |>
  write.csv("random_forest.csv", quote = FALSE, row.names = FALSE)
