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
    gender:InternetService +
    gender:Contract +
    gender:PaymentMethod +
    SeniorCitizen:InternetService +
    SeniorCitizen:Contract +
    SeniorCitizen:PaymentMethod +
    Partner:InternetService +
    Partner:Contract +
    Partner:PaymentMethod +
    Dependents:InternetService +
    Dependents:Contract +
    Dependents:PaymentMethod +
    PhoneService:Contract +
    PhoneService:PaymentMethod +
    MultipleLines:InternetService +
    MultipleLines:Contract +
    MultipleLines:PaymentMethod +
    InternetService:Contract +
    InternetService:PaperlessBilling +
    InternetService:PaymentMethod +
    OnlineSecurity:Contract +
    OnlineSecurity:PaymentMethod +
    OnlineBackup:Contract +
    OnlineBackup:PaymentMethod +
    DeviceProtection:Contract +
    DeviceProtection:PaymentMethod +
    TechSupport:Contract +
    TechSupport:PaymentMethod +
    StreamingTV:Contract +
    StreamingTV:PaymentMethod +
    StreamingMovies:Contract +
    StreamingMovies:PaymentMethod +
    Contract:PaperlessBilling +
    Contract:PaymentMethod +
    PaperlessBilling:PaymentMethod,
  data = train,
  family = binomial()
) |>
  predict(newdata = test, type = "resp") |>
  (\(.) {
     x <- rep(0, length(.))
     x[. >= 0.4] <- 1
     x
  })() |>
  attach_predictions() |>
  write.csv("logistic_regression.csv", quote = FALSE, row.names = FALSE)


#################
# Random Forest #
#################

randomForest(Churn ~ ., data = train, maxnodes = 40) |>
  predict(newdata = test) |>
  (\(.) {
     x <- rep(0, length(.))
     x[. == "Yes"] <- 1
     x
  })() |>
  attach_predictions() |>
  write.csv("random_forest.csv", quote = FALSE, row.names = FALSE)
