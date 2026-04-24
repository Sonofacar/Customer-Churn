clean <- function(df) {
  within(df, {
    gender <- as.factor(gender)
    SeniorCitizen <- (SeniorCitizen == 1) |>
      ifelse("Yes", "No") |>
      as.factor()
    Partner <- as.factor(Partner)
    Dependents <- as.factor(Dependents)
    PhoneService <- as.factor(PhoneService)
    MultipleLines <- MultipleLines |>
      (\(.) {
         .[. == "No phone service"] <- "No"
         .
      })() |>
      as.factor()
    InternetService <- as.factor(InternetService)
    OnlineSecurity <- OnlineSecurity |>
      (\(.) {
         .[. == "No internet service" ] <- "No"
         .
      })() |>
      as.factor()
    OnlineBackup <- OnlineBackup |>
      (\(.) {
         .[. == "No internet service" ] <- "No"
         .
      })() |>
      as.factor()
    DeviceProtection <- DeviceProtection |>
      (\(.) {
         .[. == "No internet service" ] <- "No"
         .
      })() |>
      as.factor()
    TechSupport <- TechSupport |>
      (\(.) {
         .[. == "No internet service" ] <- "No"
         .
      })() |>
      as.factor()
    StreamingTV <- StreamingTV |>
      (\(.) {
         .[. == "No internet service" ] <- "No"
         .
      })() |>
      as.factor()
    StreamingMovies <- StreamingMovies |>
      (\(.) {
         .[. == "No internet service" ] <- "No"
         .
      })() |>
      as.factor()
    Contract <- as.factor(Contract)
    PaperlessBilling <- as.factor(PaperlessBilling)
    PaymentMethod <- as.factor(PaymentMethod)
  })
}

train <- read.csv("train.csv") |>
  clean() |>
  within({
    Churn <- as.factor(Churn)
    rm(id)
  })
test <- read.csv("test.csv") |>
  clean()
