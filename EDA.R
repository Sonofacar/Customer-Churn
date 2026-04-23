#############
# Read data #
#############

train <- read.csv("train.csv") |>
  within({
    gender <- as.factor(gender)
    Partner <- as.factor(Partner)
    Dependents <- as.factor(Dependents)
    PhoneService <- as.factor(PhoneService)
    MultipleLines <- as.factor(MultipleLines)
    InternetService <- as.factor(InternetService)
    OnlineSecurity <- as.factor(OnlineSecurity)
    OnlineBackup <- as.factor(OnlineBackup)
    DeviceProtection <- as.factor(DeviceProtection)
    TechSupport <- as.factor(TechSupport)
    StreamingTV <- as.factor(StreamingTV)
    StreamingMovies <- as.factor(StreamingMovies)
    Contract <- as.factor(Contract)
    PaperlessBilling <- as.factor(PaperlessBilling)
    PaymentMethod <- as.factor(PaymentMethod)
    Churn <- as.factor(Churn)
  })

#################
# Check for NAs #
#################

train |>
  (\(df) {
     out <- data.frame(Column = character(0), NAs = numeric(0))
     i <- 1
     for (col in colnames(df)) {
       val <- df[[col]] |> is.na() |> sum()
       out[i, ] <- c(col, val)
       i <- i + 1
     }
     out
  })()
# No NA values, so things are a little easier


#########################
# Categorical variables #
#########################

# Basic counts
cats <- c(
  "gender",
  "Partner",
  "Dependents",
  "PhoneService",
  "MultipleLines",
  "InternetService",
  "OnlineSecurity",
  "OnlineBackup",
  "DeviceProtection",
  "TechSupport",
  "StreamingTV",
  "StreamingMovies",
  "Contract",
  "PaperlessBilling",
  "PaymentMethod"
)
cats |>
  (\(cols) {
     for (col in cols) {
       cat(col)
       table(train[[col]], train[["Churn"]]) |>
         print()
     }
  })()
