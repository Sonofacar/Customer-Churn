library(ggplot2)

#############
# Read data #
#############

train <- read.csv("train.csv") |>
  within({
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
  "SeniorCitizen",
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

# Interactions
interact <- function(data, resp, x, m) {
  Resp <- as.symbol(resp)
  resp_vals <- as.character(Resp) |> (\(.) data[[.]])() |> unique()
  X <- as.symbol(x)
  x_vals <- as.character(X) |> (\(.) data[[.]])() |> unique()
  M <- as.symbol(m)
  m_vals <- as.character(M) |> (\(.) data[[.]])() |> unique()
  freqs <- with(data, table(eval(Resp), eval(X), eval(M))) |>
    as.data.frame() |>
    (\(df) {
       colnames(df) <- c(resp, x, m, "Freq")
       df
    })()
  with(freqs, {
    output <- data.frame(
      X = character(0),
      M = character(0),
      Ratio = double(0)
    )
    i <- 1
    for (m_val in m_vals) {
      for (x_val in x_vals) {
        output[i, 1:2] <- c(x_val, m_val)
        output[i, 3] <- Freq[(eval(M) == m_val) & (eval(X) == x_val)][1] /
          Freq[(eval(M) == m_val) & (eval(X) == x_val)][2]
        i <- i + 1
      }
    }
    output
  })
}

# Here we do two things:
# 1. Make graphs comparing Churn rates by two categorical variables, similar to
#    simple slopes analysis.
# 2. Numerically compare every set of two variables. Differences in churn rates
#    are computed, normalized, and any set of variables where at least one
#    combination of responses has a value greater than 1 standard deviation
#    away is marked as a potential interaction.
#
# These both help us evaluate the interactions between variables. This might be
# useful if we continue with a linear model.
combs <- combn(cats, 2)
THRESHOLD <- 1 # Std. Devs. away from average slope
for (i in seq_along(combs[1, ])) {
  name <- paste(combs[1, i], "x", combs[2, i], ".png", sep = "")
  data <- interact(train, "Churn", combs[1, i], combs[2, i])
  (ggplot(data) +
    geom_line(aes(x = X, y = Ratio, group = M, color = M)) +
    labs(x = combs[1, i], y = "Churn", color = combs[2, i])) |>
    ggsave(name, plot = _, path = "graphs") |>
    suppressMessages()
  paste(combs[1, i], "x", combs[2, i], ": ") |> cat()
  l <- c()
  for (m in unique(data$M)) {
    l <- data[data$M == m, "Ratio"] |>
      combn(2, FUN = function(x) (x[2] - x[1]) / x[1]) |>
      (\(.) c(l, .))()
  }
  (\(.) (. - mean(.)) / sd(.))(l) |>
    (\(.) any(abs(.) > THRESHOLD))() |>
    paste("\n") |>
    cat()
}
# By these results, we have the following interactions:
# - gender x InternetService 
# - gender x Contract 
# - gender x PaymentMethod 
# - SeniorCitizen x InternetService 
# - SeniorCitizen x Contract 
# - SeniorCitizen x PaymentMethod 
# - Partner x InternetService 
# - Partner x Contract 
# - Partner x PaymentMethod 
# - Dependents x InternetService 
# - Dependents x Contract 
# - Dependents x PaymentMethod 
# - PhoneService x Contract 
# - PhoneService x PaymentMethod 
# - MultipleLines x InternetService 
# - MultipleLines x Contract 
# - MultipleLines x PaymentMethod 
# - InternetService x Contract 
# - InternetService x PaperlessBilling 
# - InternetService x PaymentMethod 
# - OnlineSecurity x Contract 
# - OnlineSecurity x PaymentMethod 
# - OnlineBackup x Contract 
# - OnlineBackup x PaymentMethod 
# - DeviceProtection x Contract 
# - DeviceProtection x PaymentMethod 
# - TechSupport x Contract 
# - TechSupport x PaymentMethod 
# - StreamingTV x Contract 
# - StreamingTV x PaymentMethod 
# - StreamingMovies x Contract 
# - StreamingMovies x PaymentMethod 
# - Contract x PaperlessBilling 
# - Contract x PaymentMethod 
# - PaperlessBilling x PaymentMethod 
