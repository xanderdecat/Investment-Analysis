# Load necessary libraries
library(readxl)
library(ggplot2)
install.packages("reshape2")
library (reshape2)
library(dplyr)
library(tidyr)
library(quadprog)

# Load the data
file_path <- "C:\\Users\\Anna\\OneDrive - UGent\\Documenten\\Investment Analysis\\Assignment\\Data exercise 2.xlsx"
data <- read_excel(file_path)
# Display the first few rows to understand the data structure
head(data)

##STEP 2.1
# Calculate monthly returns for SPY and SPTI
data <- data %>%
  arrange(Date) %>%
  mutate(
    SPY_return = (SPY / lag(SPY) - 1),
    SPTI_return = (SPTI / lag(SPTI) - 1)
  ) %>%
  na.omit() # Remove the first row with NA returns
# Display the first few rows of returns
head(data)

# Expected returns (mean monthly returns)
expected_returns <- colMeans(data[, c("SPY_return", "SPTI_return")])
# Covariance matrix
cov_matrix <- cov(data[, c("SPY_return", "SPTI_return")])

# Risk-free monthly rate
risk_free_rate <- 0.0017

# Expected returns for SPY and SPTI
mu_spy <- expected_returns["SPY_return"]
mu_spti <- expected_returns["SPTI_return"]

# Covariance and variance terms
sigma_spy2 <- cov_matrix["SPY_return", "SPY_return"]
sigma_spti2 <- cov_matrix["SPTI_return", "SPTI_return"]
sigma_spy_spti <- cov_matrix["SPY_return", "SPTI_return"]

# Optimal weight for SPY
w_spy <- (sigma_spti2 * (mu_spy - risk_free_rate) - sigma_spy_spti * (mu_spti - risk_free_rate)) / 
  (sigma_spy2 * sigma_spti2 - sigma_spy_spti^2)

# Optimal weight for SPTI
w_spti <- 1 - w_spy

# Sharpe ratio of the optimal risky portfolio
portfolio_return <- w_spy * mu_spy + w_spti * mu_spti
portfolio_sd <- sqrt(w_spy^2 * sigma_spy2 + w_spti^2 * sigma_spti2 + 2 * w_spy * w_spti * sigma_spy_spti)
sharpe_ratio <- (portfolio_return - risk_free_rate) / portfolio_sd
sharpe_ratio

# Display results
cat("Optimal weight for SPY:", round(w_spy, 4), "\n")
cat("Optimal weight for SPTI:", round(w_spti, 4), "\n")
cat("Sharpe Ratio of the Optimal Risky Portfolio:", round(sharpe_ratio, 4), "\n")

risk_aversion <- 10  # Risk aversion parameter

# Function to calculate utility for a given allocation to the risk-free asset (q)
utility_function <- function(q) {
  # Expected return and risk of the combined portfolio
  combined_return <- (1 - q) * portfolio_return + q * risk_free_rate
  combined_variance <- ((1 - q) * portfolio_sd)^2
  # Utility calculation
  combined_return - 0.5 * risk_aversion * combined_variance
}

# Optimize to find the value of q that maximizes utility
opt_result <- optimize(utility_function, interval = c(0, 1), maximum = TRUE)
optimal_q <- opt_result$maximum
optimal_utility <- opt_result$objective

# Calculate allocations
allocation_risk_free <- optimal_q * 100
allocation_risky <- (1 - optimal_q) * 100

# Display the results
cat("Optimal allocation to the risk-free asset:", round(allocation_risk_free, 3), "%\n")
cat("Optimal allocation to the risky portfolio:", round(allocation_risky, 3), "%\n")
cat("Maximum utility:", round(optimal_utility, 4), "\n")

# Pie chart of the optimal allocation
allocation_data <- data.frame(
  Allocation = c("Risk-Free Asset", "Risky Portfolio"),
  Percentage = c(allocation_risk_free, allocation_risky)
)

ggplot(allocation_data, aes(x = "", y = Percentage, fill = Allocation)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Optimal Capital Allocation with Quadratic Utility")

##STEP 2.2

# Calculate monthly returns for the new asset class, IAU
data <- data %>%
  mutate(IAU_return = (IAU / lag(IAU) - 1)) %>%
  na.omit()  # Remove the first row with NA returns

# Calculate the correlation matrix for SPY, SPTI, and IAU returns
cor_matrix <- cor(data[, c("SPY_return", "SPTI_return", "IAU_return")])

# Display the correlation matrix
print("Correlation Matrix:")
print(cor_matrix)

## STEP 2.3
# Assuming optimal weights from question 2.1 for SPY and SPTI
optimal_weights_risky <- c(w_spy, w_spti)  # Replace w_spy and w_spti with actual values

# Expected return and covariance matrix for SPY and SPTI
mu_risky <- sum(optimal_weights_risky * expected_returns[1:2])  # Expected return of risky portfolio
sigma_risky <- sqrt(t(optimal_weights_risky) %*% cov_matrix[1:2, 1:2] %*% optimal_weights_risky)  # SD of risky portfolio
# Covariance between the risky portfolio and IAU
cov_risky_IAU <- sum(optimal_weights_risky * cov_matrix[1:2, "IAU_return"])

# Define expected returns and standard deviation for the two-asset portfolio (risky + IAU)
mu_combined <- c(mu_risky, expected_returns["IAU_return"])  # Expected returns of the combined assets
sigma_combined <- matrix(c(sigma_risky^2, cov_risky_IAU, cov_risky_IAU, cov_matrix["IAU_return", "IAU_return"]), nrow = 2)
# Risk-free rate
risk_free_rate <- 0.0017

# Define optimization problem for Sharpe ratio maximization
dvec <- mu_combined - risk_free_rate  # Excess returns
Dmat <- sigma_combined
Amat <- cbind(1, diag(2))  # Constraints (sum of weights = 1 and weights >= 0)
bvec <- c(1, 0, 0)  # Constraints (sum of weights = 1 and non-negativity)

# Solve for the optimal weights using quadratic programming
opt_result_combined <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
optimal_weights_combined <- opt_result_combined$solution

# Calculate the Sharpe ratio for the new combined portfolio
combined_return <- sum(optimal_weights_combined * mu_combined)
combined_sd <- sqrt(t(optimal_weights_combined) %*% sigma_combined %*% optimal_weights_combined)
combined_sharpe_ratio <- (combined_return - risk_free_rate) / combined_sd

# Display the results
cat("Optimal weights for Risky Portfolio:", round(optimal_weights_combined[1], 4), "\n")
cat("Optimal weights for IAU:", round(optimal_weights_combined[2], 4), "\n")
cat("Sharpe Ratio of the Combined Portfolio:", round(combined_sharpe_ratio, 4), "\n")

# Display comparison
cat("Sharpe Ratio of the Original Optimal Risky Portfolio:", round(sharpe_ratio, 4), "\n")  # Sharpe ratio from question 2.1
cat("Sharpe Ratio of the Combined Portfolio with IAU:", round(combined_sharpe_ratio, 4), "\n")



