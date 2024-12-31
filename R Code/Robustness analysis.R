# Load necessary libraries
library(ggplot2)


###########################################################
######         Different m Values        ##################
###########################################################


# Function to calculate OWI, WBI, and WBI-ratio
calculate_wbi_ratio <- function(data, x_var, y_var, m_values, lambda, alpha) {
  # Calculate OWI and WBI with lambda and alpha
  data$P_Wheat <- data[["Wheat n-1"]] + 
    data[["Wheatn0"]] + (data[["Wheat n1"]] * lambda) + 
    (data[["Wheat n2"]] * lambda^2) + (data[["Wheat n3"]] * lambda^3)
  
  data$P_Ore <- data[["Ore n-1"]] + 
    data[["Ore n0"]] + (data[["Ore n1"]] * lambda) + 
    (data[["Ore n2"]] * lambda^2) + (data[["Ore n3"]] * lambda^3)
  
  data$P_Brick <- data[["Brick n-1"]] + 
    data[["Brick n0"]] + (data[["Brick n1"]] * lambda) + 
    (data[["Brick n2"]] * lambda^2) + (data[["Brick n3"]] * lambda^3)
  
  data$P_Wood <- data[["Wood n-1"]] + 
    data[["Wood n0"]] + (data[["Wood n1"]] * lambda) + 
    (data[["Wood n2"]] * lambda^2) + (data[["Wood n3"]] * lambda^3)
  
  data$T_WB <- data$P_Wood + data$P_Brick
  data$T_OW <- data$P_Ore + data$P_Wheat
  
  data$OWI <- with(data, 
                   (pmin(P_Ore / T_OW, P_Wheat / T_OW)^alpha) * ((P_Ore + P_Wheat)^(1 - alpha)))
  data$WBI <- with(data, 
                   (pmin(P_Wood / T_WB, P_Brick / T_WB)^alpha) * ((P_Wood + P_Brick)^(1 - alpha)))
  
  # Store results
  results <- data.frame(m = numeric(), WBI_Ratio = numeric(), Above = numeric(), Below = numeric())
  
  # Iterate over m values
  for (m in m_values) {
    # Classify points
    data$Position <- ifelse(
      data$WBI > data$OWI + m, "Above",
      ifelse(data$WBI < data$OWI - m, "Below", "Neutral")
    )
    
    # Count classifications
    counts <- table(factor(data$Position, levels = c("Above", "Below", "Neutral")))
    
    # Calculate WBI-ratio
    wbi_ratio <- ifelse(counts["Below"] > 0, counts["Above"] / counts["Below"], NA)
    
    # Store results
    results <- rbind(results, data.frame(
      m = m,
      WBI_Ratio = wbi_ratio,
      Above = counts["Above"],
      Below = counts["Below"]
    ))
    
    # Debug output
    cat(sprintf("m: %.2f | Above: %d | Below: %d\n", m, counts["Above"], counts["Below"]))
  }
  
  return(results)
}

# Define parameters
m_values <- seq(0.8, 3.3, by = 0.2)  # Values for m
lambda <- 0.9  # Fixed lambda value
alpha <- 0.2  # Fixed alpha value

# Calculation
results <- calculate_wbi_ratio(data, "OWI", "WBI", m_values, lambda, alpha)

# Plot the results
ggplot(results, aes(x = m, y = WBI_Ratio)) +
  geom_line(color = "black") +
  geom_point(color = "black") +
  theme_minimal() +
  labs(
    title = paste("WBI-ratio bei Variation von m"),
    x = "m",
    y = "WBI-ratio"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#### Regression analysis for variation of m

# Perform regression analysis
run_regression <- function(results) {
  # Remove NA values from results
  results_clean <- results[complete.cases(results), ]
  
  # Create linear regression model
  model <- lm(WBI_Ratio ~ m, data = results_clean)
  
  # Display regression summary
  print(summary(model))
  
  # Plot regression
  ggplot(results_clean, aes(x = m, y = WBI_Ratio)) +
    geom_point(color = "blue", size = 3) +  # Plot points
    geom_smooth(method = "lm", color = "red", se = TRUE) +  # Add regression line
    theme_minimal() +
    labs(
      title = paste("Regression of WBI-ratio on m\nLambda =", lambda, ", Alpha =", alpha),
      x = "m (absolute)",
      y = "WBI-ratio"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5)
    )
}

# Run regression analysis
run_regression(results)


####################################
###### alpha and lambda Heat map####
####################################

# Function to calculate WBI-ratio dynamically for given lambda, alpha, and m
calculate_metrics <- function(data, lambda, alpha, m) {
  # Recalculate weighted resources based on lambda
  data$P_Wheat <- data[["Wheat n-1"]] + 
    data[["Wheatn0"]] + (data[["Wheat n1"]] * lambda) + 
    (data[["Wheat n2"]] * lambda^2) + (data[["Wheat n3"]] * lambda^3)
  
  data$P_Ore <- data[["Ore n-1"]] + 
    data[["Ore n0"]] + (data[["Ore n1"]] * lambda) + 
    (data[["Ore n2"]] * lambda^2) + (data[["Ore n3"]] * lambda^3)
  
  data$P_Brick <- data[["Brick n-1"]] + 
    data[["Brick n0"]] + (data[["Brick n1"]] * lambda) + 
    (data[["Brick n2"]] * lambda^2) + (data[["Brick n3"]] * lambda^3)
  
  data$P_Wood <- data[["Wood n-1"]] + 
    data[["Wood n0"]] + (data[["Wood n1"]] * lambda) + 
    (data[["Wood n2"]] * lambda^2) + (data[["Wood n3"]] * lambda^3)
  
  # Recalculate OWI and WBI based on alpha
  data$T_WB <- data$P_Wood + data$P_Brick
  data$T_OW <- data$P_Ore + data$P_Wheat
  
  data$OWI <- with(data, 
                   (pmin(P_Ore / T_OW, P_Wheat / T_OW)^alpha) * ((P_Ore + P_Wheat)^(1 - alpha)))
  data$WBI <- with(data, 
                   (pmin(P_Wood / T_WB, P_Brick / T_WB)^alpha) * ((P_Wood + P_Brick)^(1 - alpha)))
  
  # Classify points relative to m
  data$Position <- ifelse(
    data$WBI > data$OWI + m, "Above",
    ifelse(data$WBI < data$OWI - m, "Below", "Neutral")
  )
  
  # Calculate WBI-ratio (Above / Below)
  counts <- table(factor(data$Position, levels = c("Above", "Below", "Neutral")))
  wbi_ratio <- ifelse(counts["Below"] > 0, counts["Above"] / counts["Below"], NA)
  
  return(wbi_ratio)
}

# Define parameter ranges
lambda_values <- seq(0.2, 0.9, by = 0.1)  # Vary lambda
alpha_values <- seq(0.1, 0.5, by = 0.05)  # Vary alpha
m_value <- 2  # Set m value (can be adjusted)

# Load actual dataset
data <- fread("Daten_Bachelorarbeit.csv", header = TRUE)  # Replace with correct file path
data[is.na(data)] <- 0

# Create grid for lambda and alpha
results <- expand.grid(lambda = lambda_values, alpha = alpha_values)
results$WBI_Ratio <- NA

# Iterate over all combinations of lambda and alpha
for (i in 1:nrow(results)) {
  lambda <- results$lambda[i]
  alpha <- results$alpha[i]
  results$WBI_Ratio[i] <- calculate_metrics(data, lambda, alpha, m_value)
}

# Plot the results as a heatmap
ggplot(results, aes(x = lambda, y = alpha, fill = WBI_Ratio)) +
  geom_tile() +
  scale_fill_gradient(low = "blue",  high = "red", na.value = "grey") + # Grey for NA
  theme_minimal() +
  labs(
    title = paste("Robustness Test: Impact of λ, α, and m =", m_value),
    x = "λ (Lambda)",
    y = "α (Alpha)",
    fill = "WBI-ratio"
  )


#########################################################
######         Different lambda Values        ###########
#########################################################

# Function to calculate the WBI-ratio for different lambda or alpha values
calculate_wbi_for_single_param <- function(data, lambda_values, alpha_values, fixed_alpha, fixed_lambda) {
  
  # Vary lambda (alpha fixed)
  results_lambda <- data.frame(Lambda = lambda_values, WBI_Ratio = NA)
  for (i in 1:length(lambda_values)) {
    lambda <- lambda_values[i]
    alpha <- fixed_alpha
    results_lambda$WBI_Ratio[i] <- calculate_metrics(data, lambda, alpha, m = 2)
  }
  
  # Vary alpha (lambda fixed)
  results_alpha <- data.frame(Alpha = alpha_values, WBI_Ratio = NA)
  for (i in 1:length(alpha_values)) {
    alpha <- alpha_values[i]
    lambda <- fixed_lambda
    results_alpha$WBI_Ratio[i] <- calculate_metrics(data, lambda, alpha, m = 2)
  }
  
  return(list(lambda_results = results_lambda, alpha_results = results_alpha))
}

# Function definition for calculating OWI/WBI (same as above)
calculate_metrics <- function(data, lambda, alpha, m) {
  data$P_Wheat <- data[["Wheat n-1"]] + data[["Wheatn0"]] + 
    (data[["Wheat n1"]] * lambda) + (data[["Wheat n2"]] * lambda^2) + (data[["Wheat n3"]] * lambda^3)
  data$P_Ore <- data[["Ore n-1"]] + data[["Ore n0"]] + 
    (data[["Ore n1"]] * lambda) + (data[["Ore n2"]] * lambda^2) + (data[["Ore n3"]] * lambda^3)
  data$P_Brick <- data[["Brick n-1"]] + data[["Brick n0"]] + 
    (data[["Brick n1"]] * lambda) + (data[["Brick n2"]] * lambda^2) + (data[["Brick n3"]] * lambda^3)
  data$P_Wood <- data[["Wood n-1"]] + data[["Wood n0"]] + 
    (data[["Wood n1"]] * lambda) + (data[["Wood n2"]] * lambda^2) + (data[["Wood n3"]] * lambda^3)
  
  data$T_WB <- data$P_Wood + data$P_Brick
  data$T_OW <- data$P_Ore + data$P_Wheat
  
  data$OWI <- with(data, (pmin(P_Ore / T_OW, P_Wheat / T_OW)^alpha) * ((P_Ore + P_Wheat)^(1 - alpha)))
  data$WBI <- with(data, (pmin(P_Wood / T_WB, P_Brick / T_WB)^alpha) * ((P_Wood + P_Brick)^(1 - alpha)))
  
  counts <- table(factor(ifelse(data$WBI > data$OWI + m, "Above", 
                                ifelse(data$WBI < data$OWI - m, "Below", "Neutral")), 
                         levels = c("Above", "Below", "Neutral")))
  wbi_ratio <- ifelse(counts["Below"] > 0, counts["Above"] / counts["Below"], NA)
  return(wbi_ratio)
}

# Define parameter values
lambda_values <- seq(0.2, 0.9, by = 0.1)  # Vary lambda
alpha_values <- seq(0.1, 0.5, by = 0.05)  # Vary alpha
fixed_alpha <- 0.3  # Fixed alpha value
fixed_lambda <- 0.7  # Fixed lambda value

# Load data
data <- fread("Daten_Bachelorarbeit.csv", header = TRUE)
data[is.na(data)] <- 0

# Perform calculations
results <- calculate_wbi_for_single_param(data, lambda_values, alpha_values, fixed_alpha, fixed_lambda)

# Plot for lambda values
ggplot(results$lambda_results, aes(x = Lambda, y = WBI_Ratio)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "WBI-ratio bei Variation von Lamda",
    x = "Lambda (λ)",
    y = "WBI-ratio"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


### Regression lambda:

# Regression analysis for lambda
model_lambda <- lm(WBI_Ratio ~ Lambda, data = results$lambda_results)

# Summary of regression for lambda
cat("Regression for Lambda:\n")
print(summary(model_lambda))

# Visualization of regression for lambda
ggplot(results$lambda_results, aes(x = Lambda, y = WBI_Ratio)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Regression of WBI Ratio on Lambda",
    x = "Lambda (λ)",
    y = "WBI-ratio"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


#########################################################
######         Different Alpha Values         ###########
#########################################################


# Plot for alpha values
ggplot(results$alpha_results, aes(x = Alpha, y = WBI_Ratio)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "WBI-ratio bei Variation von Alpha",
    x = "Alpha (α)",
    y = "WBI-ratio"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#### Regression alpha:

# Regression analysis for alpha
model_alpha <- lm(WBI_Ratio ~ Alpha, data = results$alpha_results)

# Summary of regression for alpha
cat("Regression for Alpha:\n")
print(summary(model_alpha))

# Visualization of regression for alpha
ggplot(results$alpha_results, aes(x = Alpha, y = WBI_Ratio)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Regression of WBI Ratio on Alpha",
    x = "Alpha (α)",
    y = "WBI-ratio"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


#######################################################
#### Varying m for different lambda values ###########
#######################################################

# Function to calculate the WBI-ratio for different m values
calculate_wbi_ratio_lambda <- function(data, x_var, y_var, m_values, lambda_values, alpha) {
  # Initialize the results data frame
  results <- data.frame(m = numeric(), Lambda = numeric(), WBI_Ratio = numeric(), Above = numeric(), Below = numeric())
  
  for (lambda in lambda_values) {
    # Calculate OWI and WBI based on lambda
    data$P_Wheat <- data[["Wheat n-1"]] + 
      data[["Wheatn0"]] + (data[["Wheat n1"]] * lambda) + 
      (data[["Wheat n2"]] * lambda^2) + (data[["Wheat n3"]] * lambda^3)
    
    data$P_Ore <- data[["Ore n-1"]] + 
      data[["Ore n0"]] + (data[["Ore n1"]] * lambda) + 
      (data[["Ore n2"]] * lambda^2) + (data[["Ore n3"]] * lambda^3)
    
    data$P_Brick <- data[["Brick n-1"]] + 
      data[["Brick n0"]] + (data[["Brick n1"]] * lambda) + 
      (data[["Brick n2"]] * lambda^2) + (data[["Brick n3"]] * lambda^3)
    
    data$P_Wood <- data[["Wood n-1"]] + 
      data[["Wood n0"]] + (data[["Wood n1"]] * lambda) + 
      (data[["Wood n2"]] * lambda^2) + (data[["Wood n3"]] * lambda^3)
    
    data$T_WB <- data$P_Wood + data$P_Brick
    data$T_OW <- data$P_Ore + data$P_Wheat
    
    data$OWI <- with(data, 
                     (pmin(P_Ore / T_OW, P_Wheat / T_OW)^alpha) * ((P_Ore + P_Wheat)^(1 - alpha)))
    data$WBI <- with(data, 
                     (pmin(P_Wood / T_WB, P_Brick / T_WB)^alpha) * ((P_Wood + P_Brick)^(1 - alpha)))
    
    # Calculate the WBI-ratio for each m value
    for (m in m_values) {
      data$Position <- ifelse(
        data$WBI > data$OWI + m, "Above",
        ifelse(data$WBI < data$OWI - m, "Below", "Neutral")
      )
      
      counts <- table(factor(data$Position, levels = c("Above", "Below", "Neutral")))
      wbi_ratio <- ifelse(counts["Below"] > 0, counts["Above"] / counts["Below"], NA)
      
      results <- rbind(results, data.frame(
        m = m,
        Lambda = lambda,
        WBI_Ratio = wbi_ratio,
        Above = counts["Above"],
        Below = counts["Below"]
      ))
    }
  }
  
  return(results)
}

# Main routine
# Load data (replace file path with your actual path)
data <- fread("Daten_Bachelorarbeit.csv", header = TRUE)
data[is.na(data)] <- 0

# Define parameters
m_values <- seq(0.8, 3.3, by = 0.2)  # Values for m
lambda_values <- seq(0.2, 0.9, by = 0.1)  # Values for lambda
alpha <- 0.2  # Fixed alpha value

# Calculate WBI-ratios
results <- calculate_wbi_ratio_lambda(data, "OWI", "WBI", m_values, lambda_values, alpha)

# Visualization: WBI-ratio for different lambda values
ggplot(results, aes(x = m, y = WBI_Ratio, color = factor(Lambda))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "WBI-ratio for varying m and lambda",
    x = "m",
    y = "WBI-ratio",
    color = "Lambda"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
