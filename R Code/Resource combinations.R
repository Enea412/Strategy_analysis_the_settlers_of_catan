# Load necessary libraries
library(ggplot2)


#########################################################################################
##Creating a column for each variable: T_WB, T_OW, OWI, BWI as mentioned in the Thesis###
#########################################################################################
alpha = 0.2


##T_WB
data$T_WB <- data[["P_Wood"]] + data[["P_Brick"]]

#T_OW
data$T_OW <- data[["P_Ore"]] + data[["P_Wheat"]]
## alpha



#OWIb

data$OWI <- with(data, 
                 (pmin(P_Ore / T_OW, P_Wheat / T_OW)^alpha) * ((P_Ore + P_Wheat)^(1 - alpha))
)
#WBI
data$WBI <- with(data, 
                 (pmin(P_Wood / T_WB, P_Brick/ T_WB)^alpha) * ((P_Wood + P_Brick)^(1 - alpha))
)



########################################
########################################
#####plotting the data on OWI=WBI########
########################################
########################################


# Function to plot and count points above (purple) and below (brown) the line OWI = WBI
plot_with_fixed_y <- function(data, x_var) {
  # Fixed y_var as WBI
  y_var <- "WBI"
  
  # Count points above and below the x = y line
  points_above <- sum(data[[x_var]] > data[[y_var]], na.rm = TRUE)
  points_below <- sum(data[[x_var]] < data[[y_var]], na.rm = TRUE)
  
  # Create the plot
  plot <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(aes(color = .data[[x_var]] > .data[[y_var]]), size = 3, alpha = 0.8) +  # Color points dynamically
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # x = y line
    scale_color_manual(
      values = c("TRUE" = "brown", "FALSE" = "purple"),
      labels = c("Oberhalb", "Unterhalb")
    ) +
    labs(
      title = paste(x_var, "&", y_var, "getrennt durch OWI = WBI"),
      subtitle = paste("Oberhalb:", points_above, "| Unterhalb:", points_below),
      x = x_var,
      y = y_var,
      color = ""
    ) +
    theme_gray() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.position = "top"
    )
  
  # Print the plot
  print(plot)
  
  # Return the counts
  return(list(Above = points_above, Below = points_below))
}

# Example Usage
result <- plot_with_fixed_y(data, "OWI")
print(result)


############################################
#Points that are part of only one strategy##
############################################

plot_with_fixed_y <- function(data, x_var) {
  # Fixed y_var as WBI
  y_var <- "WBI"
  
  # Count points where either WBI = 0 and OWI > 0 or OWI = 0 and WBI > 0
  points_WBI_0_OWI_positive <- sum(data[[y_var]] == 0 & data[[x_var]] > 0, na.rm = TRUE)
  points_OWI_0_WBI_positive <- sum(data[[x_var]] == 0 & data[[y_var]] > 0, na.rm = TRUE)
  
  # Return the counts
  return(list(
    WBI_0_OWI_positive = points_WBI_0_OWI_positive,
    OWI_0_WBI_positive = points_OWI_0_WBI_positive
  ))
}

# Example Usage
result <- plot_with_fixed_y(data, "OWI")
print(result)


##################################################################
##################################################################
#####plotting the data parted by a neutral zone  m as a number ###
##################################################################
##################################################################


# Function to plot points with a fixed neutral zone and display counts in the plot

plot_with_fixed_m <- function(data, x_var, y_var, m = 2) {
  # Classify points based on their position relative to the neutral zone
  data$Position <- ifelse(
    data[[y_var]] > data[[x_var]] + m, "Above",
    ifelse(data[[y_var]] < data[[x_var]] - m, "Below", "Neutral")
  )

  # Count the points in each category
  counts <- table(data$Position)
  points_above <- counts["Above"]
  points_below <- counts["Below"]
  points_neutral <- counts["Neutral"]
  
  # Create the plot
  plot <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(aes(color = Position), size = 3, alpha = 0.8) +  # Scatter points
    geom_abline(slope = 1, intercept = m, linetype = "dashed", color = "black") +  # Upper bound
    geom_abline(slope = 1, intercept = -m, linetype = "dashed", color = "black") +  # Lower bound
    scale_color_manual(
      values = c("Above" = "purple", "Below" = "brown", "Neutral" = "blue"),
      labels = c("Above", "Below", "Neutral")
    ) +
    labs(
      title = paste(x_var, "vs", y_var, "with Neutral Zone (m = 2)"),
      subtitle = paste(
        "Oberhalb:", points_above, 
        "| Unterhalb:", points_below, 
        "| Neutral:", points_neutral
      ),
      x = x_var,
      y = y_var,
      color = "Position"
    ) +
    theme_gray() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.position = "top"
    )
  
  # Print the plot
  print(plot)
  
  # Return counts
  return(counts)
}

# Usage

result <- plot_with_fixed_m(data, "OWI", "WBI", m = 2)  ## m wird so gewählt, dass ungefähr 30% der Punnkte Neutral sind. 
print(result)






######################################################################
######################################################################
#####plotting the data parted by a neutral zone  m as a percentage ###
######################################################################
######################################################################


# Define the target neutral percentage globally
neutral_percentage <- 0.5  # Change this value to adjust the target percentage of neutral points

# Function to calculate the proportion of points in the neutral zone for a given m
calculate_neutral_ratio <- function(data, x_var, y_var, m) {
  data$Position <- ifelse(
    data[[y_var]] > data[[x_var]] + m, "Above",
    ifelse(data[[y_var]] < data[[x_var]] - m, "Below", "Neutral")
  )
  counts <- table(factor(data$Position, levels = c("Above", "Below", "Neutral")))
  neutral_count <- counts["Neutral"]
  total_count <- sum(counts)
  
  if (total_count == 0 || is.na(neutral_count)) {
    return(NA)  # Return NA if no valid data points
  }
  
  return(neutral_count / total_count)
}

# Function to find the optimal m for a target neutral ratio
find_optimal_m <- function(data, x_var, y_var, target_neutral_ratio = 0.4, tolerance = 0.01, max_iter = 100) {
  lower_bound <- 0  # Start with a small m
  upper_bound <- max(abs(data[[x_var]] - data[[y_var]]), na.rm = TRUE)  # Upper bound based on data range
  iter <- 0  # Iteration counter
  
  # Binary search to find optimal m
  while (iter < max_iter) {
    iter <- iter + 1
    mid_m <- (lower_bound + upper_bound) / 2
    neutral_ratio <- calculate_neutral_ratio(data, x_var, y_var, mid_m)
    
    if (is.na(neutral_ratio)) {
      warning("Neutral ratio is NA; check your data for issues.")
      return(NA)
    }
    
    if (abs(neutral_ratio - target_neutral_ratio) <= tolerance) {
      return(mid_m)  # Found the optimal m
    } else if (neutral_ratio < target_neutral_ratio) {
      lower_bound <- mid_m  # Increase m to include more neutral points
    } else {
      upper_bound <- mid_m  # Decrease m to reduce neutral points
    }
  }
  
  warning("Max iterations reached without finding an exact m")
  return((lower_bound + upper_bound) / 2)  # Return the best estimate
}

# Enhanced plot function with dynamic m determination
plot_with_dynamic_m <- function(data, x_var, y_var, target_neutral_ratio = NULL, tolerance = 0.01) {
  # Determine m dynamically
  if (is.null(target_neutral_ratio)) {
    target_neutral_ratio <- neutral_percentage  # Use global neutral percentage if not provided
  }
  optimal_m <- find_optimal_m(data, x_var, y_var, target_neutral_ratio, tolerance)
  
  if (is.na(optimal_m)) {
    stop("Optimal m could not be determined. Check your data and ensure no missing or invalid values.")
  }
  
  # Classify points based on their position relative to the neutral zone
  data$Position <- ifelse(
    data[[y_var]] > data[[x_var]] + optimal_m, "Above",
    ifelse(data[[y_var]] < data[[x_var]] - optimal_m, "Below", "Neutral")
  )
  
  # Count the points in each category
  counts <- table(factor(data$Position, levels = c("Above", "Below", "Neutral")))
  points_above <- counts["Above"]
  points_below <- counts["Below"]
  points_neutral <- counts["Neutral"]
  
  # Create the plot
  plot <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(aes(color = Position), size = 3, alpha = 0.8) +  # Scatter points
    geom_abline(slope = 1, intercept = optimal_m, linetype = "dashed", color = "black") +  # Upper bound
    geom_abline(slope = 1, intercept = -optimal_m, linetype = "dashed", color = "black") +  # Lower bound
    scale_color_manual(
      values = c("Above" = "brown", "Below" = "purple", "Neutral" = "blue"),
      labels = c("Above", "Below", "Neutral")
    ) +
    labs(
      title = paste(x_var, "vs", y_var, "with Neutral Zone (dynamic m)"),
      subtitle = paste(
        "m:", round(optimal_m, 3),
        "| Above:", points_above, 
        "| Below:", points_below, 
        "| Neutral:", points_neutral
      ),
      x = x_var,
      y = y_var,
      color = "Position"
    ) +
    theme_gray() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.position = "top"
    )
  
  # Print the plot
  print(plot)
  
  # Return the optimal m and counts
  return(list(optimal_m = optimal_m, counts = counts))
}

# Usage Example
# Assuming "data" is your dataset with "OWI" and "WBI" as the columns
result_dynamic <- plot_with_dynamic_m(data, "OWI", "WBI")
cat("Optimal m for", neutral_percentage * 100, "% neutral points:", result_dynamic$optimal_m, "\n")


######################################################
######################################################
#####            Clustering with data             ####
######################################################
######################################################


# Auswahl der relevanten Spalten: OWI und WBI
owi_wbi_data <- data[, c("OWI", "WBI")]

# Fehlende Werte entfernen
owi_wbi_data <- na.omit(owi_wbi_data)

# Daten skalieren
scaled_data <- scale(owi_wbi_data)

# k-Means Clustering (z.B. 3 Cluster)
set.seed(42)  # Für Reproduzierbarkeit
kmeans_result <- kmeans(scaled_data, centers = 5)

# Cluster zu den Originaldaten hinzufügen
owi_wbi_data$Cluster <- as.factor(kmeans_result$cluster)

# Cluster-Zentren zurückskalieren (falls notwendig)
centers <- as.data.frame(kmeans_result$centers)
colnames(centers) <- colnames(scaled_data)  # Spaltennamen übernehmen

# Zentren wieder in den Originalmaßstab transformieren
centers <- scale(owi_wbi_data[, 1:2], center = attr(scaled_data, "scaled:center"), 
                 scale = attr(scaled_data, "scaled:scale")) %*% diag(rep(1, ncol(owi_wbi_data[, 1:2]))) 

# Umwandlung in DataFrame für ggplot
centers <- as.data.frame(centers)
names(centers) <- c("OWI", "WBI")

# Visualisierung der Cluster
ggplot(owi_wbi_data, aes(x = OWI, y = WBI, color = Cluster)) +
  geom_point(size = 3) +
  geom_point(data = centers, aes(x = OWI, y = WBI), 
             color = "red", size = 4, shape = 4) +
  labs(title = "Cluster-Analyse von OWI und WBI",
       x = "OWI",
       y = "WBI") +
  theme_minimal()







  






