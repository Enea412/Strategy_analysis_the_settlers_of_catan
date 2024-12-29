
################################################
################################################
######   Calculating Mean         ##############
################################################
################################################

###########################################
####   Calculate mean of each column  #####
###########################################

# Load data
library(data.table)

data <- fread("Daten_Bachelorarbeit.csv", header = TRUE)

data[is.na(data)] <- 0


# List of all colomn names
spalten <- c("Ore n-1", "Wheat n-1", "Sheep n-1", "Brick n-1", "Wood n-1", 
             "Ore n0", "Wheatn0", "Sheep n0", "Brick n0", "Wood n0", 
             "Ore n1", "Wheat n1", "Sheep n1", "Brick n1", "Wood n1", 
             "Ore n2", "Wheat n2", "Sheep n2", "Brick n2", "Wood n2", 
             "Ore n3", "Wheat n3", "Sheep n3", "Brick n3", "Wood n3", 
             "settlements", "cities", "VP-card(s)", "army", "road")

# Loop over all columns to calculate the average
for (spalte in spalten) {
  mean_value <- mean(data[[spalte]], na.rm = TRUE)
  
  # replace "" with _ 
  variable_name <- gsub(" ", "_", spalte)   
  variable_name <- gsub("-", "_", variable_name) 
  
  # assign mean
  assign(variable_name, mean_value)
}


######################################
###### 1st and 2nd Settlement Mean####
######################################

##ore
first_settlement_mean_ore <- Ore_n_1+Ore_n0

##wheat
first_settlement_mean_wheat <- Wheat_n_1+Wheatn0

##sheep
first_settlement_mean_sheep <- Sheep_n_1+Sheep_n0

##wood

first_settlement_mean_wood <- Wood_n_1 + Wood_n0

##brick

first_settlement_mean_brick <- Brick_n_1+ Brick_n0

########################################
######Weighted ressource Value##########
########################################

lambda <- 0.7
Wheat_n_1

##Wheat
weighted_mean_Wheat <- function(){
  weighted_mean <- Wheat_n_1 + Wheatn0 + 
    (Wheat_n1 * lambda) + 
    (Wheat_n2 * lambda^2) + 
    (Wheat_n3 * lambda^3)
  return(weighted_mean)
}

print(weighted_mean_Wheat())

weighted_mean_Ore <- function() {
  weighted_mean <- Ore_n_1 + Ore_n0 + 
    (Ore_n1 * lambda) + 
    (Ore_n2 * lambda^2) + 
    (Ore_n3 * lambda^3)
  
  return(weighted_mean)
}

## sheep

weighted_mean_Sheep <- function() {
  weighted_mean <- Sheep_n_1 + Sheep_n0 + 
    (Sheep_n1 * lambda) + 
    (Sheep_n2 * lambda^2) + 
    (Sheep_n3 * lambda^3)
  
  return(weighted_mean)
}

## Brick

weighted_mean_Brick <- function() {
  weighted_mean <- Brick_n_1 + Brick_n0 + 
    (Brick_n1 * lambda) + 
    (Brick_n2 * lambda^2) + 
    (Brick_n3 * lambda^3)
  
  return(weighted_mean)
}

##Wood

weighted_mean_Wood <- function() {
  weighted_mean <- Wood_n_1 + Wood_n0 + 
    (Wood_n1 * lambda) + 
    (Wood_n2 * lambda^2) + 
    (Wood_n3 * lambda^3)
  
  return(weighted_mean)
}

print(weighted_mean_Wood())
print(weighted_mean_Brick())
print(weighted_mean_Ore())
print(weighted_mean_Sheep())
print(weighted_mean_Wheat())


########################################
######    Army vs. Road       ##########
########################################

## here the mean point being achived by winners through either biggest army or longest road is being calculated 

mean_road <- mean(data$road, na.rm = TRUE) /2

mean_army <- mean(data$army, na.rm = TRUE) / 2

print(mean_road)
print(mean_army)
########################################
####  Mean settlements and cities  #####
########################################


mean_settlements <- mean(data$settlements, na.rm = TRUE)

mean_cities <- mean(data$cities, na.rm = TRUE) / 2  # divided by two, because in the table the point achieved for a city are being listed, which is two per city




#####################################################################################################
##Creating a column for each variable: P_wheat, P_Ore, P_Brick, P_Wood, as mentioned in the thesis###
#####################################################################################################

# this Function calculates the weighted Resource x being used for each column. the mean of P_Resource equals weighted_mean_Resource

##P_wheat

data$P_Wheat <- data[["Wheat n-1"]] + 
  data[["Wheatn0"]] + 
  (data[["Wheat n1"]] * lambda) + 
  (data[["Wheat n2"]] * lambda^2) + 
  (data[["Wheat n3"]] * lambda^3)

## P_Ore
data$P_Ore <-  data[["Ore n-1"]] + 
  data[["Ore n0"]] + 
  (data[["Ore n1"]] * lambda) + 
  (data[["Ore n2"]] * lambda^2) + 
  (data[["Ore n3"]] * lambda^3)

##P_Brick

data$P_Brick <-  data[["Brick n-1"]] + 
  data[["Brick n0"]] + 
  (data[["Brick n1"]] * lambda) + 
  (data[["Brick n2"]] * lambda^2) + 
  (data[["Brick n3"]] * lambda^3)

##P_Wood
data$P_Wood <-  data[["Wood n-1"]] + 
  data[["Wood n0"]] + 
  (data[["Wood n1"]] * lambda) + 
  (data[["Wood n2"]] * lambda^2) + 
  (data[["Wood n3"]] * lambda^3)

##P_Sheep
data$P_Sheep <-  data[["Sheep n-1"]] + 
  data[["Sheep n0"]] + 
  (data[["Sheep n1"]] * lambda) + 
  (data[["Sheep n2"]] * lambda^2) + 
  (data[["Sheep n3"]] * lambda^3)


###############################################
#######Ressources, which werent used at all####
###############################################

# Funktion zum Zählen der Nullen in einer angegebenen Spalte
count_zeros_column <- function(data, column_name) {
  # Zähle, wie viele Nullen in der angegebenen Spalte sind
  zero_count <- sum(data[[column_name]] == 0)
  
  # Rückgabe der Anzahl der Nullen
  return(zero_count)
}

# Liste der Spaltennamen
columns_to_check <- c("P_Wheat", "P_Ore", "P_Wood", "P_Sheep", "P_Brick")

# Schleife über alle Spalten und Ausgabe der Anzahl der Nullen
for (col in columns_to_check) {
  zero_count <- count_zeros_column(data, col)
  print(paste(col, "->", zero_count, "mal nicht gebraucht"))
}
 
#####################################
#######Ploting most used Ressource###
#####################################

# Durchschnittswerte berechnen
average_usage <- colMeans(data.frame(data$P_Wheat, data$P_Brick, data$P_Ore, data$P_Wood, data$P_Sheep))

print(average_usage)
# Datenrahmen für ggplot2 erstellen
average_df <- data.frame(
  resource = c("Wheat", "Brick", "Ore", "Wood", "Sheep"),
  average_value = as.numeric(average_usage)
)

# ggplot2-Plot erstellen
library(ggplot2)

ggplot(average_df, aes(x = resource, y = average_value, fill = resource)) +
  geom_bar(stat = "identity") +  # Balkendiagramm
  theme_minimal() +              # Minimaler Stil
  labs(
    title = "Durchschnittliche Nutzung der Ressourcen",
    x = "Ressource",
    y = "Durchschnittliche Nutzung",
    fill = "Ressource"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Schöne Farbpalette
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Beschriftung drehen
  )



###################################################
######Adjusted for the number of hexes available###
###################################################

# Durchschnittswerte berechnen (mit Gewichtung für Brick und Ore)
average_usage <- colMeans(data.frame(data$P_Wheat, data$P_Brick * 4/3, data$P_Ore * 4/3, data$P_Wood, data$P_Sheep))

average_df <- data.frame(
  resource = c("Wheat", "Brick", "Ore", "Wood", "Sheep"),
  average_value = as.numeric(average_usage)
)

# Prozentwerte berechnen
average_df$percent <- (average_df$average_value / sum(average_df$average_value)) * 100

# ggplot2-Plot erstellen
library(ggplot2)

ggplot(average_df, aes(x = resource, y = average_value, fill = resource)) +
  geom_bar(stat = "identity") +  # Balkendiagramm
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),  # Prozentwerte als Text
    vjust = -0.5  # Text oberhalb der Balken
  ) +
  theme_minimal() +              # Minimaler Stil
  labs(
    title = "Wertvollsten Ressourcen",
    x = "Ressource",
    y = "Bereinigte P Werte",
    fill = "Ressource"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Schöne Farbpalette
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Beschriftung drehen
  )




###################################################
#######  1st and 2nd Settlement mean Resource   ###
###################################################

# Durchschnittswerte berechnen
average_usage <- colMeans(data.frame(data$`Wheat n-1`+ data$Wheatn0, data$`Brick n-1`+ data$`Brick n0`, data$`Sheep n-1`+data$`Sheep n0`, data$`Ore n-1`+data$`Ore n0`, data$`Wood n-1`+data$`Wood n0`))

average_df <- data.frame(
  resource = c("Wheat", "Brick", "Ore", "Wood", "Sheep"),
  average_value = as.numeric(average_usage)
)

# Prozentwerte berechnen
average_df$percent <- (average_df$average_value / sum(average_df$average_value)) * 100

# ggplot2-Plot erstellen
library(ggplot2)

ggplot(average_df, aes(x = resource, y = average_value, fill = resource)) +
  geom_bar(stat = "identity") +  # Balkendiagramm
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),  # Prozentwerte als Text
    vjust = -0.5  # Text oberhalb der Balken
  ) +
  theme_minimal() +              # Minimaler Stil
  labs(
    title = "Ressourcen der Erst- und Zweitsiedlungen ",
    x = "Ressource",
    y = "Durchschnittliche Nutzung",
    fill = "Ressource"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Schöne Farbpalette
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Beschriftung drehen
  )

# Werte für jede Ressource ausgeben
cat(paste("Weizen ->", average_df$average_value[1], "\n"))
cat(paste("Ziegel ->", average_df$average_value[2], "\n"))
cat(paste("Erz ->", average_df$average_value[3], "\n"))
cat(paste("Holz ->", average_df$average_value[4], "\n"))
cat(paste("Schaf ->", average_df$average_value[5], "\n"))

