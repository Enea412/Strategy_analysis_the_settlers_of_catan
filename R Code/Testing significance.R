####################################
##### testing significance    ######
####################################

#######################
####army vs. road###### no significant difference: p_value = 0.05178
#######################


# Berechnung der Gesamtanzahl der Siege für jede Kategorie
n_army <- sum(data$army / 2)  # Gesamtanzahl für "Grösste Rittermacht"
n_road <- sum(data$road / 2)  # Gesamtanzahl für "Längste Strasse"

# Gesamtanzahl der beobachteten Ereignisse
total <- n_army + n_road

# Durchführung des Binomialtests
binom.test(x = n_road, n = total, p = 0.5)


####################################
######above or below OWI=WBI######## no significant difference p=0.08051
####################################

A
# points above and below OWI=WBI
n_above <- 143
n_below <- 114
n_total <- n_above + n_below

# Binomial test
binom_test <- binom.test(n_above, n_total, p = 0.5, alternative = "two.sided")

print(binom_test)


####################################
######above or below F_Neutral###### no significant difference P=value: 0.08268
####################################

A
# number of points above or below F_Neutral (m = 2)
n_above <- 76
n_below <- 100
n_total <- n_above + n_below

# Binomial test
binom_test <- binom.test(n_above, n_total, p = 0.5, alternative = "two.sided")

#print
print(binom_test)



####################################
######Just OWI or Just WBI    ###### Significantly more just OWI strategy p_value = 0.02203
####################################

# count extreme strategies
bottom <- 49
left <- 28
n_total <- left + bottom

# Binomial test
binom_test <- binom.test(left, n_total, p = 0.5, alternative = "two.sided")

#print

print(binom_test) 

###############################################################
######### # Chi-Quadrat-Test for average usage of Ressources### Not significant  (p= 0.7746)
##############################################################

# observed values
observed <- c(Wood = 6.257222, Brick = 5.325863, Ore = 6.089137, Sheep = 6.209407, Wheat = 7.554967)

# expected probability (number of hexes of a Ressource / all Ressource hexces)
expected_prob <- c(Wood = 4/18, Wheat = 4/18, Sheep = 4/18, Ore = 3/18, Brick = 3/18)

# total
total <- sum(observed)

# expected value
expected <- total * expected_prob

chisq.test(x = observed, p = expected_prob)

###############################################
###Chi-Quadrat test for 1st and 2nd settlements#### not significant p_value = 0.8814
###############################################

# observed Werte
observed <- c(Wood = 3.61111, Brick = 3.65185, Ore = 4.051851, Sheep = 4.09629, Wheat = 4.85185)

# expected probability
expected_prob <- c(Wood = 4/18, Wheat = 4/18, Sheep = 4/18, Ore = 3/18, Brick = 3/18)

# number observed
total <- sum(observed)

# calculate expected value
expected <- total * expected_prob


chisq.test(x = observed, p = expected_prob)



#####################################
######for each resource         #####
#####################################


# Observed values
observed <- c(Wood = 6.257222, Brick = 5.325863, Ore = 6.089137, Sheep = 6.209407, Wheat = 7.554967)

# Expected probabilities
expected_prob <- c(Wood = 4/18, Wheat = 4/18, Sheep = 4/18, Ore = 3/18, Brick = 3/18)

# Total number of observations
total <- sum(observed)

# Calculate expected values
expected <- total * expected_prob

# Calculate results for each resource
chi_square_results <- lapply(names(observed), function(resource) {
  # Observed and expected values for the specific resource
  observed_value <- observed[resource]
  expected_value <- expected[resource]
  
  # Calculate chi-square statistic
  chi_square_stat <- (observed_value - expected_value)^2 / expected_value
  
  # Calculate p-value (df = 1, as there is only one category)
  p_value <- pchisq(chi_square_stat, df = 1, lower.tail = FALSE)
  
  # Return result as a list
  list(Chi_Square_Stat = chi_square_stat, P_Value = p_value)
})

# Display results
names(chi_square_results) <- names(observed)
chi_square_results

