############### FFCW PROJECT #################
#### Title:  FFCW                         ###       
#### Goal:   Make plots for the paper     ###      
#### Date:   01.12.2023                   ###                  
#### Author: Yayouk Willems               ###      
#############################################

############################# 1) Load Packages #############################
install.packages(c("tidyr", "ggplot2", "dplyr", "gridExtra"))
library(tidyr)
library(ggplot2)
library(dplyr)
library(gridExtra)

### Data
#Data points are based on results from Latent Change Models , see supplementary tables

############################# 2) Plot Delta and Intercept DNAm - Race (Black Vs. White) #############################

##### 2.1) DunedinPACE Intercept - Race (Black Vs. White) ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.229, 0.195, 0.226, 0.154, 0.141),
  Result1_SD =         c(0.030, 0.031, 0.036, 0.033, 0.03)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggDP <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
  geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkred", alpha = 0.5) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkred", alpha = 0.5, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
  labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
  facet_wrap(~Result, ncol = 3) +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10
    ))

print(ggDP)

##### 2.1) GrimAge Acceleration Intercept - Race (Black Vs. White) ####

# GrimAGE Intercept Black/White
data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.128, 0.138, 0.127, 0.074, 0.082),
  Result1_SD =         c(0.028, 0.029, 0.032, 0.030, 0.032)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggGA <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
  geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkgreen", alpha = 0.5) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkgreen", alpha = 0.5, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
  labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
  facet_wrap(~Result, ncol = 3) +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10
    ))

print(ggGA)

##### 2.3) PhenoAge Acceleration Intercept - Race (Black Vs. White) ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.397, 0.378, 0.397, 0.370, 0.355),
  Result1_SD =         c(0.027, 0.028, 0.032, 0.030, 0.031)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggPA <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
  geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkblue", alpha = 0.5) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkblue", alpha = 0.5, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
  labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
  facet_wrap(~Result, ncol = 3) +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10
    ))

print(ggPA)

##### 2.4) Group Intercept Plots DNAm - Race (Black Vs. White) ####

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggDP <- ggDP +
  labs(title = "DP")

ggGA <- ggGA +
  labs(title = "GA") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

ggPA <- ggPA +
  labs(title = "PA") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

# Print the plots
print(ggDP)
print(ggGA)
print(ggPA)

grid.arrange(ggDP, ggGA, ggPA, ncol = 3)

##### 2.5) DunedinPACE Delta - Race (Black Vs. White) ####

# DunedinPACE DELTA Black/White
data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.158, 0.135, 0.176, 0.140, 0.137),
  Result1_SD =         c(0.029, 0.029, 0.033, 0.030, 0.032)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggDP_d <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
  geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkred", alpha = 0.5) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkred", alpha = 0.5, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
  labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
  facet_wrap(~Result, ncol = 3) +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10
    ))

print(ggDP_d)

##### 2.6) GrimAge Acceleration Delta - Race (Black Vs. White) ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.075, 0.048, 0.062, 0.082, 0.053),
  Result1_SD =         c(0.030, 0.031, 0.036, 0.033, 0.034)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggGA_d <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
  geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkgreen", alpha = 0.5) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkgreen", alpha = 0.5, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
  labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
  facet_wrap(~Result, ncol = 3) +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10
    ))

print(ggGA_d)


##### 2.7) PhenoAge Acceleration Delta - Race (Black Vs. White) ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.181, 0.153, 0.189, 0.185, 0.182),
  Result1_SD =         c(0.031, 0.032, 0.037, 0.034, 0.035)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggPA_d <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
  geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkblue", alpha = 0.5) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkblue", alpha = 0.5, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
  labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
  facet_wrap(~Result, ncol = 3) +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10
    ))

print(ggPA_d)


##### 2.8) Group Delta Plots DNAm - Race (Black Vs. White) ####

# DunedinPACE on one axes

ggDP <- ggDP +
  labs(title = "DP intercept")

ggDP_d <- ggDP_d +
  labs(title = "DP delta") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

grid.arrange(ggDP, ggDP_d, ncol = 3)

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggDP_d <- ggDP_d +
  labs(title = "DP")

ggGA_d <- ggGA_d +
  labs(title = "GA") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

ggPA_d <- ggPA_d +
  labs(title = "PA") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

# Print the plots
print(ggDP_d)
print(ggGA_d)
print(ggPA_d)

grid.arrange(ggDP_d, ggGA_d, ggPA_d, ncol = 3)


############################# 3) Plot Delta and Intercept DNAm-Racial Segregation (Theil Index) #############################

##### 3.1) DunedinPACE Intercept - Racial Segregation ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.067, 0.058, 0.057, 0.032, -0.007),
  Result1_SD =         c(0.023, 0.023, 0.027, 0.023, 0.026)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggDP_r <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
  geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkred", alpha = 0.5) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkred", alpha = 0.5, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
  labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
  facet_wrap(~Result, ncol = 3) +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10
    ))

print(ggDP_r)

##### 3.2) GrimAge Acceleration Intercept - Racial Segregation ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.062, 0.060, 0.040, 0.039, 0.027),
  Result1_SD =         c(0.021, 0.021, 0.023, 0.021, 0.023)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggGA_r <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
  geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkgreen", alpha = 0.5) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkgreen", alpha = 0.5, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
  labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
  facet_wrap(~Result, ncol = 3) +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10
    ))

print(ggGA_r)

##### 3.3) PhenoAge Acceleration Intercept - Racial Segregation ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.182, 0.166, 0.186, 0.158, 0.124),
  Result1_SD =         c(0.022, 0.022, 0.025, 0.022, 0.024)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggPA_r <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
  geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkblue", alpha = 0.5) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkblue", alpha = 0.5, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
  labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
  facet_wrap(~Result, ncol = 3) +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10
    ))

print(ggPA_d)

##### 3.4) Group Intercept Plots DNAm - Racial segregation ####

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggDP_r <- ggDP_r +
  labs(title = "DP")

ggGA_r <- ggGA_r +
  labs(title = "GA") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

ggPA_r <- ggPA_r +
  labs(title = "PA") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

# Print the plots
print(ggDP_r)
print(ggGA_r)
print(ggPA_r)

grid.arrange(ggDP_r, ggGA_r, ggPA_r, ncol = 3)

##### 3.5) DunedinPACE Delta - Racial Segregation ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.066, 0.056, 0.055, 0.049, 0.031),
  Result1_SD =         c(0.021, 0.021, 0.024, 0.021, 0.023)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggDP_Dr <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
  geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkred", alpha = 0.5) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkred", alpha = 0.5, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
  labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
  facet_wrap(~Result, ncol = 3) +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10
    ))

print(ggDP_Dr)

##### 3.6) GrimAge Acceleration Delta - Racial Segregation ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.040, 0.030, 0.055, 0.030, 0.010),
  Result1_SD =         c(0.022, 0.023, 0.026, 0.023, 0.025)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggGA_Dr <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
  geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkgreen", alpha = 0.5) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkgreen", alpha = 0.5, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
  labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
  facet_wrap(~Result, ncol = 3) +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10
    ))

print(ggGA_Dr)

##### 3.6) PhenoAge Acceleration Delta - Racial Segregation ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.078, 0.072, 0.095, 0.073, 0.061),
  Result1_SD =         c(0.023, 0.023, 0.026, 0.023, 0.025)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggPA_Dr <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
  geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkblue", alpha = 0.5) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkblue", alpha = 0.5, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
  labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
  facet_wrap(~Result, ncol = 3) +
  scale_x_discrete(labels = new_labels) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10
    ))

print(ggPA_Dr)

##### 3.7) Group Delta Plots DNAm - Racial segregation ####

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggDP_Dr <- ggDP_Dr +
  labs(title = "DP")

ggGA_Dr <- ggGA_Dr +
  labs(title = "GA") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

ggPA_Dr <- ggPA_Dr +
  labs(title = "PA") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

# Print the plots
print(ggDP_Dr)
print(ggGA_Dr)
print(ggPA_Dr)

grid.arrange(ggDP_Dr, ggGA_Dr, ggPA_Dr, ncol = 3)


############################# 4) Plot Delta and Intercept DNAm - Skin Tone #############################

#only within racialized groups
#only includes Black, LatinX and Mixed race kids
#sample size much smaller

##### 4.1) DunedinPACE Intercept - Skin Tone ####
data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.088, 0.073, 0.132, 0.074, 0.065),
  Result1_SD =         c(0.039, 0.038, 0.043, 0.039, 0.039)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggDP_skin <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
                geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
                geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
                geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkred", alpha = 0.5) +
                geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkred", alpha = 0.5, size = 0.5) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
                labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
                facet_wrap(~Result, ncol = 3) +
                scale_x_discrete(labels = new_labels) +
                scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
                theme_minimal() +
                theme(
                  axis.text.x = element_text(angle = 90, hjust = 1),
                  panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank(),
                  legend.position = "top",
                  legend.title = element_text(size = 12, face = "bold"),
                  legend.text = element_text(size = 10
                  ))

print(ggDP_skin)

###### 4.2) GrimAge Acceleration  Intercept - Skin Tone ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.001, -0.010, -0.004, -0.027, -0.007),
  Result1_SD =         c(0.035, 0.036, 0.039, 0.036, 0.036)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggGA_skin <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
            geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
            geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
            geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkgreen", alpha = 0.5) +
            geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkgreen", alpha = 0.5, size = 0.5) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
            labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
            facet_wrap(~Result, ncol = 3) +
            scale_x_discrete(labels = new_labels) +
            scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 90, hjust = 1),
              panel.background = element_rect(fill = "white"),
              panel.grid = element_blank(),
              legend.position = "top",
              legend.title = element_text(size = 12, face = "bold"),
              legend.text = element_text(size = 10
              ))

print(ggGA_skin)

###### 4.3) PhenoAge Acceleration Intercept - Skin Tone  ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.273, 0.266, 0.267, 0.261, 0.258),
  Result1_SD =         c(0.035, 0.036, 0.040, 0.036, 0.036)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggPA_skin <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
           geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
           geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
           geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkblue", alpha = 0.5) +
           geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkblue", alpha = 0.5, size = 0.5) +
           geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
           labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
           facet_wrap(~Result, ncol = 3) +
           scale_x_discrete(labels = new_labels) +
           scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
           theme_minimal() +
           theme(
             axis.text.x = element_text(angle = 90, hjust = 1),
             panel.background = element_rect(fill = "white"),
             panel.grid = element_blank(),
             legend.position = "top",
             legend.title = element_text(size = 12, face = "bold"),
             legend.text = element_text(size = 10
             ))

print(ggPA_skin)

##### 4.4) Group Intercept Plots DNAm - Skin Tone ####

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggDP_skin <- ggDP_skin +
  labs(title = "DP")

ggGA_skin <- ggGA_skin +
  labs(title = "GA") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

ggPA_skin <- ggPA_skin +
  labs(title = "PA") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

# Print the plots
print(ggDP_skin)
print(ggGA_skin)
print(ggPA_skin)

grid.arrange(ggDP_skin, ggGA_skin, ggPA_skin, ncol = 3)

##### 4.4) DunedinPACE Delta - Skin Tone ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.053, 0.053, 0.038, 0.057, 0.059),
  Result1_SD =         c(0.034, 0.035, 0.039, 0.035, 0.035)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggDP_skin_D <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
                 geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
                 geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
                 geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkred", alpha = 0.5) +
                 geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkred", alpha = 0.5, size = 0.5) +
                 geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
                 labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
                 facet_wrap(~Result, ncol = 3) +
                 scale_x_discrete(labels = new_labels) +
                 scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
                 theme_minimal() +
                 theme(
                   axis.text.x = element_text(angle = 90, hjust = 1),
                   panel.background = element_rect(fill = "white"),
                   panel.grid = element_blank(),
                   legend.position = "top",
                   legend.title = element_text(size = 12, face = "bold"),
                   legend.text = element_text(size = 10
                   ))

print(ggDP_skin_D)


###### 4.5) GrimAge Acceleration  Delta - Skin Tone ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.067, 0.061, 0.047, 0.070, 0.061),
  Result1_SD =         c(0.036, 0.037, 0.041, 0.037, 0.037)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggGA_skin_D <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
                geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
                geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
                geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkgreen", alpha = 0.5) +
                geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkgreen", alpha = 0.5, size = 0.5) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
                labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
                facet_wrap(~Result, ncol = 3) +
                scale_x_discrete(labels = new_labels) +
                scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
                theme_minimal() +
                theme(
                  axis.text.x = element_text(angle = 90, hjust = 1),
                  panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank(),
                  legend.position = "top",
                  legend.title = element_text(size = 12, face = "bold"),
                  legend.text = element_text(size = 10
                  ))

print(ggGA_skin_D)

###### 4.6) PhenoAge Acceleration Delta - Skin Tone  ####

data <- data.frame(
  Group = c("A", "B", "C", "D", "E"),
  Result1_EffectSize = c(0.113, 0.105, 0.101, 0.116, 0.110),
  Result1_SD =         c(0.038, 0.038, 0.041, 0.038, 0.038)
)

# Reshape the data from wide to long format
data_long <- data %>%
  gather(Result, Value, -Group)

# Extract the Result number and variable name
data_long <- separate(data_long, Result, into = c("Result", "Variable"), sep = "_")

# Create separate dataframes for effect sizes and standard deviations
data_effect_size <- data_long[data_long$Variable == "EffectSize", ]
data_sd <- data_long[data_long$Variable == "SD", ]

# Calculate confidence intervals for effect sizes
data_effect_size$CI_lower <- data_effect_size$Value - 1.96 * data_sd$Value
data_effect_size$CI_upper <- data_effect_size$Value + 1.96 * data_sd$Value

# Define a vector of new labels
new_labels <- c("Main Effect", "Controlling for Postnatal", "Controlling for Perinatal", "Controlling for SES", "Controlling for Neighborhood")

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggPA_skin_D <- ggplot(data_effect_size, aes(x = Group, y = Value, ymin = CI_lower, ymax = CI_upper)) +
                geom_hline(yintercept = seq(-1, 1, by = 0.1), color = "gray", linetype = "dotted", size = 0.5) +  # Add horizontal grid lines
                geom_vline(xintercept = seq(1, nrow(data_effect_size), by = 1), color = "gray", linetype = "dotted", size = 0.5) +  # Add vertical dotted lines for each point
                geom_point(position = position_dodge(width = 0.2), size = 4, shape = 19, color = "darkblue", alpha = 0.5) +
                geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, color = "darkblue", alpha = 0.5, size = 0.5) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
                labs(x = "Group", y = "Effect Size", title = "Effect Sizes with Confidence Intervals") +
                facet_wrap(~Result, ncol = 3) +
                scale_x_discrete(labels = new_labels) +
                scale_y_continuous(breaks = seq(-1, 1, by = 0.1), limits = c(-0.2, 0.5)) +  # Set y-axis limits to -0.2 to 1
                theme_minimal() +
                theme(
                  axis.text.x = element_text(angle = 90, hjust = 1),
                  panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank(),
                  legend.position = "top",
                  legend.title = element_text(size = 12, face = "bold"),
                  legend.text = element_text(size = 10
                  ))

print(ggPA_skin_D)

##### 4.7) Group Delta Plots DNAm - Skin tone ####

# Create a scatter plot with points, error bars, horizontal lines at 0 and grid lines, and vertical dotted lines for each point
ggDP_skin_D <- ggDP_skin_D +
  labs(title = "DP")

ggGA_skin_D <- ggGA_skin_D +
  labs(title = "GA") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

ggPA_skin_D <- ggPA_skin_D +
  labs(title = "PA") +
  theme(axis.title.y = element_blank())  # Hide y-axis label

# Print the plots
print(ggDP_skin_D)
print(ggGA_skin_D)
print(ggPA_skin_D)

grid.arrange(ggDP_skin_D, ggGA_skin_D, ggPA_skin_D, ncol = 3)

############################# 5) Plot Interaction effects #############################

Data_DNAm <- read.csv("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/forMplus_DNAmOnly_01122023.csv")

#only include White and Black youth and create a seperate variable 
Data_BlWh <- subset(Data_DNAm, ck6ethrace == 1 | ck6ethrace == 2)

Data_BlWh$BlWh <- Data_BlWh$ck6ethrace
Plot1 <- ggplot(data = Data_BlWh, aes(x = scale(Ngh_overall_NEW), y = scale(PCGrim_9_accell_res_array_Std9), color = factor(BlWh)))+ 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Neighborhood SES", y = "GrimAge Acceleration age 9", color = "Age") +
  scale_color_manual(values = c("red3", "royalblue3"), name = "Race/Ethnicity", labels = c("White", "Black")) +
  ylim(-2, 3) +
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16) ,
        panel.background = element_blank())
  
print(Plot1)

#associations between intercept and delta per race group are calculated in Mplus , see Mplus scripts

