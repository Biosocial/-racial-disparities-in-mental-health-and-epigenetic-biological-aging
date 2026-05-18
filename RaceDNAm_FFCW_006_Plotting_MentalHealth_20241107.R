############### FFCW PROJECT #################
#### Title:  FFCW                         ###       
#### Goal:   Data PLotting                ###      
#### Date:   07.11.2024                   ###                  
#### Author: Deniz Fraemke               ###      
#############################################

#First determine directory on where to save the plots
dir_plot = "~"

# Plot Internalizing Model implied

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define ages
ages <- c(3, 5, 7, 9, 11, 13, 15)

# Model intercepts and standard errors
mu_I <- 0.824
se_I <- 0.007
mu_S1 <- -0.032
se_S1 <- 0.004
mu_S2 <- 0.045
# Note: SE of S2 is excluded per your request

# Race group coefficients and standard errors
race_effects <- list(
  White = list(I = 0,        se_I = 0,     S1 = 0,        se_S1 = 0,     S2 = 0),
  Black = list(I = 0.037,    se_I = 0.004, S1 = -0.019,   se_S1 = 0.003, S2 = 0.008),
  Latinx = list(I = 0.045, se_I = 0.005, S1 = -0.014,   se_S1 = 0.004, S2 = 0.001)
)

# Initialize data frame
df_int <- data.frame()

# Calculate MH scores and confidence intervals for each race group
for (race in names(race_effects)) {
  # Expected latent variables for the race group
  E_I  <- mu_I  + race_effects[[race]]$I
  E_S1 <- mu_S1 + race_effects[[race]]$S1
  E_S2 <- mu_S2 + race_effects[[race]]$S2  # S2 included in mean but SE ignored
  
  # Standard errors of the latent variables
  SE_I  <- sqrt(se_I^2  + race_effects[[race]]$se_I^2)
  SE_S1 <- sqrt(se_S1^2 + race_effects[[race]]$se_S1^2)
  # SE_S2 is excluded from SE calculations
  
  # Calculate MH scores and their standard errors at each age
  MH_data <- lapply(ages, function(age) {
    # Compute MH_mean including S2
    if (age == 3) {
      MH_mean <- E_I
      MH_se <- SE_I
    } else if (age == 5) {
      MH_mean <- E_I + E_S1 * 1
      MH_se <- sqrt(SE_I^2 + (1 * SE_S1)^2)
    } else if (age == 7) {
      MH_mean <- E_I + E_S1 * 2
      MH_se <- sqrt(SE_I^2 + (2 * SE_S1)^2)
    } else if (age == 9) {
      MH_mean <- E_I + E_S1 * 3 + E_S2 * 1
      MH_se <- sqrt(SE_I^2 + (3 * SE_S1)^2)
    } else if (age == 11) {
      MH_mean <- E_I + E_S1 * 4 + E_S2 * 2
      MH_se <- sqrt(SE_I^2 + (4 * SE_S1)^2)
    } else if (age == 13) {
      MH_mean <- E_I + E_S1 * 5 + E_S2 * 3
      MH_se <- sqrt(SE_I^2 + (5 * SE_S1)^2)
    } else if (age == 15) {
      MH_mean <- E_I + E_S1 * 6 + E_S2 * 4
      MH_se <- sqrt(SE_I^2 + (6 * SE_S1)^2)
    }
    # Compute 95% confidence intervals
    MH_lower <- MH_mean -  MH_se
    MH_upper <- MH_mean +  MH_se
    return(data.frame(Age = age, MH = MH_mean, MH_lower = MH_lower, MH_upper = MH_upper))
  })
  
  # Combine data and add race group
  temp_df <- do.call(rbind, MH_data)
  temp_df$Race <- race
  df_int <- rbind(df_int, temp_df)
}

# Define the colors
race_colors <- c("#377EB8", "#4DAF4A", "#E41A1C")

 # 

# Plot the MH scores over age for each race group with confidence intervals
plot_int_model <-  ggplot(df_int, aes(x = Age, y = MH, color = Race, fill = Race)) +
                geom_line(size = 1.2) +
                geom_ribbon(aes(ymin = MH_lower, ymax = MH_upper), alpha = 0.2, color = NA)+
                geom_point(size = 2) +
                scale_x_continuous(breaks = c(3, 5, 7, 9, 11, 13, 15)) +
  scale_color_manual(name = "Race", values = race_colors) +
  scale_fill_manual(name = "Race", values = race_colors) + 
  theme_minimal()+
                labs(
                 # title = "Expected Log Mental Health Score over Age by Race",
                  x = "Age",
                  y = "Log-transformed mean Internalizing score"
                ) +
                theme(
                  legend.title = element_blank()+
                  theme(
                    axis.title.x = element_text(size = 7),
                    axis.title.y = element_text(size = 7),
                    axis.text.x = element_text(size = 7),
                    axis.text.y = element_text(size = 7),
                    legend.title = element_text(size =7),
                    legend.text = element_text(size = 7)
                  )
                )  
plot_int_model


ggsave(paste0(dir_plot,"Aikins_Fig1C_Int_Model_implied_",format(Sys.Date(),"%Y%m%d" ),".pdf"),plot_int_model, width = 15, height = 15, units = "cm")
 


# Plot Internalizing & sex-stratified - Model implied

# Define ages
ages <- c(3, 5, 7, 9, 11, 13, 15)
s
# Overall means and standard error
mu_I <- 0.815
se_mu_I <- 0.013
mu_S1 <- -0.019
se_mu_S1 <- 0.009
mu_S2 <- 0.022
se_mu_S2 <- 0.012

# Define race groups and their effects
race_groups <- c("White", "Black", "Latinx")
race_effects <- list(
  White = list(I = 0, se_I = 0, S1 = 0, se_S1 = 0, S2 = 0, se_S2 = 0),
  Black = list(I = 0.051, se_I = 0.016, S1 = -0.041, se_S1 = 0.010, S2 = 0.046, se_S2 = 0.014),
  Latinx = list(I = 0.052, se_I = 0.018, S1 = -0.018, se_S1 = 0.012, S2 = 0.013, se_S2 = 0.016)
)

# Define sex groups and their effects
sex_groups <- c("Male", "Female")
sex_effects <- list(
  Male = list(I = 0, se_I = 0, S1 = 0, se_S1 = 0, S2 = 0, se_S2 = 0),
  Female = list(I = -0.001, se_I = 0.008, S1 = -0.008, se_S1 = 0.006, S2 = 0.020, se_S2 = 0.008)
)

# Interaction effects
interaction_effects <- list(
  "Black_Female" = list(I = -0.009, se_I = 0.010, S1 = 0.015, se_S1 = 0.007, S2 = -0.025, se_S2 = 0.009),
  "Latinx_Female" = list(I = -0.004, se_I = 0.011, S1 = 0.002, se_S1 = 0.007, S2 = -0.008, se_S2 = 0.010)
)

# Initialize data frame
df<- data.frame()

# Calculate MH scores and confidence intervals for each combination of race and sex
for (sex in sex_groups) {
  for (race in race_groups) {
    # Base effects
    race_eff <- race_effects[[race]]
    sex_eff <- sex_effects[[sex]]
    
    # Interaction key
    interaction_key <- paste0(race, "_", sex)
    if (interaction_key %in% names(interaction_effects)) {
      interaction_eff <- interaction_effects[[interaction_key]]
    } else {
      interaction_eff <- list(I = 0, se_I = 0, S1 = 0, se_S1 = 0, S2 = 0, se_S2 = 0)
    }
    
    # Expected latent variables for the group
    E_I <- mu_I + race_eff$I + sex_eff$I + interaction_eff$I
    E_S1 <- mu_S1 + race_eff$S1 + sex_eff$S1 + interaction_eff$S1
    E_S2 <- mu_S2 + race_eff$S2 + sex_eff$S2 + interaction_eff$S2
    
    # Standard errors (assuming independence)
    SE_I <- sqrt(se_mu_I^2 + race_eff$se_I^2 + sex_eff$se_I^2 + interaction_eff$se_I^2)
    SE_S1 <- sqrt(se_mu_S1^2 + race_eff$se_S1^2 + sex_eff$se_S1^2 + interaction_eff$se_S1^2)
    SE_S2 <- sqrt(se_mu_S2^2 + race_eff$se_S2^2 + sex_eff$se_S2^2 + interaction_eff$se_S2^2)
    
    # Calculate MH scores and their standard errors at each age
    MH_data <- lapply(ages, function(age) {
      # Compute MH_mean and MH_se
      if (age == 3) {
        MH_mean <- E_I
        MH_se <- SE_I
      } else if (age == 5) {
        MH_mean <- E_I + E_S1 * 1
        MH_se <- sqrt(SE_I^2 + (1 * SE_S1)^2)
      } else if (age == 7) {
        MH_mean <- E_I + E_S1 * 2
        MH_se <- sqrt(SE_I^2 + (2 * SE_S1)^2)
      } else if (age == 9) {
        MH_mean <- E_I + E_S1 * 3 + E_S2 * 1
        MH_se <- sqrt(SE_I^2 + (3 * SE_S1)^2 )
      } else if (age == 11) {
        MH_mean <- E_I + E_S1 * 4 + E_S2 * 2
        MH_se <- sqrt(SE_I^2 + (4 * SE_S1)^2 )
      } else if (age == 13) {
        MH_mean <- E_I + E_S1 * 5 + E_S2 * 3
        MH_se <- sqrt(SE_I^2 + (5 * SE_S1)^2 )
      } else if (age == 15) {
        MH_mean <- E_I + E_S1 * 6 + E_S2 * 4
        MH_se <- sqrt(SE_I^2 + (6 * SE_S1)^2)
      }
      # Compute 95% confidence intervals
      MH_lower <- MH_mean -  MH_se
      MH_upper <- MH_mean +  MH_se
      return(data.frame(Age = age, MH = MH_mean, MH_lower = MH_lower, MH_upper = MH_upper))
    })
    
    # Combine data and add group information
    temp_df <- do.call(rbind, MH_data)
    temp_df$Race <- race
    temp_df$Sex <- sex
    df <- rbind(df, temp_df)
  }
}


# Define the colors
race_colors <- c("#377EB8", "#4DAF4A", "#E41A1C")

# Plot for Females
df_female <- df %>% filter(Sex == "Female")

plot_int_model_female <- ggplot(df_female, aes(x = Age, y = MH, color = Race, roup = Race, fill = Race)) + 
  geom_line(size = 1.2,linetype = 'dotted') +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = MH_lower, ymax = MH_upper), alpha = 0.2, color = NA)+
  theme_minimal() +
  scale_x_continuous(breaks = c(3, 5, 7, 9, 11, 13, 15)) +
  scale_color_manual(name = "Race", values = race_colors) +
  scale_fill_manual(name = "Race", values = race_colors) + 
  labs(
    title = "(A) Girls ",
    x = "Age",
    y = "Log-transformed Internalizing Score"
  ) +
  theme(
    legend.title = element_blank()+
      theme(
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size =7),
        legend.text = element_text(size = 7)
      )
  )
plot_int_model_female


ggsave(paste0(dir_plot,"Aikins_Fig1C_Int_Girls_Model_implied_",format(Sys.Date(),"%Y%m%d" ),".pdf"),plot_int_model_female, width = 15, height = 15, units = "cm")


# Plot for Males
df_male <- df %>% filter(Sex == "Male")

plot_int_model_male <- ggplot(df_male, aes(x = Age, y = MH, color = Race, roup = Race, fill = Race)) + 
  geom_line(size = 1.2,linetype = 'dashed') +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = MH_lower, ymax = MH_upper), alpha = 0.2, color = NA)+
  theme_minimal() +
  scale_x_continuous(breaks = c(3, 5, 7, 9, 11, 13, 15)) +
  scale_color_manual(name = "Race", values = race_colors) +
  scale_fill_manual(name = "Race", values = race_colors) + 
  labs(
    title = "(B) Boys",
    x = "Age",
    y = "Log-transformed Internalizing Score"
  ) +
  theme(
    legend.title = element_blank()+
      theme(
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size =7),
        legend.text = element_text(size = 7)
      )
  )

ggsave(paste0(dir_plot,"Aikins_Fig1C_Int_Boys_Model_implied_",format(Sys.Date(),"%Y%m%d" ),".pdf"),plot_int_model_male, width = 15, height = 15, units = "cm")


# Plot Externalizing  - Model implied

# Model intercepts and standard errors (Means)
mu_I <- 0.976
se_I <- 0.009
mu_S1 <- -0.087
se_S1 <- 0.005
mu_S2 <- 0.087
se_S2 <- 0.007

# Race group coefficients and standard errors
race_effects <- list(
  White = list(I = 0,       se_I = 0,     S1 = 0,       se_S1 = 0,     S2 = 0,       se_S2 = 0),
  Black = list(I = 0.02,    se_I = 0.007, S1 = -0.008,  se_S1 = 0.004, S2 = 0.011,   se_S2 = 0.005),
  Latinx = list(I = 0.006,  se_I = 0.008, S1 = -0.004,  se_S1 = 0.004, S2 = 0.004,   se_S2 = 0.005)
)

# Initialize data frame
df <- data.frame()

# Calculate MH scores and confidence intervals for each race group
for (race in names(race_effects)) {
  # Expected latent variables for the race group
  E_I  <- mu_I  + race_effects[[race]]$I
  E_S1 <- mu_S1 + race_effects[[race]]$S1
  E_S2 <- mu_S2 + race_effects[[race]]$S2  # S2 included in mean but SE ignored
  
  # Standard errors of the latent variables
  SE_I  <- sqrt(se_I^2  + race_effects[[race]]$se_I^2)
  SE_S1 <- sqrt(se_S1^2 + race_effects[[race]]$se_S1^2)
  # SE_S2 is excluded from SE calculations
  
  # Calculate MH scores and their standard errors at each age
  MH_data <- lapply(ages, function(age) {
    # Compute MH_mean including S2
    if (age == 3) {
      MH_mean <- E_I
      MH_se <- SE_I
    } else if (age == 5) {
      MH_mean <- E_I + E_S1 * 1
      MH_se <- sqrt(SE_I^2 + (1 * SE_S1)^2)
    } else if (age == 7) {
      MH_mean <- E_I + E_S1 * 2
      MH_se <- sqrt(SE_I^2 + (2 * SE_S1)^2)
    } else if (age == 9) {
      MH_mean <- E_I + E_S1 * 3 + E_S2 * 1
      MH_se <- sqrt(SE_I^2 + (3 * SE_S1)^2)
    } else if (age == 11) {
      MH_mean <- E_I + E_S1 * 4 + E_S2 * 2
      MH_se <- sqrt(SE_I^2 + (4 * SE_S1)^2)
    } else if (age == 13) {
      MH_mean <- E_I + E_S1 * 5 + E_S2 * 3
      MH_se <- sqrt(SE_I^2 + (5 * SE_S1)^2)
    } else if (age == 15) {
      MH_mean <- E_I + E_S1 * 6 + E_S2 * 4
      MH_se <- sqrt(SE_I^2 + (6 * SE_S1)^2)
    }
    # Compute 95% confidence intervals
    MH_lower <- MH_mean - MH_se
    MH_upper <- MH_mean + MH_se
    return(data.frame(Age = age, MH = MH_mean, MH_lower = MH_lower, MH_upper = MH_upper))
  })
  
  # Combine data and add race group
  temp_df <- do.call(rbind, MH_data)
  temp_df$Race <- race
  df <- rbind(df, temp_df)
}


# Define the colors
race_colors <- c("#377EB8", "#4DAF4A", "#E41A1C")


# Plot the MH scores over age for each race group with confidence intervals
plot_ext_model <-  ggplot(df, aes(x = Age, y = MH, color = Race, fill = Race)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = MH_lower, ymax = MH_upper), alpha = 0.2, color = NA)+
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(3, 5, 7, 9, 11, 13, 15)) +
  scale_color_manual(name = "Race", values = race_colors) +
  scale_fill_manual(name = "Race", values = race_colors) + 
  theme_minimal()+
  labs(
    # title = "Expected Log Mental Health Score over Age by Race",
    x = "Age",
    y = "Log-transformed mean Externalizing score"
  ) +
  theme(
    legend.title = element_blank()+
      theme(
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size =7),
        legend.text = element_text(size = 7)
      )
  )  
plot_ext_model


ggsave(paste0(dir_plot,"Aikins_Fig1C_Ext_Model_implied_",format(Sys.Date(),"%Y%m%d" ),".pdf"),plot_ext_model, width = 15, height = 15, units = "cm")

# Plot Externalizing & sex-stratified - Model implied


Overall means and standard errors
mu_I <- 0.976
se_mu_I <- 0.009
mu_S1 <- -0.087
se_mu_S1 <- 0.005
mu_S2 <- 0.087
se_mu_S2 <- 0.007

# Variances of latent variables
Var_I <- 0.011
Var_S1 <- 0.001
Var_S2 <- 0.001

# Covariances between latent variables
Cov_I_S1 <- -0.003
Cov_I_S2 <- 0.003
Cov_S1_S2 <- -0.001

# Define race groups and their effects
race_groups <- c("White", "Black", "Latinx")
race_effects <- list(
  White = list(I = 0, se_I = 0, S1 = 0, se_S1 = 0, S2 = 0, se_S2 = 0),
  Black = list(I = 0.020, se_I = 0.007, S1 = -0.008, se_S1 = 0.004, S2 = 0.011, se_S2 = 0.005),
  Latinx = list(I = 0.006, se_I = 0.008, S1 = -0.004, se_S1 = 0.004, S2 = 0.004, se_S2 = 0.005)
)

# Define sex groups and their effects
sex_groups <- c("Male", "Female")
sex_effects <- list(
  Male = list(I = 0, se_I = 0, S1 = 0, se_S1 = 0, S2 = 0, se_S2 = 0),
  Female = list(I = -0.021, se_I = 0.005, S1 = 0.002, se_S1 = 0.003, S2 = -0.001, se_S2 = 0.003)
)


# Initialize data frame
df_ext <- data.frame()

# Calculate MH scores and confidence intervals for each combination of race and sex
for (sex in sex_groups) {
  for (race in race_groups) {
    # Base effects
    race_eff <- race_effects[[race]]
    sex_eff <- sex_effects[[sex]]
    
    # Interaction key
    interaction_key <- paste0(race, "_", sex)
    if (interaction_key %in% names(interaction_effects)) {
      interaction_eff <- interaction_effects[[interaction_key]]
    } else {
      interaction_eff <- list(I = 0, se_I = 0, S1 = 0, se_S1 = 0, S2 = 0, se_S2 = 0)
    }
    
    # Expected latent variables for the group
    E_I <- mu_I + race_eff$I + sex_eff$I + interaction_eff$I
    E_S1 <- mu_S1 + race_eff$S1 + sex_eff$S1 + interaction_eff$S1
    E_S2 <- mu_S2 + race_eff$S2 + sex_eff$S2 + interaction_eff$S2
    
    # Standard errors (assuming independence)
    SE_I <- sqrt(se_mu_I^2 + race_eff$se_I^2 + sex_eff$se_I^2 + interaction_eff$se_I^2)
    SE_S1 <- sqrt(se_mu_S1^2 + race_eff$se_S1^2 + sex_eff$se_S1^2 + interaction_eff$se_S1^2)
    SE_S2 <- sqrt(se_mu_S2^2 + race_eff$se_S2^2 + sex_eff$se_S2^2 + interaction_eff$se_S2^2)
    
    # Calculate MH scores and their standard errors at each age
    MH_data <- lapply(ages, function(age) {
      # Compute MH_mean and MH_se
      if (age == 3) {
        MH_mean <- E_I
        MH_se <- SE_I
      } else if (age == 5) {
        MH_mean <- E_I + E_S1 * 1
        MH_se <- sqrt(SE_I^2 + (1 * SE_S1)^2)
      } else if (age == 7) {
        MH_mean <- E_I + E_S1 * 2
        MH_se <- sqrt(SE_I^2 + (2 * SE_S1)^2)
      } else if (age == 9) {
        MH_mean <- E_I + E_S1 * 3 + E_S2 * 1
        MH_se <- sqrt(SE_I^2 + (3 * SE_S1)^2 )
      } else if (age == 11) {
        MH_mean <- E_I + E_S1 * 4 + E_S2 * 2
        MH_se <- sqrt(SE_I^2 + (4 * SE_S1)^2 )
      } else if (age == 13) {
        MH_mean <- E_I + E_S1 * 5 + E_S2 * 3
        MH_se <- sqrt(SE_I^2 + (5 * SE_S1)^2 )
      } else if (age == 15) {
        MH_mean <- E_I + E_S1 * 6 + E_S2 * 4
        MH_se <- sqrt(SE_I^2 + (6 * SE_S1)^2)
      }
      # Compute 95% confidence intervals
      MH_lower <- MH_mean -  MH_se
      MH_upper <- MH_mean +  MH_se
      return(data.frame(Age = age, MH = MH_mean, MH_lower = MH_lower, MH_upper = MH_upper))
    })
    
    # Combine data and add group information
    temp_df <- do.call(rbind, MH_data)
    temp_df$Race <- race
    temp_df$Sex <- sex
    df_ext <- rbind(df_ext, temp_df)
  }
}

# Plot for Females
df_female <- df_ext %>% filter(Sex == "Female")

plot_Ext_model_female <- ggplot(df_female, aes(x = Age, y = MH, color = Race, roup = Race, fill = Race)) + 
  geom_line(size = 1.2,linetype = 'dotted') +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = MH_lower, ymax = MH_upper), alpha = 0.2, color = NA)+
  theme_minimal() +
  scale_x_continuous(breaks = c(3, 5, 7, 9, 11, 13, 15)) +
  scale_color_manual(name = "Race", values = race_colors) +
  scale_fill_manual(name = "Race", values = race_colors) + 
  labs(
    title = "(A) Girls ",
    x = "Age",
    y = "Log-transformed Externalizing Score"
  ) +
  theme(
    legend.title = element_blank()+
      theme(
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size =7),
        legend.text = element_text(size = 7)
      )
  )
plot_Ext_model_female

ggsave(paste0(dir_plot,"Aikins_Fig1C_Ext_Girls_Model_implied_",format(Sys.Date(),"%Y%m%d" ),".pdf"),plot_Ext_model_female, width = 15, height = 15, units = "cm")


# Plot for Males
df_male <- df_ext %>% filter(Sex == "Male")

plot_Ext_model_male <- ggplot(df_male, aes(x = Age, y = MH, color = Race, roup = Race, fill = Race)) + 
  geom_line(size = 1.2,linetype = 'dashed') +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = MH_lower, ymax = MH_upper), alpha = 0.2, color = NA)+
  theme_minimal() +
  scale_x_continuous(breaks = c(3, 5, 7, 9, 11, 13, 15)) +
  scale_color_manual(name = "Race", values = race_colors) +
  scale_fill_manual(name = "Race", values = race_colors) + 
  labs(
    title = "(B) Boys",
    x = "Age",
    y = "Log-transformed Externalizing Score"
  ) +
  theme(
    legend.title = element_blank()+
      theme(
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size =7),
        legend.text = element_text(size = 7)
      )
  )

ggsave(paste0(dir_plot,"Aikins_Fig1C_Ext_Boys_Model_implied_",format(Sys.Date(),"%Y%m%d" ),".pdf"),plot_Ext_model_male, width = 15, height = 15, units = "cm")


