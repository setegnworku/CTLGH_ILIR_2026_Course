#milk_yield_plots/animal for bencmarking

rm(list=ls())
milk <- read.table("milkdata.txt",header=TRUE, fill=TRUE)

#install.packages("gridExtra")
#install.packages("cowplot")
#install.packages("dplyr")
#install.packages("ggplot2")

library(ggplot2)
library(dplyr)

############################ Edit data to suit your analysis ###############

# Identify duplicates

duplicates <- milk[duplicated(milk[c("animal_id", "milkingdate")]), ]

# Remove duplicates remove the duplicates while keeping the first occurrence of each unique combination.

milk_clean <- milk%>%
  distinct(animal_id, milkingdate, .keep_all = TRUE)

###### Add a new column for seasons based on the month groupings

milk_clean <- milk_clean %>%
  mutate(season = case_when(
    calvingmonth %in% c("December", "January", "February") ~ "Winter",
    calvingmonth %in% c("March", "April", "May") ~ "Spring",
    calvingmonth %in% c("June", "July", "August") ~ "Summer",
    calvingmonth %in% c("September", "October", "November") ~ "Fall",
    TRUE ~ NA_character_
  ))

#################### change factors to number coding 

milk_clean$province_code <- as.numeric(factor(milk_clean$province))

Province_reference_file <- data.frame(
  province = levels(factor(milk_clean$province)),
  province_numeric = 1:length(levels(factor(milk_clean$province)))
)

milk_clean$district_code <- as.numeric(factor(milk_clean$district))

district_reference_file <- data.frame(
  district = levels(factor(milk_clean$district)),
  district_numeric = 1:length(levels(factor(milk_clean$district)))
)

milk_clean$ward_code <- as.numeric(factor(milk_clean$ward))
ward_reference_file <- data.frame(
  ward = levels(factor(milk_clean$ward)),
  ward_numeric = 1:length(levels(factor(milk_clean$ward)))
)

milk_clean$calvingyear_code <- as.numeric(factor(milk_clean$calvingyear))

calvingyear_reference_file <- data.frame(
  calvingyear= levels(factor(milk_clean$calvingyear)),
  calvingyear_numeric = 1:length(levels(factor(milk_clean$calvingyear)))
)

milk_clean$season_code <- as.numeric(factor(milk_clean$season))

season_reference_file <- data.frame(
  season = levels(factor(milk_clean$season)),
  season_numeric = 1:length(levels(factor(milk_clean$season)))
)


####### combine cols to create herd_year.......for contemporary groups

milk_clean$herd_year <- paste(milk_clean$id_herd, milk_clean$calvingyear, sep = "_") # combine cols to create herd_year

milk_clean$herd_year_code <- as.numeric(factor(milk_clean$herd_year))

herd_year_reference_file <- data.frame(
  herd_year = levels(factor(milk_clean$herd_year)),
  herd_year_numeric = 1:length(levels(factor(milk_clean$herd_year)))
)

##### combine cols to create herd_year_season

milk_clean$herd_year_season <- paste(milk_clean$id_herd, milk_clean$calvingyear, milk_clean$season, sep = "_") # combine cols to create herd_year

milk_clean$herd_year_season_code <- as.numeric(factor(milk_clean$herd_year_season))

herd_year_season_reference_file <- data.frame(
  herd_year_season = levels(factor(milk_clean$herd_year_season)),
  herd_year_season_numeric = 1:length(levels(factor(milk_clean$herd_year_season)))
)


##### Categorize DIM into 3 groups

milk_clean <- milk_clean %>%
  mutate(dim_group = case_when(
    dim < 100 ~ "1",
    dim >= 100 & dim <= 200 ~ "2",
    dim > 200 ~ "3"
  ))

##### Change zero value to 1 in DIM column


zero_count <- sum(milk_clean$dim== 0)
zero_count

milk_clean$dim[milk_clean$dim == 0] <- 1


###### Check for NA


na_count <- sum(is.na(milk_clean))
milk_no_na <- milk_clean[complete.cases(milk_clean), ] # Identify complete cases (rows with no NAs)
#colSums(is.na(milk_clean)) # Count missing values

############################## Animal Benchmarking without adjustment ###############################

library(ggplot2)
library(dplyr)

# Calculate the average milk yield per district
district_averages <- milk_no_na %>%
  group_by(district) %>%
  summarize(district_avg_milk_yield = mean(totalmilk, na.rm = TRUE))

# Calculate the average milk yield per animal animal_averages <- milk_no_na %>%
animal_averages <- milk_no_na %>%
  group_by(animal_id, district) %>%
  summarize(animal_avg_milk_yield = mean(totalmilk, na.rm = TRUE))

# Incase you recieve a message, this message is informational, not an error. Each farm has multiple records per animal ( at least 12 for one record per month) 
# it means that summarise() was applied to a grouped data frame (e.g., grouped by animal_id using group_by(animal_id)

# Merge district averages with the animal averages
plot_data <- animal_averages %>%
  left_join(district_averages, by = "district")

###This Script creates and saves bar plots comparing the milk yield of individual animals to the district average.

# Create a list to store plots
plots <- list()

# Loop through each animal_id and create a plot
for (animal in unique(plot_data$animal_id)) {
  # Filter data for the current animal
  animal_data <- plot_data %>%
    filter(animal_id == animal)
  
  # Create the bar plot
  p <- ggplot(animal_data) +
    geom_bar(aes(x = "Animal Average", y = animal_avg_milk_yield, fill = "Animal Average"), stat = "identity", position = "dodge", width = 0.4) +
    geom_bar(aes(x = "District Average", y = district_avg_milk_yield, fill = "District Average"), stat = "identity", position = "dodge", width = 0.4) +
    labs(title = paste("Milk Yield for Animal ID:", animal),
         x = "Category",
         y = "Milk Yield",
         fill = "Category") +
    scale_fill_manual(values = c("Animal Average" = "blue", "District Average" = "red")) +
    theme_minimal()
  
  # Store the plot in the list
  plots[[as.character(animal)]] <- p
}

# Save plots to a PDF file
pdf("animal_vs_district_milk_yield_barplots.pdf", width = 8, height = 6)
for (plot in plots) {
  print(plot)
}
dev.off()
########################### herd benchmarking without adjustment ###########

# Calculate the average milk yield per district
district_averages <- milk_no_na %>%
  group_by(district) %>%
  summarize(district_avg_milk_yield = mean(totalmilk, na.rm = TRUE))

# Calculate the average milk yield per herd
herd_averages <- milk_no_na %>%
  group_by(id_herd, district) %>%
  summarize(herd_avg_milk_yield = mean(totalmilk, na.rm = TRUE))

# Merge district averages with the animal averages
plot_data <- herd_averages %>%
  left_join(district_averages, by = "district")

# Create a list to store plots
plots <- list()

# Loop through each animal_id and create a plot
for (herd in unique(plot_data$id_herd)) {
  # Filter data for the current animal
  herd_data <- plot_data %>%
    filter(id_herd == herd)
  
  # Create the bar plot
  p <- ggplot(herd_data) +
    geom_bar(aes(x = "Herd Average", y = herd_avg_milk_yield, fill = "Herd Average"), stat = "identity", position = "dodge", width = 0.4) +
    geom_bar(aes(x = "District Average", y = district_avg_milk_yield, fill = "District Average"), stat = "identity", position = "dodge", width = 0.4) +
    labs(title = paste("Milk Yield for Herd ID:", herd),
         x = "Category",
         y = "Milk Yield",
         fill = "Category") +
    scale_fill_manual(values = c("Herd Average" = "blue", "District Average" = "red")) +
    theme_minimal()
  
  # Store the plot in the list
  plots[[as.character(herd)]] <- p
}

# Save plots to a PDF file
pdf("herd_vs_district_milk_yield_barplots.pdf", width = 8, height = 6)
for (plot in plots) {
  print(plot)
}
dev.off()



################################### Adjust milk yield 

milk_no_na$parity <- as.factor(milk_no_na$parity)
milk_no_na$ward <- as.factor(milk_no_na$ward)
milk_no_na$dim_group <- as.factor(milk_no_na$dim_group)
milk_no_na$age_m <- as.factor(milk_no_na$age_m)
milk_no_na$season <- as.factor(milk_no_na$season)
milk_no_na$herd_year <- as.factor(milk_no_na$herd_year)
milk_no_na$herd_year_season <- as.factor(milk_no_na$herd_year_season)

# Fit a linear model
model <- lm(totalmilk ~ age_m + parity + dim_group + season + ward , data = milk_no_na)

# Capture the summary output

sink("lm_output.txt")  # Redirect output to a file
summary(model)          # Print full model summary
sink()                  # Stop redirecting output

# Capture the summary output another way
model_summary <- capture.output(summary(model))

# Write the summary to a file
writeLines(model_summary, "model_summary.txt")

############# adjusted milk yield

# Extract residuals
residuals <- resid(model)

# Calculate the mean of the fitted values from the model
mean_fitted <- mean(fitted(model))

# Add residuals to the mean of the fitted values
adjusted_values <- mean_fitted + residuals

# Combine adjusted values with the original dataset
milk_no_na$residuals <- residuals
milk_no_na$adjusted_totalmilk <- adjusted_values

# View the updated dataset
head(milk_no_na)

######### replot animal average with adjusted milk yield

# Calculate the average milk yield per district
district_averages <- milk_no_na %>%
  group_by(district) %>%
  summarize(district_avg_milk_yield = mean(adjusted_totalmilk, na.rm = TRUE))

# Calculate the average milk yield per animal
animal_averages <- milk_no_na %>%
  group_by(animal_id, district) %>%
  summarize(animal_avg_milk_yield = mean(adjusted_totalmilk, na.rm = TRUE))

# Merge district averages with the animal averages
plot_data <- animal_averages %>%
  left_join(district_averages, by = "district")

# Create a list to store plots
plots <- list()

# Loop through each animal_id and create a plot
for (animal in unique(plot_data$animal_id)) {
  # Filter data for the current animal
  animal_data <- plot_data %>%
    filter(animal_id == animal)
  
  # Create the bar plot
  p <- ggplot(animal_data) +
    geom_bar(aes(x = "Animal Average", y = animal_avg_milk_yield, fill = "Animal Average"), stat = "identity", position = "dodge", width = 0.4) +
    geom_bar(aes(x = "District Average", y = district_avg_milk_yield, fill = "District Average"), stat = "identity", position = "dodge", width = 0.4) +
    labs(title = paste("Milk Yield for Animal ID:", animal),
         x = "Category",
         y = "Milk Yield",
         fill = "Category") +
    scale_fill_manual(values = c("Animal Average" = "blue", "District Average" = "red")) +
    theme_minimal()
  
  # Store the plot in the list
  plots[[as.character(animal)]] <- p
}

# Save plots to a PDF file
pdf("adj_animal_vs_district_milk_yield_barplots.pdf", width = 8, height = 6)
for (plot in plots) {
  print(plot)
}
dev.off()


######### replot herd_yield_plot with adjusted data

# Calculate the average milk yield per district
district_averages <- milk_no_na %>%
  group_by(district) %>%
  summarize(district_avg_milk_yield = mean(adjusted_totalmilk, na.rm = TRUE))

# Calculate the average milk yield per herd
herd_averages <- milk_no_na %>%
  group_by(id_herd, district) %>%
  summarize(herd_avg_milk_yield = mean(adjusted_totalmilk, na.rm = TRUE))

# Merge district averages with the animal averages
plot_data <- herd_averages %>%
  left_join(district_averages, by = "district")

# Create a list to store plots
plots <- list()

# Loop through each animal_id and create a plot
for (id_herd in unique(plot_data$id_herd)) {
  # Filter data for the current animal
  herd_data <- plot_data %>%
    filter(id_herd == herd)
  
  # Create the bar plot
  p <- ggplot(herd_data) +
    geom_bar(aes(x = "herd Average", y = herd_avg_milk_yield, fill = "herd Average"), stat = "identity", position = "dodge", width = 0.4) +
    geom_bar(aes(x = "District Average", y = district_avg_milk_yield, fill = "District Average"), stat = "identity", position = "dodge", width = 0.4) +
    labs(title = paste("Milk Yield for Herd ID:", herd),
         x = "Category",
         y = "Milk Yield",
         fill = "Category") +
    scale_fill_manual(values = c("Herd Average" = "blue", "District Average" = "red")) +
    theme_minimal()
  
  # Store the plot in the list
  plots[[as.character(herd)]] <- p
}

# Save plots to a PDF file
pdf("adj_herd_vs_district_milk_yield_barplots.pdf", width = 8, height = 6)
for (plot in plots) {
  print(plot)
}
dev.off()


####### select animals with 3 records and more 

result <- milk_no_na %>%
  group_by(animal_id) %>%
  filter(n() >= 3) %>%
  ungroup()

###### Count the number of unique herds and animals
herd_count <- milk_clean %>% 
  summarise(unique_herds = n_distinct(id_herd),
            unique_animals = n_distinct(animal_id))

print(herd_count)