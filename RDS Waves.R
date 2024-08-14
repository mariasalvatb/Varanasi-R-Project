##CREATING A NEW WAVE VARIABLE

# Initialize wave column
data_filtered$wave <- NA
# Seeds are wave 0
data_filtered$wave[data_filtered$seed == 1] <- 0
# Function to assign waves 
max_iterations <- 100  # Set a reasonable number to prevent infinite loops
for (i in 1:max_iterations) {
  previous_wave_ids <- data_filtered$studyid[!is.na(data_filtered$wave) & data_filtered$wave == i - 1]
  next_wave_ids <- unique(c(data_filtered$outcoupon1[data_filtered$incoupon %in% previous_wave_ids],
                            data_filtered$outcoupon2[data_filtered$incoupon %in% previous_wave_ids]))
  data_filtered$wave[data_filtered$studyid %in% next_wave_ids & is.na(data_filtered$wave)] <- i
}

##38 observations with NA wave, assign them a default max+1 wave number for further analysis
# Identify the maximum wave number
max_wave <- max(data_filtered$wave, na.rm = TRUE)
# Assign the maximum wave + 1 to observations with NA in the wave variable
data_filtered$wave[is.na(data_filtered$wave)] <- max_wave + 1

##Group the waves every 5
data_filtered$wave_group <- cut(data_filtered$wave,
                                breaks = 5,
                                labels = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"),
                                include.lowest = TRUE)



##RDS RECRUITMENT TREES (I took the cade from Talia and adapted, but it does not work)

# Load necessary libraries
library(igraph)
library(RDS)

# Select the relevant columns and prepare the dataset for RDS analysis
forrdsdata <- data_filtered[, c("studyid", "incoupon", "nw3", "seed", "outcoupon1", "outcoupon2")]
forrdsdata$max.coupons <- 2  # Assuming each participant gets 2 coupons

# Ensure the seed column is logical (TRUE for seeds, FALSE for non-seeds)
forrdsdata$seed <- forrdsdata$seed == 1

# Assign a unique identifier for seeds
forrdsdata$recruiter.id <- ifelse(forrdsdata$seed, NA, forrdsdata$incoupon)

# Create the RDS data frame
rdsdata <- data.frame(
  id = forrdsdata$studyid,
  recruiter.id = forrdsdata$recruiter.id,
  network.size.variable = forrdsdata$nw3,
  max.coupons = forrdsdata$max.coupons,
  seed = forrdsdata$seed
)

# Convert to RDS data frame
rdsdataset <- as.rds.data.frame(forrdsdata)

# Plot the recruitment tree
plot(rdsdataset, plot.type = 'Recruitment tree')
