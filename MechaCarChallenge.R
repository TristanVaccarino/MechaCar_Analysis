# Deliverable #1 --------------------------------------------------------------------------------------------------------
# Load library
library(dplyr)

# Import data from the csv file
MechaCar_mpg <- read.csv(file="MechaCar_mpg.csv", check.names = F,stringsAsFactors = F)

# Create linear regression using lm() function
lmdata <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_mpg)
lmdata

# Use summary to determine p-value
# p-value = 5.35e-11
summary(lmdata)

# Deliverable #2 --------------------------------------------------------------------------------------------------------
# Import Suspension data
MechaCar_psi <- read.csv(file="Suspension_Coil.csv", check.names = F,stringsAsFactors = F)

# Get a Summary table
total_summary <- MechaCar_psi %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), Std_Dev=sd(PSI))
total_summary

# Lot Summary
lot_summary <- MechaCar_psi %>% group_by(Manufacturing_Lot) %>% 
  summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), Std_Dev=sd(PSI),Num_Vehicles=n(), .groups = 'keep')
lot_summary

# Deliverable #3 --------------------------------------------------------------------------------------------------------
# Overall 
t.test(MechaCar_psi$PSI,mu=1500)

# By Lot
# - Lot1
t.test(subset(MechaCar_psi, Manufacturing_Lot=="Lot1")$PSI, mu=1500)

# - Lot2
t.test(subset(MechaCar_psi, Manufacturing_Lot=="Lot2")$PSI, mu=1500)

# - Lot3
t.test(subset(MechaCar_psi, Manufacturing_Lot=="Lot3")$PSI, mu=1500)





