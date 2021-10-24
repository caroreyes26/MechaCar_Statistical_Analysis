# Deliverable 1
# 3. Use the library function to load the dplyr package 
library(dplyr)
#4. Import and read in the MechaCar_mpg.csv file as a dataframe
mecha_mpg <- read.csv(file='Resources/MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
#5. Perform linear regression using the lm()function. In the lm () function, pass all six variables and add the dataframe you created in step 4 as the data parameter
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_mpg)
#6. Using the summary() function, determine the p-value and the r-squared value for the linear regression model.
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_mpg)) 

#Deliverable 2
#2. Import and read in the suspension_coil.csv file as a table 
mecha_coil <- read.csv(file='Resources/Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
#3. write an RScript that creates a total_summary dataframe using the summarize() function to get the mean, median,variance,and standard deviation of the suspension coil's PSI column
total_summary <- mecha_coil %>% summarize(Mean_PSI=mean(PSI),
                                          Median_PSI=median(PSI),
                                          Var_PSI=var(PSI),
                                          Std_Dev_PSI=sd(PSI),
                                          Num_Coil=n(), .groups = 'keep') 
#4. Create a lot_summary dataframe using the group_by() and the summarize() functions to group each manufacturing lot.                                                                
lot_summary <- mecha_coil  %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),
                                                                         Median_PSI=median(PSI),
                                                                         Var_PSI=var(PSI),
                                                                         Std_Dev_PSI=sd(PSI),
                                                                         Num_Coil=n(), .groups = 'keep')  
# Deliverable 3
#1. Write and RScript using the t.test() function to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1,500 lbs per square inch. 
t.test(mecha_coil$PSI,mu=1500)
#2. Write three more RScripts using the t.test() function and its subset() argument to determine if the PSI for each manufacturing lot is statistically different from the population mean of 1,500 pounds per square inch. 
lot1 <- subset(mecha_coil, Manufacturing_Lot=="Lot1")
lot2 <- subset(mecha_coil, Manufacturing_Lot=="Lot2")
lot3 <- subset(mecha_coil, Manufacturing_Lot=="Lot3")

t.test(lot1$PSI,mu=1500)
t.test(lot2$PSI,mu=1500)
t.test(lot3$PSI,mu=1500)
