# deliverable 1

MechaCar_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_table) #generate multiple linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_table)) #generate summary statistics


# deliverable 2
Suspension_coil_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
summarize_suspension <- Suspension_coil_table %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep') #create summary table with multiple columns
summarize_suspension_by_lot <- Suspension_coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep') #create summary table with multiple columns


# deliverable 3
t.test(Suspension_coil_table$PSI,mu=1500)
t.test(subset(Suspension_coil_table, Manufacturing_Lot =="Lot1")$PSI,mu=1500)
t.test(subset(Suspension_coil_table, Manufacturing_Lot =="Lot2")$PSI,mu=1500)
t.test(subset(Suspension_coil_table, Manufacturing_Lot =="Lot3")$PSI,mu=1500)