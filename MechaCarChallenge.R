libary(dplyr)
library(tidyverse)
MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_mpg)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_mpg)) 
summary(lm(mpg ~ vehicle_length + ground_clearance,data=MechaCar_mpg))

### Deliverable 2
Suspension_Coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors =  F)

total_summary <- Suspension_Coil %>% summarize(Mean_PSI=mean(PSI),
                                          Median_PSI=median(PSI),
                                          Var_PSI=var(PSI),
                                          Std_Dev_PSI=sd(PSI),
                                          Num_Coil=n(), .groups = 'keep') 

lot_summary <- Suspension_Coil  %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),
                                                                         Median_PSI=median(PSI),                                                                        
                                                                         Var_PSI=var(PSI),
                                                                         Std_Dev_PSI=sd(PSI),
                                                                         Num_Coil=n(), .groups = 'keep')
#next we add the plot
plt1 <- ggplot(Suspension_Coil,aes(y=PSI)) #import dataset into ggplot2
plt1 + geom_boxplot() #add boxplot

#adding individual lots
plt2 <- ggplot(Suspension_Coil,aes(x=Manufacturing_Lot,y=PSI)) #import dataset into ggplot2
plt2 + geom_boxplot()

# Deliverable 3
t.test(Suspension_Coil$PSI,mu=1500)

#t.test() 3 times with different lots
lot1 <- subset(Suspension_Coil, Manufacturing_Lot=="Lot1")
lot2 <- subset(Suspension_Coil, Manufacturing_Lot=="Lot2")
lot3 <- subset(Suspension_Coil, Manufacturing_Lot=="Lot3")

t.test(lot1$PSI,mu=1500)
t.test(lot2$PSI,mu=1500)
t.test(lot3$PSI,mu=1500)
