setwd("/cloud/project")

sleep_data <- read.csv("Sleep_health_and_lifestyle_dataset.csv", header = TRUE)

# attach allow was to interact with the data without using the "$"
attach(sleep_data)

names(sleep_data)

#Justification for performing linear regression

#Ho: data is not normal; ha: data is normal
#reject null hypothesis that the outcome is not normal and conclude
# that the outcome is normal
shapiro.test(Sleep.Duration)
#if P-value is less than 0.05, you reject the null (the data is reject)
#Meaning data is normal

#histogram of the outcome shows to be a bimoodal distribution
#this could be an indication that sleep duration might be diifferent
#by a particular group - maybe gender.
#given our large sample size of n=374 obs., by the
hist(Sleep.Duration)


is.character(Blood.Pressure)

#sleep_data$systolic = substr(Blood.Pressure, 1, 3) 

#sleep_data$systolic = as.numeric(sleep_data$systolic)

#sleep_data$diastolic = substr(Blood.Pressure, 5, 6) 

#sleep_data$diastolic = as.numeric(sleep_data$diastolic)

#install.packages("leaps")
library(leaps)

#Now we run the regsubsets to find tge best model
output <- regsubsets(Sleep.Duration ~ Gender + Age + Occupation + 
                       Quality.of.Sleep + Physical.Activity.Level + 
                       Stress.Level + BMI.Category + Heart.Rate + Daily.Steps +
                       Sleep.Disorder + systolic + diastolic, data=sleep_data,
                                                                      nvmax=12)

summOut1 <- summary(output)

summOut1

n1 <- length(Sleep.Duration)
n1

p1 <- apply(summOut1$which,1,sum)

summOut1$which

p1

aic1 <- summOut1$bic - log(n1) * p1 + 2 * p1

plot(p1, aic1, ylab = "AIC1")

summOut1

#best model is the one with all the predictors as it has the lowest AIC
model1 <- lm(Sleep.Duration ~ Gender + Age + Occupation + Quality.of.Sleep + 
               Physical.Activity.Level +
               Stress.Level + BMI.Category + Heart.Rate + Daily.Steps +
               Sleep.Disorder + systolic + diastolic, data=sleep_data)

summary(model1)

#Interpretation for the significant variable from this model:
# Accountant is used as a Reference point, Compared with every other category.

#sleep duration increases significantly by 0.027 units for every unit
#increase in age, adjusting for everything else.

#sleep duration increases significantly by 0.83 units for Doctors
# vs. Accountants, adjusting for everything else.

#sleep duration increases significantly by 0.78 units for engineer
# vs. Accountants, adjusting for everything else.

#sleep duration increases significantly by 0.73 units for Lawyer
# vs. Accountants, adjusting for everything else.

#sleep duration increases significantly by 0.242 units for Nurse
# vs. Accountants, adjusting for everything else.

#sleep duration increases significantly by 1.45 units for Sales Rep
# vs. Accountants, adjusting for everything else.

#sleep duration increases significantly by 0.64 units for Sales Person
# vs. Accountants, adjusting for everything else.

#sleep duration increases significantly by 0.46 units for Scientist
# vs. Accountants, adjusting for everything else.

#sleep duration increases significantly by 0.63 units for Software Engineers
# vs. Accountants, adjusting for everything else.

#sleep duration increases significantly by 0.29 units for Teachers
# vs. Accountants, adjusting for everything else.

#sleep duration increases significantly by 0.29 units for every unit 
#increase in quality of sleep score, adjusting for everything else.

#sleep duration increases significantly by 0.009 units for every unit
#increase in physical activity level score, adjusting for everything else.

#sleep duration decreases significantly by 0.16 units for every unit 
#increase in stress level score, adjusting for everything else.

#sleep duration decreases significantly by 0.6 units for Obese
# vs. normal weight, adjusting for everything else.

#sleep duration decreases significantly by 0.34 units for OverWeight
# vs. normal weight, adjusting for everything else.

#sleep duration increases significantly by 0.03 units for every unit
# increase in heart rate, adjusting for everything else.

#sleep duration decreases significantly by 0.0001 units for every unit
# increase in daily steps, adjusting for everything else.

#sleep duration decreases significantly by 0.121 units for every unit
# increase in systolic reading, adjusting for everything else.

#sleep duration increases significantly by 0.135 units for every unit
# increase in diastolic reading, adjusting for everything else.

#table(Occupation)

#table(BMI.Category)


#Checking for multicollinearity using vif and tolerance

#install.packages("car")
library(car)

vif(model1)
#if the vif (last column) shows to be greater than 10, it implies that
#there is such a strong relationship between variables such that these
#may be collinear, if collinear this will bias the results of the model
#from our results, we see that systolic and diastolic may be colinear

#tolerance the inverse of vif; we run as an extra check
#sometimes what vif is not able to identify, tolerance might
1/vif(model1)

#for the result we are looking for the last column to be >0.10, if less than
#this implies collinear. the two variable are systolic and diastolic.