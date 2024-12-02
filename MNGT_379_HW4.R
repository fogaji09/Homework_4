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

sleep_data$systolic = substr(Blood.Pressure, 1, 3) 

sleep_data$systolic = as.numeric(sleep_data$systolic)

sleep_data$diastolic = substr(Blood.Pressure, 5, 6) 

sleep_data$diastolic = as.numeric(sleep_data$diastolic)

install.packages("leaps")
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

table(Occupation)

table(BMI.Category)

table()