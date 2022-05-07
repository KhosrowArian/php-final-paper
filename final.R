# plot death rate versus the number of cases
data<-read.csv("/Users/khosrow/Desktop/covid_data.csv")

x = data$new_cases_per_million
y = data$new_deaths_per_million


plot(x = data$new_cases_per_million,
     y = data$new_deaths_per_million,
     ylab = "new_deaths_per_million",
     xlab = "new_cases_per_million",
     xlim = c(0, 25000), ylim = c(0, 50), # Limits
     panel.first = abline(h = seq(2, 2000, 25), col = "grey80"))

abline(lm(y ~ x), col='blue' , lty='dashed')
title("number of cases versus death rate", adj = 1,
      cex.main = 0.8, font.main = 2, col.main = "black")


# plot death rate versus the number of cases
data<-read.csv("/Users/khosrow/Desktop/covid_data.csv")

x = data$new_vaccinations
y = data$new_deaths_per_million


plot(x = data$new_vaccinations,
     y = data$new_deaths_per_million,
     ylab = "new deaths per million",
     xlab = "new vaccinations",
     xlim = c(0, 500000), ylim = c(0, 50), # Limits
     panel.first = abline(h = seq(2, 2000, 25), col = "grey80"))

abline(lm(y ~ x), col='blue' , lty='dashed')
title("number of vaccinations versus death rate", adj = 1,
      cex.main = 0.8, font.main = 2, col.main = "black")



# linear regression part 1
data<-read.csv("/Users/khosrow/Desktop/covid_data.csv")
linearMod <- lm(new_deaths_per_million ~ new_cases_per_million, data=data)  # build linear regression model on full data
print(linearMod)

data<-read.csv("/Users/khosrow/Desktop/covid_data.csv")
linearMod <- lm(new_deaths_per_million ~ vaccine_me, data=data)  # build linear regression model on full data
print(linearMod)


# linear regression part 2
data<-read.csv("/Users/khosrow/Desktop/covid_data.csv")
linearMod = lm(new_deaths_per_million ~ new_cases_per_million, data=data)  # build linear regression model on full data
summary(linearMod)

data<-read.csv("/Users/khosrow/Desktop/covid_data.csv")
linearMod = lm(new_deaths_per_million ~ vaccine_me, data=data)  # build linear regression model on full data
summary(linearMod)

data<-read.csv("/Users/khosrow/Desktop/covid_data.csv")
linearMod = lm(new_deaths_per_million ~ stringency_index, data=data)  # build linear regression model on full data
summary(linearMod)

# linear regression part 3
data<-read.csv("/Users/khosrow/Desktop/covid_data.csv")
model <- lm(new_deaths_per_million ~ new_cases_per_million + vaccine_me + stringency_index, data=data)  
print(model)
summary(model)


#Check if mean error=0
mean(model$residuals)
t.test(model$residuals, mu=0)

#check if correlation between error and X=0
# cor(model$residuals, data$percentageexpenditure, method="pearson")
# cor.test(model$residuals, data$percentageexpenditure, method="pearson")

#check homoscedasticity
plot(fitted(model), resid(model))


#check normality of errors
qqnorm(resid(linearMod))


# EDA plots
data<-read.csv("/Users/khosrow/Desktop/covid_data.csv")
total_cases = data$total_cases
new_vaccinations = data$new_vaccinations
new_cases_per_million = data$new_cases_per_million
new_deaths_per_million = data$new_deaths_per_million
stringency_index = data$stringency_index

hist(new_deaths_per_million)

boxplot(new_cases_per_million)
title("the range or new cases", adj = 1,
      cex.main = 0.8, font.main = 2, col.main = "black")

boxplot(total_cases ~ stringency_index, data=data)
title("how stringency index affects total cases", adj = 1,
      cex.main = 0.8, font.main = 2, col.main = "black")








