setwd("C:/Users/VANAJA AGARWAL/Downloads/")
sta_data = read.csv("Group2_STA202_Project - Final Sheet.csv", header = TRUE)
print(names(sta_data))

hist(sta_data$Natural.Population.Growth.rate..per.cent.,xlab="Natural Population Growth Rate",main = "Population Growth rate Histogram")
hist(sta_data$Public.expenditure.on.health..Rs.crores.)
hist(sta_data$Expenditure.on.relief.on.natural.calamities..Rs.Lakh.)
hist(sta_data$Production.of.total.food.grains..thousand.tonnes.)
hist(sta_data$State.Wise.Invested.Capital)
hist(sta_data$Unemployment.Per.1000.)
hist(sta_data$Capital.Expenditure)
hist(sta_data$GDP.per.capita)

sta_data$log_Natural_Population_Growth <- log10(sta_data$Natural.Population.Growth.rate..per.cent.)
sta_data$log_Public_Expenditure_on_Health <- log10(sta_data$Public.expenditure.on.health..Rs.crores.)
sta_data$log_Expenditure_on_Relief <- log10(sta_data$Expenditure.on.relief.on.natural.calamities..Rs.Lakh.)
sta_data$log_Production_of_Food_Grains <- log10(sta_data$Production.of.total.food.grains..thousand.tonnes.)
sta_data$log_State_Wise_Invested_Capital <- log10(sta_data$State.Wise.Invested.Capital)
sta_data$log_Unemployment_Per_1000 <- log10(sta_data$Unemployment.Per.1000.)
sta_data$log_Capital_Expenditure <- log10(sta_data$Capital.Expenditure)
sta_data$log_GDP_per_capita <- log10(sta_data$GDP.per.capita)

hist(sta_data$log_Unemployment_Per_1000, main="Log Transformation: Unemployment per 1000")

hist(sta_data$log_Natural_Population_Growth)

shapiro.test(sta_data$log_Natural_Population_Growth)
shapiro.test(sta_data$log_Unemployment_Per_1000)
boxplot(sta_data$log_Natural_Population_Growth)

library(MASS)
data1 <- unlist(as.vector(sta_data$log_Unemployment_Per_1000))
fit1 <- fitdistr(data1, "normal", method = "mle")
mean_fit1 <- fit1$estimate["mean"]
sd_fit1 <- fit1$estimate["sd"]
# Plot histogram of the data
hist(data1, freq = FALSE, main = "Unemployment_Per_1000 Histogram with Fitted Normal Distribution", xlab = "Log Unemployment per 1000")

# Add fitted normal distribution curve
curve(dnorm(x, mean = mean_fit1, sd = sd_fit1), add = TRUE, col = "blue", lwd = 2)

data2 <- unlist(as.vector(sta_data$log_Natural_Population_Growth))
fit2 <- fitdistr(data2, "normal", method = "mle")
mean_fit2 <- fit2$estimate["mean"]
sd_fit2 <- fit2$estimate["sd"]
hist(data2, freq = FALSE, main = "Natural Population Growth Rate Histogram with Fitted Normal Distribution", xlab = "Log Natural Population Growth Rate")

# Add fitted normal distribution curve
curve(dnorm(x, mean = mean_fit2, sd = sd_fit2), add = TRUE, col = "red", lwd = 2)
