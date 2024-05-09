setwd("C:/Users/VANAJA AGARWAL/Downloads/")
two_sample_t_test = read.csv("Hypothesis testing_STA202 - Sheet1.csv", header = TRUE)
gdp_per_capita = two_sample_t_test$GDP.per.capita
state = two_sample_t_test$State_encoding

shapiro.test(gdp_per_capita)
shapiro.test(state)

boxplot(gdp_per_capita~state, col=c(5,4))


data1 <- subset(two_sample_t_test, select = -State)

#Parametric t test
t.test(gdp_per_capita~state, var.equal = TRUE, data = data1)

#Non-parametric test
wilcox.test(gdp_per_capita ~ state, data = data1)

#Chi-square test
GDP_Per_Capita_Per_States <- matrix(c(11,19,39,21), byrow = T, nrow = 2)
rownames(GDP_Per_Capita_Per_States) <- c("Southern Staes", "Northern States")
colnames(GDP_Per_Capita_Per_States) <- c("Below Average", "Above Average")
model <- chisq.test(GDP_Per_Capita_Per_States)
model
