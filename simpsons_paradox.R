#############################################
### Generating Data for Simpson's Paradox ###
#############################################
# A more detailed tutorial can be found no the Quora blog Rticles
# Rticle: https://rticles.quora.com/Generating-Data-for-Simpsons-Paradox
library(tidyverse)

# Generating correlated data with mvrnorm() from the MASS library
library(MASS)

# sample means
mu <- c(20,4)

# define our covariance as a matrix, and specify the covariance relationship (i.e. 0.7 in this case)
Sigma <- matrix(.7, nrow=2, ncol=2) + diag(2)*.3

# create variables
vars <- mvrnorm(n=100, mu=mu, Sigma=Sigma)

head(vars)
cor(vars)

# Plot the variables
plot(vars[,1],vars[,2])

# Create a function for multiple pairwise correlated variables
corVars<-function(m1,m2,confVar){
  mu <- c(m1,m2)
  Sigma <- matrix(.7, nrow=2, ncol=2) + diag(2)*.3
  vars <- mvrnorm(n=100, mu=mu, Sigma=Sigma)
  Var1<-vars[,1]
  Var2<-vars[,2]
  df<-as.data.frame(cbind(Var1 = Var1,Var2 = Var2,Var3 = confVar))
  df$Var1<-as.numeric(as.character(df$Var1))
  df$Var2<-as.numeric(as.character(df$Var2))
  df
}

# Quick check to make sure the function works
d1 <- corVars(m1 = 20, m2 = 4, confVar = "A")
plot(d1$Var1,d1$Var2)

# Re-running for multiple sets and combining into a single dataframe
d1 <- corVars(m1 = 20, m2 = 4, confVar = "A")
d2 <- corVars(m1 = 18, m2 = 6, confVar = "B")
d3 <- corVars(m1 = 16, m2 = 8, confVar = "C")
d4 <- corVars(m1 = 14, m2 = 10, confVar = "D")
d5 <- corVars(m1 = 12, m2 = 12, confVar = "E")

df<-rbind(d1,d2,d3,d4,d5)
# Checking the correlation between the two variables
cor.test(df$Var1,df$Var2)

# Strong negative correlation: -0.82


# Plotting the correlation 
df %>%
  ggplot(aes(x=Var1,y=Var2)) +
  geom_point()+
  geom_smooth(method="lm")+
  xlab("Variable One")+ylab("Variable Two")


# Now replotting while controlling for the third confounding variable
df %>%
  ggplot(aes(x=Var1,y=Var2,fill=Var3,colour=Var3)) +
  geom_point()+
  geom_smooth(method="lm")+
  xlab("Variable One")+ylab("Variable Two")