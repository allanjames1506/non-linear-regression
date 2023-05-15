
library('readr')
library('dplyr')
library('drc')
library('ggplot2')

setwd("/Users/Allan/Documents/non_linear_regressiom/")

hugh <- read.csv("./00_raw_data/dataset.csv") %>% 
  rename(blood_pressure = x, pulse_rate = y)
toxdata <- ryegrass
str(ryegrass)
str(hugh)

model<- drm(rootl~conc, data=ryegrass, fct=LL.3(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))

model_hugh <- drm(blood_pressure~pulse_rate, data=hugh, fct=LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))

mselect(model.LL3, fctList = list(W1.3(),W1.4(), W2.3(), W2.4(),  LL.4()),linreg=TRUE) 

#you don't need the 'names = ' argument but it's useful to label the b, c, d, and e parameters until you're familiar with

plot(model, type="all")
?plot
plot(model_hugh, type = 'all',
     xaxt = "n", yaxt = "n")

axis(1, at = c(50, 60, 70, 80))

summary(model)

summary(model_hugh)


toxdata <- toxdata %>% 
  mutate(percent_response = rootl/(mean(toxdata$rootl[toxdata$conc==0]))*100)

# hugh <- hugh %>% 
#   mutate(percent_response = blood_pressure/(mean(hugh$blood_pressure[hugh$pulse_rate==107]))*100)
# 
# model_fixed_hugh<- drm(percent_response~pulse_rate, data=hugh, 
#                   fct=LL.4(fixed=c(NA, 107, 100, NA),
#                            names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
# 
# plot(model_fixed_hugh, main="LL.4(fixed=c(NA, 107, 100, NA))")

lmHugh <- lm(pulse_rate ~ blood_pressure, data = hugh)
summary(lmHugh)
plot(lmHugh)
plot(hugh$pulse_rate, hugh$blood_pressure)
abline(lmHugh)


box_plot <- hugh %>%
  ggplot(aes(blood_pressure, blood_pressure)) +
  geom_boxplot()

box_plot
