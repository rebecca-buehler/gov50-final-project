#setting working directory
setwd("/Users/eli.harristrent/MPhil Year 1")
#loading packages
library(readxl)
library(modelsummary)
suppressPackageStartupMessages(library("texreg"))
library(ggplot2)
library(dplyr)
library(stargazer)
library(anytime)
library(lubridate)
library(haven)

#reading data
beccadata <- read_excel("Ethiopia_1997-2023_Nov03.xlsx")

#adding location control (capital or not capital)
beccadata$CAPITAL <- as.factor(ifelse(beccadata$ADMIN1 == "Addis Ababa",1,
                                      0))

#creating subset with data only after 2020 and with civilians 
POST2020ETHdata <- beccadata %>%
  filter(EVENT_DATE >= "2020-11-03") 

#MODELS
#fixing event type (IV) as a factor
POST2020ETHdata$EVENT_TYPE <- as.factor(POST2020ETHdata$EVENT_TYPE)
summary(as.factor(POST2020ETHdata$EVENT_TYPE))
#running models
model1 <- lm(FATALITIES ~ EVENT_TYPE, data = POST2020ETHdata)
summary(model1)
model2 <- lm(FATALITIES ~ EVENT_TYPE + YEAR, data = POST2020ETHdata)
summary(model2)
model3 <- lm(FATALITIES ~ EVENT_TYPE + YEAR + CAPITAL, data = POST2020ETHdata)
summary(model3)

#making it look pretty with stargazer
#can export to anything but chose text here just to show results easily 
stargazer(model1, model2, model3, 
          title = "Event Type's Effect on Fatality Count", 
          type = "latex",
          covariate.labels = c("Explosions/Remote Violence", "Protests",
                               "Riots","Strategic Developments","Violence Against Civilians",
                               "Year","Capital","Constant"),
          dep.var.labels = "Fatalities")

#year seems to be important, let's plot
#making the plot look like an NYT graph
nyt_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Georgia"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold")
  )
#creating the plot
ggplot(beccadata, aes(x = YEAR, y = FATALITIES)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, fill = "grey", color = "red") + 
  labs(title = "Fatalities by Year in Ethiopia", x = "Year", y = "Fatalities") + 
  scale_x_continuous(limits = c(1997, 2024)) +
  scale_y_continuous(limits = c(0, 200)) + 
  nyt_theme
