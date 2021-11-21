#ANOVA: 
ggplot(data = religionsuicidetemp , aes(y= AgeStandardizedSuicideRatesPer100000Population2015, x=Religion)) + 
  geom_boxplot() + geom_smooth(method = lm, se= FALSE) + 
  labs(title = "Suicide Rate Per Capita by Income Levels and Religion", x = "Religions",
       y = "Age Standarized Suicide Rates Per 100000 in 2015")

fit <- aov(AgeStandardizedSuicideRatesPer100000Population2015 ~ Religion, data = religionsuicidetemp)
summary(fit)
TukeyHSD(fit)

#GRAPHS/ VISUALISATIONS: 
ggplot(data = happinesssuicidetemp, 
       aes(y= AgeStandardizedSuicideRatesPer100000Population2015, x=Happiness.Score)) + 
  facet_grid(.~ size_f) + geom_point() + geom_smooth(method = lm, se= FALSE) + 
  labs(title = "Happiness Score vs. Suicide Rate Per Capita by Income Levels", 
       x = "Happiness Score (0-10)", y = "Age Standarized Suicide Rates Per 100000 in 2015")

ggplot(data = psychsuicidetemp, aes(y= AgeStandardizedSuicideRatesPer100000Population2015, 
                                    x=I(log(PsychaiatristsPer100000)))) + 
  facet_grid(.~ size_f) + geom_point() + geom_smooth(method = lm, se= FALSE) + 
  labs(title = "Log(Number of Psychiatrists Per Capita) vs. Suicide Rate Per Capita by Income Levels", 
       x = "Log(Number of Psychiatrists Per Capita)", y= "Age Standarized Suicide Rates Per 100000 in 2015")

ggplot(data = tempssuicidetemp, aes(y= AgeStandardizedSuicideRatesPer100000Population2015, x=Annual.Temperature)) 
+ facet_grid(.~ size_f) + geom_point() + geom_smooth(method = lm, se= FALSE) 
+ labs(title = "Annual Average Temperature vs. Suicide Rate Per Capita by Income Levels", 
       x = "Annual Average Temperature in Fahrenheit", y = "Age Standarized Suicide Rates Per 100000 in 2015")

#LINEAR REGRESSION:
#Joining the three variables:
HappinessTemp <- left_join(happiness, temperaturenew, by = "Country")
PsychHappinessTemp <- left_join(HappinessTemp, psyclol_, by = "Country") 
PsychHappinessTempClean <- select(PsychHappinessTemp, c("Country", "Happiness.Score", 
                                                        "AgeStandardizedSuicideRatesPer100000Population2015" ,
                                                        "PsychaiatristsPer100000", "Annual.Temperature", "IncomeGroup.x"))

#Regression model for high income 
HighIncomeVariables <- PsychHappinessTempClean[ which(test2$IncomeGroup.x=="High income"), ]
testa <- lm(AgeStandardizedSuicideRatesPer100000Population2015~ PsychaiatristsPer100000 
            + Happiness.Score + Annual.Temperature, data=HighIncomeVariables)
summary(testa)
#Psychiatrists plots
plot(AgeStandardizedSuicideRatesPer100000Population2015~PsychaiatristsPer100000, data = HighIncomeVariables)
abline(lm(HighIncomeVariables$AgeStandardizedSuicideRatesPer100000Population2015~HighIncomeVariables$PsychaiatristsPer100000), col="blue")
#Happiness plots
plot(AgeStandardizedSuicideRatesPer100000Population2015~ Happiness.Score, data = HighIncomeVariables)
abline(lm(HighIncomeVariables$AgeStandardizedSuicideRatesPer100000Population2015~HighIncomeVariables$Happiness.Score), 
       col="blue")
#Annual Temp plots
plot(AgeStandardizedSuicideRatesPer100000Population2015~ Annual.Temperature, data = HighIncomeVariables)
abline(lm(HighIncomeVariables$AgeStandardizedSuicideRatesPer100000Population2015~HighIncomeVariables$Annual.Temperature), 
       col="blue")
#Normal Probability plot 
qqnorm(testa$residuals)
qqline(testa$residuals)

#Regression model for upper middle income 
UpperIncomeVariables <- test2[ which(test2$IncomeGroup.x=="Upper middle income"), ]
testd <- lm(AgeStandardizedSuicideRatesPer100000Population2015~ PsychaiatristsPer100000 
            + Happiness.Score+ Annual.Temperature, data=UpperIncomeVariables)
summary(testd)

#Regression model for lower middle income 
LowerIncomeVariables <- test2[ which(test2$IncomeGroup.x=="Lower middle income"), ]
testc <- lm(AgeStandardizedSuicideRatesPer100000Population2015~ PsychaiatristsPer100000 
            + Happiness.Score+ Annual.Temperature, data=LowerIncomeVariables)
summary(testc)

#Regression model for low income 
LowIncomeVariables <- test2[ which(test2$IncomeGroup.x=="Low income"), ]
testb <- lm(AgeStandardizedSuicideRatesPer100000Population2015~ PsychaiatristsPer100000 
            + Happiness.Score+ Annual.Temperature, data=LowIncomeVariables)
summary(testb)

