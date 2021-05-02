# Import SwedishMotorInsurance data for analysis
head(SMInsurance)
View(SMInsurance)
summary(SMInsurance)
#through summary we can see that the mean of kilometer is 2.986 ie, 10,000-15,000
#the maximum number of claims are 3338 and mean is 51.87 
#the maximum payment is 18245026 and mean is 257008

# Task 2 Find Corelation between variabls Claims, Insured & Payment
cr = cor(SMInsurance)
cr
library(corrplot)
corrplot(cr,type="lower")
corrplot(cr, method="number")

##we perform correlation to find out the relation between Claims, Insured & Payment
rel1 = cor(SMInsurance$Claims,SMInsurance$Payment)
rel1*100 # Corelation between Claims & Payment is 0.9954 or 99.54%
plot(SMInsurance$Claims,SMInsurance$Payment, cex = 0.5, xlab = "Claims", 
     ylab = "Payment", main = "Claims Vs Payment" ) 

rel2 = cor(SMInsurance$Insured,SMInsurance$Payment)
rel2*100  #Corelation between Insured & Payment is 0.9332 or 93.32%
plot(SMInsurance$Insured,SMInsurance$Payment, cex = 0.5, xlab = "Insured", 
     ylab = "Payment", main = "Insured Vs Payment")

rel3 = cor(SMInsurance$Claims,SMInsurance$Insured)
rel3*100  #Corelation between Claims & Insured is 0.9103478 or 91.03%
plot(SMInsurance$Claims,SMInsurance$Insured, cex = 0.5, xlab = "Claims", 
     ylab = "Insured", main = "Claims Vs Insured" )

plot(SMInsurance)

# Task 3 All other variables are affecting Payment increase or decrease
model1 = lm(Payment~., data = SMInsurance)
summary(model1)

model2 = lm(Payment~ Kilometres+Zone+Make+Insured+Claims, data = SMInsurance)
summary(model2)

model3 = lm(Payment~ Kilometres+Zone+Insured+Claims, data = SMInsurance)
summary(model3)
# Above three model- model 3 is good model here 4 independent variable influencing Payment
# Low P value of all 4 independent variable is most significant

## Task 4 find at what location , kilometer bonus increase in Insured, claim & Payment

aggkm=apply(insur[,c(5,6,7)],2,function(x) tapply(x,insur$Kilometres,mean))
aggkm
##in these 5 zones we could see 1st km dist has been more insured than others with 2nd being close to it
##the amount of claims in the 2nd km distribution is higher hence more payments
aggzone=apply(insur[,c(5,6,7)],2,function(x) tapply(x,insur$Zone,mean))
aggzone
##zone number 4 has higher number of insured vehicle and more amount of claims and payment is more due to claims
aggbonus = apply(insur[,c(5,6,7)],2,function(y) tapply(y,insur$Bonus,mean))
aggbonus

#Task 5 What affect Claims rates so company decide premium for diffrent situation

model4 = lm(Claims~  Kilometres+Zone+Insured+Bonus+Make, data = SMInsurance)
summary(model4)
