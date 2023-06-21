diamond.lm<-lm(diamond$PRICE ~ diamond$CLARITY) 
summary(diamond.lm)
aggregate(diamond$PRICE, list(diamond$CLARITY), FUN=mean)
r1<-with(diamond, tapply(diamond$PRICE, diamond$CLARITY, mean))
r1
d.anova<-anova(diamond.lm)

d.pwt<-pairwise.t.test(diamond$PRICE, diamond$CLARITY, p.adjust.method = "none")

IF..<-relevel(as.factor(diamond$CLARITY), ref = "IF")
changeref = lm(diamond$PRICE ~ IF..)
confint(changeref, level = 0.99)
VS1..<-relevel(as.factor(diamond$CLARITY), ref = "VS1")
changeref = lm(diamond$PRICE ~ VS1..)
confint(changeref, level = 0.99)
VS2..<-relevel(as.factor(diamond$CLARITY), ref = "VS2")
changeref = lm(diamond$PRICE ~ VS2..)
confint(changeref, level = 0.99)

VVS1..<-relevel(as.factor(diamond$CLARITY), ref = "VVS1")
changeref = lm(diamond$PRICE ~ VVS1..)
confint(changeref, level = 0.99)

VVS2..<-relevel(as.factor(diamond$CLARITY), ref = "VVS2")
changeref = lm(diamond$PRICE ~ VVS2..)
confint(changeref, level = 0.99)
