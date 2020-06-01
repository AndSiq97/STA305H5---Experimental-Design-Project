airplane_data = read.csv("airplanedata.csv", header = TRUE)
attach(airplane_data)
summary(airplane_data)
names(airplane_data)


##ANOVA with blocking (comparing one factor analysis to two factor analysis)

#Two WAY ANOVA (without interaction)

modelblock <- lm(Flying.Distance ~ Type.of.Airplane.Design + Type.of.Paper.Texture)
summary(modelblock)
anova(modelblock) 


#One Way ANOVA (for each factor separately)

model1 <- lm(Flying.Distance ~ Type.of.Airplane.Design)
summary(model1)
anova(model1)

model2 <- lm(Flying.Distance ~ Type.of.Paper.Texture)
summary(model2)
anova(model2)

#Two Way ANOVA with Interactions

modelinter <- lm(Flying.Distance ~ Type.of.Airplane.Design * Type.of.Paper.Texture)
summary(modelinter)
anova(modelinter)


#Two Way ANOVA for additive model with only main effects (same to modelblock)

modeladd <- lm(Flying.Distance ~ Type.of.Airplane.Design + Type.of.Paper.Texture)
summary(modeladd)
anova(modeladd)


#interaction plot

with(airplane_data,interaction.plot(Type.of.Paper.Texture, Type.of.Airplane.Design, Flying.Distance, col=c("red", "blue"), main="Interaction Plot", xlab="Mean", ylab="Flying Distance"))





