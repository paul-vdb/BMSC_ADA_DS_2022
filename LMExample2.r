
library(tidyverse)
library(readxl)
library(emmeans)
library(lattice)
library(corrplot)
library(DHARMa)
library(ggeffects)

generateData <- function(seed = NULL)
{	
	if(is.null(seed)) seed <- floor(runif(1,1,10000))
	print(paste("Random Seed:", seed))
	set.seed(seed)
	dataType <- rbinom(1, size = 1, prob = 0.5)
	dat <- data.frame(temperature = runif(100, 10, 13), 
		pH = runif(100, 7.5, 7.8), 
		DissolvedOxygen = runif(100, 7, 8),
		concentrationOfAlgae = runif(100, 0, 20),
		initialSizeClass = sample(c("Small", "Large"), 100, replace = TRUE)
		)
	betas <- c(runif(1, 4, 5), rnorm(4, 0, 2), runif(1, 0, 4))
	sigma <- runif(1, 0.75, 15)
	include <- rbinom(5, size = 1, prob = 0.8)
	betas <- betas/10
	sigma <- sigma/10
	tmp <- betas[1] +
		dat$temperature*betas[2] + 
		dat$pH*betas[3]*include[2] + 
		dat$DissolvedOxygen*betas[4] + 
		dat$concentrationOfAlgae*betas[5]*include[4] +
	  (dat$initialSizeClass == "Large")*betas[6]*include[5] +
		rnorm(100, 0, sqrt(sigma))
	dat$growth <- exp(tmp)
	return(dat)
}

my.data <- generateData()
my.data$initialSizeClass <- factor(my.data$initialSizeClass)

## Plot and make sure my predictors aren't correlated.
splom(my.data)

## Looks like a non-linear trend in year...
mod <- lm(log(growth) ~ temperature+pH+DissolvedOxygen+
	concentrationOfAlgae+initialSizeClass, data = my.data)
summary(mod)	

op <- par(mfrow=c(2,2),mar=c(5,4,1,2))
Resid <- resid(mod)
Fitted <- fitted(mod)
plot(Fitted, Resid)
abline(h = c(-2*sd(Resid), 0,  2*sd(Resid)), 
	col = 'red', lty = 'dashed')
hist(Resid,xlab="Residuals",main="")
qqnorm(Resid)
qqline(Resid)
abline(h = c(-2*sd(Resid), 0,  2*sd(Resid)), col = 'red',
	lty = 'dashed')	
plot(factor(my.data$initialSizeClass), Resid, xlab="Year",ylab="Residuals")
abline(h = c(-2*sd(Resid), 0,  2*sd(Resid)), col = 'red',
	lty = 'dashed')	

dev.off()

par(mfrow = c(1,2))
plot(mod, which = c(1,2))

simulationOutput <- simulateResiduals(fittedModel = mod, 
	method = "traditional", plot = F)
plot(simulationOutput)

## Interpret summary output.
summary(mod)

mod2 <- lm(log(growth) ~ temperature+pH+
	concentrationOfAlgae+initialSizeClass, data = my.data)

AIC(mod)
AIC(mod2)

## Let's plot the results, holding everything but one value constant.
## I'm lazy so let's use a package.
algaeLine <- ggpredict(mod, terms = c("initialSizeClass","concentrationOfAlgae"), 
	typical = 'median')
plot(algaeLine)

algaeLine <- ggpredict(mod, terms = c("concentrationOfAlgae","temperature"), 
	typical = 'mean')
plot(algaeLine)
	
pred.data <- data.frame(algaeLine)

ggplot(data = pred.data, aes(y = predicted, x = x)) +
	geom_line(aes(col = group)) + theme_classic() +
	geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), alpha = 0.2) +
	geom_point(data = my.data, aes(x=concentrationOfAlgae, y=growth), col = 'black') +
	labs(x = "Concentration Algae", y = "Growth of Oyster (Units per Hour)") 