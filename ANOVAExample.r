## Libraries

library(tidyverse)
library(ggeffects)
library(emmeans)

## Load in the data
sess.data <- read.csv("Sessile.csv")

glimpse(sess.data)
summary(sess.data)

sess.data <- sess.data %>% 
	mutate(Orientation = ifelse(Orientation == "vertical", "Vertical", Orientation))

sess.data <- sess.data %>% 
	mutate(Orientation = gsub("vert", "Vert", Orientation))

## Plot
ggplot(data = sess.data, aes(x = Copper, y = Richness, colour = Orientation)) + 
	geom_boxplot() + geom_point(alpha = 0.1, position = position_dodge(width = 0.75))

interaction.plot(sess.data$Copper, sess.data$Orientation, sess.data$Richness)

## Fit the model.
fit.aov <- aov(Richness ~ Copper*Orientation, data = sess.data)
summary(fit.aov)

## Look at the residuals.
op <- par(mfrow=c(2,2),mar=c(5,4,1,2))
Resid <- resid(fit.aov)
Fitted <- fitted(fit.aov)
plot(Fitted, Resid)
abline(h = c(-2*sd(Resid), 0,  2*sd(Resid)), 
	col = 'red', lty = 'dashed')
hist(Resid,xlab="Residuals",main="")
qqnorm(Resid)
qqline(Resid)
abline(h = c(-2*sd(Resid), 0,  2*sd(Resid)), col = 'red',
	lty = 'dashed')	
plot(factor(sess.data$Copper), Resid, xlab="Year",ylab="Residuals")
abline(h = c(-2*sd(Resid), 0,  2*sd(Resid)), col = 'red',
	lty = 'dashed')	

par(mfrow = c(1,2))
plot(fit.aov, which = 1:2)

summary(fit.aov)






means.cond <- emmeans(fit.aov, ~ Copper * Orientation)
results.dat <- data.frame(means.cond)
results.dat$pairNames <- paste(results.dat$Copper, results.dat$Orientation, sep = '-')
results.dat <- results.dat[order(results.dat$emmean),]
results.dat$pairNames <- factor(results.dat$pairNames, levels = results.dat$pairNames)

sess.data$pairNames <- paste(sess.data$Copper, sess.data$Orientation, sep = '-')

ggplot(data = results.dat, aes(x = pairNames, y = emmean)) +
	geom_point() + geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +
	xlab("Copper - Orientation") + ylab("Richness") +
	geom_boxplot(data = sess.data, aes(x = pairNames, y = Richness), alpha = 0.1)
	
diff.rich <- contrast(means.cond, "pairwise")

## Test the main effect differences... should you?
emmeans(fit.aov, pairwise~Copper)
emmeans(fit.aov, pairwise~Orientation)

## The Tukey Way
TukeyHSD(fit.aov, "Orientation")
TukeyHSD(fit.aov, "Copper")
TukeyHSD(fit.aov)

