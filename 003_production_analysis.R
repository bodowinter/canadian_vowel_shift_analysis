## Bodo Winter
## May 3, 2015; Amended May 15-17, 2015; May 19, 2015
## Adapted new style and cleaned code: Jan 17, 2016
## Canadian Vowel Shift production Data Analysis


##------------------------------------------------------------------
## Preliminaries:
##------------------------------------------------------------------

## Set to main analysis directory (everything else is relative to that):

mainDir <- '/Users/teeniematlock/Desktop/research/kettig_canadian_vowel_shift/analysis'

## Set to processed data:

subDir <- 'processed_data'
setwd(file.path(mainDir, subDir))

## Load in data:

vowels <- read.csv('production_processed.csv')

## Packages:

library(car)
library(lme4)
library(dplyr)

## Add IPA symbols to main table:

vowels$VowelIPA <- as.character(vowels$Vowel)
vowels[vowels$Vowel == 'ae',]$VowelIPA <- 'æ'
vowels[vowels$Vowel == 'e',]$VowelIPA <- 'ɛ'
vowels[vowels$Vowel == 'u',]$VowelIPA <- 'ʌ'
vowels[vowels$Vowel == 'o',]$VowelIPA <- 'ɔ'

## Function for Dunn-Sidak correction:

dunnsidak <- function(P,N){1 - ((1 - P) ^ N)}


##------------------------------------------------------------------
## Plot young and old vowel ellipses:
##------------------------------------------------------------------

## Get speaker means:

xagr <- aggregate(F1 ~ Speaker * Age * Vowel, vowels, mean)
xagr$F2 <- aggregate(F2 ~ Speaker * Age * Vowel, vowels, mean)[, 'F2']
xagr$pch <- ifelse(xagr$Age == 'O', 24, 20)			# plotting characters
xagr$Gender <- substr(xagr$Speaker, 1, 1)	# gender info

## Add IPA charachters:

xagr$VowelIPA <- vowels[match(xagr$Vowel, vowels$Vowel), ]$VowelIPA

## Find the rows that are extreme values:

extremes <- which(xagr$Speaker == 'F1988a' & xagr$Vowel == 'e')
extremes <- c(extremes, which(xagr$Speaker == 'M1992a' & xagr$Vowel == 'ae'))

## Set confidence level of ellipsoid:

confidence <- 0.95

## Decide here whether you want double plot or single plot:

single <- F

## Create vowel plot with ellipses for age categories:

if (single) {
	quartz('', 8, 6)
	par(mai = c(1.25, 1.5, 0.5, 0.5))
	} else {
		quartz('', 12, 5.5)
		par(mai = c(1.25, 0.5, 0.35, 0.15), omi = c(0, 1, 0.65, 0), mfrow = c(1, 2))
		}
plot(1, 1, type = 'n',
	xlim = c(1750, 1000), ylim = c(650, 450),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
if (!single) text(1725, 460, labels = '(a)', cex = 2, font = 2)
if (!single) mtext(side = 3, 'Age', cex = 2, font = 2, line = 0.6)
mtext(side = 1, 'F2', line = 3, cex = 1.75, font = 2)
mtext(side = 1, '(Lobanov-normalized)', line = 4.25, cex = 1.15, font = 2)
mtext(side = 2, 'F1', line = 5.25, cex = 1.75, font = 2)
mtext(side = 2, '(Lobanov-normalized)', line = 4, cex = 1.15, font = 2)
axis(side = 1, seq(1750, 1000, -250), lwd.ticks = 2, font = 2, cex.axis = 1.5)
axis(side = 2, seq(650, 450, -50), lwd.ticks = 2, font = 2, cex.axis = 1.5, las = 2)
text(xagr[-extremes,]$F2, xagr[-extremes,]$F1, labels = xagr[-extremes, ]$VowelIPA, cex = 1.15)
# Plot extreme speakers:
text(xagr[extremes, ]$F2,
	xagr[extremes, ]$F1, font = 2, labels = xagr[extremes, ]$VowelIPA, cex = 2)
## Add ellipses:
for (i in 1:4) {					# plots points
	this_vowel <- levels(xagr$Vowel)[i]
	old <- as.matrix(xagr[xagr$Vowel == this_vowel & xagr$Age == 'O', c('F2', 'F1')])
	young <- as.matrix(xagr[xagr$Vowel == this_vowel & xagr$Age == 'Y', c('F2', 'F1')])
	dataEllipse(old,
		levels = confidence, add = T, plot.points = F, center.pch = F, lwd = 2, col = 'black', lty = 2)
	dataEllipse(young,
		levels = confidence, add = T, plot.points = F, center.pch = F, lwd = 2, col = 'black')
	}
## Add legend:
legend('topright', lty = c(2, 1), lwd = c(2, 2), legend = c('old', 'young'), box.lwd = 2, cex = 1.25)
box(lwd = 2)

## Create vowel plot with ellipses for gender categories:

if (single) {
	quartz('', 8, 6)
	par(mai = c(1.25, 1.5, 0.5, 0.5))
	}
plot(1, 1, type = 'n',
	xlim = c(1750, 1000), ylim = c(650, 450),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
if (!single) text(1725, 460, labels = '(b)', cex = 2, font = 2)
if (!single) mtext(side = 3, 'Gender', cex = 2, font = 2, line = 0.6)
mtext(side = 1, 'F2', line = 3, cex = 1.75, font = 2)
mtext(side = 1, '(Lobanov-normalized)', line = 4.25, cex = 1.15, font = 2)
if (single) mtext(side = 2, 'F1', line = 5.25, cex = 1.75, font = 2)
if (single) mtext(side = 2, '(Lobanov-normalized)', line = 4, cex = 1.15, font = 2)
axis(side = 1, seq(1750, 1000, -250), lwd.ticks = 2, font = 2, cex.axis = 1.5)
if (single) axis(side = 2, seq(650, 450, -50), lwd.ticks = 2, font = 2, cex.axis = 1.5, las = 2)
text(xagr[-extremes,]$F2, xagr[-extremes,]$F1, labels = xagr[-extremes, ]$VowelIPA, cex = 1.15)
# Plot extreme speakers:
text(xagr[extremes, ]$F2,
	xagr[extremes, ]$F1, font = 2, labels = xagr[extremes, ]$VowelIPA, cex = 2)
## Add ellipses:
for (i in 1:4) {					# plots points
	this_vowel <- levels(xagr$Vowel)[i]
	male <- as.matrix(xagr[xagr$Vowel == this_vowel & xagr$Gender == 'M', c('F2', 'F1')])
	female <- as.matrix(xagr[xagr$Vowel == this_vowel & xagr$Gender == 'F', c('F2', 'F1')])
	dataEllipse(male,
		levels = confidence, add = T, plot.points = F, center.pch = F, lwd = 2, col = 'black', lty = 2)
	dataEllipse(female,
		levels = confidence, add = T, plot.points = F, center.pch = F, lwd = 2, col = 'black')
	}
## Add legend:
legend('topright', lty = c(2, 1), lwd = c(2, 2), legend = c('male', 'female'), box.lwd = 2, cex = 1.25)
box(lwd = 2)




##------------------------------------------------------------------
## Omnibus test of normalized F1/F2:
##------------------------------------------------------------------

## For better convergence, re-code the categorical predictors (deviation coding):

vowels <- mutate(vowels,
	Age01 = as.numeric(Age) - 1.5,
	Gender01 = as.numeric(Gender) - 1.5)

## First, test of the overall hypothesis that not all vowels behave the same with respect to Age:
## The PlaceOfArticulation:Vowel cannot be fitted because not all combinations attested:

xmdl.F1 <- lmer(F1 ~ Age01 + Gender01 + Vowel + Lg10WF_c + 
	Age01:Vowel + Gender01:Vowel + Lg10WF_c:Vowel + 
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 
	Voicing:Vowel + Syllables:Vowel + MannerOfArticulation:Vowel + 
	(1 +Vowel|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	vowels, REML = F)
xmdl.F2 <- lmer(F2 ~ Age01 + Gender01 + Vowel + Lg10WF_c +
	Age01:Vowel + Gender01:Vowel + Lg10WF_c:Vowel + 
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 
	Voicing:Vowel + Syllables:Vowel + MannerOfArticulation:Vowel + 
	(1+Vowel|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	vowels, REML = F)

## Null models without age/vowel interaction:

xmdl.F1.noage <- lmer(F1 ~ Age01 + Gender01 + Vowel + Lg10WF_c + 
	Gender01:Vowel + Lg10WF_c:Vowel + 
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	Voicing:Vowel + Syllables:Vowel + MannerOfArticulation:Vowel + 
	(1+Vowel|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	vowels, REML = F)
xmdl.F2.noage <- lmer(F2 ~ Age01 + Gender01 + Vowel + Lg10WF_c + 
	Gender01:Vowel + Lg10WF_c:Vowel + 
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 		
	Voicing:Vowel + Syllables:Vowel + MannerOfArticulation:Vowel + 
	(1+Vowel|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	vowels, REML = F)

## Null models without gender/vowel interaction:

xmdl.F1.nogender <- lmer(F1 ~ Age01 + Gender01 + Vowel + Lg10WF_c + 
	Age01:Vowel + Lg10WF_c:Vowel + 
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 		
	Voicing:Vowel + Syllables:Vowel + MannerOfArticulation:Vowel + 
	(1+Vowel|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	vowels, REML = F)
xmdl.F2.nogender <- lmer(F2 ~ Age01 + Gender01 + Vowel + Lg10WF_c + 
	Age01:Vowel + Lg10WF_c:Vowel + 
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 		
	Voicing:Vowel + Syllables:Vowel + MannerOfArticulation:Vowel + 
	(1+Vowel|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	vowels, REML = F)

## Do the likelihood ratio tests:

anova(xmdl.F1.noage, xmdl.F1, test = 'Chisq')		# p = 0.01
anova(xmdl.F2.noage, xmdl.F2, test = 'Chisq')		# p < 0.0001

anova(xmdl.F1.nogender, xmdl.F1, test = 'Chisq')		# p = 0.045
anova(xmdl.F2.nogender, xmdl.F2, test = 'Chisq')		# p = 0.023

## Checking normality and heteroskedasticity assumption:

quartz('', 11, 6)			# F1
par(mfrow = c(1, 2))	
qqnorm(residuals(xmdl.F1)); qqline(residuals(xmdl.F1)); hist(residuals(xmdl.F1))

quartz('', 11, 6)			# F2
par(mfrow = c(1, 2))
qqnorm(residuals(xmdl.F2)); qqline(residuals(xmdl.F2)); hist(residuals(xmdl.F2))

plot(fitted(xmdl.F1), residuals(xmdl.F1))
plot(fitted(xmdl.F2), residuals(xmdl.F2))

## Looks all pretty good.



##------------------------------------------------------------------
## Vowel-by-vowel analyses:
##------------------------------------------------------------------

## For better convergence, re-code the categorical predictors:

contrasts(vowels$Voicing) <- contr.sum(2) / 2
contrasts(vowels$Syllables) <- contr.sum(2) / 2
contrasts(vowels$MannerOfArticulation) <- contr.sum(4) / 4
contrasts(vowels$PlaceOfArticulation) <- contr.sum(4) / 4

## Full models:

summary(ae.F1 <- lmer(F1 ~ Age01 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'ae'), REML = F))
summary(ae.F2 <- lmer(F2 ~ Age01 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'ae'), REML = F))
summary(e.F1 <- lmer(F1 ~ Age01 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'e'), REML = F))
summary(e.F2 <- lmer(F2 ~ Age01 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'e'), REML = F))
summary(u.F1 <- lmer(F1 ~ Age01 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'u'), REML = F))
summary(u.F2 <- lmer(F2 ~ Age01 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'u'), REML = F))
summary(o.F1 <- lmer(F1 ~ Age01 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'o'), REML = F))
summary(o.F2 <- lmer(F2 ~ Age01 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'o'), REML = F))

## Null models with 'gender' (for testing 'Age' effect):

ae.F1.noage <- lmer(F1 ~ 1 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'ae'), REML = F)
ae.F2.noage <- lmer(F2 ~ 1 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'ae'), REML = F)
e.F1.noage <- lmer(F1 ~ 1 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'e'), REML = F)
e.F2.noage <- lmer(F2 ~ 1 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'e'), REML = F)
u.F1.noage <- lmer(F1 ~ 1 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'u'), REML = F)
u.F2.noage <- lmer(F2 ~ 1 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'u'), REML = F)
o.F1.noage <- lmer(F1 ~ 1 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'o'), REML = F)
o.F2.noage <- lmer(F2 ~ 1 + Gender01 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'o'), REML = F)

## Null models with 'age' (for testing 'Gender' effect):

ae.F1.nogender <- lmer(F1 ~ Age01 + 1 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'ae'), REML = F)
ae.F2.nogender <- lmer(F2 ~ Age01 + 1 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'ae'), REML = F)
e.F1.nogender <- lmer(F1 ~ Age01 + 1 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'e'), REML = F)
e.F2.nogender <- lmer(F2 ~ Age01 + 1 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'e'), REML = F)
u.F1.nogender <- lmer(F1 ~ Age01 + 1 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'u'), REML = F)
u.F2.nogender <- lmer(F2 ~ Age01 + 1 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'u'), REML = F)
o.F1.nogender <- lmer(F1 ~ Age01 + 1 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'o'), REML = F)
o.F2.nogender <- lmer(F2 ~ Age01 + 1 +
	Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
	(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
	subset(vowels, Vowel == 'o'), REML = F)

## Testing age effect:

anova(ae.F1.noage, ae.F1, test = 'Chisq')
anova(ae.F2.noage, ae.F2, test = 'Chisq')	# p < 0.0001
anova(e.F1.noage, e.F1, test = 'Chisq')		# p = 0.008
anova(e.F2.noage, e.F2, test = 'Chisq')		# p = 0.004
anova(o.F1.noage, o.F1, test = 'Chisq')
anova(o.F2.noage, o.F2, test = 'Chisq')
anova(u.F1.noage, u.F1, test = 'Chisq')		# p = 0.098
anova(u.F2.noage, u.F2, test = 'Chisq')		# p = 0.016
	
## Testing gender effect:

anova(ae.F1.nogender, ae.F1, test = 'Chisq')
anova(ae.F2.nogender, ae.F2, test = 'Chisq')			# p = 0.014
anova(e.F1.nogender, e.F1, test = 'Chisq')			# p = 0.046
anova(e.F2.nogender, e.F2, test = 'Chisq')
anova(o.F1.nogender, o.F1, test = 'Chisq')			# p = 0.064
anova(o.F2.nogender, o.F2, test = 'Chisq')
anova(u.F1.nogender, u.F1, test = 'Chisq')
anova(u.F2.nogender, u.F2, test = 'Chisq')			# p = 0.03



##------------------------------------------------------------------
## Influence diagnostics (leave-one-out by subjects):
##------------------------------------------------------------------

## Create speaker list:

speakers <- as.character(unique(vowels$Speaker))

## Create objects to save results:

ae.F1tab <- data.frame(AgeY = numeric(length(speakers)), GenderM = numeric(length(speakers)))
ae.F2tab <- ae.F1tab
e.F1tab <- ae.F1tab
e.F2tab <- ae.F1tab
o.F1tab <- ae.F1tab
o.F2tab <- ae.F1tab
u.F1tab <- ae.F1tab
u.F2tab <- ae.F1tab

## Loop through and exclude one speaker each:

for(i in 1:length(speakers)){
	xtemp <- vowels[vowels != speakers[i],]
	xtemp$Speaker <- factor(xtemp$Speaker)

	ae.F1tab[i,] <- summary(lmer(F1 ~ Age01 + Gender01 +
		Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
		(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
		subset(xtemp, Vowel == 'ae'), REML = F))$coefficients[c('Age01', 'Gender01'), c('t value')]
	ae.F2tab[i,] <- summary(lmer(F2 ~ Age01 + Gender01 +
		Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
		(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
		subset(xtemp, Vowel == 'ae'), REML = F))$coefficients[c('Age01', 'Gender01'), c('t value')]
	e.F1tab[i,] <- summary(lmer(F1 ~ Age01 + Gender01 +
		Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
		(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
		subset(xtemp, Vowel == 'e'), REML = F))$coefficients[c('Age01', 'Gender01'), c('t value')]
	e.F2tab[i,] <- summary(lmer(F2 ~ Age01 + Gender01 +
		Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
		(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
		subset(xtemp, Vowel == 'e'), REML = F))$coefficients[c('Age01', 'Gender01'), c('t value')]
	u.F1tab[i,] <- summary(lmer(F1 ~ Age01 + Gender01 +
		Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
		(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
		subset(xtemp, Vowel == 'u'), REML = F))$coefficients[c('Age01', 'Gender01'), c('t value')]
	u.F2tab[i,] <- summary(lmer(F2 ~ Age01 + Gender01 +
		Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
		(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
		subset(xtemp, Vowel == 'u'), REML = F))$coefficients[c('Age01', 'Gender01'), c('t value')]
	o.F1tab[i,] <- summary(lmer(F1 ~ Age01 + Gender01 +
		Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
		(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
		subset(xtemp,Vowel == 'o'), REML = F))$coefficients[c('Age01', 'Gender01'), c('t value')]
	o.F2tab[i,] <- summary(lmer(F2 ~ Age01 + Gender01 +
		Voicing + Syllables + MannerOfArticulation + PlaceOfArticulation + 	
		(1|Speaker) + (1|Context) + (0+Age01|Context) + (0+Gender01|Context),
		subset(xtemp, Vowel == 'o'), REML = F))$coefficients[c('Age01', 'Gender01'), c('t value')]
	
	cat(paste(i, '\n'))
	}

## Look at results (it's o.k. here to follow a "|t > 2| is significant" heuristic):

ae.F1tab				# excluding speaker 28 would make Age effect significant
ae.F2tab
e.F1tab				# gender result not robust (hovering around ~2)
e.F2tab
o.F1tab				# gender result not robust (hovering around ~2)
o.F2tab
u.F1tab				# age result not strong, gender result emerges if subject 20 is excluded
u.F2tab				# gender result unduly affected by speaker 23, 24 and 26 (not significant if any of these is excluded)


##------------------------------------------------------------------
## Calculate Euclidian distance from 'F1988' and 'M1992' vowel means:
##------------------------------------------------------------------

## F1 & F2 values of extreme speakers ('F1988a' for 'e' and 'M1992a' for 'ae'):

extreme_vals <- xagr[extremes, c('F1', 'F2')]

## Extract F1/F2 values:

eLEAD <- as.numeric(extreme_vals[1, c('F1', 'F2')])
aeLEAD <- as.numeric(extreme_vals[2, c('F1', 'F2')])

## Euclidian distance of /e/ vowels to eLEAD:

vowels$ED_e <- rep(NA, nrow(vowels))
F1dist <- (vowels[vowels$Vowel == 'e', ]$F1 - eLEAD[1]) ^ 2
F2dist <- (vowels[vowels$Vowel == 'e', ]$F2 - eLEAD[2]) ^ 2
ED_e <- sqrt(F1dist + F2dist)

## Back into the data frame:

vowels[vowels$Vowel == 'e', ]$ED_e <- ED_e

## Euclidian distance of /e/ vowels to eLEAD:

vowels$ED_ae <- rep(NA,nrow(vowels))
F1dist <- (vowels[vowels$Vowel == 'ae', ]$F1 - aeLEAD[1]) ^ 2
F2dist <- (vowels[vowels$Vowel == 'ae', ]$F2 - aeLEAD[2]) ^ 2
ED_ae <- sqrt(F1dist + F2dist)

## Back into the data frame:

vowels[vowels$Vowel == 'ae', ]$ED_ae <- ED_ae

## Create a 'both shift' variable:

vowels$ED_both <- vowels$ED_ae
vowels[vowels$Vowel == 'e', ]$ED_both <- vowels[vowels$Vowel == 'e', ]$ED_e



##------------------------------------------------------------------
## Analyze and plot Labov hierarchy with the new Euclidian distance measure:
##------------------------------------------------------------------

## Make that categorical:
## (It is possible to code this continuously, which preserves the ordinal information;
## the results remain the same)

vowels$LabovHierarchy <- as.factor(vowels$LabovHierarchy)

## Create an LMER of that:

hierarchy.mdlE <- lmer(ED_e ~ LabovHierarchy + 
	(1|Speaker) + (1+LabovHierarchy|Context),
	subset(vowels, Vowel == 'e'), REML = F)
summary(hierarchy.mdlE)
hierarchy.mdlAE <- lmer(ED_ae ~ LabovHierarchy + 
	(1|Speaker) + (1+LabovHierarchy|Context),
	subset(vowels, Vowel == 'ae'), REML = F)
summary(hierarchy.mdlAE)
hierarchy.both <- lmer(ED_both ~ LabovHierarchy + 
	(1|Speaker) + (1+LabovHierarchy|Context),
	subset(vowels, (Vowel == 'ae' | Vowel == 'e')), REML = F)		# convergence issue
summary(hierarchy.both)

## Create null models:

hierarchy.mdlE.null <- lmer(ED_e ~ 1 + 
	(1|Speaker) + (1+LabovHierarchy|Context),
	subset(vowels, Vowel == 'e'), REML = F)
hierarchy.mdlAE.null <- lmer(ED_ae ~ 1 + 
	(1|Speaker) + (1+LabovHierarchy|Context),
	subset(vowels, Vowel == 'ae'), REML = F)			# convergence issue
hierarchy.both.null <- lmer(ED_both ~ 1 + 
	(1|Speaker) + (1+LabovHierarchy|Context),
	subset(vowels, (Vowel == 'ae' | Vowel == 'e')), REML = F)

## Likelihood ratio tests:

anova(hierarchy.mdlE.null, hierarchy.mdlE, test = 'Chisq')		# p = 0.08
anova(hierarchy.mdlAE.null, hierarchy.mdlAE, test = 'Chisq')		# p < 0.0001
anova(hierarchy.both.null, hierarchy.both, test = 'Chisq')		# p = 0.0002

## Function that computes predicted values for LMER model:

predict.lmer <- function(fit, data, pred = 'LabovHierarchy', depvar = 'ED_e') {
	newdata <- data.frame(unique(data[, pred]))
	newdata <- cbind(newdata,numeric(nrow(newdata)))
	names(newdata) <- c('LabovHierarchy', depvar)
	mm <- model.matrix(terms(fit), newdata)
	newdata[, depvar] <- predict(fit, newdata, re.form = NA)
	pvar1 <- diag(mm %*% tcrossprod(vcov(fit), mm))
	newdata$UB <- newdata[, depvar] + 1.96 * sqrt(pvar1)
	newdata$LB <- newdata[, depvar] - 1.96 * sqrt(pvar1)
	return(newdata)
	}

E.pred <- predict.lmer(hierarchy.mdlE, vowels)
AE.pred <- predict.lmer(hierarchy.mdlAE, vowels, depvar = 'ED_ae')
both.pred <- predict.lmer(hierarchy.both, vowels, depvar = 'ED_both')

## Make the plot:

quartz('', 8, 6); par(mai = c(1, 1.5, 1, 0.5))
plot(1, 1, type = 'n', xlim = c(0.5, 4.5), ylim = c(0, 200), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
axis(side = 1, at = 1:4, labels = c('Young/\nFemale', 'Young/\nMale', 'Old/\nFemale', 'Old/\nMale'),
	font = 2, cex.axis = 1.35, line = 2, tick = F)
points(as.numeric(both.pred$LabovHierarchy), both.pred$ED_both, pch = 15, cex = 1.5)
arrows(x0 = as.numeric(both.pred$LabovHierarchy), x1 = as.numeric(both.pred$LabovHierarchy),
	y0 = both.pred$LB, y1 = both.pred$UB,
	code = 3, angle = 90, lwd = 2)
axis(side = 2, seq(0, 200, 50), lwd.ticks = 2, font = 2, cex.axis = 1.5, las = 2)
mtext(side = 2, 'Euclidian Distance to Shift Leader', line = 5.25, cex = 1.75, font = 2)
mtext(side = 2, '(averaged over /æ/ and /ɛ/)', line = 3.8, cex = 1.35, font = 2)
box(lwd = 2)



##------------------------------------------------------------------
## Add speaker production means and 'shift-leadingness' to perception data:
##------------------------------------------------------------------

## Get speaker averages:

library(plyr)
EDs <- ddply(vowels[, c('Speaker', 'ED_e', 'ED_ae', 'ED_both')], .(Speaker), numcolwise(mean), na.rm = T)
formants <- ddply(vowels[, c('Speaker', 'Vowel', 'F1', 'F2')], .(Speaker, Vowel), numcolwise(mean))

## Reformat 'formants' table:

formants_new <- data.frame(Speaker=formants[formants$Vowel == 'ae', ]$Speaker)
formants_new$F1_ae <- formants[formants$Vowel == 'ae', ]$F1
formants_new$F2_ae <- formants[formants$Vowel == 'ae', ]$F2
formants_new$F1_e <- formants[formants$Vowel == 'e', ]$F1
formants_new$F2_e <- formants[formants$Vowel == 'e', ]$F2
formants_new$F1_o <- formants[formants$Vowel == 'o', ]$F1
formants_new$F2_o <- formants[formants$Vowel == 'o', ]$F2
formants_new$F1_u <- formants[formants$Vowel == 'u', ]$F1
formants_new$F2_u <- formants[formants$Vowel == 'u', ]$F2

## Combine into a speaker mean table:

speakers <- cbind(formants_new, EDs[, c('ED_e', 'ED_ae', 'ED_both')])

## Load in perception data:

xdata <- read.csv('perception_processed.csv')

## Add to perception data:

xdata <- cbind(xdata, speakers[match(xdata$Subject, speakers$Speaker), -1])

## Write to a new table:

write.table(xdata, 'perception_processed_with_production.csv',
	sep = ',', row.names = F)



##------------------------------------------------------------------
## Analyze AE vs. E shift correlation:
##------------------------------------------------------------------

## AE vs. E shift correlation:

cor.test(speakers$ED_e, speakers$ED_ae)			# sig. t(26) = 4.31, p<0.0001

## Add gender and age information to mean table:

speakers$Age <- vowels[match(speakers$Speaker,vowels$Speaker), ]$Age
speakers$Gender <- vowels[match(speakers$Speaker,vowels$Speaker), ]$Gender

## Combine age and gender info to form groups:

speakers$Group <- paste(speakers$Gender, speakers$Age, sep = ':')

## Test whether there is an interaction for the correlation with group (hopefully not):

anova(lm(ED_e ~ ED_ae * Group, speakers))

## Regression model for plotting the line and CI:

xmdl <- lm(ED_e ~ ED_ae, speakers)

## Predictions for CIs:

xvals <- seq(0, 250, 0.01)
newdata <- data.frame(ED_ae = xvals)
newdata$UB <- predict(xmdl, newdata = newdata, se.fit = T)$fit + 1.96 * predict(xmdl, newdata = newdata, se.fit = T)$se.fit
newdata$LB <- predict(xmdl, newdata = newdata, se.fit = T)$fit - 1.96 * predict(xmdl, newdata = newdata, se.fit = T)$se.fit

## Make a plot of this:

quartz('', 8, 6);par(mai = c(1.25, 1.75, 0.75, 0.5))
plot(speakers$ED_ae, speakers$ED_e, xlim = c(0, 250), ylim = c(0, 250), type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
mtext(side = 1, 'Euclidian distance to /æ/ shift leader', line = 3.25, cex = 1.75, font = 2)
mtext(side = 2, 'Euclidian distance\nto /ɛ/ shift leader', line = 4, cex = 1.75, font = 2)
axis(side = 1, seq(0, 250, 50), lwd.ticks = 2, font = 2, cex.axis = 1.5)
axis(side = 2, seq(0, 250, 50), lwd.ticks = 2, font = 2, cex.axis = 1.5, las = 2)
polygon(x = c(xvals, rev(xvals)), y = c(newdata$UB, rev(newdata$LB)), col = rgb(0, 0, 0, 0.25), border = NA)
abline(xmdl, lwd = 4)
points(speakers[speakers$Age == 'Y', ]$ED_ae, speakers[speakers$Age == 'Y', ]$ED_e, cex = 1.5, pch = 19, lwd = 1.25)
points(speakers[speakers$Age == 'O', ]$ED_ae, speakers[speakers$Age == 'O', ]$ED_e, cex = 1.5, pch = 17, lwd = 1.25)
legend('bottomright', legend = c('old', 'young'), box.lwd = 2,cex = 1.25, pch = c(17, 19))
box(lwd = 2)



##------------------------------------------------------------------
## Analyze shift / age correlation:
##------------------------------------------------------------------

## In analogy to Boberg (2005: 137) [see paper]

## Get age info from main table:

speakers$BirthYear <- vowels[match(speakers$Speaker,vowels$Speaker),]$BirthYear

## Correlation table for ED:

cor(speakers[, c(grep('ED', colnames(speakers)), ncol(speakers))])

## Tests:

cor.test(speakers$BirthYear, speakers$ED_ae)
cor.test(speakers$BirthYear, speakers$ED_e)
cor.test(speakers$BirthYear, speakers$ED_both)

## Construct table for formal test of the idea that /ae/ is more strongly correlated with age than /e/:

age_cor <- data.frame(BirthYear = rep(speakers$BirthYear, 2))
age_cor$Vowel <- c(rep('ae', nrow(speakers)), rep('e', nrow(speakers)))
age_cor$ED <- c(speakers$ED_ae, speakers$ED_e)

## Regression for formal test of the idea that /ae/ is more strongly correlated with age than /e/:

summary(lm(ED ~ BirthYear * Vowel,age_cor))

## For individual formants and vowels:

cor.test(speakers$BirthYear, speakers$F1_ae)
cor.test(speakers$BirthYear, speakers$F2_ae)			# sig.
cor.test(speakers$BirthYear, speakers$F1_e)			# sig.
cor.test(speakers$BirthYear, speakers$F2_e)			# sig.
cor.test(speakers$BirthYear, speakers$F1_o)
cor.test(speakers$BirthYear, speakers$F2_o)
cor.test(speakers$BirthYear, speakers$F1_u)
cor.test(speakers$BirthYear, speakers$F2_u)			# sig., but not after correction



##------------------------------------------------------------------
## Continuous age and shift leadingness analysis:
##------------------------------------------------------------------

## Add age information:

speakers$Age <- vowels[match(speakers$Speaker, vowels$Speaker),]$Age

## Regression model for old group:

xvals <- seq(1920, 2000, 0.1)
newdata <- data.frame(BirthYear = xvals)

summary(xmdl.old <- lm(ED_both ~ BirthYear, subset(speakers, Age == 'O')))
newdata$old_UB <- predict(xmdl.old, newdata = newdata,se.fit = T)$fit +
	 1.96 * predict(xmdl.old, newdata = newdata, se.fit = T)$se.fit
newdata$old_LB <- predict(xmdl.old, newdata = newdata, se.fit = T)$fit -
	 1.96 * predict(xmdl.old, newdata = newdata, se.fit = T)$se.fit

## Regression model for young group:

summary(xmdl.young <- lm(ED_both ~ BirthYear, subset(speakers, Age == 'Y')))
newdata$young_UB <- predict(xmdl.young,
	newdata=newdata, se.fit = T)$fit + 1.96 * predict(xmdl.young, newdata = newdata, se.fit = T)$se.fit
newdata$young_LB <- predict(xmdl.young,
	newdata=newdata, se.fit = T)$fit - 1.96 * predict(xmdl.young, newdata = newdata, se.fit = T)$se.fit

## Make a double plot of this:

quartz('', 11, 5); par(mai  =c(1.5, 0.25, 0.25, 0.25), mfrow = c(1, 2), omi = c(0, 1.25, 0, 0.25))
plot(speakers$BirthYear, speakers$ED_both, xlim = c(1935, 1965), ylim = c(75, 225),
	type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
mtext(side = 1,  'Birth Year\n(old speakers)',line = 4.75,cex = 1.75, font = 2)
mtext(side = 2,'Euclidian distance\nto shift leader (/æ/ and /ɛ/)',line=3.8,cex = 1.75, font = 2)
axis(side = 1, seq(1935, 1965, 10), lwd.ticks = 2, font = 2, cex.axis = 1.5)
axis(side = 2, seq(75, 225, 50), lwd.ticks = 2, font = 2, cex.axis = 1.5)
polygon(x = c(xvals, rev(xvals)), y = c(newdata$old_UB, rev(newdata$old_LB)), col =rgb(0, 0, 0, 0.25), border = NA)
abline(xmdl.old, lwd = 4)
points(speakers[speakers$Age == 'O', ]$BirthYear, speakers[speakers$Age == 'O', ]$ED_both, cex = 1.5, pch = 19, lwd = 1.25)
box(lwd = 2)

plot(speakers$BirthYear, speakers$ED_both, xlim = c(1980, 2000), ylim = c(0, 250),
	type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
mtext(side = 1, 'Birth Year\n(young speakers)', line = 4.75, cex = 1.75, font = 2)
polygon(x = c(xvals, rev(xvals)), y = c(newdata$old_UB, rev(newdata$old_LB)),
	col = rgb(0, 0, 0, 0.25), border = NA)
abline(xmdl.old, lwd = 4)
points(speakers[speakers$Age == 'Y',]$BirthYear, speakers[speakers$Age == 'Y',]$ED_both,
	cex = 1.5, pch = 19, lwd = 1.25)
axis(side = 1, seq(1980, 2000, 5), lwd.ticks = 2, font = 2, cex.axis = 1.5)
box(lwd = 2)



##------------------------------------------------------------------
## Does /ae/ correlate with neighboring "u" (BUT)?
##------------------------------------------------------------------

## Models:

summary(lm(ED_ae ~ F2_u, speakers))		# p = 0.0002
summary(lm(ED_ae ~ F1_u, speakers))		# not F1 (makes senses)

summary(lm(ED_ae ~ F2_o, speakers))
summary(lm(ED_ae ~ F1_o, speakers))



##------------------------------------------------------------------
## Distance between /e/ and /ae/ - is it constant across speakers?
##------------------------------------------------------------------

## Calculate /ae/ to /e/ distance:

F1dist <- (speakers$F1_ae - speakers$F1_e) ^ 2
F2dist <- (speakers$F2_ae - speakers$F2_e) ^ 2
speakers$AE_E_dist <- sqrt(F1dist + F2dist)

## See whether this is affect by age and gender:

anova(lm(AE_E_dist ~ Age * Gender, speakers))	# no effects; distance constant!



