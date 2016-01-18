## Bodo Winter
## May 3, 2015; Amended May 15-17, 2015; May 19, 2015
## Adapted new style and cleaned code: Jan 17, 2016
## Canadian Vowel Shift perception Data Analysis

##------------------------------------------------------------------
## Load in data:
##------------------------------------------------------------------

## Set to main analysis directory (everything else is relative to that):

mainDir <- '/Users/teeniematlock/Desktop/research/kettig_canadian_vowel_shift/analysis'

## Set to sub-directory that contains by-participant perception data:

subDir <- 'processed_data'
setwd(file.path(mainDir, subDir))

## Load in data:

xdata <- read.csv('perception_processed_with_production.csv')

## Packages:

library(lme4)
library(mgcv)
library(car)
library(party)		# for binary partioning of the response function
library(plyr)

## Add IPA symbols to main table:

xdata$VowelIPA <- as.character(xdata$ChosenVowel)
xdata[xdata$ChosenVowel == 'ae', ]$VowelIPA <- 'æ'
xdata[xdata$ChosenVowel == 'e', ]$VowelIPA <- 'ɛ'
xdata[xdata$ChosenVowel == 'u', ]$VowelIPA <- 'ʌ'
xdata[xdata$ChosenVowel == 'o', ]$VowelIPA <- 'ɔ'

## Make different columns for each Vowel:

xdata$AE <- ifelse(xdata$ChosenVowel == 'ae', 1, 0)
xdata$E <- ifelse(xdata$ChosenVowel == 'e', 1, 0)
xdata$O <- ifelse(xdata$ChosenVowel == 'o', 1, 0)
xdata$U <- ifelse(xdata$ChosenVowel == 'u', 1, 0)



##------------------------------------------------------------------
## Make a plot with 'mean F2/F1' at which Subject categorizes stimulus as Vowel:
##------------------------------------------------------------------

## Create a table with mean F1/F2 per categorization:

xagr <- aggregate(F1 ~ ChosenVowel * Subject, xdata, mean)
xagr$F2 <- aggregate(F2 ~ ChosenVowel * Subject, xdata, mean)$F2

## Add age and gender information:

xagr <- cbind(xagr, xdata[match(xagr$Subject, xdata$Subject), c('Age', 'Gender', 'BirthYear')])
xagr$VowelIPA <- xdata[match(xagr$ChosenVowel, xdata$ChosenVowel), ]$VowelIPA

## Change rownames:

rownames(xagr) <- 1:nrow(xagr)

## Set confidence level of ellipsoid:

confidence <- 0.85	# different from other plot - needs to be mentioned in paper

## Create vowel plot:

quartz('', 8, 6); par(mai = c(1, 1.5, 1, 0.75))
plot(1, 1, type = 'n', xlim = c(1950, 1200), ylim = c(950, 675), xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
mtext(side = 1, 'Synthetic F2 continuum (Hz)', line = 3, cex = 1.75, font = 2)
mtext(side = 2, 'Synthetic F1 continuum (Hz)', line = 4.35, cex = 1.75, font = 2)
mtext(side = 3, 'Points of categorization', line = 0.9, cex = 2.25, font = 2)
axis(side = 1, seq(1950, 1200, -150), lwd.ticks = 2, font = 2, cex.axis = 1.5)
axis(side = 2, seq(950, 700, -50), lwd.ticks = 2, font = 2, cex.axis = 1.5,las=2)
for(i in 1:4){					# do this for Powerpoint plotting
	this_vowel <- levels(xagr$ChosenVowel)[i]
	text(xagr[xagr$ChosenVowel==this_vowel, ]$F2, xagr[xagr$ChosenVowel==this_vowel, ]$F1,
		labels=xagr[xagr$ChosenVowel==this_vowel, ]$VowelIPA, cex = 1.15)
	}
## Add ellipses:
for(i in 1:4){					# plots points
	this_vowel <- levels(xagr$ChosenVowel)[i]
	old <- as.matrix(xagr[xagr$ChosenVowel == this_vowel & xagr$Age == 'old', c('F2', 'F1')])
	young <- as.matrix(xagr[xagr$ChosenVowel == this_vowel & xagr$Age == 'young', c('F2', 'F1')])
	dataEllipse(old,
		levels = confidence, add = T, plot.points = F, center.pch = F, lwd = 2, col = 'black', lty = 2)
	dataEllipse(young,
		levels = confidence, add = T, plot.points = F, center.pch = F, lwd = 2, col = 'black')
	}
## Add legend:
legend('topright', lty = c(2, 1), lwd = c(2, 2), legend = c('old', 'young'), box.lwd = 2, cex = 1.25)
box(lwd = 2)



##------------------------------------------------------------------
## Generate categorization tables for descriptive analysis:
##------------------------------------------------------------------

## Function that takes a table and generates matrix with most frequent IPA character:

generate_categorization_table <- function(dataframe, threshold, maxtable = F) {
	xtab <- table(dataframe$F1, dataframe$F2, dataframe$VowelIPA)
	M <- matrix(character(6 * 16), nrow = 6, ncol = 16)
	rownames(M) <- sort(unique(dataframe$F1))
	colnames(M) <- rev(sort(unique(dataframe$F2)))
	
	## Reverse contents of xtab:
	
	xtab[, , 1] <- xtab[, 16:1, 1]
	xtab[, , 2] <- xtab[, 16:1, 2]
	xtab[, , 3] <- xtab[, 16:1, 3]
	xtab[, , 4] <- xtab[, 16:1, 4]

	for (i in 1:6) {			# majority rule, must reach mininum 70% (or another threshold value)
		for (j in 1:16) {			
			this_position <- xtab[i, j, ]
			if (any(this_position / sum(this_position) > threshold)) {
				M[i, j] <- names(which.max(xtab[i, j, ]))
				}
			}
		}
	
	if (maxtable) {
		for (i in 1:6) {
			for (j in 1:16) {
				this_position <- xtab[i, j, ]
				M[i, j] <- names(this_position[which.max(this_position)])
				}
			}
		}

	return(M)
	}

## Make tables:

generate_categorization_table(xdata[xdata$Age == 'old' & xdata$Gender == 'M', ],
	threshold = 0.7, maxtable = T)
generate_categorization_table(xdata[xdata$Age == 'old' & xdata$Gender == 'F', ],
	threshold = 0.7, maxtable = T)
generate_categorization_table(xdata[xdata$Age == 'young' & xdata$Gender == 'M', ],
	threshold = 0.7, maxtable = T)
generate_categorization_table(xdata[xdata$Age == 'young' & xdata$Gender == 'F', ],
	threshold = 0.7, maxtable = T)
	
## Old versus young tables:

generate_categorization_table(xdata[xdata$Age == 'old', ],
	threshold = 0.7, maxtable = T)
generate_categorization_table(xdata[xdata$Age == 'young', ],
	threshold = 0.7, maxtable = T)

## Function that makes a plot of this:

plot_categorizations <- function(dataframe, threshold, maxtable = T) {
	M <- generate_categorization_table(dataframe, threshold = threshold, maxtable = maxtable)
	plot(1, 1, xlim = c(0.55, 15.4), ylim = c(0.15, 5.7),
		type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab ='')
	for (i in 1:16) {
		for (j in 1:6) {
			text(x = i - 0.5, y = j - 0.5, labels = M[6:1, ][j, i], font = 1, cex = 1.05)
			}
		}
	abline(h = 1:6, lty = 1, col = 'gray')
	abline(v = 1:16, lty = 1, col = 'gray')
	box(lwd = 2)
	}

quartz('', 9, 6)
par(mfrow = c(2, 2), mai = c(0.05, 0.05, 0.05, 0.05), omi = c(1, 1, 0.75, 0.25))
plot_categorizations(xdata[xdata$Age == 'old' & xdata$Gender == 'M', ], threshold = 0.7)
mtext(text = 'Old', side = 2, cex = 1.5, line = 3.75, font = 2)
mtext(text = 'Male', side = 3, cex = 1.5, line = 0.9, font = 2)
axis(side = 2, at = seq(0.5, 5.5, 1), labels = seq(950, 700, -50), font = 2, las = 2, lwd.ticks = 2)
plot_categorizations(xdata[xdata$Age == 'old' & xdata$Gender == 'F', ], threshold = 0.7)
mtext(text = 'Female', side = 3, cex = 1.5, line = 0.9, font = 2)
plot_categorizations(xdata[xdata$Age == 'young' & xdata$Gender == 'M', ], threshold = 0.7)
mtext(text = 'Young', side = 2, cex = 1.5, line = 3.75, font = 2)
axis(side = 2, at = seq(0.5, 5.5, 1), labels = seq(950, 700, -50), font = 2, las = 2, lwd.ticks = 2)
axis(side = 1, at = seq(0.5, 15.5, 2), labels = seq(1950, 1200, -100), font = 2, las = 2, lwd.ticks = 2)
plot_categorizations(xdata[xdata$Age == 'young' & xdata$Gender == 'F', ], threshold = 0.7)
axis(side = 1, at = seq(0.5, 15.5, 2), labels = seq(1950, 1200, -100), font = 2, las = 2, lwd.ticks = 2)




##------------------------------------------------------------------
## Categorization curves along F2:
##------------------------------------------------------------------

## Make a speakers mean column:

EDs <- ddply(xdata[, c('Subject', 'ED_e', 'ED_ae', 'ED_both')], .(Subject), numcolwise(mean), na.rm = T)

## Check for age & gender effects:

EDs <- cbind(EDs, xdata[match(EDs$Subject, xdata$Subject), c('Age', 'Gender')])

## Extract speakers to loop through:

listeners <- unique(xdata$Subject)

## Vectors to store results:

AE_thresh_F2 <- rep(NA, length(listeners))
E_thresh_F2 <- rep(NA, length(listeners))
O_thresh_F2 <- rep(NA, length(listeners))
U_thresh_F2 <- rep(NA, length(listeners))

## Loop through speakers:

for (i in 1:length(listeners)) {
	xtemp <- filter(xdata, Subject == listeners[i])
	
	# simple binary thresholds:
	
	AE_split_F2 <- ctree(AE ~ F2, xtemp)@tree$psplit$splitpoint
	E_split_F2 <- ctree(E ~ F2, xtemp)@tree$psplit$splitpoint
	O_split_F2 <- ctree(O ~ F2, xtemp)@tree$psplit$splitpoint
	U_split_F2 <- ctree(U ~ F2, xtemp)@tree$psplit$splitpoint

	# save:

	if(!is.null(AE_split_F2)) AE_thresh_F2[i] <- AE_split_F2
	if(!is.null(E_split_F2)) E_thresh_F2[i] <- E_split_F2
	if(!is.null(O_split_F2)) O_thresh_F2[i] <- O_split_F2
	if(!is.null(U_split_F2)) U_thresh_F2[i] <- U_split_F2

	}

## Create F2 ED dataframe:

ED_F2 <- EDs
ED_F2$AE <- AE_thresh_F2
ED_F2$E <- E_thresh_F2
ED_F2$O <- O_thresh_F2
ED_F2$U <- U_thresh_F2

## Correlate thresholds with each other:

these_columns <- c('AE', 'E', 'O', 'U')
M_F2 <- matrix(rep(NA, 4 * 4), nrow = 4)
rownames(M_F2) <- these_columns
colnames(M_F2) <- these_columns
M_F2_sig <- M_F2

## Loop through and perform correlations:

for (i in 1:4) {
	for (j in 1:4) {
		M_F2[i, j] <- cor(ED_F2[, these_columns[i]], ED_F2[, these_columns[j]], use = 'complete.obs')
		M_F2_sig[i, j] <- cor.test(ED_F2[, these_columns[i]], ED_F2[, these_columns[j]], use = 'complete.obs')$p.value
		}
	}

## Check results:

round(M_F2, 2)		# E and U have a high r = 0.71
M_sig	# E and U are significantly correlated p = 0.003

## Correlate shift-leadingness:

prod_M_F2 <- matrix(rep(NA, 2 * 4), nrow = 2)
rownames(prod_M_F2) <- c('ED_ae', 'ED_e')
colnames(prod_M_F2) <- these_columns
prod_M_F2_sig <- prod_M_F2

for (i in 1:2) {
	for (j in 1:4) {
		prod_M_F2[i, j] <- cor(ED_F2[, rownames(prod_M_F2)[i]], ED_F2[, these_columns[j]], use = 'complete.obs')
		prod_M_F2_sig[i, j] <- cor.test(ED_F2[, rownames(prod_M_F2)[i]], ED_F2[, these_columns[j]], use = 'complete.obs')$p.value
		}
	}

## Check results:

prod_M_F2
prod_M_F2_sig		# n.s.

## Check age / gender effects on categorization curves:

anova(lm(AE ~ Age + Gender, EDs_F2))
anova(lm(E ~ Age + Gender, EDs_F2))
anova(lm(U ~ Age + Gender, EDs_F2))		# n.s.
anova(lm(O ~ Age + Gender, EDs_F2))



##------------------------------------------------------------------
## Categorization curves along F1:
##------------------------------------------------------------------

## Extract speakers to loop through:

listeners <- unique(xdata$Subject)

## Vectors to store results:

AE_thresh_F1 <- rep(NA, length(listeners))
E_thresh_F1 <- rep(NA, length(listeners))
O_thresh_F1 <- rep(NA, length(listeners))
U_thresh_F1 <- rep(NA, length(listeners))

## Loop through speakers:

for (i in 1:length(listeners)) {
	xtemp <- filter(xdata, Subject == listeners[i])
		
	# simple binary thresholds:
	
	AE_split_F1 <- ctree(AE ~ F1, xtemp)@tree$psplit$splitpoint
	E_split_F1 <- ctree(E ~ F1, xtemp)@tree$psplit$splitpoint
	O_split_F1 <- ctree(O ~ F1, xtemp)@tree$psplit$splitpoint
	U_split_F1 <- ctree(U ~ F1, xtemp)@tree$psplit$splitpoint

	# save:

	if(!is.null(AE_split_F1)) AE_thresh_F1[i] <- AE_split_F1
	if(!is.null(E_split_F1)) E_thresh_F1[i] <- E_split_F1
	if(!is.null(O_split_F1)) O_thresh_F1[i] <- O_split_F1
	if(!is.null(U_split_F1)) U_thresh_F1[i] <- U_split_F1

	}

## Create F2 ED dataframe:

ED_F1 <- EDs
ED_F1$AE <- AE_thresh_F1
ED_F1$E <- E_thresh_F1
ED_F1$O <- O_thresh_F1
ED_F1$U <- U_thresh_F1

## Correlate thresholds with each other:

these_columns <- c('AE', 'E', 'O', 'U')
M_F1 <- matrix(rep(NA, 4 * 4), nrow = 4)
rownames(M_F1) <- these_columns
colnames(M_F1) <- these_columns
M_F1_sig <- M_F1

## Loop through and perform correlations:

for (i in 1:4) {
	for (j in 1:4) {
		M_F1[i, j] <- cor(ED_F1[, these_columns[i]], ED_F1[, these_columns[j]], use = 'complete.obs')
		M_F1_sig[i, j] <- cor.test(ED_F1[, these_columns[i]], ED_F1[, these_columns[j]], use = 'complete.obs')$p.value
		}
	}

## Check results:

round(M_F1, 2)		# O and U have a high r = -0.91
M_F1_sig	# O and U have p = 0.03

## Correlate shift-leadingness:

prod_M_F1 <- matrix(rep(NA, 2 * 4), nrow = 2)
rownames(prod_M_F1) <- c('ED_ae', 'ED_e')
colnames(prod_M_F1) <- these_columns
prod_M_F1_sig <- prod_M_F1

for (i in 1:2) {
	for (j in 1:4) {
		prod_M_F1[i, j] <- cor(ED_F1[, rownames(prod_M)[i]], ED_F1[, these_columns[j]], use = 'complete.obs')
		prod_M_F1_sig[i, j] <- cor.test(ED_F1[, rownames(prod_M)[i]], ED_F1[, these_columns[j]], use = 'complete.obs')$p.value
		}
	}

## Check results:

prod_M_F1
prod_M_F1_sig		# n.s.

## Check for age & gender effects:

EDs <- cbind(EDs_F1, xdata[match(EDs_F1$Subject, xdata$Subject), c('Age', 'Gender')])

## Check age / gender effects on categorization curves:

anova(lm(AE ~ Age + Gender, EDs_F1))
anova(lm(E ~ Age + Gender, EDs_F1))
anova(lm(U ~ Age + Gender, EDs_F1))		# n.s.
anova(lm(O ~ Age + Gender, EDs_F1))




##------------------------------------------------------------------
## Make plots for F1 and F2 perceptual correlations:
##------------------------------------------------------------------

## Perform correlations for detailed statistics to report:

cor.test(ED_F1$O, ED_F1$U, use = 'complete.obs')	# O vs. U; only four datapoints
cor.test(ED_F2$E, ED_F2$U, use = 'complete.obs')	# E vs. U

## Construct models for plotting:

summary(OvsU.mdl <- lm(U ~ O, ED_F1))
summary(EvsU.mdl <- lm(U ~ E, ED_F2))

## Data for ploting:

xvals <- seq(1200, 1900, 0.01)
newdata <- data.frame(E = xvals)

## Get fit:

newdata <- cbind(newdata,
		as.data.frame(predict(EvsU.mdl, newdata = newdata, se.fit = T)[1:2]))

## Upper and lower confidence intervals:

newdata$UB <- newdata$fit + 1.96 * newdata$se.fit
newdata$LB <- newdata$fit - 1.96 * newdata$se.fit

## Make a plot out of this:

quartz('', 8, 6)
par(mai = c(1.5, 1.5, 0.5, 0.5))
plot(ED_F2$E, ED_F2$U, type = 'n',
	xlim = c(1800, 1400), ylim = c(1800, 1300),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
axis(side = 1, at = seq(1800, 1400, -100), lwd.ticks = 2, font = 2, cex.axis = 1.5)
axis(side = 2, at = seq(1800, 1300, -100), lwd.ticks = 2, font = 2, cex.axis = 1.5, las = 2)
mtext(side = 1, 'F2 Threshold of /ɛ/', font = 2, line = 3.8, cex = 1.75)
mtext(side = 2, 'F2 Threshold of /ʌ/', font = 2, line = 4.75, cex = 1.75)
polygon(x = c(xvals, rev(xvals)), y = c(newdata$UB, rev(newdata$LB)), col = rgb(0, 0, 0, 0.25), border = NA)
abline(EvsU.mdl, lwd = 2)
set.seed(2)
points(jitter(ED_F2$E, factor = 0.65), jitter(ED_F2$U, factor = 0.65), pch = 19, cex = 1.15)
box(lwd = 2)





