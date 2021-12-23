# Study of sonorants spectral moments
library("lme4")
library("lmerTest")
library("sjPlot")
library("xtable")
library("emmeans")
library("sjmisc")
library("ggplot2")
library("RColorBrewer")
library("gridExtra")
library("grid")
theme_set(theme_sjplot())
options(xtable.timestamp = "")

# options defaults
options(digits=2)
options(scipen=999)

# Load data
sm <- read.csv("spectralmoments.csv")
sm_CG = sm[sm$Variety=="CG",]
sm_SMG = sm[sm$Variety=="AG",]
sm_stressed = sm[sm$Stress=="Stressed",]
sm_unstressed = sm[sm$Stress =="Unstressed",]
# Create the table for the article

## Summary tables
means <- aggregate(cbind(duration,intensity,cog,sdev,skew,kurt) ~ Variety + Segment + Stress, data=sm,mean)
colnames(means) <- paste("M", colnames(means), sep = ".")
sds <- aggregate(cbind(duration,intensity,cog,sdev,skew,kurt) ~ Variety + Segment + Stress, data=sm,sd)
colnames(sds) <- paste("SD", colnames(sds), sep = ".")
summarytable <- cbind(means,sds[,4:9])
xtable(summarytable[c("M.Variety", "M.Segment", "M.Stress", "M.duration", "SD.duration", "M.intensity", "SD.intensity", "M.cog", "SD.cog", "M.sdev", "SD.sdev", "M.skew", "SD.skew", "M.kurt", "SD.kurt")] )
cor.test(log(sm$duration), log(sm$cog))

# Duration - statistics (log transformed)
mdur1 <- lmer(log(duration) ~Variety * Segment * Stress + (1|keyword) + (1|Speaker), data=sm)
mdur2 <- lmer(log(duration) ~Variety * Segment + Stress + (1|keyword) + (1|Speaker), data=sm)
emmeans(mdur1, list(pairwise ~ Segment*Variety*Stress), adjust="tukey")
anova(mdur1,mdur2)
summary(mdur1)

# COG - statistics (log transformed)
mcog1 <- lmer(log(cog)~ Variety * Segment * Stress + (1|keyword) + (1|Speaker), data=sm)
mcog2 <- lmer(log(cog)~ Variety * Segment + Stress + (1|keyword) + (1|Speaker), data=sm)
emmeans(mcog1, list(pairwise ~ Segment*Variety*Stress), adjust="tukey")
anova(mcog2)
summary(mcog2)

# SD - statistics (log transformed)
msdev1 <- lmer(log(sdev)~ Variety * Segment * Stress + (1|keyword) + (1|Speaker), data=sm)
msdev2 <- lmer(log(sdev)~ Variety * Segment + Stress + (1|keyword) + (1|Speaker), data=sm)
emmeans(msdev1, list(pairwise ~ Segment*Variety*Stress), adjust="tukey")
anova(msdev1,msdev2)
summary(msdev2) 


# Skewness - statistics
mskew1 <- lmer(skew ~ Variety * Segment * Stress + (1|keyword) + (1|Speaker), data=sm)
mskew2 <- lmer(skew ~ Variety * Segment + Stress + (1|keyword) + (1|Speaker), data=sm)
anova(mskew1,mskew2)
summary(mskew1)
emmeans(mskew1, list(pairwise ~ Variety*Stress*Segment), adjust="tukey")


# Kurtosis - statistics
mkurt1 <- lmer(kurt ~ Variety * Segment * Stress + (1|keyword) + (1|Speaker), data=sm)
mkurt2 <- lmer(kurt ~ Variety * Segment + Stress + (1|keyword) + (1|Speaker), data=sm)
anova(mkurt1,mkurt2)
summary(mkurt2) 
emmeans(mkurt1, list(pairwise ~ Variety*Stress*Segment), adjust="tukey")

