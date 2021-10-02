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


hist(sm$duration)
hist(sm$cog)
hist(sm$kurt)
hist(sm$skew)

# Duration - statistics (log transformed)
mdur1 <- lmer(log(duration) ~Variety * Segment * Stress + (1|keyword) + (1|Speaker), data=sm)
mdur2 <- lmer(log(duration) ~Variety * Segment + Stress + (1|keyword) + (1|Speaker), data=sm)
emmeans(mdur1, list(pairwise ~ Segment*Variety*Stress), adjust="tukey")
anova(mdur1,mdur2)
summary(mdur1)
plot_model(mdur1, type = "pred", terms = c("Variety", "Segment"))


# Duration - stress and unstressed models (not log transformed)
stress_dur <- lmer(duration ~Variety * Segment + (1|keyword) + (1|Speaker), data=sm_stressed)
summary(stress_dur)
emmeans(stress_dur, list(pairwise ~ Segment*Variety), adjust="tukey")

unstress_dur <- lmer(duration ~Variety * Segment + (1|keyword) + (1|Speaker), data=sm_unstressed)
summary(unstress_dur)
emmeans(unstress_dur, list(pairwise ~ Segment*Variety), adjust="tukey")


# Intensity - statistics (log transformed)
mint1 <- lmer(log(intensity)~ Variety * Segment * Stress + (1|keyword) + (1|Speaker), data=sm)
mint2 <- lmer(log(intensity)~ Variety * Segment + Stress + (1|keyword) + (1|Speaker), data=sm)
emmeans(mint1, list(pairwise ~ Segment*Variety*Stress), adjust="tukey")
anova(mint1,mint2)
summary(mint1) 
plot_model(mdur1, type = "pred", terms = c("Variety", "Segment"))


# Intensity - stress and unstressed models (not log transformed)
stress_dur <- lmer(intensity ~Variety * Segment + (1|keyword) + (1|Speaker), data=sm_stressed)
summary(stress_dur)
emmeans(stress_dur, list(pairwise ~ Segment*Variety), adjust="tukey")
unstress_dur <- lmer(intensity ~Variety * Segment + (1|keyword) + (1|Speaker), data=sm_unstressed)
summary(unstress_dur)
emmeans(unstress_dur, list(pairwise ~ Segment*Variety), adjust="tukey")


# COG - statistics (log transformed)
mcog1 <- lmer(log(cog)~ Variety * Segment * Stress + (1|Speaker), data=sm)
emmeans(mcog1, list(pairwise ~ Segment*Variety*Stress), adjust="tukey")
anova(mcog1)


# COG - stress and unstressed models (not log transformed)
stress_cog <- lmer(cog~Variety * Segment + (1|keyword) + (1|Speaker), data=sm_stressed)
summary(stress_cog)
emmeans(stress_cog, list(pairwise ~ Segment*Variety), adjust="tukey")

unstress_cog <- lmer(cog ~Variety * Segment + (1|keyword) + (1|Speaker), data=sm_unstressed)
summary(unstress_cog)
emmeans(unstress_cog, list(pairwise ~ Segment*Variety), adjust="tukey")

plot_model(mcog2, type = "pred",  terms = c("Variety", "Segment", "Stress"))
plot_model(mcog2, type = "int")
plot_model(mcog2, sort.est = TRUE)
plot_model(mcog2, type = "std")
means <- aggregate(cbind(skew) ~  Variety + Segment + Stress, data=sm,mean)
boxplot(means$skew~means$Segment*means$Variety)

# SD - statistics (log transformed)
msdev1 <- lmer(log(sdev)~ Variety * Segment * Stress + (1|keyword) + (1|Speaker), data=sm)
msdev2 <- lmer(log(sdev)~ Variety * Segment + Stress + (1|keyword) + (1|Speaker), data=sm)
emmeans(msdev1, list(pairwise ~ Segment*Variety*Stress), adjust="tukey")
anova(msdev1,msdev2)
summary(msdev2) 
sd_pairs = emmeans(msdev1, list(pairwise ~ Variety*Stress*Segment), adjust="tukey")

sd_pairs

# SD - stress and unstressed models (not log transformed)
stress_sdev <- lmer(sdev~Variety * Segment + (1|keyword) + (1|Speaker), data=sm_stressed)
summary(stress_sdev)
emmeans(stress_sdev, list(pairwise ~ Segment*Variety), adjust="tukey")

unstress_sdev <- lmer(sdev ~Variety * Segment + (1|keyword) + (1|Speaker), data=sm_unstressed)
summary(unstress_sdev)
emmeans(unstress_sdev, list(pairwise ~ Segment*Variety), adjust="tukey")

# Skewness - statistics (log transformed)
mskew1 <- lmer(skew~ Variety * Segment * Stress + (1|keyword) + (1|Speaker), data=sm)
mskew2 <- lmer(skew~ Variety * Segment + Stress + (1|keyword) + (1|Speaker), data=sm)
anova(mskew1,mskew2)
summary(mskew1)
emmeans(mskew1, list(pairwise ~ Variety*Stress*Segment), adjust="tukey")


# Skewness - stress and unstressed models (not log transformed)
stress_skew <- lmer(skew~Variety * Segment + (1|keyword) + (1|Speaker), data=sm_stressed)
summary(stress_skew)
emmeans(stress_skew, list(pairwise ~ Segment*Variety), adjust="tukey")

unstress_skew <- lmer(skew ~Variety * Segment + (1|keyword) + (1|Speaker), data=sm_unstressed)
summary(unstress_skew)
emmeans(unstress_skew, list(pairwise ~ Segment*Variety), adjust="tukey")


# Kurtosis - statistics (log transformed)
mkurt1 <- lmer(kurt ~ Variety * Segment * Stress + (1|keyword) + (1|Speaker), data=sm)
mkurt2 <- lmer(kurt ~ Variety * Segment + Stress + (1|keyword) + (1|Speaker), data=sm)
anova(mkurt1,mkurt2)
summary(mkurt2) 
emmeans(mkurt1, list(pairwise ~ Variety*Stress*Segment), adjust="tukey")


# Kurtosis - stress and unstressed models (not log transformed)
stress_kurt <- lmer(kurt ~ Variety * Segment + (1|keyword) + (1|Speaker), data=sm_stressed)
summary(stress_kurt)
emmeans(stress_kurt, list(pairwise ~ Segment*Variety), adjust="tukey")

unstress_kurt <- lmer(kurt ~ Variety * Segment + (1|keyword) + (1|Speaker), data=sm_unstressed)
summary(unstress_kurt)
emmeans(unstress_kurt, list(pairwise ~ Segment*Variety), adjust="tukey")




###Make export size 10 x 8 in pdf
sm_stressed = sm[sm$Stress=="Stressed",]
sm_unstressed = sm[sm$Stress=="Unstressed",]
cog_stressed <- ggplot(sm_stressed, aes(x = Segment, y = cog, fill = Variety)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    scale_y_continuous(name = "CoG (Hz)",
                       breaks = seq(0, 2000, 200),
                       limits=c(0, 2000)) +
    #ggtitle("Center of Gravity ") +
    scale_x_discrete(name = "Segment - Stressed")  + 
    scale_fill_brewer(palette = "Paired") +     
    labs(fill = "Variety") + 
    theme(legend.position="top", legend.title = element_blank())
  cog_unstressed <- ggplot(sm_unstressed, aes(x = Segment, y = cog, fill = Variety)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    scale_y_continuous(name = "CoG (Hz)",
                       breaks = seq(0, 2000, 200),
                       limits=c(0, 2000)) +
    #ggtitle("Center of Gravity ") +
    scale_x_discrete(name = "Segment - Unstressed")  + 
    scale_fill_brewer(palette = "Paired") +
    labs(fill = "Variety") + 
    theme(legend.position="top", legend.title = element_blank())
  
cogplot = grid.arrange(cog_stressed,cog_unstressed,nrow = 1,
               top=textGrob("Center of Gravity", gp=gpar(fontsize=15,font=8)))
cogplot 
ggsave("Center_of_Gravity.pdf", plot=cogplot, width = 10, height = 5)


  
  
  
  
dur_stressed <- ggplot(sm_stressed, aes(x = Segment, y = duration, fill = Variety)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    scale_y_continuous(name = "Duration (ms)",
                       breaks = seq(0, 100, 10),
                       limits=c(0, 130)) +
    #ggtitle("Center of Gravity ") +
    scale_x_discrete(name = "Segment - Stressed")  + 
    scale_fill_brewer(palette = "Paired") +
    labs(fill = "Variety") + 
    theme(legend.position="top", legend.title = element_blank())
dur_unstressed <- ggplot(sm_unstressed, aes(x = Segment, y = duration, fill = Variety)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    scale_y_continuous(name = "Duration (ms)",
                       breaks = seq(0, 130, 10),
                       limits=c(0, 130)) +
    #ggtitle("Center of Gravity ") +
    scale_x_discrete(name = "Segment - Unstressed")  + 
    scale_fill_brewer(palette = "Paired") +
    labs(fill = "Variety") + 
    theme(legend.position="top", legend.title = element_blank())
  
 durplot = grid.arrange(dur_stressed,dur_unstressed,nrow = 1,
               top=textGrob("Duration", gp=gpar(fontsize=15,font=8)))
  
 durplot  
  ggsave("Duration.pdf", plot=durplot, width = 10, height = 5)
  
  
  
  boxplot(sm_unstressed$duration~sm_unstressed$Variety*sm_unstressed$Segment, outline=FALSE)
  library(gplots)
  # Plot the mean of teeth length by dose groups
  plotmeans(duration ~ c(Segment,Variety), data = sm_unstressed, frame = FALSE)
  
  
  sd_stressed <- ggplot(sm_stressed, aes(x = Segment, y = sdev, fill = Variety)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    scale_y_continuous(name = "Spectral SD (Hz)",
                       breaks = seq(0, 2000, 200),
                       limits=c(0, 2000)) +
    #ggtitle("Center of Gravity ") +
    scale_x_discrete(name = "Segment - Stressed")  + 
    scale_fill_brewer(palette = "Paired") +
    labs(fill = "Variety") + 
    theme(legend.position="top", legend.title = element_blank())
  sd_unstressed <- ggplot(sm_unstressed, aes(x = Segment, y = sdev, fill = Variety)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    scale_y_continuous(name = "Spectral SD (Hz)",
                       breaks = seq(0, 2000, 200),
                       limits=c(0, 2000)) +
    #ggtitle("Center of Gravity ") +
    scale_x_discrete(name = "Segment - Unstressed")  + 
    scale_fill_brewer(palette = "Paired") +
    labs(fill = "Variety") + 
    theme(legend.position="top", legend.title = element_blank())
  
  sdplot = grid.arrange(sd_stressed,sd_unstressed,nrow = 1,
               top=textGrob("Spectral Standard Deviation", gp=gpar(fontsize=15,font=8)))
  sdplot
  ggsave("SD.pdf", plot=sdplot, width = 10, height = 5)

  
  
  
  
  skew_stressed <- ggplot(sm_stressed, aes(x = Segment, y = skew, fill = Variety)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    scale_y_continuous(name = "Skewness",
                       breaks = seq(0, 40, 5),
                       limits=c(0, 40)) +
    #ggtitle("Center of Gravity ") +
    scale_x_discrete(name = "Segment - Stressed")  + 
    scale_fill_brewer(palette = "Paired") +
    labs(fill = "Variety") + 
    theme(legend.position="top", legend.title = element_blank())
  skew_unstressed <- ggplot(sm_unstressed, aes(x = Segment, y = skew, fill = Variety)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    scale_y_continuous(name = "Skewness",
                       breaks = seq(0, 40, 5),
                       limits=c(0, 40)) +
    #ggtitle("Center of Gravity ") +
    scale_x_discrete(name = "Segment - Unstressed")  + 
    scale_fill_brewer(palette = "Paired") +
    labs(fill = "Variety") + 
    theme(legend.position="top", legend.title = element_blank())
  skewplot = grid.arrange(skew_stressed,skew_unstressed,nrow = 1,
               top=textGrob("Skewness", gp=gpar(fontsize=15,font=8)))
  skewplot
  ggsave("Skewness.pdf", plot=skewplot, width = 10, height = 5)
  
  
  kurt_stressed <- ggplot(sm_stressed, aes(x = Segment, y = kurt, fill = Variety)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    scale_y_continuous(name = "Kurtosis",
                       breaks = seq(0, 400, 50),
                       limits=c(0, 400)) +
    #ggtitle("Center of Gravity ") +
    scale_x_discrete(name = "Segment - Stressed")  + 
    scale_fill_brewer(palette = "Paired") +
    labs(fill = "Variety") + 
    theme(legend.position="top", legend.title = element_blank())
  kurt_unstressed <- ggplot(sm_unstressed, aes(x = Segment, y = kurt, fill = Variety)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    scale_y_continuous(name = "Kurtosis",
                       breaks = seq(0, 400, 50),
                       limits=c(0, 400)) +
    #ggtitle("Center of Gravity ") +
    scale_x_discrete(name = "Segment - Unstressed")  + 
    scale_fill_brewer(palette = "Paired") +
    labs(fill = "Variety") + 
    theme(legend.position="top", legend.title = element_blank())
  
 kurtplot =  grid.arrange(kurt_stressed,kurt_unstressed,nrow = 1,
               top=textGrob("Kurtosis", gp=gpar(fontsize=15,font=8)))
 kurtplot 
  ggsave("Kurtosis.pdf", plot=kurtplot, width = 10, height = 5)
