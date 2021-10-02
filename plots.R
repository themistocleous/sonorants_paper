#Plots
# Charalambos Themistocleous 2016
# Study of sonorants spectral moments
# xtable defaults
rm(list=ls(all=TRUE))
library("lme4")
library("lmerTest")
library("sjPlot")
library("xtable")
library("ggplot2")
library("RColorBrewer")
library("gridExtra")
library("grid")
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
source("Definitions.r")
# options defaults
options(digits=2)
options(scipen=999)

# Load data
sm <- read.csv("spectralmoments.csv")
sm_stressed = sm[sm$Stress=="Stressed",]
sm_unstressed = sm[sm$Stress=="Unstressed",]

# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
cog_stressed <- summarySE(sm_stressed, measurevar="cog", groupvars=c("Variety","Segment"))
cog_unstressed <- summarySE(sm_unstressed, measurevar="cog", groupvars=c("Variety","Segment"))
sd_stressed <- summarySE(sm_stressed, measurevar="sdev", groupvars=c("Variety","Segment"))
sd_unstressed <- summarySE(sm_unstressed, measurevar="sdev", groupvars=c("Variety","Segment"))
skew_stressed <- summarySE(sm_stressed, measurevar="skew", groupvars=c("Variety","Segment"))
skew_unstressed <- summarySE(sm_unstressed, measurevar="skew", groupvars=c("Variety","Segment"))
kurt_stressed <- summarySE(sm_stressed, measurevar="kurt", groupvars=c("Variety","Segment"))
kurt_unstressed <- summarySE(sm_unstressed, measurevar="kurt", groupvars=c("Variety","Segment"))
dur_stressed <- summarySE(sm_stressed, measurevar="duration", groupvars=c("Variety","Segment"))
dur_unstressed <- summarySE(sm_unstressed, measurevar="duration", groupvars=c("Variety","Segment"))
int_stressed <- summarySE(sm_stressed, measurevar="intensity", groupvars=c("Variety","Segment"))
int_unstressed <- summarySE(sm_unstressed, measurevar="intensity", groupvars=c("Variety","Segment"))



# LINE GRAPHS 
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

# Black error bars - notice the mapping of 'group=supp' -- without it, the error
# bars won't be dodged!
cog_s = ggplot(cog_stressed, aes(x=Segment, y=cog, colour=Variety, group=Variety)) + 
  geom_errorbar(aes(ymin=cog-ci, ymax=cog+ci), colour="black", width=.2, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3) +
  scale_y_continuous(name = "Center of Gravity (Hz)",
                     breaks = seq(600, 1500, 200),
                     limits=c(600, 1500)) +
#  ggtitle("Center of Gravity") +
  xlab("Segment - Stressed") +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=22), plot.title = element_text(hjust = 0.5))
cog_u= ggplot(cog_unstressed, aes(x=Segment, y=cog, colour=Variety, group=Variety)) + 
  geom_errorbar(aes(ymin=cog-ci, ymax=cog+ci), colour="black", width=.2, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3) +
  scale_y_continuous(name = "Center of Gravity (Hz)",
                     breaks = seq(600, 1500, 200),
                     limits=c(600, 1500)) +
#  ggtitle("Center of Gravity") +
  xlab("Segment - Unstressed") +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=22), plot.title = element_text(hjust = 0.5))
cogplot = grid.arrange(cog_s,cog_u, nrow = 1, ncol=2,
                       top=textGrob("Center of Gravity", gp=gpar(fontsize=22,font=8)))
cogplot
ggsave("cogplot.pdf", plot=cogplot, width = 10, height = 5)

sd_s = ggplot(sd_stressed, aes(x=Segment, y=sdev, colour=Variety, group=Variety)) + 
  geom_errorbar(aes(ymin=sdev-ci, ymax=sdev+ci), colour="black", width=.2, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3) +
  scale_y_continuous(name = "Spectral SD (Hz)",
                     breaks = seq(600, 1800, 200),
                     limits=c(600, 1800)) +
#  ggtitle("Standard Deviation") +
  xlab("Segment - Stressed") +
  ylab("Spectral SD (Hz)") +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=22), plot.title = element_text(hjust = 0.5))
sd_u = ggplot(sd_unstressed, aes(x=Segment, y=sdev, colour=Variety, group=Variety)) + 
  geom_errorbar(aes(ymin=sdev-ci, ymax=sdev+ci), colour="black", width=.2, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3) +
  scale_y_continuous(name = "Spectral SD (Hz)",
                     breaks = seq(600, 1800, 200),
                     limits=c(600, 1800)) +
#  ggtitle("Standard Deviation") +
  xlab("Segment - Unstressed") +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=22), plot.title = element_text(hjust = 0.5))

sdplot = grid.arrange(sd_s,sd_u, nrow = 1, ncol=2,
                       top=textGrob("Spectral Standard Deviation", gp=gpar(fontsize=22,font=8)))
sdplot
ggsave("SD.pdf", plot=sdplot, width = 10, height = 5)


skew_s = ggplot(skew_stressed, aes(x=Segment, y=skew, colour=Variety, group=Variety)) + 
  geom_errorbar(aes(ymin=skew-ci, ymax=skew+ci), colour="black", width=.2, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3) +
  scale_y_continuous(name = "Spectral Skewness",
                     breaks = seq(5, 18, 2.5),
                     limits=c(5, 18)) +
#  ggtitle("Skewness") +
  xlab("Segment - Stressed") +
  ylab("Spectral Skewness") +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=22), plot.title = element_text(hjust = 0.5))
skew_u = ggplot(skew_unstressed, aes(x=Segment, y=skew, colour=Variety, group=Variety)) + 
  geom_errorbar(aes(ymin=skew-ci, ymax=skew+ci), colour="black", width=.2, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3) +
  scale_y_continuous(name = "Spectral Skewness",
                     breaks = seq(5, 18, 2.5),
                     limits=c(5, 18)) +
#  ggtitle("Skewness") +
  xlab("Segment - Unstressed") +
  ylab("Spectral Skewness") +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=22), plot.title = element_text(hjust = 0.5))

skewplot = grid.arrange(skew_s,skew_u, nrow = 1, ncol=2,
                      top=textGrob("Spectral Skewness", gp=gpar(fontsize=22,font=8)))
skewplot
ggsave("skewplot.pdf", plot=skewplot, width = 10, height = 5)


kurt_s = ggplot(kurt_stressed, aes(x=Segment, y=kurt, colour=Variety, group=Variety)) + 
  geom_errorbar(aes(ymin=kurt-ci, ymax=kurt+ci), colour="black", width=.2, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3) +
  scale_y_continuous(name = "Spectral Kurtosis",
                     breaks = seq(100, 900, 200),
                     limits=c(100, 900)) +
#  ggtitle("Kurtosis") +
  xlab("Segment - Stressed") +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=22), plot.title = element_text(hjust = 0.5))
kurt_u = ggplot(kurt_unstressed, aes(x=Segment, y=kurt, colour=Variety, group=Variety)) + 
  geom_errorbar(aes(ymin=kurt-ci, ymax=kurt+ci), colour="black", width=.2, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3) +
  scale_y_continuous(name = "Spectral Kurtosis",
                     breaks = seq(100, 900, 200),
                     limits=c(100, 900)) +
#  ggtitle("Kurtosis") +
  xlab("Segment - Unstressed") +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=22), plot.title = element_text(hjust = 0.5))

kurtplot = grid.arrange(kurt_s,kurt_u, nrow = 1, ncol=2,
                        top=textGrob("Spectral Kurtosis", gp=gpar(fontsize=22,font=8)))
kurtplot
ggsave("kurtplot.pdf", plot=kurtplot, width = 10, height = 5)


dur_s = ggplot(dur_stressed, aes(x=Segment, y=duration, colour=Variety, group=Variety)) + 
  geom_errorbar(aes(ymin=duration-ci, ymax=duration+ci), colour="black", width=.2, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3) +
  scale_y_continuous(name = "Duration (ms)",
                     breaks = seq(20, 90, 10),
                     limits=c(20, 90)) +
  #ggtitle("Duration") +
  xlab("Segment - Stressed") +
  ylab("Duration (ms)") +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=22), plot.title = element_text(hjust = 0.5))

dur_u = ggplot(dur_unstressed, aes(x=Segment, y=duration, colour=Variety, group=Variety)) + 
  geom_errorbar(aes(ymin=duration-ci, ymax=duration+ci), colour="black", width=.2, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3) +
  scale_y_continuous(name = "Duration (ms)",
                     breaks = seq(20, 90, 10),
                     limits=c(20, 90)) +
  #ggtitle("Duration") +
  xlab("Segment - Unstressed") +
  ylab("Duration (ms)") +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=22), plot.title = element_text(hjust = 0.5))
durplot = grid.arrange(dur_s,dur_u, nrow = 1, ncol=2,
                        top=textGrob("Duration", gp=gpar(fontsize=22,font=8)))

ggsave("durplot.pdf", plot=durplot, width = 10, height = 5)



int_s = ggplot(int_stressed, aes(x=Segment, y=intensity, colour=Variety, group=Variety)) + 
  geom_errorbar(aes(ymin=intensity-ci, ymax=intensity+ci), colour="black", width=.2, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3) +
  scale_y_continuous(name = "intensity (ms)",
                     breaks = seq(0, 160, 10),
                     limits=c(0, 160)) +
  #ggtitle("intensity") +
  xlab("Segment - Stressed") +
  ylab("intensity (ms)") +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=22), plot.title = element_text(hjust = 0.5))
int_u = ggplot(int_unstressed, aes(x=Segment, y=intensity, colour=Variety, group=Variety)) + 
  geom_errorbar(aes(ymin=intensity-ci, ymax=intensity+ci), colour="black", width=.2, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3) +
  scale_y_continuous(name = "intensity (ms)",
                     breaks = seq(0, 160, 10),
                     limits=c(0, 160)) +
  #ggtitle("intensity") +
  xlab("Segment - Unstressed") +
  ylab("intensity (ms)") +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=22), plot.title = element_text(hjust = 0.5))


spplot = grid.arrange(cog_s,cog_u,sd_s,sd_u,skew_s,skew_u,kurt_s,kurt_u, nrow = 4, ncol=2,
                       top=textGrob("Spectral Moments", gp=gpar(fontsize=22,font=8)))

spplot  
ggsave("durplot.pdf", plot=durplot, width = 10, height = 5)
durplot = grid.arrange(dur_s,dur_u, nrow = 1, ncol=2,
                      top=textGrob("Duration", gp=gpar(fontsize=22,font=8)))

durplot  
 

