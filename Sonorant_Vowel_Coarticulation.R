# Sonorant-Vowel Coarticulation
rm(list=ls(all=TRUE))
source("Definitions.r")
library("lme4")
library("lmerTest")
library("xtable")
library(pracma)
library(pdist)
library(car)
library(lme4)
library(lmerTest)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# options defaults
options(digits=2)
options(scipen=999)

#fm <- subset(fm, Position == "Initial")
# Set Graphics
par()              # view current settings
opar <- par()      # make a copy of current settings
lwd = 2
cex = 1.3




fm  <- read.csv("formants.csv", header = T)


names(fm)[names(fm)=="F1.5"] <- "F1.05"
names(fm)[names(fm)=="F2.5"] <- "F2.05"
names(fm)[names(fm)=="F3.5"] <- "F3.05"
names(fm)[names(fm)=="F4.5"] <- "F4.05"
names(fm)[names(fm)=="F5.5"] <- "F5.05"

fm <- fm[c("F1.05", "F1.10", "F1.15", "F1.20", "F1.25", "F1.30", "F1.35", "F1.40", "F1.45", "F1.50", "F1.55", "F1.60", "F1.65", "F1.70", "F1.75", "F1.80", "F1.85", "F1.90", "F1.95", 
           "F2.05", "F2.10", "F2.15", "F2.20", "F2.25", "F2.30", "F2.35", "F2.40", "F2.45", "F2.50", "F2.55", "F2.60", "F2.65", "F2.70", "F2.75", "F2.80", "F2.85", "F2.90", "F2.95", 
           "F3.05", "F3.10", "F3.15", "F3.20", "F3.25", "F3.30", "F3.35", "F3.40", "F3.45", "F3.50", "F3.55", "F3.60", "F3.65", "F3.70", "F3.75", "F3.80", "F3.85", "F3.90", "F3.95", 
           "F4.05", "F4.10", "F4.15", "F4.20", "F4.25", "F4.30", "F4.35", "F4.40", "F4.45", "F4.50", "F4.55", "F4.60", "F4.65", "F4.70", "F4.75", "F4.80", "F4.85", "F4.90", "F4.95", 
           "F5.05", "F5.10", "F5.15", "F5.20", "F5.25", "F5.30", "F5.35", "F5.40", "F5.45", "F5.50", "F5.55", "F5.60", "F5.65", "F5.70", "F5.75", "F5.80", "F5.85", "F5.90", "F5.95", 
           "X2", "aa", "file.1", "label.1", "duration", "f0_min", "f0_mean", "f0_max", "int_min", "int_mean", "int_max", "F1", 
           "F2", "F3", "Keyword", "Speaker", "Variety", "Segment", "Selection","Position", "Stress", "X", "AA", "file", "label", "Duration")] 

#write.csv(fm, "fm.csv")


Vowel <- as.character(fm$Keyword)
Vowel[grep('[i]', Vowel)] <- "i";
Vowel[grep('[^i]', Vowel)] <- "a";
fm$Vowel <- factor(Vowel);


aggregate(F1.05 ~ Vowel + Keyword, data=fm, mean)
aggregate(F1.05 ~ Segment + Vowel + Stress + Variety, data=fm, mean)




descriptives <- function(df)
{
  sapply(df, function(cl) list(means = mean(cl, na.rm = TRUE), 
                               sds = sd(cl, na.rm = TRUE)))
}
calculatepoly <- function(data)
{
  n <- nrow(data)
  df <- data.frame(x = numeric(n), y = numeric(n), z = numeric(n), 
                   stringsAsFactors = FALSE)
  for (i in 1:n)
  {
    df[i, ] <- polyfit(1:19, as.numeric(data[i, ]), 2)
  }
  df
}

evaluatepoly <- function(data)
{
  n <- nrow(data)
  df <- data.frame(a = numeric(n), b = numeric(n), c = numeric(n), 
                   d = numeric(n), e = numeric(n), f = numeric(n), g = numeric(n), 
                   h = numeric(n), i = numeric(n), j = numeric(n), k = numeric(n), 
                   l = numeric(n), stringsAsFactors = FALSE)
  for (i in 1:nrow(data))
  {
    
    df[i, ] <- polyval(as.numeric(data[i, ]), seq(0.2, 0.8, 
                                                  0.05))
  }
  df
}

dist <- function(x, y)
{
  n <- nrow(x)
  df <- data.frame(distance = numeric(n), stringsAsFactors = FALSE)
  for (i in 1:nrow(x))
  {
    
    df[i, ] <- pdist2(polyval(as.numeric(x[i, ]), seq(0.2, 
                                                      0.8, 0.05)), polyval(as.numeric(y[i, ]), seq(0.2, 
                                                                                                   0.8, 0.05)))
  }
  df
}

dur <- function(x)
{
  n <- length(x)
  df <- data.frame(a = numeric(n), b = numeric(n), c = numeric(n), 
                   d = numeric(n), e = numeric(n), f = numeric(n), g = numeric(n), 
                   h = numeric(n), i = numeric(n), j = numeric(n), k = numeric(n), 
                   l = numeric(n), m = numeric(n), stringsAsFactors = FALSE)
  for (i in 1:length(x))
  {
    
    df[i, ] <- as.numeric(x[i]) * seq(0.2, 0.8, 0.05)
  }
  df
}



fm <- subset(fm, Position == "Initial")
fm <- subset(fm, Stress == "Unstressed")
fm <- droplevels(fm)


all.f1  <- fm[c(grep("F1.05", colnames(fm), fixed = T):grep("F1.95", colnames(fm), fixed = T),
               grep("file", colnames(fm)),
               grep("Variety", colnames(fm)),
               grep("Keyword", colnames(fm)),
               grep("Speaker", colnames(fm)),
               grep("Stress", colnames(fm)),
               grep("Segment", colnames(fm)),
               grep("Vowel", colnames(fm)),
               grep("Position", colnames(fm)),
               grep("Keyword", colnames(fm))
)]


all.f1  <- cbind(all.f1, as.data.frame(calculatepoly(all.f1[1:19])))



all.f2 <- fm[c(grep("F2.05", colnames(fm), fixed = T):grep("F2.95", colnames(fm), fixed = T),
               grep("file", colnames(fm)),
               grep("Variety", colnames(fm)),
               grep("Keyword", colnames(fm)),
               grep("Speaker", colnames(fm)),
               grep("Stress", colnames(fm)),
               grep("Segment", colnames(fm)),
               grep("Vowel", colnames(fm)),
               grep("Position", colnames(fm)),
               grep("Keyword", colnames(fm))
               
)]
all.f2 <- cbind(all.f2, as.data.frame(calculatepoly(all.f2[1:19])))

all.f3 <- fm[c(grep("F3.05", colnames(fm), fixed = T):grep("F3.95", colnames(fm), fixed = T),
               grep("file", colnames(fm)),
               grep("Variety", colnames(fm)),
               grep("Keyword", colnames(fm)),
               grep("Speaker", colnames(fm)),
               grep("Stress", colnames(fm)),
               grep("Segment", colnames(fm)),
               grep("Vowel", colnames(fm)),
               grep("Position", colnames(fm)),
               grep("Keyword", colnames(fm))
)]
all.f3 <- cbind(all.f3, as.data.frame(calculatepoly(all.f3[1:19])))



all.f4 <- fm[c(grep("F4.05", colnames(fm), fixed = T):grep("F4.95", colnames(fm), fixed = T),
               grep("file", colnames(fm)),
               grep("Variety", colnames(fm)),
               grep("Keyword", colnames(fm)),
               grep("Speaker", colnames(fm)),
               grep("Stress", colnames(fm)),
               grep("Segment", colnames(fm)),
               grep("Vowel", colnames(fm)),
               grep("Position", colnames(fm)),
               grep("Keyword", colnames(fm))
)]               
all.f4 <- cbind(all.f4, as.data.frame(calculatepoly(all.f4[1:19])))




all.f5 <- fm[c(grep("F5.05", colnames(fm), fixed = T):grep("F5.95", colnames(fm), fixed = T),
               grep("file", colnames(fm)),
               grep("Variety", colnames(fm)),
               grep("Keyword", colnames(fm)),
               grep("Speaker", colnames(fm)),
               grep("Stress", colnames(fm)),
               grep("Segment", colnames(fm)),
               grep("Vowel", colnames(fm)),
               grep("Position", colnames(fm)),
               grep("Keyword", colnames(fm))
)]   
all.f5 <- cbind(all.f5, as.data.frame(calculatepoly(all.f5[1:19])))

polyformants <- cbind(all.f1, all.f2,all.f3, all.f4,all.f5)
#write.csv(polyformants, "polyformants.csv")


#sink("results.txt", append=FALSE)
options(width=1000)
f1.z <- lmer(z ~  Segment * Variety * Vowel + (1|Speaker) + (1|Keyword), data=all.f1)
summary(f1.z)
f1.y <- lmer(y ~  Segment * Variety * Vowel + (1|Speaker) + (1|Keyword), data=all.f1 )
summary(f1.y)
f1.x <- lmer(x ~  Segment * Variety * Vowel + (1|Speaker) + (1|Keyword), data=all.f1 )
summary(f1.x)

f2.z <- lmer(z ~  Segment *  Variety * Vowel + (1|Speaker) + (1|Keyword), data=all.f2)
summary(f2.z)
f2.y <- lmer(y ~  Segment * Variety * Vowel + (1|Speaker) + (1|Keyword), data=all.f2)
summary(f2.y)
f2.x <- lmer(x ~  Segment * Variety * Vowel + (1|Speaker) + (1|Keyword), data=all.f2)
summary(f2.x)

f3.z <- lmer(z ~  Segment * Variety * Vowel + (1|Speaker) + (1|Keyword), data=all.f3)
summary(f3.z)
f3.y <- lmer(y ~  Segment * Variety * Vowel + (1|Speaker) + (1|Keyword), data=all.f3)
summary(f3.y)
f3.x <- lmer(x ~  Segment * Variety * Vowel + (1|Speaker) + (1|Keyword), data=all.f3)
summary(f3.x)

f4.z <- lmer(z ~  Segment * Variety * Vowel + (1|Speaker) + (1|Keyword), data=all.f4)
summary(f4.z)
f4.y <- lmer(y ~  Segment * Variety * Vowel + (1|Speaker) + (1|Keyword), data=all.f4)
summary(f4.y)
f4.x <- lmer(x ~  Segment * Variety * Vowel + (1|Speaker) + (1|Keyword), data=all.f4)
summary(f4.x)

