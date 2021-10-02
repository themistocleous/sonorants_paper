rm(list = ls(all.names = TRUE))
# LOAD DATA
n1 <- read.csv("n1.csv", sep = "\t")
n2 <- read.csv("n2.csv", sep = "\t") 
names(n1)[names(n1)=="Filename"] <- "file"
nn <- merge(n2,n1,by = intersect(names("file"), names("label")))
#n[n=="--undefined--"] <- NA 
write.csv(nn, "n.csv")
n <- read.csv("n.csv")
# For the Calculation of the data I kept the window number to 6 and for all segmentls I kept the window size to 0.015 whereas for /r/ which has overall very small duration I kept the window size to .005
nn <- merge(n2,n1,by = intersect(names("file"), names("label")))
rm(list=ls(all=TRUE))
source("Definitions.R")
formants <- read.csv("n.csv", header = T)
sm <- read.csv("sm2.csv", sep = "\t",header = T)
op <- options(); utils::str(op) # op is a named list
formants <- formants[!formants$Selection =="NoTarget",]
droplevels(formants)
a <- unique(formants$file)
summary(formants$file == a)
b <- unique(sm$file)
summary(sm$file==b)
#find where sm != to formants and then cbind problem solved!
summary(a==b)
cbind(sm,formants)