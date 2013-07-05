### R code from vignette source 'TriMatch.Rnw'

###################################################
### code chunk number 1: setup
###################################################
require(TriMatch)
data(tutoring)
options(digits=3)
options(width=80)
options(continue="  ")


###################################################
### code chunk number 2: TriMatch.Rnw:80-81
###################################################
names(tutoring)


###################################################
### code chunk number 3: TriMatch.Rnw:86-87
###################################################
table(tutoring$treat, tutoring$Course, useNA="ifany")


###################################################
### code chunk number 4: TriMatch.Rnw:92-95
###################################################
formu <- ~ Gender + Ethnicity + Military + ESL + EdMother + EdFather + 
Age + Employment + Income + Transfer + GPA
tutoring.tpsa <- trips(tutoring, tutoring$treat, formu)


###################################################
### code chunk number 5: TriMatch.Rnw:100-101 (eval = FALSE)
###################################################
## plot(tutoring.tpsa)


###################################################
### code chunk number 6: TriMatch.Rnw:106-107
###################################################
tutoring.matched <- trimatch(tutoring.tpsa, exact=tutoring[,c("Course")]) 


###################################################
### code chunk number 7: TriMatch.Rnw:112-114
###################################################
tutoring.matched.caliper <- trimatch(tutoring.tpsa, 
exact=tutoring[,c("Course")], method=NULL)


###################################################
### code chunk number 8: TriMatch.Rnw:119-124
###################################################
tutoring.matched.2to1 <- trimatch(tutoring.tpsa, 
exact=tutoring[,c("Course")], method=OneToN, M1=2, M2=1)
tutoring.matched.3to2 <- trimatch(tutoring.tpsa, 
exact=tutoring[,c("Course")], 
method=OneToN, M1=3, M2=2)


###################################################
### code chunk number 9: triangleplot
###################################################
print(plot(tutoring.matched, rows=c(50), line.alpha=1, draw.segments=TRUE))


###################################################
### code chunk number 10: TriMatch.Rnw:144-145
###################################################
summary(unmatched(tutoring.matched))


###################################################
### code chunk number 11: TriMatch.Rnw:148-149
###################################################
summary(unmatched(tutoring.matched.caliper))


###################################################
### code chunk number 12: TriMatch.Rnw:152-153
###################################################
summary(unmatched(tutoring.matched.2to1))


###################################################
### code chunk number 13: TriMatch.Rnw:156-157
###################################################
summary(unmatched(tutoring.matched.3to2))


###################################################
### code chunk number 14: multibalance
###################################################
print(multibalance.plot(tutoring.tpsa) + ggtitle("Covariate Balance Plot"))


###################################################
### code chunk number 15: balancegrid
###################################################
bplots <- balance.plot(tutoring.matched, tutoring[,all.vars(formu)], 
		legend.position="none", x.axis.labels=c("C","T1","T1"), x.axis.angle=0)
print(plot(bplots, cols=3, byrow=FALSE))


###################################################
### code chunk number 16: TriMatch.Rnw:195-198
###################################################
matched.out <- merge(tutoring.matched, tutoring$Grade)
names(matched.out)
head(matched.out)


###################################################
### code chunk number 17: TriMatch.Rnw:203-207
###################################################
s1 <- summary(tutoring.matched, tutoring$Grade)
names(s1)
s1$friedman.test
s1$t.tests


###################################################
### code chunk number 18: TriMatch.Rnw:212-217
###################################################
s2 <- summary(tutoring.matched.caliper, tutoring$Grade)
s3 <- summary(tutoring.matched.2to1, tutoring$Grade)
s4 <- summary(tutoring.matched.3to2, tutoring$Grade)

print("Max Treat"=s1, "Caliper"=s2, "2-to-1"=s3, "3-to-2"=s4)


###################################################
### code chunk number 19: boxdiffplots
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(boxdiff.plot(tutoring.matched, tutoring$Grade, 
			 ordering=c("Treat2","Treat1","Control")) + 
	ggtitle("Maximum Treatment Matching"), vp=vplayout(1,1))
print(boxdiff.plot(tutoring.matched.caliper, tutoring$Grade, 
			 ordering=c("Treat2","Treat1","Control")) +
	ggtitle("Caliper Matching"), vp=vplayout(1,2))
print(boxdiff.plot(tutoring.matched.2to1, tutoring$Grade, 
			 ordering=c("Treat2","Treat1","Control")) +
	ggtitle("2-to-1-to-n Matching"), vp=vplayout(1,3))


###################################################
### code chunk number 20: loessplot
###################################################
print(loess3.plot(tutoring.matched.caliper, tutoring$Grade, ylab="Grade", 
			points.alpha=.1, method="loess"))


###################################################
### code chunk number 21: setupnmes
###################################################
data(nmes)
nmes <- subset(nmes, select=c(packyears, smoke, LASTAGE, MALE, 
RACE3, beltuse, educate, marital, SREGION, POVSTALB, HSQACCWT, TOTALEXP))


###################################################
### code chunk number 22: TriMatch.Rnw:272-273
###################################################
nmes <- na.omit(nmes)


###################################################
### code chunk number 23: TriMatch.Rnw:278-286
###################################################
nmes$smoke <- factor(nmes$smoke, levels=c(0,1,2), 
labels=c("Never","Smoker","Former"))
nmes$LogTotalExp <- log(nmes$TOTALEXP + 1)
(medPY <- median(nmes[nmes$smoke != "Never",]$packyears))
table(nmes$smoke, nmes$packyears > medPY)
nmes$smoke2 <- ifelse(nmes$smoke == "Never", "Never", 
ifelse(nmes$packyears > 17, "Heavy", "Moderate"))
table(nmes$smoke, nmes$smoke2, useNA="ifany")


###################################################
### code chunk number 24: packyearsAndTotalExp
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1, heights=unit(c(1,3), "null"))))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(ggplot(nmes[nmes$smoke != "Never",], 
			 aes(x=log(packyears+1), color=smoke, fill=smoke)) + 
	  	geom_density(alpha=.1) + 
	  	#ggtitle("log(Pack Year) and log(Total Expenditures)") +
	  	theme(legend.position="none", plot.margin=rep(unit(0, "cm"), 4)) +
	  	xlab("") + ylab("Density"), 
	  vp=vplayout(1,1))
print(ggplot(nmes[nmes$smoke != "Never",], 
			 aes(x=log(packyears+1), y=LogTotalExp, color=smoke, fill=smoke)) + 
	  	geom_point(alpha=.2) + 
	  	geom_smooth(method="loess") +
	  	scale_color_hue("") + scale_fill_hue("") +
	  	theme(legend.position=c(.9,1), plot.margin=rep(unit(0, "cm"), 4)) + 
	  	xlab("log(Pack Year)") + ylab("log(Total Expenditures)"),
	  vp=vplayout(2,1))



###################################################
### code chunk number 25: TriMatch.Rnw:321-324
###################################################
nmes$LastAge5 <- cut(nmes$LASTAGE, 
breaks=quantile(nmes$LASTAGE, probs=seq(0,1,1/5)),
include.lowest=TRUE, orderd_result=TRUE)


###################################################
### code chunk number 26: TriMatch.Rnw:329-331
###################################################
formu <- ~ LASTAGE + MALE + RACE3 + beltuse + educate + marital + 
SREGION + POVSTALB


###################################################
### code chunk number 27: TriMatch.Rnw:336-338
###################################################
tpsa.smoke <- trips(nmes, nmes$smoke, formu)
tpsa.packyears <- trips(nmes, nmes$smoke2, formu)


###################################################
### code chunk number 28: nmestriangleplots
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
p.smoke <- plot(tpsa.smoke, sample=c(.05), edge.alpha=.1) + ggtitle("Treatment Variable: Current Smoking Status")
p.packyears <- plot(tpsa.packyears, sample=c(.05), edge.alpha=.1) + ggtitle("Treatment Variable: Lifetime Smoking Frequency")
print(p.smoke, vp=vplayout(1,1))
print(p.packyears, vp=vplayout(1,2))


###################################################
### code chunk number 29: TriMatch.Rnw:359-363
###################################################
tmatch.smoke <- trimatch(tpsa.smoke, 
exact=nmes[,c("LastAge5","MALE","RACE3")])
tmatch.packyears <- trimatch(tpsa.packyears, 
exact=nmes[,c("LastAge5","MALE","RACE3")])


###################################################
### code chunk number 30: TriMatch.Rnw:368-370
###################################################
summary(unmatched(tmatch.smoke))
summary(unmatched(tmatch.packyears))


###################################################
### code chunk number 31: nmesbalanceplots
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
p.smoke <- multibalance.plot(tpsa.smoke) + ggtitle("Treatment Variable: Current Smoking Status")
p.packyears <- multibalance.plot(tpsa.packyears) + ggtitle("Treatment Variable: Lifetime Smoking Frequency")
print(p.smoke, vp=vplayout(1,1))
print(p.packyears, vp=vplayout(1,2))


###################################################
### code chunk number 32: nmesboxdiffplots
###################################################
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(boxdiff.plot(tmatch.smoke, nmes$LogTotalExp, ordering=c("Smoker","Former","Never")) + 
	ggtitle("Treatment Variable: Current Smoking Status"), vp=vplayout(1,1))
print(boxdiff.plot(tmatch.packyears, nmes$LogTotalExp, ordering=c("Heavy","Moderate","Never")) +
	ggtitle("Treatment Variable: Lifetime Smoking Frequency"), vp=vplayout(1,2))


###################################################
### code chunk number 33: TriMatch.Rnw:413-418
###################################################
sum.smoke <- summary(tmatch.smoke, nmes$LogTotalExp, 
ordering=c("Smoker","Former","Never"))
sum.packyears <- summary(tmatch.packyears, nmes$LogTotalExp, 
ordering=c("Heavy","Moderate","Never"))
print("Current Smoking Status"=sum.smoke, "Smoking Frequency"=sum.packyears)


###################################################
### code chunk number 34: TriMatch.Rnw:421-423
###################################################
sum.smoke$t.tests
sum.packyears$t.test


