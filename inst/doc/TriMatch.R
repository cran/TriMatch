## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  error = FALSE
)
options(digits = 2)

## ----setup--------------------------------------------------------------------
library(TriMatch)
data("tutoring")
data("nmes")

## -----------------------------------------------------------------------------
names(tutoring)

## -----------------------------------------------------------------------------
table(tutoring$treat, tutoring$Course, useNA="ifany")

## -----------------------------------------------------------------------------
formu <- ~ Gender + Ethnicity + Military + ESL + EdMother + EdFather + 
Age + Employment + Income + Transfer + GPA
tutoring.tpsa <- trips(tutoring, tutoring$treat, formu)

## -----------------------------------------------------------------------------
plot(tutoring.tpsa)

## -----------------------------------------------------------------------------
tutoring.matched <- trimatch(tutoring.tpsa, exact=tutoring[,c("Course")])

## -----------------------------------------------------------------------------
tutoring.matched.caliper <- trimatch(tutoring.tpsa, 
exact=tutoring[,c("Course")], method=NULL)

## -----------------------------------------------------------------------------
tutoring.matched.2to1 <- trimatch(tutoring.tpsa, 
exact=tutoring[,c("Course")], method=OneToN, M1=2, M2=1)
tutoring.matched.3to2 <- trimatch(tutoring.tpsa, 
exact=tutoring[,c("Course")], 
method=OneToN, M1=3, M2=2)

## ----triangleplot, fig.cap = 'Traingle Plot'----------------------------------
print(plot(tutoring.matched, rows=c(50), line.alpha=1, draw.segments=TRUE))

## -----------------------------------------------------------------------------
summary(unmatched(tutoring.matched))

## -----------------------------------------------------------------------------
summary(unmatched(tutoring.matched.caliper))

## -----------------------------------------------------------------------------
summary(unmatched(tutoring.matched.2to1))

## -----------------------------------------------------------------------------
summary(unmatched(tutoring.matched.3to2))

## ----multibalance, fig.cap = 'Multiple Covariate Balance Plot of Absolute Standardized Effect Sizes Before and After Propensity Score Adjustment'----
print(multibalance.plot(tutoring.tpsa) + ggtitle("Covariate Balance Plot"))

## ----balance, fig.cap = 'Covariate Balance Plots', cache = TRUE---------------
bplots <- balance.plot(tutoring.matched, tutoring[,all.vars(formu)], 
		legend.position="none", x.axis.labels=c("C","T1","T1"), x.axis.angle=0)
print(plot(bplots, cols=3, byrow=FALSE))

## -----------------------------------------------------------------------------
matched.out <- merge(tutoring.matched, tutoring$Grade)
names(matched.out)
head(matched.out)

## -----------------------------------------------------------------------------
s1 <- summary(tutoring.matched, tutoring$Grade)
names(s1)
s1$friedman.test
s1$t.tests

## -----------------------------------------------------------------------------
s2 <- summary(tutoring.matched.caliper, tutoring$Grade)
s3 <- summary(tutoring.matched.2to1, tutoring$Grade)
s4 <- summary(tutoring.matched.3to2, tutoring$Grade)

print("Max Treat"=s1, "Caliper"=s2, "2-to-1"=s3, "3-to-2"=s4)

## ----boxplots, fig.cap = 'Boxplot of Differences', fig.show = 'hold', out.width = '30%'----
boxdiff.plot(tutoring.matched, tutoring$Grade, 
			 ordering=c("Treat2","Treat1","Control")) + 
	ggtitle("Maximum Treatment Matching")
boxdiff.plot(tutoring.matched.caliper, tutoring$Grade, 
			 ordering=c("Treat2","Treat1","Control")) +
	ggtitle("Caliper Matching")
boxdiff.plot(tutoring.matched.2to1, tutoring$Grade, 
			 ordering=c("Treat2","Treat1","Control")) +
	ggtitle("2-to-1-to-n Matching")

## ----loess, fig.cap = 'Loess Plot for Caliper Matching'-----------------------
loess3.plot(tutoring.matched.caliper, tutoring$Grade, ylab="Grade", 
			points.alpha=.1, method="loess")

## -----------------------------------------------------------------------------
data(nmes)
nmes <- subset(nmes, select = c(packyears, smoke, LASTAGE, MALE, RACE3, beltuse, educate, marital, SREGION, POVSTALB, HSQACCWT, TOTALEXP))

## -----------------------------------------------------------------------------
nmes <- na.omit(nmes)

## -----------------------------------------------------------------------------
nmes$smoke <- factor(nmes$smoke, levels=c(0,1,2), labels=c("Never","Smoker","Former"))
nmes$LogTotalExp <- log(nmes$TOTALEXP + 1)
(medPY <- median(nmes[nmes$smoke != "Never",]$packyears))
table(nmes$smoke, nmes$packyears > medPY)
nmes$smoke2 <- ifelse(nmes$smoke == "Never", "Never", 
ifelse(nmes$packyears > 17, "Heavy", "Moderate"))
table(nmes$smoke, nmes$smoke2, useNA="ifany")

## ----packyearsAndTotalExp, fig.cap = 'Relationship Between Pack Year and Total Expenditures by Current Smoking Status', fig.show = 'hold', out.width = '50%'----
ggplot(nmes[nmes$smoke != "Never",], aes(x=log(packyears+1), color=smoke, fill=smoke)) +
	  	geom_density(alpha=.1) + 
	  	theme(legend.position="none", plot.margin=rep(unit(0, "cm"), 4)) +
	  	xlab("") + ylab("Density")
ggplot(nmes[nmes$smoke != "Never",], aes(x=log(packyears+1), y=LogTotalExp, color=smoke, fill=smoke)) + 
	  	geom_point(alpha=.2) + 
	  	geom_smooth(method="loess", formula = y ~ x) +
	  	scale_color_hue("") + scale_fill_hue("") +
	  	theme(legend.position=c(.9,1), plot.margin=rep(unit(0, "cm"), 4)) + 
	  	xlab("log(Pack Year)") + ylab("log(Total Expenditures)")

## -----------------------------------------------------------------------------
nmes$LastAge5 <- cut(nmes$LASTAGE, 
breaks=quantile(nmes$LASTAGE, probs=seq(0,1,1/5)),
include.lowest=TRUE, orderd_result=TRUE)

## -----------------------------------------------------------------------------
formu <- ~ LASTAGE + MALE + RACE3 + beltuse + educate + marital + 
SREGION + POVSTALB

## -----------------------------------------------------------------------------
tpsa.smoke <- trips(nmes, nmes$smoke, formu)
tpsa.packyears <- trips(nmes, nmes$smoke2, formu)

## ----nmestriangleplot, fig.cap = 'Triangle Plots for NMES', fig.show = 'hold', out.width = '50%'----
p.smoke <- plot(tpsa.smoke, sample=c(.05), edge.alpha=.1) + ggtitle("Treatment Variable: Current Smoking Status")
p.packyears <- plot(tpsa.packyears, sample=c(.05), edge.alpha=.1) + ggtitle("Treatment Variable: Lifetime Smoking Frequency")
p.smoke
p.packyears

## ----cache = TRUE-------------------------------------------------------------
tmatch.smoke <- trimatch(tpsa.smoke, 
exact=nmes[,c("LastAge5","MALE","RACE3")])
tmatch.packyears <- trimatch(tpsa.packyears, 
exact=nmes[,c("LastAge5","MALE","RACE3")])

## -----------------------------------------------------------------------------
summary(unmatched(tmatch.smoke))
summary(unmatched(tmatch.packyears))

## ----nmesbalance, fig.cap = 'Multiple Covariate Balance Plots for NMES', fig.show = 'hold', out.width = '50%'----
p.smoke <- multibalance.plot(tpsa.smoke) + ggtitle("Treatment Variable: Current Smoking Status")
p.packyears <- multibalance.plot(tpsa.packyears) + ggtitle("Treatment Variable: Lifetime Smoking Frequency")
p.smoke
p.packyears

## ----nmesboxplots, fig.cap = 'Boxplot of Differences for NMES', fig.show = 'hold', out.width = '50%'----
boxdiff.plot(tmatch.smoke, nmes$LogTotalExp, ordering=c("Smoker","Former","Never")) + 
	ggtitle("Treatment Variable: Current Smoking Status")
boxdiff.plot(tmatch.packyears, nmes$LogTotalExp, ordering=c("Heavy","Moderate","Never")) +
	ggtitle("Treatment Variable: Lifetime Smoking Frequency")

## -----------------------------------------------------------------------------
sum.smoke <- summary(tmatch.smoke, nmes$LogTotalExp, 
ordering=c("Smoker","Former","Never"))
sum.packyears <- summary(tmatch.packyears, nmes$LogTotalExp, 
ordering=c("Heavy","Moderate","Never"))
print("Current Smoking Status" = sum.smoke, "Smoking Frequency" = sum.packyears)

## -----------------------------------------------------------------------------
sum.smoke$t.tests
sum.packyears$t.test

