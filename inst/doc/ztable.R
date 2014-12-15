## ----, eval=FALSE--------------------------------------------------------
#  options(ztable.type="html")

## ----,results="asis",message=FALSE---------------------------------------
require(ztable)
options(ztable.type="html")
options(ztable.colnames.bold=TRUE)
ztable(head(mtcars),caption="Table 1. Basic Use")

## ----,results='asis'-----------------------------------------------------
ztable(head(mtcars),zebra=NULL,size=3,
       caption="Table 2. Non-zebra Table with small size")

## ----,results='asis'-----------------------------------------------------
ztable(head(mtcars[c(1:7)]),zebra=2,zebra.color="lightcyan",size=7,
       caption="Table 3. Left-sided caption at botom with large font",
       caption.placement="bottom",caption.position="l") 

## ----,results="asis"-----------------------------------------------------
out <- aov(mpg ~ ., data=mtcars)
ztable(out)

## ----,results='asis'-----------------------------------------------------
fit <- lm(mpg ~ cyl + disp + wt + drat + am, data=mtcars)
ztable(fit)

## ----,results='asis'-----------------------------------------------------
a=anova(fit)
ztable(a)

## ----,results='asis'-----------------------------------------------------
fit2 <- lm(mpg ~ cyl+wt, data=mtcars)
b=anova(fit2,fit)
ztable(b)
ztable(b,show.heading=FALSE)

## ----,results='asis',warning=FALSE---------------------------------------
require(survival)
data(colon)
attach(colon)
out <- glm(status ~ rx+obstruct+adhere+nodes+extent, data=colon, family=binomial)
ztable(out)

## ----,results='asis'-----------------------------------------------------
ztable(anova(out))

## ----,results='asis'-----------------------------------------------------
op <- options(contrasts = c("contr.helmert", "contr.poly"))
npk.aov <- aov(yield ~ block + N*P*K, npk) 
ztable(npk.aov,zebra=1)

## ----,results='asis'-----------------------------------------------------
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
ztable(lm.D9)
ztable(anova(lm.D9))

## ----,results='asis'-----------------------------------------------------
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
d.AD <- data.frame(treatment, outcome, counts)
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
ztable(glm.D93)

## ----,results='asis',message=FALSE---------------------------------------
data(USArrests)
pr1 <- prcomp(USArrests) 
ztable(pr1)
ztable(summary(pr1))

## ----,results='asis',message=FALSE---------------------------------------
colon$TS = Surv(time,status==1) 
out=coxph(TS~rx+obstruct+adhere+differ+extent+surg+node4,data=colon)
ztable(out)

## ----,results='asis',message=FALSE---------------------------------------
ztable(head(mtcars,15),zebra=0) 
