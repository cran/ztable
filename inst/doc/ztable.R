## ----, eval=FALSE--------------------------------------------------------
#  options(ztable.type="html")

## ----,results="asis",message=FALSE---------------------------------------
require(ztable)
options(ztable.type="html")
options(ztable.zebra=1)
options(ztable.zebra.color="platinum")
options(ztable.colnames.bold=TRUE)
ztable(head(mtcars))

## ----,results='asis'-----------------------------------------------------
ztable(head(mtcars),zebra=NULL,size=3,
       caption="Table 1. Non-zebra Table with small size")

## ----,results='asis'-----------------------------------------------------
ztable(head(mtcars[c(1:7)]),zebra=2,zebra.color="lightcyan",size=7,
       caption="Table 2. Left-sided caption at botom with large font",
       caption.placement="bottom",caption.position="l") 

## ----,results="asis"-----------------------------------------------------
out <- aov(mpg ~ ., data=mtcars)
ztable(out)

## ----,results='asis'-----------------------------------------------------
fit <- lm(mpg ~ cyl + disp + wt + drat + am, data=mtcars)
ztable(fit)

## ----,results='asis'-----------------------------------------------------
a=anova(fit)
str(a)
ztable(a)

## ----,results='asis'-----------------------------------------------------
fit2 <- lm(mpg ~ cyl+wt, data=mtcars)
b=anova(fit2,fit)
str(b)
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
ztable(anova(lm.D9),align="|c|rrrr|r|")

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
ztable(head(mtcars,15),zebra=0,zebra.color=NULL) 

## ----,results='asis'-----------------------------------------------------
z=ztable(head(mtcars[1:3]),tabular=TRUE,zebra.color="peach-orange")
z1=ztable(head(iris[1:3]),tabular=TRUE,zebra=2)

parallelTables(width=c(0.5,0.5),list(z,z1),type="html")
parallelTables(width=c(0.5,0.5),list(z,"figures/ztable3.png"),type="html")

## ----,results='asis'-----------------------------------------------------
z1=ztable(head(iris),zebra=2)
z1
print(z1,zebra.type=2)
print(z1,zebra=1,zebra.type=2,zebra.colnames=TRUE)

## ----,results='asis'-----------------------------------------------------
options(ztable.zebra.color=NULL)
(z1=ztable(head(iris),zebra=0,zebra.type=2))

## ----,results='asis'-----------------------------------------------------
update_ztable(z1,colnames.bold=TRUE,zebra.colnames=TRUE)

## ----,results='asis'-----------------------------------------------------
print(z1,zebra.color=c(rep("white",5),"peach"),zebra.colnames=TRUE)

## ----,results='asis'-----------------------------------------------------
zlist=list(y=c(3,NA,3,6),x=c(NA,4,4,6),color=c("platinum","peach","darkchampagne","scarlet"))
z1=update_ztable(z1,zebra.list=zlist,zebra.type=3)
print(z1)

## ----,results='asis'-----------------------------------------------------
ztable(head(iris),zebra=0,zebra.type=0)
ztable(head(iris),zebra=0,zebra.type=0,zebra.color=zcolors$name,zebra.colnames=TRUE)

## ----,results='asis'-----------------------------------------------------
ztable(head(iris),zebra=0,zebra.type=0,zebra.color=1:7,zebra.colnames=TRUE)
ztable(head(mtcars[,1:9]),zebra=0,zebra.type=0,zebra.color=1:9,zebra.colnames=TRUE)

## ----,results='asis'-----------------------------------------------------
mycolor=rep("white",6)
for(i in 1:40){
    mycolor=c(mycolor,"white",zcolors$name[((i-1)*5+1):((i-1)*5+5)])
}
a=c(zcolors$name[1:5])
for(i in 2:40){
    a=rbind(a,zcolors$name[((i-1)*5+1):((i-1)*5+5)])
}
a=data.frame(a,stringsAsFactors=FALSE,row.names=NULL)
ztable(a,zebra=0,zebra.type=0,zebra.color=mycolor,include.rownames=FALSE,
       include.colnames=FALSE,longtable=TRUE)

