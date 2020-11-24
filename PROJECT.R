



#DATA
fd=read.table(file.choose(),header=T,sep=",")
fd
head(fd)

#CHANGING CATEGORICAL VARIABLES
fd$Medium=ifelse(fd$grade>6 & fd$grade<=10,1,0)
fd$High=ifelse(fd$grade>10,1,0)
fd$age=2018-fd$yr_built
attach(fd)
fd3=data.frame(price,bedrooms,bathrooms,sqft_living,sqft_lot,floors,waterfront,view,condition,High,Medium,sqft_above,sqft_basement,age,yr_renovated,sqft_living15,sqft_lot15)
fd3
head(fd3)

#MODEL BUILDING
#MODEL 1
model1=(lm(price~.,data=fd3))
summary(model1)

#MODEL 2: FORWARD SELECTION
abc=step(model1,score=list(upper=FULL,lower=~1),direction="forward",trace=FALSE)
summary(abc)
model2=lm(price~.,data=fd3)
summary(model2)

#MODEL 3: BACKWARD ELIMINATION
abcde=step(model1,score=list(upper=FULL,lower=~1),direction="backward",trace=FALSE)
summary(abcde)

#MODEL 4: STEPWISE
abcdef=step(model1,score=list(upper=FULL,lower=~1),direction="both",trace=FALSE)
summary(abcdef)
abcdefi=step(lm(price~.,data=fd3))
summary(abcdefi)

#PREDICTED VALUE
attach(fd3)
#PREDICTED VALUE
fd3$predict=-3.585e+05+(-4.502e+04*bedrooms)+(bathrooms*5.465e+04)+(sqft_living*1.982e+02)+(floors*6.094e+04)+(waterfront*5.383e+05)+(view*4.933e+04)+(condition*2.270e+04)+(High*5.029e+05)+(Medium*6.061e+04)+(age*2.961e+03)+(yr_renovated*2.062e+01)+(sqft_living15*7.151e+01)+(sqft_lot15*-6.878e-01)  
head(fd3)

#MAE AND MAPE
fd3$error=(fd3$price-fd3$predict)
head(fd3)
mae=mean(abs(fd3$error))
mae

fd3$abs_err=abs(fd3$price-fd3$predict)/fd3$price
head(fd3)
mape=mean(fd3$abs_err)*100
mape

#NORMALITY OF ERROR
qqnorm(fd3$error) 
hist(fd3$error) 
shapiro.test(fd3$error) 


#DETECTING AND REMOVING OUTLIERS
fd3$res=abs(rstandard(abcdef))
fd3_new=subset(fd3,fd3$res<1.96)
head(fd3_new)

abcdefg=(lm(fd3_new$price~bedrooms+bathrooms+sqft_living+floors+waterfront+condition+High+Medium+age+yr_renovated+sqft_living15+qft_lot15, data=fd3_new)) 
summary(abcdefg) 

#INTERACTIONS
fd3_new$flr_bdr=fd3_new$price*fd3_new$High
fd3_new$hg_age=fd3_new$High*fd3_new$age
fd3_new$bth_bd_sqlvg=
f3_new$sqlot_wtrft_cndi=
fd_3new$yr_condi_med_sqlt=
fd3_new$sqlt_wtr_sqbsm_sqftab=



#MULTICOLLINEARITY
install.packages("faraway")

