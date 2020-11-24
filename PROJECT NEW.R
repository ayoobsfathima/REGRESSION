



#DATA
fd=read.table(file.choose(),header=T,sep=",")
fd
head(fd)

#CHANGING CATEGORICAL VARIABLES
fd$Medium=ifelse(fd$grade>6 & fd$grade<=10,1,0)
fd$High=ifelse(fd$grade>10,1,0)
fd$age=2018-fd$yr_built
attach(fd)
fd3=data.frame(price,bedrooms,bathrooms,sqft_living,sqft_lot,floors,waterfront,condition,High,Medium,sqft_above,sqft_basement,age,yr_renovated,sqft_living15,sqft_lot15)
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
fd3$predict=-3.855e+05+(-4.867e+04*bedrooms)+(bathrooms*5.650e+04)+(sqft_living*2.163e+02)+(floors*6.525e+04)+(waterfront*6.910e+05)+(condition*2.317e+04)+(High*5.150e+05)+(Medium*6.026e+04)+(age*3.118e+03)+(yr_renovated*2.336e+01)+(sqft_living15*8.362e+01)+(sqft_lot15*-6.592e-01)+(sqft_above*-1.780e+01)   
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

abcdefg=step(lm(fd3_new$price~bedrooms+bathrooms+sqft_living+floors+waterfront+condition+High+Medium+sqft_above+age+yr_renovated+sqft_living15+sqft_lot15, data=fd3_new)) 
summary(abcdefg) 

#INTERACTIONS
fd3_new$flr_bdr=fd3_new$floors*fd3_new$High
fd3_new$hg_age=fd3_new$High*fd3_new$age
fd3_new$bth_bd_sqlvg=fd3_new$bathrooms*fd3_new$bedrooms*fd3_new$sqft_living
fd3_new$sqlot_wtrft_cndi=fd3_new$sqft_lot*fd3_new$waterfront*fd3_new$condition 
fd3_new$yr_condi_med_sqlt=fd3_new$yr_renovated*fd3_new$condition*fd3_new$Medium*fd3_new$sqft_lot
head(fd3_new)

this interaction doesnt work
fd3_new$sqlt_wtr_sqbsm_sqftab=fd3_new$sqft_lot*fd3_new$waterfront*fd3_new$sqft_basement*fd3_new$sqft_above

abcdefgh=step(lm(fd3_new$price~bedrooms+bathrooms+sqft_living+floors+waterfront+condition+High+Medium+sqft_above+age+yr_renovated+sqft_living15+sqft_lot15+flr_bdr+hg_age+bth_bd_sqlvg+sqlot_wtrft_cndi+yr_condi_med_sqlt, data=fd3_new)) 
summary(abcdefgh) 

#NEW PREDICTED VALUE
attach(fd3_new)
fd3_new$predict_new=-2.738e+05+(-4.718e+04*bedrooms)+(bathrooms*2.955e+04)+(sqft_living*1.257e+02)+(floors*7.056e+04)+(waterfront*6.503e+05)+(condition*2.245e+04)+(High*-1.701e+05)+(Medium*9.463e+04)+(age*2.814e+03)+(yr_renovated*1.787e+01)+(sqft_living15*9.319e+01)+(sqft_lot15*-3.305e-01)+(flr_bdr*4.840e-01)+(bth_bd_sqlvg*1.827e+00)+(yr_condi_med_sqlt*-6.850e-05)+(hg_age*-6.666e+02)+(sqlot_wtrft_cndi*-3.046e-01)      
head(fd3_new)


#NEW MAE AND MAPE
fd3_new$error_new=(fd3_new$price-fd3_new$predict_new)
head(fd3_new)
mae_new=mean(abs(fd3_new$error_new))
mae_new

fd3_new$abs_err2=abs(fd3_new$price-fd3_new$predict_new)/fd3_new$price
head(fd3_new)
mape_new=mean(fd3_new$abs_err2)*100
mape_new

#NEW NORMALITY OF ERROR
qqnorm(fd3_new$error_new) 
hist(fd3_new$error_new) 
shapiro.test(fd3$error) 




#MULTICOLLINEARITY
install.packages("faraway")

fd3_new$bedrooms=(fd3_new$bedrooms)
fd3_new$flr_bdr=log(fd3_new$flr_bdr)^2
fd3_new$floors_bedrms1=(fd3_new$bedroom/fd3_new$floors)
head(fd3_new)

abcdefghk=step(lm(fd3_new$price~floors_bedrms1+bathrooms+sqft_living+waterfront+condition+High+Medium+sqft_above+age+yr_renovated+sqft_living15+sqft_lot15+hg_age+bth_bd_sqlvg+sqlot_wtrft_cndi+yr_condi_med_sqlt, data=fd3_new)) 
summary(abcdefghk) 


#NEW PREDICTED VALUE
attach(fd3_new)
fd3_new$predict_new=-1.537e+05+(-5.143e+04*floors_bedrms1)+(bathrooms*2.709e+04)+(sqft_living*1.562e+02)+(waterfront*6.954e+05)+(condition*2.163e+04)+(High*4.891e+05)+(Medium*9.270e+04)+(sqft_above*-4.461e+01)+(age*2.513e+03)+(yr_renovated*2.002e+01)+(sqft_living15*9.253e+01)+(sqft_lot15*-2.914e-01)+(hg_age*1.910e+03)+(bth_bd_sqlvg*1.567e+00)+(yr_condi_med_sqlt*-7.039e-05)      
head(fd3_new)


#NEW MAE AND MAPE
fd3_new$error_new=(fd3_new$price-fd3_new$predict_new)
head(fd3_new)
mae_new=mean(abs(fd3_new$error_new))
mae_new

fd3_new$abs_err2=abs(fd3_new$price-fd3_new$predict_new)/fd3_new$price
head(fd3_new)
mape_new=mean(fd3_new$abs_err2)*100
mape_new

