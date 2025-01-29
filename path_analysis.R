library(lavaan)
library(semPlot)
df <- read.csv("data.csv")
df<-df[df$realm!='Antarctica' & df$realm!='Oceania',]
df<-df[c("med_rich", "Clim.PC1", "MAT_STD", "Human", "vas_rich", "glottolog")]

df<-na.omit(df)
cor.table<-cor(df, use="complete.obs", method="pearson")


#Simplified model with important variables
model='
med_rich~glottolog+Human+MAT_STD+Clim.PC1+vas_rich
vas_rich~MAT_STD+Clim.PC1
glottolog~MAT_STD+Clim.PC1
'

model.fit=sem(model,sample.cov=cor.table,sample.nobs=328)
summary(model.fit,fit.measures=TRUE,standardized=TRUE, rsquare=TRUE)


#*********************************************************************************************************
#-----------------------------------------Residual SEM----------------------------------------------------
#*********************************************************************************************************

m1<-lm(med_rich~vas_rich, data=df)
res<-as.data.frame(m1$residuals)
colnames(res)<-c("residuals")
df<-cbind(df,res)
a<-df[c(2:4,6:7)]

cor.table<-cor(a, use="complete.obs", method="pearson")

#Simplified model with residuals
model='
residuals~glottolog+Human+MAT_STD+Clim.PC1
glottolog~MAT_STD+Clim.PC1
'
model.fit=sem(model,sample.cov=cor.table,sample.nobs=328)
summary(model.fit,fit.measures=TRUE,standardized=TRUE, rsquare=TRUE)

#****************************************************************************************************************************************
#****************************************************Regional models*********************************************************************
#****************************************************************************************************************************************
library(lavaan)
df <- read.csv("data.csv")
df<-df[df$realm!='Antarctica' & df$realm!='Oceania',]


#****************************************************************************************************************************************
#****************************************************Neotropics*************************************************************************
#****************************************************************************************************************************************
n_trop<-df[df$realm == 'Neotropic',]
n_trop<-n_trop[c("med_rich", "Clim.PC1", "MAT_STD", "Human", "vas_rich", "glottolog")]

n_trop<-na.omit(n_trop)
cor.table<-cor(n_trop, use="complete.obs", method="pearson")
cor.table

#Simplified model with important variables
model='
med_rich~glottolog+Human+MAT_STD+Clim.PC1+vas_rich
vas_rich~MAT_STD+Clim.PC1
glottolog~MAT_STD+Clim.PC1
'
model.fit=sem(model,sample.cov=cor.table,sample.nobs=49)
summary(model.fit,fit.measures=TRUE,standardized=TRUE, rsquare=TRUE)


#****************************************************************************************************************************************
#****************************************************Indomalaya*************************************************************************
#****************************************************************************************************************************************
indo<-df[df$realm == 'Indomalaya',]
indo<-indo[c("med_rich", "Clim.PC1", "MAT_STD", "Human", "vas_rich", "glottolog")]

indo<-na.omit(indo)
cor.table<-cor(indo, use="complete.obs", method="pearson")

#Simplified model with important variables
model='
med_rich~glottolog+Human+MAT_STD+Clim.PC1+vas_rich
vas_rich~MAT_STD+Clim.PC1
glottolog~MAT_STD+Clim.PC1
'
model.fit=sem(model,sample.cov=cor.table,sample.nobs=31)
summary(model.fit,fit.measures=TRUE,standardized=TRUE, rsquare=TRUE)


#****************************************************************************************************************************************
#****************************************************Australasia*************************************************************************
#****************************************************************************************************************************************
austra<-df[df$realm == 'Australasia',]
austra<-austra[c("med_rich", "Clim.PC1", "MAT_STD", "Human", "vas_rich", "glottolog")]

austra<-na.omit(austra)
cor.table<-cor(austra, use="complete.obs", method="pearson")

#Simplified model with important variables
model='
med_rich~glottolog+Human+MAT_STD+Clim.PC1+vas_rich
vas_rich~MAT_STD+Clim.PC1
glottolog~MAT_STD+Clim.PC1
'
model.fit=sem(model,sample.cov=cor.table,sample.nobs=22)
summary(model.fit,fit.measures=TRUE,standardized=TRUE, rsquare=TRUE)


#****************************************************************************************************************************************
#*******************************************************Nearctic*************************************************************************
#****************************************************************************************************************************************
n_arc<-df[df$realm == 'Nearctic',]
n_arc<-n_arc[c("med_rich", "Clim.PC1", "MAT_STD", "Human", "vas_rich", "glottolog")]

n_arc<-na.omit(n_arc)
cor.table<-cor(n_arc, use="complete.obs", method="pearson")

#Simplified model with important variables
model='
med_rich~glottolog+Human+MAT_STD+Clim.PC1+vas_rich
vas_rich~MAT_STD+Clim.PC1
glottolog~MAT_STD+Clim.PC1
'
model.fit=sem(model,sample.cov=cor.table,sample.nobs=70)
summary(model.fit,fit.measures=TRUE,standardized=TRUE, rsquare=TRUE)


#****************************************************************************************************************************************
#*******************************************************Palearctic*************************************************************************
#****************************************************************************************************************************************
p_arc<-df[df$realm == 'Palearctic',]
p_arc<-p_arc[c("med_rich", "Clim.PC1", "MAT_STD", "Human", "vas_rich", "glottolog")]

p_arc<-na.omit(p_arc)
cor.table<-cor(p_arc, use="complete.obs", method="pearson")

#Simplified model with important variables0
#Simplified model with important variables
model='
med_rich~glottolog+Human+MAT_STD+Clim.PC1+vas_rich
vas_rich~MAT_STD+Clim.PC1
glottolog~MAT_STD+Clim.PC1
'
model.fit=sem(model,sample.cov=cor.table,sample.nobs=95)
summary(model.fit,fit.measures=TRUE,standardized=TRUE, rsquare=TRUE)


#****************************************************************************************************************************************
#*******************************************************Afrotropic***********************************************************************
#****************************************************************************************************************************************
afro<-df[df$realm == 'Afrotropic',]
afro<-afro[c("med_rich", "Clim.PC1", "MAT_STD", "Human", "vas_rich", "glottolog")]

afro<-na.omit(afro)
cor.table<-cor(afro, use="complete.obs", method="pearson")

#Simplified model with important variables0
#Simplified model with important variables
model='
med_rich~glottolog+Human+MAT_STD+Clim.PC1+vas_rich
vas_rich~MAT_STD+Clim.PC1
glottolog~MAT_STD+Clim.PC1
'
model.fit=sem(model,sample.cov=cor.table,sample.nobs=61)
summary(model.fit,fit.measures=TRUE,standardized=TRUE, rsquare=TRUE)


