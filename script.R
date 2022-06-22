# STEP 1 : conception and data definition
#~~~~~~~~~~~~~~~~~~~~~~~~~~~STEP 2 : data collection~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#import data
library(readxl)
stat <- read_excel("C:/Users/imani/OneDrive/Bureau/ourProject/testo.xlsx")
View(stat)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~STEP 3 : data preparation~~~~~~~~~~~~~~~~~~~~~~~~~
# 3_1 renaming our columns:

j <- 1
while (j<=12) {
  for (i in 8:20) {
    n <- "Item"
    names(stat)[i]<- print(paste0(n,j))
    j <- j + 1
  }
}

names(stat)[names(stat)=="Are you in the \"Génie Informatique\" or \"Génie industrielle\" branch ?"]="filiere"

# 3.2 conversion : 

stat$Age = as.numeric(stat$Age)

stat$Gender = factor(stat$Gender)

stat$`Family status` = factor(stat$`Family status`)

stat$`Are you a 3rd year student at ENSAK?` = factor(stat$`Are you a 3rd year student at ENSAK?`)

stat$filiere = factor(stat$filiere)






h <- 8
for (h in 8:20) {
  item <- names(stat)[h]
  if(is.character(stat[[item]])){
    stat[[item]] = factor(stat[[item]], levels = c("Strongly Agree", "Agree", "Neither agree or disagree", "Disagree", "Strongly Disagree"))
  }
}




#3.3 nettoyage
#3.3.1 traitments des valeurs aberrantes
boxplot(stat$Age)
boxplot.stats(stat$Age)
boxplot.stats(stat$Age)$out
for (out in boxplot.stats(stat$Age)$out) {
  for (l in 1:length(stat$Age)) {
    if(stat$Age[l] == out){
      stat$Age[l] = NA
    }
  }
}
#3.3.2 traitments des valeurs manquantes:
pro=sum(is.na(stat$Age))/length(stat$Age)
pro
# 3 cas : realimenter ,estimer et supprimer
if(pro<0.05 && (length(stat$Age)-sum(is.na(stat$Age))>=30)){
  print("supprimer")
}else{
  print("estimer")
}
# supprimer
stat <- na.omit(stat)
# supprimer les personnes hors champs d'etude
stat <- stat[-28,]
stat <- stat[-42,]
stat <- stat[-51,]
dim(stat)

#deleting unused columns
stat$Horodateur=NULL
stat$`Adresse e-mail`=NULL





# test de normalité
#age normality
shapiro.test(stat$Age)    #doesn't follow the normal law

# test quasi-normality

library(moments)
kurtosis(stat$Age)
skewness(stat$Age)  
                          #almost quasi normale, but not normal
qqnorm(stat$Age)
qqline(stat$Age)



#analyse de fiabilite de notre questionnaire


#our survey has two item parts (units)
#items about perception on importance of internships: items 1,4,6,9,10,12,13
#items about difficulty of getting an internship: items 2,3,5,7,8,11


#lets test the reliability of these two grps of items

#grp importance

library(psych)
importance=stat[c(6,9,11,14,15,17,18)]





#codification
levels(importance$`Item1`)=c(1,2,3,4,5)
levels(importance$`Item4`)=c(1,2,3,4,5)
levels(importance$`Item9`)=c(1,2,3,4,5)
levels(importance$`Item6`)=c(5,4,3,2,1)
levels(importance$`Item10`)=c(5,4,3,2,1)
levels(importance$`Item12`)=c(5,4,3,2,1)
levels(importance$`Item13`)=c(5,4,3,2,1)



for (i in names(importance)) {
  importance[[i]]=as.character(importance[[i]])
  importance[[i]]=as.numeric(importance[[i]])
}






#using correlation matrices, or a bartlet score
cor(importance,method = "pearson")


corr.test(importance,method = "pearson")



bartlett.test(importance)






#p-value < 5%. we must measure the internal coherence with alpha de crombach
library(psych)
alpha(importance,check.keys=TRUE)      # alpha=0.68, now we can do factor analysis


#grp difficlty


library(psych)


diff=stat[c(7,8,10,12,13,16)]

levels(diff$`Item2`)=c(1,2,3,4,5)
levels(diff$`Item3`)=c(1,2,3,4,5)
levels(diff$`Item5`)=c(1,2,3,4,5)
levels(diff$`Item7`)=c(1,2,3,4,5)
levels(diff$`Item8`)=c(5,4,3,2,1)
levels(diff$`Item11`)=c(1,2,3,4,5)


for (i in names(diff)) {
  diff[[i]]=as.character(diff[[i]])
  diff[[i]]=as.numeric(diff[[i]])
}



cor(diff,method = "pearson")

bartlett.test(diff)


alpha(diff)       #alpha=0.61. we good, we can move with factor analysis





#extracting factors with factor analysis
#we can also do an ACP analysis

local({
  .PC <- princomp(~Item2+Item3+Item5+Item7+Item8+Item11, cor=TRUE, 
                  data=diff)
  cat("\nComponent loadings:\n")
  print(unclass(loadings(.PC)))
  cat("\nComponent variances:\n")
  print(.PC$sd^2)
  cat("\n")
  print(summary(.PC))
  screeplot(.PC)
  
})          #we can see that the cumulative proportion reaches 0.68 by the third component


#but for the rest of our study we will use factors extracted by factor analysis
local({
  .FA <- factanal(~Item2+Item3+Item5+Item7+Item8+Item11, factors=2, 
                  rotation="varimax", scores="Bartlett", data=diff)
  print(.FA)
  diff <<- within(diff, {
    F2 <- .FA$scores[,2]
    F1 <- .FA$scores[,1]
  })
})



local({
  .FA <- factanal(~Item1+Item4+Item9+Item6+Item10+Item12+Item13, factors=2, 
                  rotation="varimax", scores="Bartlett", data=importance)
  print(.FA)
  importance <<- within(importance, {
    F2 <- .FA$scores[,2]
    F1 <- .FA$scores[,1]
  })
})



#~~~~~~~~~~~~~~~~~~~~~~STEP4  :traitement des données ~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1:exploration numerique

summary(stat)


#4.2:exploration graphique

plot(stat$Gender)
hist(stat$Age)
plot(stat$`Are you a 3rd year student at ENSAK?`)
plot(stat$`Family status`)




#functions
library(ggplot2)
calculer <- function(item){
  c <- 1
  a <- 0
  b <- 0
  d <- 0
  e <- 0
  f <- 0
  for (c in 1:length(stat[[item]])) {
    if(stat[[item]][c] == "Strongly Agree"){
      a <- a + 1
    }else if(stat[[item]][c] == "Agree"){
      b <- b + 1
    }else if(stat[[item]][c] == "Neither agree or disagree"){
      d <- d + 1 
    }else if(stat[[item]][c] == "Disagree"){
      e <- e + 1
    }else{
      f <- f + 1
    }
  }
  nb <<- c(a,b,d,e,f)
  return(nb)
}

colored_plot <- function(item)
{
  df <- data.frame(
    group = c("Strongly Agree", "Agree", "Neither agree or disagree", "Disagree", "Strongly Disagree"),
    value = calculer(item)
  )
  bp<- ggplot(df, aes(x="", y=value, fill=group))+
    geom_bar(width = 1, stat = "identity") + ggtitle(item) + ylab("nb of observations")
  bp
  
}



colored_plot("Item1")
colored_plot("Item2")
colored_plot("Item3")
colored_plot("Item4")
colored_plot("Item5")
colored_plot("Item6")
colored_plot("Item7")
colored_plot("Item8")
colored_plot("Item9")
colored_plot("Item10")
colored_plot("Item11")
colored_plot("Item12")
colored_plot("Item13")






#4.3: test des hypotheses

# we will test how representative our sample is
# we have two categorical variables that partition our sample: gender & fillier
# test if our sample is representative by gender

chisq.test(table(stat$Gender),    p=c(1/2,1/2)) 
summary(stat$Gender)
#p-value> 5%=>il n'y a pas de difference en terme de frequence des hommes et des femmes 


#we test if our sample is representative by filier
summary(stat$filiere)
chisq.test(table(stat$filiere),    p=c(1/2,1/2))
#p-value> 5%=>il n'y a pas de difference en terme de frequence des info et des indus



#now relationship between these two var quali
chisq.test(table(stat$Gender,stat$filiere))
#p-value>5%: gender doesnt have an association with your filliere choice


#d'apres ces resulats. our sample is good and "exploratoire"


summary(stat$Age)

#etude univariee des items

chisq.test(table(stat$`Item1`))     #there is a significant difference between the answers
chisq.test(table(stat$`Item2`))     #there is a significant difference between the answers
chisq.test(table(stat$`Item3`))     #there is a significant difference between the answers
chisq.test(table(stat$`Item4`))     #there is a significant difference between the answers
chisq.test(table(stat$`Item5`))     #there is a significant difference between the answers
chisq.test(table(stat$`Item6`))     #there is a significant difference between the answers
chisq.test(table(stat$`Item7`))     #there is a significant difference between the answers
chisq.test(table(stat$`Item8`))     #there is a significant difference between the answers
chisq.test(table(stat$`Item9`))     #there is a significant difference between the answers
chisq.test(table(stat$`Item10`))     #there is a significant difference between the answers
chisq.test(table(stat$`Item11`))     #there is a significant difference between the answers
chisq.test(table(stat$`Item12`))     #there is a significant difference between the answers
chisq.test(table(stat$`Item13`))     #there is a significant difference between the answers


# étude bivaiée

tab <- table(stat$filiere, stat$Gender)
tab
library(questionr)
cprop(tab)
lprop(tab)
chisq.test(tab)
mosaicplot(tab)       #gender doesnt have an association with your filliere choice

#we move to see if theres an association between our categorical vars and our items

tab1 = table(stat$filiere, stat$`Item1`)
tab1
fisher.test(tab1)
# we can not reject the null hypothesis Ho
# there is no relationship between the two categorical variables. 
tab2 = table(stat$filiere, stat$`Item2` )
tab2
fisher.test(tab2)
# there is no relationship between the two categorical variables.
tab3 = table(stat$filiere, stat$`Item3` )
tab3
fisher.test(tab3)
# there is no relationship between the two categorical variables.
tab4 = table(stat$filiere, stat$`Item4` )
tab4
fisher.test(tab4)
# there is no relationship between the two categorical variables.
tab5 = table(stat$filiere, stat$`Item5` )
tab5
fisher.test(tab5)
# there is no relationship between the two categorical variables.
tab6 = table(stat$filiere, stat$`Item6` )
tab6
fisher.test(tab6)
# there is no relationship between the two categorical variables.
tab7 = table(stat$filiere, stat$`Item7` )
tab7
fisher.test(tab7)
# there is no relationship between the two categorical variables.
tab8 = table(stat$filiere, stat$`Item8` )
tab8
fisher.test(tab8)
# we see that the p-value is less than the significance level of 5%, we can reject the null hypothesis.
# Ho accepted the variables are dependent, there is a relationship between the two categorical variables.
tab9 = table(stat$filiere, stat$`Item9` )
tab9
fisher.test(tab9)
# there is no relationship between the two categorical variables.
tab10 = table(stat$filiere, stat$`Item10` )
tab10
fisher.test(tab10)
# there is no relationship between the two categorical variables.
tab11 = table(stat$filiere, stat$`Item11` )
tab11
fisher.test(tab11)
# there is no relationship between the two categorical variables.
tab12 = table(stat$filiere, stat$`Item12` )
tab12
fisher.test(tab12)
# there is no relationship between the two categorical variables.
tab13 = table(stat$filiere, stat$`Item13` )
tab13
fisher.test(tab13)
# there is no relationship between the two categorical variables.
tab14 = table(stat$Gender, stat$`Item1`)
tab14
fisher.test(tab14)
# there is no relationship between the two categorical variables.
tab15 = table(stat$Gender, stat$`Item2`)
tab15
fisher.test(tab15)
# there is no relationship between the two categorical variables.
tab16 = table(stat$Gender, stat$`Item3`)
tab16
fisher.test(tab16)
# there is no relationship between the two categorical variables.
tab17 = table(stat$Gender, stat$`Item4`)
tab17
fisher.test(tab17)
# there is no relationship between the two categorical variables.
tab18 = table(stat$Gender, stat$`Item5`)
tab18
fisher.test(tab18)
# there is no relationship between the two categorical variables.
tab20 = table(stat$Gender, stat$`Item7`)
tab20
fisher.test(tab20)
# there is no relationship between the two categorical variables.
tab21 = table(stat$Gender, stat$`Item8`)
tab21
fisher.test(tab21)
# there is no relationship between the two categorical variables.
tab22 = table(stat$Gender, stat$`Item9`)
tab22
fisher.test(tab22)
# there is no relationship between the two categorical variables.
tab23 = table(stat$Gender, stat$`Item10`)
tab23
fisher.test(tab23)
# there is no relationship between the two categorical variables.
tab24 = table(stat$Gender, stat$`Item11`)
tab24
fisher.test(tab24)
# there is no relationship between the two categorical variables.
tab25 = table(stat$Gender, stat$`Item12`)
tab25
fisher.test(tab25)
# there is no relationship between the two categorical variables.
tab26 = table(stat$Gender, stat$`Item13`)
tab26
fisher.test(tab26)
# there is no relationship between the two categorical variables.


#exploration de notre questionaire

table(stat$`Item1`)
table(stat$`Item2`)
table(stat$`Item3`)
table(stat$`Item4`)
table(stat$`Item5`)
table(stat$`Item6`)
table(stat$`Item7`)
table(stat$`Item8`)
table(stat$`Item9`)
table(stat$`Item10`)
table(stat$`Item11`)
table(stat$`Item12`)
table(stat$`Item13`)


#analyse multivariee
item_matrix=cbind(diff,importance)
item_matrix=item_matrix[c(-7,-8,-17,-16)]


library(psych)
corr.test(item_matrix)


#now, testing our research hypothesis
#there is a positive correlation between the students’ perception of the importance of internships and their perception of the difficulty to find one.


shapiro.test(importance$F1)       #not normal
shapiro.test(diff$F1)             #not normal
shapiro.test(diff$F2)             #not normal
shapiro.test(importance$F1)       #not normal

cor.test(importance$F1,diff$F1, method = "spearman")    #no correlation


hyp=data.frame(imp1=importance$F1,imp2=importance$F2, diff1=diff$F1,diff2=diff$F2)
cor(hyp,method = "spearman")     #lets look at the correlattion coefficients

library(psych)
corr.test(hyp,method = "spearman")





#this is the answer to our research question


#we failed to reject the null hypotheses


#we conclude there is no correlation between perception on internships and perception on their difficulty



#####regression
#we will attempt to make a regression model of our research data

#our variable a expliquer is:  perception of difficulty of getting an internship

#Y= F1 of perception of difficulty

#our variable expliquative is:  perception on importance of internships

# X1= F1 & F2 of perception of importance

#we'll start with a simple linear regression and encrease the variables

reg <- lm(hyp$diff1 ~ hyp$imp1, data=hyp)

summary(reg)

#lets test for the prerequisites of our regression model


#1) X and Y must be quanti

#2) Y must follow loi normal

shapiro.test(diff$F1)     #doesn't follow normal law


library(moments)
kurtosis(diff$F1)         #not quasi normale
skewness(diff$F1)

#this is a problem but we will keep going

#3) X and Y must be correlated


shapiro.test(importance$F1)       #follows normal law    so the model will be better

cor.test(importance$F1,diff$F1, method = "spearman")      #not correlated, we move on


#4)   a must be significantly different than 0



summary(reg)  #p-value>5%   le a n'est pas significativement different de 0 


#5) residual must follow loi normal

res=summary(reg)$residuals

shapiro.test(res)   #res does not follow loi normal


#6)  residual must not be correlated to X or Y

cor.test(res, hyp$diff1)      #correlation

cor.test(res, hyp$imp1)   #pas de correlation


#conclusion: this model is not adequate 

plot(hyp$diff1,hyp$imp1,  pch=16, cex=1)
abline(reg, lwd=5, col="red")


#lets move to multiple linear regression



reg2 <- lm(hyp$diff1 ~ hyp$imp1+hyp$imp2, data=hyp)

summary(reg2)           #we can see the improvment already



#3) X and Y must be correlated

cor.test(hyp$imp2,hyp$diff1, method = "spearman")      #not correlated, we move on



summary(reg2)  #p-value>5%   le a n'est pas significativement different de 0 


#5) residual must follow loi normal

res2=summary(reg2)$residuals

shapiro.test(res2)   #res2 doesn't follow loi normal



#6)  residual must not be correlated to X or Y

cor.test(res2, hyp$diff1)      #correlation

cor.test(res2, hyp$imp1)   #pas de correlation

cor.test(res2, hyp$imp2)      #pas de correlation



#7) testing the independance of our variables

car::vif(lm(hyp$diff1 ~ hyp$imp1+hyp$imp2, data=hyp))
#Vif < 10 => our variables are indepandant


summary(reg2)       # we can see that the p-value has decreased and the r_sq slight;u encreased. which is good, but not enough improvement

#our model is still not adequate








