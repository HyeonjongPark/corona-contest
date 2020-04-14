# install.packages("Epi")
# install.packages("ztable")
# install.packages("sjmisc")
# install.packages("plotROC")

rm(list = ls())

require(Epi)
require(pROC)
require(ztable)
require(moonBook)
library(plotROC)
source("ROC_sub.R")

#ROC함수는 세개의 리스트를 반환하는데 첫번째 res는 데이타프레임으로 각각의 lr.eta값에 대해 민감도, 특이도 등이 들어 있읍니다. 
#최적의 cutoff point는 이중 민감도+특이도의 합이 제일 큰 것이 되겠읍니다. 즉 다음과 같이 구할 수 있읍니다.

radial$sex = NULL
radial %>% head
radial$smoking = as.character(radial$smoking)
radial %>% str()

radial$smoking[radial$smoking == "non-smoker"] = 0
radial$smoking[radial$smoking == "ex-smoker"] = 1
radial$smoking[radial$smoking == "smoker    "] = 2

radial$smoking = as.integer(radial$smoking)

colSums(is.na(radial))
radial = na.omit(radial)


a1=ROC(form=male~height,data=radial,plot="ROC")
a1 ## ROC 함수는 세개의 리스트 반환


nothing = glm(male ~ 1, data = radial, family = binomial)
fullmod = glm(male ~ age+height+weight+HBP+DM+smoking+TC+TG+
                HDL+LDL+hsCRP+NTAV+PAV, data = radial)

both = step(nothing, list(lower=formula(nothing),upper=formula(fullmod)),
         direction="both")

fullmod %>% summary()
both %>% summary()


# 1. lr.eta 값ㅇㅔ대해 민감도 특이도
# 최적의 cut off point 는 민감도 + 특이도의 합이 제일 큰 것
# 
# classifier = svm(male ~ .,
#                  data = radial,
#                  type = 'C-classification',
#                  kernel = 'linear')
# classifier
# y_pred %>% length()
# radial %>% nrow()
# radial[,-which(names(radial)=="male")] %>% nrow()
# # Predicting the Validation set results
# y_pred = predict(classifier, newdata = radial[,-which(names(radial)=="male")])
# y_pred = as.factor(as.character(y_pred))

# radial$male %>% length()
# ROC(y_pred, radial$male)


optimal_lr.eta=function(x){
  no=which.max(x$res$sens+x$res$spec)[1]
  result=x$res$lr.eta[no]
  result
}
optimal_lr.eta(a1) 

optimal_cutpoint=function(x){
  y=optimal_lr.eta(x)
  b0=unname(x$lr$coeff[1])
  b1=unname(x$lr$coeff[2])
  result=(-log(1/y-1)-b0)/b1
  result
} 

optimal_cutpoint(a1) 


a2=ROC(form=male~smoking,data=radial,plot="ROC")
a2
optimal_cutpoint(a2) 

plot(a1$res$sens~I(1-a1$res$spec),type="l")
par(new=TRUE)
plot(a2$res$sens~I(1-a2$res$spec),type="l",axes=F,xlab="",ylab="",col="red")


b1=roc(male~height,radial,ci=T,percent=T)
b2=roc(male~smoking,radial,ci=T,percent=T)

plot(b1)

plot(b2,add=TRUE,col="red")


roc.test(b1,b2,plot=T)

wilcox.test(height~male,data=radial)



plot_ROC(a1)

plot_ROC(a2)

plot_ROC(a1,a2)






a3=ROC(form=male~NTAV,data=radial,plot="ROC")
a3 ## ROC 함수는 세개의 리스트 반환

plot_ROC(a1,a2,a3)





a4=ROC(form=male~height+smoking+NTAV, data=radial,plot="ROC")
plot_ROC(a4)
plot_ROC(a1,a2,a3,a4,show.sens=FALSE)


# perform stepwise backward regression
result=step_ROC(male~height+smoking+NTAV,data=radial,plot=FALSE)

plot_ROC(result$initial,result$final,show.lr.eta=FALSE,show.sens=FALSE,type=1)


print(result$table,type="html")



