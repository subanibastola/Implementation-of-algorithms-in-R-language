################################################################################################
#Decesion tree on IRIS using Rpart function and Tree. Both works the same
################################################################################################

install.packages("rpart")
library(rpart)
A=data.frame(iris)
psych::pairs.panels(A[,1:4])
sf=sample(2,nrow(A),replace=TRUE,prob = c(0.7,0.3))
trd= A[sf==1,]
tsd= A[sf==2,]

model_rpart=rpart(Species~.,data=trd)

plot(model_rpart)
text(model_rpart)

pred_rpart=predict(model_rpart,tsd)
pred_rpart=ifelse(pred_rpart[,1]==1,"setosa",ifelse(pred_rpart[,2]>0.5,"versicolor","virginica"))

rpart1=table(pred_rpart,tsd$Species)
mis=(nrow(tsd)-diag(tree1))
rpart1

################################################################################################

library(tree)
A=data.frame(iris)
psych::pairs.panels(A[,1:4])
sf=sample(2,nrow(A),replace=TRUE,prob = c(0.7,0.3))
trd= A[sf==1,]
tsd= A[sf==2,]
model_tree=tree(Species~.,data=trd)
plot(model_tree)
text(model_tree)

pred_tree=predict(model_rpart,tsd)
pred_t=ifelse(pred_tree[,1]>0.5,"setosa",ifelse(pred_tree[,2]>0.5,"versicolor","virginica"))
tree1=table(pred_t,tsd$Species)
tree1
