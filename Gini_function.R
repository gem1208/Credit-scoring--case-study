
rm(list = ls())

#read data
dane <- read.csv("C:/Users/Ngoc/Desktop/Master 2/Statistical learning method/case study/sample_data.csv")
unique(dane$DefFlag)  #

#split dataset and create test, train, validation dataset

test <- dane[is.na(dane$DefFlag),] # is DeffLag has missing values?
mydata <-dane[!is.na(dane$DefFlag),]

set.seed(0)

rand<-sample(1:nrow(mydata), 0.8*nrow(mydata))
train <-mydata[rand,]
valid <-mydata[-rand,]
  
# model1
mod <- glm(DefFlag~ Age  + Monthly_Income + Monthly_Spendings +  Job_type, data = train, family = "binomial")

pred <- predict(mod, newdata = valid, type="response")


#model2
mod2 <- glm(DefFlag~ Age, data = ..., family = "binomial")
pred2 <-predict(mod2, newdata = valid, type= "response")

#predict values for validation set


# Input data format

#'> head(test_data)
#'  Application_ID   Score DefFlag
#'1    CCC20000001 0.32452       0
#'2    CCC20000002 0.23452       0
#'3    CCC20000003 0.21421       0
#'4    CCC20000004 0.51253       0
#'5    CCC20000005 0.24125       0
#'6    CCC20000006 0.69321       0


# Define function

Gini_value <- function(
                       score, #prediction from model
                       target #target binary variable (0/1)
                       ){
  
  default <- ifelse(target==0, 'G','B')
  d <- data.frame(FY = default, SCORE = score)
  s <- table(d[,2],d[,1])
  sHeader <- colnames(s)
  s <- cbind(s,apply(s,2,cumsum))
  colnames(s) <- c(sHeader,paste(sHeader,"_cum",sep=""))
  s <- cbind(s , s[,"B_cum"]/max(s[,"B_cum"]) , s[,"G_cum"]/max(s[,"G_cum"]),
             diff(c(0,s[,"G_cum"]))/max(s[,"G_cum"]))
  colnames(s)<-c(sHeader,
                 paste(sHeader,"_cum",sep=""),
                 c("%cum_bad","%cum_good","%good"))
  p <- 1:nrow(s)
  s <- cbind(s, c( s[1,7] , s[p[-1],7]+s[(p-1)[-1],7]) ) 
  s <- cbind(s, c(0,s[1:(nrow(s)-1),"%cum_bad"]))
  colnames(s)[length(colnames(s))] <- "%cum_bad_prev"
  auc <- sum(s[,"%good"]*(s[,"%cum_bad"]+s[,"%cum_bad_prev"])*0.5)
  gini_value <- abs( 2 * ( auc - 0.5 ) )
  return(gini_value)
  
}


#install.packages('Hmisc')
library(Hmisc)
rcorr.cens(test_data$Score, test_data$DefFlag)['Dxy']

#############

#choose model

Gini_value(progn, valid$DefFlag)
Gini_value(progn2, valid$DefFlag)

#prepare model for whole dataset

mod <- glm(DefFlag ~ Age + Monthly_Income + Monthly_Spendings + Job_type, data=mydata, family="binomial")

mypredict <-predict(mod, newdata = test, type= "response")

#predict values for test set

#prepare data with final results

Prediction <- data.frame(Application_ID = test$Application_ID, Score = mypredict)

Data <- data.frame(Application_ID = mydata$Application_ID, Score = mydata$DefFlag)

final <- rbind(Data, Prediction)

#check if scores are correct
sum(is.na(final$Score))
nrow(final) == 100000

#save file
write.csv(final, "Output.csv", row.names = F)
