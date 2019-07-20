library(tidyverse)
library(rpart)
library(woeBinning)
library(purrr)
library(tidyr)
library(ggplot2)
library(ROCR)
library(randomForest)
library(ClustOfVar)
library(gbm)
library(caret)
library(xgboost)
library(Hmisc)
install.packages("ClustOfVar")
library(ClustOfVar)
options(repos = c(CRAN = "http://cran.rstudio.com"))


set.seed(1704)

#loading data

dane_geo <- read.csv2("final_data.csv", header = T, sep = ",", dec = ".")
#filtering out the data without DefFlags

#useful variables - means, maximums and minimums for three groups of variables: DPD, NotionalOverdue, NotionalValue
# for example mean_dpd3 stands for mean calculated for dpd over previous three months
# variables for 3,6,9 and 12 months were retained in the dataset but you can change that if needed



dane_geo <- dane_geo %>% 
  ##średnie i maksima dla zmiennych dpd
  mutate( mean_dpd2 = rowMeans(select(.,DPD_lag1:DPD_lag2)),
          mean_dpd3 = rowMeans(select(.,DPD_lag1:DPD_lag3)),
          mean_dpd4 = rowMeans(select(.,DPD_lag1:DPD_lag4)),
          mean_dpd5 = rowMeans(select(.,DPD_lag1:DPD_lag5)),
          mean_dpd6 = rowMeans(select(.,DPD_lag1:DPD_lag6)),
          mean_dpd7 = rowMeans(select(.,DPD_lag1:DPD_lag7)),
          mean_dpd8 = rowMeans(select(.,DPD_lag1:DPD_lag8)),
          mean_dpd9 = rowMeans(select(.,DPD_lag1:DPD_lag9)),
          mean_dpd10 = rowMeans(select(.,DPD_lag1:DPD_lag10)),
          mean_dpd11 = rowMeans(select(.,DPD_lag1:DPD_lag11)),
          mean_dpd12 = rowMeans(select(.,DPD_lag1:DPD_lag12)),
          max_dpd2 = apply(select(., DPD_lag1:DPD_lag2),1,max),
          max_dpd3 = apply(select(., DPD_lag1:DPD_lag3),1,max),
          max_dpd4 = apply(select(., DPD_lag1:DPD_lag4),1,max),
          max_dpd5 = apply(select(., DPD_lag1:DPD_lag5),1,max),
          max_dpd6 = apply(select(., DPD_lag1:DPD_lag6),1,max),
          max_dpd7 = apply(select(., DPD_lag1:DPD_lag7),1,max),
          max_dpd8 = apply(select(., DPD_lag1:DPD_lag8),1,max),
          max_dpd9 = apply(select(., DPD_lag1:DPD_lag9),1,max),
          max_dpd10 = apply(select(., DPD_lag1:DPD_lag10),1,max),
          max_dpd11 = apply(select(., DPD_lag1:DPD_lag11),1,max),
          max_dpd12 = apply(select(., DPD_lag1:DPD_lag12),1,max),
          min_dpd2 = apply(select(., DPD_lag1:DPD_lag2),1,min),
          min_dpd3 = apply(select(., DPD_lag1:DPD_lag3),1,min),
          min_dpd4 = apply(select(., DPD_lag1:DPD_lag4),1,min),
          min_dpd5 = apply(select(., DPD_lag1:DPD_lag5),1,min),
          min_dpd6 = apply(select(., DPD_lag1:DPD_lag6),1,min),
          min_dpd7 = apply(select(., DPD_lag1:DPD_lag7),1,min),
          min_dpd8 = apply(select(., DPD_lag1:DPD_lag8),1,min),
          min_dpd9 = apply(select(., DPD_lag1:DPD_lag9),1,min),
          min_dpd10 = apply(select(., DPD_lag1:DPD_lag10),1,min),
          min_dpd11 = apply(select(., DPD_lag1:DPD_lag11),1,min),
          min_dpd12 = apply(select(., DPD_lag1:DPD_lag12),1,min),
          #średnie i maksima dla zmiennych NotionalValue
          mean_nval2 = rowMeans(select(.,NotionalValue_lag1:NotionalValue_lag2)),
          mean_nval3 = rowMeans(select(.,NotionalValue_lag1:NotionalValue_lag3)),
          mean_nval4 = rowMeans(select(.,NotionalValue_lag1:NotionalValue_lag4)),
          mean_nval5 = rowMeans(select(.,NotionalValue_lag1:NotionalValue_lag5)),
          mean_nval6 = rowMeans(select(.,NotionalValue_lag1:NotionalValue_lag6)),
          mean_nval7 = rowMeans(select(.,NotionalValue_lag1:NotionalValue_lag7)),
          mean_nval8 = rowMeans(select(.,NotionalValue_lag1:NotionalValue_lag8)),
          mean_nval9 = rowMeans(select(.,NotionalValue_lag1:NotionalValue_lag9)),
          mean_nval10 = rowMeans(select(.,NotionalValue_lag1:NotionalValue_lag10)),
          mean_nval11= rowMeans(select(.,NotionalValue_lag1:NotionalValue_lag11)),
          mean_nval12 = rowMeans(select(.,NotionalValue_lag1:NotionalValue_lag12)),
          max_nval2 = apply(select(., NotionalValue_lag1:NotionalValue_lag2),1,max),
          max_nval3 = apply(select(., NotionalValue_lag1:NotionalValue_lag3),1,max),
          max_nval4 = apply(select(., NotionalValue_lag1:NotionalValue_lag4),1,max),
          max_nval5 = apply(select(., NotionalValue_lag1:NotionalValue_lag5),1,max),
          max_nval6 = apply(select(., NotionalValue_lag1:NotionalValue_lag6),1,max),
          max_nval7 = apply(select(., NotionalValue_lag1:NotionalValue_lag7),1,max),
          max_nval8 = apply(select(., NotionalValue_lag1:NotionalValue_lag8),1,max),
          max_nval9 = apply(select(., NotionalValue_lag1:NotionalValue_lag9),1,max),
          max_nval10 = apply(select(., NotionalValue_lag1:NotionalValue_lag10),1,max),
          max_nval11 = apply(select(., NotionalValue_lag1:NotionalValue_lag11),1,max),
          max_nval12 = apply(select(., NotionalValue_lag1:NotionalValue_lag12),1,max),
          min_nval2 = apply(select(., NotionalValue_lag1:NotionalValue_lag2),1,min),
          min_nval3 = apply(select(., NotionalValue_lag1:NotionalValue_lag3),1,min),
          min_nval4 = apply(select(., NotionalValue_lag1:NotionalValue_lag4),1,min),
          min_nval5 = apply(select(., NotionalValue_lag1:NotionalValue_lag5),1,min),
          min_nval6 = apply(select(., NotionalValue_lag1:NotionalValue_lag6),1,min),
          min_nval7 = apply(select(., NotionalValue_lag1:NotionalValue_lag7),1,min),
          min_nval8 = apply(select(., NotionalValue_lag1:NotionalValue_lag8),1,min),
          min_nval9 = apply(select(., NotionalValue_lag1:NotionalValue_lag9),1,min),
          min_nval10 = apply(select(., NotionalValue_lag1:NotionalValue_lag10),1,min),
          min_nval11 = apply(select(., NotionalValue_lag1:NotionalValue_lag11),1,min),
          min_nval12 = apply(select(., NotionalValue_lag1:NotionalValue_lag12),1,min),
          #średnie i maksima dla zmiennych NotionalOverdue
          mean_novd2 = rowMeans(select(.,NotionalOverdue_lag1:NotionalOverdue_lag2)),
          mean_novd3 = rowMeans(select(.,NotionalOverdue_lag1:NotionalOverdue_lag3)),
          mean_novd4 = rowMeans(select(.,NotionalOverdue_lag1:NotionalOverdue_lag4)),
          mean_novd5 = rowMeans(select(.,NotionalOverdue_lag1:NotionalOverdue_lag5)),
          mean_novd6 = rowMeans(select(.,NotionalOverdue_lag1:NotionalOverdue_lag6)),
          mean_novd7 = rowMeans(select(.,NotionalOverdue_lag1:NotionalOverdue_lag7)),
          mean_novd8 = rowMeans(select(.,NotionalOverdue_lag1:NotionalOverdue_lag8)),
          mean_novd9 = rowMeans(select(.,NotionalOverdue_lag1:NotionalOverdue_lag9)),
          mean_novd10 = rowMeans(select(.,NotionalOverdue_lag1:NotionalOverdue_lag10)),
          mean_novd11 = rowMeans(select(.,NotionalOverdue_lag1:NotionalOverdue_lag11)),
          mean_novd12 = rowMeans(select(.,NotionalOverdue_lag1:NotionalOverdue_lag12)),
          max_novd2 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag2),1,max),
          max_novd3 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag3),1,max),
          max_novd4 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag4),1,max),
          max_novd5 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag5),1,max),
          max_novd6 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag6),1,max),
          max_novd7 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag7),1,max),
          max_novd8 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag8),1,max),
          max_novd9 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag9),1,max),
          max_novd10 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag10),1,max),
          max_novd11 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag11),1,max),
          max_novd12 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag12),1,max),
          min_novd2 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag2),1,min),
          min_novd3 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag3),1,min),
          min_novd4 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag4),1,min),
          min_novd5 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag5),1,min),
          min_novd6 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag6),1,min),
          min_novd7 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag7),1,min),
          min_novd8 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag8),1,min),
          min_novd9 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag9),1,min),
          min_novd10 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag10),1,min),
          min_novd11 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag11),1,min),
          min_novd12 = apply(select(., NotionalOverdue_lag1:NotionalOverdue_lag12),1,min),
          spend_over_income = Monthly_Spendings / Monthly_Income)

dane_geo <-  dane_geo %>% select(Application_ID: DefFlag, mean_dpd3, mean_dpd6, mean_dpd9, mean_dpd12,
                         max_dpd3, max_dpd6, max_dpd9, max_dpd12,
                         min_dpd3, min_dpd6, min_dpd9, min_dpd12,
                         mean_nval3, mean_nval6,mean_nval9, mean_nval12,
                         max_nval3, max_nval6, max_nval9, max_nval12,
                         min_nval3, min_nval6, min_nval9, min_nval12,
                         mean_novd3, mean_novd6, mean_novd9, mean_novd12,
                         max_novd3, max_novd6,max_novd9, max_novd12,
                         min_novd3, min_novd6,min_novd9, min_novd12)


prog_geo <- dane_geo %>% filter(is.na(DefFlag))
dane_geo <- dane_geo %>% filter(!is.na(DefFlag))
##splitting the data into training and validation sets - respectively 70% and 30%
#we build our models on the training set and test their accuracy on validation

SplitDataSet <- function(data.set, training.fraction){
  random.numbers <- sample.int(nrow(data.set))
  quantiles <- quantile(random.numbers, probs = c(0, training.fraction, 1))
  split.labels <- cut(random.numbers, quantiles, include.lowest = T,
                      labels = c("training", "validation"))
  return(split(data.set, split.labels))
}

set_geo <- SplitDataSet(dane_geo, 0.7)

training_geo <- set_geo$training
validation_geo <- set_geo$validation


#pca is a technique meant to reduce the dimensionality of a dataset with highly correlated variables
#pca was performed on 3 groups of highly correlated variables: DPD, NationalValue, NotionalOverdue
# for DPD 3 principal components were retained: dpd_pc1, dpd_pc2, dpd_pc3
# for NOtionaValue 1 principal component was retained: nval_pc1
# for NotionalOverdue 4 principal components were retained: novd_pc1, novd_pc2, novd_pc3, novd_pc4
# for each group retained components represent more than 90% of the variance of input variables
# the principal components are not correlated with one another so if they are significant you can use all of them in your model

pca_dpd <- prcomp(select(training_geo, DPD_lag1:DPD_lag12), scale = TRUE, center = TRUE)
vd_dpd_pc <- as.data.frame(predict(pca_dpd, newdata = select(validation_geo, DPD_lag1:DPD_lag12)))
prog_dpd_pc <- as.data.frame(predict(pca_dpd, newdata = select(prog_geo, DPD_lag1:DPD_lag12)))
summary(pca_dpd)

pca_nval <- prcomp(select(training_geo, NotionalValue_lag1:NotionalValue_lag12), scale = TRUE, center = TRUE)
vd_nval_pc <- as.data.frame(predict(pca_nval, newdata = select(validation_geo, NotionalValue_lag1:NotionalValue_lag12)))
prog_nval_pc <- as.data.frame(predict(pca_nval, newdata = select(prog_geo, NotionalValue_lag1:NotionalValue_lag12)))
summary(pca_nval)

pca_novd <- prcomp(select(training_geo, NotionalOverdue_lag1:NotionalOverdue_lag12), scale = TRUE, center = TRUE)
vd_novd_pc <- as.data.frame(predict(pca_novd, newdata = select(validation_geo, NotionalOverdue_lag1:NotionalOverdue_lag12)))
prog_novd_pc <- as.data.frame(predict(pca_novd, newdata = select(prog_geo, NotionalOverdue_lag1:NotionalOverdue_lag12)))
summary(pca_novd)

training_geo <- training_geo %>% cbind(pca_dpd$x[,c(1:3)]) %>% rename( dpd_pc1 = PC1, dpd_pc2 = PC2, dpd_pc3 = PC3) %>%
  cbind(pca_nval$x[,c(1,2)]) %>% select(-PC2) %>% rename(nval_pc1 = PC1)  %>% 
  cbind(pca_novd$x[,c(1:4)]) %>% rename(novd_pc1 = PC1, novd_pc2 = PC2, novd_pc3 = PC3, novd_pc4 = PC4)

validation_geo <- validation_geo %>% cbind(select(vd_dpd_pc, PC1, PC2, PC3 )) %>% rename( dpd_pc1 = PC1, dpd_pc2 = PC2, dpd_pc3 = PC3) %>%
  cbind(select(vd_nval_pc,PC1)) %>% rename(nval_pc1 = PC1) %>%
  cbind(select(vd_novd_pc, PC1:PC4)) %>% rename(novd_pc1 = PC1, novd_pc2 = PC2, novd_pc3 = PC3, novd_pc4 = PC4)

prog_geo <- prog_geo %>% cbind(select(prog_dpd_pc, PC1, PC2, PC3 )) %>% rename( dpd_pc1 = PC1, dpd_pc2 = PC2, dpd_pc3 = PC3) %>%
  cbind(select(prog_nval_pc,PC1)) %>% rename(nval_pc1 = PC1) %>%
  cbind(select(prog_novd_pc, PC1:PC4)) %>% rename(novd_pc1 = PC1, novd_pc2 = PC2, novd_pc3 = PC3, novd_pc4 = PC4)


training_geo <-  training_geo %>% select(Application_ID: Unemployed_highschool_and_lower_number, DefFlag, mean_dpd3, mean_dpd6, mean_dpd9, mean_dpd12,
                                 max_dpd3, max_dpd6, max_dpd9, max_dpd12,
                                 min_dpd3, min_dpd6, min_dpd9, min_dpd12,
                                 mean_nval3, mean_nval6,mean_nval9, mean_nval12,
                                 max_nval3, max_nval6, max_nval9, max_nval12,
                                 min_nval3, min_nval6, min_nval9, min_nval12,
                                 mean_novd3, mean_novd6, mean_novd9, mean_novd12,
                                 max_novd3, max_novd6,max_novd9, max_novd12,
                                 min_novd3, min_novd6,min_novd9, min_novd12,
                                 dpd_pc1, dpd_pc2, dpd_pc3,
                                 nval_pc1,
                                 novd_pc1, novd_pc2, novd_pc3, novd_pc4,
                                 NotionalOverdue_t0, NotionalValue_t0, DPD_t0)

validation_geo <-  validation_geo %>% select(Application_ID: Unemployed_highschool_and_lower_number, DefFlag, mean_dpd3, mean_dpd6, mean_dpd9, mean_dpd12,
                                         max_dpd3, max_dpd6, max_dpd9, max_dpd12,
                                         min_dpd3, min_dpd6, min_dpd9, min_dpd12,
                                         mean_nval3, mean_nval6,mean_nval9, mean_nval12,
                                         max_nval3, max_nval6, max_nval9, max_nval12,
                                         min_nval3, min_nval6, min_nval9, min_nval12,
                                         mean_novd3, mean_novd6, mean_novd9, mean_novd12,
                                         max_novd3, max_novd6,max_novd9, max_novd12,
                                         min_novd3, min_novd6,min_novd9, min_novd12,
                                         dpd_pc1, dpd_pc2, dpd_pc3,
                                         nval_pc1,
                                         novd_pc1, novd_pc2, novd_pc3, novd_pc4,
                                         NotionalOverdue_t0, NotionalValue_t0, DPD_t0)

prog_geo <-  prog_geo %>% select(Application_ID: Unemployed_highschool_and_lower_number, DefFlag, mean_dpd3, mean_dpd6, mean_dpd9, mean_dpd12,
                                         max_dpd3, max_dpd6, max_dpd9, max_dpd12,
                                         min_dpd3, min_dpd6, min_dpd9, min_dpd12,
                                         mean_nval3, mean_nval6,mean_nval9, mean_nval12,
                                         max_nval3, max_nval6, max_nval9, max_nval12,
                                         min_nval3, min_nval6, min_nval9, min_nval12,
                                         mean_novd3, mean_novd6, mean_novd9, mean_novd12,
                                         max_novd3, max_novd6,max_novd9, max_novd12,
                                         min_novd3, min_novd6,min_novd9, min_novd12,
                                         dpd_pc1, dpd_pc2, dpd_pc3,
                                         nval_pc1,
                                         novd_pc1, novd_pc2, novd_pc3, novd_pc4,
                                         NotionalOverdue_t0, NotionalValue_t0, DPD_t0)
# functions undersample and oversample are meant to make our dataset balanced
# function undersample is keeping all observations with flag "1" and chooses randomly a number of "0"s
# in order to achieve the desired fraction of "1"s in the sample for example
#if we want to have 50% of ones in our training set and we have 5000 obs with 1 it will randomly choose 5000 "0"s
#with or without replacement
#oversample samples observations with "1" with replacement to achieve the same result
#generaly speaking undersampling seems a better idea
#resampled is a set which was build using this function
UnderSample <- function(dane, target , target_prop =  0.5, replacement = F ){


dt <- split(dane, target)

unique_values <- unique(target)
start_number <- as.vector(table(target))
start_proportion <- as.vector(prop.table(table(target)))

n_cases <- start_number[2]
to_sample <- round((n_cases / target_prop) - n_cases)

rows <- sample.int(nrow(dt$`0`), to_sample, replace = replacement)

new_sample <- rbind(dt$`0`[rows,], dt$`1`)

return(new_sample)

}


resampled_geo <- UnderSample(training_geo, training_geo$DefFlag, 0.5)





#the purpose of categorization is to enable us to measure the dependence between our predictor variables
#and the target DefFlag
#we can use a variety of measures like chi2 test, out V-Cramer coefficient but the function WOE returns
#automatically IV (Information Value) - higher values implies stronger relationship (better for us)



# WoeBinning categorizes variables according to the target variable in order to achieve the
#best discrimination power i.e. the ablility to distinguish ones from zeros
#description of the arguments: https://cran.r-project.org/web/packages/woeBinning/woeBinning.pdf


#categorization of all the numeric variables
#tabulate.binning contains the categories, distributions and Woe of categorized variables


#kategoryzacja wszystkich zmiennych numeric
#jako tabulate.binning zapisuje, kategorie, rozk?ady, WOE pokategoryzowanych zmiennych
#jako BinPlot zapisuje wykresy pokategoryzowanych zmiennych i wykres variables w kolejnosci information value
#wi?kszo?? dzieli na dwie kategorie, trzeba by popatrze? indywidualnie na rozk?ady zmiennych
#w zbiorze binned -> skopiowany zbi?r resampled+dodane zmienne pokategoryzowane

## the training.binned and validation.binned contain binned variables
## you can use them or not according to your preference
## they enable you to take account of nonlinear relationships in the data
# and are robust against outliers
binned_geo <- resampled_geo

###binning of the dataset with added geographical variables

##i added someiprovements i exclude variables for which i have calculated minimums and maximums

WoeBinning_geo <- function(target, variable, min.perc.total, min.perc.class, stop.limit, event.class){
  
  binning_geo <<- woe.tree.binning(binned_geo, target, variable, min.perc.total, min.perc.class, stop.limit, event.class)
  
  tabulate.binning.geo <<- woe.binning.table(binning_geo)
  
  binned_geo <<- woe.binning.deploy(binned_geo, binning_geo)
  
  BinPlot_geo <<-woe.binning.plot(binning_geo, multiple.plots=FALSE)
  
  
}

classes <- sapply(training_geo, class)
num_names <- names(classes[classes == "numeric" | classes == "integer"]) 
num_names <- num_names[num_names!= "DefFlag" & num_names != "GEO_region"]

WoeBinning_geo('DefFlag', num_names, min.perc.total=0.01, min.perc.class=0.01,
           stop.limit=0.1, event.class='bad')

## the training.binned and validation.binned contain binned variables
## you can use them or not according to your preference
## they enable you to take account of nonlinear relationships in the data
# and are robust against outliers
training_geo.binned <- woe.binning.deploy(training_geo, binning_geo)
validation_geo.binned <- woe.binning.deploy(validation_geo, binning_geo)
prog_geo.binned <- woe.binning.deploy(prog_geo, binning_geo)


## iv contains the variable names and their information value (sorted by information value)


iv_geo <- as.data.frame(binning_geo[,c(1,3)], col.names = c("zmienna", "iv") ) 

best_iv_geo <- iv_geo[iv_geo$V2 > 0.02,]  %>% select(V1) %>% unlist() %>% unname()


####NOWA CZĘŚĆ
tree <- ClustOfVar::hclustvar(binned_geo[,best_iv_geo])
plot(tree)

n_clus <- 20
clusters <- cutreevar(tree, n_clus, matsim = TRUE)

cluster_id <- clusters$cluster


variables <- binning_geo[,1] %>% unlist()
iv <- binning_geo[,3] %>% unlist()
iv_clust <- data.frame(variables,iv, stringsAsFactors = FALSE)
row.names(iv_clust) <- t(iv_clust[,1])
iv_clust <- iv_clust[best_iv_geo,]
iv_clust <- iv_clust %>% cbind(cluster_id)
colnames(iv_clust) <- c("var", "iv", "cluster_id")

cls <- split(iv_clust, cluster_id)
clust_best <- rep(NA, n_clus)

for(i in 1:n_clus){
  cls[[i]] <- arrange(cls[[i]], desc(cls[[i]]$iv))
  cls[[i]] <- cls[[i]] %>% head(1) %>% select(var) %>% as.character()
  clust_best[i] <- cls[[i]]
}

clust_best <- setdiff(clust_best, c("max_novd3", "mean_dpd12","NotionalOverdue_t0","min_novd9", "min_dpd3","max_dpd12"))

######


##współczynniki chi2 pomiędzy pobinowanymi zmiennymi i DefFlagiem
#chi2 test between all factor variables in the input dataframe and the target variable
df_chisq_test <- function(df, target_variable){
  target <- as.factor(df[,target_variable])
  var_cat <- sapply(df, class)
  nms <- names(var_cat[var_cat == "factor"])
  nms <- nms[ nms != target_variable & nms != "Application_ID"]
  chi2 <- c()
  p_value <- c()
  cramer_v <- c()
  for (i in 1:length(nms)){
    test <- chisq.test(df[,nms[i]], df[,target_variable])
    chi2 <- c(chi2, test$statistic)
    p_value <- c(p_value, test$p.value)
    x_cats <- length(levels(df[,nms[i]])) - 1
    y_cats <- length(levels(target)) - 1
    cram <- sqrt(test$statistic / length(df[,target_variable]) / min(c(x_cats,y_cats)))
    cramer_v <- c(cramer_v,cram)
  }
  
  results <- data.frame(nms, round(chi2,3), round(p_value,3), round(cramer_v,3))
}


#the same for the dataset with added geographical variables
df_chi2_geo <- df_chisq_test(training_geo.binned, "DefFlag")
df_chi2_geo <- df_chi2_geo[order(df_chi2_geo$round.cramer_v..3., decreasing = TRUE),]

best_20_geo <- df_chi2_geo[order(df_chi2_geo$round.cramer_v..3., decreasing = TRUE),] %>% select(nms) %>% head(20)
best_20_geo <- best_20_geo[best_20_geo != "DefFlag" & best_20_geo != "DefFlag.binned"]


korelacja <- cor(as.matrix(binned_geo[,best_iv_geo]))
## best_20 is a vector with 20 best variables
## best_20_geo contains also the geographical variables



#-----
#Drzewa z boostingiem gradientowym

#best_predictors_up_to_date predictors <- select(binned, best_20_iv, Job_type:Monthly_Spendings) 

# best_model_up_to_date bst <- xgboost::xgb.train(data = dmodel, max.depth = 4,
#                          nrounds = 600 , eta = 0.01, subsample = 0.8, gamma = 1,colsample_bytree = 0.8, watchlist = watchlist, eval.metric = "auc", eval.metric = "rmse", verbose = F, objective = "binary:logistic")
predictors <- select(binned_geo, clust_best, Job_type:Monthly_Spendings) 

#predictors <- select(binned_geo, clust_best, Job_type:Car_status) 

mt <- model.matrix(~.+0, binned_geo[,c("Job_type", "Marital_status", "Home_status", "Car_status")])
nm <- names(predictors)
nm <- setdiff(nm,c("Job_type", "Marital_status", "Home_status", "Car_status") )
mt_model <- cbind(mt, binned_geo[,nm])

mt <- model.matrix(~.+0, training_geo.binned[,c("Job_type", "Marital_status", "Home_status", "Car_status")])
mt_train <- cbind(mt, training_geo.binned[,nm])

mt <- model.matrix(~.+0, validation_geo.binned[,c("Job_type", "Marital_status", "Home_status", "Car_status")])
mt_valid <- cbind(mt, validation_geo.binned[,nm])

mt <- model.matrix(~.+0, prog_geo.binned[,c("Job_type", "Marital_status", "Home_status", "Car_status")])
mt_prog <- cbind(mt, prog_geo.binned[,nm])


dtrain <- xgb.DMatrix(data = data.matrix(mt_train), label = data.matrix(training_geo.binned[,"DefFlag"]))
dtest <- xgb.DMatrix(data =  data.matrix(mt_valid), label = data.matrix(validation_geo.binned[,"DefFlag"]))

dmodel <- xgb.DMatrix(data =  data.matrix(mt_model), label = data.matrix(binned_geo[,"DefFlag"]))
watchlist = list(train = dtrain, test = dtest)



#-----  
# XGBOOST na najlepszych 20 pobinowanych zmiennych
# 4 - 600 - 50,9 - to jest mój jak dotychczas najlepszy model
#subsample 0.4 - GINI 51,1

#4-600 i 20 najlepszych zmiennych według iv daje GINI 52,5 na zbiorze testowym
#najlepszy model to było zmienne z IV większym niż 0.15, ze zbioru dane (bez zmiennych geograficznych i ze wszystkimi zmiennymi dla których liczyłem max i min itp.)


#najlepszy model i najlepsze zmienne
#best_20_iv_geo <- iv_geo[iv_geo$V2 > 0.15,]  %>% select(V1) %>% unlist() %>% unname()

#bst <- xgboost::xgb.train(data = dmodel, max.depth = 4,
#                          nrounds = 500 , eta = 0.01, subsample = 0.8, gamma = 1,colsample_bytree = 0.8, watchlist = watchlist, eval.metric = "auc", eval.metric = "rmse", verbose = F, objective = "binary:logistic")


bst <- xgboost::xgb.train(data = dmodel, max.depth = 4,
                          nrounds = 1000 , eta = 0.01, subsample = 0.8, gamma = 1,colsample_bytree = 0.8, watchlist = watchlist, eval.metric = "auc", eval.metric = "rmse", verbose = F, objective = "binary:logistic")


eval_log <- bst$evaluation_log


par(mfrow = c(1,2))
plot(x = eval_log$iter, y= eval_log$test_auc, type = "l")
plot(x = eval_log$iter, y = eval_log$test_rmse, type = "l")

par(mfrow = c(1,1))
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)

top_features <- importance_matrix[c(1:11),1]
top_features <- unlist(top_features)
xgb.plot.importance(importance_matrix = importance_matrix)

# ocenanie drzew z boostingiem xgboost-----------------------------------------------------------


model <- bst

in_prop <- as.vector(prop.table(table(dane_geo$DefFlag)))
rs_prop <- as.vector(prop.table(table(binned_geo$DefFlag)))

score.or.class <- gain <- lift <- roc <- auc <- prediction.object <- list()
score.or.class[[1]] <- list(training_geo.binned$DefFlag, training_geo.binned$DefFlag)

prediction_test <- predict(model, new = data.matrix(mt_train), type = "prob")
prediction_valid <- predict(model, new = data.matrix(mt_valid), type ="prob")
prediction_prog <- predict(model, new = data.matrix(mt_prog), type = "prob" )

prediction_test <- prediction_test*(in_prop[2] / rs_prop[2]) / (prediction_test*in_prop[1]/rs_prop[2] + (1 -prediction_test) *in_prop[2]/rs_prop[2])

pred_test <- data.frame(training_geo.binned$Application_ID, prediction_test)
colnames(pred_test) <- c("Application_ID", "Score")

prediction_valid <- prediction_valid*(in_prop[2] / rs_prop[2]) / (prediction_valid*in_prop[1]/rs_prop[2] + (1-prediction_valid) *in_prop[2]/rs_prop[2])

pred_val <- data.frame(validation_geo.binned$Application_ID, prediction_valid)
colnames(pred_val) <- c("Application_ID", "Score")
prediction_prog <- prediction_prog*(in_prop[2] / rs_prop[2]) / (prediction_prog*in_prop[1]/rs_prop[2] + (1-prediction_prog) *in_prop[2]/rs_prop[2])

pred_prog <- data.frame(prog_geo.binned$Application_ID, prediction_prog)
colnames(pred_prog) <- c("Application_ID", "Score")


pred <- pred_test %>% rbind(pred_val) %>% rbind(pred_prog) 
#write.table(pred, "output_final.csv", sep = ";", dec = ".", row.names = FALSE)

score.or.class[[2]] <- list(as.vector(prediction_test),
                            training_geo.binned$DefFlag)
score.or.class[[3]] <- list(as.vector(prediction_valid),
                            validation_geo.binned$DefFlag)
class.average <- mean(training_geo.binned$DefFlag)
random.class <- 1
for (i in 1:(nrow(training_geo.binned) - 1)) {
  random.class <- c(random.class, mean(random.class) < class.average)
}
score.or.class[[4]] <- list(seq(0, 1, len = nrow(training_geo.binned)), random.class)

for (i in 1:length(score.or.class)) {
  prediction.object[[i]] <- prediction(score.or.class[[i]][[1]],
                                       score.or.class[[i]][[2]])
  gain[[i]] <- performance(prediction.object[[i]], "tpr", "rpp")
  lift[[i]] <- performance(prediction.object[[i]], "lift", "rpp")
  roc[[i]] <- performance(prediction.object[[i]], "tpr", "fpr")
  auc[[i]] <- performance(prediction.object[[i]], "auc")
}

LEGEND_LABELS <- c("wizard", "train", "test", "random")
ShowCurve <- function(list, name, AUC = FALSE, legend.position = "right") {
  for (i in 1:length(list)) {
    plot(list[[i]], main = paste(name, " curve"),
         col = gray((i - 1) / 4), lwd = 2, add = (i != 1), xlim = c(0, 1))
    if (AUC) {
      text(.2, 0.9 - i * 0.1, pos = 4, col = gray((i - 1) / 4), cex = .9,
           paste("AUC =", round(auc[[i]]@y.values[[1]], digit = 2)))
    }
  }
  legend(legend.position, lty = 2, lwd = 2, col = gray(0:3 / 4),
         y.intersp = .6, legend = LEGEND_LABELS, seg.len = 0.6, bty = "n")
}
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
ShowCurve(gain, "Gain")
ShowCurve(lift, "Lift", legend.position = "topright")
ShowCurve(roc, "ROC", AUC = TRUE)

Gini_train = 2*(auc[[2]]@"y.values"[[1]] - 0.5)
Gini_test = 2*(auc[[3]]@"y.values"[[1]] - 0.5)

print(Gini_train)
print(Gini_test)

#-----


#write.csv(training.binned, "training_binned.csv")
#write.csv(validation.binned, "validation_binned.csv")
#write.csv(binned, "binned.csv")

#write.csv(training_geo.binned, "training_geo_binned.csv")
#write.csv(validation_geo.binned, "validation_geo_binned_geo.csv")
#write.csv(binned_geo[,best_20_iv_geo], "binned_geo.csv")


