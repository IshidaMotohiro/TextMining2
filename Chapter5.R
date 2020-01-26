
# 石田基広『実践 Rによるテキストマイニング』森北出版 スクリプト vers.1.0
# 第 5 章
## section 5.5

iPhone_spread <- iPhone_counts %>% 
                   spread(key = TERM, value = n, fill = 0)

iPhone_spread  <- iPhone_spread %>% 
                    left_join(iPhone_data %>% 
                      select(status_id, Po))

iPhone_spread <- iPhone_spread %>% select(-status_id)
dim(iPhone_spread)
save(iPhone_spread, file = "iPhone_spread.Rdata")


iPhone_mat <- iPhone_spread %>% select(-Po) %>% 
                 as.matrix(dimnames = list(NULL, 
                           colnames(iPhone_spread)))

parallel::detectCores()

library(doParallel)
cl <- makeCluster(4) 
registerDoParallel(cl)
library(glmnet)
system.time(lasso_cv_auc <- cv.glmnet(x = iPhone_mat , y = iPhone_spread$Po,
                                      type.measure = "auc", 
                                      alpha = 1, family = "binomial",
                                      parallel = TRUE))

save(lasso_cv_auc, file = "lasso_cv_auc.Rdata")
# stopCluster(cl)



plot(lasso_cv_auc)

library(broom)
lasso_cv_auc %>% tidy() %>% filter(lambda ==  lasso_cv_auc$lambda.min |
                                   lambda ==  lasso_cv_auc$lambda.1se)

coef(lasso_cv_auc, s = "lambda.min") %>% head()



lasso_preds <- predict(lasso_cv_auc, s = "lambda.min", 
                       newx = iPhone_mat, type = "class")

preds_tbl <- table(lasso_preds, iPhone_spread$Po)

preds_tbl

## p.90

library(caret)

confusionMatrix(as.factor(lasso_preds), as.factor(iPhone_spread$Po), positive = "1")


(3471 + 1503) / ( 3471 + 1503 + 338 + 32)


1503 / (338 + 1503)

3471 /(3471 + 32)


library(pROC)
lasso_preds_res <- predict(lasso_cv_auc, s = "lambda.min", 
                              newx = iPhone_mat, type = "response")
lasso_roc <- roc(as.numeric(iPhone_spread$Po), as.numeric(lasso_preds_res))
ggroc(lasso_roc)

lasso_roc$auc


library(broom)
library(forcats)
## 係数の抽出

rownames(lasso_cv_auc$glmnet.fit$beta) <- iconv(row.names(lasso_cv_auc$glmnet.fit$beta),
                                                from = "UTF-8", to = "CP932",
                                                sub = "")
coefs <- lasso_cv_auc$glmnet.fit %>%
  tidy() %>%
  dplyr::filter(lambda == lasso_cv_auc$lambda.1se)
## 係数を可視化
coefs %>%
  group_by(estimate > 0) %>%
  top_n(10, abs(estimate)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  labs(
    x = NULL,
    title = "iPhoneについてのツィート",
    subtitle = "ポジティブな単語とネガティブな単語"
  )
dev.off()


## Chapter 5 :section 5.6
library(caret)

set.seed(123)

inTrain <- createDataPartition(
  y = iPhone_spread$Po,
  p = .7,
  list = FALSE
)
training <- iPhone_spread[ inTrain,]
testing  <- iPhone_spread[-inTrain,]



library(doParallel)
cl <- makeCluster(4) # parallel::detectCores() /2 == 4
registerDoParallel(cl)
train_cntrl <-  trainControl(method = "cv", number = 10)
train_grid <- expand.grid(alpha  =  seq(0.01,1.0, by = 0.01) , 
                          lambda = 10^(0:5 * -1))

gc(); gc()
training_X <- training %>% select(-Po)
glmnet_fit_by_caret_elastic  <- train(x = training_X, 
                                       y = as.factor(training$Po),
                                       method = "glmnet", 
                                       tuneGrid = train_grid,
                                       trControl = train_cntrl 
                              # , preProc = c("center", "scale")
)

## 並列処理を止める
stopCluster(cl)
## 時間がかかる処理結果は保存しておく
save(glmnet_fit_by_caret_elastic, file = "glmnet_fit_by_caret_elastic.Rdata")


# glmnet_fit_by_caret <- train(as.factor(Po) ~ .,
#                              data = training,
#                              ...

glmnet_fit_by_caret_elastic$bestTune

testing_ <- testing %>% select(-Po)
yhat <- predict(glmnet_fit_by_caret_elastic, testing_)
confusionMatrix(table(yhat, as.factor(testing$Po)), positive = "1")

varImp(glmnet_fit_by_caret_elastic)






library(tidymodels)
iPhone_spread <- iPhone_spread %>% mutate(Po = as.factor(Po))
splited_data <- initial_split(iPhone_spread, p = 0.5, strata = c('Po')) 
training_data <- training(splited_data)
# training_data <- training_data %>% mutate(Po = as.factor(Po)) 
test_data <- testing(splited_data)
# test_data <- test_data %>% mutate(Po = as.factor(Po))

rec <- recipe(Po ~ ., data = training_data) 
rec

rec_dat <- rec %>% prep(training = training_data)
rec_dat

train_baked <- rec_dat %>% juice() 
test_baked <- rec_dat %>% bake(test_data)

sprintf("%2.5f", 10^(0:5 * -1))




glmnet_model_tidy <- logistic_reg(mixture = 1, penalty  = 10^0) %>% 
    set_engine("glmnet")
glmnet_model_tidy

glmnet_model_tidy <- logistic_reg(mixture = 1, penalty  = 10^(0:5 * -1)) %>% 
                       set_engine("glmnet")
glmnet_model_tidy %>% str()

lasso_tidy <- glmnet_model_tidy %>% fit(Po ~ ., data = train_baked)

lasso_tidy[[2]]$method
lasso_tidy$fit # lasso_tidy[[3]]$call 
lasso_tidy[[3]][[2]] # 推定値


preds  <- test_baked %>% select(Po) %>% 
            bind_cols(fitted = multi_predict(lasso_tidy, test_baked))
preds

preds$.pred[[1]]

preds$.pred[[1]][3, ]

preds_flat <- preds %>% mutate(.pred = map(.pred, bind_rows)) %>%
                          unnest()
preds_flat

preds_flat %>% group_by(penalty) %>% metrics(Po, .pred_class)


## データ整形のために epitools を使う
library(epitools)
## Titanic の配列をデータフレームに変換
Titanic1 <- expand.table(Titanic)
library(rpart)
Titanics_rpart <- rpart (Survived ~ Sex + Age+ Class, data = Titanic1)
Titanics_rpart
library(rpart.plot) 
prp(Titanics_rpart, type=2, extra=101,
     nn = TRUE, fallen.leaves = TRUE, faclen = 0, varlen = 0,
     shadow.col = "grey", branch.lty = 3, cex = 1.2, split.cex = 1.2,
     under.cex = 1.2)
dev.off()

train_cntrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
## パラメータのグリッドサーチ設定の例1
## rf_train_grid <- expand.grid(mtry = 800:1400, splitrule = "gini", 
##                              min.node.size = c(10, 20))
## パラメータのグリッドサーチ設定の例2
rf_train_grid <- expand.grid(mtry = 1000, min.node.size = 1,
                             splitrule = "gini")  
rf_fit_by_caret <- train(x = training[ ,-5535], 
                         y = as.factor(training$Po),
                         method = "ranger", 
                         tuneGrid = rf_train_grid,
                         trControl = train_cntrl, 
                         importance = "impurity", 
                         preProc = c("center", "scale")
)

rf_tidy <- rand_forest(mode = "classification", trees = 20,
                       min_n = 100, mtry = 1000) %>% 
                       set_engine("randomForest", num.threads = 
                       parallel::detectCores()-2, seed = 123)
rf_tidy_fit <- tf_tidy %>% fit(Po ~ ., data = train_baked)



library(randomForest)
rf_tidy_fit

fitteds <- predict(rf_tidy_fit, test_baked)
fitteds$.pred

rf_tidy_preds <- test_baked %>% select(Po) %>% add_column(Fit = fitteds$.pred)
rf_tidy_preds <- tibble(Po = test_baked$Po, Fit = fitteds$.pred$res)
rf_tidy_preds 
rf_tidy_preds %>% select(Fit) %>% pull () %>% head
rf_tidy_preds  %>% colnames()
rf_tidy_preds <- rf_tidy_preds %>% mutate(Fit = fct_recode(Fit, `1` = "TRUE", `0` = "FALSE"))

