

# 石田基広『実践 Rによるテキストマイニング』森北出版 スクリプト vers.1.0
# 第 6 章
## section 6.1

library(tidyverse) 
load("tab.Rdata")
td <- apply(tab[2:11, 2:11], c(1,2), as.integer)
td <- as.matrix(td) ; rownames(td) <- tab[2:11, 1] ;colnames(td) <- tab[1, 2:11]
td

svd_d <- svd(td)
svd_d$u

jitY <- jitter(svd_d$u[,2], amount=.1)
jitX <- jitter(svd_d$u[,1], amount=.1)
plot(jitX, jitY, type = "n")
text(jitX, jitY, labels = rownames(td))

## Chapter6 # seciton 6.2
package = "https://cran.r-project.org/package=softmaxreg&version=1.2"
utils::install.packages(pkgs = package, repos = NULL)
library(softmaxreg)
data(word2vec)
dim(word2vec)


install.packages("Rtsne")
library(Rtsne)
tsne_words <- Rtsne(as.matrix(word2vec[,-1]),check_duplicate = FALSE, 
                    verbose = TRUE)


idx <- 1:100
plot(tsne_words$Y[idx],  t = 'n', main = "Rtsne")
text(tsne_words$Y[idx], labels = word2vec[idx,1])
dev.off()

word2vec %>% filter(word %in% c("peace", "begin", "with", "a", "smile"))

wordEmbed("Peace begin with a smile", word2vec, meanVec = TRUE)

wordEmbed(c("Peace", "begin", "with", "a", "smile"), 
            word2vec, meanVec = TRUE) %>%
              colMeans()

loadURLData("http://archive.ics.uci.edu/ml/machine-learning-databases/00217/C50.zip", 
             getwd(),  unzip = TRUE)

## 訓練データ
subFoler <- c('AaronPressman', 'BernardHickey')
docTrain <- document(path = paste("C50train/",subFoler, sep = ""), pattern = 'txt')
xTrain <- wordEmbed(docTrain, dictionary = word2vec)
yTrain <-  c(rep(1,50), rep(2,50))
## テストデータ
docTest <- document(path = paste("C50test/",subFoler, sep = ""), pattern = 'txt')
xTest <- wordEmbed(docTest, dictionary = word2vec)
yTest <- c(rep(1,50), rep(2,50))
# 

xTrain %>% dim
xTrain[1:2, 1:10]

library(glmnet)
res <- cv.glmnet(xTrain, yTrain, alpha = 1, family = "binomial")
pred <- predict(res, xTest, type = "class")
library(caret)
confusionMatrix(table(pred, yTest))


# setwd("~/tmp")
source("http://rmecab.jp/R/Aozora.R")
kokoro <- Aozora("https://www.aozora.gr.jp/cards/000148/files/773_ruby_5968.zip")

library(RMeCab)
gc(); gc()

library(tidyverse)
## 形態素解析を実行
kokoro_separated <- RMeCabText(kokoro)
kokoro_separated %>%  map(function(x)  
  if_else((x[[1]] == "Ｋ" | (x[[2]] %in% c("名詞", "形容詞", "動詞"))  && 
              (!x[[3]] %in% c("数", "非自立", "接尾") )
	        && (x[[8]] != "*")), 
           x[[8]], "")) %>% unlist () %>%
    str_remove_all("する|いう|ある|いる|ない|なる" ) %>%
    paste(" ", collapse = "") %>%
    writeLines(con = "kokoro_separated.txt") 
# 
install.packages("text2vec")
library(text2vec)
gc(); gc()
kokoro <- readLines("kokoro_separated.txt", warn = FALSE)


## 後で再利用することを考えオブジェクトを保存しておく
## save(kokoro, file = "kokoro.Rdata");
### load("kokoro.Rdata") #でオブジェクトを再現できる

tokens <- space_tokenizer(kokoro)
iter <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(iter)

vocab <- prune_vocabulary(vocab, term_count_min = 5)

vectorizer <- vocab_vectorizer(vocab)

tcm <- create_tcm(iter, vectorizer, skip_grams_window = 10)

RcppParallel::setThreadOptions(1)
set.seed(789)

# glove <- GlobalVectors$new(word_vectors_size = 100, 
#                            vocabulary = vocab, 
#                            x_max = 10)
glove <- GlobalVectors$new(rank = 100, x_max = 10)# for text2Vec ver.06
# 以下、本書初版の出力とは一致しないことがあります
glove_fit_kokoro <- glove$fit_transform(tcm, n_iter = 100)



dim(glove_fit_kokoro)

word_vectors <- glove_fit_kokoro + t(glove$components)

kokoro_sim <- sim2(word_vectors, word_vectors["先生", , drop = FALSE],
                    method = "cosine", norm = "l2")
kokoro_sim <- tibble(term = rownames(kokoro_sim), score = kokoro_sim)

kokoro_sim

kokoro_sim  %>% group_by(score > 0) %>% top_n(20, abs(score)) %>%
                  ungroup() %>% ggplot(aes(fct_reorder(term, score),
                                       score, fill = score > 0)) +
                                       geom_col(alpha = 0.8, 
                                           show.legend = FALSE) +
                                           coord_flip()



rm(sensei)
sensei  <- word_vectors["先生", , drop = FALSE]  - 
             word_vectors["Ｋ", , drop = FALSE] +
    word_vectors["お嬢さん" , , drop = FALSE]

mat <- sim2(as.matrix(word_vectors), as.matrix(sensei),
            method = "cosine", norm = "l2")
sort(mat[,1],decreasing = TRUE) %>% head(20) %>% round(3)


sort(mat[,1],decreasing = FALSE) %>% head(20) %>% round(3)



## section 6.2.3

# load("iPhone_data.Rdata")
iPhone_data <- iPhone_data %>% select(Po,text)
## for windows users
if( (.Platform$OS.type == "windows") & any(unique(Encoding(iPhone_data$text)) %in% "UTF-8")){
  iPhone_data <- iPhone_data %>% mutate(text = iconv(text, 
                                                     from = "UTF-8", 
                                                     to = "CP932",
                                                       sub = ""))} else{
                                                       }


library(caret)
set.seed(123)
inTrain <- createDataPartition(
  y = iPhone_data$Po,
  p = .7,
  list = FALSE
)
training <- iPhone_data[as.matrix(sample(inTrain)),]
testing  <- iPhone_data[-inTrain,]

library(RMeCab)
gc(); gc()

rmecabc_fast <- function(po, txt, train = TRUE){
     txt <- unlist(RMeCabC(txt, 1))
     txt <- txt[names(txt) %in% c("名詞")] 
     evals <- paste0("__label__", ifelse(po,1,2))
     txt <- paste(txt, collapse = " ")
     if(train)  txt <- paste(evals,  txt)
     tibble(Eval = evals, SENTENCE = txt)
}


## 訓練データ

train_iPhone <- pmap_dfr(list(training$Po, training$text, train = TRUE),  
                             ~ rmecabc_fast(..1, ..2, ..3)
 )

train_iPhone$SENTENCE[1] %>% Encoding()
writeLines(train_iPhone$SENTENCE, con = "train_iPhone.txt")

## テストデータ
test_iPhone <- pmap_dfr(list(testing$Po, testing$text, train = FALSE),
                             ~ rmecabc_fast(..1, ..2, ..3)
 )
test_iPhone$SENTENCE[1] %>% Encoding()
writeLines(test_iPhone$SENTENCE, con = "test_iPhone.txt")


train_iPhone %>% head()
test_iPhone %>% head()
install.packages("fastTextR") # fastTextR パッケージのバージョン 2.0 以降、関数名に変更があります。読者にご指摘いただきました。ここに記して御礼申し上げます。_
library(fastTextR)
model <- ft_train("train_iPhone.txt", method = "supervised", #  ver1.0 # fasttext 
                  control = ft_control(nthreads = 3L))#  ver1.0 # ft.controol 

iPhone_words <- ft_words(model)# get_words(model)
word_vec <- ft_word_vectors(model, iPhone_words) #  ver1.0 # get_word_vector
word_vec %>% tail()

test <- readLines("test_iPhone.txt")
labels <- trimws(gsub(",.*", "", test))
table(labels)
test <- ft_normalize(test)
test <- trimws(sub(".*?,", "", test))
head(test, 2)
preds <- ft_predict(model, newdata = test_iPhone$SENTENCE)# ver1.0 # predict newdata_file = 

install.packages("rminer")
library(rminer)
mmetric(as.factor(test_iPhone$Eval), preds$label, metric = "ACC")



