

# 石田基広『実践 Rによるテキストマイニング』森北出版 スクリプト vers.1.0
# 第 7 章
## section 7.1

library(reticulate)


## use_python(
##   python = reticulate:::python_unix_binary(bin = "python3.6"),
##   required = TRUE
## )

use_python(
  python = ("/usr/bin/python3.6"),
  required = TRUE
)

gensim <- reticulate::import(module = "gensim")

model <- gensim$models$KeyedVectors$load_word2vec_format(
           "entity_vector.model.bin", binary = TRUE)


# model2 <- gensim$models$Word2Vec$load("Kyubyong/ja.bin")


results <- model$most_similar("アメリカ", topn = 5L)


library(tidyverse)
results %>%  map_df(., ~ tibble(country = .[[1]], similarity = .[[2]]))


results2 <- model$most_similar(positive = list("パリ","日本"),
                   negative = list("フランス"), topn = 5L)

results2 %>%  map_df(., ~ tibble(country = .[[1]], similarity = .[[2]]))



## section 7.2


library(tidyverse)
iPhone_data %>% head()
iPhone_data %>% NROW()


devtools::install_github("rstudio/keras")
library(keras)
install_keras()
library(RMeCab)
rmecab_c <- function(x)  paste(RMeCabC(x, 1), collapse = " ")
rmecab_c(iPhone_data$text[1])
iPhone_separated <- iPhone_data %>% mutate(text = map_chr(text, rmecab_c))
iPhone_separated <- iPhone_separated %>% mutate(N = nchar(text))
# iPhone_separated %>% select(N) %>% max()
# iPhone_separated %>% filter(str_detect(text, "スペック"))
texts <- iPhone_separated  %>%  pull(text)
texts[1:3]

# テキスト内の単語をベクトル化する。
# max_features <- 10000

tokenizer <- text_tokenizer() %>% # , char_level = FALSE) %>% #num_words = max_features, 
  fit_text_tokenizer(texts)
# tokenizer %>% summary()
tokenizer$word_index %>% head() %>% unlist()
# 

texts_sequences <- texts_to_sequences(tokenizer, texts)
texts_sequences [1]

words_vec <- names(tokenizer$word_index)
words_vec %>% head()

names(words_vec) <- tokenizer$word_index
names(words_vec) %>% head()

texts[1]

mat <- texts_to_matrix(tokenizer, texts, mode = "binary")
mat[1:5, 1:5]

sizes <- texts_sequences %>% map_int(length)# %>% max()
max(sizes)
which(sizes == max(sizes))
seq_pad <- pad_sequences(texts_sequences, maxlen = 95, padding = "post")
seq_pad [1:5,1:5]
seq_pad [4065:4067,91:95]
seq_pad  %>% dim()
seq_pad  %>% max()

model <- keras_model_sequential()
model %>% 
  layer_embedding(input_dim = 8210+1, output_dim = 256) %>% 
  layer_global_average_pooling_1d() 
model %>% summary()

model %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 1, activation = "sigmoid")


model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

Y <- iPhone_data %>% mutate(Y = if_else(Po == TRUE,1,0)) %>% pull(Y)

set.seed(123)
idx <- sample(1:5344, 3000)
x_train  <- seq_pad[idx, ]
y_train <- Y[idx] %>% as.matrix()
x_test  <- seq_pad[-idx, ]
y_test <- Y[-idx] %>% as.matrix()


history <- model %>% fit(
  x_train,
  y_train,
  epochs = 20,
  batch_size = 100,
  validation_split = 0.3)

plot(history)


model %>%  evaluate(x_test, y_test)

