
# 石田基広『実践 Rによるテキストマイニング』森北出版 スクリプト vers.1.0
## 第 3 章
## section 3.3 


library(RMeCab)
gc(); gc()
prime <- docDF("data/prime/utf8", 
               type = 1, pos = c("名詞", "形容詞", "動詞"))

prime <- docDF("data/prime/sjis", 
               type = 1, pos = c("名詞", "形容詞", "動詞"))

## 列名を短縮化する
library(tidyverse)
library(magrittr)
colnames(prime) %<>% str_replace("_general-policy-speech.txt", "")
colnames(prime) %<>% str_replace("(\\d{4})\\d{4}_(\\d{3})", "\\1_\\2")

prime %>% dim()

prime <- prime %>% filter(POS2 != "数")

stop_words  <- read_tsv(
   "http://svn.sourceforge.jp/svnroot/slothlib/CSharp/Version1/SlothLib/NLP/Filter/StopWord/word/Japanese.txt",
                           col_names = "TERM")

ja_stop_words  <- stop_words %>% 
      add_row(TERM = c("ある", "する", "てる", "いる", "の", "いう", "しまう",
                       "なる", "おる", "ん", "の", "れる"))


prime <- prime %>% anti_join(ja_stop_words)

prime %>% dim()


## 後で再利用することを考えオブジェクトを保存しておく
## save(prime, file = "prime.Rdata");
### load("prime.Rdata") #でオブジェクトを再現できる


duplicated_terms <- prime %>% select(TERM) %>% group_by(TERM) %>%
                                summarize(N = n()) %>% 
                                  arrange(desc(N)) %>% filter (N > 1)
duplicated_terms

df <- tibble(TERM = c("機", "機", "木"), Col1 = 1:3, Col2 = 1:3)
df
df %>% group_by(TERM) %>% summarize_all(list(sum))

prime2 <- prime %>% select(-c(POS1, POS2)) %>% group_by(TERM) %>%
                      summarize_all(list(sum))

duplicated_terms2 <- prime2 %>% select(TERM) %>% group_by(TERM) %>% 
                       summarize(N = n()) %>% arrange(desc(N)) %>% 
                         filter (N > 1)
duplicated_terms2

prime3 <- prime2 %>% select(-TERM) %>% as.matrix()
rownames(prime3) <- prime2 %>% select(TERM) %>% pull()

library(tm)
prime3 <- prime3 %>% t() %>% as.DocumentTermMatrix(weighting = weightTf)

library(stm)
prime_dfm <- readCorpus(prime3, type = "slam")


## 後で再利用することを考えオブジェクトを保存しておく
## save(prime_dfm, file = "prime_dfm.Rdata");
### load("prime_dfm.Rdata") #でオブジェクトを再現できる


## 年代を表す列を追加する
prime_year <- prime_dfm[["documents"]] %>% names() %>% str_sub(1,4)
prime_dfm$meta <- list(Year = as.numeric(prime_year))

prime_selectedK <- searchK(documents = prime_dfm$documents, 
                           vocab = prime_dfm$vocab, K = 3:10,  
                           data = prime_dfm$meta)

save(prime_selectedK, file = "prime_selectedK.Rdata")

prime_selectedK$results %>% select(K, lbound,residual, semcoh, heldout) %>%
                        gather(Metric, Value, -K) %>%
                          ggplot(aes(K, Value, color = Metric)) +
                            geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
                            facet_wrap(~Metric, scales = "free_y") 
dev.off()

prime_cov <- stm(documents = prime_dfm$documents, 
                  vocab = prime_dfm$vocab, K = 4, prevalence =~ Year,
                  data = prime_dfm$meta, verbose = FALSE)



summary(prime_cov)

## 後で再利用することを考えオブジェクトを保存しておく
## save(prime_cov, file = "prime_cov.Rdata");
### load("prime_cov.Rdata") #でオブジェクトを再現できる


prime_topic_estimate <- estimateEffect(formula = 1:4 ~ Year, 
                                         stmobj = prime_cov, 
                                         metadata = prime_dfm$meta)

summary(prime_topic_estimate)


plot(prime_topic_estimate, "Year", method = "continuous",
     topics = 1:4, model = prime_cov)

dev.off()

prime_cov2 <- stm(documents = prime_dfm$documents, 
                  vocab = prime_dfm$vocab, K = 4, prevalence =~ s(Year, 4),
                  data = prime_dfm$meta,  verbose = FALSE)



summary(prime_cov2)


## 後で再利用することを考えオブジェクトを保存しておく
## save(prime_cov2, file = "prime_cov2.Rdata");
### load("prime_cov2.Rdata") #でオブジェクトを再現できる


prime_topic_estimate2 <- estimateEffect(formula = 1:4 ~ s(Year, 4), 
                                         stmobj = prime_cov2, 
                                         metadata = prime_dfm$meta)

summary(prime_topic_estimate2)


plot(prime_topic_estimate2, "Year", method = "continuous",
      topics = 1:4, model = prime_cov)

dev.off()



prime2 %>% colnames()
prime3 <- prime2[, c(1,69:82)]
prime3 %>% colnames()

prime3 <- prime3 %>% mutate(N = prime3 %>% select(-TERM) %>% rowSums(.)) %>% filter(N > 0)
prime3_party <- c("LDP","LDP","LDP","LDP","LDP","LDP", "DPJ","DPJ","DPJ","DPJ","DPJ","DPJ", "LDP","LDP")


prime3_terms <- prime3 %>% select(TERM) %>% pull()
prime3 <- prime3 %>% select(-c(TERM, N)) %>% as.matrix()


rownames(prime3) <- prime3_terms



library(tm)
prime3 <- prime3 %>% t() %>% as.DocumentTermMatrix(weighting = weightTf)


## 後で再利用することを考えオブジェクトを保存しておく
## save(prime3, file = "prime3.Rdata");
### load("prime3.Rdata") #でオブジェクトを再現できる




library(stm)
prime3_dfm <- readCorpus(prime3, type = "slam")


prime3_dfm$meta <- list(Party = prime3_party)


prime3_cov <- stm(documents = prime3_dfm$documents, 
                  vocab = prime3_dfm$vocab, K = 4,
                  content =~ Party,
                  init.type = "Spectral", seed = 123, 
                  data = prime3_dfm$meta, verbose = FALSE)


summary(prime3_cov)

## 後で再利用することを考えオブジェクトを保存しておく
## save(prime3_cov, file = "prime3_cov.Rdata");
### load("prime3_cov.Rdata") #でオブジェクトを再現できる


plot(prime3_cov, type = "perspective", topics = 2)


#############
source("http://rmecab.jp/R/Aozora.R")
kokoro <- Aozora("https://www.aozora.gr.jp/cards/000148/files/773_ruby_5968.zip")


kokoro_txt <- readLines(kokoro)[-c(1:3)]

kokoro_txt %>% head()
kokoro_df <- tibble(text = kokoro_txt[kokoro_txt != ""])

pat1 <- "^[上中下]\\W*"
# pat1 <- "$\\s*[上中下]\\s*"
# pat <- "$[上中下]"
kokoro_df <- kokoro_df %>% mutate(flag1 = if_else(str_detect(text, pat1), 
                                                  TRUE, FALSE))
kokoro_df %>% select(flag1) %>% table()

kokoro_df <- kokoro_df %>% mutate(part = cumsum(flag1))

kokoro_df %>% select(part) %>% pull() %>% table()

pat2 <- "^\\s*[一二三四五六七八九十]{1,3}\\s*$"
kokoro_df <- kokoro_df %>% mutate(flag2 = ifelse(str_detect(text, pat2), TRUE, FALSE))

kokoro_df %>% select(flag2) %>% table()

kokoro_df <- kokoro_df %>% mutate(section = cumsum(flag2))

## 後で再利用することを考えオブジェクトを保存しておく
## save(kokoro_df, file = "kokoro_df.Rdata");
### load("kokoro_df.Rdata") #でオブジェクトを再現できる

kokoro_df2 <- kokoro_df %>% mutate(id = sprintf("Part%s_Section%0.3d", 
                                                 part, section))
kokoro_df2 %>% select(part, section, id) %>% tail()
kokoro_df2 %>% select(part) %>% table()

X <- c(1, 1, 0, 0)
Y <- c(1, 0, 1, 0)
xor(X, Y)

kokoro_df2 <- kokoro_df2 %>% filter(!xor(flag1, flag2)) %>% 
                select(-c(flag1, flag2, part, section))

kokoro_df2 <- kokoro_df2 %>% group_by(id) %>% 
                summarize(text = paste(text, collapse = ""))
kokoro_df2 %>% NROW()


kokoro_df2$text[1] %>% Encoding()# unknown on Windows; UTF-8 on Mac

if( (.Platform$OS.type == "windows") & any(unique(Encoding(kokoro_df2$text)) %in% "UTF-8")) {
  kokoro_df2 <- kokoro_df2 %>% mutate(text = iconv(text, 
                                                     from = "UTF-8", 
                                                     to = "CP932",
                                                       sub = ""))} else{
                                                       }


# kokoro_df2 %>%  write.csv(file = "kokoro_df2.csv", quote = FALSE, 
#                           row.names = FALSE)
# 


## 後で再利用することを考えオブジェクトを保存しておく
## save(kokoro_df2, file = "kokoro_df2.Rdata");
### load("kokoro_df2.Rdata") #でオブジェクトを再現できる


library(RMeCab)

gc(); gc()

kokoro_tdm <- docMatrixDF(kokoro_df2$text, pos = c("名詞","形容詞","動詞"))
kokoro_dtm <- t(kokoro_tdm)
kokoro_dtm %>% dim()

## 節ごとに形態素解析にかけて結果をデータフレームにする処理
rmecabcK <- function(id, text){
     x <- unlist(RMeCabC(text, 1))
     x  <- x[x == "Ｋ" | names(x) %in% c("名詞", "動詞","形容詞")] 
     tibble(ID = id, TERM = x)
}
gc(); gc()
kokoro_mecab <- map2_dfr(kokoro_df2$id, kokoro_df2$text,
                         ~ rmecabcK(..1, ..2))


kokoro_mecab

kokoro_count <- kokoro_mecab %>% count(ID, TERM)
kokoro_count %>% arrange(n) %>% tail(10)

stop_words  <- read_tsv(
   "http://svn.sourceforge.jp/svnroot/slothlib/CSharp/Version1/SlothLib/NLP/Filter/StopWord/word/Japanese.txt",
                           col_names = "TERM")
ja_stop_words  <- stop_words %>% 
      add_row(TERM = c("ある", "する", "てる", "いる", "の", "いう", "しまう",
                       "なる", "おる", "ん", "の", "れる"))


kokoro_nostopwords <- kokoro_count %>% anti_join(ja_stop_words)
kokoro_nostopwords %>% head()

library(tidytext)
kokoro_dtm <- kokoro_nostopwords %>% 
                cast_dtm(document = "ID", term = "TERM", value = "n")

kokoro_dtm %>% dim()


## 後で再利用することを考えオブジェクトを保存しておく
## save(kokoro_dtm, file = "kokoro_dtm.Rdata");
### load("kokoro_dtm.Rdata") #でオブジェクトを再現できる

# library(tm) kokoro_dtm %>% findFreqTerms(5)

# kokoro_dtm <- tm::removeSparseTerms(kokoro_dtm, sparse = 0.33)
# kokoro_dtm %>% dim()

library(topicmodels)

set.seed(123)
kokoro_topics <- LDA(kokoro_dtm, k = 9, seed = 123)


## 後で再利用することを考えオブジェクトを保存しておく
## save(kokoro_topics, file = "kokoro_topics.Rdata");
### load("kokoro_topics.Rdata") #でオブジェクトを再現できる

library(broom)
topics <- kokoro_topics %>% tidy() 
head(topics)

topics20 <- topics %>% group_by(topic) %>% top_n(20,beta) %>%
                 ungroup() %>% mutate(term = reorder(term, beta)) %>% 
                   arrange(topic,-beta)


library(ggplot2)
topics20 <- topics %>% group_by(topic) %>% top_n(20,beta) %>%
                 ungroup() %>% mutate(term = reorder(term, beta)) %>% 
                   arrange(topic,-beta)

#
cairo_pdf("p47.pdf", width = 7, height = 7)# for Windows Users # cairo_pdf("p47.pdf", width = 7, height = 7, family = "Meiryo")
topics20 %>% mutate(term = reorder(term, beta)) %>% 
               group_by(topic,term) %>% arrange(desc(beta)) %>% 
                 ungroup() %>% mutate(term = factor(paste(term, topic, sep = "_"),
                                      levels = rev(paste(term, topic, sep = "_")))) %>% 
                   ggplot(aes(term, beta, fill = beta)) + geom_bar(stat = "identity") +
                     facet_wrap(~topic, scales = "free") + coord_flip()
# ggsave(tmp, filename = "p47.pdf")
dev.off()

## install.packages("devtools")
## devtools::install_github("dgrtwo/drlib")

## topics20 %>% mutate(term = reorder(term, beta)) %>% 
##                group_by(topic,term) %>% arrange(desc(beta)) %>% 
##                  ungroup() %>% mutate(term = 
##                                        drlib::reorder_within(term, beta, topic)) %>% 
##                    ggplot(aes(term, beta, fill = beta)) + geom_bar(stat = "identity") +
##                      drlib::scale_x_reordered() + facet_wrap(~topic, scales = "free") + 
##                      coord_flip()


## section 3.9

library(ldatuning)

findK <- FindTopicsNumber(
  kokoro_dtm,
  topics = seq(from = 2, to = 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)



findK %>% head()

FindTopicsNumber_plot(findK)

library(stm)
kokoro_dfm <- readCorpus(kokoro_dtm, type = "slam")

kokoro_dfm$documents %>% names() %>% head()

Part <- kokoro_dfm$documents %>% names() %>% str_sub(1,5)
Time <- seq_along(Part)
kokoro_dfm$meta <- data.frame(Part, Time)

## 後で再利用することを考えオブジェクトを保存しておく
## save(kokoro_dfm, file = "kokoro_dfm.Rdata");
### load("kokoro_dfm.Rdata") #でオブジェクトを再現できる

kokoro_cov0 <- stm(documents = kokoro_dfm$documents, 
                   vocab = kokoro_dfm$vocab, K = 3, prevalence =~ Part + Time,
                   data = kokoro_dfm$meta,  verbose = FALSE)



topic_estimate_effect0 <- estimateEffect(formula = 1:3 ~ Part + Time,
                                          stmobj = kokoro_cov0,
                                          metadata = kokoro_dfm$meta)

summary(topic_estimate_effect0)


plot(topic_estimate_effect0, "Time", method = "continuous",
     topics = 1:3, model = kokoro_cov0)


kokoro_cov1 <- stm(documents = kokoro_dfm$documents, 
                    vocab = kokoro_dfm$vocab, K = 3, 
                    prevalence =~ Part + s(Time, 4),
                    data = kokoro_dfm$meta, verbose = FALSE)



summary(kokoro_cov1)


## 後で再利用することを考えオブジェクトを保存しておく
## save(kokoro_cov1, file = "kokoro_cov1.Rdata");
### load("kokoro_cov1.Rdata") #でオブジェクトを再現できる

topic1 <- findThoughts(kokoro_cov1, texts = str_sub(kokoro_df2$text, 1,160), 
                        n = 2, topics = 1)
plot(topic1)



topic_estimate_effect1 <- estimateEffect(formula = 1:3 ~ Part + s(Time, 4), 
                                          stmobj = kokoro_cov1, 
                                          metadata = kokoro_dfm$meta)

plot(topic_estimate_effect1, "Time", method = "continuous", 
      topics = 1:3, model = kokoro_cov1)
## dev.off()


