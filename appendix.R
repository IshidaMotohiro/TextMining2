

# 石田基広『実践 Rによるテキストマイニング』森北出版 スクリプト vers.1.0
## 補遺 A.1


stringi:::stri_split_boundaries("すもももももももものうち", type = "word")



## 補遺 A.2

library(udpipe)
udmodel <- udpipe_download_model(language = "japanese")
udmodel <- udpipe_load_model(file = udmodel$file_model)


x <- udpipe_annotate(udmodel, 
                     x = "本を読んだ。")

if(.Platform$OS.type == "windows") {
  txt <- iconv("本を読んだ。",  to = "UTF-8")
  x <- udpipe_annotate(udmodel, x = txt)
}


library(tidyverse)

x <- as_tibble(x)
x

x <- udpipe(x = c("本を読んだ。\n\n読み終えました。", 
                  "ご飯を食べます。今日は和食です。"), 
            object = "japanese")


if(.Platform$OS.type == "windows") {
  txts <- iconv(c("本を読んだ。\n\n読み終えました。", 
                "ご飯を食べます。今日は和食です。"),  to = "UTF-8")
  x <- udpipe_annotate(udmodel, x = txts)
}

x <- as_tibble(x)
x

x %>% select(doc_id, paragraph_id,  sentence_id, 
             token, lemma, upos) %>% head()



source("http://rmecab.jp/R/Aozora.R")
dazai <- Aozora("https://www.aozora.gr.jp/cards/000035/files/1567_ruby_4948.zip")


merosu <- dazai %>% readLines() %>% udpipe(object="japanese") %>% 
                      as_tibble()


if(.Platform$OS.type == "windows") {
   merosu <- dazai %>% readLines() %>% 
     iconv(from = "CP932", to = "UTF-8", sub = "") %>% 
     udpipe(object="japanese") %>% as_tibble()
  
}

merosu

merosu %>% summarize(Paragraph_size = max(paragraph_id), 
                     Sentece_size = max(sentence_id) )

merosu %>% filter(upos %in% c("NOUN", "VERB", "ADJ")) %>% 
             select(lemma) %>% pull() %>% txt_freq() %>% head()

merosu_co <- cooccurrence(x = subset(merosu, upos %in% c("NOUN")), 
                     term = "lemma", 
                     group = c("doc_id"))
merosu_co %>% head()

library(igraph)
library(ggraph)
merosu_netw <- merosu_co %>% head(50) 
merosu_netw <- graph_from_data_frame(merosu_netw)
merosu_netw %>% ggraph(layout = "fr") +
  geom_edge_link( edge_colour = "green") +
  geom_node_text(aes(label = name), col = "blue", size = 5) +
  theme(legend.position = "none") +
  labs(title = "『走れメロス』")


## 補遺 A.3


if(.Platform$OS.type == "windows" & any(unique(Encoding(kokoro_df2$text)) %in% "unknown")) {
  kokoro_df2 <- kokoro_df2 %>% mutate(text = iconv(text, from = "CP932",
                                                   to = "UTF-8", sub = ""))
}


kokoro_df3 <- udpipe(kokoro_df2$text, object = "japanese")


kokoro_df3 <- kokoro_df3 %>% filter(upos %in% c("NOUN", "VERB", "ADJ"))

kokoro_df4 <- document_term_frequencies(kokoro_df3, 
                                        document = "doc_id", 
                                        term = "lemma")
kokoro_df4 %>% head()

kokoro_df4 <- kokoro_df4 %>% anti_join(ja_stop_words, 
                                       by = c("term" = "TERM"))
kokoro_df4 %>% class
library(tidytext)
kokoro_dtm_ud <- cast_dtm(kokoro_df4, 
                          document = "doc_id", term = "term", 
                          value = "freq")

kokoro_dtm_ud %>% class()

# kokoro_dtm_ud <- dtm_remove_lowfreq(kokoro_dtm_ud, minfreq = 5)

library(topicmodels)
kokoro_m_ud <- LDA(kokoro_dtm_ud, k = 5, method = "Gibbs", 
         control = list(burnin = 2000, best = TRUE, seed = 123))


library(broom)
topics <- kokoro_m_ud %>% tidy() 
library(ggplot2)
topics20 <- topics %>% group_by(topic) %>% top_n(20,beta) %>%
                 ungroup() %>% mutate(term = reorder(term, beta)) %>% 
                   arrange(topic,-beta)

topics20 %>% mutate(term = reorder(term, beta)) %>% 
               group_by(topic,term) %>% arrange(desc(beta)) %>% 
                 ungroup() %>% mutate(term = factor(paste(term, topic, sep="_"),
                                       levels=rev(paste(term, topic, sep = "_")))) %>% 
              ggplot(aes(term, beta, fill = beta)) + geom_bar(stat = "identity")+
                facet_wrap(~topic, scales = "free") + coord_flip()
dev.off()



## 補遺 A.4

library(udpipe)
chumon2 <- readLines("NORUBY/chumonno_oi_ryoriten2.txt") %>% 
             udpipe(object = "japanese")


if(.Platform$OS.type == "windows") {
  chumon2 <- readLines("NORUBY/chumonno_oi_ryoriten2.txt") %>% iconv(from = "CP932",
                                                                     to = "UTF-8",
                                                                     sub = "") %>% 
    udpipe(object = "japanese")
}

chumon2 %>% slice(60:100) %>% 
              select(doc_id, paragraph_id, sentence_id, lemma, upos, sentence)

chumon2$textrank_id <- unique_identifier(chumon2, c("doc_id", 
                                                    "paragraph_id", 
                                                    "sentence_id"))
sentences <- chumon2 %>% select(textrank_id, sentence) %>% distinct()
terminology <- chumon2 %>% filter(upos %in% c("NOUN", "ADJ", "VERB")) %>% 
                 select(textrank_id, lemma)
terminology %>% head()

library(textrank)
txtrank <- textrank_sentences(data = sentences, terminology = terminology)

summary(txtrank, n = 5)

kw <- textrank_keywords(chumon2$lemma,
                          relevant = chumon2$upos %in% c("NOUN", "VERB", "ADJ"))
kw$keywords %>% filter(ngram > 1 & freq > 1)

# 補遺 A.5 RMeCab* を活用する


library(tidyverse)

dummy <- c("私は真面目な学生です。", "彼女は数学専攻の学生です。", "彼らは物理学を専攻している。")
dir.create("dummy")
for (i in seq(dummy)) {
   td <- tempfile("tmp", tmpdir = "dummy")
   write(dummy[i] , file = td)
   ## 以下の 1 行は一時ファイルの存在を確認するだけ
   if (file.exists(td)) cat(td, "exists", "\n")
}
library (RMeCab)
gc(); gc()
x <- docDF("dummy", type = 1, pos = c("名詞"))
x %>% head()

df <- tibble(TEXT = dummy)
df

df2 <- docDF(df, "TEXT", type = 1, pos = c("名詞","形容詞"))
df2 %>% tail()

df3 <- docMatrixDF(df$TEXT,  pos = c("名詞","形容詞"))
df3 %>% tail()


## 補遺 A.5.2

files <- list.files("~/data/doc", full.names = TRUE, pattern = "txt")
library(tidyverse)
Z <- files %>% map_df(~{
                file_n <- .
                x <-RMeCabText(file_n)
                map2_df(x, file_n,  {
                  ~ if(.x[2] %in% c( "名詞", "動詞")  &
                                 !(.x[3] %in% c("固有名詞",  "数"))) 
                      tibble(F = file_n, X = .x[1], Y = .x[2])
})})

