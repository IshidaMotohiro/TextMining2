
# 石田基広『実践 Rによるテキストマイニング』森北出版 スクリプト vers.1.0

# 第 1 章
## section 1.1 
source("http://rmecab.jp/R/Aozora.R")# 青空文庫ダウンロード解析用機能の取得
kenji <- Aozora("https://www.aozora.gr.jp/cards/000081/files/43754_ruby_17594.zip")

kenji
# [1] "NORUBY/chumonno_oi_ryoriten2.txt"


library(tidyverse)

chumon <- readLines(kenji)
chumon %>% head()

chumon <- chumon[-(1:2)] 
# 必要があればテキストファイルとして保存しておく
chumon %>% writeLines(kenji)


### chapter 1 section 1.2

library(RMeCab)
gc(); gc()
chumon_df <- docDF(kenji, type = 1, pos = c("名詞","形容詞","動詞"))


chumon_df %>% arrange(chumonno_oi_ryoriten2.txt) %>% tail()

chumon_df %>% filter(TERM == "二人")

### 「二」と「人」を「二人」に統合する例
### まず、「二」と「人」それぞれの出現回数を確認する。
###  ここで str_detect() は stringr パッケージの関数であり、指定した列を検索する。

  chumon_df %>% filter(str_detect(TERM, "二|人"))
  ### TERM   POS1   POS2     chumonno_oi_ryoriten2.txt
  ### 1 主人    名詞      一般                                             1
  ### 2   二     名詞          数                                           31
  ### 3 二つ   名詞       一般                                             2
  ### 4   人    名詞       一般                                             3
  ### 5   人    名詞       接尾                                            27

### 次に、「二人」が出現した回数を確認しなければならないが、
### これにはバイグラムを使う（詳細は 1.4 節で解説する）。

  chumon_df2 <- docDF(kenji, type = 1, N = 2)
  chumon_df2 %>% filter(TERM == "二-人" )
### TERM           POS1    POS2   chumonno_oi_ryoriten2.txt
### 1  二-人    名詞-名詞   数-接尾                                          25


### 「二人」は25回出現していることがわかる。
### そこで、別々にカウントされている「二」と「人」それぞれの頻度から、
### 「二人」として結合した分を差し引く。
### すなわち、品詞細分類が「数（詞）」の「二」の出現回数31回と、
### 細分類が「接尾」の「人」の27回のそれぞれから25を引く。
### データフレームの中身を変更する場合、dplyrでは mutate()を利用する。

  chumon_df2 <- chumon_df %>% mutate(across(chumonno_oi_ryoriten2.txt, 
                                           ~ifelse((TERM == "二" & POS2 == "数" |
                                                     TERM == "人" & POS2 == "接尾"), 
                                                   chumonno_oi_ryoriten2.txt -25,
                                                   chumonno_oi_ryoriten2.txt)))
chumon_df2 %>% filter(TERM == "二" & POS2 == "数" |
                        TERM == "人" & POS2 == "接尾")
### 最後に add_case() で「二人」を新規のデータとして追加する。

  chumon_df2 <- chumon_df2 %>% add_case(TERM = "二人",
                                      POS1 = "名詞",
                                      POS2 = "数詞",
                                    chumonno_oi_ryoriten2.txt = 25)
chumon_df2
### ただし、「二人」以外に出現する「二」や「人」はそのまま残すのは不自然であるともいえる。
### 個々の単語を結合する場合、十分な注意が必要である。

## section 1.3 
  
library(wordcloud2)
chumon_df %>% arrange(chumonno_oi_ryoriten2.txt) %>% tail(100) %>% 
               select(TERM, chumonno_oi_ryoriten2.txt) %>% wordcloud2()

chumon_words100 <- chumon_df %>% arrange(chumonno_oi_ryoriten2.txt) %>% 
                     tail(100) %>% select(TERM, FREQ = chumonno_oi_ryoriten2.txt)

### Macでプロットに日本語を表示させるには環境設定が必要
### 以下を実行してから、R/RStudioを再起動させると良い
### source("http://rmecab.jp/R/Rprofile.R")

chumon_words100 %>% wordcloud2()

chumon_df %>% select(POS2) %>% distinct()

chumon_df %>% filter(POS2 == "数")

chumon_df %>% filter(POS2 != "数" | TERM == "二") %>% filter(POS2 == "数")
## 結果を確認したうえでオブジェクトを上書き
chumon_df <- chumon_df %>% filter(POS2 != "数" | TERM == "二") 

chumon_df %>% filter(TERM %in% c("ある", "いる"))

(docDF(data.frame(X = "看板にそう書いてあるじゃないか"), "X", type = 1))

(docDF(data.frame(X = "こんどはこんないいこともある"), "X", type = 1))

chumon_words100 %>% filter(!TERM %in% c("ある","いる")) %>%
                      wordcloud2()


## section 1.4 

library(RMeCab)
gc(); gc()
bigram <- docDF(kenji, type = 1, N = 2, nDF = TRUE, 
                    pos = c("名詞", "形容詞", "動詞"))
bigram <- bigram %>% set_names(c("N1", "N2", "POS1", "POS2", "FREQ"))
## FREQ 列の降順にして最後の 6 行を表示
bigram  %>% arrange(FREQ) %>% tail()


remove_words <- c("ある", "いる", "の", "よう", "ん", 
                  "する", "くる", "なか", "こと")
bigram2 <- bigram %>% filter(!N1 %in% remove_words, 
                             !N2 %in% remove_words, FREQ > 1)

library(igraph)
library(ggraph)
bi_net  <- graph_from_data_frame(bigram2)
ggraph(bi_net, layout = "graphopt") + geom_edge_diagonal(alpha = 1, 
                                        label_colour = "blue") +
                                      geom_node_label(aes(label = name), 
                                        size = 5, repel = TRUE, max.overlaps =100)



## サイズの大きいテキスト集合の解析
## あらかじめ、必要そうなメモリを確保しておくと
## クラッシュする可能性が減ります


df <- data.frame (TERM=rep(NA,10000), POS1=rep(NA,10000), POS2 = rep(NA,10000), test.txt= rep(NA,10000) )
df <- docDF("test.txt", type = 1, pos = c("名詞","形容詞","動詞"))
