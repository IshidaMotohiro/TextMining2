
# 石田基広『実践 Rによるテキストマイニング』森北出版 スクリプト vers.1.0
# 第 2 章
## section 2.1  

## 辞書のダウンロード

dic <- read.table("http://www.lr.pi.titech.ac.jp/~takamura/pubs/pn_ja.dic", 
   sep = ":", stringsAsFactors = FALSE, fileEncoding = "CP932"
   # , encoding = "UTF-8"# Windowsでこの行は不要なので行頭に半角#を追記すること
)



library(tidyverse)
## 冒頭15行を確認
dic %>% head(15)

dic %>% group_by(V1) %>% filter(n() > 1) %>% arrange(desc(V1))

dic  %>% arrange(desc(V1))  %>% group_by(V1) %>% filter(n() > 1) %>% 
           summarize(Diff = (max(V4) - min(V4))) %>% arrange(Diff) %>% 
             tail()

dic  %>% filter(V1 == "大人")

dic <- dic %>% arrange(V1) %>% group_by(V1) %>% 
                 summarize(SCORE = mean(V4)) %>% 
                   select(TERM = V1, SCORE)

chumon[43]

library(stringr)
## 全角スペースを削除し文章単位に分割する。
sent_chumon <- chumon %>% str_remove_all("　") %>% 
                            str_split(pattern = "(?<=。)") %>% 
                              unlist()
## なおstr_remove()などを利用すると
## 文字コードは強制的にUTF-8に変換されている
## そのため、Windowsでは後でCP932に戻す必要がある
## カギ括弧も削除
sent_chumon <- sent_chumon %>% str_remove_all("「|」")
## 空のレコード("")が残っていればこれを削除する
sent_chumon <- sent_chumon[sent_chumon != ""] 
## 結果を確認
sent_chumon %>% head(3)

## 文章番号を振っておく
sent_chumon <- tibble(S = sent_chumon) %>% mutate(ID = row_number())


## Windowsでは文字コードがUTF-8に変換されているのでCP932に戻す

## for windows users
if( (.Platform$OS.type == "windows") & Encoding(sent_chumon$S[1]) == "UTF-8"){
  sent_chumon <- sent_chumon %>% mutate(S = iconv(S, from = "UTF-8",
                                                  to = "CP932", sub = ""))
} else{
}



sent_chumon$S[1] %>% Encoding() # unknown on Windows; UTF-8 on Mac

## 最長の文と最短の文
sent_chumon %>% mutate(N = nchar(S))  %>% 
                  filter(N == max(N) | N == min(N))

library(RMeCab)
library(purrr) 
## 文ごとに形態素解析にかけて結果をデータフレームにする処理
rmecabc <- function(id, sent){
     x <- unlist(RMeCabC(sent, 1))
     tibble(ID = id, TERM = x)
}

gc();gc()
terms_chumon <- map2_dfr(sent_chumon$ID, sent_chumon$S,
                          ~ rmecabc(..1, ..2)
)
## 単語と出現した文章番号がペアになったデータが生成される
terms_chumon



terms_chumon <- terms_chumon %>% left_join(dic)

em_chumon <- terms_chumon %>% group_by(ID) %>% 
               summarise(EM = sum(SCORE, na.rm = TRUE))

em_chumon %>% select(EM) %>% summary()

em_chumon %>% filter(EM == min(EM)) %>% left_join(sent_chumon) %>%
                select(S) %>% pull()


em_chumon %>% ggplot(aes(x = ID, y = EM)) + geom_line() 
dev.off()

em_chumon %>% filter(EM < -5) %>% left_join(sent_chumon) %>% select(S) %>% pull()


library(scales)
em_chumon %>% left_join(sent_chumon %>% mutate(L = nchar(S)) ) %>% 
                ggplot() + geom_line(aes(ID, EM * 5), colour = "red") +
                           geom_line(aes(ID, L), colour = "blue") +
                           scale_y_continuous(name = "文長", 
                                              sec.axis = sec_axis(~ . /5, 
                                                         name = "感情極性値"))
dev.off()



