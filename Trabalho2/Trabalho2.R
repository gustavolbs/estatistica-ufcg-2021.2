## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl) # Para gerar .R
# Outra forma para Extrair Chunk do RMarkdown:
#https://felixfan.github.io/extract-r-code/

knitr::opts_chunk$set(echo = TRUE)

## ----instala_e_ou_carrega_pacotes_e_instancia_dados, echo=FALSE, warning=FALSE----
library(ggplot2)
teste2 <- readxl::read_excel("C:\\Users\\diego\\Desktop\\teste.xlsx", sheet = 1)

teste2


## ----box-plot_disponibilidade-------------------------------------------------
ggplot(data = teste2, mapping = aes(x = ebookStudyAvaliable)) +
  geom_histogram(color = "white") +
  facet_wrap(~ ebookReadFrequency, ncol = 1) +
  labs(x="Disponibilidade de ebooks para estudo")

## ----box-plot_correlacao------------------------------------------------------

ebookStudyAvaliable = teste2$ebookStudyAvaliable
ebookReadFrequency = teste2$ebookReadFrequency

ebookAvaliableAux = c()
ebookFreqAux = c()

for(i in 1:length(ebookReadFrequency)){
  value <- 0
  indexFreq <- ebookReadFrequency[i]
  avaliableIndex <- ebookStudyAvaliable[i]
  if(avaliableIndex > 2){
    if(indexFreq == "Raramente"){
      value = 3
    }else if(indexFreq == "1 a 3 vezes"){
      value <- 4
    }else if(indexFreq  == "Mais de 3 vezes"){
       value <- 5
    }
    if(value > 0){
        ebookAvaliableAux <- c(ebookAvaliableAux, avaliableIndex)
        ebookFreqAux <- c(ebookFreqAux, value)
    }
  }

}
cor.test(ebookFreqAux,ebookAvaliableAux, method = "spearman", exact=FALSE) 

