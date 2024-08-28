########################################################
# Ativar os Pacotes 
library(likert)
library(readxl)
library(dplyr)
library(plyr)
library(plotly)
library(ggplot2)
#library(knitr)
library(table1)
library(flextable)
library(RColorBrewer)
library(rstatix)
library(haven)
#########################################################


#########################################################
# Definir Diretório de Trabalho

setwd("C:/Users/mario.valente/Documents/Github2/Pesquisa_Clima_Organizacional/Escala_Likert")
#########################################################


#########################################################
# Leitura de Base de Dados
Dados_Clima <- read_excel("Dados_Clima.xls")
Dados_Motociclista <- read_excel("Dados_motociclista.xls")
Dados_Motorista <- read_excel("Dados_motorista.xls")

teste_spps <- read_spss("CLIMA_ORGANIZACIONAL.sav")


Dados_Clima[,1:9] <- lapply(Dados_Clima[,1:9], 
                            factor, 
                            levels=1:5,
                            labels = c("Sempre", 
                                       "Quase Sempre", 
                                       "Raramente", 
                                       "Nunca", 
                                       "Não Tenho Opnião"),
                            order = TRUE)




Dados_Motociclista[,1:6] <- lapply(Dados_Motociclista[,1:6], 
                            factor, 
                            levels=1:3,
                            labels = c("Sim", 
                                       "Não",
                                       "Não Informado"),
                            order = TRUE)
###################################################################



###################################################################
#Definir Tipos de variáveis

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), 
       c("", "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), 
               function(y) with(y,
  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}
################################################################################




###################################################################
# Função Calcular P-valor das variáveis:contínuas/categóricas.


pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}
###################################################################




###################################################################
#Tabela dos Itens 

caption  <- "Pesquisa Comportamento do Motociclista"
footnote <- "Fonte: CNP/DETRAN-PA"




table1(~., 
       data = Dados_Clima,
       #ctable = TRUE,
       overall = "n(%)",
       #overall = F,
       #decimal.mark = ",",
       caption = caption, 
       footnote = footnote,
       #topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       topclass = "Rtable1-zebra",
       #render.continuous=my.render.cont,
       #render.categorical=my.render.cat
       #extra.col=list(`P-value`=pvalue)
)

table1(~., 
       data = Dados_Motociclista,
       #ctable = TRUE,
       overall = "n(%)",
       #overall = F,
       #decimal.mark = ",",
       caption = caption, 
       footnote = footnote,
       #topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       topclass = "Rtable1-zebra",
       #render.continuous=my.render.cont,
       #render.categorical=my.render.cat
       #extra.col=list(`P-value`=pvalue)
)


################################################################################


###################################################################
# Nomear as Categorias Likert
nomes <- read_excel("Dados_Clima.xls", sheet = 3)
colnames(Dados_Clima)[1:9] <- nomes$Nomes
table1(~., data = Dados_Clima, overall = "n(%)", decimal.mark = ",")


nomes <- read_excel("Dados_Motociclista.xls", sheet = 3)
colnames(Dados_Motociclista)[1:6] <- nomes$Nomes
table1(~., data = Dados_Motociclista, overall = "n(%)", decimal.mark = ",")





###################################################################
# Gerar Plot Likert
dados_grafico <- likert(as.data.frame(Dados_Clima[1:9]))
dados_grafico2 <- likert(as.data.frame(Dados_Motociclista[1:6]))


paleta <- brewer.pal(5, "RdBu")
paleta[3] <- "#DFDFDF"


g1 <- likert.bar.plot(dados_grafico, text.size=4)+
  theme(axis.text.y=element_text(size="12"))+
  labs(x="", y = "Frequência (%)", size=12)+
  ggtitle("Pesquisa de Comportamento do Motociclista")+
  scale_fill_manual(values = paleta,
                    breaks = levels(Dados_Clima$`Orientações que Vc Recebe sobre o seu Trabalho são Claras/Objetivas?`))+
  guides(fill = guide_legend(title = "Resposta"))+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white"))
ggplotly(g1)




grafico1 <- likert.bar.plot(dados_grafico2, text.size=4)+
  theme(axis.text.y=element_text(size="12"))+
  labs(x="", y = "Frequência (%)", size=12)+
  ggtitle("Pesquisa de Comportamento do Motociclista")+
  scale_fill_manual(values = paleta,
                    breaks = levels(Dados_Motociclista$`Para Conduzir Motocicleta Você Precisa Ser Habilitado?`))+
  guides(fill = guide_legend(title = "Resposta"))+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white"))
ggplotly(grafico1)
################################################################################



################################################################################
# Gerar Plot Likert p/ Grupos
dados_grafico_grupo <- likert(as.data.frame(Dados_Clima[1:9]),
                              grouping = Dados_Clima$GENERO)


g2 <- likert.bar.plot(dados_grafico_grupo, text.size=4)+
  theme(axis.text.y=element_text(size="12"))+
  labs(x="", y="Frequencia (%)", size=12)+
  ggtitle("Pesquisa de Clima Organizacional")+
  scale_fill_manual(values = paleta,
                    breaks = levels(Dados_Clima$`Orientações que Vc Recebe sobre o seu Trabalho são Claras/Objetivas?`))+
  guides(fill = guide_legend(title = "Resposta"))+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white"))
ggplotly(g2)



# Gerar Plot Likert p/ Grupos
dados_grafico2_grupo <- likert(as.data.frame(Dados_Motociclista[1:6]),
                              grouping = Dados_Motociclista$GENERO)


grafico2 <- likert.bar.plot(dados_grafico2_grupo, text.size=4)+
  theme(axis.text.y=element_text(size="12"))+
  labs(x="", y="Frequencia (%)", size=12)+
  ggtitle("Pesquisa de Comportamento do Motociclista")+
  scale_fill_manual(values = paleta,
                    breaks = levels(Dados_Motociclista$`Para Conduzir Motocicleta Você Precisa Ser Habilitado?`))+
  guides(fill = guide_legend(title = "Resposta"))+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white"))
ggplotly(grafico2)

################################################################################




















