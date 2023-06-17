###########SBS - Classes Sociais##########
#####Opções######
options(scipen = 999)

#####Base de Dados#####
library(readxl)
bd <- read_excel("bd_clivagens_classe_sbs.xlsx")

#######Recodificação de Variáveis#######
####Escalas - Variável Dependente####
library(tidyverse)
library(psych)
###Moral###
bd$P36 <- case_match(bd$P36, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)
bd$P37 <- case_match(bd$P37, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)

bd$P38 <- case_match(bd$P38, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)

bd$P39 <- case_match(bd$P39, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)

bd$P40 <- case_match(bd$P40, "Zero" ~ 0,
                             "Um" ~ 1,
                             "Dois" ~ 2,
                             "Três" ~ 3,
                             "Quatro" ~ 4,
                             "Cinco" ~ 5,
                             "Seis" ~ 6,
                             "Sete" ~ 7,
                             "Oito" ~ 8,
                             "Nove" ~ 9,
                             "Dez" ~ 10,
                             "Não Respondeu" ~ NA,
                             "Não sabe" ~ NA)

bd$P41 <- case_match(bd$P41, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)

bd$P42 <- case_match(bd$P42, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)

bd$P43 <- case_match(bd$P43, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)

bd$P44 <- case_match(bd$P44, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)

bd$P45 <- case_match(bd$P45, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)

bd$P46 <- case_match(bd$P46, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)

bd$P47 <- case_match(bd$P47, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)

bd$P48 <- case_match(bd$P48, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)

bd$P49 <- case_match(bd$P49, "Zero" ~ 0,
                     "Um" ~ 1,
                     "Dois" ~ 2,
                     "Três" ~ 3,
                     "Quatro" ~ 4,
                     "Cinco" ~ 5,
                     "Seis" ~ 6,
                     "Sete" ~ 7,
                     "Oito" ~ 8,
                     "Nove" ~ 9,
                     "Dez" ~ 10,
                     "Não Respondeu" ~ NA,
                     "Não sabe" ~ NA)

#Análise fatorial 
fabd <- bd[c("P36", "P37","P38", "P40", "P41", "P42", "P43", "P44", "P45",
             "P47", "P49")]

fa.parallel(fabd)

fa.r <- principal(fabd, nfactors = 3,n.obs = 1500)

print(fa.r$loadings, cutoff = 0.45)

alpha(fabd[c("P36", "P37", "P38", "P45", "P48")], cumulative = F)
alpha(fabd[c("P40", "P43", "P47")])
alpha(fabd[c("P41","P42", "P49")])

#####Recodificação de Variáveis####
######Estatismo########

bd$est <- bd$P36 + bd$P37 + bd$P38 + bd$P45
bd$est <- bd$est/40*100

######Valores MOrais######
bd$val <- bd$P40 +bd$P43+bd$P47
bd$val <- bd$val/30*100

######Punitivismo####
bd$pun <- bd$P41+bd$P42+bd$P49
bd$pun <- bd$pun/30*100

######Variáveis de Controle#######
###Renda###
bd$renda <- case_match(bd$P60, "ATÉ 1 SM (ATÉ R$ 1.212,00)" ~ 1,
                               "MAIS DE 1 A 2 SM (MAIS DE R$ 1.212,00 A R$ 2.424,00)" ~ 2,
                               "MAIS DE 2 A 5 SM (MAIS DE R$ 2.424,00 A R$ 6.060,00)" ~ 3,
                               "MAIS DE 5 A 10 SM (MAIS DE R$ 6.060,00 A R$ 12.120,00)" ~ 4,
                               "MAIS DE 10 A 20 SM (MAIS DE R$ 12.120,00 A R$ 24.240,00)" ~ 5, 
                               " MAIS DE 20 SM (MAIS DE R$ 24.240,00)" ~ 6)

###Escolaridade###
bd$esc <- case_match(bd$P12, "Até a 4a série do Ensino Fundamental (Antigo Primário)" ~ 1,
                             "Da 5a à 9a série do Ensino Fundamental (Antigo Ginásio)" ~ 2,
                             "Ensino Médio (Antigo Colegial) incompleto" ~ 3,
                             "Ensino Médio (Antigo Colegial) completo" ~ 3,
                             "Superior incompleto" ~ 4,
                             "Superior completo ou mais" ~ 4)

###Idade###
bd$Idade <- bd$P8

###Sexo###
bd$Sexo <- bd$P6

###religião###
bd$Relig <- case_match(bd$P13, 
                             "Católico" ~ "Católico",
                             "Espírita Kardecista" ~ "Outras",
                             "Evangélico Pentecostal ou Carismático (por exemplo: Igreja de Deus, Assembleia de Deus, Igreja Universal do Reino de De" ~ "Protestantes",
                             "Igreja de Jesus Cristo dos Santos dos Últimos Dias ou SUD (Mórmon)" ~ "Outras",
                             "Não respondeu" ~ NA,
                             "Nenhuma" ~ "Nenhuma",
                             "Outra religião" ~ "Outras",
                             "Protestante (por exemplo: Luterana, Presbiteriana, Calvinista, Metodista e Batista)" ~ "Protestantes",
                             "Religiões de matriz afro (Umbanda, Candomblé, Quimbanda)" ~ "Outras",
                             "Testemunha de Jeová" ~ "Outras")

table(bd$Relig)
bd$Relig <- factor(bd$Relig, 
                   levels = c("Nenhuma", "Outras", 
                              "Católico", "Protestantes"))

###Classe social###
bd$tip_classe_4c <- factor(bd$tip_classe_4c)
bd$tip_classe_4c <- relevel(bd$tip_classe_4c, ref = "Trabalhadores manuais e precarizados")

####Modelos####
bdm <- bd[c("Sexo", "Idade", "esc", "val", "pun", "est", "tip_classe_4c", "Relig")]
######Valores#####

summary(bdm$val)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00   16.67   33.33   40.71   66.67  100.00     145 

mod.v1 <- lm(val~tip_classe_4c, data = bdm)
mod.v2 <- lm(val~tip_classe_4c+Sexo+Idade+esc+Relig, data = bdm)

######Estatismo######
mod.e1 <- lm(est~tip_classe_4c, data = bdm)
mod.e2 <- lm(est~tip_classe_4c+Sexo+Idade+esc+Relig, data = bdm)

######Punitivismo######
mod.p1 <- lm(pun~tip_classe_4c, data = bdm)
mod.p2 <- lm(pun~tip_classe_4c+Sexo+Idade+esc+Relig, data = bdm)

save(bdm, file = "bdm.RData")

library(sjPlot)
tab_model(mod.v2, mod.e2, mod.p2, wrap.labels = 45, 
          show.ci = F, show.se = F,  p.style = "numeric")
########
#GRÁFICOS########
#Coef do modelo
#tenho que lembrar e colar minhas linhas da fonte

library(ggplot2)
library(broom.mixed)

#VALORES####
# Ajustar o modelo de regressão
mod.v2 <- lm(val ~ tip_classe_4c + Sexo + Idade + esc + Relig, data = bdm)

# Extrair os coeficientes e p-valores do modelo
coef_data <- tidy(mod.v2)

# Ordenar os coeficientes pela variável preditora
coef_data <- coef_data[order(coef_data$term), ]

# Criar um dataframe para os níveis de significância
significance <- data.frame(term = coef_data$term, signif = ifelse(coef_data$p.value < 0.05, "*", ""))

# Juntar os coeficientes e níveis de significância
coef_data <- merge(coef_data, significance, by = "term")

# Criar o gráfico de coeficientes com cores pastéis
Mod1 <- ggplot(coef_data, aes(x = term, y = estimate, fill = term)) +
  geom_bar(stat = "identity", width = 0.5, color = "black") +
  geom_text(aes(label = signif), vjust = -0.5, size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  # Aplicar a paleta de cores pastéis
  labs(x = "Variável preditora", y = "Coeficiente estimado",
  title = "Valores") +
  theme_minimal() +
  theme(legend.position = "none")


Fonte <-  theme(text = element_text(family = "serif", size = 12),
                title = element_text(color = "black"),
                axis.line = element_line(color = "black"), 
                axis.text = element_text(colour = "black", size = rel(0.7)),
                plot.background = element_rect(fill = "grey90", colour = "black", 
                                               linewidth = 1)) 
Gra1 <- Mod1 + Fonte


#ESTATISMO####
# Ajustar o modelo de regressão
mod.e2 <- lm(est~tip_classe_4c+Sexo+Idade+esc+Relig, data = bdm)

# Extrair os coeficientes e p-valores do modelo
coef_data <- tidy(mod.e2)

# Ordenar os coeficientes pela variável preditora
coef_data <- coef_data[order(coef_data$term), ]

# Criar um dataframe para os níveis de significância
significance <- data.frame(term = coef_data$term, signif = ifelse(coef_data$p.value < 0.05, "*", ""))

# Juntar os coeficientes e níveis de significância
coef_data <- merge(coef_data, significance, by = "term")

# Criar o gráfico de coeficientes com cores pastéis
Mod2 <- ggplot(coef_data, aes(x = term, y = estimate, fill = term)) +
  geom_bar(stat = "identity", width = 0.5, color = "black") +
  geom_text(aes(label = signif), vjust = -0.5, size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  # Aplicar a paleta de cores pastéis
  labs(x = "Variável preditora", y = "Coeficiente estimado",
       title = "Estatismo") +
  theme_minimal() +
  theme(legend.position = "none")


Fonte <-  theme(text = element_text(family = "serif", size = 12),
                title = element_text(color = "black"),
                axis.line = element_line(color = "black"), 
                axis.text = element_text(colour = "black", size = rel(0.7)),
                plot.background = element_rect(fill = "grey90", colour = "black", 
                                               linewidth = 1)) 
Gra2 <- Mod2 + Fonte


#Punitivismo####
# Ajustar o modelo de regressão
mod.p2 <- lm(pun~tip_classe_4c+Sexo+Idade+esc+Relig, data = bdm)

# Extrair os coeficientes e p-valores do modelo
coef_data <- tidy(mod.p2)

# Ordenar os coeficientes pela variável preditora
coef_data <- coef_data[order(coef_data$term), ]

# Criar um dataframe para os níveis de significância
significance <- data.frame(term = coef_data$term, signif = ifelse(coef_data$p.value < 0.05, "*", ""))

# Juntar os coeficientes e níveis de significância
coef_data <- merge(coef_data, significance, by = "term")

# Criar o gráfico de coeficientes com cores pastéis
Mod3 <- ggplot(coef_data, aes(x = term, y = estimate, fill = term)) +
  geom_bar(stat = "identity", width = 0.5, color = "black") +
  geom_text(aes(label = signif), vjust = -0.5, size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +  # Aplicar a paleta de cores pastéis
  labs(x = "Variável preditora", y = "Coeficiente estimado",
       title = "Punitivismo") +
  theme_minimal() +
  theme(legend.position = "none")


Fonte <-  theme(text = element_text(family = "serif", size = 12),
                title = element_text(color = "black"),
                axis.line = element_line(color = "black"), 
                axis.text = element_text(colour = "black", size = rel(0.7)),
                plot.background = element_rect(fill = "grey90", colour = "black", 
                                               linewidth = 1)) 
Gra3 <- Mod3 + Fonte



# Remover rótulos do eixo y, exceto no primeiro gráfico
Gra2 <- Gra2 + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
Gra3 <- Gra3 + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())


# Organizar os gráficos lado a lado
ggpubr::ggarrange(Gra1, Gra2, Gra3, ncol = 3)

plot_model(type = "pred", terms = c(Gra1, Gra2, Gra3))

###
#preditos
plot_model(mod.v2, type = "pred", 
           terms = c("Sexo", "Idade", 
                 "esc", "Relig"))

plot_model(mod.v2, type = "pred", 
           terms = c("Sexo", "Idade"))


# Ajuste do modelo de regressão linear
mod.v2 <- lm(val ~ tip_classe_4c + Sexo + Idade + esc + Relig, data = bdm)

# Criação do gráfico de valores previstos
plot_model(mod.v2, type = "pred", terms = c("tip_classe_4c", "Sexo"))
