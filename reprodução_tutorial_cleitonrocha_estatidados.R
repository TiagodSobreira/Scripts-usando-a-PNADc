#Importando as bibliotecas necess�rias
library(dplyr)
library(srvyr)
library(readr)
library(ggplot2)


#definindo a pasta de trabalho
setwd("C:/Users/tiago/Downloads/PNAD_COVID_062020 (1)")
pnad_covid <- read_csv("PNAD_COVID_062020.csv", col_types = cols(.default = "d"))


#ligando pesos e filtrando salvador
pnad_com_pesos <- pnad_covid %>%
  as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE) %>%
  filter(CAPITAL == "29")


#Criando colunas com vari�veis
pnad_com_pesos <- pnad_com_pesos %>%
  mutate(one = 1,
         sexo = ifelse(A003 == 1, "Homem", "Mulher"),
         idade = case_when(
           A002 %in% 15:24 ~ "15-24",
           A002 %in% 25:34 ~ "25-34",
           A002 %in% 35:49 ~ "35-39",
           A002 %in% 50:64 ~ "50-64",
           A002 > 64 ~ "65+"),
         cor = case_when(
           A004 == 1 ~ "Branca",
           A004 == 2 ~ "Preta",
           A004 == 4 ~ "Parda"),
         escolaridade = factor(case_when(
           A005 %in% 1:2 ~ "Sem Instru��o ou Fundamental Incompleto",
           A005 %in% 3:4 ~ "Fundamental Completo ou M�dio Incompleto",
           A005 %in% 5:6 ~ "M�dio Completo ou Superior Incompleto",
           A005 == 7 ~ "Superior Completo",
           A005 == 8 ~ "P�s-gradua��o"),
           levels = c("Sem Instru��o ou Fundamental Incompleto",
                      "Fundamental Completo ou M�dio Incompleto",
                      "M�dio Completo ou Superior Incompleto",
                      "Superior Completo",
                      "P�s-gradua��o")),
         tipo_emprego = factor(case_when(
           C007 == 1 ~ "Trabalhador dom�stico (empregador dom�stico, cuidados, bab�)",
           C007 == 2 ~ "Militar",
           C007 == 3 ~ "Policial ou Bombeiro",
           C007 == 4 ~ "Setor privado",
           C007 == 5 ~ "Setor p�blico",
           C007 == 6 ~ "Empregador",
           C007 == 7 ~ "Aut�nomo (Conta pr�pria)"),
           levels = c("Trabalhador dom�stico (empregador dom�stico, cuidados, bab�)",
                      "Militar",
                      "Policial ou Bombeiro",
                      "Setor privado",
                      "setor p�blico",
                      "Empregador",
                      "Aut�nomo (Conta pr�pria)")),
         faixa_salario = factor(case_when(
           C01012 <= 1044 ~ "Menos de um sal�rio min�mo",
           C01012 %in% c(1045:2090) ~ "Entre 1 e 2",
           C01012 %in% c(2091:3135) ~ "Entre 2 e 3",
           C01012 %in% c(3136:4180) ~ "Entre 3 e 4",
           C01012 %in% c(4181:5225) ~ "Entre 4 e 5",
           C01012 >= 5226 ~ "Mais de 5"),
           levels = c("Menos de um sal�rio min�mo",
                      "Entre 1 e 2",
                      "Entre 2 e 3",
                      "Entre 3 e 4",
                      "Entre 4 e 5",
                      "Mais de 5")),
         domicilio_situacao = factor(case_when(
           F001 == 1 ~ "Pr�prio - j� pago",
           F001 == 2 ~ "Pr�prio - ainda pagando",
           F001 == 3 ~ "Alugado",
           F001 %in% 4:6 ~ "Cedido (Por empregador, familiar ou outro)"),
           levels = c("Pr�prio - j� pago",
                      "pr�prio - ainda pagando",
                      "Alugado",
                      "Cedido (Por empregador, famliar ou outro)")),
         home_office = ifelse(C013 == 1, "Home Office", "Presencial"),
         auxilio_emergencial = ifelse(D0051 == 1, "Aux�lio", "Sem aux�lio"))


#Vamos olhar para o sexo e a cor das pessoas que est�o em home office em salvador
home_sexo_cor <- pnad_com_pesos %>%
  group_by(sexo, cor) %>%
  summarise(home_office = survey_total(C013 == 1, na.rm = TRUE),
            mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()

#Imprimindo a vari�vel calculada
print(home_sexo_cor)


#Construindo um gr�fico com as estat�sticas calculadas acima
home_sexo_cor_ssa <- ggplot(home_sexo_cor, aes(fill = cor, y = trab_home_office, x = sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
  labs(x = "Sexo", fill = "Cor/Ra�a: ", caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
       title = "Pessoas em home office, por cor/ra�a e sexo - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7")) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")# Salvando
ggsave(plot = home_sexo_cor_ssa, "home_sexo_cor_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")


#Vamos olhar para a cor e o grau de instru��o das pessoas em home office em salvador
home_edu_cor <- pnad_com_pesos %>%
  group_by(escolaridade, cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()

#Imprindo as estat�sticas de home offie por escolaridade e cor
print(home_edu_cor)


#Construindo os gr�ficos de home office por escolaridade e cor
home_edu_cor_ssa <- ggplot(home_edu_cor, aes(fill = escolaridade, y = trab_home_office, x = cor)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
  labs(x = "Cor/Ra�a", fill = "Escolaridade: ", caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
       title = "Pessoas em home office, por cor/ra�a e escolaridade - Salvador/BA ") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e")) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")# Salvando em PNG
ggsave(plot = home_edu_cor_ssa, "home_edu_cor_ssa.png",
       width = 14, height = 7, dpi = 150, units = "in",type = "cairo")

#Plotando o gr�fico constru�d
print(home_edu_cor_ssa)


#Vamos olhar trabalhores em home office por sexo e idade em salvador
home_sexo_idade <- pnad_com_pesos %>%
  group_by(sexo, idade) %>%
  summarise(home_office = survey_total(C013 == 1, na.rm = TRUE),
            mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()

#Imprindo as estat�sticas de home office por idade e sexo
print(home_sexo_idade)


#Construindo um gr�fico com as estat�sticas que foram calculados acima
home_sexo_idade_ssa <- ggplot(home_sexo_idade, aes(fill = idade, y = trab_home_office, x = sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
  labs(x = "Sexo", fill = "Faixa Et�ria: ", caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
       title = "Pessoas em home office, por sexo e faixa et�ria - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e")) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")# Salvando em PNG
ggsave(plot = home_sexo_idade_ssa, "home_sexo_idade_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")


#Plotando gr�ficos de home office para idade e sexo
print(home_sexo_idade_ssa)


#Home office por tipo de emprego em salvador
home_emprego <- pnad_com_pesos %>%
  group_by(tipo_emprego) %>%
  summarise(home_office = survey_total(C013 == 1, na.rm = TRUE),
            mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na

#Imprimindo as estat�sticas de home office por tipo de emprego em salvador
print(home_emprego)


#Construindo o gr�fico para essas estat�sticas
legenda_trabalhos <- c("Trabalhador dom�stico\n (empregado dom�stico,\n cuidados, bab�)",
                       "Militar", 
                       "Policial ou\n Bombeiro",
                       "Setor privado",
                       "Setor p�blico",
                       "Empregador",
                       "Aut�nomo\n (Conta pr�pria)")# Gr�fico
home_emprego_ssa <- ggplot(home_emprego, aes(fill = tipo_emprego, y = trab_home_office, x = tipo_emprego)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=8),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "none") +
  labs(x = "Tipo de Ocupa��o",
       caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
       title = "Pessoas em home office, por tipo de ocupa��o - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e","#636e72", "#55efc4")) +
  scale_x_discrete(labels = legenda_trabalhos) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")# Salvando em PNG
ggsave(plot = home_emprego_ssa, "home_emprego_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")

#Plotando o gr�fico
print(home_emprego_ssa)


#Estat�sticas de cor e faixa salarial em home office
home_renda <- pnad_com_pesos %>%
  group_by(faixa_salario, cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office / mao_de_obra) * 100) %>%
  drop_na()# gr�fico
home_renda_ssa <- ggplot(home_renda, aes(fill = faixa_salario, y = trab_home_office, x = cor)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),size = 2.5, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
  labs(x = "Cor/Ra�a", fill = "Faixa Salarial:\n(Sal�rios m�nimos) ", caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
       title = "Pessoas em home office, por cor/ra�a e faixa salarial - Salvador/BA ") +
  scale_fill_manual(values = c("#fad390","#e55039","#4a69bd","#60a3bc","#78e08f","#079992")) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")# Salvando em PNG
ggsave(plot = home_renda_ssa, "home_renda_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")

#imprindo as estat�sticas e o gr�fico
print(home_renda)
print(home_renda_ssa)


#Pessoas que receberam aux�lio por sexo e cor
auxilio_cor_sexo <- pnad_com_pesos %>%
  group_by(cor, sexo) %>%
  summarise(
    auxilio = survey_total(D0051 == 1, na.rm = TRUE),
    total = survey_total(one, na.rm = TRUE)
  ) %>%
  mutate(pessoas_auxilio = (auxilio/total)*100) %>%
  drop_na()# gr�fico
auxilio_cor_sexo_ssa <- ggplot(auxilio_cor_sexo, aes(fill = cor, y = pessoas_auxilio, x = sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",pessoas_auxilio)),size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "bottom", legend.background = element_rect(fill="ghostwhite", size=0.7, linetype="blank")) +
  labs(fill = "Cor: ", x = "Sexo", caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
       title = "Pessoas que receberam aux�lio emergencial, por cor/ra�a e sexo -\n Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3")) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")# Salvando em PNG
ggsave(plot = auxilio_cor_sexo_ssa, "auxilio_cor_sexo_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")


#Imprimindo estat�sticas e plotando gr�ficos
print(auxilio_cor_sexo)
print(auxilio_cor_sexo_ssa)


#Estat�sticas de aux�lio emergencial por renda
auxilio_renda <- pnad_com_pesos %>%
  group_by(faixa_salario) %>%
  summarise(
    auxilio = survey_total(D0051 == 1, na.rm = TRUE),
    total = survey_total(one, na.rm = TRUE)) %>%
  mutate(pessoas_auxilio = (auxilio/total)*100) %>%
  drop_na()# gr�fico
auxilio_renda_ssa <- ggplot(auxilio_renda, aes(fill = faixa_salario, y = pessoas_auxilio, x = faixa_salario)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",pessoas_auxilio)),size = 3, position =position_dodge(width=0.9),
            hjust=-0.1, color = 'black',fontface='bold') +
  theme_classic() +
  coord_flip() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "none") +
  labs(x = "Faixa Salarial", caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
       title = "Pessoas que receberam aux�lio emergencial, por renda - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3","#6c5ce7","#fdcb6e","#636e72")) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")# Salvando em PNG
ggsave(plot = auxilio_renda_ssa, "auxilio_renda_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")


#Imprimindo as estat�sticas e plotando o gr�fico
print(auxilio_renda)
print(auxilio_renda_ssa)


#Estat�sticas de aux�lio emergencial por situa��o no dom�cilio
auxilio_domicilio <- pnad_com_pesos %>%
  group_by(domicilio_situacao) %>%
  summarise(
    auxilio = survey_total(D0051 == 1, na.rm = TRUE),
    total = survey_total(one, na.rm = TRUE)) %>%
  mutate(pessoas_auxilio  = (auxilio/total)*100) %>%
  drop_na()# ordenando eixo X
legenda_domicilio <- c("Pr�prio (j� pago)",
                       "Pr�prio (ainda pagando)",
                       "Alugado", 
                       "Cedido (Por empregador,\n Familiar ou outro)")# gr�fico
auxilio_domicilio_ssa <- ggplot(auxilio_domicilio, aes(fill = domicilio_situacao, y = pessoas_auxilio, x = domicilio_situacao)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",pessoas_auxilio)),size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
   geom_text(aes(label=paste0(format(round(auxilio,0),
                              nsmall=0,big.mark=".", decimal.mark=",")," com auxilio")),
             size = 3,position = position_stack(vjust = .5),
             vjust=-0.5, color = 'black',fontface='bold') +
   geom_text(aes(label=paste0(format(round(total,0),
                                     nsmall=0,big.mark=".", decimal.mark=",")," total")),
             size = 3,position = position_stack(vjust = .3),
             vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold", color="#000000", size=10),
        plot.title = element_text(colour = "black", size = 17, hjust=0.5),
        legend.position = "none") +
  labs(x = "Tipo de domic�lio", y ="Percentual (%)",caption = "Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
       title = "Situa��o do domic�lio daqueles que receberam o aux�lio emergencial -\n Salvador/BA") +
  scale_fill_manual(values = c("#fad390","#e55039","#4a69bd","#60a3bc","#78e08f","#079992")) +
  scale_x_discrete(labels = legenda_domicilio) +
  scale_y_discrete(limits=factor(0:100), breaks = c(0,10,20,30,40,50,60,70,80,90,100), name = "Percentual (%)")# Salvando em PNG
ggsave(plot = auxilio_domicilio_ssa, "auxilio_domicilio_ssa.png",
       width = 10, height = 5, dpi = 120, units = "in",type = "cairo")


#Imprindo as estat�sticas e plotando o gr�fico
print(auxilio_domicilio)
print(auxilio_domicilio_ssa)





