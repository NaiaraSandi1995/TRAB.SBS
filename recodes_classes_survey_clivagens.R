library(readxl)
library(dplyr)
library(summarytools)
library(writexl)

################################################################################################

banco_cliv <- read_excel("C:/Users/gusta/Dropbox/Acad�mico/DADOS/clivagens_survey/pesquisa_clivagens_banco_completo_1.xlsx")
View(banco_cliv)


################################################################################################
# 1. Atribuindo codigos as respostas abertas da questao sobre ocupacao (P11) 


banco_cliv <- banco_cliv %>%
  mutate(ocupacao_cod = recode(P11,
                               "Advogado (a)"= "1",
                               "Contador (a)" = "1",
                               "Desenvolvedor de Sistemas" = "1",
                               "Engenheiro (a)" = "1",	
                               "Executiva" =	"1",
                               "Gerente" =	"1",
                               "Matem�tico"	= "1",
                               "M�dico" =	"1",
                               "Promotor de Justi�a" = "1",
                               "Psic�loga" =	"1",
                               "Publicit�ria"	= "1",
                               "Veterin�rio" =	"1",
                               "Administrador (a) de Empresas" =	"2",
                               "Agente de viagens" =	"2",
                               "Analista de sistemas" =	"2",
                               "Analista Jur�dico" =	"2",
                               "Banc�rio" =	"2",
                               "Corretor de im�veis" =	"2",
                               "Designer Gr�fico" =	"2",
                               "Escritor (a)" =	"2",
                               "Fot�grafo"	= "2",
                               "L�der Religioso" =	"2",
                               "Nutricionista" =	"2",
                               "Personal Trainer"	= "2",
                               "Presidente de Sindicato"	= "2",
                               "Radialista"	= "2",
                               "Servidor P�blico"	= "2",
                               "T�cnico em Inform�tica/TI" =	"2",
                               "Assessor (a)"	= "3",
                               "Assistente administrativo"	= "3",
                               "Assistente cont�bil" =	"3",
                               "Assistente de educa��o"	= "3",
                               "Assistente sindical"	= "3",
                               "Atendente"	= "4",
                               "Enfermeiro (a)/T�cnico em enfermagem"	= "3",
                               "Escritur�rio (a)"	= "3",
                               "Professora/Pedagoga"	= "3",
                               "Agente de Endemias"	= "4",
                               "Agente de Sa�de"	= "4",
                               "Auxiliar administrativo"	= "4",
                               "Auxiliar de escrit�rio"	= "4",
                               "Auxiliar de Produ��o"	= "4",
                               "Balconista"	= "4",
                               "Consultor de vendas"	= "4",
                               "Cr�dito imobili�rio"	= "4",
                               "Guia tur�stico"	= "4",
                               "Operador de produ��o"	= "4",
                               "Operadora de Caixa"	= "4",
                               "Operadora de telecomunica��o"	= "4",
                               "Operadora de telemarketing"	= "4",
                               "Organizadora de Eventos"	= "4",
                               "Recepcionista"	= "4",
                               "Representante comercial"	= "4",
                               "Vendedor (a)"	= "4",
                               "Comerciante"	= "5",
                               "Empres�rio (a)"	= "5",
                               "Microempreendedor"	= "5",
                               "Profissional Liberal"	= "6",
                               "Mestre de obras"	= "7",
                               "Supervisor"	= "7",
                               "A�ougueiro"	= "8",
                               "Artes�o"	= "8",
                               "Cabelereiro(a)"	= "8",
                               "Cabelereira" = "8",
                               "Caldeireiro" = "8",
                               "Caminhoneiro"	= "8",
                               "Carpinteiro"	= "8",
                               "Chaveiro"	= "8",
                               "Confeiteira"	= "8",
                               "Costureira"	= "8",
                               "Cozinheira"	= "8",
                               "Designer de sobrancelhas"	= "8",
                               "Eletricista"	= "8",
                               "Encanador"	= "8",
                               "Esteticista"	= "8",
                               "Fabrica��o de roupas"	= "8",
                               "Fabrica��o de sapatos"	= "8",
                               "Manicure" =	"8",
                               "Marceneiro"= 	"8",
                               "Massoterapeuta"	= "8",
                               "Mec�nico"	= "8",
                               "Motorista"	= "8",
                               "Nail Designer/Designer de unhas"	= "8",
                               "Padeiro"	= "8",
                               "Pizzaiolo"	= "8",
                               "Pod�logo"	= "8",
                               "Policial Militar"	= "8",
                               "Sapateiro"	= "8",
                               "Serralheiro"	= "8",
                               "Taxista"	= "8",
                               "T�cnico em manuten��o eletroeletr�nico"	= "8",
                               "Ajudante de pedreiro"	= "9",
                               "Ambulante"	= "9",
                               "Aut�nomo/N�o especificou"	= "9",
                               "Aut�nomo/Sem especificar" = "9",
                               "Auxiliar de Limpeza"	= "9",
                               "Bab�"	= "9",
                               "Borracheiro"	= "9",
                               "Camareira"	= "9",
                               "Carteiro"	= "9",
                               "Catadora"	= "9",
                               "Constru��o Civil"	= "9",
                               "Cuidadora de Idosos"	= "9",
                               "Diarista/Faxineira"	= "9",
                               "Empregada Dom�stica"	= "9",
                               "Freelancer/Prestador de servi�os"	= "9",
                               "Gesseiro"	= "9",
                               "Lavanderia"	= "9",
                               "Manuten��o"	= "9",
                               "Marido de Aluguel"	= "9",
                               "Mascate"	= "9",
                               "Montador de m�veis"	= "9",
                               "Operador de m�quinas"	= "9",
                               "Pedreiro"	= "9",
                               "Pintor de Autos"	= "9",
                               "Porteiro"	= "9",
                               "Seguran�a"	= "9",
                               "Servi�os gerais"	= "9",
                               "Vigilante"	= "9",
                               "Zelador"	= "9",
                               "Agricultor (a)"	= "10",
                               "Jardinagem"	= "10",
                               "Lavrador (a)"	= "10",
                               .default = NA_character_))


freq(banco_cliv$ocupacao_cod)

################################################################################################
# 2. Tipologia de classes - versao workshop

banco_cliv <- banco_cliv %>%
   mutate(tip_classe = case_when(
      P10 == "Empregador / Comerciante / Patr�o" ~ "Empres�rios e Profs. Autonomos",
      P10  == "Empregado" & ocupacao_cod %in% c("5", "6", "11") ~ "Empres�rios e Profs. Autonomos",
      P10  == "Conta Pr�pria / Aut�nomo" & ocupacao_cod %in% c("1", "2", "5", "6", "11") ~ "Empres�rios e Profs. Autonomos",
      P10 == "Empregado" & ocupacao_cod %in% c("1", "2") ~ "Gerentes e Profs. Empregados",
      P10 == "Empregado" & ocupacao_cod %in% c("3", "4") ~ "Trabalhadores n-m de rotina",
      P10 == "Conta Pr�pria / Aut�nomo" & ocupacao_cod %in% c("3", "4") ~ "Trabalhadores n-m de rotina",
      P10 == "Empregado" & ocupacao_cod %in% c("7", "8") ~ "Trabalhadores e supervisores",
      P10 == "Conta Pr�pria / Aut�nomo" & ocupacao_cod %in% c("7", "8") ~ "Trabalhadores manuais e subrpoletarios",
      P10 == "Empregado" & ocupacao_cod %in% c("9", "10") ~ "Trabalhadores manuais e subrpoletarios",
      P10 == "Conta Pr�pria / Aut�nomo" & ocupacao_cod %in% c("9", "10") ~ "Trabalhadores manuais e subrpoletarios",
      TRUE ~ NA_character_))

freq(banco_cliv$tip_classe)

ctable(x = banco_cliv$ocupacao_cod,
       y = banco_cliv$tip_classe,
       prop = "r")

ctable(x = banco_cliv$P10,
       y = banco_cliv$tip_classe,
       prop = "r")

################################################################################################
# 3. Alternativa: agregacao com 4 categorias - proposta para sbs


banco_cliv <- banco_cliv %>%
   mutate(tip_classe_4c = case_when(
      P10 == "Empregador / Comerciante / Patr�o" ~ "Pequena Burguesia",
      P10  == "Empregado" & ocupacao_cod %in% c("5", "6", "11") ~ "Pequena Burguesia",
      P10  == "Conta Pr�pria / Aut�nomo" & ocupacao_cod %in% c("1","2", "5", "6", "11") ~ "Pequena Burguesia",
      P10 == "Empregado" & ocupacao_cod %in% c("1", "2", "3") ~ "Emp. Colarinho Branco",
      P10 == "Conta Pr�pria / Aut�nomo" & ocupacao_cod == "3" ~ "Emp. Colarinho Branco",
      P10 == "Empregado" & ocupacao_cod %in% c("4", "7", "8") ~ "Trabalhadores",
      P10 == "Empregado" & ocupacao_cod %in% c("9", "10") ~ "Trabalhadores manuais e precarizados",
      P10  == "Conta Pr�pria / Aut�nomo" & ocupacao_cod %in% c("4","7", "8", "9", "10") ~ "Trabalhadores manuais e precarizados",
      TRUE ~ NA_character_))

freq(banco_cliv$tip_classe_4c)

ctable(x = banco_cliv$ocupacao_cod,
       y = banco_cliv$tip_classe_4c,
       prop = "r")

ctable(x = banco_cliv$P10,
       y = banco_cliv$tip_classe_4c,
       prop = "r")

################################################################################################
# 4. Criando banco de dados

write_xlsx(banco_cliv, "C:/Users/gusta/Dropbox/Acad�mico/DADOS/clivagens_survey/bd_clivagens_classe_sbs.xlsx", 
           col_names = T)


