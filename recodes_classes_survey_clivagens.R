library(readxl)
library(dplyr)
library(summarytools)
library(writexl)

################################################################################################

banco_cliv <- read_excel("C:/Users/gusta/Dropbox/Acadêmico/DADOS/clivagens_survey/pesquisa_clivagens_banco_completo_1.xlsx")
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
                               "Matemático"	= "1",
                               "Médico" =	"1",
                               "Promotor de Justiça" = "1",
                               "Psicóloga" =	"1",
                               "Publicitária"	= "1",
                               "Veterinário" =	"1",
                               "Administrador (a) de Empresas" =	"2",
                               "Agente de viagens" =	"2",
                               "Analista de sistemas" =	"2",
                               "Analista Jurídico" =	"2",
                               "Bancário" =	"2",
                               "Corretor de imóveis" =	"2",
                               "Designer Gráfico" =	"2",
                               "Escritor (a)" =	"2",
                               "Fotógrafo"	= "2",
                               "Líder Religioso" =	"2",
                               "Nutricionista" =	"2",
                               "Personal Trainer"	= "2",
                               "Presidente de Sindicato"	= "2",
                               "Radialista"	= "2",
                               "Servidor Público"	= "2",
                               "Técnico em Informática/TI" =	"2",
                               "Assessor (a)"	= "3",
                               "Assistente administrativo"	= "3",
                               "Assistente contábil" =	"3",
                               "Assistente de educação"	= "3",
                               "Assistente sindical"	= "3",
                               "Atendente"	= "4",
                               "Enfermeiro (a)/Técnico em enfermagem"	= "3",
                               "Escriturário (a)"	= "3",
                               "Professora/Pedagoga"	= "3",
                               "Agente de Endemias"	= "4",
                               "Agente de Saúde"	= "4",
                               "Auxiliar administrativo"	= "4",
                               "Auxiliar de escritório"	= "4",
                               "Auxiliar de Produção"	= "4",
                               "Balconista"	= "4",
                               "Consultor de vendas"	= "4",
                               "Crédito imobiliário"	= "4",
                               "Guia turístico"	= "4",
                               "Operador de produção"	= "4",
                               "Operadora de Caixa"	= "4",
                               "Operadora de telecomunicação"	= "4",
                               "Operadora de telemarketing"	= "4",
                               "Organizadora de Eventos"	= "4",
                               "Recepcionista"	= "4",
                               "Representante comercial"	= "4",
                               "Vendedor (a)"	= "4",
                               "Comerciante"	= "5",
                               "Empresário (a)"	= "5",
                               "Microempreendedor"	= "5",
                               "Profissional Liberal"	= "6",
                               "Mestre de obras"	= "7",
                               "Supervisor"	= "7",
                               "Açougueiro"	= "8",
                               "Artesão"	= "8",
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
                               "Fabricação de roupas"	= "8",
                               "Fabricação de sapatos"	= "8",
                               "Manicure" =	"8",
                               "Marceneiro"= 	"8",
                               "Massoterapeuta"	= "8",
                               "Mecânico"	= "8",
                               "Motorista"	= "8",
                               "Nail Designer/Designer de unhas"	= "8",
                               "Padeiro"	= "8",
                               "Pizzaiolo"	= "8",
                               "Podólogo"	= "8",
                               "Policial Militar"	= "8",
                               "Sapateiro"	= "8",
                               "Serralheiro"	= "8",
                               "Taxista"	= "8",
                               "Técnico em manutenção eletroeletrônico"	= "8",
                               "Ajudante de pedreiro"	= "9",
                               "Ambulante"	= "9",
                               "Autônomo/Não especificou"	= "9",
                               "Autônomo/Sem especificar" = "9",
                               "Auxiliar de Limpeza"	= "9",
                               "Babá"	= "9",
                               "Borracheiro"	= "9",
                               "Camareira"	= "9",
                               "Carteiro"	= "9",
                               "Catadora"	= "9",
                               "Construção Civil"	= "9",
                               "Cuidadora de Idosos"	= "9",
                               "Diarista/Faxineira"	= "9",
                               "Empregada Doméstica"	= "9",
                               "Freelancer/Prestador de serviços"	= "9",
                               "Gesseiro"	= "9",
                               "Lavanderia"	= "9",
                               "Manutenção"	= "9",
                               "Marido de Aluguel"	= "9",
                               "Mascate"	= "9",
                               "Montador de móveis"	= "9",
                               "Operador de máquinas"	= "9",
                               "Pedreiro"	= "9",
                               "Pintor de Autos"	= "9",
                               "Porteiro"	= "9",
                               "Segurança"	= "9",
                               "Serviços gerais"	= "9",
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
      P10 == "Empregador / Comerciante / Patrão" ~ "Empresários e Profs. Autonomos",
      P10  == "Empregado" & ocupacao_cod %in% c("5", "6", "11") ~ "Empresários e Profs. Autonomos",
      P10  == "Conta Própria / Autônomo" & ocupacao_cod %in% c("1", "2", "5", "6", "11") ~ "Empresários e Profs. Autonomos",
      P10 == "Empregado" & ocupacao_cod %in% c("1", "2") ~ "Gerentes e Profs. Empregados",
      P10 == "Empregado" & ocupacao_cod %in% c("3", "4") ~ "Trabalhadores n-m de rotina",
      P10 == "Conta Própria / Autônomo" & ocupacao_cod %in% c("3", "4") ~ "Trabalhadores n-m de rotina",
      P10 == "Empregado" & ocupacao_cod %in% c("7", "8") ~ "Trabalhadores e supervisores",
      P10 == "Conta Própria / Autônomo" & ocupacao_cod %in% c("7", "8") ~ "Trabalhadores manuais e subrpoletarios",
      P10 == "Empregado" & ocupacao_cod %in% c("9", "10") ~ "Trabalhadores manuais e subrpoletarios",
      P10 == "Conta Própria / Autônomo" & ocupacao_cod %in% c("9", "10") ~ "Trabalhadores manuais e subrpoletarios",
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
      P10 == "Empregador / Comerciante / Patrão" ~ "Pequena Burguesia",
      P10  == "Empregado" & ocupacao_cod %in% c("5", "6", "11") ~ "Pequena Burguesia",
      P10  == "Conta Própria / Autônomo" & ocupacao_cod %in% c("1","2", "5", "6", "11") ~ "Pequena Burguesia",
      P10 == "Empregado" & ocupacao_cod %in% c("1", "2", "3") ~ "Emp. Colarinho Branco",
      P10 == "Conta Própria / Autônomo" & ocupacao_cod == "3" ~ "Emp. Colarinho Branco",
      P10 == "Empregado" & ocupacao_cod %in% c("4", "7", "8") ~ "Trabalhadores",
      P10 == "Empregado" & ocupacao_cod %in% c("9", "10") ~ "Trabalhadores manuais e precarizados",
      P10  == "Conta Própria / Autônomo" & ocupacao_cod %in% c("4","7", "8", "9", "10") ~ "Trabalhadores manuais e precarizados",
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

write_xlsx(banco_cliv, "C:/Users/gusta/Dropbox/Acadêmico/DADOS/clivagens_survey/bd_clivagens_classe_sbs.xlsx", 
           col_names = T)


