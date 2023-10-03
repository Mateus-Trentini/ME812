################################################################################
##### Potencial de utilização de fósforo de fontes orgânicas em Eucalyptus ##### 
#####       grandis e E. globulus: influência da micorriza arbuscular      #####
################################################################################
#                                                                              #
# Alunos:                                                                      #
#   Daniela Konno 169494                                                       #
#   Jadson Rodrigo Silva de Oliveira 218405                                    #
#   Maria Julia de Lima Silva 184181                                           #
#   Mateus Trentini 217290                                                     #
#   Matheus Eduardo Baruta Lima 241717                                         #
#                                                                              #
# Professora Supervisora:                                                      #
#   Samara Flamini Kiihl                                                       #
#                                                                              #
# Pesquisadora:                                                                #
#   Sara Adrian Lopez de Andrade                                               #
#                                                                              #
# Colaborador:                                                                 #
#   Leonardo Souza de Andrade                                                  #
#                                                                              #
################################################################################

# Bibliotecas----
library(tidyverse)
library(readxl)
library(here)
library(MASS)
library(multcomp)
library(agricolae)

# Diretorio----
path <- here() 


# Dados----
dados10 <- read_xlsx(here(path, "Dados.xlsx"), 1)
dados5 <- read_xlsx(here(path, "Dados.xlsx"), 2)


# Fatorial triplo 2 x 7 x 2 com 10 repeticoes---- #

## Antocianinas
model10_antocianinas <- aov(
  antocianinas ~ Esp * Micorriza * Fonte, 
  data = dados10
)

df <- summary(model10_antocianinas)[[1]][5] %>% 
  data.frame() %>% 
  filter(Pr..F. < 0.05) %>% 
  rownames()

summary(model10_antocianinas)

### tukey

tukey_hsd_esp <- TukeyHSD(model10_antocianinas, "Esp")
tukey_hsd_esp <- HSD.test(model10_antocianinas, "Esp")

print(tukey_hsd_esp)

tukey_hsd_micorriza <- TukeyHSD(model10_antocianinas, "Micorriza")

print(tukey_hsd_micorriza)

tukey_hsd_esp_micorriza <- TukeyHSD(model10_antocianinas, "Esp:Micorriza")

print(tukey_hsd_esp_micorriza)

## Clorofila
model10_clorofila <- aov(
  clorofila ~ Esp * Micorriza * Fonte, 
  data = dados10
)
summary(model10_clorofila)

tukey_hsd_esp_clorofila <- TukeyHSD(model10_clorofila, "Esp")
print(tukey_hsd_esp_clorofila)

tukey_hsd_micorriza_clorofila <- TukeyHSD(model10_clorofila, "Micorriza")
print(tukey_hsd_micorriza_clorofila)

tukey_hsd_fonte_clorofila <- TukeyHSD(model10_clorofila, "Fonte")
print(tukey_hsd_fonte_clorofila)

tukey_hsd_esp_fonte_clorofila <- TukeyHSD(model10_clorofila, "Esp:Fonte")
print(tukey_hsd_esp_fonte_clorofila)

## Flavonois
model10_flavonois <- aov(
  flavonois ~ Esp * Micorriza * Fonte, 
  data = dados10
)
summary(model10_flavonois)

tukey_hsd_esp_flavonois <- TukeyHSD(model10_flavonois, "Esp")
print(tukey_hsd_esp_flavonois)

tukey_hsd_micorriza_flavonois <- TukeyHSD(model10_flavonois, "Micorriza")
print(tukey_hsd_micorriza_flavonois)

tukey_hsd_micorriza_fonte_flavonois <- TukeyHSD(model10_flavonois, "Micorriza:Fonte")
print(tukey_hsd_micorriza_fonte_flavonois)

tukey_hsd_esp_micorriza_flavonois <- TukeyHSD(model10_flavonois, "Esp:Micorriza")
print(tukey_hsd_esp_micorriza_flavonois)

## NBI
model10_nbi <- aov(
  NBI ~ Esp * Micorriza * Fonte, 
  data = dados10
)
summary(model10_nbi)

tukey_hsd_esp_nbi <- TukeyHSD(model10_nbi, "Esp")
print(tukey_hsd_esp_nbi)

tukey_hsd_fonte_nbi <- TukeyHSD(model10_nbi, "Fonte")
print(tukey_hsd_fonte_nbi)

tukey_hsd_esp_fonte_nbi <- TukeyHSD(model10_nbi, "Esp:Fonte")
print(tukey_hsd_esp_fonte_nbi)

## Conteudo de potassio (mg) na planta
model10_conteudo <- aov(
  `conteudo de P mg planta -1` ~ Esp * Micorriza * Fonte, 
  data = dados10
)
summary(model10_conteudo)

tukey_hsd_esp_conteudo <- TukeyHSD(model10_conteudo, "Esp")
tukey_hsd_esp_conteudo <- HSD.test(model10_conteudo, "Esp")

print(tukey_hsd_esp_conteudo)

tukey_hsd_micorriza_conteudo <- TukeyHSD(model10_conteudo, "Micorriza")
print(tukey_hsd_micorriza_conteudo)

tukey_hsd_fonte_conteudo <- TukeyHSD(model10_conteudo, "Fonte")
print(tukey_hsd_fonte_conteudo)

tukey_hsd_esp_micorriza_conteudo <- TukeyHSD(model10_conteudo, "Esp:Micorriza")
print(tukey_hsd_esp_micorriza_conteudo)

tukey_hsd_esp_fonte_conteudo <- TukeyHSD(model10_conteudo, "Esp:Fonte")
print(tukey_hsd_esp_fonte_conteudo)

tukey_hsd_micorriza_fonte_conteudo <- TukeyHSD(model10_conteudo, "Micorriza:Fonte")
print(tukey_hsd_micorriza_fonte_conteudo)

tukey_hsd_esp_micorriza_fonte_conteudo <- TukeyHSD(model10_conteudo, "Esp:Micorriza:Fonte")
print(tukey_hsd_esp_micorriza_fonte_conteudo)


## Concentracao de potassio (g/kg)
model10_concentracao <- aov(
  `conc P g/kg` ~ Esp * Micorriza * Fonte, 
  data = dados10
)
summary(model10_concentracao)


tukey_hsd_esp_concentracao <- TukeyHSD(model10_concentracao, "Esp")
print(tukey_hsd_esp_concentracao)

tukey_hsd_micorriza_concentracao <- TukeyHSD(model10_concentracao, "Micorriza")
print(tukey_hsd_micorriza_concentracao)

tukey_hsd_fonte_concentracao <- TukeyHSD(model10_concentracao, "Fonte")
print(tukey_hsd_fonte_concentracao)


## PUE
model10_pue <- aov(
  PUE ~ Esp * Micorriza * Fonte, 
  data = dados10
)
summary(model10_pue)

tukey_hsd_esp_pue <- TukeyHSD(model10_pue, "Esp")
print(tukey_hsd_esp_pue)

tukey_hsd_micorriza_pue <- TukeyHSD(model10_pue, "Micorriza")
print(tukey_hsd_micorriza_pue)

tukey_hsd_fonte_pue <- TukeyHSD(model10_pue, "Fonte")
print(tukey_hsd_fonte_pue)

tukey_hsd_esp_fonte_pue <- TukeyHSD(model10_pue, "Esp:Fonte")
print(tukey_hsd_esp_fonte_pue)


## PUpE
model10_pupe <- aov(
  PUpE ~ Esp * Micorriza * Fonte, 
  data = dados10
)
summary(model10_pupe)

tukey_hsd_esp_pupe <- TukeyHSD(model10_pupe, "Esp")
print(tukey_hsd_esp_pupe)

tukey_hsd_micorriza_pupe <- TukeyHSD(model10_pupe, "Micorriza")
print(tukey_hsd_micorriza_pupe)

tukey_hsd_fonte_pupe <- TukeyHSD(model10_pupe, "Fonte")
print(tukey_hsd_fonte_pupe)

tukey_hsd_esp_fonte_pupe <- TukeyHSD(model10_pupe, "Esp:Fonte")
print(tukey_hsd_esp_fonte_pupe)

tukey_hsd_micorriza_fonte_pupe <- TukeyHSD(model10_pupe, "Micorriza:Fonte")
print(tukey_hsd_micorriza_fonte_pupe)

tukey_hsd_esp_micorriza_fonte_pupe <- TukeyHSD(model10_pupe, "Esp:Micorriza:Fonte")
print(tukey_hsd_esp_micorriza_fonte_pupe)


## tukey ##



# Fatorial triplo 2 x 7 x 2 com 5 repeticoes----


model5_areasup <- aov(
  `area sup (cm2)` ~ Esp * Micorriza * Fonte, 
  data = dados5
)
summary(model5_areasup)


tukey_hsd_esp_areasup <- TukeyHSD(model5_areasup, "Esp")
print(tukey_hsd_esp_areasup)


tukey_hsd_fonte_areasup <- TukeyHSD(model5_areasup, "Fonte")
print(tukey_hsd_fonte_areasup)


tukey_hsd_esp_fonte_areasup <- TukeyHSD(model5_areasup, "Esp:Fonte")
print(tukey_hsd_esp_fonte_areasup)


tukey_hsd_esp_micorriza_fonte_areasup <- TukeyHSD(model5_areasup, "Esp:Micorriza:Fonte")
print(tukey_hsd_esp_micorriza_fonte_areasup)


## Diametro medio (mm)
model5_avgdiam <- aov(
  `AvgDiam(mm)` ~ Esp * Micorriza * Fonte, 
  data = dados5
)
summary(model5_avgdiam)

tukey_hsd_esp_avgdiam <- TukeyHSD(model5_avgdiam, "Esp")
print(tukey_hsd_esp_avgdiam)

tukey_hsd_micorriza_avgdiam <- TukeyHSD(model5_avgdiam, "Micorriza")
print(tukey_hsd_micorriza_avgdiam)

tukey_hsd_fonte_avgdiam <- TukeyHSD(model5_avgdiam, "Fonte")
print(tukey_hsd_fonte_avgdiam)


## Comprimento da raiz
model5_rootlength <- aov(
  `root length` ~ Esp * Micorriza * Fonte, 
  data = dados5
)
summary(model5_rootlength)


TukeyHSD(model5_rootlength, "Esp")

TukeyHSD(model5_rootlength, "Fonte")

TukeyHSD(model5_rootlength, "Esp:Fonte")

TukeyHSD(model5_rootlength, "Esp:Micorriza:Fonte")


## Peso da raiz seca (g)
model5_rootdryweight <- aov(
  `root dry weight (g)` ~ Esp * Micorriza * Fonte, 
  data = dados5
)
summary(model5_rootdryweight)

TukeyHSD(model5_rootdryweight, "Esp")


TukeyHSD(model5_rootdryweight, "Fonte")


TukeyHSD(model5_rootdryweight, "Esp:Fonte")

## Peso do broto seco (g)
model5_dryweightshoot <- aov(
  `dry weight shoot (g)` ~ Esp * Micorriza * Fonte, 
  data = dados5
)
summary(model5_dryweightshoot)

TukeyHSD(model5_dryweightshoot, "Esp")

TukeyHSD(model5_dryweightshoot, "Micorriza")

TukeyHSD(model5_dryweightshoot, "Fonte")

TukeyHSD(model5_dryweightshoot, "Esp:Micorriza")

TukeyHSD(model5_dryweightshoot, "Esp:Fonte")

TukeyHSD(model5_dryweightshoot, "Micorriza:Fonte")

TukeyHSD(model5_dryweightshoot, "Esp:Micorriza:Fonte")

## Contagem
model5_countn <- aov(
  `cont N` ~ Esp * Micorriza * Fonte, 
  data = dados5
)
summary(model5_countn)


TukeyHSD(model5_countn, "Micorriza")

TukeyHSD(model5_countn, "Fonte")

TukeyHSD(model5_countn, "Esp:Micorriza")

TukeyHSD(model5_countn, "Esp:Micorriza:Fonte")

## Fitase
model5_fitase <- aov(
  `Fitase ng Pi/cm2/min` ~ Esp * Micorriza * Fonte, 
  data = dados5
)
summary(model5_fitase)

TukeyHSD(model5_fitase, "Esp")

TukeyHSD(model5_fitase, "Micorriza")

TukeyHSD(model5_fitase, "Fonte")

TukeyHSD(model5_fitase, "Esp:Micorriza")

TukeyHSD(model5_fitase, "Micorriza:Fonte")


## rApase
model5_rapase <- aov(
  `rAPase` ~ Esp * Micorriza * Fonte, 
  data = dados5
)
summary(model5_rapase)

TukeyHSD(model5_rapase, "Esp")

TukeyHSD(model5_rapase, "Micorriza")

TukeyHSD(model5_rapase, "Fonte")


TukeyHSD(model5_rapase, "Micorriza:Fonte")



### manova ####


# Specify the dependent variables individually
antocianinas <- dados10$antocianinas
clorofila <- dados10$clorofila
flavonois <- dados10$flavonois
NBI <- dados10$NBI
conteudo_de_P_mg_planta_1 <- dados10$`conteudo de P mg planta -1`
conc_P_g_kg <- dados10$`conc P g/kg`
PUE <- dados10$PUE
PUpE <- dados10$PUpE
fosfatase_micélio <- dados10$`fosfatase micélio`

# Perform MANOVA
manova_result <- manova(cbind(
  antocianinas, clorofila, flavonois, NBI
) ~ Esp * Fonte * Micorriza, data = dados10)

# Summary of MANOVA
summary(manova_result)

