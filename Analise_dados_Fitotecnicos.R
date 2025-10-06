# ANÁLISE DE DADOS FITOTÉCNICOS

# Durante a realização do experimento, mediante as condições experimentais (Tabela 1) em um experimento fitotécnico foram observadas amostras de raízes, resistentes ou não a concentração de nematoides fitoparasitas na cultura do café, considerando diferentes tempos de observação. Em virtude de que a variável de interesse é caracterizada por contagens entre raízes resistentes ou não, tornou-se possível realizar contagens e proporções.

#--------------------------------------------------
# (a)	Faça a análise de um modelo de regressão logística convencional para dados binários (ver planilha Ajuste binomial – Dado Binário) e verifique a qualidade de ajuste pelo teste de Hosmer-Lemeshow (ver apostila)
#-------------------------------------------------

# Carregar o pacote necessário para o teste de Hosmer-Lemeshow
# Se não o tiver instalado, execute: install.packages("ResourceSelection")
library(ResourceSelection)

# 1. Criação do conjunto de dados
# Corrigido o erro de digitação de "Concetracao" para "Concentracao"
dados <- data.frame(
  Concentracao = c(100, 500, 1000, 1500, 100, 500, 1000, 1500, 100, 500, 1000, 1500,
                   100, 500, 1000, 1500, 100, 500, 1000, 1500, 100, 500, 1000, 1500,
                   100, 500, 1000, 1500, 100, 500, 1000, 1500, 100, 500, 1000, 1500,
                   100, 500, 1000, 1500, 100, 500, 1000, 1500, 100, 500, 1000, 1500),
  Tempo = rep(c(15, 30, 60, 120), each = 12),
  Resposta = c(1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0,
               0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0,
               1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1,
               1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1)
)

# 2. Ajuste do modelo de regressão logística convencional (link = "logit")
# A função glm() é usada para ajustar modelos lineares generalizados.
# family = binomial(link = "logit") especifica a regressão logística.
mod_logit <- glm(Resposta ~ Concentracao + Tempo,
                 family = binomial(link = "logit"),
                 data = dados)

# Exibir o resumo do modelo para análise dos coeficientes
summary(mod_logit)

# 3. Teste de Qualidade do Ajuste de Hosmer-Lemeshow
# Este teste avalia a concordância entre os valores observados e os preditos pelo modelo.
# Um p-valor alto (geralmente > 0.05) sugere que o modelo se ajusta bem aos dados.
# O argumento 'g' define o número de grupos para o teste.
hosmer_test_logit <- hoslem.test(x = dados$Resposta,
                                 y = fitted(mod_logit),
                                 g = 10) # Usando g=10 como padrão, mas g=4 como no seu original também é válido.

# Exibir o resultado do teste de Hosmer-Lemeshow
print("--- Teste de Hosmer-Lemeshow para o Modelo Logit ---")
print(hosmer_test_logit)

# Gráfico de diagnóstico (opcional)
plot(fitted(mod_logit), residuals(mod_logit, type = "pearson"),
     xlab = "Valores preditos", ylab = "Resíduos de Pearson",
     main = "Gráfico de Diagnóstico")
abline(h = 0, col = "red")

#O modelo ajustado (Resposta ~ Concentracao + Tempo) indicou que o aumento da Concentracao tem um efeito negativo na chance de resistência (coeficiente = $-0.0009286$), enquanto o Tempo de observação apresentou um efeito positivo (coeficiente = $0.0042589$). Contudo, nenhuma das variáveis demonstrou ser estatisticamente significante para predizer a resposta, apresentando p-valores de 0.111 e 0.575, respectivamente. Para avaliar a qualidade do ajuste do modelo, foi realizado o teste de Hosmer-Lemeshow, que resultou em um p-valor de 0.1059 ($X^2 = 13.176, df = 8$). Uma vez que este valor é superior ao nível de significância de 0.05, não há evidências para rejeitar a hipótese nula, concluindo-se que o modelo se ajusta adequadamente aos dados observados.

#----------------------------------------------
# (b)	Independente do resultado da qualidade do ajuste, ajuste o modelo de regressão logística, considerando a abordagem aplicada a regressão logística para dados de superdispersão.
#----------------------------------------------

# O exemplo "Exposição de besouros"  da apostila ilustra uma situação onde essa abordagem é necessária. No exemplo, o modelo logístico padrão apresentou um desvio (Deviance) de 11,23 para 6 graus de liberdade, um valor consideravelmente maior que o esperado. Essa discrepância (Deviance > graus de liberdade) é um sinal clássico de sobredispersão, indicando que a variabilidade nos dados é maior do que a que o modelo binomial padrão assume.

# A maneira padrão de ajustar um modelo de regressão logística que lida com a sobredispersão em R é usar a família quasibinomial. Este modelo estima um parâmetro de dispersão (phi) a partir dos dados e o utiliza para corrigir os erros-padrão das estimativas dos coeficientes, fornecendo uma análise mais robusta.

# Ajuste do modelo de regressão logística para sobredispersão
# Usamos a família 'quasibinomial' que estima um parâmetro de dispersão
# para corrigir as estimativas de variância.
mod_quasi <- glm(Resposta ~ Concentracao + Tempo,
                 family = quasibinomial(link = "logit"),
                 data = dados)

# 3. Exibir o resumo do modelo
# Note que o output agora inclui um "Dispersion parameter" e usa
# a estatística t no lugar da z.
summary(mod_quasi)

#A análise deste modelo resultou em um parâmetro de dispersão estimado em 1.062412. Como este valor é superior a 1, ele confirma a presença de uma leve sobredispersão nos dados, indicando que a variabilidade da resposta é ligeiramente maior do que a assumida pelo modelo binomial padrão. As estimativas dos coeficientes para Concentracao (-0.0009286) e Tempo (0.0042589) não se alteram. Contudo, a principal diferença deste modelo é a correção dos erros-padrão, que agora consideram a dispersão extra. Com essa correção, os novos p-valores para os efeitos de Concentracao e Tempo foram de 0.129 e 0.590, respectivamente. Assim, mesmo utilizando um modelo estatisticamente mais adequado para a variabilidade observada, a conclusão se mantém: não há evidências de um efeito estatisticamente significativo das variáveis preditoras sobre a resistência das raízes ao nível de significância de 5%.


#-----------------------------------------------
# (c)	Estime a razão de chances considerando as estimativas dos modelos ajustados nos itens (a) e (b) 
#-----------------------------------------------

# Nessa parte é importante perceber que as estimativas dos coeficientes (beta) são idênticas nos modelos do item (a) (binomial) e do item (b) (quasibinomial). O que muda entre eles são os erros-padrão e, consequentemente, os intervalos de confiança.

# Portanto, a estimativa pontual da Razão de Chances será a mesma para ambos os modelos, mas a precisão dessa estimativa (refletida no intervalo de confiança) será diferente.

# Modelo do item (a): Logístico Padrão
mod_logit <- glm(Resposta ~ Concentracao + Tempo,
                 family = binomial(link = "logit"), data = dados)

# Modelo do item (b): Logístico com correção para Sobredispersão
mod_quasi <- glm(Resposta ~ Concentracao + Tempo,
                 family = quasibinomial(link = "logit"), data = dados)


# Calcular a Razão de Chances (Odds Ratio) para o modelo (a)
# Usamos exp() nos coeficientes e nos seus intervalos de confiança
razao_chances_a <- exp(coef(mod_logit))
ic_razao_chances_a <- exp(confint(mod_logit))

cat("--- Razão de Chances (OR) - Modelo (a): Logístico Padrão ---\n")
print(cbind(OR = razao_chances_a, ic_razao_chances_a))


# Calcular a Razão de Chances (Odds Ratio) para o modelo (b)
# A estimativa pontual do OR é a mesma, mas o IC será diferente (mais amplo)
razao_chances_b <- exp(coef(mod_quasi))
ic_razao_chances_b <- exp(confint(mod_quasi))

cat("\n--- Razão de Chances (OR) - Modelo (b): Quase-Binomial ---\n")
print(cbind(OR = razao_chances_b, ic_razao_chances_b))

# Razão de Chances (OR) e os respectivos intervalos de confiança de 95% para os modelos ajustados. Para o Modelo (a) (Logístico Padrão), a razão de chances para Concentracao foi de 0.9990719, com intervalo de confiança de [0.9978751, 1.000186], enquanto para Tempo foi de 1.0042680, com intervalo de [0.9895881, 1.019988].

# Para o Modelo (b) (Quase-Binomial), que leva em conta a sobredispersão, as estimativas pontuais da razão de chances permaneceram idênticas, como esperado. No entanto, os intervalos de confiança tornaram-se mais amplos, refletindo a maior incerteza. O intervalo para Concentracao foi de [0.9978360, 1.000220] e para Tempo foi de [0.9891394, 1.020505].

# A interpretação da razão de chances mostra que um aumento na Concentracao está associado a uma diminuição mínima na chance de resistência, enquanto o Tempo está associado a um aumento mínimo. Contudo, a conclusão mais importante é que, para ambos os modelos, o valor 1.0 está contido em todos os intervalos de confiança calculados. Isso reforça a conclusão dos itens anteriores: não há evidência estatística, ao nível de significância de 5%, de que a concentração do nematoide ou o tempo de observação influenciem a chance de resistência das raízes de café.


#-----------------------------------------
# (d)	Ajuste um modelo de regressão para dados de proporção (ver planilha Ajuste da binomial – Dados de proporção).
#-----------------------------------------

# Para este item, a estrutura dos dados é diferente. Em vez de uma resposta binária (0 ou 1) para cada observação, agora temos o número de "sucessos" (Resp) e o número de "fracassos" (Nao_Resp) para cada combinação dos preditores.

# Remove todos os objetos do ambiente
rm(list = ls())

dados_prop <- data.frame(
  Concentracao = c(100,500,1000,1500,100,500,1000,1500,100,500,1000,1500,
                   100,500,1000,1500,100,500,1000,1500,100,500,1000,1500,
                   100,500,1000,1500,100,500,1000,1500,100,500,1000,1500,
                   100,500,1000,1500,100,500,1000,1500,100,500,1000,1500),
  Tempo = c(rep(15,12), rep(30,12), rep(60,12), rep(120,12)),
  Resp = c(10,8,12,13,16,5,11,3,1,4,3,1,
           3,10,3,3,10,6,13,12,3,3,5,1,
           8,5,9,10,15,14,12,19,1,1,2,2,
           3,1,1,0,3,10,3,4,3,1,2,4),
  Nao_Resp = c(9,5,10,3,2,1,8,1,3,2,1,2,
               8,9,3,3,11,9,17,15,2,1,2,1,
               9,5,9,8,17,19,16,18,3,0,1,2,
               1,3,0,7,5,9,9,13,1,1,1,2))

# Ajuste do modelo de regressão para proporção
# A variável resposta é uma matriz com as colunas de sucesso (Resp) e fracasso (Nao_Resp)
mod_prop <- glm(cbind(Resp, Nao_Resp) ~ Concentracao + Tempo,
                family = binomial(link = "logit"),
                data = dados_prop)

# Exibir o resumo completo do modelo
# Este resumo incluirá a análise da deviance, que é crucial para
# verificar a qualidade do ajuste em dados de proporção.
summary(mod_prop)

# A análise da qualidade do ajuste, um passo crucial para este tipo de dados, indicou que o modelo é adequado. A deviance residual de 53.100 com 45 graus de liberdade  resultou em um p-valor no teste qui-quadrado de aproximadamente 0.18 (> 0.05), mostrando não haver evidências de falta de ajuste ou superdispersão.

# Na análise dos coeficientes do modelo, verificou-se que a Concentracao não teve efeito estatisticamente significativo sobre a proporção de raízes resistentes (p = 0.77033). Em contrapartida, a variável Tempo mostrou-se altamente significativa (p = 0.00273), com um coeficiente negativo. Este resultado indica que, com o passar do tempo, a proporção de raízes resistentes na cultura de café diminui de forma significativa.
