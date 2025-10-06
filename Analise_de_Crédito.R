
# Lista 1 – PRÁTICA - Modelos Lineares Generalizados
# Aplicação em Análise de Crédito
# Prof. Marcelo Angelo Cirillo


# 1. Criando o data frame original
dados <- data.frame(
  Tempo = rep(c(1, 2, 4, 8, 16, 32), 2),
  Classificacao = rep(c("inadimplentes", "adimplentes"), each = 6),
  inadimplentes = c(1, 4, 9, 13, 18, 20,   
                    19, 16, 11, 7, 2, 0),   
  adimplentes =   c(19, 16, 11, 7, 2, 0,     
                     1, 4, 9, 13, 18, 20)   
)

dados$Classificacao<-as.factor(dados$Classificacao)

attach(dados)


Total<-inadimplentes+adimplentes

Proporcao_Inad<-inadimplentes/Total

print(dados)

# ---------------------------
# (a) Estimativas e Gráfico
# ---------------------------

# Gráfico
plot(Tempo[1:6], Proporcao_Inad[1:6], type = "b", pch = 19, col = "red",
     xlab = "Tempo (semanas)", ylab = "Proporção de inadimplentes",
     main = "Proporção de inadimplentes ao longo do tempo")

# A curva é crescente, indicando que a inadimplência aumenta com o tempo. Nos primeiros períodos (entre 0 e 5 semanas), há um crescimento mais acentuado da proporção de inadimplentes, saindo de cerca de 10% para mais de 40%. Após isso, o crescimento se mantém, mas em um ritmo mais lento. Por volta da 15ª semana, a proporção já ultrapassa 80%, e chega próxima a 100% na 32ª semana.

# Essa tendência sugere que, com o passar do tempo, praticamente todos os indivíduos ou entidades observadas acabam se tornando inadimplentes, o que pode indicar um problema de sustentabilidade ou viabilidade no modelo de crédito ou cobrança em questão. A curva suavemente inclinada no final pode indicar que o número de novos inadimplentes começa a se estabilizar após um certo tempo.


# ---------------------------
# (b) Modelos Binomiais
# ---------------------------
resp <- cbind(inadimplentes,Total-inadimplentes)


#mod1 <- glm(resp ~tempo + class + tempo*class , family = binomial(
 # link="probit"),method="glm.fit") 
#summary(mod1)


mod_logit <- glm(resp ~ Tempo * Classificacao  ,
                 family = binomial(link = "logit"), data = dados)


mod_probit <- glm(resp ~Classificacao * Tempo,
                  family = binomial(link = "probit"), data = dados)


mod_cloglog <- glm(resp ~ Classificacao * Tempo,
                   family = binomial(link = "cloglog"), data = dados)


summary(mod_logit);summary(mod_probit);summary(mod_cloglog);

# NOTAS:
# Tem que colocar o summary no relatório
# somente o p valor Pr(>|z|) e os Signif. codes (***)
# 3 tabelas, 3 modelos e os seus p-valor para as 4 variáveis (Intercept, Classificacaoinadimplentes, Tempo e Classificacaoinadimplentes:Tempo)

# ---------------------------
# (c) Comparação entre modelos
# ---------------------------

# Deviance e p-valor
cat("\n>>> Deviance e p-valor:\n")
data.frame(
  Modelo = c("Logit", "Probit", "Cloglog"),
  Deviance = c(deviance(mod_logit), deviance(mod_probit), deviance(mod_cloglog)),
  p_value = c(
    pchisq(deviance(mod_logit), df.residual(mod_logit), lower.tail = FALSE),
    pchisq(deviance(mod_probit), df.residual(mod_probit), lower.tail = FALSE),
    pchisq(deviance(mod_cloglog), df.residual(mod_cloglog), lower.tail = FALSE)
  )
)

# NOTA: Colocar tabéla (modelo; Deviance; p-valor)

# Os três modelos apresentaram bom ajuste (valores-p > 0,05), mas o Logit e o Probit se destacaram por menor deviance. Entre eles, o Logit é preferido pela interpretação mais intuitiva dos coeficientes.


# Envelope simulado (Observacional)
library(glmtoolbox)
# Envelope para modelo logit
envelope(mod_logit, main = "Envelope Simulado - Logit")

# Envelope para modelo probit
envelope(mod_probit, main = "Envelope Simulado - Probit")

# Envelope para modelo cloglog
envelope(mod_cloglog, main = "Envelope Simulado - Cloglog")

# Os envelopes simulados reforçam as conclusões já obtidas por outros critérios: os modelos Logit e Probit oferecem um melhor ajuste aos dados, com resíduos bem comportados. O modelo Cloglog, embora siga a tendência esperada, apresenta maior variabilidade nos resíduos, sugerindo que é menos adequado para o conjunto de dados analisado.


#anova(mod_probit,test = "Chisq")
# ---------------------------
# (d) Odds Ratio
# ---------------------------
cat("\n>>> Razão de chances - Modelo Logit\n")
OR <- exp(coef(mod_logit))
IC <- exp(confint(mod_logit))
print(data.frame(Odds_Ratio = OR, IC_baixo = IC[,1], IC_alto = IC[,2]))

# NOTA: Apresentar a tabela (Odds_Ratio; IC_baixo; IC_alto)

# Os resultados indicam que, no tempo zero e para adimplentes (categoria de referência), a chance do evento ocorrer é alta (OR = 6,87; IC 95%: 3,29 a 16,10). O aumento no tempo reduz significativamente essa chance, com uma razão de chances de 0,74 (IC 95%: 0,65 a 0,83), ou seja, uma queda de cerca de 26% por unidade de tempo. Para inadimplentes, a chance inicial do evento é muito menor (OR = 0,02; IC 95%: 0,0065 a 0,061), comparada aos adimplentes. No entanto, a interação entre tempo e inadimplentes é significativa (OR = 1,81; IC 95%: 1,55 a 2,19), indicando que, entre inadimplentes, o efeito do tempo é inverso: a chance do evento aumenta com o tempo.

# ---------------------------
# (e) Diagnóstico de Outliers
# ---------------------------

# Calcular distância de Cook
cooksd <- cooks.distance(mod_logit)
outliers_cook <- which(cooksd > 4 / nrow(mod_logit$data))  # Limiar comum

# Plotar
plot(cooksd, pch = 16, main = "Distância de Cook para Influência de Observações")
abline(h = 4 / nrow(mod_probit$data), col = "red")

# As observações 1, 5, 7 e 12 possuem valores elevados da distância de Cook, ultrapassando o limite usual. Isso significa que esses pontos exercem influência considerável no ajuste do modelo e podem alterar os parâmetros estimados se forem removidos ou modificados.


# Gráficos para Diagnóstico Visual
library(ggplot2)

residuos_padronizados <- rstudent(mod_probit)
dados_diag <- mod_probit$data
dados_diag$residuos_padronizados <- residuos_padronizados

# Resíduos vs. Valores Ajustados
ggplot(data = mod_probit$data, aes(x = fitted(mod_probit), y = residuos_padronizados)) +
  geom_point() +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
  labs(title = "Resíduos Padronizados vs. Valores Ajustados", x = "Valores Ajustados", y = "Resíduos Padronizados")

# O gráfico de resíduos padronizados versus valores ajustados mostra uma distribuição razoavelmente simétrica e dentro dos limites de ±2, indicando que o modelo não apresenta grandes violações dos pressupostos e que os resíduos estão dentro do esperado.

# ---------------------------
# (f) Modelo Poisson
# ---------------------------

# Poisson com ligação log (efeitos multiplicativos)
mod_pois_log <- glm(inadimplentes ~ Classificacao * Tempo,
                    family = poisson(link = "log"), data = dados)
summary(mod_pois_log)


# O modelo de Poisson com ligação log apresentou bom ajuste (deviance = 14,26; AIC = 65,66). Todos os coeficientes foram significativos. O tempo reduz a inadimplência entre adimplentes, mas essa queda é menor entre os inadimplentes devido à interação positiva entre tempo e classificação.  


# Poisson com ligação identidade (efeitos aditivos)
mod_pois_id <- glm(inadimplentes ~ Classificacao * Tempo,
                   family = poisson(link = "identity"), data = dados)

# O modelo de Poisson com ligação identidade (efeitos aditivos) não convergiu.


# Deviance e p-valor
cat("\n>>> Deviance e p-valor:\n")
data.frame(
  Modelo = c("log (efeitos multiplicativos)"),
  Deviance = c(deviance(mod_pois_log)),
  p_value = c(
    pchisq(deviance(mod_pois_log), df.residual(mod_pois_log), lower.tail = FALSE)
  )
)

# O modelo de Poisson com ligação log apresentou uma deviance de 14,26 com valor-p de 0,075, indicando bom ajuste aos dados (não há evidência de falta de ajuste, pois p > 0,05). Isso reforça que a modelagem com efeitos multiplicativos foi adequada para representar a inadimplência ao longo do tempo.


# Envelope simulado (Observacional)
library(glmtoolbox)
# Envelope para modelo Poisson com log
envelope(mod_pois_log, main = "Envelope Simulado - Poisson (Log)")

# Odds Ratio
cat("\n>>> Razão de chances - Poisson (Log)\n")
OR <- exp(coef(mod_pois_log))
IC <- exp(confint(mod_pois_log))
print(data.frame(Odds_Ratio = OR, IC_baixo = IC[,1], IC_alto = IC[,2]))

# NOTA: Adicionar a tabéla de razão de chance

# O modelo indica que, no tempo zero, a chance de inadimplência é alta para adimplentes. Para inadimplentes, a chance inicial é menor, mas com o tempo essa chance aumenta, refletida pela interação significativa entre tempo e classificação. Enquanto para adimplentes a chance de inadimplência diminui cerca de 14% a cada unidade de tempo, para inadimplentes ocorre o efeito contrário, com aumento progressivo da chance ao longo do tempo.


# Diagnóstico de Outliers
# Calcular distância de Cook
cooksd <- cooks.distance(mod_pois_log)
outliers_cook <- which(cooksd > 4 / nrow(model.frame(mod_pois_log)))# Limiar comum

# Plotar
plot(cooksd, pch = 16, main = "Distância de Cook para Influência de Observações")
abline(h = 4 / nrow(model.frame(mod_pois_log)), col = "red")

# Calcular os resíduos padronizados
residuos_padronizados <- rstandard(mod_pois_log)

# Gráficos para Diagnóstico Visual
library(ggplot2)
# Resíduos vs. Valores Ajustados
ggplot(data = mod_pois_log$data, aes(x = fitted(mod_pois_log), y = residuos_padronizados)) +
  geom_point() +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
  labs(title = "Resíduos Padronizados vs. Valores Ajustados", x = "Valores Ajustados", y = "Resíduos Padronizados")


########################################################


#     https://chatgpt.com/share/6841a7ae-8f60-8000-8ea7-b43577671d3b



