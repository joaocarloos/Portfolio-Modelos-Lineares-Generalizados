library(sandwich)
library(lmtest)
library(gee)


# Criar vetores
res <- c(1,1,0,0,0, 1,1,1,0,0, 0,1,0,0,0, 0,0,0,0,1,
         0,1,0,1,0, 0,1,1,1,1, 0,1,0,0,1, 1,0,0,1,1,
         1,0,1,0,1, 1,0,0,1,1, 1,0,1,1,0, 1,1,0,1,1,
         1,0,1,0,1, 0,1,1,0,0, 0,1,0,0,1)

uni <- rep(1:15, each = 5)
periodo <- rep(1:5, times = 15)

# Criar o conjunto de dados
dados <- data.frame(res, uni, periodo)

head(dados)

# -----------------------
# a) Ajuste um modelo de regressão logística.
# -----------------------

# Modelo de regressão logística simples
modelo_logit <- glm(res ~ periodo, family = binomial(link = "logit"), data = dados)
summary(modelo_logit)


# b) Modelo de quase-verossimilhança para dados binários.
mod_qv <- glm(res ~ periodo, family = quasibinomial(link = "logit"), data = dados)

summary(mod_qv)

# -----------------------
# c) Faça um estudo considerando o modelo de regressão logística para superdispersão, dada a suposição de que, existe a correlação entre as respostas obtidas em relação a cada universidade.
# -----------------------


# Correção robusta por cluster (universidade)
vcov_uni <- sandwich::vcovCL(mod_qv, cluster = ~uni)

# Teste com erros robustos
coeftest(mod_qv, vcov. = vcov_uni)

# -----------------------
# (d) Sobre a suposição de que esta correlação é uniforme, ajuste um modelo GEE.
# -----------------------

mod_gee <- geeglm(res ~ periodo, family = binomial(link = "logit"),
                  data = dados, id = uni, corstr = "exchangeable")


summary(mod_gee)

# -----------------------
# (e) Considerando a estimativa de alfa, em relação a ajuste do GEE com a correlação uniforme, reproduza a matriz de correlação para superdispersão e ajuste um modelo GEE especificando esta matriz como fixa
# -----------------------

# Extraia a estimativa da correlação intra-cluster (alfa chapéu)
alpha_hat <- mod_gee$geese$alpha
alpha_hat

# Criar matriz 5x5 com alpha_hat fora da diagonal
alpha_mat <- matrix(alpha_hat, nrow = 5, ncol = 5)
diag(alpha_mat) <- 1  # Diagonal com 1s


# Criar matriz de correlação fixa a partir do alfa estimado

print(alpha_mat)



# 2. Ajustar o modelo GEE com estrutura de correlação fixa

mod_gee_fixa <- gee(
  formula = res ~ periodo,       # modelo de regressão
  id = uni,                      # identificador do cluster (ex: universidade)
  data = dados,                  # base de dados
  family = binomial(link = "logit"),  # modelo binomial com logit
  corstr = "fixed",              # usar matriz de correlação fixa
  R = alpha_mat                  # matriz de correlação definida acima
)


summary(mod_gee_fixa)



#                                         FIM

