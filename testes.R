dados5$evolucao <- as.factor(dados5$evolucao)
dados5$vacina_cov <- as.factor(dados5$vacina_cov)

fit <- glm(data = dados5, evolucao ~ vacina_cov*variante, family = binomial)
summary(fit)

output$print3 <- renderPrint({
  summary(glm(data = selectData2(), get(input$caracteristicas1) ~ vacina_cov*variante, family = binomial))
})



dados5 <- dados5 %>% 
  mutate(variante2 = case_when(
    variante == "original" ~ "1",
    variante == "gama" ~ "2",
    variante == "delta" ~ "3",
    variante == "omicron" ~ "4",
    TRUE ~ NA_character_))

dados5 <- dados5 %>% 
  mutate(vacina_cov2 = case_when(
    vacina_cov == "sim"  ~ "1",
    vacina_cov == "não"  ~ "0",
    TRUE ~ NA_character_))


# 1 jeito
dados5 <- dados5 %>% 
  mutate(original = as.numeric(case_when(
    variante == "original"  ~ "1",
    is.na(variante)  ~ NA_character_,
    TRUE ~ "0")))

dados5 <- dados5 %>% 
  mutate(gama = as.numeric(case_when(
    variante == "gama"  ~ "1",
    is.na(variante)  ~ NA_character_,
    TRUE ~ "0")))

dados5 <- dados5 %>% 
  mutate(delta = as.numeric(case_when(
    variante == "delta"  ~ "1",
    is.na(variante)  ~ NA_character_,
    TRUE ~ "0")))

dados5 <- dados5 %>% 
  mutate(omicron = as.numeric(case_when(
    variante == "omicron"  ~ "1",
    is.na(variante)  ~ NA_character_,
    TRUE ~ "0")))

# 2 Jeito
fit <- glm(data=dados5,evolucao ~ vacina_cov + variante + vacina_cov*variante,family = binomial)
m1 <- model.matrix(fit)
dados5$vacina_cov_gama <- m1$vacina_covsim.variantegama
dados5$vacina_cov_delta <- m1$vacina_covsim.variantedelta
dados5$vacina_cov_omicron <- m1$vacina_covsim.varianteomicron

dados5$vacina_orig <- as.factor(vacina_cov2*original)


dados5$variante2 <- as.numeric(dados5$variante2)
dados5$vacina_cov2 <- as.numeric(dados5$vacina_cov2)
# dados5$febre2 <- as.numeric(dados5$febre2)

dados5$teste <- dados5$vacina_cov2*dados5$variante2

summary(glm(data=dados5,evolucao ~ vacina_cov + variante + variante*vacina_cov,family = binomial))



