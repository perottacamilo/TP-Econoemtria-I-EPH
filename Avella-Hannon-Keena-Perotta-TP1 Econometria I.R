#Paquetes necesarios
install.packages(c("tidyverse", "AER", "eph", "modelsummary"), dependencies = TRUE)

library(tidyverse)    
library(AER)
library(eph)
library(modelsummary) 

rm(list = ls())

#Base de microdatos EPH cuarto cuatrimestre 2025 (individual)
eph_4_25 <- get_microdata(
  year = 2025,
  period = 4,
  type = "individual")

#Pasamos los nombres de las variables a minuscula
names(eph_4_25) <- tolower(names(eph_4_25))

#Filtramos la base
eph_4_25_2 <- eph_4_25 %>%
filter(ch03 == 01, #Jefes de hogar
p21 > 0, #Aquellos que tienen ingresos
aglomerado == 32, #Viven en el CABA
ch06 >= 25 & ch06 <= 65) %>%
mutate(ingreso = p21,
edad = ch06,
estado_civil = ch07,
sexo = ch04)

#Creamos una variable del ingreso en logaritmos
eph_4_25_2 <- eph_4_25_2 %>%
  mutate(log_ingreso = log(p21))


#Salario promedio mensual

ejercicio1 <- eph_4_25_2 %>%
summarise(
"Tabla 1: Salario Promedio Mensual" = weighted.mean(as.numeric(ingreso), as.numeric(pondiio, na.rm = TRUE)))

print(ejercicio1)

#Tabla ejercicio1
datasummary_df(
ejercicio1, fmt = 0)

#Salario promedio mensual por sexo

#Definiendo Mujer como base de la variable sexo
eph_4_25_2 <- eph_4_25_2 %>%
mutate(
sexo = if_else(ch04 == 2, 0, 1))
    
#Creamos la variable sexo1 donde aparece la etiqueta "Mujer" y "Hombre"
eph_4_25_2 <- eph_4_25_2 %>% 
  mutate(Sexo = ifelse(sexo == 1, "Hombre", "Mujer"))

#Ingreso promedio por sexo
tabla_sexo <- eph_4_25_2 %>%
      group_by(Sexo) %>%
      summarise(
        "Conteo" = n(),
       "Ingreso Promedio" = weighted.mean(as.numeric(ingreso), as.numeric(pondiio, na.rm = TRUE)))

#Tabla ingreso por sexo
datasummary_df(
  tabla_sexo,
  title = "Tabla 2: Salario Promedio Mensual por Sexo (CABA)",
  fmt = 0)



#Creamos la variable rango_edad por grupos de a 10 años
eph_4_25_2 <- eph_4_25_2 %>% 
  mutate( rango_edad = case_when(
    edad >= 25 & edad <= 34 ~ "25-34",
    edad >= 35 & edad <= 44 ~ "35-44",
    edad >= 45 & edad <= 54 ~ "45-54",
    edad >= 55 & edad <= 65 ~ "55-65"))


#Ingreso promedio por grupo de edad
tabla_edad <- eph_4_25_2 %>%
  group_by(rango_edad) %>%
  summarise(
    "Salario promedio" = weighted.mean(as.numeric(ingreso), as.numeric(pondiio, na.rm = TRUE)),
    Obs = n())

#Tabla de ingreso por rango de edad
datasummary_df(tabla_edad, title = "Tabla 3: Salario promedio por grupo de edad", fmt = 0)


# ---- AÑOS DE EDUCACIÓN ----

eph_4_25_2 <- eph_4_25_2 %>%
  mutate(aeduc = case_when(
    
    # Nunca asistió / menores de 2 / educación especial
    ch10 == 3                          ~ 0,
    edad < 2                           ~ 0,
    ch12 == 9                          ~ 0,
    
    # No completaron el nivel
    # Jardín/preescolar
    ch13 == 2 & ch12 == 1             ~ 0,
    
    # Primaria incompleta
    ch13 == 2 & ch12 == 2 & ch14 == 0 ~ 1,
    ch13 == 2 & ch12 == 2 & ch14 == 1 ~ 2,
    ch13 == 2 & ch12 == 2 & ch14 == 2 ~ 3,
    ch13 == 2 & ch12 == 2 & ch14 == 3 ~ 4,
    ch13 == 2 & ch12 == 2 & ch14 == 4 ~ 5,
    ch13 == 2 & ch12 == 2 & ch14 == 5 ~ 6,
    ch13 == 2 & ch12 == 2 & ch14 == 6 ~ 7,
    
    # EGB incompleto
    ch13 == 2 & ch12 == 3 & ch14 == 0 ~ 1,
    ch13 == 2 & ch12 == 3 & ch14 == 1 ~ 2,
    ch13 == 2 & ch12 == 3 & ch14 == 2 ~ 3,
    ch13 == 2 & ch12 == 3 & ch14 == 3 ~ 4,
    ch13 == 2 & ch12 == 3 & ch14 == 4 ~ 5,
    ch13 == 2 & ch12 == 3 & ch14 == 5 ~ 6,
    ch13 == 2 & ch12 == 3 & ch14 == 6 ~ 7,
    ch13 == 2 & ch12 == 3 & ch14 == 7 ~ 8,
    ch13 == 2 & ch12 == 3 & ch14 == 8 ~ 9,
    
    # Secundaria incompleta
    ch13 == 2 & ch12 == 4 & ch14 == 0 ~ 8,
    ch13 == 2 & ch12 == 4 & ch14 == 1 ~ 9,
    ch13 == 2 & ch12 == 4 & ch14 == 2 ~ 10,
    ch13 == 2 & ch12 == 4 & ch14 == 3 ~ 11,
    ch13 == 2 & ch12 == 4 & ch14 == 4 ~ 12,
    ch13 == 2 & ch12 == 4 & ch14 == 5 ~ 13,
    
    # Polimodal incompleto
    ch13 == 2 & ch12 == 5 & ch14 == 0 ~ 10,
    ch13 == 2 & ch12 == 5 & ch14 == 1 ~ 11,
    ch13 == 2 & ch12 == 5 & ch14 == 2 ~ 12,
    ch13 == 2 & ch12 == 5 & ch14 == 3 ~ 13,
    
    # Terciario incompleto
    ch13 == 2 & ch12 == 6 & ch14 == 0          ~ 13,
    ch13 == 2 & ch12 == 6 & ch14 == 1          ~ 14,
    ch13 == 2 & ch12 == 6 & ch14 >= 2 & ch14 < 98 ~ 15,
    
    # Universitario incompleto
    ch13 == 2 & ch12 == 7 & ch14 == 0          ~ 13,
    ch13 == 2 & ch12 == 7 & ch14 == 1          ~ 14,
    ch13 == 2 & ch12 == 7 & ch14 == 2          ~ 15,
    ch13 == 2 & ch12 == 7 & ch14 == 3          ~ 16,
    ch13 == 2 & ch12 == 7 & ch14 == 4          ~ 17,
    ch13 == 2 & ch12 == 7 & ch14 >= 5 & ch14 < 98 ~ 18,
    
    # Posgrado incompleto
    ch13 == 2 & ch12 == 8 & ch14 == 0          ~ 19,
    ch13 == 2 & ch12 == 8 & ch14 == 1          ~ 20,
    ch13 == 2 & ch12 == 8 & ch14 == 2          ~ 21,
    ch13 == 2 & ch12 == 8 & ch14 >= 3 & ch14 < 98 ~ 22,
    
    # Completaron el nivel
    ch13 == 1 & ch12 == 1                      ~ 1,
    ch13 == 1 & ch12 == 2                      ~ 8,
    ch13 == 1 & ch12 == 3                      ~ 11,
    ch13 == 1 & (ch12 == 4 | ch12 == 5)        ~ 13,
    ch13 == 1 & ch12 == 6                      ~ 16,
    ch13 == 1 & ch12 == 7                      ~ 19,
    ch13 == 1 & ch12 == 8                      ~ 23,
    
    TRUE ~ NA_real_))


#Punto 2
#Regresion lineal del salario (log) controlando por educ (cualitativa usando dummy), sexo, edad (continua) y estado civil

#Creacion de variables Dummy para educacion (Base: secundario completo)
#Ponemos a secundario_compl como base

eph_4_25_2 <- eph_4_25_2 %>%
  mutate(nivel_ed = case_when
         (nivel_ed == 4 ~ 0,
           nivel_ed == 1 ~ 1,
           nivel_ed == 2 ~ 2,
           nivel_ed == 3 ~ 3,
           nivel_ed == 5 ~ 5,
           nivel_ed == 6 ~ 6,
           nivel_ed == 7 ~ 7))



eph_4_25_2 <- eph_4_25_2 %>% 
  mutate(primaria_inc = if_else(
    nivel_ed == 1, 1, 0))  

eph_4_25_2 <- eph_4_25_2 %>% 
  mutate(primaria_compl = if_else(
    nivel_ed == 2, 1, 0))  

eph_4_25_2 <- eph_4_25_2 %>% 
  mutate(secundaria_inc = if_else(
    nivel_ed == 3, 1, 0))  

eph_4_25_2 <- eph_4_25_2 %>% 
  mutate(universidad_inc = if_else(
    nivel_ed == 5, 1, 0))  

eph_4_25_2 <- eph_4_25_2 %>% 
  mutate(universidad_compl = if_else(
    nivel_ed == 6, 1, 0))  

eph_4_25_2 <- eph_4_25_2 %>% 
  mutate(sin_instruccion = if_else(
    nivel_ed == 7, 1, 0))



#Defino soltero (5) como base (0)
eph_4_25_2 <- eph_4_25_2 %>%
  mutate(estado_civil_1 = case_when
         (estado_civil == 5 ~ 0,
           estado_civil == 1 ~ 1,
           estado_civil == 2 ~ 2,
           estado_civil == 3 ~ 3,
           estado_civil == 4 ~ 4))

#Creacion de variables Dummy para estado_civil
eph_4_25_2 <- eph_4_25_2 %>% 
  mutate(unido = if_else(
    estado_civil_1 == 1, 1, 0))  

eph_4_25_2 <- eph_4_25_2 %>%      
  mutate(viudo = if_else(
    estado_civil_1 == 4, 1, 0)) 

eph_4_25_2 <- eph_4_25_2 %>%
  mutate(separado = if_else(
    estado_civil_1 == 3, 1, 0))

eph_4_25_2 <- eph_4_25_2 %>%
  mutate(casado = if_else(
    estado_civil_1 == 2, 1, 0))



#Ejercicio2 usando variables Dummmy
ejercicio2 <- lm(log_ingreso ~ primaria_inc + primaria_compl + secundaria_inc + 
                  universidad_inc + universidad_compl + sin_instruccion + sexo + edad + 
                  viudo + unido + separado + casado, data = eph_4_25_2, weights = pondiio)

summary(ejercicio2)


#Tabla de la regresion modelo1
modelsummary(
  list("Educ" = ejercicio2),
  title = "Tabla 4: Regresion con educacion como variables dummy",
  stars     = TRUE,
  statistic = NULL,
  fmt       = 3,
  coef_rename = c(
    "edad" = "Edad",
    "primaria_inc" = "Primaria Incompleta",
    "primaria_compl" = "Primaria Completa",
    "secundaria_inc" = "Secundaria Incompleta",
    "universidad_inc" = "Sup. Univ. Incompleta",
    "universidad_compl" = "Sup. Univ. Completa",
    "(Intercept)" = "Constante",
    "sexo" = "Sexo",
    "viudo" = "Viudo",
    "unido" = "Unido",
    "separado" = "Separado",
    "casado" = "Casado"
  ),
  gof_map = c("nobs", "adj.r.squared"))

#Notar que desaparece "primaria_inc" y "sin_instruccion" ya que no hay ninguna observacion luego de filtrar la muestra

#Ejercicio2.1 usando educacion como variable continua
ejercicio2.1 <- lm(log_ingreso ~ aeduc + sexo + edad + 
viudo + unido + separado + casado, data = eph_4_25_2, weights = pondiio)

summary(ejercicio2.1)

#Tabla regresion modelo1.2
modelsummary(
  list("Educ" = ejercicio2.1),
  title = "Tabla 5: Regresion con educacion como variable continua",
  stars     = TRUE,
  statistic = NULL,
  fmt       = 3,
  coef_rename = c(
    "edad" = "Edad",
    "aeduc" = "Años_Edcuacion",
    "(Intercept)" = "Constante",
    "sexo" = "Sexo",
    "viudo" = "Viudo",
    "unido" = "Unido",
    "separado" = "Separado",
    "casado" = "Casado"
  ),
  gof_map = c("nobs", "adj.r.squared"))


#Punto 3
ejercicio3 <- lm(log_ingreso ~ primaria_compl*sexo + secundaria_inc*sexo + 
                universidad_inc*sexo + universidad_compl*sexo + sexo + edad + 
                viudo + unido + separado + casado, data = eph_4_25_2, weights = pondiio)

summary(ejercicio3)

#Tabla ejercicio3
modelsummary(
  list("Educ" = ejercicio3),
  title = "Tabla 6: Regresion con interaccion entre sexo y educacion como variable dummy",
  stars     = TRUE,
  statistic = NULL,
  fmt       = 3,
  coef_rename = c(
    "edad" = "Edad",
    "primaria_compl*sexo" = "Primaria Completa:Hombre ",
    "secundaria_inc*sexo" = "Secundaria Incompleta:Hombre",
    "universidad_inc*sexo" = "universidad Incompleta:Hombre ",
    "universidad_compl*sexo" = "Universidad Completa:Hombre ",
    "primaria_inc" = "Primaria Incompleta",
    "primaria_compl" = "Primaria Completa",
    "secundaria_inc" = "Secundaria Incompleta",
    "universidad_inc" = "Sup. Univ. Incompleta",
    "universidad_compl" = "Sup. Univ. Completa",
    "(Intercept)" = "Constante",
    "sexo" = "Sexo",
    "viudo" = "Viudo",
    "unido" = "Unido",
    "separado" = "Separado",
    "casado" = "Casado"
  ),
  gof_map = c("nobs", "adj.r.squared"))

#Ocurre lo mismo que con el ejercicio2, aca tampoco aparecem "primaria_inc:sexo", "sexo:sin_instruccion"

#Realizamos el test de hipótesis conjunta (Test F)
test_f <- linearHypothesis(ejercicio3, c(
  "primaria_compl:sexo = 0",
  "sexo:secundaria_inc = 0",
  "sexo:universidad_inc = 0",
  "sexo:universidad_compl = 0"))

print(test_f)

#Punto 4
#Creamos un perfil que contiene el rango de edad y el sexo
perfil <- expand.grid(
        edad = 25:65,
        sexo = c(0, 1),
        universidad_compl = 1,    
        primaria_inc = 0, 
        primaria_compl = 0, 
        secundaria_inc = 0, 
        universidad_inc = 0, 
        sin_instruccion = 0,
        casado = 1,               
        viudo = 0, 
        unido = 0, 
        separado = 0)
              
#Agregamos sexo1 que contiene etiquetas
perfil <- perfil %>%
      mutate(sexo1 = ifelse(sexo == 1, "Hombre", "Mujer"))

#Prediccion en logaritmos
predicciones_log <- predict(ejercicio2, newdata = perfil, interval = "confidence", level = 0.95)
              
#Unimos las predicciones al perfil
resultados <- cbind(perfil, predicciones_log)
              
#Transformamos los resultados en logaritmos utilizando la funcion exponencial
resultados <- resultados %>%
       mutate(
        salario_estimado = exp(fit),     # fit es la predicción puntual
        limite_inf = exp(lwr),           #lwr es el límite inferior del intervalo
        limite_sup = exp(upr)            # upr es el límite superior del intervalo
                )

View(resultados)              
              
              
#Grafico (lineal) de la prediccion del salario por edad entre hombres y mujeres
ggplot(resultados, aes(x = edad, y = salario_estimado, color = sexo1, fill = sexo1)) +
            # La línea central de la estimación
            geom_line(linewidth = 1) + 
            # El intervalo de confianza sombreado 
            geom_ribbon(aes(ymin = limite_inf, ymax = limite_sup), alpha = 0.2, color = NA) +
            # Etiquetas y títulos
            labs(
                title = "Salario Promedio Mensual Estimado por Edad y Sexo",
                  subtitle = "Perfil: Universitario Completo, Casado",
                x = "Edad",
                y = "Salario (Pesos)",
                color = "Sexo",
                fill = "Sexo"
                ) +
              theme_minimal() +
              scale_y_continuous(labels = scales::comma)
              
#Notar que el grafico es lineal y que encuentra el salario maximo en la edad de 25 años (Coeficienta edad negativo)
#Por lo que mas preciso seria utilizar un modelo cuadratico donde aparezca la variable edad^2

            
#Usando edad^2
              
#Corremos una regresion cuadratica con edad^2
modelo_cuadratico <- lm(log_ingreso ~ primaria_inc + primaria_compl + secundaria_inc + 
                  universidad_inc + universidad_compl + sin_instruccion + sexo + 
                  edad + I(edad^2) + viudo + unido + separado + casado, 
                  data = eph_4_25_2, weights = pondiio)
              
              
#Extraemos las predicciones del modelo lineal y las pasamos a pesos
              pred_lineal <- predict(ejercicio2, newdata = perfil, interval = "confidence", level = 0.95)
              df_lineal <- cbind(perfil, pred_lineal) %>%
                mutate(
                  salario = exp(fit),
                  lwr_salario = NA,
                  upr_salario = NA,
                  modelo = "1. Lineal (Original)"
                )
              
#Extraemos las predicciones del modelo cuadratico y las pasamos a pesos
pred_cuadratica <- predict(modelo_cuadratico, newdata = perfil, interval = "confidence", level = 0.95)
              df_cuadratica <- cbind(perfil, pred_cuadratica) %>%
                mutate(
                  salario = exp(fit),
                  lwr_salario = exp(lwr),
                  upr_salario = exp(upr),
                  modelo = "2. Cuadrático")
              
#Unimos ambas tablas para graficarlas juntas
df_comparacion <- dplyr::bind_rows(df_lineal, df_cuadratica)
              
#Generamos el grafico comparativo
ggplot(df_comparacion, aes(x = edad, y = salario, color = sexo1, linetype = modelo)) +
  geom_ribbon(
    data = subset(df_comparacion, modelo == "2. Cuadrático"),
    aes(ymin = lwr_salario, ymax = upr_salario, fill = sexo1),
    alpha = 0.2,       # Transparencia baja para que no tape las líneas
    color = NA,        # Sin borde para la banda (queda más prolijo)
    show.legend = FALSE # Evita que el cuadrado del ribbon ensucie la leyenda
  ) +
  geom_line(linewidth = 1.2) +
                labs(
                  title = "Evolución Salarial: Lineal vs. Curva",
                  subtitle = "Perfil: Universitario Completo, Casado",
                  x = "Edad",
                  y = "Salario Estimado Mensual (Pesos)",
                  color = "Sexo",
                  linetype = "Especificación"
                ) +
                theme_minimal() +
                scale_y_continuous(labels = scales::comma) +
                theme(
                  plot.title = element_text(face = "bold", size = 14),
                  legend.position = "bottom")
              
              
#Para encontrar a que edad se obtiene el mayor salario
#Extraemos los coeficientes del modelo cuadratico
coeficientes <- coef(modelo_cuadratico)
              
#Nombramos los coeficientes de edad y edad^2
beta1 <- coeficientes["edad"]
beta2 <- coeficientes["I(edad^2)"]
              
#Calculamos la edad máxima usando la fórmula de CPO
edad_maxima <- -beta1 / (2 * beta2)
              
#Edad maxima
cat("La edad donde se maximiza el salario es a los:", round(edad_maxima, 1), "años\n")


# Nuevo perfil con la carrera terminada
perfil_2 <- data.frame(
  edad = 25,                
  sexo = 1, #Hombre                 
  
  universidad_compl = 1,
  primaria_inc = 0, primaria_compl = 0, secundaria_inc = 0, 
  universidad_inc = 0, sin_instruccion = 0,
  casado = 0, viudo = 0, unido = 0, separado = 0)

# Prediccion en logaritmos
prediccion_log_2 <- predict(ejercicio2, newdata = perfil_2, interval = "confidence", level = 0.95)

# Pasamos de logaritmo a pesos
salario_esperado <- exp(prediccion_log_2)

print(salario_esperado)

#Tabla del salario esperado
tabla_prediccion <- as.data.frame(salario_esperado)

colnames(tabla_prediccion) <- c("Salario Esperado", "Límite Inferior", "Límite Superior")

datasummary_df(
  tabla_prediccion,
  fmt = 0,
  title = "Predicción del Salario Mensual (Universitario Completo, Soltero)",
  notes = "Estimación con intervalo de confianza (95%)")

              
              