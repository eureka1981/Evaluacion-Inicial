library(vroom)
library(tidyverse)


# El principal conjunto de datos que usaremos son las lesiones `injuries`,

injuries <- vroom::vroom("~/Prueba/injuries.tsv")
## Le agrego esta línea para convertirla en base de datos
injuries <-as.data.frame(injuries )
injuries 

# Cada fila representa un solo accidente con 10 variables:
# * `trmt_date` es la fecha en que se vio a la persona en el hospital
# * ``age`, `sex`, y `race`
# * `body_part` es la ubicación de la lesión en el cuerpo
# * `diag` da el diagnóstico básico
# * `prod_code` asociado con la lesión.
# * La variable `weight` es el peso estadístico
# * La variable `narrative` es una breve historia


library(readxl)
products <- read_excel("~/Prueba/products.xlsx")
products

population <- read_excel("~/Prueba/population.xlsx")
population


### Exploración

# Comenzaremos mirando un producto con una historia interesante: 649, "toilets".

selected <- injuries %>% filter(prod_code == 649)
nrow(selected)


# Tablas ------------------------------------------------------------------
# algunos resúmenes básicos


selected %>%
  count(location, wt = weight, sort = TRUE)


selected %>%
  count(body_part, wt = weight, sort = TRUE)


selected %>%
  count(diag, wt = weight, sort = TRUE)

# También podemos explorar el patrón según la edad y el sexo.

summary <- selected %>%
  count(age, sex, wt = weight)
summary

summary %>%
  ggplot(aes(age, n, colour = sex)) +
  geom_line() +
  labs(y = "Número estimado de lesiones")





summary <- selected %>%
  count(age, sex, wt = weight) %>%
  left_join(population, by = c("age", "sex")) %>%
  mutate(rate = n / population * 1e4)

summary

# Al trazar la tasa,
# una tendencia sorprendentemente diferente después de los 50 años:
# Esto se debe a que las mujeres tienden a vivir más que los hombres,

summary %>%
  ggplot(aes(age, rate, colour = sex)) +
  geom_line(na.rm = TRUE) +
  labs(y = "Lesiones por cada 10.000 personas")


