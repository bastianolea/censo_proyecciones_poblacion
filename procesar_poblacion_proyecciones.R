library(dplyr)
library(tidyr)
library(stringr)

proyecciones_0 <- readxl::read_excel("censo_proyecciones_poblacion.xlsx")

#limpiar y convertir a formato largo (una observación por fila)
proyecciones_long <- proyecciones_0 |> 
    janitor::clean_names() |> 
    pivot_longer(cols = starts_with("poblacion_"), 
                 names_to = "año", values_to = "población") |> 
    rename(cut_region = region, region = nombre_region, 
           cut_provincia = provincia, provincia = nombre_provincia,
           cut_comuna = comuna, comuna = nombre_comuna)

# población por año y comuna ----
proyecciones_año <- proyecciones_long |> 
    group_by(cut_region, region, 
             cut_provincia, provincia,
             cut_comuna, comuna, 
             año) |> 
    summarize(población = sum(población)) |> 
    mutate(año = str_extract(año, "\\d{4}") |> as.numeric()) |> 
    ungroup()

proyecciones_año

## guardar ----
proyecciones_año |> 
    arrow::write_parquet("resultados/censo_proyecciones_año.parquet")

proyecciones_año |> 
    write.csv2("resultados/censo_proyecciones_año.csv")



# población por año, género, edad y comuna ----
proyecciones_edad_genero <- proyecciones_long |> 
    mutate(año = str_extract(año, "\\d{4}") |> as.numeric(),
           género = ifelse(sexo_1_hombre_2_mujer == 1, "masculino", "femenino")) |> 
    select(-sexo_1_hombre_2_mujer)

proyecciones_edad_genero |> 
    arrow::write_parquet("resultados/censo_proyecciones_año_edad_genero.parquet")


# población edad promedio ----
proyecciones_edad <- proyecciones_edad_genero |> 
    group_by(cut_region, region, cut_provincia, provincia, cut_comuna, comuna,
             año, edad) |> 
    summarize(poblacion = sum(población, na.rm = T))

# edad promedio por comuna
proyecciones_edad_promedio <- proyecciones_edad |> 
    group_by(cut_region, region, cut_provincia, provincia, cut_comuna, comuna,
             año) |>
    summarize(poblacion = weighted.mean(edad, poblacion))

write.csv2(proyecciones_edad_promedio, "resultados/censo_proyecciones_edad_prom.csv")


# población menor de edad y tercera edad ----
# porcentaje de cada comuna que son menores de 18
proyecciones_porcentaje_menores <- proyecciones_edad |> 
    mutate(menor = ifelse(edad < 18, "menor", "mayor")) |> 
    group_by(cut_region, region, cut_provincia, provincia, cut_comuna, comuna,
             año, menor) |> 
    summarize(poblacion = sum(poblacion)) |> 
    ungroup() |> 
    pivot_wider(names_from = menor, values_from = poblacion) |> 
    mutate(menores_porcentaje = menor/mayor)

write.csv2(proyecciones_porcentaje_menores, "resultados/censo_proyecciones_edad_menores.csv")


# porcentaje de cada comuna que son mayores de 60
proyecciones_porcentaje_terceraedad <- proyecciones_edad |> 
    mutate(mayor = ifelse(edad >= 60, "mayor", "otros")) |> 
    group_by(cut_region, region, cut_provincia, provincia, cut_comuna, comuna,
             año, mayor) |> 
    summarize(poblacion = sum(poblacion)) |> 
    ungroup() |> 
    pivot_wider(names_from = mayor, values_from = poblacion) |> 
    mutate(menores_porcentaje = mayor/otros)

write.csv2(proyecciones_porcentaje_terceraedad, "resultados/censo_proyecciones_edad_adultomayor.csv")