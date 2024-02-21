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
    arrow::write_parquet("censo_proyecciones_año.parquet")

#población por año, género, edad y comuna ----
proyecciones_2 <- proyecciones_long |> 
    mutate(año = str_extract(año, "\\d{4}") |> as.numeric(),
           género = ifelse(sexo_1_hombre_2_mujer == 1, "masculino", "femenino")) |> 
    select(-sexo_1_hombre_2_mujer)

proyecciones_2 |> 
    arrow::write_parquet("censo_proyecciones_año_edad_genero.parquet")
