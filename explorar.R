library(arrow)
library(dplyr)

datos <- arrow::read_parquet("datos/datos_procesados/censo_proyecciones_año.parquet")

datos |> 
    summarize(min(año),
              max(año))