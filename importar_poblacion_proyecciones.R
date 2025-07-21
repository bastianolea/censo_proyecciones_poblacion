# descargar proyecciones de población censo chile
# https://www.ine.gob.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion

# crear carpetas
dir.create("datos")
dir.create("datos/datos_originales/")

# descargar
download.file("https://www.ine.gob.cl/docs/default-source/proyecciones-de-poblacion/cuadros-estadisticos/base-2017/estimaciones-y-proyecciones-2002-2035-comunas.xlsx", 
              destfile = "datos/datos_originales/censo_proyecciones_poblacion.xlsx"
)
