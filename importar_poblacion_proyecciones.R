# descargar proyecciones de población censo chile
# https://www.ine.gob.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion

#descargar
download.file("https://www.ine.gob.cl/docs/default-source/proyecciones-de-poblacion/cuadros-estadisticos/base-2017/estimaciones-y-proyecciones-2002-2035-comunas.xlsx", 
              destfile = "datos/censo_proyecciones_poblacion.xlsx"
)
