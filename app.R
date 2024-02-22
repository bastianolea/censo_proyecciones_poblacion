library(shiny)
library(dplyr)
library(arrow)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(gt)
library(gtExtras)
library(thematic)
library(bslib)
library(glue)
library(colorspace)

color_principal = "#365c8d"
color_destacado = "#3a87c8"
color_fondo = "#FFFFFF"
color_negativo = "#e5084b"
color_masculino = "#770072" |> lighten(0.2)
color_femenino = "#3a1582" |> lighten(0.2)


proyecciones <- arrow::read_parquet("censo_proyecciones_año.parquet")

proyecciones_edad <- arrow::read_parquet("censo_proyecciones_año_edad_genero.parquet")

options(spinner.type = 4, spinner.color = color_destacado)

#tema automático
thematic_shiny(font = "auto", bg = color_fondo, fg = color_principal, accent = color_destacado)

css <- function(text) {
    tags$style(glue(text, .open = "{{", .close = "}}"))
}

ui <- fluidPage(
    theme = bs_theme(
        bg = color_fondo, fg = color_principal, primary = color_destacado
    ),
    
    # css ----
    css(".explicacion {
      font-size: 80%; opacity: 1; line-height: 1.3;
      }"),
    
    fluidRow(
        #título ----
        div(style = "padding-top: 12px; padding-bottom: 20px;",
            
            titlePanel(
                h1("Censo de población: proyecciones", 
                   style = glue("font-weight: bold; color: {color_destacado}")),
                windowTitle = "Proyecciones Censo"),
            
            div(style = "margin-bottom: 8px; font-size: 80%;",
                
                markdown("Visualizador de datos oficiales de proyecciones poblacionales realizadas por el [Instituto Nacional de Estadísticas](https://www.ine.gob.cl) de Chile"),
                
                em(tags$a("Bastián Olea Herrera", 
                          href = "http://bastian.olea.biz",
                          target = "_blank"))
            ),
            
            div(style = "padding-top: 18px;",
                markdown("Aplicación web que visualiza los datos oficiales del [Instituto Nacional de Estadísticas](https://www.ine.gob.cl) de Chile sobre [proyecciones de población](https://www.ine.gob.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion); es decir, estimaciones del crecimiento poblacional hacia el futuro, a partir de los datos obtenidos en los censos oficiales.")
            ),
            hr()
        )
    ),
    
    # evolución ----
    
    fluidRow(
        column(12, style = "margin-top: -20px;",
               
               ## regional ----
               fluidRow(
                   column(7, 
                          h2("Evolución de la población regional"),
                          p("En este gráfico puedes analizar los cambios poblacionales proyectados para las regiones de Chile que elijas. Los puntos indican mediciones de población realizadas por censos nacionales de población y vivienda, mientras que el resto de las líneas representan proyecciones de dichas mediciones."),
                          
                          p(class = "explicacion",
                            "Selecciona una o más regiones para incluirlas en el gráfico."),
                          
                          pickerInput("regiones",
                                      label = h4("Regiones"),
                                      width = "100%",
                                      multiple = TRUE,
                                      choices = NULL,
                                      options = list(width = FALSE,
                                                     noneSelectedText = "Sin selección")
                          ),
                          
                          div(style = "overflow-x: scroll;",
                              plotOutput("grafico_proyeccion_regiones", width = "700px") |> withSpinner()
                          ),
                          br()
                   ),
                   
                   ### comparar años ----
                   column(5,
                          
                          h2("Cambios en la población regional"),
                          p("Esta tabla detalla una lista de las regiones seleccionadas, con su población correspondiente para los años que elijas, junto a el cambio porcentual entre ambas fechas."),
                          
                          p(class = "explicacion",
                            "Selecciona dos años para ajustar la comparación entre poblaciones regionales."),
                          
                          sliderInput("comparar_años", 
                                      label = h4("Seleccione dos años a comparar"), 
                                      min = min(proyecciones_edad$año), max = max(proyecciones_edad$año),
                                      value = c(2002, 2023), ticks = T, sep = "", width = "100%"),
                          
                          div(style = "margin-top: 20px;",
                              gt_output("comparacion_años_regiones") |> withSpinner()
                          )
                   )
               ),
               hr()
               
        )
    ),
    
    ## comunal ----
    fluidRow(
        column(12, 
               
               fluidRow(
                   column(7, 
                          h2("Evolución de la población comunal"),
                          p("Visualiazción de los cambios poblacionales proyectados para las comunas del país."),
                          
                          p(class = "explicacion",
                            "A partir de las regiones seleccionadas más arribal, elige una o varias comunas que serán incluidas en el gráfico."),
                          
                          pickerInput("comunas",
                                      label = h4("Comunas"),
                                      width = "100%",
                                      multiple = TRUE,
                                      choices = NULL,
                                      options = list(maxOptions = 8, 
                                                     maxOptionsText = "Máximo 8",
                                                     noneSelectedText = "Sin selección",
                                                     width = FALSE)
                          ),
                          
                          div(style = "overflow-x: scroll;",
                              plotOutput("grafico_proyeccion_comunas", width = "700px") |> withSpinner()
                          ),
                          br()
                   ),
                   
                   # comparar años ----
                   column(5,
                          
                          h2("Cambios en la población comunal"),
                          p("Esta tabla detalla una lista de las comunas seleccionadas, con su población correspondiente para los años que elijas, junto a el cambio porcentual entre ambas fechas."),
                          
                          p(class = "explicacion",
                            "Selecciona dos años para ajustar la comparación entre poblaciones."),
                          
                          sliderInput("comparar_años", 
                                      label = h4("Seleccione dos años a comparar"), 
                                      min = min(proyecciones_edad$año), max = max(proyecciones_edad$año),
                                      value = c(2002, 2023), ticks = T, sep = "", width = "100%"),
                          
                          div(style = "margin-top: 20px;",
                              gt_output("comparacion_años_comunas") |> withSpinner()
                          )
                   )
               ),
               
               hr()
        )
    ),
    
    
    
    
    # pirámides ----
    fluidRow(
        column(12,
               h2("Pirámides poblacionales"),
               p("Vuelve a elegir una región y una comuna para poder visualizar su pirámide poblacional, que corresponde a la distribución de la población de dicha comuna, pero separada en grupos de edad y género."),
               
               p(class = "explicacion",
                 "Elige una región, luego una comuna de dicha región, y ajusta el año para ver las diferencias en la pirámides poblacionales."),
               
               sliderInput("año_piramide", label = h4("Año"), 
                           min = min(proyecciones_edad$año), max = max(proyecciones_edad$año),
                           value = 2023, ticks = T, sep = "", width = "100%"),
               
        ),
    ),
    
    fluidRow(
        ## región ----
        column(6,
               pickerInput("region_piramide",
                           label = h4("Región"),
                           width = "100%",
                           multiple = F,
                           choices = NULL,
                           options = list(width = FALSE,
                                          noneSelectedText = "Sin selección")
                           
               ),
               fluidRow(
                   column(12, style = "margin: auto; max-width: 500px; overflow-x: scroll;",
                          plotOutput("grafico_piramide_region", width = "500px") |> withSpinner()
                          
                   )
               )
        ),
        
        ## comuna ----
        column(6,
               pickerInput("comuna_piramide",
                           label = h4("Comuna"),
                           width = "100%",
                           multiple = F,
                           choices = NULL,
                           options = list(noneSelectedText = "Sin selección",
                                          width = FALSE)
               ),
               
               
               fluidRow(
                   column(12, style = "margin: auto; max-width: 500px; overflow-x: scroll;",
                          plotOutput("grafico_piramide_comuna", width = "500px") |> withSpinner()
                          
                   )
               )
        )
    ),
    
    
    ## firma ----
    fluidRow(
        column(12, style = "opacity: 0.6; font-size: 80%;",
               hr(),
               
               markdown("Diseñado y programado por [Bastián Olea Herrera.](https://bastian.olea.biz)"),
               
               markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
               
               markdown("Fuente de los datos: [Instituto Nacional de Estadísticas, Chile](https://www.ine.gob.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion)"),
               
               markdown("Código de fuente de esta app y de la obtención de los datos [disponibles en GitHub.](https://github.com/bastianolea/censo_proyecciones_poblacion)"),
               
               div(style = "height: 20px")
               
        )
    )
    
    
)


server <- function(input, output, session) {
    
    #función que filtra las comunas de las regiones seleccionadas
    filtrar_comunas <- function(datos, seleccion) {
        if ("Todas las regiones" %in% seleccion) {
            lista_comunas <- split(unique(datos$comuna) |> sort(), 
                                   unique(datos$region))
        } else {
            # browser()
            proyecciones_region <- datos |> 
                filter(region %in% seleccion) |> 
                select(region, comuna) |> 
                arrange(region, comuna) |> 
                distinct()
            
            lista_comunas <- split(proyecciones_region, ~region) |> lapply(pull, comuna)
        }
    }
    
    
    #selectores ----
    ## regiones ----
    updatePickerInput(session, "regiones",
                      choices = c(as.character(unique(proyecciones$region))),
                      selected = c("Metropolitana de Santiago", "Valparaíso", "Biobío", "Maule"),
                      options = list(width = FALSE, noneSelectedText = "Sin selección")
    )
    updatePickerInput(session, "region_piramide",
                      choices = c(as.character(unique(proyecciones$region))),
                      selected = "Metropolitana de Santiago",
                      options = list(width = FALSE, noneSelectedText = "Sin selección")
    )
    
    ## filtrar comunas ----
    #filtrar comunas según regiones elegidas
    lista_comunas <- reactive(filtrar_comunas(proyecciones, input$regiones))
    lista_comunas_piramide <- reactive(filtrar_comunas(proyecciones, input$region_piramide))
    
    
    ## comunas ----
    #pone las comunas en el selector de comunas según la región elegida
    
    ### evolución ----
    observeEvent(input$regiones, {
        req(length(input$regiones) > 0)
        
        updatePickerInput(session,
                          inputId = "comunas",
                          choices = lista_comunas(),
                          selected = c("La Pintana", "Puente Alto", "Ñuñoa", "Vitacura", "Independencia", "La Florida"),
                          options = list(maxOptions = 8, 
                                         maxOptionsText = "Máximo 8",
                                         width = FALSE,
                                         noneSelectedText = "Sin selección")
        )
    })
    
    ### pirámide ----
    observeEvent(input$region_piramide, {
        req(length(input$region_piramide) > 0)
        
        updatePickerInput(session,
                          inputId = "comuna_piramide",
                          choices = lista_comunas_piramide(),
                          selected = "Puente Alto",
                          options = list(width = FALSE,
                                         noneSelectedText = "Sin selección")
        )
    })
    #—----
    
    # datos ----
    ## datos evolución ----
    
    datos_regiones <- reactive({
        req(length(input$regiones) > 0)
        proyecciones |> 
            filter(region %in% input$regiones)
    })
    
    ### regiones ----
    proyecciones_regiones <- reactive({
        req(length(input$regiones) > 0)
        
        datos_regiones() |> 
            mutate(proyeccion = ifelse(año > 2017, "futuro", "pasado"),
                   proyeccion_b = ifelse(año > 2016, "futuro", "pasado")) |> 
            mutate(año_censal = ifelse(año %in% c(2002, 2017), "censo", "proyección"))
    })
    
    ### comunas ----
    proyecciones_comunas <- reactive({
        req(length(input$comunas) > 0)
        
        proyecciones |> 
            filter(comuna %in% input$comunas) |> 
            mutate(proyeccion = ifelse(año > 2017, "futuro", "pasado"),
                   proyeccion_b = ifelse(año > 2016, "futuro", "pasado")) |> 
            mutate(año_censal = ifelse(año %in% c(2002, 2017), "censo", "proyección"))
    })
    
    ## datos pirámide ----
    
    datos_edad_region <- reactive({
        req(length(input$region_piramide) > 0)
        # browser()
        proyecciones_edad |> 
            filter(año == input$año_piramide) |>
            filter(region == input$region_piramide)
    })
    
    ### comuna ----
    proyecciones_piramide_comuna <- reactive({
        datos_edad_region() |> 
            filter(comuna == input$comuna_piramide) |> 
            # crear grupos de edad
            mutate(edad_categoría = cut(edad, breaks = c(-Inf, 5, 14, 19, 24, 29, 39, 49, 59, 69, Inf))) |> 
            # sumar población por grupos de edad
            group_by(género, edad_categoría) |> 
            summarize(población = sum(población),
                      min = min(edad),
                      max = max(edad)) |> 
            # generar etiquetas
            mutate(etiqueta = ifelse(min == 70, "70 o más años", paste(min, "a", max, "años")),
                   etiqueta = reorder(etiqueta, edad_categoría))
    })
    
    ### región ----
    proyecciones_piramide_region <- reactive({
        datos_edad_region() |> 
            # crear grupos de edad
            mutate(edad_categoría = cut(edad, breaks = c(-Inf, 5, 14, 19, 24, 29, 39, 49, 59, 69, Inf))) |> 
            # sumar población por grupos de edad
            group_by(género, edad_categoría, region) |> 
            summarize(población = sum(población),
                      min = min(edad),
                      max = max(edad)) |> 
            # generar etiquetas
            mutate(etiqueta = ifelse(min == 70, "70 o más años", paste(min, "a", max, "años")),
                   etiqueta = reorder(etiqueta, edad_categoría))
    })
    
    
    #—----
    
    # gráficos ----
    
    ## grafico evolución ----
    
    
    ### regiones ----
    output$grafico_proyeccion_regiones <- renderPlot({
        req(length(input$regiones) > 0)
        
        # dev.new()
        proyecciones_regiones() |> 
            mutate(region = stringr::str_wrap(region, 15)) |> 
            group_by(año, region, proyeccion, proyeccion_b,
                     año_censal) |> 
            summarize(población = sum(población, na.rm = T)) |>
            ggplot(aes(x = año, y = población,
                       color = reorder(region, población, decreasing = T),
                       alpha = proyeccion, size = año_censal,
                       group = region)) +
            # figuras
            geom_vline(xintercept = 2024, linewidth = 1, color = color_destacado, alpha = 0.4) +
            geom_line(aes(alpha = proyeccion_b), linewidth = 1.2, show.legend = F) +
            geom_point() +
            # escalas
            scale_alpha_manual(values = c("pasado" = 1, "futuro" = 0.3)) +
            scale_size_manual(values = c("proyección" = NULL, "censo" = 4), guide = "none") +
            scale_x_continuous(breaks = c(2002, seq(2005, 2035, by = 5), 2017)) +
            scale_y_continuous(labels = function(x) paste(x/1000, "mil")) +
            # temas
            theme_minimal() +
            theme(legend.position = "right",
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(color = color_destacado),
                  axis.text = element_text(size = 8),
                  axis.text.x = element_text(size = 8, angle = -90, vjust = 0.5),
                  panel.grid.minor.x = element_blank(),
                  legend.text.align = 0,
                  legend.title = element_text(color = color_destacado)
            ) +
            # etiquetas
            labs(y = "Población residente proyectada",
                 alpha = "Temporalidad", 
                 color = "Regiones", fill = "Regiones") +
            guides(colour = guide_legend(override.aes = list(size = 4), order = 1),
                   alpha = F) #guide_legend(override.aes = list(size = 4)))
        # browser()
    }, res = 100)
    
    
    ### comunas ----
    output$grafico_proyeccion_comunas <- renderPlot({
        req(length(input$comunas) > 0)
        
        # dev.new()
        proyecciones_comunas() |> 
            ggplot(aes(x = año, y = población,
                       color = reorder(comuna, población, decreasing = T),
                       alpha = proyeccion, size = año_censal,
                       group = comuna)) +
            # figuras
            geom_vline(xintercept = 2024, linewidth = 1, color = color_destacado, alpha = 0.4) +
            geom_line(aes(alpha = proyeccion_b), linewidth = 1.2, show.legend = F) +
            geom_point() +
            # escalas
            scale_alpha_manual(values = c("pasado" = 1, "futuro" = 0.3)) +
            scale_size_manual(values = c("proyección" = NULL, "censo" = 4), guide = "none") +
            scale_x_continuous(breaks = c(2002, seq(2005, 2035, by = 5), 2017)) +
            scale_y_continuous(labels = function(x) paste(x/1000, "mil")) +
            # temas
            theme_minimal() +
            theme(legend.position = "right",
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(color = color_destacado),
                  axis.text = element_text(size = 8),
                  axis.text.x = element_text(size = 8, angle = -90, vjust = 0.5),
                  panel.grid.minor.x = element_blank(),
                  legend.text.align = 0,
                  legend.title = element_text(color = color_destacado)
            ) +
            # etiquetas
            labs(y = "Población residente proyectada",
                 alpha = "Temporalidad", 
                 color = "Comunas", fill = "Comunas") +
            guides(colour = guide_legend(override.aes = list(size = 4), order = 1),
                   alpha = F, size = F) #guide_legend(override.aes = list(size = 4)))
        
    }, res = 100)
    
    
    ## grafico piramide ----
    
    ### comuna ----
    output$grafico_piramide_comuna <- renderPlot({
        req(length(input$comuna_piramide) > 0)
        
        proyecciones_piramide_comuna() |> 
            mutate(población = ifelse(género == "masculino", población, -población)) |> 
            ggplot(aes(x = población, y = etiqueta, 
                       fill = género, color = género)) +
            scale_x_continuous(labels = ~case_when(.x == 0 ~ "",
                                                   abs(.x) < 1000 ~ as.character(abs(.x)),
                                                   abs(.x) >= 1000 ~ paste(abs(.x)/1000, "mil")),
                               limits = ~c(-max(abs(.x)), max(abs(.x))),
                               expand = expansion(c(0.1, 0.1))
            ) +
            geom_text(aes(
                label = ifelse(género == "masculino",
                               paste(" ", format(abs(población), big.mark = ".", trim = T)),
                               paste(format(abs(población), big.mark = ".", trim = T), " ")
                ),
                hjust = ifelse(género == "masculino", 0, 1)
            ),
            size = 2.4
            ) +
            geom_col(width = 0.5) +
            geom_vline(xintercept = 0, linewidth = 1, color = "white") +
            theme_minimal() +
            scale_fill_manual(values = c("masculino" = color_masculino, "femenino" = color_femenino), 
                              aesthetics = c("fill", "color")) +
            theme(legend.position = "bottom",
                  axis.title = element_text(color = color_destacado),
                  axis.text = element_text(size = 8),
                  panel.grid.minor.x = element_blank(),
                  legend.text.align = 0,
                  legend.title = element_text(color = color_destacado),
                  legend.text = element_text(margin = margin(l = -3, r = 5))
            ) +
            labs(y = "Categorías de edad", x = "Población por grupo de edad y género",
                 fill = "Género", color = "Género")
    }, res = 100)
    
    ### region ----
    output$grafico_piramide_region <- renderPlot({
        req(length(input$region_piramide) > 0)
        
        proyecciones_piramide_region() |> 
            mutate(población = ifelse(género == "masculino", población, -población)) |> 
            ggplot(aes(x = población, y = etiqueta, 
                       fill = género, color = género)) +
            scale_x_continuous(labels = ~case_when(.x == 0 ~ "",
                                                   abs(.x) < 1000 ~ as.character(abs(.x)),
                                                   abs(.x) >= 1000 ~ paste(abs(.x)/1000, "mil")),
                               limits = ~c(-max(abs(.x)), max(abs(.x))),
                               expand = expansion(c(0.1, 0.1))
            ) +
            geom_text(aes(
                label = ifelse(género == "masculino",
                               paste(" ", format(abs(población), big.mark = ".", trim = T)),
                               paste(format(abs(población), big.mark = ".", trim = T), " ")
                ),
                hjust = ifelse(género == "masculino", 0, 1)
            ),
            size = 2.4
            ) +
            geom_col(width = 0.5) +
            geom_vline(xintercept = 0, linewidth = 1, color = "white") +
            theme_minimal() +
            scale_fill_manual(values = c("masculino" = color_masculino, "femenino" = color_femenino), 
                              aesthetics = c("fill", "color")) +
            theme(legend.position = "bottom",
                  axis.title = element_text(color = color_destacado),
                  axis.text = element_text(size = 8),
                  panel.grid.minor.x = element_blank(),
                  legend.text.align = 0,
                  legend.title = element_text(color = color_destacado),
                  legend.text = element_text(margin = margin(l = -3, r = 5))
            ) +
            labs(y = "Categorías de edad", x = "Población por grupo de edad y género",
                 fill = "Género", color = "Género")
    }, res = 100)
    
    #—----
    
    # tablas ----
    
    ## tabla comparar años ----
    
    ### comunas ----
    output$comparacion_años_comunas <- render_gt({
        req(length(input$comunas) > 0)
        
        año_2 = sym(as.character(input$comparar_años[2]))
        año_1 = sym(as.character(input$comparar_años[1]))
        
        proyecciones_comunas() |> 
            filter(año %in% input$comparar_años) |> 
            arrange(desc(población)) |> 
            tidyr::pivot_wider(id_cols = comuna, names_from = año, values_from = población) |> 
            mutate(cambio := (!!año_2 - !!año_1) / !!año_2) |> 
            mutate(flecha = case_when(cambio >= 0.5 ~ "angles-up",
                                      cambio >= 0 ~ "arrow-up",
                                      cambio < 0 ~ "arrow-down")) |> 
            relocate(flecha, .before = cambio) |> 
            #tabla
            gt() |> 
            gt_fa_column(flecha, palette = c("angles-up" = color_negativo, "arrow-up" = color_destacado, "arrow-down" = color_negativo)) |> 
            fmt_percent(
                columns = cambio,
                decimals = 1,
            ) |> 
            cols_align(columns = where(is.numeric), align = "left") |> 
            fmt_number(columns = 2:3, sep_mark = ".", decimals = 0) |> 
            cols_label(
                cambio = "% de cambio",
                comuna = "Comuna",
                flecha = ""
            ) |> 
            tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |> 
            tab_options(table.font.color = color_principal)
    })
    
    ### regiones ----
    output$comparacion_años_regiones <- render_gt({
        req(length(input$regiones) > 0)
        
        # dev.new()
        # proyecciones_comunas()
        # browser()
        
        año_2 = sym(as.character(input$comparar_años[2]))
        año_1 = sym(as.character(input$comparar_años[1]))
        
        proyecciones_regiones() |> 
            filter(año %in% input$comparar_años) |> 
            group_by(año, region) |> 
            summarize(población = sum(población, na.rm = T)) |> 
            arrange(desc(población)) |> 
            tidyr::pivot_wider(id_cols = region, names_from = año, values_from = población) |> 
            mutate(cambio := (!!año_2 - !!año_1) / !!año_2) |> 
            mutate(flecha = case_when(cambio >= 0.5 ~ "angles-up",
                                      cambio >= 0 ~ "arrow-up",
                                      cambio < 0 ~ "arrow-down")) |> 
            relocate(flecha, .before = cambio) |> 
            #tabla
            gt() |> 
            gt_fa_column(flecha, palette = c("angles-up" = color_negativo, "arrow-up" = color_destacado, "arrow-down" = color_negativo)) |> 
            fmt_percent(
                columns = cambio,
                decimals = 1,
            ) |> 
            cols_align(columns = where(is.numeric), align = "left") |> 
            fmt_number(columns = 2:3, sep_mark = ".", decimals = 0) |> 
            cols_label(
                cambio = "% de cambio",
                region = "Región",
                flecha = ""
            ) |> 
            tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |> 
            tab_options(table.font.color = color_principal)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
