

plot_tvvt <- function(.data_inmet, 
         yvar = "tmed_anual",
         tvvt,
         intercepto,
         rquad,
         nmarcas = 20,
         lapse_rate = -6.5,
         cor_tvvt = "red",
         nl_reg = TRUE
         ){
  
  lab_eixo_y <- dplyr::case_match(
    yvar,
    "tmed_anual" ~ "Temperatura média anual (°C)",
    "tmax_anual" ~ "Temperatura máxima anual (°C)",
    "tmin_anual" ~ "Temperatura mínima anual (°C)",
    "tcomp_anual" ~ "Temperatura compensada anual (°C)"
  )
    
    # dados para plot e como mapear as variaveis nos eixos
    g <- ggplot(data = .data_inmet, mapping = aes(x = altitude, y = !!rlang::sym(yvar))) +
    # adicionar pontos
    geom_point() +
    # adicionar reta da regressão linear
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, col = cor_tvvt) +
    #geom_smooth(se = FALSE, linetype = 2, colour = "white") + 
    # adicionar mais marcas nos eixos 
    scale_y_continuous(
      name = lab_eixo_y,
      breaks = scales::pretty_breaks(nmarcas)
    ) +
    scale_x_continuous(
      name = "Altitude (m)",
      breaks = scales::pretty_breaks(nmarcas)
    ) +
    geom_abline(
      slope = lapse_rate/1000,
      intercept = intercepto, 
      linetype = 3
    ) +
    ggtitle(label = glue::glue("TVVT: {round(tvvt, 2)} °C/km, R² = {round(rquad, 2)}"), 
            subtitle = glue::glue("Lapse rate: {round(lapse_rate, 3)} °C/km")) +
    theme(plot.title = element_text(color = cor_tvvt))
  
    if (nl_reg) {
      g <- g + geom_smooth(se = FALSE, linetype = 2, linewidth = 0.2, color = cor_tvvt)
    }
  g
}






#' Baixa e importa polígonos de estados e regiões do Brasil
#'
#' @return objeto da classe sf, Geometry type: MULTIPOLYGON
#' @export
#'
#' @examples
#' pols_estados <- load_pols_states()
#' 
load_pols_states <- function(){
  
  outfile <- here::here("output", "BR_UF_2022", "BR_UF_2022.qs")
  if (fs::file_exists(outfile)) return(qs::qread(outfile))
  
  # para baixar poligonos dos estados e regioes do brasil do site do ibge
  link_ibge <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2022/Brasil/BR/BR_UF_2022.zip"
  file_ibge <- here::here("output", basename(link_ibge))
  
  # baixa zip
  download.file(link_ibge, destfile = file_ibge)
  
  # descompacta em subdiretorio
  unzip_dir <- here("output", gsub(".zip", "", basename(file_ibge)))
  unzip(file_ibge, exdir = unzip_dir)
  # lista arquivo shapefile
  file_ibge <- fs::dir_ls(unzip_dir, glob = "*.shp")
  
  
  # importa shapefile
  pols <- sf::st_read(file_ibge, quiet = TRUE)
  # transforma para crs WGS84 (longlat)
  pols <- sf::st_transform(pols, crs = 4326) 
  # renomeia para nomes compativeis com as variaveis das informacoes das EMCs 
  # do INMET
  pols <- dplyr::rename(pols,
                        "uf" = "SIGLA_UF", 
                        "regiao" = "NM_REGIAO"
  ) |> 
    dplyr::select(regiao, uf) |> 
    dplyr::mutate(regiao = gsub("\\n", "", regiao))
  
  file_ibge <- gsub(".shp", ".qs", file_ibge)
  qs::qsave(pols, file_ibge)
  message("Arquivo baixado em: \n", file_ibge)
  
  pols
  
}



#' Plota taxa de variação verticar da temperatura por região do Brasil
#'
#' @param yvar um dos caracteres 'tmed_anual', 'tcomp_anual', 'tmax_anual', 'tmin_anual'.
#' @param lr_intercs vetor com valores de temperatura que interceptam o eixo y, default 19:30.
#'
#' @return objeto do ggplot2
#'
#' @examples
plot_tvvt_by_region <- function(.data = data_inmet, yvar = "tmed_anual", lr_intercs = 19:30){
  
  checkmate::assert_names(
    names(.data), 
    must.include = c("altitude", "regiao", yvar)
    )
  
  titulo <- dplyr::case_when(
    yvar == "tmed_anual" ~ "média",
    yvar == "tcomp_anual" ~ "compensada",
    yvar == "tmin_anual" ~ "mínima",
    yvar == "tmax_anual" ~ "máxima",
    .default = ""
  )
  
  ggplot(
    data = .data,
    mapping = aes(x = altitude,
                  y = !!rlang::sym(yvar),
                  colour = regiao, 
                  shape = regiao
    )
  ) +
    # adicionar pontos
    stat_poly_line(se = FALSE) +
    stat_poly_eq(use_label(c("eq", "R2")), label.x = 0.99, size = 2.7) +
    geom_point() +
    # adicionar mais marcas nos eixos
    scale_y_continuous(
      name = glue::glue("Temperatura {titulo} anual (°C)"),
      breaks = scales::pretty_breaks(15),
      # limits = data_inmet |> dplyr::select(contains("_anual")) |> range()
    ) +
    scale_x_continuous(
      name = "Altitude (m)",
      breaks = scales::pretty_breaks(15)
    ) +
    geom_abline(
      slope = lapse_rate/1000,
      intercept = lr_intercs, linewidth = 0.3, 
      linetype = 3
    )  +
    geom_smooth(se = FALSE, linetype = 2, linewidth = 0.3)
}

