# Nombres y apellidos: Emily Lady Quinto de la Cruz
# Código de matrícula: 2024200518K
# Tema N.º 65 del temario: Estimación del beta de las acciones peruanas frente al índice S&P/BVL
# Fecha de extracción: COMPLETAR (aaaa-mm-dd) el día que ejecutes este script
# -----------------------------------------------------------------------------
# 02_scraping_web.R  (VÍA SCRAPING)
# Rastrea la tabla HTML pública de BCRPData con el Índice General Bursátil BVL
# (serie SERIE_INDICE). Se usa para VERIFICAR contra la serie obtenida por API (01),
# tal como pide el temario ("descarga programática para verificación").
# Respeta robots.txt, User-Agent identificable y pausa entre solicitudes.
# Ejecutar desde la carpeta 3_Base_de_datos_y_codigo:  source("codigo/02_scraping_web.R")
# -----------------------------------------------------------------------------

source("codigo/00_parametros.R")
instalar_si_falta(c("httr2", "rvest", "xml2"))
library(httr2)
library(rvest)

BASE <- "https://estadisticas.bcrp.gob.pe"
log_msg("=== INICIO 02_scraping_web | serie ", SERIE_INDICE, " ===")

# ---- 1. robots.txt: si prohíbe la ruta, NO se rastrea (numeral 2.4.8) --------
rb <- request(paste0(BASE, "/robots.txt")) |> req_user_agent(USER_AGENT) |>
  req_error(is_error = function(r) FALSE) |> req_perform()
log_msg("robots.txt | HTTP ", resp_status(rb))
robots <- if (resp_status(rb) == 200) resp_body_string(rb) else ""
writeLines(robots, file.path(DIR_CRUDOS, "robots_bcrp.txt"))
if (grepl("Disallow:\\s*/estadisticas/series", robots))
  stop("robots.txt prohíbe la ruta. Documenta en incidencias_fuente.md y solicita sustitución al docente.")
Sys.sleep(PAUSA_SEG)

# ---- 2. Solicitud de la página de resultados (HTML) --------------------------
fmt_bcrp <- function(d) paste(format(d, "%Y"), as.integer(format(d, "%m")), as.integer(format(d, "%d")), sep = "-")
# URL sin parámetros de fecha: devuelve la serie completa (2006-hoy); 03 recorta a la ventana
url <- paste0(BASE, "/estadisticas/series/diarias/resultados/", SERIE_INDICE)
resp <- request(url) |> req_user_agent(USER_AGENT) |>
  req_error(is_error = function(r) FALSE) |> req_perform()
log_msg("GET ", url, " | HTTP ", resp_status(resp))
if (resp_status(resp) != 200)
  stop("HTTP ", resp_status(resp), ": documenta el bloqueo en incidencias_fuente.md.")
html_txt <- resp_body_string(resp)
writeLines(html_txt, file.path(DIR_BCRP_HTML, paste0(SERIE_INDICE, ".html")))   # crudo intacto

# ---- 3. Extracción con selectores CSS (tabla de BCRPData) ---------------------
# Selectores: celdas <td class="periodo"> (fecha, p. ej. "02Ene06") y <td class="dato"> (valor).
pagina  <- read_html(html_txt)
limpiar <- function(x) trimws(gsub("[[:space:]]+", " ", x))      # quita saltos de línea (\r\n)
periodo <- limpiar(html_text2(html_elements(pagina, "td.periodo")))
dato    <- limpiar(html_text2(html_elements(pagina, "td.dato")))
if (length(periodo) == 0 || length(periodo) != length(dato))
  stop("Los selectores td.periodo / td.dato no coinciden: revisa datos_crudos/bcrp_html.")
crudo <- data.frame(periodo = periodo, valor_texto = dato, stringsAsFactors = FALSE)
archivo <- file.path(DIR_CRUDOS, paste0("datos_crudos_bcrp_html_", CODIGO, ".csv"))
write.csv(crudo, archivo, row.names = FALSE)
log_msg("Guardado ", archivo, " | filas rastreadas: ", nrow(crudo))
log_msg("=== FIN 02_scraping_web ===")
