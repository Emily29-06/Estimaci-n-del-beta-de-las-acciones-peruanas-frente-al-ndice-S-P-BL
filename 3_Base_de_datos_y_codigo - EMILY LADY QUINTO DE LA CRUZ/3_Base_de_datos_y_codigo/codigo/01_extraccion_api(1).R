# Nombres y apellidos: Emily Lady Quinto de la Cruz
# Código de matrícula: 2024200518K
# Tema N.º 65 del temario: Estimación del beta de las acciones peruanas frente al índice S&P/BVL
# Fecha de extracción: COMPLETAR (aaaa-mm-dd) el día que ejecutes este script
# -----------------------------------------------------------------------------
# 01_extraccion_api.R  (VÍA API)
# Consume (1) el endpoint público de series históricas de Yahoo Finance (acciones) y
# (2) la API de BCRPData (índice general BVL y tipo de cambio). Yahoo:
#   https://query1.finance.yahoo.com/v8/finance/chart/<SIMBOLO>
# Parámetros: period1/period2 (UNIX), interval=1d, includeAdjustedClose=true.
# Guarda (a) el JSON tal como llega, y (b) un CSV crudo sin editar valores.
# Ejecutar desde la carpeta 3_Base_de_datos_y_codigo:  source("codigo/01_extraccion_api.R")
# -----------------------------------------------------------------------------

source("codigo/00_parametros.R")
instalar_si_falta(c("httr2", "jsonlite"))
library(httr2)
library(jsonlite)

ENDPOINT <- "https://query1.finance.yahoo.com/v8/finance/chart/"

# Fechas -> segundos UNIX (UTC). period2 suma un día para incluir FECHA_CORTE.
period1 <- as.numeric(as.POSIXct(FECHA_INICIO, tz = "UTC"))
period2 <- as.numeric(as.POSIXct(FECHA_CORTE + 1, tz = "UTC"))

log_msg("=== INICIO 01_extraccion_api | ventana ", FECHA_INICIO, " a ", FECHA_CORTE, " ===")

# Convierte NULL (día sin dato) en NA para poder armar columnas numéricas
vec <- function(x) vapply(x, function(z) if (is.null(z)) NA_real_ else as.numeric(z), numeric(1))

# Descarga un símbolo con hasta 3 intentos. Devuelve data.frame (o NULL si falla)
descargar_simbolo <- function(simbolo) {
  url <- paste0(ENDPOINT, URLencode(simbolo, reserved = TRUE))
  for (intento in 1:3) {
    resp <- tryCatch(
      request(url) |>
        req_user_agent(USER_AGENT) |>
        req_url_query(period1 = format(period1, scientific = FALSE),
                      period2 = format(period2, scientific = FALSE),
                      interval = "1d", includeAdjustedClose = "true") |>
        req_error(is_error = function(r) FALSE) |>   # no abortar: revisamos el código HTTP
        req_perform(),
      error = function(e) NULL)
    estado <- if (is.null(resp)) NA else resp_status(resp)
    if (!is.null(resp) && estado == 200) {
      txt <- resp_body_string(resp)
      writeLines(txt, file.path(DIR_JSON, paste0(gsub("[^A-Za-z0-9]", "_", simbolo), ".json")))
      js  <- fromJSON(txt, simplifyVector = FALSE)
      res <- js$chart$result[[1]]
      if (is.null(res$timestamp)) { log_msg(simbolo, " | HTTP 200 | sin datos"); return(NULL) }
      q   <- res$indicators$quote[[1]]
      adj <- res$indicators$adjclose[[1]]$adjclose
      df <- data.frame(
        simbolo   = simbolo,
        timestamp = vec(res$timestamp),
        gmtoffset = as.numeric(res$meta$gmtoffset),
        moneda    = if (is.null(res$meta$currency)) NA_character_ else res$meta$currency,
        open = vec(q$open), high = vec(q$high), low = vec(q$low),
        close = vec(q$close), adjclose = if (is.null(adj)) NA_real_ else vec(adj),
        volume = vec(q$volume),
        stringsAsFactors = FALSE)
      log_msg(simbolo, " | HTTP ", estado, " | filas descargadas: ", nrow(df))
      return(df)
    }
    log_msg(simbolo, " | HTTP ", estado, " | intento ", intento, " fallido")
    Sys.sleep(PAUSA_SEG * intento)
  }
  NULL
}

simbolos <- CANDIDATAS
lista <- list()
for (s in simbolos) {
  lista[[s]] <- descargar_simbolo(s)
  Sys.sleep(PAUSA_SEG)                                  # pausa entre solicitudes
}

crudo <- do.call(rbind, lista)
n_ok <- if (is.null(crudo)) 0 else length(unique(crudo$simbolo))
if (n_ok < N_ACCIONES)
  stop("Solo respondieron ", n_ok, " acciones (se necesitan >= ", N_ACCIONES, "). Agrega candidatas en 00_parametros.R.")

archivo <- file.path(DIR_CRUDOS, paste0("datos_crudos_", CODIGO, ".csv"))
write.csv(crudo, archivo, row.names = FALSE)
log_msg("Guardado ", archivo, " | filas: ", nrow(crudo), " | símbolos con datos: ", n_ok)

# ---- BCRPData (VÍA API): índice general BVL y tipo de cambio -----------------
# Formato de la API: .../series/api/<CODIGO>/json/<aaaa-m-d>/<aaaa-m-d>/esp
fmt_bcrp <- function(d) paste(format(d, "%Y"), as.integer(format(d, "%m")), as.integer(format(d, "%d")), sep = "-")
descargar_bcrp <- function(nombre, codigo) {
  url <- paste0("https://estadisticas.bcrp.gob.pe/estadisticas/series/api/", codigo,
                "/json/", fmt_bcrp(FECHA_INICIO), "/", fmt_bcrp(FECHA_CORTE), "/esp")
  resp <- request(url) |> req_user_agent(USER_AGENT) |>
    req_error(is_error = function(r) FALSE) |> req_perform()
  estado <- resp_status(resp)
  if (estado != 200) { log_msg("BCRP ", codigo, " | HTTP ", estado, " | fallo"); return(NULL) }
  txt <- resp_body_string(resp)
  writeLines(txt, file.path(DIR_BCRP_JSON, paste0(codigo, ".json")))
  js  <- fromJSON(txt, simplifyVector = FALSE)
  nombre_serie <- js$config$series[[1]]$name
  per <- js$periods
  df <- data.frame(
    serie = nombre, codigo = codigo, nombre_serie = nombre_serie,
    periodo = vapply(per, function(p) as.character(p$name), character(1)),
    valor   = vapply(per, function(p) { v <- p$values[[1]]; if (is.null(v) || v %in% c("n.d.", "")) NA_real_ else as.numeric(v) }, numeric(1)),
    stringsAsFactors = FALSE)
  log_msg("BCRP ", codigo, " (", nombre_serie, ") | HTTP ", estado, " | filas descargadas: ", nrow(df))
  df
}
lista_bcrp <- list()
for (n in names(SERIES_BCRP)) { lista_bcrp[[n]] <- descargar_bcrp(n, SERIES_BCRP[[n]]); Sys.sleep(PAUSA_SEG) }
bcrp <- do.call(rbind, lista_bcrp)
if (is.null(bcrp) || !("indice_igbvl" %in% bcrp$serie))
  stop("No se pudo descargar el índice del BCRP. Revisa log_ejecucion.txt y documenta en incidencias_fuente.md.")
archivo2 <- file.path(DIR_CRUDOS, paste0("datos_crudos_bcrp_api_", CODIGO, ".csv"))
write.csv(bcrp, archivo2, row.names = FALSE)
log_msg("Guardado ", archivo2, " | filas: ", nrow(bcrp))

# Versiones exactas de R y paquetes (exigido: sessionInfo.txt)
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
log_msg("=== FIN 01_extraccion_api ===")
