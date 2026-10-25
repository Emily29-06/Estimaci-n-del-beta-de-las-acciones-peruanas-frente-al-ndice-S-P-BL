# Nombres y apellidos: Emily Lady Quinto de la Cruz
# Código de matrícula: 2024200518K
# Tema N.º 65 del temario: Estimación del beta de las acciones peruanas frente al índice S&P/BVL
# Fecha de extracción: COMPLETAR (aaaa-mm-dd) el día que ejecutes 01 y 02
# -----------------------------------------------------------------------------
# 00_parametros.R
# Parámetros congelados y funciones comunes. Lo cargan los otros scripts con
# source("codigo/00_parametros.R"). Ejecutar SIEMPRE con el directorio de trabajo
# en la carpeta "3_Base_de_datos_y_codigo" (setwd en la CONSOLA, no en el script).
# -----------------------------------------------------------------------------

# ---- Identificación ---------------------------------------------------------
CODIGO <- "2024200518K"          # código de matrícula (va en los nombres de archivo)

# ---- Ventana de extracción (constantes, NO fechas dinámicas) ----------------
# Se amplía respecto del mínimo del temario (2015-2025) para superar la exigencia.
FECHA_INICIO <- as.Date("2010-01-01")
FECHA_CORTE  <- as.Date("2025-12-31")

# ---- Acciones (Yahoo Finance, VÍA API) --------------------------------------
# Candidatas a verificar: no todas tendrán historia en Yahoo. El script 01 registra
# en el log cuáles responden y 03 aplica la regla de cobertura (abajo).
CANDIDATAS <- c(
  "ALICORC1.LM", "BACKUSI1.LM", "CVERDEC1.LM", "FERREYC1.LM", "UNACEMC1.LM",
  "VOLCABC1.LM", "INRETC1.LM",  "CONTINC1.LM", "CORAREI1.LM", "CASAGRC1.LM",
  "ENGEPEC1.LM", "MINSURI1.LM", "SCCO.LM",     "BAP.LM",      "PODERC1.LM",
  "LUSURC1.LM",  "EDELNOC1.LM", "ENDISPC1.LM", "CPACASC1.LM", "AENZAC1.LM",
  "BVN.LM",      "BROCALC1.LM", "HOCHSCC1.LM", "MILPOC1.LM",  "TELEFBC1.LM",
  "IFS.LM",      "AUSTRAC1.LM", "RELAPAC1.LM", "ATACOBC1.LM", "POMALCC1.LM",
  "GRAMONC1.LM", "PARAMOC1.LM", "MINCORI1.LM", "SIDERC1.LM"
)

# ---- Índice S&P/BVL Peru General y otras series (BCRPData) ------------------
# Yahoo Finance NO ofrece historia del índice (HTTP 404 en ^SPBLPGPT; ver
# incidencias_fuente.md). Fuente sustituta autorizada por el docente: BCRPData.
# El BCRP publica el Índice General Bursátil BVL (= S&P/BVL Peru General, ex IGBVL).
SERIE_INDICE <- "PD38026MD"     # Índice General Bursátil BVL (índice), diaria
SERIES_BCRP  <- c(
  indice_igbvl = SERIE_INDICE,
  tc_venta     = "PD04638PD"    # Tipo de cambio interbancario venta (VERIFICAR el nombre en el log)
)

# ---- Reglas de selección de la muestra (se usan en 03) ----------------------
N_ACCIONES        <- 18      # se supera el mínimo del temario (15)
MIN_OBS_ACCION    <- 2500    # mínimo de días con precio para entrar a la muestra

# ---- Buenas prácticas de rastreo (numeral 2.4.8 de la consigna) -------------
USER_AGENT <- paste0("UNCP-Finanzas-I/2026-II (estudiante: Emily Quinto de la Cruz; ",
                     "uso academico; contacto: COMPLETAR-CORREO-INSTITUCIONAL)")
PAUSA_SEG  <- 2              # pausa entre solicitudes (la consigna exige >= 1 s)

# ---- Rutas RELATIVAS --------------------------------------------------------
DIR_CRUDOS      <- "datos_crudos"
DIR_JSON        <- file.path(DIR_CRUDOS, "yahoo_json")
DIR_BCRP_JSON   <- file.path(DIR_CRUDOS, "bcrp_json")
DIR_BCRP_HTML   <- file.path(DIR_CRUDOS, "bcrp_html")
DIR_PROCESADOS  <- "datos_procesados"
DIR_SALIDAS     <- "salidas"
ARCH_LOG        <- "log_ejecucion.txt"
for (d in c(DIR_CRUDOS, DIR_JSON, DIR_BCRP_JSON, DIR_BCRP_HTML, DIR_PROCESADOS, DIR_SALIDAS))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

# ---- Función de registro (log_ejecucion.txt) --------------------------------
log_msg <- function(...) {
  linea <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
  cat(linea, "\n")
  cat(linea, "\n", file = ARCH_LOG, append = TRUE)
}

# ---- Paquetes: se instalan solo si faltan -----------------------------------
instalar_si_falta <- function(pkgs) {
  faltan <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltan)) install.packages(faltan, repos = "https://cloud.r-project.org")
}

# ---- Fechas del BCRP ("09Ene06", "09.Ene.06", "09Ene2006") -> Date ----------
parse_fecha_bcrp <- function(x) {
  meses <- c(Ene=1, Feb=2, Mar=3, Abr=4, May=5, Jun=6, Jul=7, Ago=8, Set=9, Sep=9, Oct=10, Nov=11, Dic=12)
  m <- regmatches(x, regexec("^\\s*(\\d{1,2})[. -]?([A-Za-z]{3})[. -]?(\\d{2,4})\\s*$", x))
  vapply(m, function(z) {
    if (length(z) < 4) return(NA_real_)
    mes <- meses[tools::toTitleCase(tolower(z[3]))]
    anio <- as.integer(z[4]); if (anio < 100) anio <- anio + ifelse(anio <= 69, 2000L, 1900L)
    as.numeric(as.Date(sprintf("%04d-%02d-%02d", anio, mes, as.integer(z[2]))))
  }, numeric(1)) |> as.Date(origin = "1970-01-01")
}

list.files("datos_crudos")
head(read.csv("datos_crudos/datos_crudos_bcrp_html_2024200518K.csv"), 4)


x <- readLines("codigo/02_scraping_web.R", warn = FALSE)
length(x); any(grepl('source\\("codigo/02', x)); any(grepl("limpiar", x))

source("codigo/02_scraping_web.R")

source("codigo/03_limpieza_datos.R")

x <- readLines("codigo/03_limpieza_datos.R", warn = FALSE)
any(grepl("revisar_fechas", x))
source("codigo/03_limpieza_datos.R")
getwd()


writeLines(r"---(# Nombres y apellidos: Emily Lady Quinto de la Cruz
# Código de matrícula: 2024200518K
# Tema N.º 65 del temario: Estimación del beta de las acciones peruanas frente al índice S&P/BVL
# Fecha de extracción: 2026-10-23 (API Yahoo y BCRP) y 2026-10-24 (scraping BCRP)
# -----------------------------------------------------------------------------
# 03_limpieza_datos.R
# Depura, tipifica y une las fuentes por la llave común FECHA:
#   (a) acciones: Yahoo Finance (API)           -> datos_crudos_<codigo>.csv
#   (b) índice y tipo de cambio: BCRPData (API) -> datos_crudos_bcrp_api_<codigo>.csv
#   (c) índice: BCRPData (scraping HTML)        -> datos_crudos_bcrp_html_<codigo>.csv
# Produce datos_procesados/datos_procesados_<codigo>.csv (retornos logarítmicos),
# tablas de apoyo en /salidas, el hash SHA-256 y el diccionario de variables.
# Ejecutar desde 3_Base_de_datos_y_codigo:  source("codigo/03_limpieza_datos.R")
# -----------------------------------------------------------------------------

source("codigo/00_parametros.R")
instalar_si_falta("digest")
library(digest)
log_msg("=== INICIO 03_limpieza_datos ===")

# ---- Función: texto de fecha del BCRP ("03Ene10", "03.Ene.10") -> Date (a prueba de fallos)
parse_fecha <- function(x) {
  x <- gsub("[[:space:]]+", "", as.character(x))
  m <- regmatches(x, regexec("^(\\d{1,2})[.-]?([A-Za-z]{3,4})[.-]?(\\d{2,4})$", x))
  meses <- c(ene=1, feb=2, mar=3, abr=4, may=5, jun=6, jul=7, ago=8, set=9, sep=9, oct=10, nov=11, dic=12)
  out <- vapply(m, function(z) {
    if (length(z) < 4) return(NA_real_)
    mes <- unname(meses[substr(tolower(z[3]), 1, 3)])
    if (is.na(mes)) return(NA_real_)
    anio <- as.integer(z[4]); if (anio < 100) anio <- anio + ifelse(anio <= 69, 2000L, 1900L)
    as.numeric(as.Date(sprintf("%04d-%02d-%02d", anio, as.integer(mes), as.integer(z[2])), optional = TRUE))
  }, numeric(1))
  as.Date(out, origin = "1970-01-01")
}
# Registra cuántas fechas no se pudieron leer (con ejemplos) y detiene si son más del 1 %
revisar_fechas <- function(texto, fecha, nombre) {
  mal <- is.na(fecha)
  log_msg(nombre, ": fechas no interpretadas = ", sum(mal), " de ", length(fecha),
          if (any(mal)) paste0(" | ejemplos: ", paste(head(unique(texto[mal]), 5), collapse = " ; ")) else "")
  if (mean(mal) > 0.01) stop("Más del 1 % de las fechas de ", nombre, " no se pudo leer: revisa el formato.")
}

# ---- Función: detecta "picos" que se revierten (error de dato, no movimiento real)
marcar_picos <- function(x, umbral, tol) {
  r  <- c(NA, diff(log(x)))          # retorno del día t (respecto de t-1)
  r2 <- c(r[-1], NA)                 # retorno del día t+1
  !is.na(r) & !is.na(r2) & abs(r) > umbral & abs(r2) > umbral &
    sign(r) != sign(r2) & abs(r + r2) < tol
}

# ---- 1. Acciones (Yahoo Finance) --------------------------------------------
y <- read.csv(file.path(DIR_CRUDOS, paste0("datos_crudos_", CODIGO, ".csv")), stringsAsFactors = FALSE)
y$fecha <- as.Date(as.POSIXct(y$timestamp + y$gmtoffset, origin = "1970-01-01", tz = "UTC"))
y <- y[!is.na(y$fecha) & y$fecha >= FECHA_INICIO & y$fecha <= FECHA_CORTE, ]
y <- y[!is.na(y$adjclose) & y$adjclose > 0, ]
y <- y[!duplicated(y[, c("simbolo", "fecha")], fromLast = TRUE), ]
y <- y[order(y$simbolo, y$fecha), ]
log_msg("Yahoo: filas válidas dentro de la ventana: ", nrow(y), " | símbolos: ", length(unique(y$simbolo)))

# Regla de cobertura declarada: >= MIN_OBS_ACCION días con precio; se toman las N_ACCIONES con más días
cob <- do.call(rbind, lapply(split(y, y$simbolo), function(d) data.frame(
  simbolo = d$simbolo[1], moneda = d$moneda[1], n_obs = nrow(d),
  primera = as.character(min(d$fecha)), ultima = as.character(max(d$fecha)),
  pct_dias_sin_negociacion = round(100 * mean(is.na(d$volume) | d$volume == 0), 1),
  stringsAsFactors = FALSE)))
cob <- cob[order(-cob$n_obs), ]
cob$en_muestra <- cob$simbolo %in% head(cob$simbolo[cob$n_obs >= MIN_OBS_ACCION], N_ACCIONES)
write.csv(cob, file.path(DIR_SALIDAS, "cobertura_acciones.csv"), row.names = FALSE)
sel <- cob$simbolo[cob$en_muestra]
if (length(sel) < 15) stop("Menos de 15 acciones cumplen la regla de cobertura; revisa MIN_OBS_ACCION.")
log_msg("Muestra: ", length(sel), " acciones: ", paste(sel, collapse = ", "))
y <- y[y$simbolo %in% sel, ]

# Picos de datos en acciones (umbral 30 %): se eliminan esas filas y se registran
atipicos <- data.frame()
pico_y <- ave(y$adjclose, y$simbolo, FUN = function(x) as.numeric(marcar_picos(x, 0.30, 0.05)))
if (any(pico_y == 1)) {
  atipicos <- rbind(atipicos, data.frame(serie = y$simbolo[pico_y == 1], fecha = as.character(y$fecha[pico_y == 1]),
                                         valor = y$adjclose[pico_y == 1], stringsAsFactors = FALSE))
  y <- y[pico_y == 0, ]
}

# ---- 2. BCRPData por API: índice y tipo de cambio ----------------------------
b <- read.csv(file.path(DIR_CRUDOS, paste0("datos_crudos_bcrp_api_", CODIGO, ".csv")), stringsAsFactors = FALSE)
b$fecha <- parse_fecha(b$periodo); revisar_fechas(b$periodo, b$fecha, "BCRP API")
b <- b[!is.na(b$fecha) & b$fecha >= FECHA_INICIO & b$fecha <= FECHA_CORTE & !is.na(b$valor), ]
ind <- b[b$serie == "indice_igbvl", c("fecha", "valor")]; names(ind)[2] <- "indice"
tc  <- b[b$serie == "tc_venta",     c("fecha", "valor")]; names(tc)[2]  <- "tc_venta"
ind <- ind[order(ind$fecha), ]; tc <- tc[order(tc$fecha), ]
log_msg("BCRP API: índice ", nrow(ind), " días | tipo de cambio ", nrow(tc), " días")

# ---- 3. BCRPData por scraping: verificación del índice -----------------------
h <- read.csv(file.path(DIR_CRUDOS, paste0("datos_crudos_bcrp_html_", CODIGO, ".csv")), stringsAsFactors = FALSE)
h$fecha <- parse_fecha(h$periodo); revisar_fechas(h$periodo, h$fecha, "BCRP scraping")
h$indice_html <- suppressWarnings(as.numeric(gsub(",", "", trimws(h$valor_texto))))
h <- h[!is.na(h$fecha) & h$fecha >= FECHA_INICIO & h$fecha <= FECHA_CORTE & !is.na(h$indice_html), ]
v <- merge(ind, h[, c("fecha", "indice_html")], by = "fecha")
v$coincide <- abs(v$indice - v$indice_html) < 0.05
validacion <- data.frame(dias_api = nrow(ind), dias_scraping = nrow(h), dias_comunes = nrow(v),
                         dias_coinciden = sum(v$coincide), pct_coincidencia = round(100 * mean(v$coincide), 2))
write.csv(validacion, file.path(DIR_SALIDAS, "validacion_indice_api_vs_scraping.csv"), row.names = FALSE)
write.csv(v[!v$coincide, ], file.path(DIR_SALIDAS, "validacion_discrepancias.csv"), row.names = FALSE)
log_msg("Validación índice API vs scraping: ", validacion$dias_coinciden, "/", validacion$dias_comunes,
        " coinciden (", validacion$pct_coincidencia, " %)")

# ---- 4. Picos en el índice (umbral 8 %) --------------------------------------
pico_i <- marcar_picos(ind$indice, 0.08, 0.03)
if (any(pico_i)) {
  atipicos <- rbind(atipicos, data.frame(serie = "indice_igbvl", fecha = as.character(ind$fecha[pico_i]),
                                         valor = ind$indice[pico_i], stringsAsFactors = FALSE))
  log_msg("Índice: ", sum(pico_i), " valor(es) atípico(s) revertido(s) puesto(s) en NA: ",
          paste(ind$fecha[pico_i], collapse = ", "))
  ind$indice[pico_i] <- NA          # el dato crudo NO se toca; solo se marca en el procesado
}
write.csv(atipicos, file.path(DIR_SALIDAS, "atipicos_detectados.csv"), row.names = FALSE)

# ---- 5. Retornos logarítmicos y unión por fecha ------------------------------
ind$ret_indice <- c(NA, diff(log(ind$indice)))          # calendario bursátil = días del índice
tc$ret_tc      <- c(NA, diff(log(tc$tc_venta)))

p <- merge(y[, c("fecha", "simbolo", "moneda", "adjclose", "volume")],
           ind[, c("fecha", "indice", "ret_indice")], by = "fecha")     # solo días con índice
p <- merge(p, tc, by = "fecha", all.x = TRUE)
p <- p[order(p$simbolo, p$fecha), ]
p$pos <- match(p$fecha, ind$fecha)                      # posición en el calendario del índice

# Retorno de la acción solo entre días consecutivos del calendario (sin saltos)
p$ret_accion <- ave(seq_len(nrow(p)), p$simbolo, FUN = function(i) {
  r  <- c(NA, diff(log(p$adjclose[i])))
  ok <- c(FALSE, diff(p$pos[i]) == 1)
  r[!ok] <- NA
  r
})
usd <- !is.na(p$moneda) & p$moneda == "USD"
p$ret_accion_pen   <- p$ret_accion + ifelse(usd, p$ret_tc, 0)      # retorno en soles
p$sin_negociacion  <- is.na(p$volume) | p$volume == 0              # día sin negociación (precio arrastrado)

final <- p[, c("fecha", "simbolo", "moneda", "adjclose", "volume", "sin_negociacion",
               "ret_accion", "ret_accion_pen", "indice", "ret_indice", "tc_venta", "ret_tc")]
final$fecha <- format(final$fecha, "%Y-%m-%d")

archivo <- file.path(DIR_PROCESADOS, paste0("datos_procesados_", CODIGO, ".csv"))
write.csv(final, archivo, row.names = FALSE)
hash <- digest(archivo, algo = "sha256", file = TRUE)
writeLines(c(paste("archivo:", basename(archivo)), paste("sha256:", hash)),
           file.path(DIR_SALIDAS, "hash_sha256.txt"))
log_msg("Guardado ", archivo, " | filas: ", nrow(final), " | retornos válidos: ", sum(!is.na(final$ret_accion_pen)))
log_msg("SHA-256 de datos_procesados: ", hash)

# ---- 6. Resumen de la muestra y diccionario de variables ---------------------
res <- do.call(rbind, lapply(split(final, final$simbolo), function(d) data.frame(
  simbolo = d$simbolo[1], moneda = d$moneda[1], n_retornos = sum(!is.na(d$ret_accion_pen)),
  pct_sin_negociacion = round(100 * mean(d$sin_negociacion), 1), stringsAsFactors = FALSE)))
write.csv(res, file.path(DIR_SALIDAS, "resumen_muestra.csv"), row.names = FALSE)

dic <- c(
  "# Diccionario de variables — datos_procesados", "",
  "| Variable | Definición | Unidad | Frecuencia | Fuente (endpoint) |", "|---|---|---|---|---|",
  "| fecha | Fecha de la sesión bursátil (llave común) | aaaa-mm-dd | Diaria | Yahoo / BCRPData |",
  "| simbolo | Ticker de la acción en Yahoo Finance | texto | — | query1.finance.yahoo.com/v8/finance/chart |",
  "| moneda | Moneda de cotización informada por Yahoo | PEN/USD | — | Yahoo (meta.currency) |",
  "| adjclose | Precio de cierre ajustado por dividendos y splits | moneda de cotización | Diaria | Yahoo |",
  "| volume | Volumen negociado | acciones | Diaria | Yahoo |",
  "| sin_negociacion | Verdadero si volumen es 0 o falta (precio arrastrado) | lógico | Diaria | Calculada |",
  "| ret_accion | Retorno logarítmico diario de la acción | log | Diaria | Calculada |",
  "| ret_accion_pen | Retorno logarítmico de la acción expresado en soles | log | Diaria | Calculada |",
  "| indice | Índice General Bursátil BVL (S&P/BVL Peru General) | puntos | Diaria | BCRPData API PD38026MD |",
  "| ret_indice | Retorno logarítmico diario del índice | log | Diaria | Calculada |",
  "| tc_venta | Tipo de cambio interbancario venta | S/ por US$ | Diaria | BCRPData API PD04638PD |",
  "| ret_tc | Variación logarítmica diaria del tipo de cambio | log | Diaria | Calculada |", "",
  paste0("Fecha de corte: ", FECHA_CORTE, ". Ventana: ", FECHA_INICIO, " a ", FECHA_CORTE, "."))
writeLines(dic, "diccionario_variables.md")
log_msg("=== FIN 03_limpieza_datos ===")
)---", "codigo/03_limpieza_datos.R", useBytes = TRUE)

x <- readLines("codigo/03_limpieza_datos.R", warn = FALSE, encoding = "UTF-8")
x[1] <- sub('^writeLines\\(r"---\\(', "", x[1])
x <- x[!grepl('^\\)---"', x)]
writeLines(x, "codigo/03_limpieza_datos.R", useBytes = TRUE)
head(x, 1); tail(x, 1)

source("codigo/03_limpieza_datos.R")
list.files("datos_procesados"); list.files("salidas")
cat(tail(readLines("log_ejecucion.txt"), 14), sep = "\n")


