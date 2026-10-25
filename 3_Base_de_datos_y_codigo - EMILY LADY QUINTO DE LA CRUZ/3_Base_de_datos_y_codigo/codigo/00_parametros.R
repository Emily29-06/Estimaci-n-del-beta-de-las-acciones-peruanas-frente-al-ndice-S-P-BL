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
