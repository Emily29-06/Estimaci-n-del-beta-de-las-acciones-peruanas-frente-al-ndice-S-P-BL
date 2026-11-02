# Nombres y apellidos: Emily Lady Quinto de la Cruz
# Código de matrícula: 2024200518K
# Tema N.º 65 del temario: Estimación del beta de las acciones peruanas frente al índice S&P/BVL
# Fecha de extracción: 2026-10-23 (API) y 2026-10-24 (scraping); datos congelados al 2025-12-31
# -----------------------------------------------------------------------------
# 04_analisis.R
# Lee datos_procesados y genera TODAS las tablas y figuras del artículo en /salidas:
#   - beta por MCO con errores HAC (Newey-West), beta de Dimson (rezago/adelanto),
#     beta semanal y beta ajustado de Blume, por acción
#   - betas por subperiodo (pre-COVID, COVID-19, post) y betas anuales
#   - betas móviles de 250 días y estadísticos de estabilidad
#   - mapas de calor (beta anual y correlaciones), betas móviles y gráfico de betas
# No hay aleatoriedad en este análisis, por lo que no se fija semilla.
# Ejecutar desde 3_Base_de_datos_y_codigo:  source("codigo/04_analisis.R")
# -----------------------------------------------------------------------------

source("codigo/00_parametros.R")
instalar_si_falta(c("ggplot2", "zoo", "sandwich", "lmtest"))
library(ggplot2); library(zoo); library(sandwich); library(lmtest)
log_msg("=== INICIO 04_analisis ===")

VENTANA <- 250      # días de la ventana móvil (aprox. 1 año bursátil)
MIN_OBS <- 60       # mínimo de observaciones para estimar un beta anual o de subperiodo

# ---- 1. Datos ----------------------------------------------------------------
d <- read.csv(file.path(DIR_PROCESADOS, paste0("datos_procesados_", CODIGO, ".csv")), stringsAsFactors = FALSE)
d$fecha <- as.Date(d$fecha)
d <- d[order(d$simbolo, d$fecha), ]

# Rezago y adelanto del retorno del índice según el calendario bursátil (para el beta de Dimson)
cal <- unique(d[, c("fecha", "ret_indice")]); cal <- cal[order(cal$fecha), ]
cal$ri_l1 <- c(NA, head(cal$ret_indice, -1))
cal$ri_f1 <- c(tail(cal$ret_indice, -1), NA)
d <- merge(d, cal[, c("fecha", "ri_l1", "ri_f1")], by = "fecha")
d <- d[order(d$simbolo, d$fecha), ]
d$ra <- d$ret_accion_pen        # retorno de la acción en soles
d$ri <- d$ret_indice            # retorno del índice
dv <- d[!is.na(d$ra) & !is.na(d$ri), ]
log_msg("Observaciones con retorno de acción e índice: ", nrow(dv), " | acciones: ", length(unique(dv$simbolo)))

# ---- 2. Beta por acción: MCO + HAC, Dimson, Blume ----------------------------
estimar <- function(x) {
  m  <- lm(ra ~ ri, data = x)
  V  <- NeweyWest(m, lag = 5, prewhite = FALSE)
  b  <- unname(coef(m)[2]); se <- sqrt(V[2, 2])
  x3 <- x[!is.na(x$ri_l1) & !is.na(x$ri_f1), ]
  m3 <- lm(ra ~ ri + ri_l1 + ri_f1, data = x3)
  V3 <- NeweyWest(m3, lag = 5, prewhite = FALSE)
  w  <- c(0, 1, 1, 1)                                  # suma de los tres coeficientes
  bd <- sum(coef(m3)[2:4]); sed <- sqrt(as.numeric(t(w) %*% V3 %*% w))
  data.frame(simbolo = x$simbolo[1], n = nrow(x), beta_ols = b, se_hac = se, t = b / se,
             p = 2 * pt(-abs(b / se), df = nrow(x) - 2), r2 = summary(m)$r.squared,
             beta_dimson = bd, se_dimson = sed, beta_blume = (2 / 3) * b + 1 / 3,
             stringsAsFactors = FALSE)
}
res <- do.call(rbind, lapply(split(dv, dv$simbolo), estimar))

beta_semanal <- function(x) {                          # retornos semanales (suma de logs por semana ISO)
  x$sem <- format(x$fecha, "%G-%V")
  a <- aggregate(cbind(ra, ri) ~ sem, data = x, FUN = sum)
  n <- aggregate(ra ~ sem, data = x, FUN = length)
  a <- a[n$ra >= 3, ]
  unname(coef(lm(ra ~ ri, data = a))[2])
}
res$beta_semanal <- vapply(split(dv, dv$simbolo), beta_semanal, numeric(1))[res$simbolo]
res$pct_sin_negociacion <- round(100 * tapply(d$sin_negociacion, d$simbolo, mean)[res$simbolo], 1)
res <- res[order(-res$beta_ols), ]
write.csv(res, file.path(DIR_SALIDAS, "tabla_betas.csv"), row.names = FALSE)
log_msg("Beta MCO: promedio ", round(mean(res$beta_ols), 3), " | rango ", round(min(res$beta_ols), 3),
        " a ", round(max(res$beta_ols), 3), " | acciones con beta > 1: ", sum(res$beta_ols > 1))

# ---- 3. Betas por subperiodo y por año --------------------------------------
periodos <- list("2010-2019" = c("2010-01-01", "2019-12-31"),
                 "2020 (COVID-19)" = c("2020-01-01", "2020-12-31"),
                 "2021-2025" = c("2021-01-01", "2025-12-31"))
sub <- do.call(rbind, lapply(names(periodos), function(nm) {
  z <- dv[dv$fecha >= as.Date(periodos[[nm]][1]) & dv$fecha <= as.Date(periodos[[nm]][2]), ]
  do.call(rbind, lapply(split(z, z$simbolo), function(x) {
    if (nrow(x) < MIN_OBS) return(NULL)
    data.frame(periodo = nm, simbolo = x$simbolo[1], n = nrow(x),
               beta = unname(coef(lm(ra ~ ri, data = x))[2]), stringsAsFactors = FALSE)
  }))
}))
write.csv(sub, file.path(DIR_SALIDAS, "tabla_betas_subperiodos.csv"), row.names = FALSE)

dv$anio <- as.integer(format(dv$fecha, "%Y"))
ba <- do.call(rbind, lapply(split(dv, list(dv$simbolo, dv$anio), drop = TRUE), function(x) {
  if (nrow(x) < MIN_OBS) return(NULL)
  data.frame(simbolo = x$simbolo[1], anio = x$anio[1], beta = unname(coef(lm(ra ~ ri, data = x))[2]),
             stringsAsFactors = FALSE)
}))
write.csv(ba, file.path(DIR_SALIDAS, "tabla_betas_anuales.csv"), row.names = FALSE)

# ---- 4. Betas móviles y estabilidad ------------------------------------------
roll <- do.call(rbind, lapply(split(dv, dv$simbolo), function(x) {
  if (nrow(x) < VENTANA + 10) return(NULL)
  b <- rollapplyr(cbind(x$ra, x$ri), width = VENTANA,
                  FUN = function(m) cov(m[, 1], m[, 2]) / var(m[, 2]), by.column = FALSE, fill = NA)
  data.frame(simbolo = x$simbolo[1], fecha = x$fecha, beta_movil = as.numeric(b), stringsAsFactors = FALSE)
}))
roll <- roll[!is.na(roll$beta_movil), ]
est <- do.call(rbind, lapply(split(roll, roll$simbolo), function(z)
  data.frame(simbolo = z$simbolo[1], media = mean(z$beta_movil), desv_est = sd(z$beta_movil),
             minimo = min(z$beta_movil), maximo = max(z$beta_movil), stringsAsFactors = FALSE)))
write.csv(est, file.path(DIR_SALIDAS, "tabla_estabilidad_beta_movil.csv"), row.names = FALSE)
log_msg("Beta móvil (", VENTANA, " días): desviación estándar promedio ", round(mean(est$desv_est), 3))

# ---- 5. Figuras (PDF vectorial + PNG) -----------------------------------------
tema <- theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"),
        legend.position = "right")
guardar <- function(p, nombre, w, h) {
  ggsave(file.path(DIR_SALIDAS, paste0(nombre, ".pdf")), p, width = w, height = h)
  ggsave(file.path(DIR_SALIDAS, paste0(nombre, ".png")), p, width = w, height = h, dpi = 300)
}
orden <- res$simbolo[order(res$beta_ols)]                # acciones ordenadas por beta

# Figura 1: mapa de calor del beta anual por acción
ba$simbolo <- factor(ba$simbolo, levels = orden)
f1 <- ggplot(ba, aes(factor(anio), simbolo, fill = beta)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", beta)), size = 2.3) +
  scale_fill_gradient2(low = "#2C7BB6", mid = "#FFFFBF", high = "#D7191C", midpoint = 1, name = "Beta") +
  labs(x = "Año", y = NULL) + tema
guardar(f1, "fig1_mapa_calor_beta_anual", 9, 6)

# Figura 2: mapa de calor de correlaciones entre retornos de las acciones
ancho <- reshape(dv[, c("fecha", "simbolo", "ra")], idvar = "fecha", timevar = "simbolo", direction = "wide")
colnames(ancho) <- sub("^ra\\.", "", colnames(ancho))
cm <- cor(ancho[, -1], use = "pairwise.complete.obs")
ord <- hclust(as.dist(1 - cm))$order
cm <- cm[ord, ord]
cl <- expand.grid(x = colnames(cm), y = rownames(cm), stringsAsFactors = FALSE)
cl$r <- as.vector(cm)
cl$x <- factor(cl$x, levels = colnames(cm)); cl$y <- factor(cl$y, levels = rev(rownames(cm)))
f2 <- ggplot(cl, aes(x, y, fill = r)) + geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#2C7BB6", mid = "white", high = "#D7191C", midpoint = 0, limits = c(-0.2, 1), name = "Correlación") +
  labs(x = NULL, y = NULL) + tema + theme(axis.text.x = element_text(angle = 45, hjust = 1))
guardar(f2, "fig2_mapa_calor_correlaciones", 7.5, 6.5)

# Figura 3: betas móviles por acción
roll$simbolo <- factor(roll$simbolo, levels = orden)
f3 <- ggplot(roll, aes(fecha, beta_movil)) + geom_hline(yintercept = 1, color = "grey60", linetype = "dashed") +
  geom_line(color = "#2C7BB6", linewidth = 0.4) + facet_wrap(~simbolo, ncol = 3) +
  labs(x = NULL, y = paste0("Beta móvil (", VENTANA, " días)")) + tema + theme(strip.text = element_text(size = 7))
guardar(f3, "fig3_betas_moviles", 9, 8)

# Figura 4: beta MCO y beta de Dimson con intervalos de 95 %
pl <- rbind(data.frame(simbolo = res$simbolo, metodo = "MCO (HAC)", beta = res$beta_ols, se = res$se_hac),
            data.frame(simbolo = res$simbolo, metodo = "Dimson", beta = res$beta_dimson, se = res$se_dimson))
pl$simbolo <- factor(pl$simbolo, levels = orden)
f4 <- ggplot(pl, aes(beta, simbolo, color = metodo)) + geom_vline(xintercept = 1, color = "grey60", linetype = "dashed") +
  geom_pointrange(aes(xmin = beta - 1.96 * se, xmax = beta + 1.96 * se), position = position_dodge(width = 0.5), size = 0.3) +
  scale_color_manual(values = c("MCO (HAC)" = "#2C7BB6", "Dimson" = "#D7191C"), name = NULL) +
  labs(x = "Beta (IC 95 %)", y = NULL) + tema
guardar(f4, "fig4_betas_mco_dimson", 7, 6)

# ---- 6. Tablas en LaTeX (se incluyen en el artículo con \input) --------------
fila <- function(v) paste0(paste(v, collapse = " & "), " \\\\")
tab <- data.frame(Accion = res$simbolo, N = res$n, `Beta MCO` = sprintf("%.3f", res$beta_ols),
                  `EE (HAC)` = sprintf("%.3f", res$se_hac), `R2` = sprintf("%.3f", res$r2),
                  `Beta Dimson` = sprintf("%.3f", res$beta_dimson), `Beta semanal` = sprintf("%.3f", res$beta_semanal),
                  `Beta Blume` = sprintf("%.3f", res$beta_blume), `% sin neg.` = sprintf("%.1f", res$pct_sin_negociacion),
                  check.names = FALSE, stringsAsFactors = FALSE)
names(tab)[ncol(tab)] <- "\\% sin neg."          # en LaTeX el % debe escaparse
writeLines(c(paste0("\\begin{tabular}{l", strrep("r", ncol(tab) - 1), "}"), "\\toprule",
             fila(names(tab)), "\\midrule", apply(tab, 1, fila), "\\bottomrule", "\\end{tabular}"),
           file.path(DIR_SALIDAS, "tabla_betas.tex"))

log_msg("Figuras y tablas guardadas en /", DIR_SALIDAS)
log_msg("=== FIN 04_analisis ===")
