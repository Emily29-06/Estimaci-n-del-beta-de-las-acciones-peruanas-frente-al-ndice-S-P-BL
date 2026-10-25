# Incidencias de fuente — Tema 65 (Finanzas I, Unidad II)

**Estudiante:** Emily Lady Quinto de la Cruz · código 2024200518K

## Incidencia 1: el índice S&P/BVL Peru General no tiene historia en Yahoo Finance
- **Fecha y hora:** 2026-10-21, ~19:23–19:36 (ver `log_ejecucion.txt`)
- **Solicitud:** `https://query1.finance.yahoo.com/v8/finance/chart/%5ESPBLPGPT` (símbolo `^SPBLPGPT`)
- **Resultado:** HTTP 404 en los tres intentos. Las 13 acciones locales respondieron HTTP 200.
- **Búsqueda de símbolo alterno:** `.../v1/finance/search` con las consultas "SPBLPGPT", "S&P Lima", "BVL Peru" devolvió HTTP 200 sin resultados; "Peru General" solo devolvió índices MSCI; "iShares MSCI Peru" solo el ETF EPU. Ninguno es el índice S&P/BVL.
- **Captura de pantalla:** adjuntar (consola de RStudio del 21/10/2026).

## Sustitución autorizada
- **Autorización del docente:** el Dr. Machacuay autorizó cambiar la fuente (fecha: COMPLETAR; adjuntar captura del mensaje).
- **Fuente sustituta:** BCRPData, serie diaria *Índice General Bursátil BVL (índice)*, código `PD38026MD`, por API (script 01) y por rastreo de la tabla HTML pública (script 02, verificación).
- **Se mantiene** Yahoo Finance (API) para las acciones.
