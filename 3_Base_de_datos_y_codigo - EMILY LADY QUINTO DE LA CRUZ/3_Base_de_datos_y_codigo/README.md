# Estimación del beta de las acciones peruanas frente al índice S&P/BVL

**Asignatura:** Finanzas I (055D) · Escuela Profesional de Economía · UNCP · 2026-II · Unidad II
**Docente:** Dr. Ciro Iván Machacuay Meza
**Estudiante:** Emily Lady Quinto de la Cruz · **Código de matrícula:** 2024200518K
**Tema del temario:** N.º 65 — Estimación del beta de las acciones peruanas frente al índice S&P/BVL

## Fuentes de datos y endpoints
| Vía | Fuente | Endpoint / URL | Uso |
|---|---|---|---|
| API | Yahoo Finance | `https://query1.finance.yahoo.com/v8/finance/chart/<SIMBOLO>` (interval=1d) | Precio ajustado, volumen y moneda de las acciones |
| API | BCRPData | `https://estadisticas.bcrp.gob.pe/estadisticas/series/api/<CODIGO>/json/<inicio>/<fin>/esp` | Índice General Bursátil BVL (`PD38026MD`) y tipo de cambio interbancario venta (`PD04638PD`) |
| Scraping | BCRPData (HTML) | `https://estadisticas.bcrp.gob.pe/estadisticas/series/diarias/resultados/PD38026MD` | Misma serie del índice, rastreada con selectores `td.periodo` y `td.dato`, para verificación |

El índice S&P/BVL Peru General no tiene historia en Yahoo Finance (HTTP 404); la sustitución de fuente fue autorizada por el docente (ver `incidencias_fuente.md`).

## Parámetros congelados
- `FECHA_INICIO = 2010-01-01` · `FECHA_CORTE = 2025-12-31` (constantes en `codigo/00_parametros.R`).
- Muestra: las acciones con mayor cobertura (mínimo 2 500 días con precio), máximo 18–20.

## Orden de ejecución
Desde la carpeta de este proyecto (directorio de trabajo), en R:
1. `source("codigo/01_extraccion_api.R")` — Yahoo + BCRP API → `datos_crudos/`
2. `source("codigo/02_scraping_web.R")` — BCRP HTML → `datos_crudos/`
3. `source("codigo/03_limpieza_datos.R")` — limpieza y unión → `datos_procesados/datos_procesados_2024200518K.csv`
4. `source("codigo/04_analisis.R")` — betas, pruebas, tablas y figuras → `salidas/`

`codigo/00_parametros.R` lo cargan los demás scripts. No se requieren claves de API (ver `.env.example`).

## Versión del lenguaje y librerías
R 4.5.2. Versiones exactas de los paquetes (httr2, jsonlite, rvest, xml2, digest, etc.) en `sessionInfo.txt`.

## Integridad
- **SHA-256 de `datos_procesados_2024200518K.csv`:** 90d7ec827b9e1b6bc927819d835ef8b8cafab9c924523273a97780b511eaf08a.
- El hash se coteja contra el archivo entregado. El log de ejecución (`log_ejecucion.txt`) registra fecha y hora de cada extracción, filas descargadas y código HTTP.
- Los datos crudos no se editan. Los valores atípicos se marcan en el procesado y se registran en `salidas/atipicos_detectados.csv`.

## Repositorio
Enlace: COMPLETAR (URL de GitHub).
