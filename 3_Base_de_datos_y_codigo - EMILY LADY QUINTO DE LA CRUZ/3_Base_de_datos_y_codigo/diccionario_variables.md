# Diccionario de variables — datos_procesados

| Variable | Definición | Unidad | Frecuencia | Fuente (endpoint) |
|---|---|---|---|---|
| fecha | Fecha de la sesión bursátil (llave común) | aaaa-mm-dd | Diaria | Yahoo / BCRPData |
| simbolo | Ticker de la acción en Yahoo Finance | texto | — | query1.finance.yahoo.com/v8/finance/chart |
| moneda | Moneda de cotización informada por Yahoo | PEN/USD | — | Yahoo (meta.currency) |
| adjclose | Precio de cierre ajustado por dividendos y splits | moneda de cotización | Diaria | Yahoo |
| volume | Volumen negociado | acciones | Diaria | Yahoo |
| sin_negociacion | Verdadero si volumen es 0 o falta (precio arrastrado) | lógico | Diaria | Calculada |
| ret_accion | Retorno logarítmico diario de la acción | log | Diaria | Calculada |
| ret_accion_pen | Retorno logarítmico de la acción expresado en soles | log | Diaria | Calculada |
| indice | Índice General Bursátil BVL (S&P/BVL Peru General) | puntos | Diaria | BCRPData API PD38026MD |
| ret_indice | Retorno logarítmico diario del índice | log | Diaria | Calculada |
| tc_venta | Tipo de cambio interbancario venta | S/ por US$ | Diaria | BCRPData API PD04638PD |
| ret_tc | Variación logarítmica diaria del tipo de cambio | log | Diaria | Calculada |

Fecha de corte: 2025-12-31. Ventana: 2010-01-01 a 2025-12-31.
