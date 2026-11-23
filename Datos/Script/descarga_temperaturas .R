# Descarga de temperatura diaria por provincia (2015-2023)
# Fuente: Open-Meteo Historical Weather API (reanálisis ERA5), sin clave de API
# Paquetes: install.packages(c("jsonlite", "dplyr", "readr"))

library(jsonlite)
library(dplyr)
library(readr)

# 1. Provincias: código INE, capital y coordenadas ---------------------------
provincias <- tribble(
  ~cod_ine, ~provincia,        ~lat,    ~lon,
  "01", "Álava",               42.8467, -2.6716,
  "02", "Albacete",            38.9943, -1.8585,
  "03", "Alicante",            38.3452, -0.4810,
  "04", "Almería",             36.8381, -2.4597,
  "05", "Ávila",               40.6564, -4.6818,
  "06", "Badajoz",             38.8794, -6.9707,
  "07", "Baleares",            39.5696,  2.6502,
  "08", "Barcelona",           41.3874,  2.1686,
  "09", "Burgos",              42.3439, -3.6969,
  "10", "Cáceres",             39.4753, -6.3724,
  "11", "Cádiz",               36.5271, -6.2886,
  "12", "Castellón",           39.9864, -0.0513,
  "13", "Ciudad Real",         38.9863, -3.9291,
  "14", "Córdoba",             37.8882, -4.7794,
  "15", "A Coruña",            43.3623, -8.4115,
  "16", "Cuenca",              40.0704, -2.1374,
  "17", "Girona",              41.9794,  2.8214,
  "18", "Granada",             37.1773, -3.5986,
  "19", "Guadalajara",         40.6337, -3.1667,
  "20", "Gipuzkoa",            43.3183, -1.9812,
  "21", "Huelva",              37.2614, -6.9447,
  "22", "Huesca",              42.1401, -0.4089,
  "23", "Jaén",                37.7796, -3.7849,
  "24", "León",                42.5987, -5.5671,
  "25", "Lleida",              41.6176,  0.6200,
  "26", "La Rioja",            42.4627, -2.4450,
  "27", "Lugo",                43.0097, -7.5568,
  "28", "Madrid",              40.4168, -3.7038,
  "29", "Málaga",              36.7213, -4.4214,
  "30", "Murcia",              37.9922, -1.1307,
  "31", "Navarra",             42.8125, -1.6458,
  "32", "Ourense",             42.3358, -7.8639,
  "33", "Asturias",            43.3614, -5.8494,
  "34", "Palencia",            42.0095, -4.5288,
  "35", "Las Palmas",          28.1235, -15.4363,
  "36", "Pontevedra",          42.4310, -8.6444,
  "37", "Salamanca",           40.9701, -5.6635,
  "38", "Santa Cruz de Tenerife", 28.4636, -16.2518,
  "39", "Cantabria",           43.4623, -3.8100,
  "40", "Segovia",             40.9429, -4.1088,
  "41", "Sevilla",             37.3891, -5.9845,
  "42", "Soria",               41.7664, -2.4790,
  "43", "Tarragona",           41.1189,  1.2445,
  "44", "Teruel",              40.3456, -1.1065,
  "45", "Toledo",              39.8628, -4.0273,
  "46", "Valencia",            39.4699, -0.3763,
  "47", "Valladolid",          41.6523, -4.7245,
  "48", "Bizkaia",             43.2630, -2.9350,
  "49", "Zamora",              41.5035, -5.7440,
  "50", "Zaragoza",            41.6488, -0.8891
)

# 2. Función de descarga para una provincia ---------------------------------
descargar_provincia <- function(cod_ine, provincia, lat, lon,
                                inicio = "2015-01-01", fin = "2023-12-31") {
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive",
    "?latitude=", lat, "&longitude=", lon,
    "&start_date=", inicio, "&end_date=", fin,
    "&daily=temperature_2m_mean,temperature_2m_max,temperature_2m_min",
    "&timezone=Europe%2FMadrid"
  )

  # Hasta 5 intentos; si la API nos frena (429), esperamos 70 s
  # (el límite se reinicia cada minuto)
  resp <- NULL
  for (intento in 1:5) {
    resp <- tryCatch(suppressWarnings(fromJSON(url)), error = function(e) NULL)
    if (!is.null(resp$daily)) break
    message("  Límite alcanzado, esperando 70 s (intento ", intento, "/5)...")
    Sys.sleep(70)
  }
  if (is.null(resp$daily)) {
    warning("Fallo en: ", provincia)
    return(NULL)
  }

  data.frame(
    cod_ine   = cod_ine,
    provincia = provincia,
    fecha     = as.Date(resp$daily$time),
    tmed      = resp$daily$temperature_2m_mean,
    tmax      = resp$daily$temperature_2m_max,
    tmin      = resp$daily$temperature_2m_min
  )
}

# 3. Descargar las 50 provincias --------------------------------------------
dir.create("datos", showWarnings = FALSE)
archivo_csv <- "datos/temperatura_provincias_2015_2023.csv"

# Si ya hay un CSV de una ejecución anterior, lo leemos y solo descargamos
# las provincias que faltan
if (file.exists(archivo_csv)) {
  temperaturas <- read_csv(archivo_csv, col_types = cols(cod_ine = col_character()))
} else {
  temperaturas <- tibble()
}
hechas <- unique(temperaturas$cod_ine)
pendientes <- provincias %>% filter(!cod_ine %in% hechas)
message("Provincias ya descargadas: ", length(hechas),
        " | pendientes: ", nrow(pendientes))

for (i in seq_len(nrow(pendientes))) {
  message("Descargando ", pendientes$provincia[i], " (", i, "/", nrow(pendientes), ")")
  nuevo <- do.call(descargar_provincia, as.list(pendientes[i, ]))
  if (!is.null(nuevo)) {
    temperaturas <- bind_rows(temperaturas, nuevo)
    write_csv(temperaturas, archivo_csv)  # guardamos tras cada provincia
  }
  Sys.sleep(10)  # pausa larga para no superar el límite de la API
}

# 4. Resumen final (el CSV ya se ha guardado tras cada provincia) -----------
message("Provincias descargadas: ", n_distinct(temperaturas$cod_ine), " de 50")

glimpse(temperaturas)

