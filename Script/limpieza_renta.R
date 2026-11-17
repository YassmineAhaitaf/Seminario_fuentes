# Limpieza de la renta (INE, ADRH, tabla 53689)
# Resultado: una fila por provincia y año, con código INE de 2 dígitos
# Paquetes: library(tidyverse)

library(tidyverse)

# 1. Tabla de códigos INE (los nombres están tal cual vienen en el CSV) -----
claves <- tribble(
  ~provincia,                ~cod_ine,
  "Araba/Álava",             "01",
  "Albacete",                "02",
  "Alicante/Alacant",        "03",
  "Almería",                 "04",
  "Ávila",                   "05",
  "Badajoz",                 "06",
  "Balears, Illes",          "07",
  "Barcelona",               "08",
  "Burgos",                  "09",
  "Cáceres",                 "10",
  "Cádiz",                   "11",
  "Castellón/Castelló",      "12",
  "Ciudad Real",             "13",
  "Córdoba",                 "14",
  "Coruña, A",               "15",
  "Cuenca",                  "16",
  "Girona",                  "17",
  "Granada",                 "18",
  "Guadalajara",             "19",
  "Gipuzkoa",                "20",
  "Huelva",                  "21",
  "Huesca",                  "22",
  "Jaén",                    "23",
  "León",                    "24",
  "Lleida",                  "25",
  "Rioja, La",               "26",
  "Lugo",                    "27",
  "Madrid",                  "28",
  "Málaga",                  "29",
  "Murcia",                  "30",
  "Navarra",                 "31",
  "Ourense",                 "32",
  "Asturias",                "33",
  "Palencia",                "34",
  "Palmas, Las",             "35",
  "Pontevedra",              "36",
  "Salamanca",               "37",
  "Santa Cruz de Tenerife",  "38",
  "Cantabria",               "39",
  "Segovia",                 "40",
  "Sevilla",                 "41",
  "Soria",                   "42",
  "Tarragona",               "43",
  "Teruel",                  "44",
  "Toledo",                  "45",
  "Valencia/València",       "46",
  "Valladolid",              "47",
  "Bizkaia",                 "48",
  "Zamora",                  "49",
  "Zaragoza",                "50",
  "Ceuta",                   "51",
  "Melilla",                 "52"
)

# 2. Leer y limpiar --------------------------------------------------------
renta <- read_csv2("Datos/Datos_renta.csv", show_col_types = FALSE)

renta_prov <- renta %>%
  rename(provincia = Provincias,
         indicador = `Indicadores de renta media`,
         anio      = Periodo) %>%
  filter(!is.na(provincia),                       # solo filas provinciales
         is.na(Islas),                            # sin filas de islas
         indicador %in% c("Renta neta media por persona",
                          "Mediana de la renta por unidad de consumo")) %>%
  mutate(renta = parse_number(Total,
                              locale = locale(decimal_mark = ",", grouping_mark = "."))) %>%
  left_join(claves, by = "provincia") %>%
  filter(!cod_ine %in% c("51", "52")) %>%         # fuera Ceuta y Melilla
  mutate(indicador = case_when(
    indicador == "Renta neta media por persona" ~ "renta_media_persona",
    TRUE                                        ~ "renta_mediana_uc")) %>%
  select(cod_ine, provincia, anio, indicador, renta) %>%
  pivot_wider(names_from = indicador, values_from = renta)
# Resultado: una fila por provincia y año, con dos columnas de renta:
#   renta_media_persona  (renta neta media por persona, en euros)
#   renta_mediana_uc     (mediana de la renta por unidad de consumo, en euros)

# 3. Comprobaciones --------------------------------------------------------
stopifnot(!any(is.na(renta_prov$cod_ine)))        # todas las provincias con código
n_distinct(renta_prov$cod_ine)                    # debe ser 50
nrow(renta_prov)                                  # debe ser 450 (50 x 9)
renta_prov %>%
  filter(is.na(renta_media_persona) | is.na(renta_mediana_uc)) %>%
  count(provincia, name = "anios_sin_dato")       # solo Navarra y Araba/Álava

