library(stringdist)
library(ggplot2)
library(scales)
library(RColorBrewer)
library("rstudioapi")    
library(patchwork)
library(tidyverse)
library(plotly)
library(ggiraph)
library(patchwork)

setwd(dirname(getActiveDocumentContext()$path))

data <- read.csv("Uso_Transporte_MAYO_2018-JUNIO_2020.csv", header = FALSE, sep = ";", stringsAsFactors = FALSE, skip =2, fileEncoding = "ISO-8859-1")

# Split each row based on multiple semicolons (;;)
data <- data %>%
  mutate(row_data = strsplit(as.character(V1), ";;")) %>%
  unnest(row_data) %>%
  mutate(row_data = strsplit(row_data, ";")) %>%
  unnest(row_data)
# Separate the row_data into separate columns
data <- data %>%
  separate(row_data, into = paste0("Category", 1:9), sep = ";", fill="right")
# Remove the first and last columns
data <- data %>%
  select(-1, -ncol(data)) %>%
  select(where(~ any(!is.na(.))))
# Remove the 10th column
data <- data[, -10]
# Get the names from the first row
new_names <- unlist(data[1, ])
# Rename the columns
data <- setNames(data, new_names)
# Filter rows based on conditions using if_any and if_all
data <- data %>%
  filter(if_any(everything(), ~ !(.x %in% names(data))),
         if_all(everything(), ~ !(.x == "?")))
# Remove "?" from all strings in the data frame
data <- data %>% mutate(across(everything(), ~str_replace_all(., "[-|!?]", " ")))
data <- data %>% mutate(across(everything(), ~str_replace_all(., "[,]", "")))
# Remove trailing spaces using if_any and if_all
data <- data %>%
  mutate(across(where(~any(str_detect(., "\\s"))), str_trim), 
         across(where(~all(str_detect(., "\\s"))), str_trim))
# Capitalize
data <- data %>%
  mutate(across(everything(), toupper))
# Function to replace accent marks
remove_accents <- function(string) {
  chartr("áéíóúüñÁÉÍÓÚÜ", "aeiouunAEIOUU", string)
}
# Apply the function to all columns
data <- data %>%
  mutate(across(everything(), remove_accents))
# Normalize strings by removing diacritics (accent marks)
normalize_string <- function(string) {
  return(stri_trans_general(string, "Latin-ASCII"))
}

# Convert to numeric
data$`TOTAL KM` <- as.numeric(as.character(data$`TOTAL KM`))
data$COMBUSTIBLE <- as.numeric(as.character(data$COMBUSTIBLE))

#-------------------------------------------------------------------------------------------------------------------#

# List of words for grouping
Provincias <- c("SAN JOSE", "GUANACASTE", "PUNTARENAS", "LIMON" , "CARTAGO" ,"HEREDIA", "ALAJUELA")

Cantones <- c("SAN JOSE", "SAN MARCOS", "SAN RAMON", "LIMON", "SARAQUIQUI", "MORA", "SANTA CRUZ", "LIBERIA", 
              "NICOYA", "ALAJUELA", "SAN RAFAEL", "SAN ISIDRO", "NARANJO", "QUESADA", "PALMARES", "PUNTARENAS", "GUACIMO", 
              "SANTO DOMINGO", "COTO BRUS", "MATINA", "GUAPILES", "CARIARI", "DESAMPARADOS", "LA URUCA", "SIQUIRRES", "SAN VICENTE", 
              "BUENOS AIRES", "POAS", "CURRIDABAT", "ESPARZA", "SAN PABLO", "PURRAL", "TURRIALBA", "CANAS", "DESAMPARADOS", "SAN JUAN", 
              "HORQUETAS", "RITA", "SAN PEDRO", "SAN RAFAEL ABAJO", "CARTAGO", "GUADALUPE", "SAN ISIDRO", "PARAISO", "SARTALILLO", 
              "AGUAS ZARCAS", "SAN FRANCISCO", "PUERTO VIEJO", "GUACIMA", "QUEPOS", "BAGACES", "SAN JUAN DE DIOS", "HEREDIA", "ZAPOTE", 
              "ABANGARITOS", "COLON", "PITAL", "ROXANA", "SAN ISIDRO", "BATAN", "UPALA", "PARRITA", "POZOS", "FORTUNA", "SAN RAFAEL ARRIBA", 
              "FLORENCIA", "SARDINAL", "GRECIA", "SAN VITO", "COPALCHI", "LOS CHILES", "SANTA ANA", "MATINILLA", "PUERTO JIMENEZ", "SANTIAGO", 
              "ALAJUELITA", "ESCAZU", "PATARRA", "SIXAOLA", "JACO", "SAN JOSECITO", "GOLFITO", "NANDAYURE", "RIO CUARTO", "SABALITO", "RIO SEGUNDO", 
              "PUENTE DE PIEDRA", "LA VIRGEN", "SAN JOSECITO", "JIMENEZ", "SAN MARCOS", "SAN ANTONIO", "DULCE NOMBRE DE JESUS", "OROTINA", "CANOAS", 
              "TRES RIOS", "CARRILLOS", "SAN IGNACIO", "PIEDADES NORTE", "PIEDADES", "TACARES", "DULCE NOMBRE", "TURRUCARES", "SAN PEDRO", "ATENAS", 
              "BUENOS AIRES", "SANTA LUCIA", "BRIBRI", "SARCHI", "SAN RAFAEL", "TOBOSI", "JUAN VINAS", "VUELTA DE JORCO", "GENERAL VIEJO", "SAN JUAN GRANDE", 
              "SAN JERONIMO", "AQUIARES", "SAN GABRIEL", "CINCO ESQUINAS", "COYOLAR", "PACAYAS", "SAN JOSE DE LA MONTANA", "QUEBRADILLAS", "SARCHI SUR", 
              "TIERRA BLANCA", "BARVA", "TUCURRIQUE", "MACACONA", "SAN PABLO", "SAN MIGUEL OESTE", "CIRRI", "ZARCERO", "LLANO GRANDE", "SALITRAL", "SAN CRISTOBAL SUR", 
              "SAN ANTONIO", "FRAILES", "CIPRESES", "SAN JERONIMO", "MATA DE PLATANO", "SAN CARLOS", "SABANA", "PEREZ ZELEDON", "GUANACASTE", "CIUDAD NEILY", "POCOCI", "PAVAS"
)

Instituciones <- c("TEATRO NACIONAL", "AEROPUERTO", "AREA METROPOLITANA", "ASAMBLEA LEGISLATIVA", "BANCO", "CANAL 7", "CASA PRESIDENCIAL|PRESIDENCIA DE LA REPUBLICA", 
                   "CCSS", "CENTRO DE CONVENCIONES", "CLUB UNION", "COLEGIO DE ABOGADOS", "COLEGIO DE INGENIEROS", "CONTRALORIA", "COUNTRY CLUB", "CRHOY", 
                   "DEFENSORIA DE LOS HABITANTES", "DIARIO EXTRA", "DINADECO", "IMAS", "EMBAJADA", "HOTEL", "ICE", "ICODER", "INCOFER", "ICT", "INA", "INDER", "INS","ACUEDUCTOS Y ALCANTARILLADOS|AYA|A Y A", 
                   "MINISTERIO DE EDUCACION|MEP", "ESTADIO NACIONAL", "FISCALIA GENERAL","MINISTERIO DE HACIENDA", "MINISTERIO DE OBRAS PUBLICAS|MOPT", "MINISTERIO DE TRABAJO", "MINISTERIO DE AMBIENTE", "MINISTERIO DE SALUD", 
                   "MUNICIPALIDAD", "MINISTERIO DE AGRICULTURA Y GANADERIA|MINISTERIO AGRICULTURA Y GANADERIA", "MINISTERIO DE SEGURIDAD","MINISTERIO DE ECONOMIA", "COOPEANDE", "INFOCOOP","RADIO", "REGISTRO NACIONAL", 
                   "SALA CONSTITUCIONAL", "TEATRO NACIONAL", "UNIVERSIDAD"
)

Destino_Mix <- c(Cantones,Instituciones)

#-------------------------------------------------------------------------------------------------------------------#
# Function to check if any word in group_words is present in the value
group_destinations_cantones <- function(value) {
  matched_words <- Destino_Mix[sapply(Destino_Mix, function(word) grepl(word, value, ignore.case = TRUE))]
  if (length(matched_words) > 0) {
    return(matched_words[1])  # Return the first matched word as the group
  } else {
    return("Other")  # Return "Other" if no match found
  }
}

# Apply the function to create a new column for grouping
data <- data %>%
  mutate(Destino_Group = sapply(DESTINO, group_destinations_cantones))

# Create a summary dataframe with counts of each group
group_counts <- data %>%
  group_by(Destino_Group) %>%
  summarise(count = n())

#-------------------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------------------#

##################### UNIDIMENSIONALES #########################################

#-------------------------------------------TIPO DE SERVICIO GENERAL----------------------------------------------------#

data_counts <- data %>%
  count(`TIPO SERVICIO`)

# Rename columns for clarity
colnames(data_counts) <- c("Servicio", "Cantidad")

# Create the ggplot object
servicio_plot <- ggplot(data_counts, aes(x = Servicio, y = Cantidad, fill = Servicio )) +
  geom_bar(stat = "identity") +
  labs(
    title = "<b>Tipos de Servicios</b>",  # Bold title
    x = "Servicio",  # X-axis label
    y = "Cantidad"   # Y-axis label
  ) +
  theme_minimal()

# Convert ggplot to plotly
servicio_plotly <- ggplotly(servicio_plot)

# Print the interactive plot
print(servicio_plotly)

#----------------------------------------------DESTINOS GENERAL - TOP 10----------------------------------------------#

# Get the top 10 destination groups
top_10_groups <- group_counts %>%
  top_n(10, wt = count) %>%
  mutate(Destino_Group = fct_reorder(Destino_Group, count))

# Create the ggplot object without fct_reorder
destino <- ggplot(top_10_groups, aes(x = count, y = Destino_Group, fill = Destino_Group)) +
  geom_bar(stat = "identity") +
  labs(x = "Visitas frecuentadas", y = "Destino", title = "<b>Top 10 destinos mas frecuentados</b>") +  # Bold title and axis labels
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 45, hjust = 1))

# Replace "other" with your desired text on the Y-axis
destino <- destino + scale_y_discrete(labels = function(x) ifelse(x == "Other", "OTROS", x))

# Convert ggplot to plotly
destino_plotly <- ggplotly(destino)

# Customize the layout to remove the legend and adjust Y-axis text angle
destino_plotly <- destino_plotly %>%
  layout(
    legend = list(orientation = "h", y = -0.15),  # Position legend below the plot
    yaxis = list(tickangle = 45)  # Rotate Y-axis text to 45 degrees
  )

# Print the plotly plot with the modified title and axis labels
print(destino_plotly)

#----------------------------------------------DIPUTADO/A GENERAL - TOP 5----------------------------------------------#

diputados_counts <- data %>%
  count(`DIPUTADO (A)`)

# Rename columns for clarity
colnames(diputados_counts) <- c("Diputados", "Viajes")

# Sort the dataframe by the travel column in descending order
diputados_counts <- diputados_counts %>%
  arrange(desc(Viajes))

# Take top 5 of deputies whith the most travels
top_10_diputados <- diputados_counts[1:10, ]

# Create the ggplot object
diputado_plot <- ggplot(top_10_diputados, aes(x = Viajes, y = reorder(Diputados, Viajes) , fill = Diputados)) +
  geom_bar(stat = "identity") +
  labs(
    title = "<b>Uso de Transporte por Diputado - TOP 10</b>",  # Bold title
    x = "CANTIDAD DE VIAJES",  # X-axis label
    y = '', # Y-axis label
    fill = "DIPUTADOS"   # fill-axis label
  ) +
  theme_minimal()

# Convert ggplot to plotly
diputado_plotly <- ggplotly(diputado_plot, tooltip = c("Viajes"))  # Customize label


# Print the interactive plot
print(diputado_plotly)


##################### BIDIMENSIONALES #########################################
#----------------------------------------------Total KM vs Chofer----------------------------------------------#

# Create the ggplot object 
totalkm_chofer_plot <-ggplot(data, aes(x = `TOTAL KM`, y = `CHOFER`)) +
  geom_point() +
  labs(
    title = "<b>Total KM vs. Chofer<b>",  # Bold title
    x = "Total de Kilómetros",       # X-axis label
    y = "Chofer"                     # Y-axis label
  ) +
  theme_minimal()

# Convert ggplot to plotly
totalkm_chofer_plotly <- ggplotly(totalkm_chofer_plot)

# Print the interactive plot
print(totalkm_chofer_plotly)


#----------------------------------------------Combustible vs Destino----------------------------------------------#







##################### MULTIDIMENSIONALES #########################################

# Aggregate data
summary_df <- data %>%
  group_by(`DIPUTADO (A)`, DESTINO, CHOFER, COMBUSTIBLE, `TIPO SERVICIO`) %>%
  summarise(TOTAL_KM_mean = mean(`TOTAL KM`, na.rm = TRUE))

# Convert non-numeric COMBUSTIBLE values to NA
summary_df$COMBUSTIBLE <- as.numeric(as.character(summary_df$COMBUSTIBLE))

# Clean up non-numeric or problematic values
summary_df$COMBUSTIBLE[is.na(summary_df$COMBUSTIBLE) | is.infinite(summary_df$COMBUSTIBLE) | summary_df$COMBUSTIBLE < 0] <- NA

# Create interactive plot with facet_grid
p <- plot_ly(data = summary_df, x = ~`DIPUTADO (A)`, y = ~TOTAL_KM_mean, color = ~`TIPO SERVICIO`,
             type = "bar", hoverinfo = "text",
             text = ~paste("DESTINO:", DESTINO, "<br>",
                           "CHOFER:", CHOFER, "<br>",
                           "COMBUSTIBLE:", COMBUSTIBLE, "<br>",
                           "TOTAL KM Mean:", TOTAL_KM_mean)) %>%
  layout(
    barmode = "group",
    title = list(text = "<b>Kilometros recorridos por Diputado (a)</b>", font = list(size = 16)),  # Bold title
    xaxis = list(tickangle = 45, title = "<b>Diputado (a)</b>"),  # Rotate X-axis by 45 degrees
    yaxis = list(title = "<b>Total Kilometros (KM)</b>"),  # Y-axis label in bold
    colorway = brewer.pal(5, "Set2")  # Specify at least 3 colors for color palette
  )

# Print the interactive plot
print(p)

##################### FACETA #########################################




##################### COMPUESTA #########################################
