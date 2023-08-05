#Proyecto final 

#Instalar paquetes----
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")

#cargar paquetes----
library(readxl)
library(tidyverse)
library(ggplot2)

#importar base de datos----
balances_2014 <- read_excel("Data/balances_2014.xlsx")
balance_2014 <- tibble(balances_2014)

#analisis de data----
str(balance_2014)
str(empresas)

#seleccionar las columnas de interes ----
empresas <- balance_2014 %>% select(nombre_cia,situacion,tipo,tamanio,pais,provincia,canton,ciudad,ciiu4_nivel1,ciiu4_nivel6,
                                    v345,v539,v599,v499,v698,v498,trab_direc,trab_admin) %>% 
  rename(Empresas=nombre_cia,
         Status=situacion,
         Tipo_de_empresas=tipo,
         Tamanio=tamanio,
         Pais=pais,
         Provincia=provincia,
         Canton=canton,
         Ciudad=ciudad,
         Actividad_economica=ciiu4_nivel1,
         Subactividad_economica=ciiu4_nivel6,
         Activos_corrientes=v345,
         Pasivos_corrientes=v539,
         Pasivo=v599,
         Activo=v499,
         Patrimonio=v698,
         Activo_no_corriente=v498,
         Trab_directos= trab_direc,
         Trab_administrativos= trab_admin
  )
#agregar columnas calculadas
empresas <- empresas %>% mutate(
  Liquidez_corriente=Activos_corrientes/Pasivos_corrientes,
  Endeudamiento_del_activo=Pasivo/Activo,
  Endeudamiento_patrimonial=Pasivo/Patrimonio,
  Endeudamiento_del_Activo_fijo=Patrimonio/Activo_no_corriente,
  Apalancamiento=Activo/Patrimonio
) %>% view("empresas")

#limpiar datos nulos e infinitos 

empresas1 <- empresas %>% filter(
  !is.na(Apalancamiento)& !is.infinite(Apalancamiento)
)

empresas2 <- empresas %>% filter(
  !is.na(Endeudamiento_del_activo)& !is.infinite(Endeudamiento_del_activo)
) 

empresas3 <- empresas %>% filter(
  !is.na(Liquidez_corriente)& !is.infinite(Liquidez_corriente)
) 

empresas4 <- empresas %>% filter(
  !is.na(Endeudamiento_patrimonial)& !is.infinite(Endeudamiento_patrimonial)
)

empresas5<- empresas %>% filter(
  !is.na(Endeudamiento_del_Activo_fijo)& !is.infinite(Endeudamiento_del_Activo_fijo)
) 
empresas6<- empresas %>% filter(
  !is.na(Apalancamiento)& !is.infinite(Apalancamiento)
)
#Preguntas de investigacion----

#1.¿El endeudamiento del activo fue mayor en empresas micro + pequeñas vs. grandes? Las micro empresas las mas endudadas

endeudamientoactivo_promedio_tipoempresa <- empresas2 %>% group_by(Tamanio) %>% 
  summarise(endeudamientoactivo_prom= mean(Endeudamiento_del_activo, na.rm = TRUE)) %>% 
  view("Tipo de empresa mas endeudada por activos")

end_act_prom_ordenado <- endeudamientoactivo_promedio_tipoempresa %>% 
  arrange(desc(endeudamientoactivo_promedio_tipoempresa)) %>% 
  view("Mayor endeudamiento activo por tamanio de empresa ordenado")

#2.¿La liquidez por tipo de compañía es diferente entre aquellas empresas que tienen más de
#60 trabajadores directos y que cuenta con 100 a 800 trabajadores administrativos?

liquidez_promedio <- empresas3 %>%
  filter(Trab_directos > 60, Trab_administrativos >= 100, Trab_administrativos <= 800) %>%
  group_by(Tipo_de_empresas) %>%
  summarise(prom_liquidez = mean(Liquidez_corriente, na.rm = TRUE)) %>% 
  view("Liquidez por tipo de compania y trabajadores")

#3. top 10 empresas con mas apalancamiento 

top10_empresas_apalancamiento <- empresas1 %>% arrange(desc(Apalancamiento)) %>% slice_head(n=10) %>% 
  view("Top10Apalancamiento")

# Tabla resumen----
# Crea una tabla resumiendo el número total de empresas por actividad económica y
#por actividad económica por cada cantón. La tabla simplemente debe aparecer
#como un data frame o tibble en tu script.

empresas_por_actividad <- empresas %>%
  group_by(Actividad_economica) %>%
  summarise(num_empresas = n(), .groups = "drop") %>%
  arrange(desc(num_empresas)) %>%
 view("Tabla resumen 1") 

empresas_por_actividad_canton <- empresas %>%
  group_by(Actividad_economica, Canton) %>%
  summarise(num_empresas = n(), .groups = "drop")%>% 
  arrange(desc(num_empresas)) %>%
  view("Tabla resumen 2")

#GRAFICOS----

# Gráfico de barras de liquidez por status y provincia
# Status
ggplot(empresas3, aes(x = Status, y = Liquidez_corriente, fill = Provincia)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Status) +
labs(title = "Comparación de Liquidez por Status y Provincia",
       x = "Status",
       y = "Liquidez",
       fill = "Provincia")

# Provincia 
ggplot(empresas3, aes(x = Provincia, y = Liquidez_corriente, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Provincia) +
  labs(title = "Comparación de Liquidez por Status y Provincia",
       x = "Provincia",
       y = "Liquidez",
       fill = "Status")


# Gráfico de barras de solvencia por status y provincia
# Solvencia - Endeudamiento activo

ggplot(empresas2, aes(x = Status, y = Endeudamiento_del_activo, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Provincia) +
  labs(title = "Comparación de Solvencia endeudamiento activo por Status y Provincia",
       y = "Endeudamiento activo",
       fill = "Status")

#filtro por una provincia PASTAZA
empresas_pastaza <- empresas2 %>% filter(Provincia=="PASTAZA")

ggplot(empresas_pastaza, aes(x = Status, y = Endeudamiento_del_activo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de Solvencia 1 por Status y Provincia",
       x = "Status",
       y = "Solvencia")

# Solvencia - Endeudamiento patrimonial 
ggplot(empresas4, aes(x = Status, y = Endeudamiento_patrimonial, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Provincia) +
  labs(title = "Comparación de Solvencia endeudamiento patrimonial por Status y Provincia",
       y = "Endeudamiento patrimonial",
       fill = "Status")
# Solvencia - Endeudamiento activo fijo 
ggplot(empresas5, aes(x = Status, y = Endeudamiento_del_Activo_fijo, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Provincia) +
  labs(title = "Comparación de Solvencia endeudamiento del activo fijo por Status y Provincia",
       y = "Endeudamiento del activo fijo",
       fill = "Status")
# Solvencia - apalancamiento
ggplot(empresas6, aes(x = Status, y = Apalancamiento, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Provincia) +
  labs(title = "Comparación de Solvencia Apalancamiento por Status y Provincia",
       y = "Apalancamiento",
       fill = "Status")

# Gráfico de barras de liquidez por tipo de empresa
ggplot(empresas3, aes(x = Tipo_de_empresas, y = Liquidez_corriente, fill = Tipo_de_empresas)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Tipo_de_empresas) +
  labs(title = "Comparación de Liquidez por tipo de empresas",
       y = "Liquidez_corriente",
       fill = "Tipo de Empresas")

# Gráfico de barras de solvencia por tipo de empresa
# Solvencia - Endeudamiento activo
ggplot(empresas2, aes(x = Tipo_de_empresas, y = Endeudamiento_del_activo, fill = Tipo_de_empresas)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Tipo_de_empresas) +
  labs(title = "Comparación de Solvencia endeudamiento activo por Tipo de Empresas",
       y = "Endeudamiento activo",
       fill = "Tipo de empresas")
# Solvencia - Endeudamiento patrimonial 
ggplot(empresas4, aes(x = Tipo_de_empresas, y = Endeudamiento_patrimonial, fill = Tipo_de_empresas)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Tipo_de_empresas) +
  labs(title = "Comparación de Solvencia endeudamiento patrimonial por Tipo de empresas",
       y = "Endeudamiento patrimonial",
       fill = "Tipo de empresas")
# Solvencia - Endeudamiento activo fijo 
ggplot(empresas5, aes(x = Tipo_de_empresas, y = Endeudamiento_del_Activo_fijo, fill = Tipo_de_empresas)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Tipo_de_empresas) +
  labs(title = "Comparación de Solvencia endeudamiento del activo fijo por Tipo de empresas",
       y = "Endeudamiento del activo fijo",
       fill = "Tipo de empresas")
# Solvencia - apalancamiento
ggplot(empresas6, aes(x = Tipo_de_empresas, y = Apalancamiento, fill = Tipo_de_empresas)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Tipo_de_empresas) +
  labs(title = "Comparación de Solvencia Apalancamiento por Tipo de empresas",
       y = "Apalancamiento",
       fill = "Tipo de empresas")