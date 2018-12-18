library(here)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(purrr)

rm(list=ls())

d <- read.csv(file = here::here("BASES", "defunciones.txt"), header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)

d$id_ocurr <- paste(d$mun_n_ocurr, d$ent_n_ocurr,  sep = ", ")
d$fecha_ocurr_ano2 <- ymd(paste(d$fecha_ocurr_ano, "01", "01", sep = "-"))
d$fecha_ocurr_anomes2 <- ymd(paste(d$fecha_ocurr_anomes, "01", sep = "-"))
d$fecha_ocurr_anomesdia2 <- ymd(d$fecha_ocurr_anomesdia)
d$edad_3 <- as.character(cut(x = d$edad_2, breaks = seq(from = 0, to = 120, by = 20)))






#Evolución de las principales causas de muerte por mes (relativo) ####

d_group_anomes <- d %>% 
  filter(fecha_ocurr_anomes2 >= "2012-01-01") %>% 
  group_by(fecha_ocurr_anomes2, lista_mex_1, lista_mex_1_c) %>% 
  summarise("n" = n()) %>% 
  arrange(fecha_ocurr_anomes2, lista_mex_1)

tots <- d_group_anomes %>% 
  group_by(fecha_ocurr_anomes2) %>% 
  summarise("n" = sum(n)) %>% 
  arrange(fecha_ocurr_anomes2)

d_group_anomes$tot <- NA
for(i in seq_along(tots$n)){
  d_group_anomes$tot[d_group_anomes$fecha_ocurr_anomes2 == tots$fecha_ocurr_anomes2[i]] <- tots$n[i]
}
d_group_anomes$perc <- NA
d_group_anomes$perc <- (d_group_anomes$n / d_group_anomes$tot) * 100

#d_group_anomes[is.na(d_group_anomes$tot),]
#sapply(unique(d_group_anomes$fecha_ocurr_anomes2), function(x){
#  sum(d_group_anomes$perc[d_group_anomes$fecha_ocurr_anomes2 == x])
#})


tokeep <- d_group_anomes %>% 
  filter(fecha_ocurr_anomes2 == "2017-12-01") %>%
  group_by(lista_mex_1, lista_mex_1_c) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  slice(1:10)

d_group_anomes <- d_group_anomes %>% 
  filter(lista_mex_1 %in% tokeep$lista_mex_1)

levels <- tokeep$lista_mex_1_c
d_group_anomes$lista_mex_1_c <- factor(x = d_group_anomes$lista_mex_1_c, 
                                       levels = levels, 
                                       labels = levels)

plot_pormes_perc <- ggplot(d_group_anomes) +
  
  geom_line(mapping = aes(x = fecha_ocurr_anomes2, y = perc, color = lista_mex_1_c), 
            show.legend = TRUE, alpha = .5) +
  geom_smooth(mapping = aes(x = fecha_ocurr_anomes2, y = perc, color = lista_mex_1_c), 
              show.legend = TRUE, se = FALSE, span = .3) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m", 
               date_minor_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  
  guides(col=guide_legend(title="Causa de muerte")) +
  xlab("Fecha") + 
  ylab("Porcentaje") +
  ggtitle(label = "Evolución de las principales causas de muerte por mes (como % del total de muertes en el periodo)", 
          subtitle = "registrado por el registro civil y las agencias del ministerio público (INEGI)\n(lineas gruesas destacan la tendencia)")








#Evolución de las principales causas de muerte por mes ####


tokeep <- d %>% 
  filter(fecha_ocurr_ano == 2017) %>%
  group_by(lista_mex_1, lista_mex_1_c) %>% 
  summarise("n" = n()) %>%
  arrange(desc(n)) %>% 
  ungroup() %>% 
  slice(1:10)


d_group_anomes <- d %>% 
  filter(fecha_ocurr_ano >= 2012) %>% 
  filter(lista_mex_1 %in% tokeep$lista_mex_1) %>% 
  group_by(lista_mex_1, lista_mex_1_c, fecha_ocurr_anomes2) %>% 
  summarise("n" = n()) %>%
  arrange(desc(n))

levels <- data.frame(d_group_anomes %>% 
  filter(fecha_ocurr_anomes2 == "2017-12-01") %>% 
  arrange(desc(n)))$lista_mex_1_c
d_group_anomes$lista_mex_1_c <- factor(x = d_group_anomes$lista_mex_1_c, 
                                       levels = levels, 
                                       labels = levels)

plot_pormes <- ggplot(d_group_anomes) +
  
  geom_line(mapping = aes(x = fecha_ocurr_anomes2, y = n, color = lista_mex_1_c), 
            show.legend = TRUE, alpha = .5) +
  geom_smooth(mapping = aes(x = fecha_ocurr_anomes2, y = n, color = lista_mex_1_c), 
              show.legend = TRUE, se = FALSE, span = .5) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m", 
               date_minor_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  
  guides(col=guide_legend(title="Causa de muerte")) +
  xlab("Fecha") + 
  ylab("Número de defunciones") +
  ggtitle(label = "Evolución de las principales causas de muerte por mes", 
          subtitle = "registrado por el registro civil y las agencias del ministerio público (INEGI)\n(lineas gruesas destacan la tendencia)")







#Evolución de las principales causas de muerte por año ####


tokeep <- d %>% 
  filter(fecha_ocurr_ano == 2017) %>%
  group_by(lista_mex_1, lista_mex_1_c) %>% 
  summarise("n" = n()) %>%
  arrange(desc(n)) %>% 
  ungroup() %>% 
  slice(1:10)


d_group_ano <- d %>% 
  filter(fecha_ocurr_ano >= 2012) %>% 
  filter(lista_mex_1 %in% tokeep$lista_mex_1) %>% 
  group_by(lista_mex_1, lista_mex_1_c, fecha_ocurr_ano2) %>% 
  summarise("n" = n()) %>%
  arrange(desc(n))

levels <- data.frame(d_group_ano %>% 
                       filter(fecha_ocurr_ano2 == "2017-01-01") %>% 
                       arrange(desc(n)))$lista_mex_1_c
d_group_ano$lista_mex_1_c <- factor(x = d_group_ano$lista_mex_1_c, 
                                       levels = levels, 
                                       labels = levels)

plot_poraño <- ggplot(d_group_ano) +
  
  geom_line(mapping = aes(x = fecha_ocurr_ano2, y = n, color = lista_mex_1_c), 
            show.legend = TRUE, alpha = 1, size = 1) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  
  guides(col=guide_legend(title="Causa de muerte")) +
  xlab("Fecha") + 
  ylab("Número de defunciones") +
  ggtitle(label = "Evolución de las principales causas de muerte por año", 
          subtitle = "registrado por el registro civil y las agencias del ministerio público (INEGI)")








#Evolución de las principales causas de muerte por mes en el 2017 ####

tokeep <- d %>% 
  filter(fecha_ocurr_ano == 2017) %>%
  group_by(lista_mex_1, lista_mex_1_c) %>% 
  summarise("n" = n()) %>%
  arrange(desc(n)) %>% 
  ungroup() %>% 
  slice(1:10)

d_group_mes2017 <- d %>% 
  filter(fecha_ocurr_ano == 2017) %>% 
  filter(lista_mex_1 %in% tokeep$lista_mex_1) %>% 
  group_by(lista_mex_1, lista_mex_1_c, fecha_ocurr_anomes2) %>% 
  summarise("n" = n()) %>%
  arrange(desc(n))

levels <- data.frame(d_group_mes2017 %>% 
                       filter(fecha_ocurr_anomes2 == "2017-12-01") %>% 
                       arrange(desc(n)))$lista_mex_1_c
d_group_mes2017$lista_mex_1_c <- factor(x = d_group_mes2017$lista_mex_1_c, 
                                       levels = levels, 
                                       labels = levels)

plot_2017 <- ggplot(d_group_mes2017) +
  
  geom_line(mapping = aes(x = fecha_ocurr_anomes2, y = n, color = lista_mex_1_c), 
            show.legend = TRUE, alpha = .8) +
  geom_smooth(mapping = aes(x = fecha_ocurr_anomes2, y = n, color = lista_mex_1_c), 
              show.legend = TRUE, se = FALSE, span = .5) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  
  guides(col=guide_legend(title="Causa de muerte")) +
  xlab("Fecha") + 
  ylab("Número de defunciones") +
  ggtitle(label = "Evolución de las principales causas de muerte por mes en el 2017", 
          subtitle = "registrado por el registro civil y las agencias del ministerio público (INEGI)")



#Principales causas de muerte (como % del total de muertes en el periodo para cada grupo) EDAD ####


cats <- unique(d$edad_3)[order(unique(d$edad_3))]
cats <- cats[!is.na(cats)]
cats <- cats[c(1,3,4,5,6,2)]

d_list <- lapply(cats, function(x){
  d %>% 
    filter(edad_3 == x) %>% 
    filter(fecha_ocurr_anomes2 >= "2012-01-01") %>% 
    group_by(edad_3, fecha_ocurr_anomes2, lista_mex_1, lista_mex_1_c) %>% 
    summarise("n" = n()) %>% 
    arrange(fecha_ocurr_anomes2, lista_mex_1)
}) ; names(d_list) <- cats

d_list <- lapply(d_list, function(d){
  tots <- d %>% 
    group_by(fecha_ocurr_anomes2) %>% 
    summarise("n" = sum(n)) %>% 
    arrange(fecha_ocurr_anomes2)
  
  d$tot <- NA
  for(i in seq_along(tots$n)){
    d$tot[d$fecha_ocurr_anomes2 == tots$fecha_ocurr_anomes2[i]] <- tots$n[i]
  }
  
  d$perc <- NA
  d$perc <- (d$n / d$tot) * 100
  
  return(d)
})


d_list <- lapply(d_list, function(d){
  tokeep <- d %>% 
    filter(fecha_ocurr_anomes2 == "2017-12-01") %>%
    group_by(lista_mex_1, lista_mex_1_c) %>% 
    arrange(desc(n)) %>% 
    ungroup() %>% 
    slice(1:10)
  
  d <- d %>% 
    filter(lista_mex_1 %in% tokeep$lista_mex_1)
  
  levels <- tokeep$lista_mex_1_c
  d$lista_mex_1_c <- factor(x = d$lista_mex_1_c, 
                            levels = levels, 
                            labels = levels)
  
  return(d)
})



plot_gruposedad <- lapply(d_list, function(d){
  ggplot(d) +
    
    geom_line(mapping = aes(x = fecha_ocurr_anomes2, y = perc, color = lista_mex_1_c), 
              show.legend = TRUE, alpha = .5) +
    geom_smooth(mapping = aes(x = fecha_ocurr_anomes2, y = perc, color = lista_mex_1_c), 
                show.legend = TRUE, se = FALSE, span = .3) +
    
    scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m", 
                 date_minor_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    
    guides(col=guide_legend(title="Causa de muerte")) +
    xlab("Fecha") + 
    ylab("Porcentaje") +
    ggtitle(label = paste("Principales causas de muerte (como % del total de muertes en el periodo para cada grupo) ", 
                          paste(": ", unique(d$edad_3)), " años", sep = ""), 
            subtitle = "registrado por el registro civil y las agencias del ministerio público (INEGI)\n(lineas gruesas destacan la tendencia)")
})

plot_gruposedad <- do.call(what = "grid.arrange", plot_gruposedad)









#Principales causas de muerte (como % del total de muertes en el periodo para cada grupo) SEXO ####


table(d$sexo_c, useNA = "always")
d_sexo <- d[d$sexo %in% c(1, 2),]


cats <- unique(d_sexo$sexo_c)[order(unique(d_sexo$sexo_c))]
cats <- cats[!is.na(cats)]


d_list <- lapply(cats, function(x){
  d %>% 
    filter(sexo_c == x) %>% 
    filter(fecha_ocurr_anomes2 >= "2012-01-01") %>% 
    group_by(sexo_c, fecha_ocurr_anomes2, lista_mex_1, lista_mex_1_c) %>% 
    summarise("n" = n()) %>% 
    arrange(fecha_ocurr_anomes2, lista_mex_1)
}) ; names(d_list) <- cats

d_list <- lapply(d_list, function(d){
  tots <- d %>% 
    group_by(fecha_ocurr_anomes2) %>% 
    summarise("n" = sum(n)) %>% 
    arrange(fecha_ocurr_anomes2)
  
  d$tot <- NA
  for(i in seq_along(tots$n)){
    d$tot[d$fecha_ocurr_anomes2 == tots$fecha_ocurr_anomes2[i]] <- tots$n[i]
  }
  
  d$perc <- NA
  d$perc <- (d$n / d$tot) * 100
  
  return(d)
})




d_list <- lapply(d_list, function(d){
  tokeep <- d %>% 
    filter(fecha_ocurr_anomes2 == "2017-12-01") %>%
    group_by(lista_mex_1, lista_mex_1_c) %>% 
    arrange(desc(n)) %>% 
    ungroup() %>% 
    slice(1:10)
  
  d <- d %>% 
    filter(lista_mex_1 %in% tokeep$lista_mex_1)
  
  levels <- tokeep$lista_mex_1_c
  d$lista_mex_1_c <- factor(x = d$lista_mex_1_c, 
                            levels = levels, 
                            labels = levels)
  
  return(d)
})



plot_grupossexo <- lapply(d_list, function(d){
  ggplot(d) +
    
    geom_line(mapping = aes(x = fecha_ocurr_anomes2, y = perc, color = lista_mex_1_c), 
              show.legend = TRUE, alpha = .5) +
    geom_smooth(mapping = aes(x = fecha_ocurr_anomes2, y = perc, color = lista_mex_1_c), 
                show.legend = TRUE, se = FALSE, span = .3) +
    
    scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m", 
                 date_minor_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    
    guides(col=guide_legend(title="Causa de muerte")) +
    xlab("Fecha") + 
    ylab("Porcentaje") +
    ggtitle(label = paste("Principales causas de muerte (como % del total de muertes en el periodo para cada grupo) ", 
                          paste(": ", unique(d$sexo_c)), sep = ""), 
            subtitle = "registrado por el registro civil y las agencias del ministerio público (INEGI)\n(lineas gruesas destacan la tendencia)")
})

plot_grupossexo <- do.call(what = "grid.arrange", plot_grupossexo)












#Principales causas de muerte (como % del total de muertes en el periodo para cada ESTADO ####


table(d$ent_n_ocurr, useNA = "always")
d_ent <- d[d$ent_ocurr %in% 1:32,]


cats <- unique(d_ent$ent_n_ocurr)[order(unique(d_ent$ent_n_ocurr))]
cats <- cats[!is.na(cats)]


d_list <- lapply(cats, function(x){
  d %>% 
    filter(ent_n_ocurr == x) %>% 
    filter(fecha_ocurr_anomes2 >= "2012-01-01") %>% 
    group_by(ent_ocurr, ent_n_ocurr, fecha_ocurr_anomes2, fecha_ocurr_ano, lista_mex_1, lista_mex_1_c) %>% 
    summarise("n" = n()) %>% 
    arrange(fecha_ocurr_anomes2, lista_mex_1)
}) ; names(d_list) <- cats

d_list <- lapply(d_list, function(d){
  tots <- d %>% 
    group_by(fecha_ocurr_anomes2) %>% 
    summarise("n" = sum(n)) %>% 
    arrange(fecha_ocurr_anomes2)
  
  d$tot <- NA
  for(i in seq_along(tots$n)){
    d$tot[d$fecha_ocurr_anomes2 == tots$fecha_ocurr_anomes2[i]] <- tots$n[i]
  }
  
  d$perc <- NA
  d$perc <- (d$n / d$tot) * 100
  
  return(d)
})

d_list_10 <- lapply(d_list, function(d){
  tokeep <- d %>% 
    filter(fecha_ocurr_anomes2 == "2017-12-01") %>%
    group_by(lista_mex_1, lista_mex_1_c) %>% 
    arrange(desc(n)) %>% 
    ungroup() %>% 
    slice(1:10)
  
  d <- d %>% 
    filter(lista_mex_1 %in% tokeep$lista_mex_1)
  
  levels <- tokeep$lista_mex_1_c
  d$lista_mex_1_c <- factor(x = d$lista_mex_1_c, 
                            levels = levels, 
                            labels = levels)
  
  return(d)
})

plot_gruposedos <- lapply(d_list_10, function(d){
  ggplot(d) +
    
    geom_line(mapping = aes(x = fecha_ocurr_anomes2, y = perc, color = lista_mex_1_c), 
              show.legend = TRUE, alpha = .5) +
    geom_smooth(mapping = aes(x = fecha_ocurr_anomes2, y = perc, color = lista_mex_1_c), 
                show.legend = TRUE, se = FALSE, span = .3) +
    
    scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m", 
                 date_minor_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    
    guides(col=guide_legend(title="Causa de muerte")) +
    xlab("Fecha") + 
    ylab("Porcentaje") +
    ggtitle(label = paste("Principales causas de muerte (como % del total de muertes en el periodo para cada estado) ", 
                          paste(": ", unique(d$ent_n_ocurr)), sep = ""), 
            subtitle = "registrado por el registro civil y las agencias del ministerio público (INEGI)\n(lineas gruesas destacan la tendencia)")
})

plot_gruposedos <- do.call(what = "grid.arrange", plot_gruposedos)



##### por entidad####


d_list_homi <- lapply(d_list, function(d){
  d %>% 
    filter(lista_mex_1 == 55)
})

cat_fecha <- d %>% 
  filter(fecha_ocurr_ano >= 2012) %>% 
  filter(!is.na(fecha_ocurr_anomes2)) %>% 
  group_by(fecha_ocurr_anomes2) %>%  
  summarise("n2" = n())
cat_fecha$n2 <- NULL


d_list_homi <- lapply(d_list_homi, function(d){
  
  ent_ocurr <- d$ent_ocurr[1]
  ent_n_ocurr <- d$ent_n_ocurr[1]
  lista_mex_1 <- d$lista_mex_1[1]
  lista_mex_1_c <- d$lista_mex_1_c[1]
  
  d <- full_join(x = cat_fecha, y = d)
  
  d$ent_ocurr[is.na(d$perc)] <- ent_ocurr
  d$ent_n_ocurr[is.na(d$perc)] <- ent_n_ocurr
  d$lista_mex_1[is.na(d$perc)] <- lista_mex_1
  d$lista_mex_1_c[is.na(d$perc)] <- lista_mex_1_c
  d$ent_ocurr[is.na(d$perc)] <- ent_ocurr
  d$n[is.na(d$perc)] <- 0
  d$perc[is.na(d$perc)] <- 0
  
  return(d)
  
  
})

d_list_homi <- lapply(d_list_homi, function(d){
  as.data.frame(d)
})

d_homi <- do.call("rbind", d_list_homi)



plot_homicidios_estados <- ggplot(d_homi) +
  geom_line(mapping = aes(x = fecha_ocurr_anomes2, y = perc, color = lista_mex_1_c), 
            show.legend = FALSE, alpha = .5) +
  geom_smooth(mapping = aes(x = fecha_ocurr_anomes2, y = perc, color = lista_mex_1_c), show.legend = FALSE, se = FALSE, span = .3) +
  facet_wrap(facets = ~ent_n_ocurr, scales = "free_y") +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m", 
               date_minor_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  
  guides(col=guide_legend(title="Causa de muerte")) +
  xlab("Fecha") + 
  ylab("Porcentaje") +
  ggtitle(label = "Homicidios (como % del total de muertes en el mes para cada estado)", 
          subtitle = "registrado por el registro civil y las agencias del ministerio público (INEGI)\n(lineas gruesas destacan la tendencia)")
















#SAVE####

ggsave(filename = "plot_pormes_perc.jpeg", plot = plot_pormes_perc, device = "jpeg", path = here::here("GRAPHS"), width = 16, height = 8)
ggsave(filename = "plot_pormes.jpeg", plot = plot_pormes, device = "jpeg", path = here::here("GRAPHS"), width = 16, height = 8)
ggsave(filename = "plot_poraño.jpeg", plot = plot_poraño, device = "jpeg", path = here::here("GRAPHS"), width = 16, height = 8)
ggsave(filename = "plot_2017.jpeg", plot = plot_2017, device = "jpeg", path = here::here("GRAPHS"), width = 16, height = 8)
ggsave(filename = "graph_gruposedad.jpeg", plot = plot_gruposedad, device = "jpeg", path = here::here("GRAPHS"), width = 36, height = 18)
ggsave(filename = "graph_grupossexo.jpeg", plot = plot_grupossexo, device = "jpeg", path = here::here("GRAPHS"), width = 16, height = 8)
ggsave(filename = "plot_gruposedos.jpeg", plot = plot_gruposedos, device = "jpeg", path = here::here("GRAPHS"), width = 48, height = 24)
ggsave(filename = "plot_homicidios_estados.jpeg", plot = plot_homicidios_estados, device = "jpeg", path = here::here("GRAPHS"), width = 16, height = 8)
#











  
  
  
  
  
  
