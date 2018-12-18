library(here)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(openxlsx)

rm(list=ls())

bases_path <- here::here("BASES", "ORIGINAL")
bases_paths <- dir(bases_path, full.names = TRUE)
bases_names <- dir(bases_path)

cats_paths1 <- paste(here::here("CATALOGOS"), 2012:2017, sep = "/")
names(cats_paths1) <- paste("x", 2012:2017, sep = "_")
cats_paths2 <- lapply(cats_paths1, function(x){
  dir(x, full.names = TRUE)
}) ; names(cats_paths2) <- paste("x", 2012:2017, sep = "_")
cats_names <- lapply(cats_paths1, function(x){
  dir(x)
})


encodings <- c(rep("UTF-8", 5), "LATIN1")

cats <- lapply(seq_along(cats_paths2), function(x){
  
  names <- lapply(cats_paths2[[x]], function(y){
    str <- str_split(string = y, pattern = "/")
    unlist(str)[length(unlist(str))]
  })
  
  cats <- lapply(cats_paths2[[x]], function(y){
    read.csv(file = y, stringsAsFactors = FALSE, encoding = encodings[x])
  }) ; names(cats) <- names
  
  return(cats)
  
}) ; names(cats) <- names(cats_paths2)







#catalogo####

#entidades

entmunnom <- lapply(cats, function(x){
  index <- grep(pattern = "decateml", x = names(x))
  decateml <- x[[index]]
  names(decateml) <- c("cve_ent", "cve_mun", "cve_loc", "nom_loc")
  
  catent <- decateml %>% 
    filter(cve_mun == 0) %>% 
    select(cve_ent, 
           nom_ent = nom_loc)
  
  catmun <- decateml %>% 
    filter(cve_loc == 0) %>% 
    select(cve_ent,
           cve_mun,
           nom_mun = nom_loc)
  
  catloc <- decateml  %>% 
    select(cve_ent,
           cve_mun,
           cve_loc,
           nom_loc)
  
  entmunnom <- reduce(.x = list(catent, catmun, catloc), .f = merge)
  entmunnom <- entmunnom[,c("cve_ent", "cve_mun", "cve_loc",
                            "nom_ent", "nom_mun", "nom_loc")]
  
  
  
  return(entmunnom)
})


entmunnom_total <- reduce(.x = entmunnom, .f = full_join)
entmunnom_total <- entmunnom_total %>% 
  select(cve_ent, cve_mun, cve_loc)
entmunnom_total <- unique(entmunnom_total)


entmunnom2017 <- left_join(x = entmunnom_total, y = entmunnom$x_2017)

missing <- entmunnom2017[is.na(entmunnom2017$nom_loc),]
entmunnom2017 <- entmunnom2017[!is.na(entmunnom2017$nom_loc),]
entmunnom2016 <- right_join(x = entmunnom$x_2016, y = missing[,1:3])

missing <- entmunnom2016[is.na(entmunnom2016$nom_loc),]
entmunnom2016 <- entmunnom2016[!is.na(entmunnom2016$nom_loc),]
entmunnom2015 <- right_join(x = entmunnom$x_2015, y = missing[,1:3])

missing <- entmunnom2015[is.na(entmunnom2015$nom_loc),]
entmunnom2015 <- entmunnom2015[!is.na(entmunnom2015$nom_loc),]
entmunnom2014 <- right_join(x = entmunnom$x_2014, y = missing[,1:3])

missing <- entmunnom2014[is.na(entmunnom2014$nom_loc),]
entmunnom2014 <- entmunnom2014[!is.na(entmunnom2014$nom_loc),]
entmunnom2013 <- right_join(x = entmunnom$x_2013, y = missing[,1:3])

missing <- entmunnom2013[is.na(entmunnom2013$nom_loc),]
entmunnom2013 <- entmunnom2013[!is.na(entmunnom2013$nom_loc),]
entmunnom2012 <- right_join(x = entmunnom$x_2012, y = missing[,1:3])

missing <- entmunnom2012[is.na(entmunnom2012$nom_loc),]
entmunnom2012 <- entmunnom2012[!is.na(entmunnom2012$nom_loc),]

entmunnom_total <- list(entmunnom2012, entmunnom2013, entmunnom2014, entmunnom2015, entmunnom2016, entmunnom2017)

decateml <- reduce(.x = entmunnom_total, .f = full_join)


catent <- decateml %>% 
  filter(cve_mun == 0) %>% 
  select(cve_ent, 
         nom_ent = nom_loc)

catmun <- decateml %>% 
  filter(cve_loc == 0) %>% 
  select(cve_ent,
         cve_mun,
         nom_mun = nom_loc)

catloc <- decateml  %>% 
  select(cve_ent,
         cve_mun,
         cve_loc,
         nom_loc)

catent$nom_ent[catent$cve_ent == 99] <- NA
catmun$nom_mun[catmun$cve_mun == 999] <- NA
catloc$nom_loc[catloc$cve_loc == 9999] <- NA


rm(list=c("entmunnom", "entmunnom_total", "entmunnom2012", "entmunnom2013", "entmunnom2014", "entmunnom2015", "entmunnom2016", "entmunnom2017", "decateml", "missing"))


#tamaño de la localidad

detamloc <- lapply(cats, function(x){
  index <- grep(pattern = "detamloc", x = names(x))
  detamloc <- x[[index]]
  names(detamloc) <- c("CVE", "DESCRIP")
  return(detamloc)
}) %>% 
  reduce(.x = ., .f = full_join)

detamloc$DESCRIP[detamloc$CVE == 99] <- NA

tloc_resid_cat <- detamloc %>% 
  select(tloc_resid = CVE, 
         tloc_resid_c = DESCRIP)
tloc_ocurr_cat <- detamloc %>% 
  select(tloc_ocurr = CVE, 
         tloc_ocurr_c = DESCRIP)

rm(list = c("detamloc"))




#causa de defuncion


catlista_mex <- lapply(cats, function(x){
  index <- grep(pattern = "delistamex", x = names(x))
  catlista_mex <- x[[index]]
  names(catlista_mex) <- c("CVE", "DESCRIP")
  return(catlista_mex)
}) %>% 
  reduce(.x = ., .f = full_join)

catlista_mex <- catlista_mex[1:417,]

catlista_mex_1 <- catlista_mex[nchar(catlista_mex$CVE) < 3,]
catlista_mex_1$CVE <- as.numeric(catlista_mex_1$CVE)
names(catlista_mex_1) <- c("CVE_gen", "DESCRIP_gen")

catlista_mex_2 <- catlista_mex
catlista_mex_2$CVE_gen <- as.numeric(str_split_fixed(string = catlista_mex$CVE, pattern = "[ABCDEFGHIJKLMNOPQRSTUVWXYZ]", n = 2)[,1])

catlista_mex <- merge(x = catlista_mex_2, y = catlista_mex_1, all.x = TRUE)
names(catlista_mex) <- c("lista_mex_1", "lista_mex_2", "lista_mex_2_c", "lista_mex_1_c")
catlista_mex$lista_mex_1 <- as.numeric(catlista_mex$lista_mex_1)

catlista_mex_1 <- unique(catlista_mex[,c("lista_mex_1", "lista_mex_1_c")])
catlista_mex_2 <- catlista_mex[,c("lista_mex_2", "lista_mex_2_c")]

rm(list = c("catlista_mex"))

#sexo y edad

catdesexo <- lapply(cats, function(x){
  index <- grep(pattern = "desexo", x = names(x))
  desexo <- x[[index]]
  names(desexo) <- c("sexo", "sexo_c")
  return(desexo)
}) %>% 
  reduce(.x = ., .f = full_join)
catdesexo$sexo_c[catdesexo$sexo == 9] <- NA


deedad <- cats$x_2017$deedad.csv
deedad$edad_2 <- as.numeric(str_split_fixed(string = deedad$CVE, pattern = ".", n = 2)[,2])
deedad$edad_2[deedad$CVE == 4998] <- NA
deedad$edad_2[deedad$CVE %in% 0:3098] <- 0
names(deedad) <- c("edad", "edad_c", "edad_2")




#ocupacion, escolaridad, edocivil, deurbrur

deocupa <- lapply(cats, function(x){
  index <- grep(pattern = "deocupa", x = names(x))
  deocupa <- x[[index]]
  names(deocupa) <- c("ocupacion", "ocupacion_c")
  return(deocupa)
})  %>% 
  reduce(.x = ., .f = full_join)
deocupa$ocupacion[deocupa$ocupacion == 98] <- 99
deocupa$ocupacion_c[deocupa$ocupacion == 99] <- NA
deocupa <- unique(deocupa)


deescol <- lapply(cats, function(x){
  index <- grep(pattern = "deesco", x = names(x))
  deescol <- x[[index]]
  names(deescol) <- c("escolarida", "escolarida_c")
  deescol$escolarida_c[deescol$escolarida == 99] <- NA
  return(deescol)
})  %>% 
  reduce(.x = ., .f = full_join)


deurbrur <- lapply(cats, function(x){
  index <- grep(pattern = "deurbrur", x = names(x))
  deurbrur <- x[[index]]
  names(deurbrur) <- c("area_ur", "area_ur_c")
  deurbrur$area_ur_c[deurbrur$area_ur == 9] <- NA
  return(deurbrur)
})   %>% 
  reduce(.x = ., .f = full_join)




# depresunto, deocutrab, desitiolesion, desitiodefun, deviofami

depresunto <- lapply(cats, function(x){
  index <- grep(pattern = "depresunto", x = names(x))
  depresunto <- x[[index]]
  names(depresunto) <- c("presunto", "presunto_c")
  return(depresunto)
})   %>% 
  reduce(.x = ., .f = full_join)


deocutrab <- lapply(cats, function(x){
  index <- grep(pattern = "deocutrab", x = names(x))
  deocutrab <- x[[index]]
  names(deocutrab) <- c("ocurr_trab", "ocurr_trab_c")
  deocutrab$ocurr_trab_c[deocutrab$ocurr_trab == 9] <- NA
  return(deocutrab)
})   %>% 
  reduce(.x = ., .f = full_join)


desitiolesion <- lapply(cats, function(x){
  index <- grep(pattern = "desitiolesion", x = names(x))
  desitiolesion   <- x[[index]]
  names(desitiolesion  ) <- c("lugar_ocur", "lugar_ocur_c")
  desitiolesion$lugar_ocur_c[desitiolesion$lugar_ocur == 9] <- NA
  return(desitiolesion  )
})    %>% 
  reduce(.x = ., .f = full_join)



desitiondefun <- lapply(cats, function(x){
  index <- grep(pattern = "desitiodefun", x = names(x))
  desitiodefun <- x[[index]]
  names(desitiodefun  ) <- c("sitio_ocur", "sitio_ocur_c")
  desitiodefun$sitio_ocur_c[desitiodefun$sitio_ocur == 99] <- NA
  return(desitiodefun  )
})  %>% 
  reduce(.x = ., .f = full_join)



deviofami <- lapply(cats, function(x){
  index <- grep(pattern = "deviofam", x = names(x))
  deviofam <- x[[index]]
  names(deviofam) <- c("vio_fami", "vio_fami_c")
  deviofam$vio_fami_c[deviofam$vio_fami == 9] <- NA
  return(deviofam)
})  %>% 
  reduce(.x = ., .f = full_join)





catsfin <- list(catdesexo = catdesexo, 
                catlista_mex_1 = catlista_mex_1,
                catlista_mex_2 = catlista_mex_2, 
                deedad = deedad, 
                deescol = deescol, 
                deocupa = deocupa, 
                deocutrab = deocutrab, 
                depresunto = depresunto, 
                desitiolesion = desitiolesion, 
                desitiondefun = desitiondefun, 
                deurbrur = deurbrur, 
                deviofami = deviofami, 
                catent = catent, 
                catmun = catmun, 
                catloc = catloc,
                tloc_ocurr_cat = tloc_ocurr_cat, 
                tloc_resid_cat = tloc_resid_cat)

rm(list = c("catdesexo", "catlista_mex", "deedad", "deescol", "deocupa", "deocutrab", "depresunto", "desitiolesion", "desitiondefun", "deurbrur", "deviofami", "tloc_ocurr_cat", "tloc_resid_cat", "catent", "catloc", "catmun", "catlista_mex_1", "catlista_mex_2"))

data.frame(sapply(catsfin, nrow))

catsfin$catloc %>% 
  filter(cve_ent == 1 & cve_mun == 1 & cve_loc == 120)







#bases####

d <- lapply(bases_paths, function(x){
  read.csv(file = x, stringsAsFactors = FALSE)
}) ; names(d) <- bases_names


tokeep <- c("ent_regis", "mun_regis", 
            "dia_regis", "mes_regis", "anio_regis",
            "ent_resid", "mun_resid", "loc_resid", "tloc_resid", 
            "ent_ocurr", "mun_ocurr", "loc_ocurr", "tloc_ocurr", 
            "dia_ocurr", "mes_ocurr", "anio_ocur", "horas", "minutos", 
            "ent_ocules", "mun_ocules", "loc_ocules",
            "sexo", "edad", "ocupacion", "escolarida", "area_ur",
            "presunto", "ocurr_trab", "lugar_ocur", "sitio_ocur", "vio_fami", "lista_mex")

d <- lapply(d, function(x){
  names(x)[1] <- "ent_regis"
  x <- x[,tokeep]
  return(x)
})


d <- lapply(d, function(d){
  d$lista_mex_1 <- str_split_fixed(string = d$lista_mex, pattern = "[ABCDEFGHIJKLMNOPQRSTUVWXYZ]", n = 2)[,1]
  d$lista_mex_1 <- as.numeric(d$lista_mex_1)
  names(d)[names(d) == "lista_mex"] <- "lista_mex_2"
  return(d)
})


d <- lapply(d, function(d){
  d[,grep(pattern = "anio|mes|dia|horas|minutos", x = names(d), value = TRUE)] <- 
    lapply(d[,grep(pattern = "anio|mes|dia|horas|minutos", x = names(d), value = TRUE)], 
           function(x){
             x[x %in% c(99,9999)] <- NA
             return(x)
           })
  
  d[,grep(pattern = "mes|hora|dia|minutos", x = names(d), value = TRUE)] <- 
    lapply(d[,grep(pattern = "mes|hora|dia|minutos", x = names(d), value = TRUE)], 
           function(x){
             as.character(ifelse(test = nchar(x) == 1, 
                                 yes = paste0("0", x), 
                                 no = x))
           })
  
  return(d)
})


d <- lapply(d, function(d){
  
  names(d)[names(d) == "horas"] <- "hora_ocurr"
  names(d)[names(d) == "minutos"] <- "min_ocurr"
  
  names(d)[names(d) == "anio_ocur"] <- "anio_ocurr"
  
  
  d$tiempo_ocurr_hora <- d$hora_ocurr
  
  d$tiempo_ocurr_horamin <- paste(d$hora_ocurr, d$min_ocurr, 
                                  sep = ":")
  d$tiempo_ocurr_horamin[is.na(d$hora_ocurr)] <- NA
  d$tiempo_ocurr_horamin[is.na(d$min_ocurr)] <- NA
  
  
  d$fecha_ocurr_ano <- d$anio_ocurr
  
  d$fecha_ocurr_anomes <- paste(d$anio_ocurr, d$mes_ocurr, 
                                sep = "-")
  d$fecha_ocurr_anomes[is.na(d$anio_ocurr)] <- NA
  d$fecha_ocurr_anomes[is.na(d$mes_ocurr)] <- NA
  
  d$fecha_ocurr_anomesdia <- paste(d$fecha_ocurr_anomes, d$dia_ocurr, 
                                   sep = "-")
  d$fecha_ocurr_anomesdia[is.na(d$fecha_ocurr_anomes)] <- NA
  d$fecha_ocurr_anomesdia[is.na(d$dia_ocurr)] <- NA
  
  
  return(d)
})



tokeep <- c("ent_resid", "mun_resid", "loc_resid", "tloc_resid", 
            "ent_ocurr", "mun_ocurr", "loc_ocurr", "tloc_ocurr", 
            "fecha_ocurr_ano", "fecha_ocurr_anomes", "fecha_ocurr_anomesdia", 
            "tiempo_ocurr_hora", "tiempo_ocurr_horamin", 
            "ent_ocules", "mun_ocules", "loc_ocules",
            "sexo", "edad", "ocupacion", "escolarida", "area_ur",
            "presunto", "ocurr_trab", "lugar_ocur", "sitio_ocur", "vio_fami", 
            "lista_mex_1", "lista_mex_2")

d <- lapply(d, function(d){
  d <- d[,tokeep]
  return(d)
})




#merges####

#entidades

d <- lapply(d, function(d){
  
  #resid 
  
  d <- merge(x = d, y = catsfin$catent, 
              by.x = c("ent_resid"), by.y = c("cve_ent"),
              all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$catmun, 
              by.x = c("ent_resid", "mun_resid"), by.y = c("cve_ent", "cve_mun"),
              all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$catloc, 
              by.x = c("ent_resid", "mun_resid", "loc_resid"), 
              by.y = c("cve_ent", "cve_mun", "cve_loc"),
              all.x = TRUE, sort = FALSE)
  
  ini <- length(names(d))-2
  fin <- ini + 2
  names(d)[ini:fin] <- c("ent_n_resid", "mun_n_resid", "loc_n_resid")
  
  
  #ocurr
  
  d <- merge(x = d, y = catsfin$catent, 
             by.x = c("ent_ocurr"), by.y = c("cve_ent"),
             all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$catmun, 
             by.x = c("ent_ocurr", "mun_ocurr"), by.y = c("cve_ent", "cve_mun"),
             all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$catloc, 
             by.x = c("ent_ocurr", "mun_ocurr", "loc_ocurr"), 
             by.y = c("cve_ent", "cve_mun", "cve_loc"),
             all.x = TRUE, sort = FALSE)
  
  ini <- length(names(d))-2
  fin <- ini + 2
  names(d)[ini:fin] <- c("ent_n_ocurr", "mun_n_ocurr", "loc_n_ocurr")
  
  
  #ocules
  
  d <- merge(x = d, y = catsfin$catent, 
             by.x = c("ent_ocules"), by.y = c("cve_ent"),
             all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$catmun, 
             by.x = c("ent_ocules", "mun_ocules"), by.y = c("cve_ent", "cve_mun"),
             all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$catloc, 
             by.x = c("ent_ocules", "mun_ocules", "loc_ocules"), 
             by.y = c("cve_ent", "cve_mun", "cve_loc"),
             all.x = TRUE, sort = FALSE)
  
  ini <- length(names(d))-2
  fin <- ini + 2
  names(d)[ini:fin] <- c("ent_n_ocules", "mun_n_ocules", "loc_n_ocules")
  
  return(d)
})


#tamaño loc

d <- lapply(d, function(d){
  d <- merge(x = d, y = catsfin$tloc_resid_cat, all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$tloc_ocurr_cat, all.x = TRUE, sort = FALSE)
  
  return(d)
})


#lista mx

d <- lapply(d, function(d){
  d <- merge(x = d, y = catsfin$catlista_mex_1, 
             all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$catlista_mex_2, 
             all.x = TRUE, sort = FALSE)
  
  return(d)
})


#sexo edad

d <- lapply(d, function(d){
  d <- merge(x = d, y = catsfin$catdesexo, 
             all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$deedad, 
             all.x = TRUE, sort = FALSE)
  return(d)
})

#escol ocupa  area urbana

d <- lapply(d, function(d){
  d <- merge(x = d, y = catsfin$deescol, 
             all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$deocupa, 
             all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$deurbrur, 
             all.x = TRUE, sort = FALSE)
  return(d)
})


#depresunto, deocutrab, desitiolesion, desitiodefun, deviofami

d <- lapply(d, function(d){
  d <- merge(x = d, y = catsfin$depresunto, 
             all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$deocutrab, 
             all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$desitiolesion, 
             all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$desitiondefun, 
             all.x = TRUE, sort = FALSE)
  d <- merge(x = d, y = catsfin$deviofami, 
             all.x = TRUE, sort = FALSE)
  return(d)
})


#save####

tokeep <- c("ent_resid", "mun_resid", "loc_resid", "tloc_resid", 
            "ent_ocurr", "mun_ocurr", "loc_ocurr", "tloc_ocurr", 
            "fecha_ocurr_ano", "fecha_ocurr_anomes", "fecha_ocurr_anomesdia", 
            "tiempo_ocurr_hora", "tiempo_ocurr_horamin", 
            "ent_ocules", "mun_ocules", "loc_ocules",
            "sexo", "edad", "ocupacion", "escolarida", "area_ur",
            "presunto", "ocurr_trab", "lugar_ocur", "sitio_ocur", "vio_fami", 
            "lista_mex_1", "lista_mex_2", 
            
            "ent_n_resid", "mun_n_resid", "loc_n_resid", "tloc_resid_c", 
            "ent_n_ocurr", "mun_n_ocurr", "loc_n_ocurr", "tloc_ocurr_c",
            "ent_n_ocules", "mun_n_ocules", "loc_n_ocules", 
            "sexo_c", "edad_c", "edad_2", "ocupacion_c", "escolarida_c", "area_ur_c", 
            "presunto_c", "ocurr_trab_c", "lugar_ocur_c", "sitio_ocur_c", "vio_fami_c",
            "lista_mex_1_c", "lista_mex_2_c"
            )


d <- lapply(d, function(d){
  d <- d[,tokeep]
  return(d)
})

d$defunciones_generales_2012.csv$base <- bases_names[1]
d$defunciones_generales_2013.csv$base <- bases_names[2]
d$defunciones_generales_2014.csv$base <- bases_names[3]
d$defunciones_generales_2015.csv$base <- bases_names[4]
d$defunciones_generales_2016.csv$base <- bases_names[5]
d$defunciones_generales_2017.CSV$base <- bases_names[6]

sum(sapply(d, nrow))

d <- do.call("rbind", d)

path_write <- here::here("BASES")
#write.csv(x = d, file = paste(path_write, "defunciones.txt", sep = "/"), fileEncoding = "UTF-8")

#read.csv(file = paste(path_write, "defunciones.txt", sep = "/"), header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)