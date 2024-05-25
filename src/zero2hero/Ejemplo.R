library("data.table")

ds <- fread("C:/Users/arnic/OneDrive/Documentos/MCD/03-DataMining/datasets/dataset_pequeno.csv")

nrow(ds)
ncol(ds)
ds[, .N] # inidica cantidad de registros

ds[, .N, foto_mes]

colnames(ds)

ds[, .N, list(foto_mes, clase_ternaria)]

nrow(ds[clase_ternaria == "BAJA+2"])

ds[foto_mes == 202107, sum(clase_ternaria == "BAJA+2") / .N]

ds[foto_mes == 202107 & ctrx_quarter < 20, sum(clase_ternaria == "BAJA+2") / .N]

ds[foto_mes == 202107 & ctrx_quarter < 20, sum(clase_ternaria == "BAJA+2") / .N] / ds[foto_mes == 202107, sum(clase_ternaria == "BAJA+2") / .N]

ds[foto_mes == 202107, ganancia := -3000]

ds[foto_mes == 202107 & clase_ternaria == "BAJA+2", ganancia := 117000]

ds[foto_mes == 202107, sum(ganancia)]

ds[foto_mes == 202107 & ctrx_quarter < 4, sum(ganancia)]

for (transacciones in 0:50)
{
  cat(transacciones, ds[foto_mes == 202107 & ctrx_quarter < transacciones, sum(ganancia)], "\n")
}

library("ggplot2") # cargo la libreria ggplot2

campo <- "cliente_antiguedad"
ggplot(ds[foto_mes == 202107], aes_string(x = campo)) +
  geom_density(trim = TRUE, na.rm = TRUE) +
  facet_grid("clase_ternaria~ .")

library("rpart") # cargo la libreria  rpart

modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = ds[foto_mes == 202107]
)

print(modelo)

library("rpart.plot")

rpart.plot::prp(modelo)

prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.3)

# genero el modelo
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = ds[foto_mes == 202107],
  xval = 0,
  cp = 0.0
)

# imprimo el modelo graficamente
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.3)

# genero el modelo
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = ds[foto_mes == 202107],
  xval = 0,
  cp = 0.0,
  maxdepth = 2
)

# imprimo el modelo graficamente
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.3, cex = 1.2)

# genero el modelo
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = ds[foto_mes == 202107],
  xval = 0,
  cp = -1,
  maxdepth = 3
)

# imprimo el modelo graficamente
options(repr.plot.width = 40, repr.plot.height = 20)
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.1, cex = 1.2)

# genero el modelo
dataset2 <- copy(ds)
julio <- copy(ds[foto_mes == 202107])
dataset2[foto_mes == 202109, clase_ternaria := "Z01"]
julio[, clase_ternaria := "Z01"]
dataset2 <- rbind(dataset2, julio)
dataset2[, foto_mes := NULL]
setorder(dataset2, clase_ternaria)

modelo <- rpart(
  formula = "clase_ternaria ~ . -Master_fultimo_cierre -Visa_fultimo_cierre -mcomisiones_mantenimiento",
  data = dataset2,
  xval = 0,
  cp = -1,
  maxdepth = 3
)

# imprimo el modelo graficamente
options(repr.plot.width = 40, repr.plot.height = 20)
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.1, cex = 1.2)

modelo$splits

gc()

rm(list = ls())

# genero el modelo
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = ds[foto_mes == 202107],
  xval = 0,
  cp = -1,
  maxdepth = 2
)

# imprimo el modelo graficamente
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.1, cex = 1.2)

ds[foto_mes == 202107, ctrx_quarter_dos := 2 * ctrx_quarter]
ds[foto_mes == 202107, ctrx_quarter_tres := 3 * ctrx_quarter]
ds[foto_mes == 202107, ctrx_quarter_cuatro := 4 * ctrx_quarter]

# genero el modelo
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = ds[foto_mes == 202107],
  xval = 0,
  cp = -1,
  maxdepth = 2
)

# imprimo el modelo graficamente
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.1, cex = 1.2)

min(ds[foto_mes == 202107, ctrx_quarter])
max(ds[foto_mes == 202107, ctrx_quarter])

boxplot(ds[foto_mes == 202107, ctrx_quarter])

hist(ds[foto_mes == 202107, ctrx_quarter])

plot(density(ds[foto_mes == 202107, ctrx_quarter]))

ds[foto_mes == 202107, ctrx_quarter_normalizado := scale(ctrx_quarter)]

plot(density(ds[foto_mes == 202107, ctrx_quarter_normalizado]))

# genero el modelo
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = ds[foto_mes == 202107],
  xval = 0,
  cp = -1,
  maxdepth = 2
)

# imprimo el modelo graficamente
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.1, cex = 1.2)

ds[foto_mes == 202107, ctrx_quarter_log := log(ctrx_quarter + 1)] # sumo el uno porque no quiero infinitos

boxplot(ds[foto_mes == 202107, ctrx_quarter_log])

plot(density(ds[foto_mes == 202107, ctrx_quarter_log]))










