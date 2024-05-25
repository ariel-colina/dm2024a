# Este script almacena definitivamente sus cinco semillas
# en el bucket, de forma que NO deba cargarlas en cada script

require( "data.table" )

# reemplazar aqui por SUS semillas 
mis_semillas <- c(369701, 369709, 369719, 369727, 369733)

tabla_semillas <- as.data.table(list( semilla = mis_semillas ))

fwrite( tabla_semillas,
    file = "C:/Users/arnic/OneDrive/Documentos/MCD/03-DataMining/datasets/mis_semillas.txt",
    sep = "\t"
)
