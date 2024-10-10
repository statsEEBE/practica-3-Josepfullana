#Bernoulli dependen de p
  #tienen dos sucesos, uno con probabilidad p y otro con 1-p
#Ej 1
  #para graficar un ensayo bernoulli

  #sabemos que el 0.65 tienen menos de dos televisores en casa y 0.35 al menos dos
x<-c(0,1)
f<-c(0.68,0.32)

plot(x,f, type="h")# si le ponemos p la hara por puntos
plot(x,f, type="h", ylim=c(0,1), col='red')
points(x,f, pch=16, col='red') #hace los puntos--> imagen de chincheta

#hacemos una simulacion
n<-43 #tamaño de la encuesta
muestra <- sample(x, n, f, replace=TRUE)#es como hacer la encuesta, el replace es la reposicion
pie(table(muestra))
mean(muestra)
table(muestra)/n #frecuencia relativa
barplot(muestra)
bar<-barplot(table(muestra)/n, ylim=c(0,1))
bar
plot(bar, f, typer='h',col='red')
lines(bar, f, type='h', col='red')
points(bar, f, pch=16, col='red') #si ejecutamos varias veces, cambia. uno es la f de prob, otro el experimento

#probabilidad 13 personas que digan si
sum(muestra) #numero de personas que nos dicen si, es un experimento, cada vez cambia
Y<-function(i){sum(sample(x, n, f, replace=TRUE))}
Y(1) #cada vez que llamo a Y nos hace el experimento

set.seed(123)
m <-400000
#sapply(1:m, Y) #repetir la encuesta m veces
hist(sapply(1:m, Y) )
barplot(table(sapply(1:m, Y))) #miramos la grafica por frecuencias
#cuando m tiende a infinito, la frecuencia tiende a la probabilidad
encuestas<-sapply(1:m, Y)
frecrel<-table(encuestas)/m
frecrel["13"] #no siempre es exacto, para que siempre de lo mismo ponemos set.seed(123) reproduce siempre el mismo
#la solucion exacta es si tiende a infinito

#en r podemos hacer y->Binom(n,p) donde 
dbinom(13, 43, 0.32) #personas que dicen que si, en un experimento de tantas personas, con probabilidad p de que digan si
br <- barplot(table(encuestas)/m)
br
lines(br, dbinom(2:29,43,0.32), type='h', col='red') #del 2-29 porque solo tenemos resultados ahi, si ponemos 0-43, no se puede superponer pq no son igual de largos que antes 

#apartado b
dbinom(17,44,0.32)
  #queremos la frecuencia acumulada=funcion de distribucion
#pbinom(y,n,p)
pbinom(16,44,0.32) #16 ya que queremos menor a 17, no menor o igual
  


#si tenim ara 24 mostres, X es la p 17 mostres tenguin com a maxim un televisor
#valor esperat de X
n<-23
x<-c(0,1)
f<-c(0.32,0.68)

xbar <- mean(encuestas) #centros de gravedad de "chinchetas y despues edificios"
n*0.68  #chincheta
#Variànica de X
var(encuestas)
n*0.68*0.32 #chinchetas
#El quantil quart de X
qbinom(0.25,24,0.68)



