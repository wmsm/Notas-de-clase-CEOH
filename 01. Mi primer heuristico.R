
punto_aleatorio = function(n = 1,x,y){
  nx = round(max(min(rnorm(n,x,sd = 3),87),0))
  ny = round(max(min(rnorm(n,y,sd = 3),61),0))
  puntos = matrix(c(nx,ny),ncol = 2,nrow = n)
  alturas = apply(puntos, 1, function(x) volcano[x[1],x[2]])
  pos = which.max(alturas)
  puntos[pos,]
}

punto_inicial = function(n=1){
  nx = round(runif(n,1,87))
  ny = round(runif(n,1,61))
  puntos = matrix(c(nx,ny),ncol = 2,nrow = n)
  alturas = apply(puntos, 1, function(x) volcano[x[1],x[2]])
  pos = which.max(alturas)
  puntos[pos,]
}

x_n = function(x) {x*0.84-0.085}

filled.contour(volcano, color.palette = terrain.colors, asp = 1)
title(main = "volcano data: filled contour map") #-0.085 #0.755

punto = punto_inicial(3)
print(c(punto,volcano[punto[1],punto[2]]))
pn = c(x_n(punto[1]/87),punto[2]/61)
points(x = pn[1], y=pn[2], col = "blue", pch=19)


for(i in 1:4){
  punton = punto_aleatorio(3,punto[1],punto[2])
  if(volcano[punto[1],punto[2]]<volcano[punton[1],punton[2]]) punto=punton
  
  pn = c(x_n(punto[1]/87),punto[2]/61)
  points(x = pn[1], y=pn[2], col = "blue", pch=19)
  
  print(c(punto,volcano[punto[1],punto[2]]))
}

