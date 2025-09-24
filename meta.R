d <- function(x1,x2,n1,n2,s1,s2){
  d = (x1-x2)/sqrt((((n1-1)*s1*s1)+((n2-1)*s2*s2))/(n1+n2-2))
  c = 1-(3/(4*(n1+n2-2)-1))
  g = d*c
  print(g)
  vd = ((n1+n2)/(n1*n2))+(d*d)/(2*(n1+n2))
  v = c*c*vd
  print(v)
}

d(30,23,60,52,27,20)
