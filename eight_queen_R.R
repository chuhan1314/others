xiexian <- function(x,m){
  n=dim(x)
tmp1=sum(eval(parse(text=paste0("c(",paste0(paste0("x[",1:m,",",m:1,"]",collapse = ",")),")"))))>1
tmp2=sum(eval(parse(text=paste0("c(",paste0(paste0("x[",(n[1]):(n[1]-m+1),",",(n[2]-m+1):(n[2]),"]",collapse = ",")),")"))))>1
  x=x[,n[1]:1]
tmp3=sum(eval(parse(text=paste0("c(",paste0(paste0("x[",1:m,",",m:1,"]",collapse = ",")),")"))))>1
tmp4=sum(eval(parse(text=paste0("c(",paste0(paste0("x[",(n[1]):(n[1]-m+1),",",(n[2]-m+1):(n[2]),"]",collapse = ",")),")"))))>1
return(tmp1+tmp2+tmp3+tmp4)
}

confact <- function(x){
  n=max(dim(x))
  col = 1 %in% (colSums(x) > 1)
  row=  1 %in% (rowSums(x) > 1)
  xie=0
   for (i in 1:max(n)) {
     while (xiexian(x,i) != 0) {
      xie=1
      break
    } 
  }
return(col+row+xie)
}

pan=function(size=8){
  x=matrix(0,nrow = size,ncol = size)
  return(x)
}

#a=1:8
#do.call(paste0,expand.grid(a,a),args = ",")

eight_queen=function(i=8){
#i=8
n=1:i
m=rep(0,i)
vars=paste0("v",n)
ms=paste0("m",n)
a=1
res=list()
p1=paste0('for (',vars," in n)",collapse = " ")
p2=paste0(ms,"=m;",ms,"[",vars,"]=1",collapse = ";")
p3=paste0(";x=cbind(",paste0(ms,collapse = ","),");if(confact(x) != 0){next}else{res[[a]]=x;a=a+1}}")
p=paste0(p1,"{",p2,p3)
eval(parse(text=p))
return(res)
}
x=eight_queen(8)

