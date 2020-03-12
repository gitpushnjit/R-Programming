# Assignment 1:
#   group members:
#   1. Sachin Mathew Jose(sj555)
#   2. Chaitanya Shah(cms69)
#   3. Priyanka Bongale(pb435)
#   4. Pushkar Gadgil(pg395)

# Question 1
linear_regression <- function(x,y) {
  n_x = length(x);
  n_y = length(y);
  
  if(n_x != n_y) {
    print("the lengths should be same..");
    return;
  }
  meanx <- sum(x)/n_x;
  meany <- sum(y)/n_x;
  
  SSxy <- sum(x*y) - (n_x * meanx * meany);
  SSxx <- sum(x*x) - (n_x * meanx * meanx);
  
  B1 <- SSxy/SSxx;
  B0 <- meany - B1*meanx;
  
  return(c(B0, B1));
}

y<- c(22.2,33.4,45.7,50.2,55.9,89.1);
x<- c(1,5,7,9,10,22);

B <- linear_regression(x,y);
print (paste('The value of B0 and B1 are: ',B[1],B[2]))

#Question 2:
x_value <- 40;
y_value <- B[2] * x_value + B[1];

print (paste('the vale of y when x =', x_value,'is :',y_value))

#question 3:
squarederror<-function(b0,b1,x,y){
  y_pred <- b1 * x + b0;
  SSE <- sum((y_pred - y)^2);
  return(SSE);
}
print(paste('The Sum of squares of errors is : ',squarederror(B[1],B[2],x,y)))

#question 4:
b0=20.7
b1=seq(0,5,.1)
computederror<-NULL
for(i	in	1:length(b1)){
  computederror[i]<-squarederror(b0,b1[i],x,y)
}
plot(b1,computederror)