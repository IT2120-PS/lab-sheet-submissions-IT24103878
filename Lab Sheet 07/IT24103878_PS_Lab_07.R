#Question 1

# 1)
#P(8.10<x<8.25)= P(x<=8.15)-P(x<=8.10)
punif(8.15,min = 0,max = 40,lower.tail = TRUE)-punif(8.10,min = 0,max = 40,lower.tail = TRUE)

# 2)
#p(x<=2)
pexp(2,rate = 1/3,lower.tail = TRUE)

# 3)
#mean-100 standard deviation-15
  #1.
  #P(x>130)
  pnorm(130,mean = 100,sd=15,lower.tail = FALSE)
  
  #2.
  qnorm(0.95, mean = 100, sd = 15)