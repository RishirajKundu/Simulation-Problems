A = 5
B = 0
BA =  2
BB = 1

c1 = c("A", "B")
c2 = c("BA", "BB")

n_days = function(path_array){
  days_count = 0
  while(sample(path_array,1) != "B"){
    days_count = days_count + A
  }
  return(days_count)
}

n_days(c1)


days_hist = c()
N = 100000
for(i in 1:N){
  k = 0
  while(sample(c2,1) != "BB"){
    k = k + n_days(c1) + BA
  }
  days_hist = append(days_hist , BB + k + n_days(c1))
}
mean(days_hist)
hist(days_hist, main = "Histogram of days required")
text(90, 40000, paste("E(Days) = ", mean(days_hist)), col = "red")

     