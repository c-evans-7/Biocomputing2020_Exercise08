# Exercise 8
# Connor Evans

# Basketball score graph
scores = read.table("UWvMSU_1-22-13.txt", header = TRUE)

UW_prealCounter = 1
MSU_prealCounter = 1

for (ll in 1:50)
{
  if (scores$team[ll] == "UW")
  {
    UW_prealCounter <-  UW_prealCounter + 1
  }
  else
  {
    MSU_prealCounter <-  MSU_prealCounter + 1
  }
}

UW_matrix <-  matrix(0, UW_prealCounter, 2)
MSU_matrix <-  matrix(0, MSU_prealCounter, 2)

MSU_iter = 2
UW_iter = 2

for (kk in 1:50)
{
  if (scores$team[kk] == "MSU")
  {
    MSU_matrix[MSU_iter, 1] <-  scores$time[kk]
    MSU_matrix[MSU_iter, 2] <-  MSU_matrix[(MSU_iter-1),2] + scores$score[kk]
    MSU_iter = MSU_iter + 1
  }
  else if (scores$team[kk] == "UW")
  {
    UW_matrix[UW_iter, 1] <-  scores$time[kk]
    UW_matrix[UW_iter, 2] <-  UW_matrix[(UW_iter-1), 2] + scores$score[kk]
    UW_iter = UW_iter + 1
  }
}

plot(UW_matrix[,1], UW_matrix[,2], type = 'l', col = "red", xlab = "Time (minutes)", ylab = "Score")
lines(MSU_matrix[,1], MSU_matrix[,2], col = "blue")
legend("topleft", legend = c("UW", "MSU"), col = c("red", "blue"), lty=1:2, cex = 0.8)




# Guessing game
sampleVec = numeric(100)
for (ii in 1:100)
  {
  sampleVec[ii] = ii
}

randomNum = sample(sampleVec, size = 1)

print("I'm thinking of a number between 0 and 100...")
print("Guess a number...")
for (jj in 1:15)
{
  ans = as.numeric(readline())
  if (ans == randomNum)
  {
    print("Correct!")
    break
  }
  else if (ans < randomNum)
  {
    print("Higher")
  }
  else if (ans > randomNum)
  {
    print("Lower")
  }
  if (ii == 15)
  {
    print("You lose!")
  }
}

