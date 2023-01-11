prop_table_laplace_smoothing <- function(t) {
  alpha = 1
  sum_rows = c(sum(t[1,]), sum(t[2,]))
  return (
    (t + alpha)/(sum_rows + 16*alpha)
  )
}
train_model <- function(df) {
  print(nrow(df))
  n = length(names(df))
  print(n)
  classifier <- array(dim=c(2, 2, n))
  classifier[,,1] <- prop.table(table(df[,1],df[,1]))
  print(classifier)
  for (i in 2:n) {
    classifier[,,i] <-
      prop_table_laplace_smoothing(
        table(
          df[,1],
          df[,i])
      )
  }
  return(classifier)
}
classify <- function(X, classifier) {
  n = length(X)
  P_C = classifier[,,1]
  
  # P(C|X) = (P(C)*P(X|C))/P(X) = P(C)*(P(x1|C)*P(x2|C)*....*P(Xn|C))/P(X)
  P_C_GIVEN_X = P_C
  
  # Mutliply P(x1|C)*P(x2|C)*....*P(Xn|C) into P_C_GIVEN_X
  for (j in 2:n){
    # Find values of P(Xj|C)
    # Divide by row sums
    cond_prob = classifier[,,j]
    
    index = if(X[j] == "n") 1 else 2
    if(X[j] == "?") {
    } else {
      P_C_GIVEN_X = P_C_GIVEN_X * cond_prob[,index]
    }
    
  }
  
  # Scale so that P_A_GIVEN_X + P_B_GIVEN_X == 1
  P_A_GIVEN_X = P_C_GIVEN_X[1,1]/(P_C_GIVEN_X[1,1] + P_C_GIVEN_X[2,2])
  P_B_GIVEN_X = P_C_GIVEN_X[2,2]/(P_C_GIVEN_X[1,1] + P_C_GIVEN_X[2,2])
  return(P_A_GIVEN_X)
}
filter_voting_data = function(df) {
  return(subset(df, 
                                 df$X2..handicapped.infants..2..y_n.!="?" &
                                   df$X3..water.project.cost.sharing..2..y_n. != "?" &
                                   df$X4..adoption.of.the.budget.resolution..2..y_n. != "?" &
                                   df$X5..physician.fee.freeze..2..y_n. != "?" &
                                   df$X6..el.salvador.aid..2..y_n. != "?" &
                                   df$X7..religious.groups.in.schools..2..y_n. != "?" &
                                   df$X8..anti.satellite.test.ban..2..y_n. != "?" &
                                   df$X9..aid.to.nicaraguan.contras..2..y_n. != "?" &
                                   df$X10..mx.missile..2..y_n. != "?" &
                                   df$X11..immigration..2..y_n. != "?" &
                                   df$X12..synfuels.corporation.cutback..2..y_n. != "?" &
                                   df$X13..education.spending..2..y_n. != "?" &
                                   df$X14..superfund.right.to.sue..2..y_n. != "?" &
                                   df$X15..crime..2..y_n. != "?" &
                                   df$X16..duty.free.exports..2..y_n. != "?" &
                                   df$X17..export.administration.act.south.africa..2..y_n. != "?"))
}

df = house.votes.84

n = length(df[1,])
m = length(df[,1])

df_shuffled = df[sample(1:nrow(df), replace = FALSE), ]
k = 10

training_data_n = floor(nrow(df_shuffled)/k)
training_data = df_shuffled[1:training_data_n,]
testing_data = df_shuffled[(training_data_n+1):nrow(df_shuffled),]
View(testing_data)
training_data_filtered = filter_voting_data(training_data)

classifier1 = train_model(training_data_filtered)
results = matrix(nrow=nrow(testing_data), ncol=3)
colnames(results) = c("party", "p_to_be_democrat", "classified_as")

for(j in 1:nrow(testing_data)) {
  p_democrat = classify(testing_data[j,], classifier1)
  if(is.nan(p_democrat)) {
    results[j,] = c(testing_data[j,1], NaN, NaN)
  } else {
    results[j,] = c(testing_data[j,1],  
                  p_democrat,
                  if (p_democrat > 0.5) "democrat" else "republican")
  }
}

accuracy = nrow(results[results[,1] == results[,3],])/nrow(testing_data)
accuracy
View(results)
