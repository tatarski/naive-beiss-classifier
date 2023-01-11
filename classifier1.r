df = house.votes.84

n = length(df[1,])
m = length(df[,1])

# Ignore rows with ?
df_filtered = subset(df, 
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
                     df$X17..export.administration.act.south.africa..2..y_n. != "?")

n_1 = length(df_filtered[1,])
m_1 = length(df_filtered[,1])


X = df[6,]
# C is one of two classes:  A or B
# Find P(C=A) and P(C=B)
P_C = prop.table(table(df_filtered$X1..Class.Name..2, df_filtered$X1..Class.Name..2))

# P(C|X) = (P(C)*P(X|C))/P(X) = P(C)*(P(x1|C)*P(x2|C)*....*P(Xn|C))/P(X)

P_C_GIVEN_X = P_C

# Mutliply P(x1|C)*P(x2|C)*....*P(Xn|C) into P_C_GIVEN_X
for (j in 2:17){
  # Find values of P(Xj|C)
  # Divide by row sums
  cond_prob = prop.table(
    table(
      df_filtered$X1..Class.Name..2,
      df_filtered[,j]),
    margin=1
  )
  
  index = if(X[j] == "n") 1 else 2
  P_C_GIVEN_X = P_C_GIVEN_X * cond_prob[,index]
}


print(P_C_GIVEN_X)

# Scale so that P_A_GIVEN_X + P_B_GIVEN_X == 1
P_A_GIVEN_X = P_C_GIVEN_X[1,1]/(P_C_GIVEN_X[1,1] + P_C_GIVEN_X[2,2])
P_B_GIVEN_X = P_C_GIVEN_X[2,2]/(P_C_GIVEN_X[1,1] + P_C_GIVEN_X[2,2])

print(P_A_GIVEN_X)
print(P_B_GIVEN_X)

train_model <- function(df) {
  n = length(names(df))
  classifier <- array(dim=c(2, 2, n))
  
  classifier[,,1] <- prop.table(table(df[,1],df[,1]))
  for (i in 2:n) {
    classifier[,,i] <-
           prop.table(
             table(
               df[,1],
               df[,i]),
             margin=1
           )
  }
  return(classifier)
}
classify <- function(X, classifier) {
  P_C = classifier[,,1]
  
  # P(C|X) = (P(C)*P(X|C))/P(X) = P(C)*(P(x1|C)*P(x2|C)*....*P(Xn|C))/P(X)
  
  P_C_GIVEN_X = P_C
  
  # Mutliply P(x1|C)*P(x2|C)*....*P(Xn|C) into P_C_GIVEN_X
  for (j in 2:17){
    # Find values of P(Xj|C)
    # Divide by row sums
    cond_prob = classifier[,,j]
    
    index = if(X[j] == "n") 1 else 2
    P_C_GIVEN_X = P_C_GIVEN_X * cond_prob[,index]
  }
  
  
  # print(P_C_GIVEN_X)
  
  # Scale so that P_A_GIVEN_X + P_B_GIVEN_X == 1
  P_A_GIVEN_X = P_C_GIVEN_X[1,1]/(P_C_GIVEN_X[1,1] + P_C_GIVEN_X[2,2])
  P_B_GIVEN_X = P_C_GIVEN_X[2,2]/(P_C_GIVEN_X[1,1] + P_C_GIVEN_X[2,2])
  
  return(P_A_GIVEN_X)
}
c1 = train_model(df_filtered)
#c1
classify(df[6,], c1)
classify(df[9,], c1)
classify(df[20,], c1)
classify(df[24,], c1)
classify(df[26,], c1)
classify(df[27,], c1)

classify(df[28,], c1)

classify(df[7,], c1)