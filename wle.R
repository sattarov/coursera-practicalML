setwd("C:\\Users\\Timur\\Documents\\practicalML")
train = read.csv("pml-training.csv", header = T);
test = read.csv("pml-testing.csv", header = T);
col_names = names(train)
