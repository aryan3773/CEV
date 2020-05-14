install.packages('gam')

library(ggplot2)
library(readr)

df = read.csv('dataset.csv',header=T)

print(dim(df))

head(df, n=10L)

tail(df, n=10L)

df[df<0] = NA

head(df, n=10L)

tail(df, n=10L)

# colnames(dataset, d.NULL=TRUE, prefix='Col') to name unnamed columns
names = colnames(df)
print(names)

df$visit_freq <- ifelse(is.na(df$visit_freq), mean(df$visit_freq, na.rm=TRUE), df$visit_freq)

df$hrs_spend_per_day <- ifelse(is.na(df$hrs_spend_per_day), mean(df$hrs_spend_per_day, na.rm=TRUE), df$hrs_spend_per_day)

df$movies_per_day <- ifelse(is.na(df$movies_per_day), mean(df$movies_per_day, na.rm=TRUE), df$movies_per_day)

df$episodes_per_day <- ifelse(is.na(df$episodes_per_day), mean(df$episodes_per_day, na.rm=TRUE), df$episodes_per_day)

df$avg_rating <- ifelse(is.na(df$avg_rating), mean(df$avg_rating, na.rm=TRUE), df$avg_rating)

df$internet_speed <- ifelse(is.na(df$internet_speed), mean(df$internet_speed, na.rm=TRUE), df$internet_speed)

df$days_since_last_visit <- ifelse(is.na(df$days_since_last_visit), mean(df$days_since_last_visit, na.rm=TRUE), df$days_since_last_visit)

head(df, n=10L)

tail(df, 10L)

summary(df)

str(df)

# Shuffling the dataset
set.seed(7)
rows=sample(nrow(df))
df1 = df[rows, ]

head(df1, n=10L)

tail(df1, n=10L)

# Splitting into train and test dataset
sample_size = floor(0.75 * nrow(df1))

## set the seed to make your partition reproducible
set.seed(142)
train_ind <- sample(seq_len(nrow(df1)), size = sample_size)

train = df1[train_ind, ]
test  = df1[-train_ind, ]

print(dim(train))
print(dim(test))



library(mgcv)

# Use bam instead of gam for a very large dataset
gam_model = gam(retained ~ s(visit_freq) + s(hrs_spend_per_day) + s(movies_per_day) + s(episodes_per_day) + s(avg_rating) + s(internet_speed) + s(days_since_last_visit), family = 'binomial', data = train)

summary(gam_model)

coef(gam_model)

par(mfrow=c(2,2))
plot(gam_model, se=T)

gam.check(gam_model)

pred.train = predict(gam_model, test)
pred.train = ifelse(pred.train > 0.5, 1, 0)
# actual = test[, c('retained'), drop = FALSE]
# Mean of the true prediction 
mean(pred.train==test$retained)

# Use bam instead of gam for a very large dataset
gam_model_1 = gam(retained ~ s(visit_freq, bs='cr') + s(hrs_spend_per_day, bs='cr') + s(movies_per_day, bs='cr') + s(episodes_per_day, bs='cr') + s(avg_rating, bs='cr') + s(internet_speed, bs='cr') + s(days_since_last_visit, bs='cr'), family = 'binomial', data = train)

summary(gam_model_1)

par(mfrow=c(2,2))
plot(gam_model_1, se=T)

coef(gam_model_1)

gam.check(gam_model_1)

pred.train1 = predict(gam_model_1, test)
pred.train1 = ifelse(pred.train1 > 0.6, 1, 0)
# actual = test[, c('retained'), drop = FALSE]
# Mean of the true prediction 
mean(pred.train1 == test$retained)
