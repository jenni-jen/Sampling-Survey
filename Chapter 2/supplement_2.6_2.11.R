library(ggplot2)

# 6/a
publication = 0:10
member = c(28, 4, 3, 4, 4, 2, 1, 0, 2, 1, 1)
df_6a = data.frame(publication, member)
ggplot(data=df_6a, aes(x=publication, y=member)) + geom_bar(stat='identity') + 
  coord_cartesian(xlim=c(0,10), ylim=c(0,30)) + scale_x_continuous(breaks=0:10) + 
  xlab('Number of Refereed Publications') + ylab('Frequency of Faculty Members') +
  ggtitle('Histogram of an SRS of 50 Faculty Members') + 
  theme(panel.grid=element_blank(), 
        plot.title=element_text(face='bold', hjust=0.5)) # plot the histogram

# 6/b
mean_6b = sum(publication * member) / 50
s2_6b = sum(member * (publication - mean_6b)^2) / 49
se_6b = sqrt(1/50-1/807) * sqrt(s2_6b)

# 6/c
G_hat_6c = sum(member * (publication - mean_6b)^3) / (50 * (sqrt(s2_6b))^3)
25 * G_hat_6c^2


# 11/a
age = 9:20
childrenNum = c(13, 35, 44, 69, 36, 24, 7, 3, 2, 5, 1, 1)
df_11a = data.frame(age, childrenNum)
ggplot(data=df_11a, aes(x=age, y=childrenNum)) + geom_bar(stat='identity') + 
  coord_cartesian(xlim=c(8,20), ylim=c(0,70)) + scale_x_continuous(breaks=8:20) + 
  xlab('Number of Children') + ylab('Age at Walking (months)') +
  ggtitle('Histogram of an SRS of 240 Children') + 
  theme(panel.grid=element_blank(), 
        plot.title=element_text(face='bold', hjust=0.5)) # plot the histogram

mean_11a = sum(age * childrenNum) / 240
s2_11a = sum(childrenNum * (age - mean_11a)^2) / 239
G_hat_11a = sum(childrenNum * (age - mean_11a)^3) / (240 * (sqrt(s2_11a))^3)
25 * G_hat_11a^2

# 11/b
se_11b = sqrt(s2_11a / 240)
CI_lb_11b = mean_11a - qnorm(0.975) * se_11b # lower bound of CI
CI_ub_11b = mean_11a + qnorm(0.975) * se_11b # upper bound of CI




