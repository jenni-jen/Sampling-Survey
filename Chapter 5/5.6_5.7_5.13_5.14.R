# Ex. 5.6
worm = read.csv('worm.csv')
colnames(worm)[1] = 'case1'
worm = t(worm); row.names(worm) = 1:12
worm_stats = data.frame(as.matrix(worm))
N=580; M=24; n=12; m=3

sum(worm_stats)
worm_stats$t_i = rowMeans(worm_stats) * M
t_unb = N * mean(worm_stats$t_i)
s2_t = var(worm_stats$t_i)
worm_stats$s2_i = apply(worm_stats[,1:3],1,var)
Vhat_t = N^2 * (1-n/N) * s2_t/n + N/n * sum((1-m/M) *M^2 * worm_stats$s2_i/m)
SE_t = sqrt(Vhat_t)
t_CI_lb = t_unb - qnorm(0.975)*SE_t; t_CI_ub = t_unb + qnorm(0.975)*SE_t

Vhat_WR_t = N^2 * s2_t/n
SE_WR_t = sqrt(Vhat_WR_t)
t_CI2_lb = t_unb - qnorm(0.975)*SE_WR_t; t_CI2_ub = t_unb + qnorm(0.975)*SE_WR_t


# Ex. 5.7
library(dplyr); library(ggplot2)
sold = read.csv('sold.csv'); sold$city = as.factor(sold$city)
sm_stats = sold %>% group_by(city) %>%
  summarise(m_i=n(), ybar_i=mean(sold), s2_i=var(sold))
sm_stats_supple = data.frame(M_i=c(52,19,37,39,8,14))
sm_stats_supple$that_i = sm_stats$ybar_i * sm_stats_supple$M_i
sm_stats = cbind(sm_stats$city, sm_stats_supple$M_i, sm_stats[,2:3], 
                 sm_stats_supple$that_i, sm_stats$s2_i)
colnames(sm_stats) = c('City','M_i','m_i','ybar_i','t_hat_i','s2_i')

ggplot(data=sold, aes(x=city,y=sold)) + geom_boxplot() +
  xlab("City") + ylab('Number of Cases Sold')

N=45; n=6
t_unb = N/n * sum(sm_stats$t_hat_i)
s2_t = var(sm_stats$t_hat_i)
attach(sm_stats)
Vhat_t = N^2 * (1-n/N) * s2_t/n + N/n * sum((1-m_i/M_i) * M_i^2 * s2_i/m_i)
SE_t = sqrt(Vhat_t)
yr = sum(t_hat_i) / sum(M_i)
Mbar = mean(M_i)
Vhat_yr = (1-n/N)/(n*Mbar^2) * 1/(n-1) * sum((M_i*ybar_i-M_i*yr)^2) + 
  1/(n*N*Mbar^2) * sum((M_i^2 * (1-m_i/M_i)) / m_i * s2_i)
SE_yr = sqrt(Vhat_yr)


# Ex. 5.13
N = 1572; M = 1:10
n0 = 1.96^2/0.03^2 * (0.1 - 0.01/M^2)
n = n0 / (1 + n0/N)
family_units = data.frame(M=M, n0=round(n0), n=round(n))
family_units = t(family_units)


# Ex. 5.14
smoker = read.csv('smoker.csv')
colnames(smoker)[3:5] = c('M_i','m_i','y_i')
attach(smoker)
pr = sum(y_i/m_i * M_i) / sum(M_i)

N=29; n=4; Mbar=mean(M_i)
smoker$p_i = y_i / m_i; attach(smoker)
smoker$s2_i = m_i/(m_i-1) * p_i * (1-p_i); attach(smoker)
Vhat_p = (1-n/N)/(n*Mbar^2) * 1/(n-1) * sum((M_i*p_i-M_i*pr)^2) + 
  1/(n*N*Mbar^2) * sum(M_i^2 * (1-m_i/M_i) / m_i * s2_i)
SE_p = sqrt(Vhat_p)
p_CI_lb = pr - qnorm(0.975)*SE_p; p_CI_ub = pr + qnorm(0.975)*SE_p

smoker$that_i = y_i/m_i * M_i; attach(smoker)
t_unb = N/n * sum(that_i)
s2_t = var(that_i)
Vhat_t = N^2 * (1-n/N)/n * s2_t + N/n * sum(M_i^2 * (1-m_i/M_i) / m_i * s2_i)
SE_t = sqrt(Vhat_t)
t_CI_lb = t_unb - qnorm(0.975)*SE_t; t_CI_ub = t_unb + qnorm(0.975)*SE_t

ybar = sum(y_i) / sum(m_i)
smoker$squaresum = y_i * (1-ybar)^2 + (m_i-y_i) * ybar^2
msto = sum(smoker$squaresum) / (sum(m_i)-1)
smoker$sw = y_i * (1-p_i)^2 + (m_i-y_i) * p_i^2
msw = sum(smoker$sw) / sum(m_i-1)
R2_a = 1 - msw/msto
msb = 1/(n-1) * sum(m_i * (p_i - ybar)^2)
c1=50; c2=2/3; C=300
m_opt = sqrt(c1*(1-R2_a) / (c2*R2_a))
n_opt = C / (c1+c2*m_opt)


