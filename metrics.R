library(DBI)

library(tidyverse)
library(tidyr)


p=347
q=2517
r=100
x <- 1:r

mydb <- dbConnect(RSQLite::SQLite(), "")
dbWriteTable(mydb, "fail_trust_network", fail)
dbWriteTable(mydb, "successful_trust_network", successful)


df <- data.frame(x,fail,successful)

data2 <- df %>%
  pivot_longer(fail:successful, names_to = "model", values_to = "y")

ggplot(data2, aes(x/r, y, color = model, shape = model)) +
  geom_abline(intercept = p, slope = 0, color = "darkred")+
  geom_abline(intercept = q, slope = 0, color = "orange")+
  geom_vline(xintercept  = 0.666, color = "blue")+
  geom_line()+
  labs(x='Trust threshold',y='Transactions',color='According to model')+
  theme_classic()+
  theme(plot.title = element_text(face="bold",size = "19"),
        plot.subtitle = element_text(size = "14"),
        plot.caption = element_text(size="14"),
        axis.title.y = element_text(size="16"),
        axis.title.x = element_text(size="16"),
        axis.text.x = element_text(size="14"),
        axis.text.y = element_text(size="16"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(size="16"),
        legend.text = element_text(size="14"),
        legend.position = "bottom",
        panel.border=element_blank())+
  scale_y_continuous(breaks = seq(0, q, by = 500.00),limits=c(0, q))


p=347
q=2517#3422
r=100
x <- 1:r

dbWriteTable(mydb, "fail_trust_network", y1)
dbWriteTable(mydb, "successful_trust_network", y2)


#precision
precision <- ((p-y1)+(q-y2)*(p/q))/(p+q*(p/q))
#recall
recall <- (p-y1)/p
#f1
f1 <- (recall+precision)/2


df <- data.frame(x,precision,recall,f1)

data2 <- df %>%
  pivot_longer(precision:f1, names_to = "group", values_to = "y")

ggplot(data2, aes(x/r, y, color = group, shape = group)) +
  geom_abline(intercept = 0.821, slope = 0, color = "blue")+
  geom_vline(xintercept  = 0.666, color = "blue")+
  geom_line()+
  labs(x='Trust threshold',y='',color='Metrics')+
  theme_classic()+
  theme(plot.title = element_text(face="bold",size = "19"),
        plot.subtitle = element_text(size = "14"),
        plot.caption = element_text(size="14"),
        axis.title.y = element_text(size="16"),
        axis.title.x = element_text(size="16"),
        axis.text.x = element_text(size="14"),
        axis.text.y = element_text(size="16"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(size="16"),
        legend.text = element_text(size="14"),
        legend.position = "bottom",
        panel.border=element_blank())



dbWriteTable(mydb, "fail_arbitrator", fail)
dbWriteTable(mydb, "successful_arbitrator", successful)


df <- data.frame(x,fail,successful)

data2 <- df %>%
  pivot_longer(fail:successful, names_to = "metric", values_to = "y")

ggplot(data2, aes(x/r, y, color = metric, shape = metric)) +
  geom_abline(intercept = p, slope = 0, color = "darkred")+
  geom_abline(intercept = q, slope = 0, color = "orange")+
  geom_vline(xintercept  = 0.607, color = "blue")+
  geom_line()+
  labs(x='Trust threshold',y='transactions',color='According to model')+
  theme_classic()+
  theme(plot.title = element_text(face="bold",size = "19"),
        plot.subtitle = element_text(size = "14"),
        plot.caption = element_text(size="14"),
        axis.title.y = element_text(size="16"),
        axis.title.x = element_text(size="16"),
        axis.text.x = element_text(size="14"),
        axis.text.y = element_text(size="16"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(size="16"),
        legend.text = element_text(size="14"),
        legend.position = "bottom",
        panel.border=element_blank())+
  scale_y_continuous(breaks = seq(0, q, by = 500.00),limits=c(0, q))



dbWriteTable(mydb, "fail_arbitrator", y1)
dbWriteTable(mydb, "successful_arbitrator", y2)

#precision
precision <- ((p-y1)+(q-y2)*(p/q))/(p+q*(p/q))
#recall
recall <- (p-y1)/p
#f1
f1 <- (recall+precision)/2
df <- data.frame(x,precision,recall,f1)

data2 <- df %>%
  pivot_longer(precision:f1, names_to = "metric", values_to = "y")

ggplot(data2, aes(x/r, y, color = metric, shape = metric)) +
  geom_abline(intercept = 0.965, slope = 0, color = "blue")+
  geom_vline(xintercept  = 0.607, color = "blue")+
  geom_line()+
  labs(x='Trust threshold',y='')+
  theme_classic()+
  theme(plot.title = element_text(face="bold",size = "19"),
        plot.subtitle = element_text(size = "14"),
        plot.caption = element_text(size="14"),
        axis.title.y = element_text(size="16"),
        axis.title.x = element_text(size="16"),
        axis.text.x = element_text(size="14"),
        axis.text.y = element_text(size="16"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(size="16"),
        legend.text = element_text(size="14"),
        legend.position = "bottom",
        panel.border=element_blank())



dbWriteTable(mydb, "fail_deposit", y1)


buyer <- (2*x/(2*x-1))*y1
seller <- (x/(2*x-1))*y1
df <- data.frame(x,buyer,seller)
#((2*x/(2*x-1))*y1
#((2*x)/(2*x-1))
#(x/r)*0.5+0.5
data2 <- df %>%
  pivot_longer(buyer:seller, names_to = "players", values_to = "y")

ggplot(data2, aes(x, y, color = players, shape = players)) +
  geom_abline(intercept = q, slope = 0, color = "orange")+
  geom_vline(xintercept  = 0.5, color = "blue")+
  geom_line()  +
  labs(x='Trust rate',  
       y="Transaction costs") +
  theme_classic()+
  theme(plot.title = element_text(face="bold",size = "19"),
        plot.subtitle = element_text(size = "14"),
        plot.caption = element_text(size="14"),
        axis.title.y = element_text(size="16"),
        axis.title.x = element_text(size="16"),
        axis.text.x = element_text(size="14"),
        axis.text.y = element_text(size="16"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(size="16"),
        legend.text = element_text(size="14"),
        legend.position = "bottom",
        panel.border=element_blank())+
  scale_y_continuous(breaks = seq(0, 1500.00, by = 200.00),limits=c(0, 1500.00)) +
  scale_x_continuous(breaks = seq(0.0, 1.0, by = 0.1),limits=c(0, 1.0)) 





dbWriteTable(mydb, "fail_deposit", fail)
dbWriteTable(mydb, "successful_deposit", successful)

df <- data.frame(x,fail,successful)

data2 <- df %>%
  pivot_longer(fail:successful, names_to = "players", values_to = "y")

ggplot(data2, aes(x, y, color = players, shape = players)) +
  geom_abline(intercept = p, slope = 0, color = "darkred")+
  geom_abline(intercept = q, slope = 0, color = "orange")+
  geom_vline(xintercept  = 0.663, color = "blue")+
  geom_line()  +
  labs(x='Trust threshold',  
       y="Transactions",color='According to model') +
  theme_classic()+
  theme(plot.title = element_text(face="bold",size = "19"),
        plot.subtitle = element_text(size = "14"),
        plot.caption = element_text(size="14"),
        axis.title.y = element_text(size="16"),
        axis.title.x = element_text(size="16"),
        axis.text.x = element_text(size="14"),
        axis.text.y = element_text(size="16"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(size="16"),
        legend.text = element_text(size="14"),
        legend.position = "bottom",
        panel.border=element_blank())+
  scale_y_continuous(breaks = seq(0, q, by = 500.00),limits=c(0, q))



dbWriteTable(mydb, "fail_deposit", y1)
dbWriteTable(mydb, "successful_deposit", y2)

#precision
precision <- ((p-y1)+(q-y2)*(p/q))/(p+q*(p/q))
#recall
recall <- (p-y1)/p
#f1
f1 <- (recall+precision)/2
df <- data.frame(x,precision,recall,f1)

data2 <- df %>%
  pivot_longer(precision:f1, names_to = "metric", values_to = "y")

ggplot(data2, aes(x, y, color = metric, shape = metric)) +
  geom_abline(intercept = 0.819, slope = 0, color = "blue")+
  geom_vline(xintercept  = 0.663, color = "blue")+
  geom_line()+
  labs(x='Trust threshold',y='')+
  theme_classic()+
  theme(plot.title = element_text(face="bold",size = "19"),
        plot.subtitle = element_text(size = "14"),
        plot.caption = element_text(size="14"),
        axis.title.y = element_text(size="16"),
        axis.title.x = element_text(size="16"),
        axis.text.x = element_text(size="14"),
        axis.text.y = element_text(size="16"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(size="16"),
        legend.text = element_text(size="14"),
        legend.position = "bottom",
        panel.border=element_blank())+
  scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits=c(0, 1))
