library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

########## 2018 ##########
##########################

df = fread('C:/Users/SOYOUNG/Desktop/count18.csv', encoding = 'UTF-8', header = TRUE)

df$count

df$count = gsub(',', '', df$count)
df$count[df$count == '100만명'] = 1000000
df$count[df$count == '10만'] = 100000
df$count[df$count == '각 30'] = 90
df$count[df$count == '별관  5000'] = 5000

sum(df$count == 'nan') # 9
sum(is.na(as.numeric(df$count))) # numeric으로 바꾸면 nan 9개 행
sum(is.na(df$count))

df$count = as.numeric(df$count)
df$V1 = as.numeric(df$V1)

table(df$count)
sum(df$count == 1e5, na.rm = T)

df_table = as.data.frame(table(df$count))
colnames(df_table) = c('pop', 'freq')
df_table$pop = as.numeric(as.character(df_table$pop))

df_table = transform(df_table, category = cut(df_table$pop, 
                                        breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500,
                                                   600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000,
                                                   10000, Inf),
                                        include.lowest = TRUE,
                                        right = FALSE,
                                        labels = c('~10', '10~20', '20~30', '30~40', '40~50', '50~60', '60~70',
                                                   '70~80', '80~90', '90~100', '100~200', '200~300', '300~400',
                                                   '400~500', '500~600', '600~700', '700~800', '800~900', '900~1000',
                                                   '1000~2000', '2000~3000', '3000~4000', '4000~5000', '5000~6000',
                                                   '6000~7000', '7000~8000', '9000~10000', '10000~1000000', '10000~1000000')))

##########################

df_sum = df_table %>% group_by(category) %>% summarize(freq = sum(freq))
write.csv(df_sum, file = 'C:/Users/SOYOUNG/Desktop/집회인원2018.csv', row.names = FALSE)

df_ten = df_sum[1:10, ]
df_hund = df_sum[11:19, ]
df_thous = df_sum[20:28, ]


p1 = ggplot(df_ten, aes(x = category, y = total_pop))
p2 = ggplot(df_hund, aes(x = category, y = total_pop))
p3 = ggplot(df_thous, aes(x = category, y = total_pop))


p1 + geom_bar(stat = 'identity', fill = 'white', colour = 'black') + 
  scale_y_continuous(breaks = seq(0, 650, 100)) +
  geom_text(aes(label = total_pop), vjust = 1.4)
  
p2  + geom_bar(stat = 'identity', fill = 'white', colour = 'black') + 
  scale_y_continuous(breaks = seq(0, 1450, 200)) +
  geom_text(aes(label = total_pop), vjust = 1.4)

p3  + geom_bar(stat = 'identity', fill = 'white', colour = 'black') + 
  scale_y_continuous(breaks = seq(0, 350, 50)) +
  geom_text(aes(label = total_pop), vjust = 1.4)




########## 2019 ##########
##########################
gathering_2019 = fread('C:/Users/SOYOUNG/Desktop/집회2019.csv', header = TRUE, encoding = 'UTF-8')
dim(gathering_2019)

str(gathering_2019) ## data.table 

pop = as.character(gathering_2019$`신고 인원`)

table(pop)

pop = gsub(',', '', pop)
pop = gsub('\n', '', pop)
pop = gsub('\\(.*?\\)','', pop)

which(pop == '') # 4개

pop = as.numeric(pop)

pop_table_2019 = data.frame(table(pop))
pop_table_2019$pop = as.numeric(as.character(pop_table_2019$pop))
pop_table_2019 = transform(pop_table_2019, category = cut(pop, 
                                              breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500,
                                                         600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000,
                                                         10000, Inf),
                                              include.lowest = TRUE,
                                              right = FALSE,
                                              labels = c('~10', '10~20', '20~30', '30~40', '40~50', '50~60', '60~70',
                                                         '70~80', '80~90', '90~100', '100~200', '200~300', '300~400',
                                                         '400~500', '500~600', '600~700', '700~800', '800~900', '900~1000',
                                                         '1000~2000', '2000~3000', '3000~4000', '4000~5000', '5000~6000',
                                                         '6000~7000', '7000~8000', '9000~10000', '10000~1000000', '10000~1000000')))


##########################
pop_2019_sum = pop_table_2019 %>% group_by(category) %>% summarize(freq = sum(Freq))
write.csv(pop_2019_sum, file = 'C:/Users/SOYOUNG/Desktop/집회인원2019.csv', row.names = FALSE)

pop_2019_ten = pop_2019_sum[1:10, ]
pop_2019_hund = pop_2019_sum[11:19, ]
pop_2019_tt = pop_2019_sum[19:28, ]

p1 = ggplot(pop_2019_ten, aes(x = category, y = total_pop))
p2 = ggplot(pop_2019_hund, aes(x = category, y = total_pop))
p3 = ggplot(pop_2019_tt, aes(x = category, y = total_pop))


p1 + geom_bar(stat = 'identity', fill = 'white', colour = 'black') +
  scale_y_continuous(breaks = seq(0, 800, 100)) +
  geom_text(aes(label = total_pop), vjust = 1.4)
p2 + geom_bar(stat = 'identity', fill = 'white', colour = 'black') +
  scale_y_continuous(breaks = seq(0, 1100, 200)) +
  geom_text(aes(label = total_pop), vjust = 1.4)
p3 + geom_bar(stat = 'identity', fill = 'white', colour = 'black') +
  scale_y_continuous(breaks = seq(0, 400, 50)) +
  geom_text(aes(label = total_pop), vjust = 1.4)
  





########## 2020 ##########
##########################
gathering_2020 = fread('C:/Users/SOYOUNG/Desktop/집회2020.csv', header = TRUE, encoding = 'UTF-8')

pop_20 = as.character(gathering_2020$`신고 인원`)

pop_20 = gsub(',', '', pop_20)
pop_20 = as.numeric(pop_20)

sum(is.na(pop_20 == 'NA')) # 10개

pop_table_2020 = data.frame(table(pop_20))
pop_table_2020$pop_20 = as.numeric(as.character(pop_table_2020$pop_20))

pop_table_2020 = transform(pop_table_2020, 
                           category = cut(pop_20, 
                                          breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500,
                                                     600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000,
                                                     10000, Inf),
                                          include.lowest = TRUE,
                                          right = FALSE,
                                          labels = c('~10', '10~20', '20~30', '30~40', '40~50', '50~60', '60~70',
                                                     '70~80', '80~90', '90~100', '100~200', '200~300', '300~400',
                                                     '400~500', '500~600', '600~700', '700~800', '800~900', '900~1000',
                                                     '1000~2000', '2000~3000', '3000~4000', '4000~5000', '5000~6000',
                                                     '6000~7000', '7000~8000', '9000~10000', '10000~1000000', '10000~1000000')))



##########################

pop_2020_sum = pop_table_2020 %>% group_by(category) %>% summarize(freq = sum(Freq))
write.csv(pop_2020_sum, file = 'C:/Users/SOYOUNG/Desktop/집회인원2020.csv', row.names = FALSE)

pop_2020_ten = pop_2020_sum[1:9, ]
pop_2020_hund = pop_2020_sum[10:16, ]
pop_2020_tt = pop_2020_sum[17:21, ]

p1 = ggplot(pop_2020_ten, aes(x = category, y = total_pop))
p2 = ggplot(pop_2020_hund, aes(x = category, y = total_pop))
p3 = ggplot(pop_2020_tt, aes(x = category, y = total_pop))

p1 + geom_bar(stat = 'identity', fill= 'white', colour = 'black') +
  scale_y_continuous(breaks = seq(0, 100, 25)) +
  geom_text(aes(label = total_pop), vjust = 1.4)

p2 + geom_bar(stat = 'identity', fill= 'white', colour = 'black') +
  scale_y_continuous(breaks = seq(0, 150, 25)) +
  geom_text(aes(label = total_pop), vjust = 1.4)

p3 + geom_bar(stat = 'identity', fill= 'white', colour = 'black') +
  scale_y_continuous(breaks = seq(0, 100, 25)) +
  geom_text(aes(label = total_pop), vjust = 1.4)
