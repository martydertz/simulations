simChicks <- function(n){
    chickPecks <- data.frame(Trial = 0,chick_pos=0, num_pecks = 0)
    for(trial in seq(1:n)){
        chicks <-rep(0, 100)
        for(i in seq(1:100)){
            r <- runif(1)
            if(i == 1 & r < .5){
                chicks[100]= chicks[100] + 1
                next
            }
            if(i == 1 & r >= .5){
                chicks[2] = chicks[2]+1
                next 
            }
            if(i==100 & r < .5){
                chicks[1] = chicks[1] +1
                next
            }
            if(i==100 & r >= .5){
                chicks[99] = chicks[99] + 1
                next
            }
            if(r < .5){
                chicks[i-1] = chicks[i-1] +1
                next
            }
            if(r >=.5){ 
                chicks[i+1] = chicks[i+1] +1}
        }
        trialData <- data.frame(Trial = trial, chick_pos = seq(1:100), num_pecks=chicks)
        chickPecks <- rbind(chickPecks, trialData)
    }

    return(chickPecks)
}


df <- simChicks(500)
df<-subset(df, Trial != 0)


library(ggplot2)
ggplot(df, aes(x=num_pecks))+geom_bar()

df2 <- as.data.frame(table(df$Trial, df$num_pecks))
names(df2)<-c("Trial", "num_pecks", "Count")
head(df2)
df2$percent_pecked <- as.numeric(df2$Count)/100


ggplot(subset(df2, num_pecks!="0"), aes(x=percent_pecked, colour=num_pecks, ..count..))+ geom_density()
