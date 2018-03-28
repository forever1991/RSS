football_gen<-function(dsn){
  ayeartotalsweek<-read.csv(dsn)
  ayeartotalsweek<-ayeartotalsweek[!apply(ayeartotalsweek == "", 1, all),]
  ayeartotalsweek<-ayeartotalsweek[-which(ayeartotalsweek[,1]=="Team"),]
  odd_num<-seq(1,dim(ayeartotalsweek)[1],2)
  even_num<-seq(2,dim(ayeartotalsweek)[1],2)
  ayeartotalsweek[odd_num,1]
  ayear_week<-data.frame("HomeTeam_name"=seq(1,dim(ayeartotalsweek)[1]/2,1),"AwayTeam_name"=0,"Home_points"=0,"Away_points"=0,"Home_win"=1)
  ayear_week[,c(1,2)]<-ayeartotalsweek[c(odd_num,even_num),1]
  ayear_week[,c(3,4)]<-ayeartotalsweek[c(odd_num,even_num),2]
  ayear_week[,5]<-as.numeric(as.numeric(ayear_week[,3])>as.numeric(ayear_week[,4]))
  return(ayear_week)
}

year=2014
data2014=football_gen('2014totalsweek1.csv')
for (week in 2:11)
{
dsn<-paste(year,'totalsweek',week,'.csv',sep="")
data2014=rbind(data2014,football_gen(dsn))
}
difference=as.numeric(data2014$Home_points)-as.numeric(data2014$Away_points)
lmodel=lm(difference~data2014$HomeTeam_name+data2014$AwayTeam_name)
