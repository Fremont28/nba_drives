#4/18/18 
#can drive rates predict a guard's success in the NBA? 
p_drives=read.csv("cluster_drives.csv")
p_totals=read.csv("totals_nba.csv")
colnames(p_drives)[1]="Player"
#merge dataframes 
p_nba=merge(p_drives,p_totals,by="Player")
mean(p_nba$MIN) #18.82 (avg minutes)

#percent drive metrics
p_nba$drives_min=p_nba$DRIVES/p_nba$MIN #percent of drives per minute 
p_nba$fgm_d=p_nba$FGM/(p_nba$FGM+p_nba$FG) #percent of field goals made stemming from drives 
p_nba$fga_d=p_nba$FGA.x/(p_nba$FGA.x+p_nba$FGA.y)
p_nba$fta_d=p_nba$FTA.x/(p_nba$FG..y+p_nba$FTA.x)
p_nba$pts_d=p_nba$PTS/(p_nba$PTS+p_nba$PS.G)
p_nba$ast_d=p_nba$AST.x/(p_nba$AST.x+p_nba$AST.y)

#read-in value above replacement metrics (basketball reference.com)
p_vorp=read.csv("vorp_metrics.csv")
p_vorp$VORP=as.numeric(as.character(p_vorp$VORP))
#subset 
p_vorp1=subset(p_vorp,select=c2("Player","VORP"))
p_vorp1$VORP=as.numeric(as.character(p_vorp1$VORP))
p_final=merge(p_nba,p_vorp1,by="Player")
p_final$vorp_0=ifelse(p_final$VORP>= 0,1,0) #binary VORP variable (1,0)

#subset guards (shooting and point guards)
p_final_g=subset(p_final,Pos=="SG"|Pos=="PG")
dim(p_final_g)

#train and test sets 
train=p_final_g[1:362,]
test=p_final_g[363:453,]

#logistic regression model 
model_lr=glm(as.factor(vorp_0)~drives_min+fga_d+fta_d+
               pts_d+ast_d,family=binomial,data=train)
summary(model_lr) 
test$pred_VORP=predict(model_lr,test) #predict probability that guard will have VORP>0 
sub_test=subset(test,select=c("Player","pred_VORP","VORP"))
missed=subset(sub_test,pred_VORP>0.5 & VORP<0)

#nba leaders in % of fta stemming from drives 
high_fta=subset(p_final_g,fta_d>=0.227 & MIN>25,select=c("Player","fta_d"))