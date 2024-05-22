a<-read.csv("pima-indians-diabetes.csv")
plot(a$BMI, a$Glu, main="Factors Affecting Diabetes", xlab="BMI", ylab="Blood Glucose", xlim=c(0,70), ylim=c(0,210), pch=19, col=ifelse((a$Diabetes == 1),"turquoise", "orange"))

library(ggplot2)
ggplot(a, aes(x=BMI, y=Glu, color=factor(Diabetes)))+geom_point()+xlab("BMI")+ylab("Blood Glucose")+guides(color=guide_legend(title="Diabetes"))+ggtitle("Factors Affecting Diabetes")

ggplot(a, aes(x=Preg, y=BMI, color=factor(Diabetes)))+geom_point()+xlab("Times Being Pregnant")+ylab("BMI")+guides(color=guide_legend(title="Diabetes"))+ggtitle("Factors Affecting Diabetes")