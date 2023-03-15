#Lendo dados
ts<-read.csv("ts-s.csv",sep=";",dec=",",header=TRUE)
ts
summary(ts)

setEPS()
postscript("ts-boxplot.eps")
png('ts-boxplot.png')
boxplot(ts,ylab="Number of Test Cases",xlab="Test Set",names=c("Randoop (R)", "EvoSuite (E)","Palus (P)", "JTExpert (J)"))

dev.off()

#Teste de Normalidade
bartlett.test(ts)

#Teste de Normalidade
shapiro.test(ts$R)
shapiro.test(ts$E)
shapiro.test(ts$P)
shapiro.test(ts$J)

attach(ts)

#Colocando dados na vertical
ts.vert<-data.frame(TestSet<-gl(4,33), Tools<-c(R,E,P,J))
ts.vert

#Media
tapply(Tools, TestSet, mean)

#Desvio Padrão
tapply(Tools, TestSet, sd)

#Variânica
tapply(Tools, TestSet, var)

kruskal.test(Tools~TestSet, ts.vert)
kruskal.test(list(R,E,P,J))

# Wilcox Test
print("Wilcox Test R e E")
me = wilcox.test(Tools[1:33],Tools[34:66],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test R e P")
mc = wilcox.test(Tools[1:33],Tools[67:99],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test R e J")
mr = wilcox.test(Tools[1:33],Tools[100:132],paired=TRUE,alternative="two.sided")

p=c(me$p.value, mc$p.value, mr$p.value)
p

# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html
padj=p.adjust(p, method = "holm", n = length(p))
#p.adjust(p, method = "bonferroni", n = length(p))
padj
padj[1]
cat("Test Sets;", "p-value;", "adjusted p-value;", "\nRE;", me$p.value, ";", padj[1], "\nRP;", mc$p.value, ";", padj[2], "\nRJ;", mr$p.value, ";", padj[3])
