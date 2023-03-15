#Lendo dados
ts<-read.csv("ts-s.csv",sep=";",dec=",",header=TRUE)
ts
summary(ts)

setEPS()
postscript("ts-boxplot.eps")
png('ts-boxplot.png')
boxplot(ts,ylab="Number of Tests",xlab="Test Sets",names=c("R", "E","P", "J", "AS"))

dev.off()

#Teste de Normalidade
bartlett.test(ts)

#Teste de Normalidade
shapiro.test(ts$R)
shapiro.test(ts$E)
shapiro.test(ts$P)
shapiro.test(ts$J)
shapiro.test(ts$AS)

attach(ts)

#Colocando dados na vertical
ts.vert<-data.frame(Tests<-gl(5,33), Tools<-c(R,E,P,J,AS))
ts.vert

#Media
tapply(Tools, Tests, mean)

#Desvio Padrão
tapply(Tools, Tests, sd)

#Variânica
tapply(Tools, Tests, var)

kruskal.test(Tools~Tests, ts.vert)
kruskal.test(list(R,E,P,J,AS))

# Wilcox Test
print("Wilcox Test R e E")
re = wilcox.test(Tools[1:33],Tools[34:66],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test R e P")
rp = wilcox.test(Tools[1:33],Tools[67:99],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test R e J")
rj = wilcox.test(Tools[1:33],Tools[100:132],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test R e AS")
ras = wilcox.test(Tools[1:33],Tools[133:165],paired=TRUE,alternative="two.sided")


p=c(re$p.value, rp$p.value, rj$p.value, ras$p.value)
p

# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html
padj=p.adjust(p, method = "holm", n = length(p))
#p.adjust(p, method = "bonferroni", n = length(p))
padj
padj[1]
cat("Test Sets;", "p-value;", "adjusted p-value;", "\nRE;", re$p.value, ";", padj[1], "\nRP;", rp$p.value, ";", padj[2], "\nRJ;", rj$p.value, ";", padj[3], "\nRAS;", ras$p.value, ";", padj[4])
