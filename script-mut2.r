#Lendo dados
mut<-read.csv("mut-s.csv",sep=";",dec=",",header=TRUE)
mut
summary(mut)

setEPS()
postscript("mut-boxplot.eps")
png('mut-boxplot.png')
boxplot(mut,ylab="Mutation Score",xlab="Test Sets",names=c("R", "E", "P", "J", "AS"))

dev.off()

#Teste de Normalidade/Honomosidade
bartlett.test(mut)

attach(mut)

#Teste de Normalidade - quando p.valur < 0.05 não é distribuição normal
shapiro.test(R)
shapiro.test(E)
shapiro.test(P)
shapiro.test(J)
shapiro.test(AS)


#Colocando dados na vertical
mut.vert<-data.frame(Score<-gl(5,33), Tools<-c(R,E,P,J,AS))
mut.vert

#Media
tapply(Tools, Score, mean)

#Desvio Padrão
tapply(Tools, Score, sd)

#Variânica
tapply(Tools, Score, var)

kruskal.test(Tools~Score, mut.vert)
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
ras

p=c(re$p.value, rp$p.value, rj$p.value, ras$p.value)
p

# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html
padj=p.adjust(p, method = "holm", n = length(p))
padj
padj[1]
cat("Test Sets;", "p-value;", "adjusted p-value;", "\nRE;", re$p.value, ";", padj[1], "\nRP;", rp$p.value, ";", padj[2], "\nRJ;", rj$p.value, ";", padj[3], "\nRAS;", ras$p.value, ";", padj[4])

# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html
padj=p.adjust(p, method = "bonferroni", n = length(p))
padj
padj[1]
cat("Test Sets;", "p-value;", "adjusted p-value;", "\nRE;", re$p.value, ";", padj[1], "\nRP;", rp$p.value, ";", padj[2], "\nRJ;", rj$p.value, ";", padj[3], "\nRAS;", ras$p.value, ";", padj[4])
