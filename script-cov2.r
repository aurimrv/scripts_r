#Lendo dados
cov<-read.csv("cov-s.csv",sep=";",dec=",",header=TRUE)
cov
summary(cov)

setEPS()
postscript("cov-boxplot.eps")
png('cov-boxplot.png')
boxplot(cov,ylab="Statement Coverage",xlab="Test Sets",names=c("R", "E","P", "J", "AS"))

dev.off()

#Teste de Normalidade
bartlett.test(cov)

#Teste de Normalidade
shapiro.test(cov$R)
shapiro.test(cov$E)
shapiro.test(cov$P)
shapiro.test(cov$J)
shapiro.test(cov$AS)

attach(cov)

#Colocando dados na vertical
cov.vert<-data.frame(Coverage<-gl(5,33), Tools<-c(R,E,P,J,AS))
cov.vert

#Media
tapply(Tools, Coverage, mean)

#Desvio Padrão
tapply(Tools, Coverage, sd)

#Variânica
tapply(Tools, Coverage, var)

kruskal.test(Tools~Coverage, cov.vert)
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
#padj=p.adjust(p, method = "bonferroni", n = length(p))
padj
padj[1]
cat("Test Sets;", "p-value;", "adjusted p-value;", "\nRE;", re$p.value, ";", padj[1], "\nRP;", rp$p.value, ";", padj[2], "\nRJ;", rj$p.value, ";", padj[3], "\nRAS;", ras$p.value, ";", padj[4])
