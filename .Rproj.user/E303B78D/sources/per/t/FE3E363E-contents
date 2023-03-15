#Lendo dados
cov<-read.csv("cov-s.csv",sep=";",dec=",",header=TRUE)
cov
summary(cov)

setEPS()
postscript("cov-boxplot.eps")
png('cov-boxplot.png')
boxplot(cov,ylab="Statement Coverage",xlab="Test Set",names=c("Randoop (R)", "EvoSuite (E)","Palus (P)", "JTExpert (J)", "AllSmart (AS)"))

dev.off()

#Teste de Normalidade
bartlett.test(cov)

#Teste de Normalidade
shapiro.test(cov$R)
shapiro.test(cov$E)
shapiro.test(cov$P)
shapiro.test(cov$J)

attach(cov)

#Colocando dados na vertical
cov.vert<-data.frame(Coverage<-gl(4,33), Tools<-c(R,E,P,J))
cov.vert

#Media
tapply(Tools, Coverage, mean)

#Desvio Padrão
tapply(Tools, Coverage, sd)

#Variânica
tapply(Tools, Coverage, var)

kruskal.test(Tools~Coverage, cov.vert)
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
