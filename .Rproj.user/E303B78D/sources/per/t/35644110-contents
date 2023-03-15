setEPS()
postscript("histogram-01.eps")

#Lendo dados
mut<-read.csv("mut-s.csv",sep=";",dec=",",header=TRUE)
mut
summary(mut)

hist(mut$E)

setEPS()
postscript("mut-boxplot.eps")
png('mut-boxplot.png')
boxplot(mut,ylab="Mutation Score",xlab="Test Set",names=c("Randoop (R)", "EvoSuite (E)","Palus (P)", "JTExpert (J)", "AllSmart (AS)"))


dev.off()

#Teste de Normalidade
bartlett.test(mut)

#Teste de Normalidade
shapiro.test(mut$R)
ks.test(mut$R, "pnorm", mean(mut$R), sd(mut$R))
#ks.test(mut$R, "pnorm", mean(mut$R), sd(mut$R), exact = TRUE)

shapiro.test(mut$E)
hist(mut$E)

shapiro.test(mut$P)
hist(mut$P)

shapiro.test(mut$J)
hist(mut$J)


attach(mut)

#Colocando dados na vertical
mut.vert<-data.frame(Score<-gl(4,33), Tools<-c(R,E,P,J))
mut.vert

#Media
tapply(Tools, Score, mean)

#Desvio Padrão
tapply(Tools, Score, sd)

#Variânica
tapply(Tools, Score, var)

kruskal.test(Tools~Score, mut.vert)
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
