# The collums corresponding test sets:
#X1 = RANDOM
#X2 = DYNAMOSA
#X3 = MIO
#X4 = MOSA
#X5 = WHOLE_SUITE
#X6 = DYNAMOSA-MIO
#X7 = DYNAMOSA-MOSA
#X8 = DYNAMOSA-WHOLE_SUITE
#X9 = MIO-MOSA
#X10 = MIO-WHOLE_SUITE
#X11 = MOSA-WHOLE_SUITE
#X12 = DYNAMOSA-MIO-MOSA
#X13 = DYNAMOSA-MIO-WHOLE_SUITE
#X14 = DYNAMOSA-MOSA-WHOLE_SUITE
#X15 = MIO-MOSA-WHOLE_SUITE
#X16 = DYNAMOSA-MIO-MOSA-WHOLE_SUITE

#Lendo dados
mut<-read.csv("compiled-mutation-score-report-cosmic-ray.csv",sep=";",dec=".",header=TRUE)
mut
summary(mut)

setEPS()
postscript("mutation-score-cosmic-ray-boxplot.eps")
png('mutation-score-cosmic-ray-boxplot.png')
boxplot(mut,ylab="Mutation Score",xlab="Test Sets",names=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))

dev.off()

#Teste de Homogeniedade
# p < 0.05 rejects the null hypothesis and conclude that not all groups have the same variance
bartlett.test(mut)

#Teste de Normalidade
# p < 0.05 rejects the null hypothesis that the data are from a normally distributed population
shapiro.test(mut$X1)
shapiro.test(mut$X2)
shapiro.test(mut$X3)
shapiro.test(mut$X4)
shapiro.test(mut$X5)
shapiro.test(mut$X6)
shapiro.test(mut$X7)
shapiro.test(mut$X8)
shapiro.test(mut$X9)
shapiro.test(mut$X10)
shapiro.test(mut$X11)
shapiro.test(mut$X12)
shapiro.test(mut$X13)
shapiro.test(mut$X14)
shapiro.test(mut$X15)
shapiro.test(mut$X16)

attach(mut)

#Colocando dados na vertical
mut.vert<-data.frame(mutation<-gl(16,21), TestSets<-c(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16))
mut.vert

#Media
tapply(TestSets, mutation, mean)

#Desvio Padrão
tapply(TestSets, mutation, sd)

#Variânica
tapply(TestSets, mutation, var)

kruskal.test(TestSets~mutation, mut.vert)
kruskal.test(list(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16))

# Wilcox Test
print("Wilcox Test X1 e X2")
x1x2 = wilcox.test(TestSets[1:21],TestSets[22:42],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X3")
x1x3 = wilcox.test(TestSets[1:21],TestSets[43:63],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X4")
x1x4 = wilcox.test(TestSets[1:21],TestSets[64:84],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X5")
x1x5 = wilcox.test(TestSets[1:21],TestSets[85:105],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X6")
x1x6 = wilcox.test(TestSets[1:21],TestSets[106:126],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X7")
x1x7 = wilcox.test(TestSets[1:21],TestSets[127:147],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X8")
x1x8 = wilcox.test(TestSets[1:21],TestSets[148:168],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X9")
x1x9 = wilcox.test(TestSets[1:21],TestSets[169:189],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X10")
x1x10 = wilcox.test(TestSets[1:21],TestSets[190:210],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X11")
x1x11 = wilcox.test(TestSets[1:21],TestSets[211:231],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X12")
x1x12 = wilcox.test(TestSets[1:21],TestSets[232:252],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X13")
x1x13 = wilcox.test(TestSets[1:21],TestSets[253:273],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X14")
x1x14 = wilcox.test(TestSets[1:21],TestSets[274:294],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X15")
x1x15 = wilcox.test(TestSets[1:21],TestSets[295:315],paired=TRUE,alternative="two.sided")

# Wilcox Test
print("Wilcox Test X1 e X16")
x1x16 = wilcox.test(TestSets[1:21],TestSets[316:336],paired=TRUE,alternative="two.sided")


p=c(x1x2$p.value, x1x3$p.value, x1x4$p.value, x1x5$p.value, x1x6$p.value, 
    x1x7$p.value, x1x8$p.value, x1x9$p.value, x1x10$p.value, x1x11$p.value, 
    x1x12$p.value, x1x13$p.value, x1x14$p.value, x1x15$p.value, x1x16$p.value)
p

# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html
padj=p.adjust(p, method = "holm", n = length(p))
#padj=p.adjust(p, method = "bonferroni", n = length(p))
padj
padj[1]
cat("Test Sets;", "p-value;", "adjusted p-value;", "\nx1x2;", x1x2$p.value, ";", padj[1], 
    "\nx1x3;", x1x3$p.value, ";", padj[2], "\nx1x4;", x1x4$p.value, ";", padj[3], 
    "\nx1x5;", x1x5$p.value, ";", padj[4], "\nx1x6;", x1x6$p.value, ";", padj[5], 
    "\nx1x7;", x1x7$p.value, ";", padj[6], "\nx1x8;", x1x8$p.value, ";", padj[7], 
    "\nx1x9;", x1x9$p.value, ";", padj[8], "\nx1x10;", x1x10$p.value, ";", padj[9], 
    "\nx1x11;", x1x11$p.value, ";", padj[10], "\nx1x12;", x1x12$p.value, ";", padj[11], 
    "\nx1x13;", x1x13$p.value, ";", padj[12], "\nx1x14;", x1x14$p.value, ";", padj[13], 
    "\nx1x15;", x1x15$p.value, ";", padj[14], "\nx1x16;", x1x16$p.value, ";", padj[15])
