library("ggplot2")
file=read.table("N_U2OS_top.csv",sep=",",header = T)

plot(log2(file$logFC),file$FDR,xlab="fold.change", ylab="q.value")
#colnames(file)=c("fold.change","q.value")
file$threshold = as.factor(file$FDR < 0.05 & abs((file$logFC)) >=1)
p=ggplot(data=file,
         aes(x=logFC, y =-log10(FDR),
             colour=threshold,fill=threshold)) +
  scale_color_manual(values=c("grey", "red","grey"))+
  geom_point(alpha=0.4, size=1.2) +
  xlim(c(-4, 4)) +
  theme_bw(base_size = 12, base_family = "Times") +
  geom_vline(xintercept=c(-1.5,1.5),lty=4,col="grey",lwd=0.6)+
  geom_hline(yintercept = -log10(0.05),lty=4,col="grey",lwd=0.6)+
  labs(x="log2 (fold change)",y="-log10 (p-value)",title="Volcano")
p=p+theme(panel.background = element_rect(fill = "transparent", color = "gray"), legend.key = element_rect(fill = "transparent", color = "transparent"))

ggplot(data=file,
       aes(x=log2(fold.change), y =-log10(q.value),
           colour=threshold,fill=threshold)) +
  scale_color_manual(values=c("grey", "red","grey"))+
  geom_point(alpha=0.4, size=1.2) +labs(x="log2 (fold change)",y="-log10 (p-value)",title="Volcano")+
  theme(panel.background = element_rect(fill = "transparent", color = "gray"), legend.key = element_rect(fill = "transparent", color = "transparent"))+geom_vline(xintercept=c(-1.5,1.5),lty=4,col="grey",lwd=0.6)+
  geom_hline(yintercept = -log10(0.05),lty=4,col="grey",lwd=0.6)
