LG <- read.csv(file = "Gibbus.csv", stringsAsFactors = TRUE)
Location <- as.factor(LG$Location)
LG$Size <- as.factor(LG$Size)


LG_means <- LG %>%
  group_by(Location) %>% 
  summarise(Nm = mean(N), 
            Nsd = sd(N), 
            Cm = mean(C), 
            Csd = sd(C))


LGplot <- ggplot()+ 
  geom_errorbar(data = LG_means, 
                aes(x = Cm, 
                    ymin = Nm - Nsd, ymax = Nm + Nsd, colour = Location), width = 0.2, cex = 1)+ 
  geom_errorbarh(data = LG_means, 
                 aes(y = Nm, 
                     xmin = Cm - Csd, xmax = Cm + Csd, colour = Location), cex = 1)+
  scale_x_continuous(limits = c(-18, -8),breaks=seq(-18, -8, 2))+ 
  scale_y_continuous(limits = c(8,16),breaks=seq(8, 16, 2))+
  scale_color_manual(values =c("Chagos " = "#FF6F00B2", "Maldives " = "#8A4198B2", "SR" = "#C71000B2"), 
                     labels = c("Chagos", "Maldives","Scott Reefs"))+
  xlab(expression(atop(bold(~delta^13~"C " ("\u2030 " [vs]~"VPDB")))))+ 
  ylab(expression(atop(bold(~delta^15~"N " ("\u2030 " [vs]~"air")))))

LGplot


LG2 <- LGplot + geom_errorbar(aes(x = -14.254, 
                                  ymin = 12.804 - 1.511, ymax = 12.804 + 1.511), cex = 1, lty = 2, colour = "#C71000B2")+
  geom_errorbarh(aes(y = 12.804, 
                     xmin = -14.254 - -2.545, xmax = -14.254 + -2.545), cex =1, lty = 2, colour = "#C71000B2")

fig2 <- LG2 + theme(text = element_text(size = 20))

fig2



