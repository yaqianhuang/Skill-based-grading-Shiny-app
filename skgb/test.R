plotstyle = list(
  theme_light(base_size=32),
  theme(panel.border = element_rect(color = 'black',fill=NA),
        axis.title.y = element_blank())
)
ggplot(df, aes(y = skill,x=num)) +
  geom_segment(aes(x=as.numeric(highlight),y=0,xend=as.numeric(highlight)+2,yend=0)) +
  labs(x='# Times demonstrated')+plotstyle
