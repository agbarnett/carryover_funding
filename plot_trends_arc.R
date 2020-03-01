# plot_trends_arc.R
# trend in success rates by gender for ARC
# Feb 2020
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# 2019 data from NHMRC from Excel spreadsheet here: https://www.nhmrc.gov.au/file/14808/download?token=GAkwLHj0 , accessed 25-Feb-2020
nhmrc_data = read.table(header=TRUE, sep='\t', text='
Gender	Applications	Funded
Female	2581	339
Male	3185	423')


# data from https://www.arc.gov.au/policies-strategies/strategy/gender-equality-research/gender-outcomes-ncgp-trend-data
arc_data = read.table(header=TRUE, sep='\t', text='
Start_Yr	Female_applied	Indeterminate/Intersex	Male_applied	Unspecified	Female_won	Indeterminate/Intersex	Male_won	Unspecified
2002	2199	 	8055	 	817	 	3221	 
2003	4285	 	15302	1	1825	 	7090	1
2004	3338	 	11429	 	1189	 	4201	 
2005	3163	 	10945	 	1179	 	4257	 
2006	3531	 	10932	 	1036	 	3628	 
2007	3724	 	10919	 	1088	 	3565	 
2008	3874	 	11619	1	1188	 	3819	1
2009	4406	 	12585	 	1365	 	4157	 
2010	4324	 	12578	8	1393	 	4233	6
2011	4481	 	12363	26	1487	 	4205	6
2012	5066	 	13021	43	1294	 	3710	11
2013	5034	 	13557	47	1293	 	3846	13
2014	4864	 	13921	49	1233	 	3790	14
2015	4618	 	13507	29	1042	 	3368	10
2016	4743	4	14811	32	1101	1	3591	6
2017	4681	2	13686	25	1059	1	3306	8
2018	4600	1	12802	39	1061	 	2996	8
2019	3838	1	10122	24	939	 	2307	6') %>%
  select(-contains('Indet'), -contains("Unspec")) %>%
  mutate(p_female_applied = Female_applied / (Male_applied+Female_applied),
         p_female = Female_won / Female_applied,
         p_male = Male_won / Male_applied)

## summary stats on gaps
# a) applications
mutate(arc_data, gap = Male_applied - Female_applied) %>%
  summarise(mean(gap))
# b) winners
mutate(arc_data, gap = Male_won - Female_won) %>%
  summarise(mean(gap))

# plot numbers over time
to.plot.apps = select(arc_data, Start_Yr, Female_applied, Male_applied) %>%
  gather(`Female_applied`, `Male_applied`, key='gender', value='number') %>%
  mutate(gender = str_remove(gender, '_applied'), type='Applications')
to.plot.wins = select(arc_data, Start_Yr, Female_won, Male_won) %>%
  gather(`Female_won`, `Male_won`, key='gender', value='number') %>%
  mutate(gender = str_remove(gender, '_won'), type='Winners')
to.plot = bind_rows(to.plot.apps, to.plot.wins)
tplot = ggplot(data=to.plot, aes(x=Start_Yr, y=number, col=factor(gender)))+
  geom_line(lwd=1.1)+
  theme_bw()+
  scale_color_manual(NULL, values=1:2)+
  theme(legend.position=c(0.8,0.8))+
  xlab('Year')+
  ylab('Numbers')+
  facet_wrap(~type, scale='free_y')
tplot

# plot success rates
splot = ggplot(data=arc_data, aes(x=Start_Yr, y=p_male))+
  geom_line(col='dark blue', lwd=1.1)+
  geom_line(aes(Start_Yr, y=p_female), col='dark red', lwd=1.1)+
  theme_bw()+
  xlab('Year')+
  ylab('Success proportion')
splot

## ratios of application numbers
summary(arc_data$Male_applied/arc_data$Female_applied) # ARC
3185/2581 # NHMRC
