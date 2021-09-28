library(tidyverse)
#Cherry_dat$Days <- forcats::as_factor(Cherry_dat$Days)
library(lme4)
library(nlme)
library(car)
library(multcomp)
library(emmeans)
library(rcompanion)
# Improt data
Cherry_dat <- rio::import('CherryAnalysis2021.xlsx')
# Check the structure of the data
str(Cherry_dat)
# Convert the required character (or numeric) variables into factor variables
Cherry_dat <- Cherry_dat %>% 
        rstatix::convert_as_factor(Treatment, Replication, Ind_ID)
# Sheet1, Sheet2, Sheet3 ... Sheet7 are dependent variables in this dataset
# Proceed for specifying the model with fixed and random effects
model = lme(Sheet2~Treatment*as_factor(Days),
            random = ~1|Ind_ID,
            data = Cherry_dat,
            correlation = corAR1(form = ~Days|Ind_ID),
            method = 'REML')

# Start a sink (a text file) for storing output into
# Always remember: .TXT files can only store text but NO figures or graphs
sink("MyTestFile.txt")
# Generate ANOVA output 
Anova(model)
# genwrate marginal mean table from emmeans package
marginal_means <- emmeans(model, ~Days:Treatment)
# Use the cld() compact letter display method to assign alphabets to the groups
# obtained by multiple comparisons
writeLines('\n')
Table2 <- cld(marginal_means, Letters = letters, adjust='tukey') %>% 
        arrange(Days, Treatment)
Table2
# Close the sink
sink()
#Required_outptut_table <- data.frame(marginal_means) 
#rio::export(Required_outptut_table, file = 'First_output.xlsx')#, overwrite=T)

# Generate a table of group-wise means with std error (for plot)
Sum = groupwiseMean(Sheet2 ~ Treatment + Days,
                    data   = Cherry_dat,
                    conf   = 0.95,
                    digits = 3,
                    traditional = T)

# Take group column from cld output and join with "Sum" table
Sum <- Sum %>% left_join(Table2 %>% dplyr::select(Treatment, Days, .group),
                  by = c('Treatment', 'Days'))
# Define a dodge variable for bar plot as well as error-bars
pd = position_dodge(.6)

# Plot function
ggplot(data = Sum, aes(x =    factor(Days), # define aesthetics with x and
                       y =    Mean,  # y variable
                       fill = Treatment, # use fill option for group variable in bar plot 
                       group=Treatment)
       )+ # grouping variable
        # use identity option in bar plot (default is frequency)
        # use position dodge (by default stacks will show up)
        geom_bar(stat = 'identity', position = pd, width = 0.5)+
        # specify ymin and ymax for error bars and choose a width
        # almost half of the width of bar
        geom_errorbar(aes(ymin=Trad.lower,
                          ymax=Trad.upper),
                      width=.25, size=.85, position = pd, color = 'black')+
        geom_point(shape=19, size=1,  position = pd)+
        # For adding the group label ans provide label variable
        geom_text(aes(label=.group), position = pd, hjust= -1, angle=90)+
        # for changing the y-axis limits as desired
        coord_cartesian(ylim=(c(4,10)))+ # theme_bw()+ #theme_minimal()+
        theme_classic()+ # We can choose any default theme setting available in ggplot2
        # Coustom labels for x and y axis as well title etc.
        labs(x='Days', y='MEAN')+#title='MY PLOT')+
        # Change the appearance of the font of plot
        theme(text = element_text(face = "bold", family = 'serif', size = 14),
              legend.direction = 'horizontal', # or the Legend orientation
              legend.position = c(.3, .9))#+  # Location on the plot 
        #scale_fill_grey(start = 0.4,end = 0.8)

ggsave('Exampleplot.emf', width = 7, height = 3.5, units = 'in')

## Looping over the variables of the entire data 
varnames=names(Cherry_dat)[4:10]
for (varname in varnames) {
        sink(paste(varname,'ANOVA and grouping.txt'))
        print(varname)
        model = lme(as.formula(paste0(varname, '~ Treatment*factor(Days)')),
                    random = ~1|Ind_ID,
                    data=Cherry_dat,
                    correlation = corAR1(form = ~Days|Ind_ID),
                    method="REML")
        
        print(Anova(model))
        marginal_means <- emmeans(model, ~Days:Treatment)
        
        print(cld(marginal_means, Letters = letters, alpha = 0.05) %>% arrange(Days,Treatment))
        sink()

        library(rcompanion)
        Sum = groupwiseMean(as.formula(paste(varname,'~ Treatment + Days')),
                            data   = Cherry_dat,
                            conf   = 0.95,
                            digits = 3,
                            traditional = FALSE,
                            percentile  = TRUE)
        library(ggplot2)
        
        pd = position_dodge(.6)
        
        ggplot(Sum, aes(x =    factor(Days),
                        y =    Mean,
                        fill = Treatment)) +
                geom_bar(stat = 'identity', position = pd, width = 0.55)+
                geom_errorbar(aes(ymin=Percentile.lower,
                                  ymax=Percentile.upper),
                              width=.25, size=0.5, position = pd, color = 'black') +
                geom_point(shape=1, size=.6,  position = pd) +
                theme_bw() + 
                theme(text = element_text(face = "bold", family = 'serif')) +
                xlab("Days")+scale_fill_grey(start = 0.4,end = 0.8)
        ggsave(paste(varname,'plot.emf'), width = 7, height = 3.5, units = 'in')
        ggsave(paste(varname,'plot.pdf'), width = 7, height = 3.5, units = 'in')
}
