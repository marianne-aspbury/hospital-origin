#-------
#Heuristic and Bayes analyses to predict hospital origin of pathogen strains based on their sequence of SNPs
#
#Produces grid-arranged plots of the result of both methods, of specified image size;
#x axis labelled by colours of hospitals also used in Maximum Likelihood Tree
#source for code on images as x axis labels: https://stackoverflow.com/a/14078391
#
#MA 08/09/17
#-------

#### Pre-amble - Packages and data imports etc. #### 
#don't clear R memory as may want obj.s from external scripts, if do insert rm(list=ls())

#set working directory. for data import etc.
filepath <- "filepath/data_folder"
setwd(filepath)

#set base of folder saving plots to within the loop
plot_filepath <- "filepath/plots_folder" 

#load relevant packages
require(plyr)
require(dplyr)
require(data.table)
require(ggplot2)
require(grid)
require(gridExtra)
require(gdata)
require(png)

# Import data 
#Isolates inc. singletons so 23686 loci. 1s indicate SNPs, 0s majority nucleotide; row name is locus number
all_isolates <- read.csv("2017 Bi-allelic 2001 to 2010 Binary dataset 23686 loci.csv", header = TRUE, row.names = 1)
#rownames(all_isolates) 

#Import hospital metadata (which uses anonymised hospital names)
all_isolates.metadata <- read.csv("Metadata 2001 to 2010 Updated Hospital Anon and RCs.csv", header = TRUE, row.names = 1)

#attach the metadata to R - means can call column names directly without doing dataframe$colname
attach(all_isolates.metadata)

#creata a 2010 metadata subset for assigning labels more easily later
metadata_2010 <- all_isolates.metadata[which(StrainYear2==2010),] 

#Isolate subsets - either csv import, or manipulate entire set already imported
#Subsetting in code MUCH FASTER than importing separate csv (0.02 s elapse vs 6.8 s for import)

isolates2001to2009_23686_loci <- all_isolates[,1:931]
isolates2010_23686_loci <- all_isolates[,932:1022]

## Compute and store samples per hospital ANONYMISED NAMES for count matrix calcs ##
#raw numbers total
setDT(all_isolates.metadata)
hospital_all_isolates_num <- all_isolates.metadata[, .N, by=Hospital_Anon]
hosp_all_isolates_nums.unlist <- unlist(hospital_all_isolates_num$N) #(usefully, col names retained)

#including null hospital
hospital_all_isolates_null_num <- rbind(hospital_all_isolates_num, data.frame("Hospital_Anon" = "Null", "N" = 1))
hosp_all_isolates_null_nums.unlist <- unlist(hospital_all_isolates_null_num$N)

#initial ref data
metadata_2001to2009 <- filter(all_isolates.metadata, StrainYear2!=2010)
setDT(metadata_2001to2009)

#make a just hospitals list to call
hosps_only <- hospital_all_isolates_num$Hospital_Anon

#lists only numbers of hospitals' samples if non-zero number from hospital
hospital_isolates2001to2009_num <- metadata_2001to2009[, .N, by=Hospital_Anon]

#find out which 2010 hospitals are not in 2001-2009 hospital isolates list
hosps_not_in_2001to2009<- data.frame(hospital_all_isolates_num[!(hosps_only%in%hospital_isolates2001to2009_num$Hospital_Anon)][,1], 
                                     N=rep(0, (nrow(hospital_all_isolates_num)-nrow(hospital_isolates2001to2009_num))))

#add back in missing hospitals with a count of 0 isolates sampled from them
hospital_isolates2001to2009_num<-rbind(hospital_isolates2001to2009_num, hosps_not_in_2001to2009)

#add a null hospital
hospital_isolates2001to2009_null_num<-rbind(hospital_isolates2001to2009_num, data.frame("Hospital_Anon" = "Null", "N" = 1))
hosp_isolates2001to2009_null_nums.unlist <- unlist(hospital_isolates2001to2009_null_num$N)

# create list of initial numbers from above, which can be adjusted in a dynamic loop fashion #
hosp_isos <- hosp_isolates2001to2009_null_nums.unlist #init num
hosp_isos_list <- cbind(hospital_all_isolates_null_num[,1], hosp_isos) #for making count matrix

#Run plotting_aesthetics_2001to2010hosps.R 
#includes basic plot themes and also ordering & colouring for aesthetically consistent plots
# this only calls if not already loaded to save time, by checking for occurence of unique obj.
if(!exists("ggplot_cols_anon", mode="function")) source("plotting_aesthetics_2001to2010hosps.R")

#Set up for plotting hospital icons in x axis labels ####
#Read in the saved images
require(png)
# folder is hospital colour code images in working directory filepath
img.filepath <- "hospital colour code images" 

img.filenames <- list.files(path = img.filepath, pattern = ".png")
img.filepath.names <- c()
for (i in 1:length(img.filenames)) img.filepath.names[i] <- paste(img.filepath, "/", img.filenames[i], sep="")
img <- lapply(img.filepath.names, png::readPNG)

#name and re-order list to match x axis order
names(img) <- sub(".png", "", img.filenames)

img <- img.geo_order <- img[names(hospcols_anon)]
#names(hospcols_anon)[which(!names(hospcols_anon)%in%names(img))]

#Set up the function to input images to x axis label - from sourcode in init. descrip. #----

# user-level interface to the element grob
my_axis = function(img) {
  structure(
    list(img=img),
    class = c("element_custom","element_blank", "element") # inheritance test workaround
  )
}
# returns a gTree with two children: the text label, and a rasterGrob below
element_grob.element_custom <- function(element, x,...)  {
  stopifnot(length(x) == length(element$img))
  tag <- names(element$img)
  # add vertical padding to leave space
  #g1 <- textGrob(paste0(tag, "\n\n\n\n\n"), x=x,vjust=0.6)
  g2 <- mapply(rasterGrob, x=x, image = element$img[tag], 
               MoreArgs = list(vjust=-2,interpolate=FALSE,
                               height=unit(0.3,"lines")),
               SIMPLIFY = FALSE)
  
  #gTree(children=do.call(gList,c(g2,list(g1))), cl = "custom_axis")
  gTree(children=do.call(gList,c(g2)), cl = "custom_axis")
}
# gTrees don't know their size and ggplot would squash it, so give it room
grobHeight.custom_axis = heightDetails.custom_axis = function(x, ...)
  unit(2, "lines")

#### Set up sum function with calls to anon names for computing count matrix ####

##Sum rows of hospital isolates using reference to pos_nums dataframe, to ascertain which columns refer to which hospitals in the ref dataframe ##
#For use in the mega-loop; dynamic testing, need to include null hospital at the end

hosp_sum_fn4 <- function(ref, pos) { #ref = all_isolates, #pos = all_isolates.metadata, #num = hospital_isolates_num
  #Create dataframe of the right size; name columns by hospital:
  res <- data.frame(matrix(0, nrow=nrow(ref), ncol=nrow(hospital_all_isolates_num))) 
  colnames(res) <- hospital_all_isolates_num$Hospital_Anon #anonymised name
  hosps_no_samples <- as.character(hosp_isos_list$Hospital_Anon[which(hosp_isos==0)]) #anonymised name
  res[,hosps_no_samples] = NA
  Null <- c(rep(0, nrow(ref))) #null hospital 'isolate'
  #Do the summing
  for (i in c(1:ncol(ref))) { #for index which is 1 to number of isolates, ie 1022, (columns in ref)
    row_num <- which(pos == colnames(ref)[i]) #return e.g. 1 for row position of required isolate in metadata, in order to find the right hospital
    hosp <- as.character(pos$Hospital_Anon[row_num]) #give hosp character to assign sum to correct column in result
    res[,hosp] <- res[,hosp] + ref[,i] #for column of i's hospital, add to pos of res[rows, columni's hospital] the values at ref[rows, columni]
  }
  res <- cbind(res, Null)
  return(res) 
}

#### Set up other functions for Heuristic and Bayes analysis ####
#Set up NaN replacement function for dataframes (when 0/0 gives NaN, use to replace with 0s)
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

#### Requiremnts for computing posterior probabilities, p_sh with beta prior hyperparameters (a,b) ####
# #Beta parameters estimated from mean & variance of 23686 loci, 1022 isolate set (2001-2010)
a <- 0.04500156
b <- 19.79106

### Function for applying general formula: p_sh <- (hosp_all_sum + a)/(nh+a+b) ###
# Takes 2 arguments, the count matrix where isolates' SNPs are summed by hospital, and the integer list of samples per hospital
div_by_nab <- function(hosp_count_matrix, isos_per_hosp) { #need isos_per_hosp to be 'unlisted' form of samples for each hosp 
  (hosp_count_matrix+a)/(isos_per_hosp+a+b)
}
###

#### Inital data for computations  must be re-set if break and restart loop  ####
#set up init. hosp_isos refs & samples per hosp: initial counts of isolates, which can be adjusted in a dynamic loop fashion
isolates_in_ref <- isolates2001to2009_23686_loci
metadata_ref <- all_isolates.metadata #this is needed if updating metadata in loop for tests where remove isolates from dataset
hosp_isos <- hosp_isolates2001to2009_null_nums.unlist #init. num of isolates by hosp
hosp_isos_list <- cbind(hospital_all_isolates_null_num[,1], hosp_isos) #correct data-type for making count matrix (to work within count function)

#### Loop of the actual computations for Heuristic & Bayes together ####
#p<-proc.time() #~ 10 minutes total, when saving with a laptop, so can be quicker
for (i in 1:ncol(isolates2010_23686_loci)) {
  
  # Heuristic first as shorter code
  
  #### Heuristic calculation parts ####
  
  ## Compute based on where 1s (SNPs) are in the query
  #Find 1s in query
  SNP_rows <- which(isolates2010_23686_loci[,i]==1)
  
  #Select relevant rows of database and query
  query_row_select <- isolates2010_23686_loci[SNP_rows,i]
  isolates_in_ref_row_select <- isolates_in_ref[SNP_rows,]
  
  ## Calculations
  #Still want the count matrix for Heuristic, just use it differently:
  #SNP counts per hospital
  count_ref_dataset_row_select <- hosp_sum_fn4(isolates_in_ref_row_select, metadata_ref) 
  
  #For normalisation, need SNP count over all hosps
  total_row_counts <- apply(count_ref_dataset_row_select, 1, sum, na.rm=TRUE)
  
  #Normalise hospital counts by scaling by SNP frequency in database
  z_sh <- count_ref_dataset_row_select/total_row_counts
  
  #If total_row_count = 0 at a position, z_sh = NaN at that position, hence fix to make 0
  z_sh[is.nan(z_sh)] <- 0
  
  #Sum normalised SNP counts within hospital
  z_h <- apply(z_sh, 2, sum)
  
  #Normalise these counts by total over hospitals
  y_h.heuristic <- z_h/sum(z_h, na.rm=TRUE)
  
  #remove null as it's always 0 for this method
  y_h.heuristic <- y_h.heuristic[-which(names(y_h.heuristic)=="Null")]
  
  #### Bayes calculation parts ####
  
  #count matrix based on reference data
  #only takes relevant isolates into account - looks up which are present in ref
  count_ref_dataset <- hosp_sum_fn4(isolates_in_ref, metadata_ref) 
  
  #probability p_sh for isolates
  p_sh <- aperm(apply(count_ref_dataset,1,div_by_nab, isos_per_hosp = hosp_isos))
  
  ##Start calculations - compute log likelihoods for practicality
  #keeps the hospital names according to p_sh automatically
  pre_log_L_h_list <- isolates2010_23686_loci[,i]*log(p_sh[,]) + (1-isolates2010_23686_loci[,i])*log(1-p_sh[,])
  log_L_h <- apply(pre_log_L_h_list, 2, sum)
  
  #if 47th hospital (null) had 50 SNPs in unseen loci - shifts required
  #50 adds for each of "50 extra 0 SNP matches" in true hosps vs query, "C" = 0
  log_L_h[1:46] <- log_L_h[1:46] + 50*log(1-a/(hosp_isos[1:46]*(a+b))) 
  #50 adds for each of "50 extra 1 SNP mis-matches" in dummy hosp vs query, "C" = 1
  log_L_h[47] <- log_L_h[47] + 50*log((1-(1+a)/(a+b))) 
  
  #Subsequent priors will be added into lists called Y_log_.... which represents an adapted log likelihood "log(Y)"
  #add in the sampling frequency prior - hosp_sampling_prior just hosp_isos/sum(hosp_isos)
  Y_log_list_sampling_prior <- log_L_h + log(hosp_isos/sum(hosp_isos)) 
  
  #shift to postitve values - just for plotting
  Y_log_list_sampling_prior_shift <- Y_log_list_sampling_prior - min(Y_log_list_sampling_prior, na.rm = TRUE)
  
  ## Add in the "prior on geographical location" - according to where query was sampled
  #Set the relative weights at hospital, RC, and general hospital levels taken from Candidate Introduction frequencies
  alpha_1 <- (1022-57)/1022
  alpha_2 <- (57-39)/1022
  alpha_3 <- 39/1022
  alpha_sum <- alpha_1 + alpha_2 + alpha_3
  
  #initialise another list for this prior - name "hosp_ep" is derived from "epidemiology"
  Y_log_list_hosp_ep_prior <- Y_log_list_sampling_prior
  
  #find the correct sample hosp, its RC hosps, and others (hosps not in the RC)
  #these are needed to apply the correct geographical clustering alphas
  hosp_sam <- as.character(metadata_2010$Hospital_Anon[i])
  hosps_in_RC <- names(which(HospitalRCs_anon==HospitalRCs_anon[hosp_sam])) #calling the named list HospitalRCs_anon
  hosps_in_RC_in_dataset <- names(HospitalRCs_anon[hosps_in_RC])[hosps_in_RC%in%as.character(Hospital_Anon[1:(931+i)])] # needed incase some of RC hospitals appear later in database
  RC_other_hosps <- hosps_in_RC_in_dataset[hosps_in_RC_in_dataset!=hosp_sam]
  RC_other_num <- length(RC_other_hosps)
  oth_hosps <- names(Y_log_list_hosp_ep_prior)[!names(Y_log_list_hosp_ep_prior)%in%hosps_in_RC]
  
  #shift likelihood according to set priors
  #First determine if query is first isolate in dataset from its hospital, and if so change NA to an init. value to enable shifts
  if (is.na(Y_log_list_hosp_ep_prior[hosp_sam])) {
    #First see if can init. to an aggregated RC log(Y) value, possible if > 1 existing hosp in its RC in database
    if (RC_other_num > 1) {
      #count for RC_other_hosps
      count_RC <- data.frame(count_RC = apply(count_ref_dataset[,RC_other_hosps], 1, sum))
      isos_RC <- sum(hosp_isos[which(hosp_isos_list$Hospital_Anon%in%RC_other_hosps)])
      p_sRC <- data.frame(p_sRC = apply(count_RC,1,div_by_nab, isos_per_hosp = isos_RC))
      log_pre_Y_RC <- isolates2010_23686_loci[,i]*log(p_sRC[,]) + (1-isolates2010_23686_loci[,i])*log(1-p_sRC[,])
      log_Y_RC <- sum(log_pre_Y_RC)
      Y_log_list_hosp_ep_prior[hosp_sam] <- log_Y_RC
    }
    #if can't aggregate an RC value for the hospital, uses dummy hosp value
    else (Y_log_list_hosp_ep_prior[hosp_sam] <- Y_log_list_hosp_ep_prior[47])
  }
  
  #Apply geographical clustering alphas to different levels
  Y_log_list_hosp_ep_prior[hosp_sam] <- Y_log_list_hosp_ep_prior[hosp_sam] + log(alpha_1/alpha_sum)
  Y_log_list_hosp_ep_prior[RC_other_hosps] <- Y_log_list_hosp_ep_prior[RC_other_hosps] + log((alpha_2/alpha_sum)/RC_other_num)
  Y_log_list_hosp_ep_prior[oth_hosps] <- Y_log_list_hosp_ep_prior[oth_hosps] + log((alpha_3/alpha_sum)/length(oth_hosps))
  
  #shift to positive log likelihoods just for plotting. This is the final plotted value, as taking exponentials for probability causes very large differences.
  Y_log_list_hosp_ep_prior_shift <- Y_log_list_hosp_ep_prior - min(Y_log_list_hosp_ep_prior, na.rm = TRUE)
  
  #shift for taking exponentials
  #t_h <- Y_log_list_sampling_prior - max(Y_log_list_sampling_prior, na.rm = TRUE) #no query geographical source prior
  t_h2 <- Y_log_list_hosp_ep_prior - max(Y_log_list_hosp_ep_prior, na.rm = TRUE) #inc. query geographical source prior
  
  #Take exponenetials for final Bayesian probability, normalise by sum
  #yh <- exp(t_h)/sum(exp(t_h), na.rm = TRUE) #no query geographical source prior
  yh2 <-  exp(t_h2)/sum(exp(t_h2), na.rm = TRUE) #inc. query geographical source prior
  

  #### update num of isolates sampled from each hosp for next loop iteration ####
  #Using ref to isolates metadata to ascertain which hosp to add sample num +1 to#
  # If want to start mid-way through the 2010 isolates, run this part wrapped with for (i in 1:(query interested in's num)){ }
  row_num <- which(all_isolates.metadata == colnames(isolates2010_23686_loci)[i])  #which row is the isolate from, by its code
  hosp <- as.character(Hospital_Anon[row_num]) #which hosp is the isolate from, by its row num in metadata
  hosp_index <- which(hospital_all_isolates_num$Hospital_Anon == hosp) #which position in list of samples is hospital
  hosp_isos[hosp_index] <- hosp_isos[hosp_index] + 1 #add 1 to num of isolates sampled from relevant hospital
  hosp_isos_list <- cbind(hospital_all_isolates_null_num[,1], hosp_isos) #for making count matrix

  ## update isolates in ref dataset 
  isolates_in_ref <- bind_cols(isolates_in_ref, isolates2010_23686_loci[i]) #bind_cols (from dplyr) preserves col names
    
  #### Plots ####
  #First, form separate plots as objects and then call grid.arrange on them at the end
  
  #Set strain code of query isolate in iteration for labelling and saving
  isolate <- metadata_2010$StrainCode2[i]
  
  #### Bayes alone: make a plot of the result for each loop, and save under separate names by isolate code ####
  ##shifted log likelihoods plot
  # set up some dataframes for ggplot to intake
    plot.df <- data.frame("hospital" = names(Y_log_list_hosp_ep_prior_shift),  "shifted log likelihood" = Y_log_list_hosp_ep_prior_shift, row.names=NULL)
    plot_hosps.df <- plot.df[-which(plot.df == "Null"), ]
  # object to set y scale max to nearest 50 of top val
  bayes.log.ymax <-  round_any(max(Y_log_list_hosp_ep_prior_shift, na.rm=TRUE), 50, f=ceiling)
  #Plot log likelihoods from Bayes analysis
  plot_Bayes.log <-
    ggplot(plot_hosps.df, na.rm=TRUE, aes(x=hospital, y=shifted.log.likelihood, fill=hospital)) +
      geom_bar(stat="identity", position="dodge") +
      labs(y = "Bayesian log likelihood (offset to 0)", x="Hospital") +
          #, title=paste('Isolate #', 931+i, ": ", isolate, "   Priors on Hospitals  (a,b) = (0.045, 19.8)", sep="")) +
      my_theme +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), #labelled anon hospitals on x axis
            legend.position = "none") +
      scale_x_discrete(expand=c(0,0), limits = c(Hospital_Anon_geo_order)) +
      scale_y_continuous(expand=c(0,0), limits = c(0, bayes.log.ymax), breaks = seq(0,bayes.log.ymax,50)) +
      geom_hline(yintercept = plot.df[which(plot.df == "Null"), 2], colour="dimgrey", linetype = 2, size=1) +
      scale_fill_manual(values = c(ggplot_cols_anon))
    
  #blank x labels version
  plot_Bayes.log.blankx <- plot_Bayes.log + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) 
    
  ##plot actual probabilities
  plot2.df <- data.frame("hospital" = names(yh2),  "probability" = yh2, row.names=NULL)
  plot_hosps2.df <- plot2.df[-which(plot2.df == "Null"), ]
  plot_Bayes.prob <- 
    ggplot(plot_hosps2.df, na.rm=TRUE, aes(x=hospital, y=probability, fill=hospital)) +
      geom_bar(stat="identity", position="dodge") +
      labs(y = "Bayesian Probability", x="Hospital") + 
           #, title=paste('Isolate #', 931+i, ":", isolate, "  Priors on Hospitals   (a,b) = (0.045, 19.8)")) +
      my_theme +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), #labelled with anon hosps on x axis
            legend.position = "none") +
      geom_hline(yintercept = plot2.df[which(plot2.df == "Null"), 2], colour="dimgrey", linetype = 2, size=1) +
      scale_x_discrete(limits = Hospital_Anon_geo_order, expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0), limits=c(0,1), breaks = seq(0,1,0.25)) +
      scale_fill_manual(values = ggplot_cols_anon)
    
  #blank x labels version
  plot_Bayes.prob.blankx <- plot_Bayes.prob + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) 
  
  #### Heuristic alone make a plot of the result for each loop, and save under separate names by isolate code ####
  #Plots: format same as for Bayes:
  plot.heuristic <- data.frame("hospital" = names(y_h.heuristic),  "heuristic probability" = y_h.heuristic, row.names=NULL)
  plot_heuristic <- 
    ggplot(plot.heuristic, na.rm=TRUE, aes(x=hospital, y=heuristic.probability, fill=hospital)) +
      geom_bar(stat="identity", position="dodge") +
      labs(y = "Heuristic Probability", x="Hospital") + #, title=paste('Isolate #', 931+i, ": ", isolate, "   Heuristic Method Result", sep="")) +
      my_theme +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), #labelled with anon hosps on x axis
            legend.position = "none") +
      scale_x_discrete(limits = c(Hospital_Anon_geo_order), expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0), breaks = seq(0,1,0.1), limits = c(0, 1)) + #fixed y axis 0 to 1
      #Max probability dependent scale, if desired:
      #scale_y_continuous(expand=c(0,0), breaks = seq(0,1,0.1), limits = c(0, signif(max(y_h.heuristic, na.rm=TRUE), 1)+.1)) +
      scale_fill_manual(values = c(ggplot_cols_anon))
  
  #blank x labels version
  plot_heuristic.blankx <- plot_heuristic + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) 
  
  #### Formatting/Saving Heuristic & Bayes Plots together, using grid.arrange ####
  
  #Plot with hospital icons on x axis
  #Reformat both plots to use icons
  plot_heuristic.colcode <- plot_heuristic.blankx + theme(axis.text.x = my_axis(img), 
                                                  axis.title.x=element_text(margin = margin(t = -18, r = 0, b = 0, l = 0)))
  plot_Bayes.colcode <- plot_Bayes.log.blankx + theme(axis.text.x = my_axis(img), 
                                                      axis.title.x=element_text(margin = margin(t = -18, r = 0, b = 0, l = 0)))
  
  #form "grobs" of each result to be able arrange both results onto one figure 
  g1 <- ggplotGrob(plot_heuristic.colcode)
  g2 <- ggplotGrob(plot_Bayes.colcode)
  
  #actual grid.arrange of the two plots side-by-side
  grid_hospcols <- grid.arrange(g1, g2, ncol=2, nrow=1, widths=c(1,1))
  
  ## Saving the arranged plots - can adjust save path and filenames. Specified image size, so not affected by gui in R.
  ggsave(plot=grid_hospcols, filename = paste(plot_filepath, '/',
                                       'Isolate_', 931+i, '_', isolate, 
                                       '.png', sep=''), device = "png",
         width=8, height=3, units="in", dpi=150)
  
  # #other plot saves with anon hospitals' names on x axes, not icons (Names to save by and filepaths to be adjusted)
  # #Saving specifying image size (decided by tests - concluded 8x6 inch dpi 100 best) not based on gui size.
  #
  # ggsave(plot = plot_Bayes.log, filename = paste(plot_filepath, "/log likelihoods plots/",
  #                         'Isolate_', 931+i, '_', isolate, "_allpriors_loglike", '.png', sep=''), device = "png",
  #                         width=8, height=6, units="in", dpi=100)
  
  # ggsave(plot = plot_Bayes.prob, filename = paste(plot_filepath, "/probability plots/",
  #         'Isolate_', 931+i, '_', isolate, "_allpriors_prob", '.png', sep=''), device = "png",
  #         width=8, height=6, units="in", dpi=100) 
  
  # ggsave(plot = plot_heuristic, filename = paste(plot_filepath, "/heuristic/",
  #                         'Isolate_', 931+i, '_', isolate, "_heuristic_MA", '.png', sep=''), device = "png",
  #        width=8, height=6, units="in", dpi=100) 
  
  #end of loop iteration, next isolate results will be computed and saved
  }
#proc.time()-p #~ 10 minutes for both methods and all 1022 isolates, saving with a laptop so could be quicker

#### END ####