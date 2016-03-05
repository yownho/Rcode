################################
# Read all netCDF file in the folder
# Output the results as tab-seprated text file
# By Yuanhao Zhao
################################

library(ncdf4)

#--------- INPUT ------------#
# Set working folder path
fdir <- "/Users/yownho/GoogleDrive/Research_Yuanhao/getdata/getdata/gdr_all"  #!IMPORTANT! Always use '/',
                                                                              # R code should be in 'fdir' folder
# Output file name
fout <- "j2_gdr_p61_Tarbela.txt"
#----------------------------#

#Get all net cdf files
names_gdr <- list.files(fdir, pattern = '.nc')

# Output data frame
df_out <- data.frame()

##### Main program
for (i in 1:length(names_gdr)){
  #Get nc file name
  fnc <- paste(fdir, names_gdr[i], sep='/')
  # Open netCDF file
  nc <- nc_open(fnc)
  
  # Get attributes
  attr <- ncatt_get(nc, 0)
  
  # Cycle number
  cycnum <- attr$cycle_number
  
  # Get all variable names
  varnames <- names(nc$var)
  
  #dimension of data
  para_dim <- nc$dim$time$len
  para_hz  <- nc$dim$meas_ind$len
  
  # Parameter of time
  para_today <- 1/86400
  
  #####################################################################
  # Variables, modify as user needs
  time_20hz <- ncvar_get(nc,nc$var$time_20hz) 
  lat <- ncvar_get(nc, nc$var$lat) 
  model_dry_tropo_corr <- ncvar_get(nc,nc$var$model_dry_tropo_corr) 
  model_wet_tropo_corr <- ncvar_get(nc,nc$var$model_wet_tropo_corr) 
  iono_corr_gim_ku <- ncvar_get(nc,nc$var$iono_corr_gim_ku) 
  solid_earth_tide <- ncvar_get(nc,nc$var$solid_earth_tide) 
  pole_tide <- ncvar_get(nc,nc$var$pole_tide) 
  alt_state_flag_ku_band_status <- ncvar_get(nc,nc$var$alt_state_flag_ku_band_status)
  lat_20hz <- ncvar_get(nc,nc$var$lat_20hz) 
  alt_20hz <- ncvar_get(nc,nc$var$alt_20hz)
  lon_20hz <- ncvar_get(nc,nc$var$lon_20hz) 
  ice_range_20hz_ku <- ncvar_get(nc,nc$var$ice_range_20hz_ku) 
  ice_sig0_20hz_ku <- ncvar_get(nc,nc$var$ice_sig0_20hz_ku) 
  ice_qual_flag_20hz_ku <- ncvar_get(nc,nc$var$ice_qual_flag_20hz_ku)
  ######################################################################
  
  for (i in 1:para_dim){
    if (lat[i]<61.90 | lat[i]>62.00) {
      next
    }
    if(model_dry_tropo_corr[i] == 32767 & model_wet_tropo_corr[i] == 32767){
      next
    }
    if(iono_corr_gim_ku[i] == 32767){
      next
    }
    if(solid_earth_tide[i] == 32767 & pole_tide[i] == 32767 ){
      next
    }
    if(alt_state_flag_ku_band_status[i] != 0){
      next
    }
    media_corr <- model_dry_tropo_corr[i] +model_wet_tropo_corr[i] +iono_corr_gim_ku[i] +solid_earth_tide[i] + pole_tide[i]
    
    for (j in 1:para_hz){
      if(lat_20hz[j,i] == 2147.483648){
        next
      }
      mjd_20hz <- time_20hz[j,i]*para_today + 51544
      icehgt_20hz <- alt_20hz[j,i] - (media_corr + ice_range_20hz_ku[j,i])
      outp <- c(cycnum, mjd_20hz, lon_20hz[j,i], lat_20hz[j,i], icehgt_20hz, ice_sig0_20hz_ku[j,i],
                ice_qual_flag_20hz_ku[j,i])
      df_out <- rbind(df_out,outp)
    }  
  }
  colnames(df_out) <- c('cycnum', 'mjd_20hz', 'lon_20hz', 'lat_20hz', 'icehgt_20hz', 'ice_sig0_20hz_ku',
                      'ice_qual_flag_20hz_ku')
}  

# Specify decimals
df_out[,'mjd_20hz'] = format(round(df_out[,'mjd_20hz'],6),nsmall=6)
df_out[,'lon_20hz'] = format(round(df_out[,'lon_20hz'],6),nsmall=6)
df_out[,'lat_20hz'] = format(round(df_out[,'lat_20hz'],6),nsmall=6)
df_out[,'icehgt_20hz'] = format(round(df_out[,'icehgt_20hz'],6),nsmall=6)
df_out[,'ice_sig0_20hz_ku'] = format(round(df_out[,'ice_sig0_20hz_ku'],6),nsmall=6)

# Write dataframe to text tab-seperated file
outtxt <- paste(fdir, fout, sep='/')
write.table(df_out, file=outtxt, sep = "\t",
            quote = FALSE, row.names = FALSE, col.names=FALSE)
    
