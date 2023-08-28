;*************************************************
; Plot_supp_fig3_ekman_maps.ncl
;
; Input ERA5 wind stresses
; Calculate Ekman pumping
; Save > regrid with CDO > input
; Plot Ekman pumping trend maps
;
; AFP 18 May 2023
;************************************************
begin

;************************************************
; Plot resources
;************************************************
  res                      = True     				
  res@gsnDraw              = False               		
  res@gsnFrame             = False       
  res@gsnAddCyclic	   = True        		

  res@txFontHeightF        = 0.02
  res@tmXBLabelFontHeightF = 0.03	 			
  res@tmYLLabelFontHeightF = 0.03
  res@tiXAxisFontHeightF   = 0.03	 			
  res@tiYAxisFontHeightF   = 0.03
  res@txFont		   = 21
  res@tmXBLabelFont 	   = 21
  res@tmYLLabelFont 	   = 21
  res@tiXAxisFont   	   = 21
  res@tiYAxisFont   	   = 21

  res@cnFillOn             = True     				
  res@cnLinesOn            = False    				
  res@gsnSpreadColors      = True     				
  res@cnLineLabelsOn       = False			        
  res@cnLevelSelectionMode = "ManualLevels"      		
  res@cnFillDrawOrder      = "PreDraw"  			
  res@mpFillAreaSpecifiers = (/"Land", "Water"/)			
  res@mpSpecifiedFillColors= (/"grey", "transparent"/) 

  res@cnMaxLevelValF 	   = 0.004
  res@cnMinLevelValF	   = -0.004
  res@cnLevelSpacingF = (res@cnMaxLevelValF - res@cnMinLevelValF)/20
  res@gsnSpreadColorStart  = 24
  res@gsnSpreadColorEnd    = 45

  res@lbLabelBarOn	   = False

  res@gsnPolar 		   = "SH"
  res@mpMaxLatF		   = -50
  res@gsnPolarLabelSpacing = 120
  res@gsnPolarLabelFontHeightF=0.000001
  res@gsnPolarLabelDistance= 1.  

  colourMap = (/ (/1, 1, 1, 1/),\ ; green-magenta - swap colour order in resSic and sic labelbar
(/0, 0, 0, 1/),\
(/0, 0.31372549, 0, 1/),\
(/0, 0.419607843, 0, 1/),\
(/0, 0.525490196, 0, 1/),\
(/0, 0.63, 0, 1/),\
(/0, 0.733333333, 0, 1/),\
(/0, 0.84, 0, 1/),\
(/0, 0.945098039, 0, 1/),\
(/0.31372549, 1, 0.31372549, 1/),\
(/0.525490196, 1, 0.525490196, 1/),\
(/0.733333333, 1, 0.733333333, 1/),\
(/1, 1, 1, 0/),\
(/1, 1, 1, 0/),\
(/1, 0.733333333, 1, 1/),\
(/1, 0.525490196, 1, 1/),\
(/1, 0.31372549, 1, 1/),\
(/0.945098039, 0, 0.945098039, 1/),\
(/0.84, 0, 0.84, 1/),\
(/0.733333333, 0, 0.733333333, 1/),\
(/0.63, 0, 0.63, 1/),\
(/0.525490196, 0, 0.525490196, 1/),\
(/0.419607843, 0, 0.419607843, 1/),\
(/0.31372549, 0, 0.31372549, 1/),\
(/0.176470588, 0, 0.784313725, 1/),\ ; red-blue
(/0.141176471, 0, 0.847058824, 1/),\
(/0.117647059, 0, 0.901960784, 1/),\
(/0.094117647, 0.109803922, 0.968627451, 1/),\
(/0.156862745, 0.341176471, 1, 1/),\
(/0.239215686, 0.529411765, 1, 1/),\
(/0.337254902, 0.690196078, 1, 1/),\
(/0.458823529, 0.82745098, 1, 1/),\
(/0.6, 0.917647059, 1, 1/),\
(/0.737254902, 0.976470588, 1, 1/),\
(/1, 1, 1, 1/),\
(/1, 1, 1, 1/),\
(/1, 0.945098039, 0.737254902, 1/),\
(/1, 0.839215686, 0.6, 1/),\
(/1, 0.674509804, 0.458823529, 1/),\
(/1, 0.470588235, 0.337254902, 1/),\
(/1, 0.239215686, 0.239215686, 1/),\
(/0.968627451, 0.152941176, 0.207843137, 1/),\
(/0.847058824, 0.082352941, 0.184313725, 1/),\
(/0.784313725, 0, 0.156862745, 1/),\
(/0.647058824, 0, 0.129411765, 1/),\
(/0.588235294, 0, 0.066666667, 1/) /)

;*************************************************
; Plot set up and new figure
;*************************************************
  wks = gsn_open_wks("pdf", "figures/ERA5_wind_stress_trend_maps_230529_2panel_regriddedv2")
  setvalues NhlGetWorkspaceObjectId()
	"wsMaximumSize":1000000000
  end setvalues
  res@cnFillPalette = colourMap
  plot = new(2, graphic)

;*************************************************
; Input obs and process
;*************************************************
  outputEkman = False
  if (outputEkman)
  	; Input wind stress data
  	in = addfile("/g/data/e14/afp599/data/ERA5/ewss_era5_moda_sfc_197801-202212.nc", "r")
  	tauu = in->ewss(12:,:,:) ; cut off 1978
  	lat = in->latitude 
  	delete(in)

  	in = addfile("/g/data/e14/afp599/data/ERA5/nsss_era5_moda_sfc_197801-202212.nc", "r")
  	tauv = in->nsss(12:,:,:) ; cut off 1978
  	delete(in)

  	; Annually average
  	tauuAv = month_to_annual(tauu, 1)
  	tauvAv = month_to_annual(tauv, 1)
  	curlAv = uv2vrF(tauuAv, tauvAv)
  	copy_VarCoords(tauu(0,:,:), curlAv(0,:,:))

  	f = coriolis_param(lat)
  	fconform = conform_dims(dimsizes(curlAv), f, 1)
  	rhof = 1025. * fconform
  	rhof = where(rhof.eq.0, rhof@_FillValue, rhof)
  	wE = curlAv / rhof

  	copy_VarCoords(tauu(0,:,:), wE(0,:,:))
  	wE!0 = "year"

  	; Output curl
  	saveFile = addfile("/g/data/e14/afp599/data/ERA5/wE_era5_moda_sfc_197901-202212.nc", "c")
 	dimNames = (/"year", "lat", "lon"/)
  	dimSizes = (/44, 721, 1440/)
  	dimUnlim = (/True, False, False/)
  	filedimdef(saveFile, dimNames, dimSizes, dimUnlim)
  	saveFile@title = "wE calculated by A. Purich"
 	saveFile@date = systemfunc("date")
  	saveFile->wE = wE
  end if

  ; Input regridded curl
  in = addfile("/g/data/e14/afp599/data/ERA5/wE_era5_moda_sfc_197901-202212_r360x180.nc", "r")
  wE = in->wE
  lat = in->lat
  delete(in)

  yyyy = ispan(1979, 2022, 1)

  ; Calculate trends over two periods
  indA = ind(yyyy.ge.2004 .and. yyyy.le.2022)
  indB = ind(yyyy.ge.1979 .and. yyyy.le.2022)

  rcA = regCoef_n(yyyy(indA), wE(indA,:,:), 0, 0)
  rcB = regCoef_n(yyyy(indB), wE(indB,:,:), 0, 0)
  
  copy_VarCoords(wE(0,:,:), rcA)
  copy_VarCoords(wE(0,:,:), rcB)

  wEA = rcA
  wEB = rcB

  copy_VarCoords(wE(0,:,:), wEA)
  copy_VarCoords(wE(0,:,:), wEB)

;*************************************************
; Plot obs
;*************************************************  
  res@gsnCenterString = "~F22~a~F21~ Ekman pumping trend 2004-2022 (m s~S~-1~N~ y~S~-1~N~)"
  plot(0) = gsn_csm_contour_map_polar(wks, wEA, res)

  res@gsnCenterString = "~F22~b~F21~ Ekman pumping trend 1979-2022 (m s~S~-1~N~ y~S~-1~N~)"
  plot(1) = gsn_csm_contour_map_polar(wks, wEB, res)

;*************************************************
; Panel plots
;*************************************************
  panel_res 		     = True
  panel_res@gsnFrame         = False 
  panel_res@gsnMaximize	     = True              		
  panel_res@gsnPanelRowSpec  = True
  panel_res@gsnPanelCenter   = True			
  panel_res@gsnPanelYWhiteSpacePercent = 5
  panel_res@gsnPanelXWhiteSpacePercent = 5
  panel_res@gsnPanelLabelBar = True
  panel_res@lbLabelFontHeightF= 0.015
  panel_res@lbLabelFont      = 21
  panel_res@lbLabelStride    = 1
  panel_res@lbBoxLineThicknessF= 0.7
  panel_res@lbBoxSeparatorLinesOn= False
  panel_res@pmLabelBarHeightF = 0.065
  panel_res@pmLabelBarWidthF = 0.5
  panel_res@gsnPanelBottom = 0.07
  
  gsn_panel(wks, plot, (/2/), panel_res)
  
  frame(wks)  
  delete([/wks, plot/])	
end
