;*************************************************
; Plot_supp_fig2_composite_maps.ncl
;
; Input NSIDC SIC
; Input 100-200 m Argo ocean temperature from Ed
; Plot composite maps for different periods
;
; AFP 15 March 2023
;************************************************
begin

;************************************************
; Plot resources
;************************************************
  res                      = True     				
  res@gsnDraw              = False               		
  res@gsnFrame             = False       
  res@gsnAddCyclic	   = True        		

  res@txFontHeightF        = 0.025
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

  res@cnMaxLevelValF 	   = 0.5
  res@cnMinLevelValF 	   = -0.5
  res@cnLevelSpacingF 	   = (res@cnMaxLevelValF - res@cnMinLevelValF)/20
  res@gsnSpreadColorStart  = 24
  res@gsnSpreadColorEnd    = 45

  res@lbLabelBarOn	   = False

  res@gsnPolar 		   = "SH"
  res@mpMaxLatF		   = -50
  res@gsnPolarLabelSpacing = 120
  res@gsnPolarLabelFontHeightF=0.000001
  res@gsnPolarLabelDistance= 1.  

  ; SIC resources
  resSic		  = True
  resSic@gsnTickMarksOn   = False
  resSic@gsnDraw 	  = False
  resSic@gsnFrame	  = False

  resSic@cnInfoLabelOn 	  = False

  resSic@cnFillOn         = True     				
  resSic@cnLinesOn        = False    				
  resSic@gsnSpreadColors  = True     				
  resSic@cnLineLabelsOn   = False			        
  resSic@cnLevelSelectionMode = "ManualLevels"      		
  resSic@cnFillDrawOrder  = "PreDraw"  			

  resSic@cnMaxLevelValF    = 20.
  resSic@cnMinLevelValF    = -20.
  resSic@cnLevelSpacingF   = (resSic@cnMaxLevelValF - resSic@cnMinLevelValF)/20
  resSic@gsnSpreadColorStart = 2
  resSic@gsnSpreadColorEnd = 23

  resSic@lbLabelBarOn	   = False

  ; Contour resources
  res2 			   = True
  res2@gsnTickMarksOn      = False
  res2@gsnDraw		   = False
  res2@gsnFrame		   = False
  res2@cnLevelSelectionMode= "ExplicitLevels" 			
  res2@cnInfoLabelOn       = False       			
  res2@cnLineLabelsOn      = False      			
  res2@cnMonoLineColor     = True
  res2@cnLineThicknessF    = 1.
  res2@gsnContourZeroLineThicknessF = 3.	    	
  res2@gsnContourNegLineDashPattern = 1      
  res2@cnLevels 	   = fspan(-10, 10, 21)

  ; Stippling 
  resSig 		     = True
  resSig@gsnDraw             = False               		
  resSig@gsnFrame            = False  
  resSig@gsnAddCyclic	     = True             		
  resSig@gsnTickMarksOn      = False
  resSig@cnLevelSelectionMode= "ManualLevels" 			
  resSig@cnMinLevelValF      = -1.05        			
  resSig@cnMaxLevelValF      = 1.05        			
  resSig@cnLevelSpacingF     = 0.01        			
  resSig@cnInfoLabelOn       = False       			
  resSig@cnLinesOn           = False       			
  resSig@cnLineLabelsOn      = False       			
  resSig@cnFillScaleF        = 0.8       			
  resSig@cnFillDotSizeF	     = 0.0023 

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
  wks = gsn_open_wks("pdf", "figures/SIC_temp_anom_comp_230705c")
  setvalues NhlGetWorkspaceObjectId()
	"wsMaximumSize":1000000000
  end setvalues
  res@cnFillPalette = colourMap
  resSic@cnFillPalette = colourMap
  plot = new(4, graphic)
  plot2 = plot
  plot3 = plot  
  plot4 = plot
  plot5 = plot

;*************************************************
; Input obs and process
;*************************************************
  ; Input SIC
  in = addfile("/g/data/e14/afp599/data/NSIDC/NSIDC_CDR_SH_v4_197811-202305_230626.nc", "r")
  sic = in->sic_cdr
  latitude = in->latitude
  longitude = in->longitude
  sicTime = in->time
  yyyymm = cd_calendar(sicTime, -1)
  delete(in)
  
  ; Calculate SIC climatology
  sicClim = clmMonTLL(sic(2:529, :, :)) ; 1979-2022

  ; Calculate SIC anomalies
  sicAnomAll = new(dimsizes(sic), float)
  sicAnomAll(:1,:,:) = sic(:1,:,:) - sicClim(10:,:,:)		  ; Anomalies for Nov-Dec 1978
  sicAnomAll(2:529,:,:) = calcMonAnomTLL(sic(2:529,:,:), sicClim) ; Anomalies for 1979-2022
  sicAnomAll(530:534,:,:) = sic(530:534,:,:) - sicClim(:4,:,:)    ; Anomalies for Jan-May 2023

  ; Composite three periods
  sicAnom = new((/3, 332, 316/), float)
  sicAnom(0,:,:) = dim_avg_n_Wrap(sicAnomAll(302:348, :, :), 0)   ; Period 1 - Jan 2004 to Aug 2007 - same as Argo
  sicAnom(1,:,:) = dim_avg_n_Wrap(sicAnomAll(349:445, :, :), 0)   ; Period 2 - Sept 2007 to Aug 2016
  sicAnom(2,:,:) = dim_avg_n_Wrap(sicAnomAll(446:, :, :), 0)      ; Period 3 - Sept 2016 to May 2023

  copy_VarCoords(sic(0:2, :, :), sicAnom)
  sicAnom@lat2d = latitude
  sicAnom@lon2d = longitude
  delete(sicAnom@long_name)
  delete(sicAnom@standard_name)
  delete(sicAnom@units)

  ; Assess SIC significance
  ave2 = dim_avg_n(sicAnomAll(:348,:,:), 0) 			  ; Full period 1
  var2 = dim_variance_n(sicAnomAll(:348,:,:), 0)
  s2 = 348+1

  ave3 = dim_avg_n(sicAnomAll(349:445,:,:), 0)			  ; Period 2
  var3 = dim_variance_n(sicAnomAll(349:445,:,:), 0)
  s3 = 445-349+1

  ave4 = dim_avg_n(sicAnomAll(446:,:,:), 0)		          ; Period 3
  var4 = dim_variance_n(sicAnomAll(446:,:,:), 0)
  s4 = 534-446+1

  pval2 = ttest(ave2, var2, s2, ave3, var3, s3, False, False)
  pval3 = ttest(ave2, var2, s2, ave4, var4, s4, False, False)

  sicSignif = new(dimsizes(sicAnom), float)
  sicSignif(1,:,:) = 1-pval2
  sicSignif(2,:,:) = 1-pval3
  copy_VarCoords(sicAnom, sicSignif)

  ; Input ocean temperature
  in = addfile("data/from_Ed_230703/argo_depth_averaged_anom_binned-100-200m_NaNcorrected.nc", "r")
  tempAnom = in->ARGO_TEMPERATURE_ANOMALY
  tempTime = in->TIME_bins

  printVarSummary(tempAnom)
  tempAnom!1 = "latitude"
  tempAnom!2 = "longitude"
  tempAnom&latitude@units = "degrees_north"
  tempAnom&longitude@units = "degrees_east"
  delete(in)

  in = addfile("data/from_Ed_230703/argo_depth_averaged_anom_binned_for_reviewer-100-200m_NaNcorrected.nc", "r")
  tempAnom2 = in->ARGO_TEMPERATURE_ANOMALY
  copy_VarCoords(tempAnom, tempAnom2)
  delete(in)

  ; Input ocean signif
  in = addfile("data/from_Ed_230621/argo_temp_ttest_pre2007_to_2007_2015_NaNcorrected.nc", "r")
  tempPval1 = in->pvalues
  copy_VarCoords(tempAnom(0,:,:), tempPval1)
  tempSignif1 = 1 - tempPval1
  delete(in)

  in = addfile("data/from_Ed_230621/argo_temp_ttest_pre2007_to_2015_2023_NaNcorrected.nc", "r")
  tempPval2 = in->pvalues
  copy_VarCoords(tempAnom(0,:,:), tempPval2)
  tempSignif2 = 1 -tempPval2
  delete(in)

  sicSignif@lat2d = latitude
  sicSignif@lon2d = longitude

  copy_VarCoords(tempAnom(0,:,:), tempSignif1)
  copy_VarCoords(tempAnom(0,:,:), tempSignif2)

  ; Extra period as suggested by the reviewer
  in = addfile("data/from_Ed_230621/argo_temp_ttest_pre2007_to_2013_2015_NaNcorrected.nc", "r")
  newPval = in->pvalues
  copy_VarCoords(tempAnom(0,:,:), newPval)
  newSignif = 1 - newPval
  copy_VarCoords(tempAnom(0,:,:), newSignif)

;*************************************************
; Plot obs
;*************************************************  
  resSic@tiYAxisString = ""
  resSic@tiXAxisString = ""
  resSic2@tiYAxisString = ""
  resSic2@tiXAxisString = ""

  ; Significance levels
  numOceanPts = 15*360
  sicClimAv = dim_avg_n(sicClim, 0)
  sicClimAv = where(sicClimAv.eq.0, sicClimAv@_FillValue, sicClimAv)
  numIcePts = num(.not.ismissing(sicClimAv))
  print("numOceanPts = " + numOceanPts)
  print("numIcePts = " + numIcePts)

  tempSigLevel = 1 - (0.05 / numOceanPts)
  sicSigLevel = 1 - (0.05 / numIcePts)

  ; Mask highest lats of data in panels (b) and (c) because data doesn't cover full period
  tempAnom(1,:,:) = where(ismissing(tempAnom(0,:,:)), tempAnom@_FillValue, tempAnom(1,:,:))
  tempAnom2(1,:,:) = where(ismissing(tempAnom(0,:,:)), tempAnom2@_FillValue, tempAnom2(1,:,:))
  
  ; For sigLevel = 0.05/15/260
  tempSignif1 = where(tempSignif1.gt.tempSigLevel, 0.95, 0.)
  tempSignif2 = where(tempSignif2.gt.tempSigLevel, 0.95, 0.)
  newSignif = where(newSignif.gt.tempSigLevel, 0.95, 0.)
  sicSignif = where(sicSignif.gt.tempSigLevel, 0.95, 0.) ; use tempSigLevel instead - as if sea ice was also on a 1x1 grid

  res@gsnCenterString = "~F22~a~F21~ Period 1 (Jan 2004 to Aug 2007)"
  plot(0) = gsn_csm_contour_map_polar(wks, tempAnom(0,:,:), res)
  plot2(0) = gsn_csm_contour(wks, sicAnom(0,:,:), resSic)
  overlay(plot(0), plot2(0))

  res@gsnCenterString = "~F22~b~F21~ Period 2 (Sep 2007 to Aug 2016)"
  plot(1) = gsn_csm_contour_map_polar(wks, tempAnom(1,:,:), res)
  plot2(1) = gsn_csm_contour(wks, sicAnom(1,:,:), resSic)
  overlay(plot(1), plot2(1))

  ; Overlay temp stippling
  plot4(1) = gsn_csm_contour(wks, tempSignif1, resSig)
  plot4(1) = ShadeGtContour(plot4(1), 0.94, 17)
  overlay(plot(1), plot4(1))

  ; Overlay sic stippling
  plot5(1) = gsn_csm_contour(wks, sicSignif(1,:,:), resSig)
  plot5(1) = ShadeGtContour(plot5(1), 0.94, 17)
  overlay(plot(1), plot5(1))

  res@gsnCenterString = "~F22~c~F21~ Ocean warming Sep 2014 to Aug 2016"
  plot(2) = gsn_csm_contour_map_polar(wks, tempAnom2(1,:,:), res)
  ; Don't plot SIC for this panel

  ; Overlay temp stippling
  plot4(2) = gsn_csm_contour(wks, newSignif, resSig)
  plot4(2) = ShadeGtContour(plot4(2), 0.94, 17)
  overlay(plot(2), plot4(2))

  res@gsnCenterString = "~F22~d~F21~ Period 3 (Sep 2016 to May 2023)"
  plot(3) = gsn_csm_contour_map_polar(wks, tempAnom(2,:,:), res)
  plot2(3) = gsn_csm_contour(wks, sicAnom(2,:,:), resSic)
  overlay(plot(3), plot2(3))

  ; Overlap temp stippling
  plot4(3) = gsn_csm_contour(wks, tempSignif2, resSig)
  plot4(3) = ShadeGtContour(plot4(3), 0.94, 17)
  overlay(plot(3), plot4(3))

  ; Overlap sic stippling
  plot5(3) = gsn_csm_contour(wks, sicSignif(2,:,:), resSig)
  plot5(3) = ShadeGtContour(plot5(3), 0.94, 17)
  overlay(plot(3), plot5(3))

;*************************************************
; Panel plots
;*************************************************
  panel_res 		     = True
  panel_res@gsnFrame         = False 
  panel_res@gsnMaximize	     = True              		
  panel_res@gsnPanelRowSpec  = True
  panel_res@gsnPanelCenter   = False			
  panel_res@gsnPanelYWhiteSpacePercent = 2
  panel_res@gsnPanelXWhiteSpacePercent = 2
  panel_res@gsnPanelLabelBar = True
  panel_res@lbLabelFontHeightF= 0.013
  panel_res@lbLabelFont      = 21
  panel_res@lbLabelStride    = 1
  panel_res@lbBoxLineThicknessF= 0.7
  panel_res@lbBoxSeparatorLinesOn= False
  panel_res@pmLabelBarHeightF = 0.065
  panel_res@pmLabelBarWidthF = 0.5

  panel_res@gsnPanelBottom = 0.07
  
  gsn_panel(wks, plot, (/2,2/), panel_res)

  txres = True
  txres@txFont = 21
  txres@txFontHeightF = 0.013
  txres@txJust = "CenterCenter"
  gsn_text_ndc(wks, "Sea ice concentration anomaly (%)", 0.5, 0.01, txres)
  gsn_text_ndc(wks, "100-200 m temperature anomaly (~S~o~N~C)", 0.5, 0.094, txres)

  ; Add SIC colour bar
  sicStr = flt2string( fspan(-10, 10, 21) )
  labelColours = colourMap(2:23,:)
  lbres = True
  lbres@lbAutoManage = False
  lbres@vpWidthF = 0.5
  lbres@vpHeightF = 0.065
  lbres@lbBoxSeparatorLinesOn = False
  lbres@lbBoxLineThicknessF = 0.7
  lbres@lbMonoFillPattern = True
  lbres@lbFillColors = labelColours
  lbres@lbOrientation = "Horizontal"
  lbres@lbLabelFontHeightF = 0.013
  lbres@lbLabelStride = 2
  lbres@lbLabelAlignment = "InteriorEdges"
  gsn_labelbar_ndc(wks, 22, sicStr, 0.25, 0.08, lbres)
  
  frame(wks)  
  delete([/wks, plot/])	
end
