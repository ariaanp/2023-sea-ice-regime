;*************************************************
; Plot_fig3_hovmullers.ncl
;
; Plot longitudinal SIE anomaly hovmuller
; SIE anomalies calculated wrt 1979-2022
; Plot 0-100 m and 100-200 m 50-65S ocean temp anomaly hovmuller
; Ocean temp anomalies calculated wrt 2004-2002
; Add zonal mean stripes at the bottom of each panel
; Overlay sea ice contours for direct comparisons
;
; AFP 15 May 2023
;************************************************
begin

;************************************************
; Plot resources
;************************************************
  res                      = True     				
  res@gsnDraw              = False               		
  res@gsnFrame             = False              		

  res@txFontHeightF        = 0.025
  res@tmXBLabelFontHeightF = 0.025	 			
  res@tmYLLabelFontHeightF = 0.025
  res@tiXAxisFontHeightF   = 0.025	 			
  res@tiYAxisFontHeightF   = 0.025
  res@txFont		   = 21
  res@tmXBLabelFont 	   = 21
  res@tmYLLabelFont 	   = 21
  res@tiXAxisFont   	   = 21
  res@tiYAxisFont   	   = 21

  res@tmBorderThicknessF   = 0.7
  res@tmXBMajorThicknessF  = 0.7
  res@tmYLMajorThicknessF  = 0.7
  res@tmXBMinorThicknessF  = 0.7
  res@tmYLMinorThicknessF  = 0.7

  res@cnFillOn             = True     				
  res@cnLinesOn            = False    				
  res@gsnSpreadColors      = True     				
  res@cnLineLabelsOn       = False			        
  res@cnLevelSelectionMode = "ManualLevels"      		
  res@cnFillDrawOrder      = "PreDraw"  			

  res@cnMaxLevelValF 	   = 0.5
  res@cnMinLevelValF 	   = -0.5
  res@cnLevelSpacingF	   = (res@cnMaxLevelValF - res@cnMinLevelValF)/20
  res@gsnSpreadColorStart  = 24
  res@gsnSpreadColorEnd    = 45

  res@lbLabelBarOn	   = True
  res@lbOrientation 	   = "Vertical"
  res@lbBoxLineThicknessF  = 0.7
  res@lbBoxSeparatorLinesOn= False

  res@vpWidthF 		   = 1.
  res@vpHeightF 	   = 0.5

  res@tmYLMode 		   = "Explicit"
  res@tmYLValues 	   = (/0, 90, 180, 270, 359/)
  res@tmYLMinorValues 	   = (/30, 60, 120, 150, 210, 240, 300, 330/)
  res@tmYLLabels 	   = (/"0~S~o~N~", "90~S~o~N~E", "180~S~o~N~E", "270~S~o~N~E", "360~S~o~N~"/)

  ; SIC resources
  resSic		  = res	

  resSic@cnMaxLevelValF    = 1.
  resSic@cnMinLevelValF    = -1.
  resSic@cnLevelSpacingF   = (resSic@cnMaxLevelValF - resSic@cnMinLevelValF)/20
  resSic@gsnSpreadColorStart = 2
  resSic@gsnSpreadColorEnd = 23

  resSic@lbLabelBarOn	   = True

  ; Stripes resources
  resZm = res
  resZm@vpHeightF = 0.05
  resZm@lbLabelBarOn = False
  resZm@cnInfoLabelOn = False
  delete([/resZm@tmYLValues, resZm@tmYLLabels/])
  resZm@tmYLValues = 0.5
  resZm@tmYLLabels = "Zonal~C~mean"

  resSicZm = resSic
  resSicZm@vpHeightF = 0.05
  resSicZm@lbLabelBarOn = False
  resSicZm@cnInfoLabelOn = False
  delete([/resSicZm@tmYLValues, resSicZm@tmYLLabels/])
  resSicZm@tmYLValues = 0.5
  resSicZm@tmYLLabels = "Zonal~C~mean"

  ; Now turn main SIE and temp year labels off
  res@tmXBLabelsOn = False
  resSic@tmXBLabelsOn = False

  ; Contour resources
  res2 			   = True
  res2@gsnTickMarksOn      = False
  res2@gsnDraw		   = False
  res2@gsnFrame		   = False
  res2@cnLevelSelectionMode= "ExplicitLevels" 			
  res2@cnInfoLabelOn       = False       			
  res2@cnLineLabelsOn      = False      			
  res2@cnMonoLineColor     = True
  res2@cnLineThicknessF    = 2
  res2@cnLevels 	   = (/-0.2/)

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
  wks = gsn_open_wks("pdf", "figures/SIC_temp_anom_hov_230626")
  setvalues NhlGetWorkspaceObjectId()
	"wsMaximumSize":1000000000
  end setvalues
  res@cnFillPalette = colourMap
  resSic@cnFillPalette = colourMap
  resZm@cnFillPalette = colourMap
  resSicZm@cnFillPalette = colourMap
  plot = new(6, graphic)
  plot2 = plot
  plot3 = plot  
  plot4 = plot
  plot5 = plot

;*************************************************
; Input obs and process
;*************************************************
  ; Input SIC
  in = addfile("/g/data/e14/afp599/data/NSIDC/NSIDC_CDR_SH_v4_197811-202305_230626_r360x180.nc", "r")
  sic = in->sic_cdr
  sicTime = in->time
  yyyymm = cd_calendar(sicTime, -1)
  sicyyyy = cd_calendar(sicTime, -1)/100 
  delete(in)

  ; Input ocean temp
  in = addfile("data/from_Ed_230621/argo_temp_anom_0-100m_depth_NaNcorrected.nc", "r")
  tempAnom = in->ARGO_TEMPERATURE_ANOMALY
  tempTime = in->TIME
  delete(in)

  in = addfile("data/from_Ed_230621/argo_temp_anom_100-200m_depth_NaNcorrected.nc", "r")
  tempAnom2 = in->ARGO_TEMPERATURE_ANOMALY
  copy_VarCoords(tempAnom, tempAnom2)
  delete(in)
  
  ; Calculate SIC climatology
  sicClim = clmMonTLL(sic(2:529, :, :)) ; 1979-2022

  ; Calculate SIC anomalies
  sicAnomAll = new(dimsizes(sic), float)
  sicAnomAll(:1,:,:) = sic(:1,:,:) - sicClim(10:,:,:)
  sicAnomAll(2:529,:,:) = calcMonAnomTLL(sic(2:529,:,:), sicClim)
  sicAnomAll(530:534,:,:) = sic(530:534,:,:) - sicClim(:4,:,:)

  ; Calculate SIE by longitude
  pi = 4.*atan(1.)
  rad = pi/180.
  rE = 6371 			; [km] 
  aE = 4*pi*rE^2 		; [km^2]
  lat = fspan(-89.5, 89.5, 180) ; data regridded to 360x180
  lon = fspan(0, 359, 360) 	; data regridded to 360x180
  dxeq = (lon(2) - lon(1))*rE*rad 						
  dx = dxeq*cos(lat*rad) 								
  dy = new(dimsizes(lat), typeof(lat))
  dy(0) = (90-abs((lat(1)+lat(0))*0.5))*rE*rad 
  dy(dimsizes(lat)-1)= dy(0)
  dy(1:dimsizes(lat)-2) = abs(lat(2:dimsizes(lat)-1)-lat(1:dimsizes(lat)-2))*rE*rad 
  area = dx*dy 	
  area!0 = "lat"
  area&lat = lat							
  delete([/lat, lon, dxeq, dx, dy/])
  cellArea = conform_dims( dimsizes(sic), area, 1)
  cellArea15 = tofloat( where(sic .gt. 15, cellArea, 0) )
  copy_VarMeta(sic, cellArea15)
  sie = dim_sum_n_Wrap(cellArea15(:, {-89.5:-40.5}, :), (/1/)) ; Antarctic
  sie3D = new((/dimsizes(sie&time), dimsizes(sie&lon), 1/), float) ; add extra dimension form clmMonTLL and calcMonAnomTLL
  sie3D(:,:,0) = sie
  
  ; Calculate SIE climatology
  sieInd = ind(sicyyyy.ge.1979 .and. sicyyyy.le.2022)
  sieClim = clmMonTLL(sie3D(sieInd,:,:))
  delete([/sieInd/])

  ; Calculate SIE anomalies
  sieInd = ind(sicyyyy.ge.2004 .and. sicyyyy.le.2022)
  sieInd2023 = ind(sicyyyy.ge.2023)
  sieAnom3D = calcMonAnomTLL(sie3D(sieInd,:,:), sieClim) ; 3D
  sieAnom2023 = sie3D(sieInd2023,:,:) - sieClim(0:4,:,:)
  sieAnom = array_append_record(sieAnom3D(:,:,0), sieAnom2023(:,:,0), 0)
  sieTime = sicTime(sieInd(0):)
  sieAnom&time = sieTime
  delete(sieAnom@long_name)
  delete(sieAnom@standard_name)
  delete(sieAnom@units)

  delete(yyyymm)
  yyyymm = cd_calendar(sieTime, -1)
  yyyyfrac = yyyymm_to_yyyyfrac(yyyymm, 0.5)

  yyyyfrac(0) = 2004.

  sieAnom!0 = "year"
  sieAnom&year = yyyyfrac
  
  tempAnom!0 = "year"
  tempAnom&year = yyyyfrac

  tempAnom2!0 = "year"
  tempAnom2&year = yyyyfrac

  sieAnom = sieAnom / 1e4

  tempAnom = lonPivot(tempAnom, 360.5)
  tempAnom2 = lonPivot(tempAnom2, 360.5)
  
  delete(tempAnom&LONGITUDE)
  tempAnom!1 = "lon"
  tempAnom&lon = sieAnom&lon
  delete(tempAnom2&LONGITUDE)
  tempAnom2!1 = "lon"
  tempAnom2&lon = sieAnom&lon

  sieAnomRaw = sieAnom
  tempAnomRaw = tempAnom
  tempAnomRaw2 = tempAnom2

  ; Smoothing
  ; Lon first
  sieAnomSm = runave_n_Wrap(sieAnom, 20, 1, 1)
  tempAnomSm = runave_n_Wrap(tempAnom, 20, 1, 1)
  tempAnomSm2 = runave_n_Wrap(tempAnom2, 20, 1, 1)

  ; Now time
  sieAnomSmSm = runave_n_Wrap(sieAnomSm, 12, 0, 0)
  tempAnomSmSm = runave_n_Wrap(tempAnomSm, 12, 0, 0)
  tempAnomSmSm2 = runave_n_Wrap(tempAnomSm2, 12, 0, 0)

  ; Replace main fields with smooth - note this is before zonal averaging here!
  sieAnom = sieAnomSmSm
  tempAnom = tempAnomSmSm
  tempAnom2 = tempAnomSmSm2

  ; Zonal means
  sieAnomZm = dim_avg_n_Wrap(sieAnomRaw, 1) ; use raw (not replaced)
  tempAnomZm = dim_avg_n_Wrap(tempAnomRaw, 1)
  tempAnomZm2 = dim_avg_n_Wrap(tempAnomRaw2, 1)

  sieZm = new((/2, 233/), float)
  sieZm(0,:) = sieAnomZm
  sieZm(1,:) = sieAnomZm

  tempZm = new((/2, 233/), double)
  tempZm(0,:) = tempAnomZm
  tempZm(1,:) = tempAnomZm
  
  tempZm2 = new((/2, 233/), double)
  tempZm2(0,:) = tempAnomZm2
  tempZm2(1,:) = tempAnomZm2
 
;*************************************************
; Plot obs
;*************************************************  
  resSic@gsnLeftString = "~F22~a~F21~ Antarctic monthly sea ice extent anomaly (x10~S~4~N~ km~S~2~N~ (~S~o~N~ lon)~S~-1~N~)"
  resSic@tiXAxisString = ""
  resSic@tiYAxisString = ""
  plot(0) = gsn_csm_contour(wks, sieAnom(lon|:, year|:), resSic)
  plot3(0) = gsn_csm_contour(wks, sieAnom(lon|:, year|:), res2)
  overlay(plot(0), plot3(0))

  res@gsnLeftString = "~F22~b~F21~ Southern Ocean 50-65~S~o~N~S 0-100 m temperature anomaly (~S~o~N~C)"
  res@tiXAxisString = ""
  res@tiYAxisString = ""
  plot(2) = gsn_csm_contour(wks, tempAnom(lon|:, year|:), res)
  plot3(2) = gsn_csm_contour(wks, sieAnom(lon|:, year|:), res2)
  overlay(plot(2), plot3(2))

  res@gsnLeftString = "~F22~c~F21~ Southern Ocean 50-65~S~o~N~S 100-200 m temperature anomaly (~S~o~N~C)"
  plot(4) = gsn_csm_contour(wks, tempAnom2(lon|:, year|:), res)
  plot3(4) = gsn_csm_contour(wks, sieAnom(lon|:, year|:), res2)
  overlay(plot(4), plot3(4))

  ; Attach zonal means
  resZm@tiXAxisString = ""
  resSicZm@tiXAxisString = ""

  resSicZm@gsnLeftString = ""
  plot(1) = gsn_csm_contour(wks, sieZm, resSicZm)

  resZm@gsnLeftString = ""
  plot(3) = gsn_csm_contour(wks, tempZm, resZm)  
  plot(5) = gsn_csm_contour(wks, tempZm2, resZm)

  ; Plot extrats
  lnres = True
  lnres@gsLineColor = "black"
  lnres@gsLineThicknessF = 3
  lnres@gsLineDashPattern = 1

  p2First = 44  ; 200709
  p3First = 152 ; 201609
  vert1a = gsn_add_polyline(wks, plot(0), (/yyyyfrac(p2First-1), yyyyfrac(p2First-1)/), (/0, 360/), lnres)
  vert1b = gsn_add_polyline(wks, plot(0), (/yyyyfrac(p3First-1), yyyyfrac(p3First-1)/), (/0, 360/), lnres)

  vert2a = gsn_add_polyline(wks, plot(2), (/yyyyfrac(p2First-1), yyyyfrac(p2First-1)/), (/0, 400/), lnres)
  vert2b = gsn_add_polyline(wks, plot(2), (/yyyyfrac(p3First-1), yyyyfrac(p3First-1)/), (/0, 400/), lnres)

  vert3a = gsn_add_polyline(wks, plot(4), (/yyyyfrac(p2First-1), yyyyfrac(p2First-1)/), (/0, 400/), lnres)
  vert3b = gsn_add_polyline(wks, plot(4), (/yyyyfrac(p3First-1), yyyyfrac(p3First-1)/), (/0, 400/), lnres)

  lnres@gsLineDashPattern = 0
  vert4a = gsn_add_polyline(wks, plot(1), (/yyyyfrac(p2First-1), yyyyfrac(p2First-1)/), (/0, 360/), lnres)
  vert4b = gsn_add_polyline(wks, plot(1), (/yyyyfrac(p3First-1), yyyyfrac(p3First-1)/), (/0, 360/), lnres)

  vert5a = gsn_add_polyline(wks, plot(3), (/yyyyfrac(p2First-1), yyyyfrac(p2First-1)/), (/0, 400/), lnres)
  vert5b = gsn_add_polyline(wks, plot(3), (/yyyyfrac(p3First-1), yyyyfrac(p3First-1)/), (/0, 400/), lnres)

  vert6a = gsn_add_polyline(wks, plot(5), (/yyyyfrac(p2First-1), yyyyfrac(p2First-1)/), (/0, 400/), lnres)
  vert6b = gsn_add_polyline(wks, plot(5), (/yyyyfrac(p3First-1), yyyyfrac(p3First-1)/), (/0, 400/), lnres)

;*************************************************
; Panel plots
;*************************************************
  panel_res 		     = True
  panel_res@gsnFrame         = False 
  panel_res@gsnMaximize	     = True              		
  panel_res@gsnPanelRowSpec  = True
  panel_res@gsnPanelCenter   = False			
  panel_res@gsnPanelDebug    = False

  pres0 = panel_res
  pres1 = panel_res
  pres2 = panel_res
  pres3 = panel_res
  pres4 = panel_res
  pres5 = panel_res
  pres6 = panel_res

  pres0@gsnPanelTop = 0.99
  pres0@gsnPanelBottom = 0.72
  gsn_panel(wks, plot(0), (/1/), pres0)

  pres1@gsnPanelTop = 0.74
  pres1@gsnPanelBottom = 0.6659
  pres1@gsnPanelRight = 0.906
  gsn_panel(wks, plot(1), (/1/), pres1)

  pres2@gsnPanelTop = 0.67
  pres2@gsnPanelBottom = 0.40
  gsn_panel(wks, plot(2), (/1/), pres2)

  pres3@gsnPanelTop = 0.42
  pres3@gsnPanelBottom = 0.3454
  pres3@gsnPanelRight = 0.9055
  gsn_panel(wks, plot(3), (/1/), pres3)

  pres4@gsnPanelTop = 0.35
  pres4@gsnPanelBottom = 0.08
  gsn_panel(wks, plot(4), (/1/), pres4)

  pres5@gsnPanelTop = 0.10
  pres5@gsnPanelBottom = 0.0254
  pres5@gsnPanelRight = 0.9055
  gsn_panel(wks, plot(5), (/1/), pres5)

  frame(wks)  
  delete([/wks, plot/])	
end
