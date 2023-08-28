;*************************************************
; Plot_fig1_timeseries.ncl
;
; Plot SIE monthly anomaly time series
; Plot SIE monthly percent anomaly time series
; SIE anomalies calculated wrt 1979-2022 		
; Plot monthly ocean temperature anomalies
; Ocean temp anomalies calculated wrt 2004-2022 	
;
; AFP 8 March 2023
;************************************************
begin

  wks = gsn_open_wks("pdf", "figures/SIE_anomaly_timeseries_230704")
  gsn_define_colormap(wks, "all_colours")
  plot = new(3, graphic)
  plot2 = new(3, graphic)

;************************************************
; Plot resources
;************************************************
  ; Time series resources 
  tsres                    = True     			
  tsres@gsnMaximize 	   = True
  tsres@gsnDraw            = False               		
  tsres@gsnFrame           = False               		

  tsres@txFont		   = 21
  tsres@tmXBLabelFont 	   = 21
  tsres@tmYLLabelFont 	   = 21
  tsres@tiXAxisFont   	   = 21
  tsres@tiYAxisFont   	   = 21
  tsres@txFontHeightF      = 0.014
  tsres@tmXBLabelFontHeightF= 0.014
  tsres@tmYLLabelFontHeightF= 0.014
  tsres@tiXAxisFontHeightF = 0.014
  tsres@tiYAxisFontHeightF = 0.014

  tsres@tmBorderThicknessF = 1.				
  tsres@tmXBMajorThicknessF= 1.				
  tsres@tmYLMajorThicknessF= 1. 
  tsres@tmXBMinorThicknessF= 1.
  tsres@tmYLMinorThicknessF= 1.

  tsres@gsnYRefLine        = 0.0 				
  tsres@gsnYRefLineDashPattern= 0
  tsres@gsnXRefLine        = 0.0 				
  tsres@gsnXRefLineDashPattern= 0

  tsres@xyLineThicknessF   = 2.       
  tsres@xyDashPattern      = 0 
  tsres@xyLineColor 	   = "grey"

  tsres@trXMinF 	   = 1978.0
  tsres@trXMaxF 	   = 2024.0
  tsres@trYMinF 	   = -2.4 
  tsres@trYMaxF 	   = 2.4 
 
  tsres@tmXBMode   	   = "Explicit"
  tsres@tmXBValues 	   = ispan(1980, 2020, 5)
  tsres@tmXBLabels 	   = ispan(1980, 2020, 5)
  tsres@tmXBMinorValues	   = ispan(1978, 2024, 1)

  tsres@vpWidthF           = 0.8
  tsres@vpHeightF          = 0.2

  ; Extra resources
  mkres			   = True 
  mkres@tfPolyDrawOrder    = "PostDraw"
  mkres@gsMarkerSizeF 	   = .005 
  mkres@gsMarkerIndex      = 16 
  mkres@gsMarkerColor	   = "darkorange"

  lnres			   = True
  lnres@tfPolyDrawOrder	   = "PostDraw"
  lnres@gsLineThicknessF   = 2

  txres			   = True
  txres@txFont 		   = 21
  txres@txFontHeightF      = 0.012
  txres@txJust             = "CenterCenter"
  txres@txFontColor 	   = "darkorange"

  ; Contour resources
  res                      = True     				
  res@gsnDraw              = False               		
  res@gsnFrame             = False               		

  res@txFont		   = 21
  res@tmXBLabelFont 	   = 21
  res@tmYLLabelFont 	   = 21
  res@tiXAxisFont   	   = 21
  res@tiYAxisFont   	   = 21
  res@txFontHeightF        = 0.014
  res@tmXBLabelFontHeightF = 0.014
  res@tmYLLabelFontHeightF = 0.014
  res@tiXAxisFontHeightF   = 0.014
  res@tiYAxisFontHeightF   = 0.014

  res@cnFillOn             = True     				
  res@cnLinesOn            = False    				
  res@gsnSpreadColors      = True     				
  res@cnLineLabelsOn       = False			        
  res@cnInfoLabelOn        = False
  res@cnLevelSelectionMode = "ManualLevels"      		
  res@gsnSpreadColorStart  = 2 
  res@gsnSpreadColorEnd    = 23 

  res@lbLabelBarOn 	   = True
  res@lbOrientation	   = "Horizontal"
  res@lbLabelFontHeightF   = 0.014
  res@lbLabelFont          = 21
  res@lbLabelStride        = 5
  res@lbBoxLineThicknessF  = 1.
  res@lbBoxSeparatorLinesOn= False

  res@tmBorderThicknessF   = 1.				
  res@tmXBMajorThicknessF  = 1. 				
  res@tmYLMajorThicknessF  = 1.   
  res@tmXBMinorThicknessF  = 1. 				
  res@tmYLMinorThicknessF  = 1. 

  res@trYReverse 	   = True
  res@gsnYAxisIrregular2Linear = True
  res@trYMaxF 		   = 600.

  res@tiYAxisString 	   = "Depth (m)"
  res@trXMinF 		   = 1978.0
  res@trXMaxF 		   = 2024.0
 
  res@tmXBMode   	   = "Explicit"
  res@tmXBValues 	   = ispan(1980, 2020, 5)
  res@tmXBLabels 	   = ispan(1980, 2020, 5)
  res@tmXBMinorValues	   = ispan(1978, 2024, 1)

  res@vpWidthF 		   = 0.789
  res@vpHeightF 	   = 0.2

  res@gsnSpreadColorStart  = 4
  res@gsnSpreadColorEnd    = 25
  res@cnMaxLevelValF       = 0.3
  res@cnMinLevelValF       = -0.3
  res@cnLevelSpacingF      = (res@cnMaxLevelValF - res@cnMinLevelValF)/20

  resSig 		     = True
  resSig@gsnDraw             = False               		
  resSig@gsnFrame            = False               		
  resSig@gsnTickMarksOn      = False
  resSig@cnLevelSelectionMode= "ManualLevels" 			
  resSig@cnMinLevelValF      = -1.05        			
  resSig@cnMaxLevelValF      = 1.05        			
  resSig@cnLevelSpacingF     = 0.01        			
  resSig@cnInfoLabelOn       = False       			
  resSig@cnLinesOn           = False       			
  resSig@cnLineLabelsOn      = False       			
  resSig@cnFillScaleF        = 0.6       			
  resSig@cnFillDotSizeF	     = 0.002

;*************************************************
; Input SIE data from NSIDC spreadsheet 
;*************************************************
  ; Input data - SH-Extent from NSIDC Excel spreadsheet, with pre-processing:
  ; - Remove month name header row
  ; - Remove annual average column
  ; - Fill missing values with -9999 (Jan-Oct 1978, Dec 1987, Jan 1988, Mar-Dec 2023)
  ; - Each row lists year, followed by 12 monthly values
  ; - Each column lists 1978 through to 2023
  nsidcSie = asciiread("data/Sea_Ice_Index_Monthly_Data_by_Year_G02135_v3.0_SH-Extent_230704.txt", (/46, 13/), "float") 
  monthlySie = nsidcSie(:, 1:12)
  monthlySie = where(monthlySie.eq.-9999, monthlySie@_FillValue, monthlySie)

  ; Calculate monthly climatology
  climYrFirst = 1979
  climYrLast = 2022
  monthlyClim = dim_avg_n(monthlySie(climYrFirst-1978:climYrLast-1978,:), 0)

  ; Calculate monthly anomalies
  monthlyClim2D = conform_dims(dimsizes(monthlySie), monthlyClim, 1)
  monthlyAnom = monthlySie - monthlyClim2D

  ; Interpolate anomalies for 198712 and 198801
  monthlyAnom(9,11) = (2*monthlyAnom(9,10) + 1*monthlyAnom(10,1))/3 ; 198712 interpolated between 198711 and 198802
  monthlyAnom(10,0) = (1*monthlyAnom(9,10) + 2*monthlyAnom(10,1))/3 ; 198801 interpolated between 198711 and 198802

  ; Calculate monthly percent anomalies
  monthlyAnomPerc = monthlyAnom / monthlyClim2D * 100

  ; Convert to 1D timeseries
  sieAnom = ndtooned(monthlyAnom)
  siePerc = ndtooned(monthlyAnomPerc)
  
  ; Year and month info
  yyyymmStr = new(dimsizes(sieAnom), string)
  counter = 0
  do iy = 1978, 2023
  do im = 1, 12
	monthStr = tostring(im)
	if (im .le. 9)
		monthStr = "0" + monthStr
	end if
	yyyymmStr(counter) = tostring(iy) + monthStr
	counter = counter + 1
	delete(monthStr)
  end do
  end do
  yyyyfrac = yyyymm_to_yyyyfrac( tofloat(yyyymmStr), 0.5)
  delete(counter)

  printSie = False
  if (printSie)
	print(sieAnom)
	print(siePerc)
	print(yyyyfrac)
  end if

  ; Indices in 1D series for specific dates
  ; Periods - dates from change point analysis
  p1First = 10   ; 197811
  p1Last = 355   ; 200708
  p2First = 356  ; 200709
  p2Last = 463   ; 201608
  p3First = 464  ; 201609
  p3Last = 545   ; 202306
  p1aFirst = 10  ; 197811
  p1aLast = 187  ; 199308
  p1bFirst = 188 ; 199309
  p1bLast = 358  ; 200711 

  ; sieAnom extreme minimums
  dec16 = 467    ; 201612
  jan23 = 540    ; 202301
  jun23 = 545	 ; 202306

  ; siePerc extreme minimums
  mar17 = 470    ; 201703
  feb22 = 529    ; 202202
  feb23 = 541    ; 202302

  ; Argo coverage
  argoFirst = 312; 200401
  argoLast = 544 ; 202305

;**************************************************
; SIE monthly anomalies
;**************************************************
  ; Plot 
  tsres@gsnLeftString = "~F22~a~F21~ Antarctic monthly sea ice extent anomaly"
  tsres@tiYAxisString = "SIE anomaly (x10~S~6~N~ km~S~2~N~)"
  plot(0) = gsn_csm_xy(wks, yyyyfrac(p1First:p1Last), sieAnom(p1First:p1Last), tsres)
  
  lnres@gsLineColor = "dodgerblue"
  p2Anom = gsn_add_polyline(wks, plot(0), yyyyfrac(p2First-1:p2Last), sieAnom(p2First-1:p2Last), lnres) ; connect to grey series

  lnres@gsLineColor = "darkorange"
  p3Anom = gsn_add_polyline(wks, plot(0), yyyyfrac(p3First-1:p3Last), sieAnom(p3First-1:p3Last), lnres) ; connect to blue series

  ; Add period averages
  lnres@gsLineThicknessF = 4
  lnres@gsLineColor = "grey"
  pAv1 = gsn_add_polyline(wks, plot(0), (/yyyyfrac(p1First), yyyyfrac(p1Last)/), (/avg(sieAnom(p1First:p1Last)), avg(sieAnom(p1First:p1Last))/), lnres)

  lnres@gsLineColor = "dodgerblue"
  pAv2 = gsn_add_polyline(wks, plot(0), (/yyyyfrac(p2First), yyyyfrac(p2Last)/), (/avg(sieAnom(p2First:p2Last)), avg(sieAnom(p2First:p2Last))/), lnres)

  lnres@gsLineColor = "darkorange"
  pAv3 = gsn_add_polyline(wks, plot(0), (/yyyyfrac(p3First), yyyyfrac(p3Last)/), (/avg(sieAnom(p3First:p3Last)), avg(sieAnom(p3First:p3Last))/), lnres)

  ; Mark extreme lows
  dec16mk = gsn_add_polymarker(wks, plot(0), yyyyfrac(dec16), sieAnom(dec16), mkres)
  dec16tx = gsn_add_text(wks, plot(0), "Dec 16", 2015.2, -2.05, txres)
  jan23mk = gsn_add_polymarker(wks, plot(0), yyyyfrac(jan23), sieAnom(jan23), mkres)
  jan23tx = gsn_add_text(wks, plot(0), "Jan 23", 2021.4, -1.75, txres)
  jun23mk = gsn_add_polymarker(wks, plot(0), yyyyfrac(jun23), sieAnom(jun23), mkres)
  jun23tx = gsn_add_text(wks, plot(0), "Jun 23", 2021.9, -2.19, txres)

;*************************************************
; Statistics to compare SIE across periods
;*************************************************
  ; Compare across three main periods
  ave1 = avg(sieAnom(p1First:p1Last))
  var1 = variance(sieAnom(p1First:p1Last))
  s1 = dimsizes(sieAnom(p1First:p1Last))

  ave2 = avg(sieAnom(p2First:p2Last))
  var2 = variance(sieAnom(p2First:p2Last))
  s2 = dimsizes(sieAnom(p2First:p2Last))

  ave3 = avg(sieAnom(p3First:p3Last))
  var3 = variance(sieAnom(p3First:p3Last))
  s3 = dimsizes(sieAnom(p3First:p3Last))

  fprob12 = ftest(var1, s1, var2, s2, 0)
  fprob23 = ftest(var2, s2, var3, s3, 0)
  fprob13 = ftest(var1, s1, var3, s3, 0)

  tprob12 = ttest(ave1, var1, s1, ave2, var2, s2, True, False)
  tprob23 = ttest(ave2, var2, s2, ave3, var3, s3, False, False)
  tprob13 = ttest(ave1, var1, s1, ave3, var3, s3, True, False)

  ; Statistics to compare sub-periods within the first period
  ave1a = avg(sieAnom(p1aFirst:p1aFirst))
  var1a = variance(sieAnom(p1aFirst:p1aLast))
  s1a = dimsizes(sieAnom(p1aFirst:p1aLast))

  ave1b = avg(sieAnom(p1bFirst:p1bLast))
  var1b = variance(sieAnom(p1bFirst:p1bLast))
  s1b = dimsizes(sieAnom(p1bFirst:p1bLast))

  tprob1a1b = ttest(ave1a, var1a, s1a, ave1b, var1b, s1b, False, False)

  printStats = False
  if (printStats)
	print("fprob12 = " + fprob12)
  	print("fprob23 = " + fprob23)
	print("fprob13 = " + fprob13)
  	print("tprob12 = " + tprob12)
  	print("tprob23 = " + tprob23)
  	print("tprob13 = " + tprob13)
  	print("tprob1a1b = " + tprob1a1b)
  end if 

  delete([/ave1, var1, s1, ave2, var2, s2, ave3, var3, s3, fprob12, fprob23, fprob13, tprob12, tprob23, tprob13, ave1a, var1a, s1a, ave1b, var1b, s1a, ave1b, var1b, s1b, tprob1a1b/])

;*************************************************
; SIE monthly percent anomalies
;*************************************************
  ; Update resources for percent anomalies
  tsres@trYMinF 	    = -50
  tsres@trYMaxF 	    = 50
  tsres@tiYAxisString  	    = "SIE anomaly (%)"
  tsres@vpWidthF 	    = 0.733 ; slightly less wide
  tsres@txFontHeightF       = 0.013
  tsres@tmXBLabelFontHeightF= 0.013
  tsres@tmYLLabelFontHeightF= 0.013
  tsres@tiXAxisFontHeightF  = 0.013
  tsres@tiYAxisFontHeightF  = 0.013

  ; Plot
  tsres@gsnLeftString = "~F22~b~F21~ Antarctic monthly sea ice extent percentage anomaly"
  plot(1) = gsn_csm_xy(wks, yyyyfrac(p1First:p1Last), siePerc(p1First:p1Last), tsres)

  lnres@gsLineThicknessF = 2
  lnres@gsLineColor = "dodgerblue"
  p2Perc = gsn_add_polyline(wks, plot(1), yyyyfrac(p2First-1:p2Last), siePerc(p2First-1:p2Last), lnres) ; connect to grey series

  lnres@gsLineColor = "darkorange"
  p3Perc = gsn_add_polyline(wks, plot(1), yyyyfrac(p3First-1:p3Last), siePerc(p3First-1:p3Last), lnres) ; connect to blue series

  ; Mark extreme lows
  mar17mk = gsn_add_polymarker(wks, plot(1), yyyyfrac(mar17), siePerc(mar17), mkres)
  mar17tx = gsn_add_text(wks, plot(1), "Mar 17", 2017, -37.5, txres)
  feb22mk = gsn_add_polymarker(wks, plot(1), yyyyfrac(feb22), siePerc(feb22), mkres)
  feb22tx = gsn_add_text(wks, plot(1), "Feb 22", 2020.5, -29.5, txres)
  feb23mk = gsn_add_polymarker(wks, plot(1), yyyyfrac(feb23), siePerc(feb23), mkres)
  feb23tx = gsn_add_text(wks, plot(1), "Feb 23", 2022.5, -43, txres)

;*********************************************
; Ocean temperature anomalies 
;*********************************************
  ; Input Argo data
  in = addfile("data/from_Ed_230703/argo_anom_time_series-50-65S.nc", "r")
  time = in->TIME
  time2 = tofloat(time)
  time2@units = time@units
  time2@calendar = time@calendar

  pressure = in->PRESSURE
  pressure2 = tofloat(pressure)

  temp = in->ARGO_TEMPERATURE_ANOMALY
  yyyymm = cd_calendar(time2, 1)/100
  yyyyfrac2 = yyyyfrac(argoFirst:argoLast)

  replace_ieeenan(temp, temp@_FillValue, 0)
  temp2 = tofloat(temp)
  temp2!0 = "year"
  temp2&year = yyyyfrac2
  temp2!1 = "pressure"
  temp2&pressure = pressure2

  pressureLat = pressure2
  pressureLat = -57.5
  load "ocean_functions.ncl"
  begin
	depth = pres2depth(pressure2, pressureLat)
  end 

  tempLong = new((/dimsizes(yyyyfrac), dimsizes(pressure2)/), float)
  tempLong(argoFirst:argoLast,:) = temp2
  tempLong!0 = "year"
  tempLong&year = yyyyfrac
  tempLong!1 = "depth";
  tempLong&depth = depth
    
  ; Plot
  res@gsnLeftString = "~F22~c~F21~ Southern Ocean 50-65~S~o~N~S temperature anomaly (~S~o~N~C)"
  plot(2) = gsn_csm_contour(wks, tempLong(depth|:, year|:), res)

  ; Add stippling for +/- 1 std dev
  tempStddev = dim_stddev_n(tempLong, 0)
  tempStddev2D = conform_dims(dimsizes(tempLong), tempStddev, 1)
  tempStippling = where(abs(tempLong).gt.tempStddev2D, 1., 0.)
  copy_VarCoords(tempLong, tempStippling)

  plot2(2) = gsn_csm_contour(wks, tempStippling(depth|:, year|:), resSig)
  plot2(2) = ShadeGtContour(plot2(2), 0.99, 17) 
  overlay(plot(2), plot2(2))

  ; Plot extras
  lnres@gsLineColor = "black"
  lnres@gsLineThicknessF = 1
  lnres@gsLineDashPattern = 0
  vert1 = gsn_add_polyline(wks, plot(2), (/yyyyfrac(argoFirst), yyyyfrac(argoFirst)/), (/0, 600/), lnres) 
  lnres@gsLineThicknessF = 3
  lnres@gsLineDashPattern = 1
  vert2 = gsn_add_polyline(wks, plot(2), (/yyyyfrac(p2First-1), yyyyfrac(p2First-1)/), (/0, 600/), lnres)
  vert3 = gsn_add_polyline(wks, plot(2), (/yyyyfrac(p3First-1), yyyyfrac(p3First-1)/), (/0, 600/), lnres)
  lnres@gsLineThicknessF = 1
  lnres@gsLineDashPattern = 0
  vert4 = gsn_add_polyline(wks, plot(2), (/yyyyfrac(argoLast), yyyyfrac(argoLast)/), (/0, 600/), lnres)

  txres@txFontColor = "grey"
  argotx = gsn_add_text(wks, plot(2), "Argo data available from 2004", 1991, 300, txres)
  
;*************************************************
; Panel plots
;*************************************************
  panel_res 		      = True
  panel_res@gsnFrame         = False 
  panel_res@gsnMaximize       = True              		
  panel_res@gsnPanelRowSpec  = True
  panel_res@gsnPanelCenter   = True		

  panel_res1 = panel_res
  panel_res1@gsnPanelBottom = 0.4
  gsn_panel(wks, plot(0:1), (/1,1/), panel_res1)

  panel_res2 = panel_res
  panel_res2@gsnPanelTop = 0.38
  panel_res2@gsnPanelLeft = 0.05
  panel_res2@gsnPanelRight = 0.96
  gsn_panel(wks, plot(2), (/1/), panel_res2)
 
  frame(wks)  	
end
