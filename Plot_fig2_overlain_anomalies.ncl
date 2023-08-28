;*************************************************
; Plot_fig2_overlain_anomalies.ncl
;
; Plot SIE 5-day anomalies overlain annually
; SIE anomalies calculated wrt 1979-2022 			
;
; AFP 28 June 2023
;************************************************
begin

  wks = gsn_open_wks("pdf", "figures/SIE_anomaly_5daily_lines_230704")
  gsn_define_colormap(wks, "all_colours")
  plot = new(1, graphic)

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

  tsres@trXMinF 	   = 1
  tsres@trXMaxF 	   = 366
  tsres@trYMinF 	   = -3.
  tsres@trYMaxF 	   = 3. 
 
  tsres@tmXBMode   	   = "Explicit"
  tsres@tmXBValues 	   = (/15,46,75,106,136,167,197,228,259,289,320,350/) ; middle of months
  tsres@tmXBLabels 	   = (/"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"/)

  tsres@vpWidthF           = 0.8
  tsres@vpHeightF          = 0.4

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
  txres@txFontHeightF      = 0.015
  txres@txJust             = "CenterCenter"
  txres@txFontColor 	   = "darkorange"

;*************************************************
; Input SIE data from NSIDC spreadsheet 
;*************************************************
  ; Input data - SH-Extent from NSIDC Excel spreadsheet, with pre-processing:
  ; - Remove month name header column
  ; - Replace month column with month numbers (Jan = 1, Feb = 2...)
  ; - Remove annual average column
  ; - Fill missing values with -9999 (Jan-Oct 1978, Dec 1987, Jan 1988, Mar-Dec 2023)
  ; - Each row lists year, followed by 12 monthly values
  ; - Each column lists 1978 through to 2023
  nsidcSie = asciiread("data/SH-5-Day-Extent_230704.txt", (/367, 48/), "float")
  
  daySie = nsidcSie(1:366, 2:47)
  daySie = where(daySie.eq.-9999, daySie@_FillValue, daySie)

  ; Calculate daily climatology
  climYrFirst = 1979
  climYrLast = 2022
  dayClim = dim_avg_n(daySie(:,climYrFirst-1978:climYrLast-1978), 1)

  ; Calculate monthly anomalies
  dayClim2D = conform_dims(dimsizes(daySie), dayClim, 0)
  dayAnom = daySie - dayClim2D

  ; Interpolate 29 Feb for 1979 to 2023
  do iy = 1979, 2023
	if (ismissing(daySie(59,iy-1978)))
		dayAnom(59,iy-1978) = (dayAnom(58,iy-1978) + dayAnom(60,iy-1978))/2
	end if
  end do

;**************************************************
; SIE monthly anomalies
;**************************************************
  dayAnom!0 = "day"
  dayAnom!1 = "year"
  doy = ispan(1,366,1)

  ; Plot 
  tsres@gsnLeftString = "Antarctic five-day sea ice extent anomaly"
  tsres@tiYAxisString = "SIE anomaly (x10~S~6~N~ km~S~2~N~)"
  plot(0) = gsn_csm_xy(wks, doy, dayAnom(year|1:29, day|:), tsres) 				; 1979-2007 grey

  lnres@gsLineColor = "grey"
  nd1978 = gsn_add_polyline(wks, plot(0), doy, dayAnom(year|0, day|:), lnres) 			; 1978 ND grey
  
  lnres@gsLineColor = "dodgerblue"
  sond2007 = gsn_add_polyline(wks, plot(0), doy(244:), dayAnom(year|29,day|244:), lnres) 	; 2007 SOND blue
  p2Anom = new(10, graphic)
  do ii = 30, 37
  	p2Anom(ii-30) = gsn_add_polyline(wks, plot(0), doy, dayAnom(year|ii, day|:), lnres) 	; 2008-2015 blue
  end do
  ja2016 = gsn_add_polyline(wks, plot(0), doy(:243), dayAnom(year|38,day|:243), lnres) 		; JFMAMJJA 2016 blue

  lnres@gsLineColor = "darkorange"
  sond2016 = gsn_add_polyline(wks, plot(0), doy(244:), dayAnom(year|38,day|244:), lnres) 	; 2016 SOND orange
  p3Anom = new(10, graphic)
  do ii = 39, 44
  	p3Anom(ii-38) = gsn_add_polyline(wks, plot(0), doy, dayAnom(year|ii,day|:), lnres) 	; 2017-2022 orange
  end do

  lnres@gsLineThicknessF = 8
  sie2023 = gsn_add_polyline(wks, plot(0), doy, dayAnom(year|45,day|:), lnres) 			; 2023 JFMAMJ orange bold
  txres@txFont = 22
  sie2023tx = gsn_add_text(wks, plot(0), "2023", 198, -2.6, txres)
  txres@txFont = 21

  ; Legend
  txres@txJust = "CenterLeft" 
  txres@txFontColor = "grey"
  p1leg = gsn_add_text(wks, plot(0), "Nov 1978 to Aug 2007", 265, 2.7, txres) 
  txres@txFontColor = "dodgerblue"
  p2leg = gsn_add_text(wks, plot(0), "Sep 2007 to Aug 2016", 265, 2.4, txres)
  txres@txFontColor = "darkorange"
  p3leg = gsn_add_text(wks, plot(0), "Sep 2016 to Jun 2023", 265, 2.1, txres)

;*************************************************
; Panel plots
;*************************************************
  panel_res 		      = True
  panel_res@gsnFrame         = False 
  panel_res@gsnMaximize       = True              		
  panel_res@gsnPanelRowSpec  = True
  panel_res@gsnPanelCenter   = True			
  gsn_panel(wks, plot(0), (/1/), panel_res)
 
  frame(wks)  	
end
