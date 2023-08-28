;*************************************************
; Plot_fig4_scatters.ncl
;
; Read in NSIDC SH-Annual-Daily-Extent min and max data
; Plot spring max vs following summer min
; Plot summer min vs following spring max
; 
; AFP 15 March 2023
;************************************************
begin	
					
  wks = gsn_open_wks("pdf", "figures/scatter_230628")

;************************************************
; Plot resources
;************************************************
  res                     = True                     		
  res@gsnDraw             = False               	
  res@gsnFrame            = False    

  res@txFontHeightF   	   = 0.022
  res@tmXBLabelFontHeightF = 0.022
  res@tmYLLabelFontHeightF = 0.022
  res@tiXAxisFontHeightF   = 0.022
  res@tiYAxisFontHeightF   = 0.022
  res@txFont		   = 21
  res@tmXBLabelFont 	   = 21
  res@tmYLLabelFont 	   = 21
  res@tiXAxisFont   	   = 21
  res@tiYAxisFont   	   = 21

  res@tmBorderThicknessF   = 1.					
  res@tmXBMajorThicknessF  = 1. 				
  res@tmYLMajorThicknessF  = 1. 

  res@xyMarkLineMode       = "Markers"
  res@xyMarkerSizeF        = 0.014
  res@xyMarker             = 16             		
  res@xyMarkerColor	   = "grey"  
  res@xyMarkerThicknessF   = 2.

  mkres 		   = True     

  lnres		   	   = True
  lnres@gsLineThicknessF   = 3.
  lnres@gsLineDashPattern  = 2.

  txres 		   = True
  txres@gsnFrame 	   = False
  txres@gsnDraw 	   = False
  txres@txFont 		   = 21
  txres@txFontHeightF 	   = 0.022
  txres@txJust		   = "CenterLeft"

  plot 			   = new(2, graphic)
  plot2			   = new(2, graphic)
  plot3			   = new(2, graphic)
	
;*************************************************
; Input SIE data fro NSIDC spreadsheet
;*************************************************
  ; Input data - SH-Annual-Daily-Extent from NSIDC spreadsheet, with pre-processing:
  ; - Remove header row
  ; - Remove rank and date columns
  ; - Add Feb 2023 minimum
  ; - Fill missing value (2023 maximum) with -9999
  ; - Each column lists 1979 through to 2023
  nsidcSie = asciiread("data/SH-Annual-Daily-Extent_min_max_230315.txt", (/45,3/), "float")
  nsidcSie = where(nsidcSie.eq.-9999, nsidcSie@_FillValue, nsidcSie)
  sieMin = nsidcSie(:,1)
  sieMax = nsidcSie(:,2)

  ; period indices - years from change point analysis
  p1First = 0  ; 1979
  p1Last = 27  ; 2006
  p2First = 28 ; 2007
  p2Last = 36  ; 2015
  p3First = 37 ; 2016
  p3Last = 43  ; 2022 - note have summer 2023 data, but not spring 2023 data
	
  ;*************************************************
  ; Plot
  ;*************************************************
  xRange = max(sieMax) - min(sieMax)
  yRange = max(sieMin) - min(sieMin)
  res@trXMinF = min(sieMax) - (0.1*xRange)
  res@trXMaxF = max(sieMax) + (0.1*xRange)
  res@trYMinF = min(sieMin) - (0.1*yRange)
  res@trYMaxF = max(sieMin) + (0.1*yRange)
  delete([/xRange, yRange/])

  res@tiXAxisString = "Spring maximum SIE (x10~S~6~N~ km~S~2~N~)"
  res@tiYAxisString = "Summer minimum SIE (x10~S~6~N~ km~S~2~N~)"

  ; Panel a
  res@gsnLeftString = "~F22~a~F21~ Maximum SIE leading minimum SIE"
  plot(0) = gsn_csm_xy(wks, transpose((/sieMax(p1First:p3Last), sieMax(p1First:p3Last)/)),  transpose((/sieMin(p1First+1:p3Last+1), sieMin(p1First+1:p3Last+1)/)), res)  
		
  mkres@gsMarkerThicknessF = 3.             
  mkres@gsMarkerSizeF = .008 
  mkres@gsMarkerIndex = 16    
  mkres@gsMarkerColor = "dodgerblue"
  plot2(0) = gsn_add_polymarker(wks, plot(0), sieMax(p2First:p2Last), sieMin(p2First+1:p2Last+1), mkres) 
  mkres@gsMarkerColor = "darkorange"           	    
  plot3(0) = gsn_add_polymarker(wks, plot(0), sieMax(p3First:p3Last), sieMin(p3First+1:p3Last+1), mkres)

  rc1 = regline(sieMax(p2First:p2Last), sieMin(p2First+1:p2Last+1))
  x1 = (/min(sieMax(p2First:p2Last))-0.05, max(sieMax(p2First:p2Last))+0.05/)
  y1 = rc1*x1 + rc1@yintercept
  lnres@gsLineColor = "dodgerblue"
  ln1 = gsn_add_polyline(wks, plot(0), x1, y1, lnres)
  corr1 = escorc(sieMax(p2First:p2Last), sieMin(p2First+1:p2Last+1))
  df = rc1@nptxy-2
  tval = rc1@tval
  b = 0.5
  pval1 = betainc( df/(df+tval^2), df/2.0, b)
  print("Spring lead corr, blue points, r = " + corr1 + ", pval = " + pval1)
  delete([/df, tval, b/])

  rc3 = regline(sieMax(p1First:p2Last), sieMin(p1First+1:p2Last+1))
  x3 = (/min(sieMax(p1First:p2Last))-0.05, max(sieMax(p1First:p2Last))+0.05/)
  y3 = rc3*x3 + rc3@yintercept
  lnres@gsLineColor = "grey"
  corr3 = escorc(sieMax(p1First:p2Last), sieMin(p1First+1:p2Last+1))
  df = rc3@nptxy-2
  tval = rc3@tval
  b = 0.5
  pval3 = betainc( df/(df+tval^2), df/2.0, b)
  print("Spring lead corr, grey and blue points, r = " + corr3 + ", pval = " + pval3)
  delete([/df, tval, b/])

  ; Also check spring lead orange points 
  rc4 = regline(sieMax(p3First:p3Last), sieMin(p3First+1:p3Last+1))
  corr4 = escorc(sieMax(p3First:p3Last), sieMin(p3First+1:p3Last+1))
  df = rc4@nptxy-2
  tval = rc4@tval
  b = 0.5
  pval4 = betainc( df/(df+tval^2), df/2.0, b)
  print("Spring lead corr, orange points, r = " + corr4 + ", pval = " + pval4)
  delete([/df, tval, b/])

  txres@txFontColor = "grey"
  text1 = gsn_add_text(wks, plot(0), "1979/80-2006/07, N=28", 19.02, 1.95, txres)
  txres@txFontColor = "dodgerblue"
  text2 = gsn_add_text(wks, plot(0), "2007/08-2015/16, N=9", 19.02, 1.8, txres)
  txres@txFontColor = "darkorange"
  text3 = gsn_add_text(wks, plot(0), "2016/17-2022/23, N=7", 19.02, 1.65, txres)

  ; Panel b
  res@gsnLeftString = "~F22~b~F21~ Minimum SIE leading maximum SIE"
  plot(1) = gsn_csm_xy(wks, transpose((/sieMax(p1First:p3Last), sieMax(p1First:p3Last)/)), transpose((/sieMin(p1First:p3Last), sieMin(p1First:p3Last)/)), res)

  mkres@gsMarkerColor = "dodgerblue"
  plot2(1) = gsn_add_polymarker(wks, plot(1), sieMax(p2First:p2Last), sieMin(p2First:p2Last), mkres)
  mkres@gsMarkerColor = "darkorange"
  plot3(1) = gsn_add_polymarker(wks, plot(1), sieMax(p3First:p3Last), sieMin(p3First:p3Last), mkres)

  rc2 = regline(sieMax(p3First:p3Last), sieMin(p3First:p3Last))
  x2 = (/min(sieMax(p3First:p3Last))-0.05, max(sieMax(p3First:p3Last))+0.05/)
  y2 = rc2*x2 + rc2@yintercept
  lnres@gsLineColor = "darkorange"
  ln2 = gsn_add_polyline(wks, plot(1), x2, y2, lnres)
  corr2 = escorc(sieMax(p3First:p3Last), sieMin(p3First:p3Last))
  df = rc2@nptxy-2
  tval = rc2@tval
  b = 0.5
  pval2 = betainc( df/(df+tval^2), df/2.0, b)
  print("Summer lead corr, orange points, r = " + corr2 + ", pval = " + pval2)
  delete([/df, tval, b/])

  ; Also check summer lead grey and blue points 
  rc5 = regline(sieMax(p1First:p2Last), sieMin(p1First:p2Last))
  corr5 = escorc(sieMax(p1First:p2Last), sieMin(p1First:p2Last))
  df = rc5@nptxy-2
  tval = rc5@tval
  b = 0.5
  pval5 = betainc( df/(df+tval^2), df/2.0, b)
  print("Summer lead corr, grey and blue points, r = " + corr5 + ", pval = " + pval5)
  delete([/df, tval, b/])

  ; Also check summer lead blue points 
  rc6 = regline(sieMax(p2First:p2Last), sieMin(p2First:p2Last))
  corr6 = escorc(sieMax(p2First:p2Last), sieMin(p2First:p2Last))
  df = rc6@nptxy-2
  tval = rc6@tval
  b = 0.5
  pval6 = betainc( df/(df+tval^2), df/2.0, b)
  print("Summer lead corr, blue points, r = " + corr6 + ", pval = " + pval6)
  delete([/df, tval, b/])

  txres@txFontColor = "grey"
  text4 = gsn_add_text(wks, plot(1), "1979-2006, N=28", 19.38, 1.95, txres)
  txres@txFontColor = "dodgerblue"
  text5 = gsn_add_text(wks, plot(1), "2007-2015, N=9", 19.38, 1.8, txres)
  txres@txFontColor = "darkorange"
  text6 = gsn_add_text(wks, plot(1), "2016-2022, N=7", 19.38, 1.65, txres)
	  	
;*************************************************
; Panel plots
;*************************************************
  panel_res 		     = True
  panel_res@gsnFrame         = False 
  panel_res@gsnMaximize	     = True              		
  panel_res@gsnPanelRowSpec  = True
  panel_res@gsnPanelCenter   = True				
  panel_res@gsnPanelYWhiteSpacePercent = 3
  panel_res@gsnPanelXWhiteSpacePercent = 4	
  panel_res@gsnPanelBottom   = 0.01
 
  gsn_panel(wks, plot, (/2/), panel_res)

  frame(wks)  	
end
