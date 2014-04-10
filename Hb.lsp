

 (defun ALE_StringSubstAll (NewStr PatStr InpStr SttPos / NewLen)
   (cond
     ( (= "" PatStr) InpStr )
     ( (setq NewLen (strlen NewStr))
       (while (setq SttPos (vl-string-search PatStr InpStr SttPos))
         (setq
           InpStr (vl-string-subst NewStr PatStr InpStr SttPos)
           SttPos (+ SttPos NewLen)
         )
       )
       InpStr
     )
   )
)

(defun c:Hb ( ;                 edit text type entities
	     / ;				no formal arguments
	     ent ;				entity info returned by nentsel
	     elist ;			entity list
	     etype ;			entity type
	     etype2 ;			entity type for dialog box label
	     OldVal ;			original text value
	     NewVal ;			new text value
	     elist2 ;			new entity list
 ;				  get_newval ;			local function
) ;end local variable list
 ;??類②?類②?類②?類②?類②?類②?類②?Start local functions ??類②?類②?類②?
   (defun get_newval (
		      OldVal ;			old value to be changed
		      etype ;			entity type for dialog box label
		      / ;				end of formal argument list
		     ) ;				end of local variable list
      (setq dcl_id (load_dialog "Hb.dcl"))
      (if (not (new_dialog "text_edit" dcl_id))
	 (exit)
      ) ;if
      (set_tile "text" OldVal)
      (if etype
	 (set_tile "box" etype)
      ) ;if
      (action_tile "text" "(setq NewVal $value)")
      (action_tile "accept" "(done_dialog 1)")
      (if (equal (start_dialog) 1)
	 nil
	 (setq NewVal nil)
      ) ;if
      (unload_dialog dcl_id)

      NewVal
   ) ;				end get_newval


 ; *****************************  Main Function  **********************************
   
   (setq Write T) ; Index Text on
   (setq Corner	(grread	(if (setq ent
				    (nentsel "\nPick point on screen for new or select TEXT for edit:")
			    )
			   T
			   T
			) ; if
		)
   )
   (setq T1 (cadr Corner))

   (while ent
      (setq
	 Write nil ; Index Text off
	 ent2  ent
	 elist (entget (car ent))
	 etype (cdr (assoc 0 elist))
      ) ;setq

      (if (member etype '("TEXT" "ATTDEF" "ATTRIB" "MTEXT"))
	 (progn
	    (setq
	       etype2 (cond
			 ((= etype "ATTDEF")
			  (strcat "Default " (cdr (assoc 3 elist)))
			 ) ;1
			 ((= etype "ATTRIB")
			  (strcat "Attribute with Tag: "
				  (cdr (assoc 2 elist))
			  )
			 ) ;2
			 ((= etype "TEXT")
               
			  (if (= 4 (length ent))
			     (progn
				(setq
				   diment  (car (last ent))
				   dimlist (entget diment)
				) ;setq
				(if (= "DIMENSION" (cdr (assoc 0 dimlist)))
				   "Dimension Text"
				   "Block text"
				) ;if dimension?
			     ) ;progn
			  ) ;if
			 ) ;3

			 ((= etype "MTEXT")
			  "MText/DIMText"
			 ) ;4


		      ) ;cond


	       OldVal (cdr (assoc 1 elist))

			
	       OldVal (chan OldVal -48) ;     to edit -->>   ************


	       NewVal (get_newval OldVal etype2)


						
	       NewVal (if NewVal
;;;			 (chan NewVal nil)
			 (chan NewVal 48)
			 nil
		      ) ;                 from edit <<--   ************


	       elist2 (if NewVal
			 (subst (cons 1 NewVal) (assoc 1 elist) elist)
			 nil
		      ) ;if NewVal returned
	    ) ;setq

	    (if	elist2
	       (progn
		  (entmod elist2)
		  (if (= 4 (length ent))
		     (progn
			(setq
			   diment  (car (last ent))
			   dimlist (entget diment)
			) ;setq
			(if (= "DIMENSION" (cdr (assoc 0 dimlist)))
			   (progn
			      (setq dimlist2 (subst (cons 1 NewVal)
						    (assoc 1 dimlist)
						    dimlist
					     )
			      )
			      (entmod dimlist2)
			      (entupd diment)
			   ) ;progn
			) ;if
			(if (= "INSERT" (cdr (assoc 0 dimlist)))
			   (entupd diment)
			) ;if
 ;       (command "dim" "update" diment "" "e")
		     ) ;progn
		     (entupd (cdr (assoc -1 elist)))
		  ) ;if
	       ) ;progn
	    ) ;if
	 ) ;progn
	 (princ "\nNot a Text, MText, Attdef or Attribute entity. ")
      ) ;if
      (setq ent (nentsel "\nSelect TEXT for edit: "))
   ) ;while



;;;      (command "TEXT") *******************************
   (if Write
      (if (setq NewVal (getstring T "\nText: "))
	 (progn
	 
		
	    (setq NewVal (chan NewVal 48))
            (command "TEXT" pause pause pause NewVal)
	 ) ;progn
      ) ;if
   ) ;if

   (princ)
) ;HB


(defun chan (Str_Old ;************************************
	     Ind / Temp_New Str_New Simbol Sim_List)
   (setq I 0
	 Str_New ""
	 Str_Lat ""
	 Lang ""
	 Str_Sim ""

	 Simb_List '("\""  " "	 "."   ","   "'"   ";"	 "~"   "!"   "@"   "#"	 "$"   "%"   "^"
		     "%"   "*"	 ")"   "("   "_"   "-"	 "+"   "="   "/"   "]"	 "["   "?"   "|"
		     ":"   "<"	 ">"   "&"
		    )

   )
	
	(if (equal Ind 48)
		(progn
			(setq Str_Old (ALE_StringSubstAll (chr 128) "\U+05D0" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 129) "\U+05D1" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 130) "\U+05D2" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 131) "\U+05D3" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 132) "\U+05D4" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 133) "\U+05D5" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 134) "\U+05D6" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 135) "\U+05D7" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 136) "\U+05D8" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 137) "\U+05D9" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 139) "\U+05DB" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 138) "\U+05DA" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 140) "\U+05DC" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 142) "\U+05DE" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 141) "\U+05DD" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 144) "\U+05E0" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 143) "\U+05DF" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 145) "\U+05E1" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 146) "\U+05E2" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 148) "\U+05E4" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 147) "\U+05E3" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 150) "\U+05E6" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 149) "\U+05E5" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 151) "\U+05E7" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 152) "\U+05E8" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 153) "\U+05E9" Str_Old nil))
			(setq Str_Old (ALE_StringSubstAll (chr 154) "\U+05EA" Str_Old nil))
		)
	)
	
	
   (setq I (+ (strlen Str_Old) 1))
   (while
      (> (setq I (1- I)) 0)
	(setq Simvol (substr Str_Old I 1))

	(cond
	   ((member Simvol Simb_List) ; Simb  *************************************
	    (setq Str_Sim (strcat Simvol Str_Sim)
	    )

	   ) ; Simb
	   ((and (> (ascii Simvol) 127) (< (ascii Simvol) 155)) ; Heb  ***********
	    (setq Str_New
		    (strcat (if	(= Lang "Lat")
			       (strcat
				  Str_New
				  Str_Sim
				  Str_Lat
			       )
			       (strcat
				  Str_New
				  Str_Lat
				  Str_Sim
			       ) ;progn
			    ) ;if
                           
                            (chr (ascii Simvol))
		    )
		  Str_Lat ""
		  Str_Sim ""
		  Lang "Heb"
	    )

	   ) ; Heb
	   (T ; Lat *********
	    (setq Str_Lat (strcat Simvol Str_Sim Str_Lat)
		  Str_Sim ""
		  Lang	  "Lat"
	    )
	   ) ; Lat
	) ;Cond


	(if (equal Ind -48)
		(progn
			(setq Str_New (ALE_StringSubstAll "\U+05D0" (chr 128) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05D1" (chr 129) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05D2" (chr 130) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05D3" (chr 131) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05D4" (chr 132) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05D5" (chr 133) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05D6" (chr 134) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05D7" (chr 135) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05D8" (chr 136) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05D9" (chr 137) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05DB" (chr 139) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05DA" (chr 138) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05DC" (chr 140) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05DE" (chr 142) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05DD" (chr 141) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05E0" (chr 144) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05DF" (chr 143) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05E1" (chr 145) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05E2" (chr 146) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05E4" (chr 148) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05E3" (chr 147) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05E6" (chr 150) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05E5" (chr 149) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05E7" (chr 151) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05E8" (chr 152) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05E9" (chr 153) Str_New nil))
			(setq Str_New (ALE_StringSubstAll "\U+05EA" (chr 154) Str_New nil))
		)
	)
;;;	(if Ind
;;;
;;;;;;	       (setq Str_New (strcat (if (and (> (ascii Simvol) 127) (< (ascii Simvol) 155))
;;;;;;					(chr (+ (ascii Simvol) 96))
;;;;;;					Simvol
;;;;;;				     ) ;if
;;;;;;				     Str_New
;;;;;;			     )
;;;;;;	       )
;;;
;;;
;;;;;;(setq Str_New (strcat Str_New
;;;;;;		      (if (and (> (ascii Simvol) 127) (< (ascii Simvol) 155))
;;;;;;			 (chr (+ (ascii Simvol) 96))
;;;;;;			 Simvol
;;;;;;		      ) ;if
;;;;;;	      )
;;;;;;)
;;;
;;;	   (cond
;;;	      ((member Simvol Simb_List) ; Simb  *************************************
;;;	       (setq Str_Sim (strcat Simvol Str_Sim)
;;;	       )
;;;
;;;	      ) ; Simb
;;;	      ((and (> (ascii Simvol) 127) (< (ascii Simvol) 155)) ; Heb  ***********
;;;	       (setq Str_New
;;;		       (strcat (if (= Lang "Lat")
;;;				  (strcat
;;;				     Str_New
;;;				     Str_Sim
;;;				     Str_Lat
;;;				  )
;;;				  (strcat
;;;				     Str_New
;;;				     Str_Lat
;;;				     Str_Sim
;;;				  ) ;progn
;;;			       ) ;if
;;;			       (chr (+ (ascii Simvol) 96))
;;;		       )
;;;		     Str_Lat ""
;;;		     Str_Sim ""
;;;		     Lang "Heb"
;;;	       )
;;;
;;;	      ) ; Heb
;;;	      (T ; Lat *********
;;;	       (setq Str_Lat (strcat Simvol Str_Sim Str_Lat)
;;;		     Str_Sim ""
;;;		     Lang    "Lat"
;;;	       )
;;;	      ) ; Lat
;;;	   ) ;Cond
;;;
;;;
;;;	   (cond
;;;	      ((member Simvol Simb_List) ; Simb  *************************************
;;;	       (setq Str_Sim (strcat Simvol Str_Sim)
;;;	       )
;;;
;;;	      ) ; Simb
;;;	      ((and (> (ascii Simvol) 223) (< (ascii Simvol) 251)) ; Heb  ***********
;;;	       (setq Str_New
;;;
;;;		       (strcat (if (= Lang "Lat")
;;;				  (strcat
;;;				     Str_New
;;;				     Str_Sim
;;;				     Str_Lat
;;;				  )
;;;				  (strcat
;;;				     Str_New
;;;				     Str_Lat
;;;				     Str_Sim
;;;				  )
;;;			       ) ;if
;;;			       (chr (- (ascii Simvol) 96))
;;;		       )
;;;		     Str_Lat ""
;;;		     Str_Sim ""
;;;		     Lang "Heb"
;;;	       )
;;;
;;;	      ) ; Heb
;;;	      (T ;*******************************************   Lat   *********
;;;	       (setq Str_Lat (strcat Simvol Str_Sim Str_Lat)
;;;		     Str_Sim ""
;;;		     Lang    "Lat"
;;;	       )
;;;	      ) ; Lat
;;;	   ) ;Cond
;;;
;;;
;;;	) ;if

   ) ;while

   (strcat Str_Sim Str_Lat Str_New)

) ;DEFUN Chan


(princ)
;;;(C:HB)


