;; 2D/3D Entity Checker
;; Created by: Sydney Lieng March 2017

;; Functions of the checker:
;; - Run through drawings (2D or 3D) and check if the entities within drawings are correct
;; - Will save all checked in a folder called "Checked"
;; - Will save all errors in a folder called "Errors"
;; - All errors will be recorded in the log file called "Log_File"


;; ----------------------------------------------------------------------------------------------------------------------------------------------------------------


  (setq Layer0 (list "Layer 0" 0))
  (setq Non3DSolid (list "Non 3D Solid" 0))
  (setq 3DSolid (list "3D Solid" 0))
  (setq Dimension (list "Dimension" 0))
  (setq Attribute (list "Attribute" 0))
  (setq AttributeDefinition(list "Attribute Definition" 0))
  (setq BlockReference (list "Block Reference or Array or External Reference" 0))
  (setq Hatch (list "Hatch" 0))
  (setq Helix (list "Helix" 0))
  (setq Leader (list "Leader" 0))
  (setq Mesh (list "Mesh" 0))
  (setq MLine (list "MLine" 0))
  (setq MText (list "MText" 0))
  (setq Multileader (list "Multileader" 0))
  (setq PolyfaceMesh (list "Polyface Mesh or 3D Polyline" 0))
  (setq PositionMaker (list "Position Maker" 0))
  (setq Region (list "Region" 0))
  (setq SectionObject (list "Section Object" 0))
  (setq Surface (list "Surface" 0))
  (setq Table (list "Table" 0))
  (setq Viewport (list "Viewport" 0))
;  (setq 3DPolyline (list "3D Polyline" 0))
;  (setq AlignedConstraintParameter (list "Aligned Constraint Parameter" 0))
;  (setq Array (list "Array" 0))
;  (setq ExternalReference (list "External Reference" 0))

;  (setq Parameter (list "Parameter" 0))

;; addVar: Will add a wrong entity to the list
(defun addVar(entity) 
  (setq entity (list (car entity) (+ (cadr entity) 1)))
)

;; writeFile: Will add text into the log file
(defun writeFile(string)
  (setq drawingName (fnsplitl (getvar "dwgname")))
  (if (/= string "")
    (progn
	  (setq file(open "DESTINATION/Log_File.txt" "a"))
	  (write-line (strcat (getvar "dwgname") ":" string) file)
	  (close file)
    )
  )
)

;; saveFile: Will save the drawing in the correct folder
(defun saveFile(string)
  (if (/= string "")
    (command "_SAVEAS" "2000" (strcat (getvar "dwgprefix") "Errors\\" (getvar "dwgname")))
    (command "_SAVEAS" "2000" (strcat (getvar "dwgprefix") "Checked\\" (getvar "dwgname")))
  )
)

;; checkEntity: Will check the entities.
(defun checkEntity()
  (if (ssget "X" '((8 . "*0*"))) (addVar Layer0))
  (if (ssget "X" '((0 . "INSERT"))) (setq BlockReference (addVar BlockReference)))
  (if (ssget "X" '((8 . "*-3-*") (-4 . "<NOT") (0 . "3DSOLID") (-4 . "NOT>"))) (setq Non3DSolid (addVar Non3DSolid)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "*DIMENSION*") (-4 . "AND>"))) (setq Dimension (addVar Dimension)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "3DSOLID") (-4 . "AND>"))) (setq 3DSolid (addVar 3DSolid)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "*ATT") (-4 . "AND>"))) (setq Attribute (addVar Attribute)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "ATTDEF") (-4 . "AND>"))) (setq AttributeDefinition (addVar AttributeDefinition)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "HATCH") (-4 . "AND>"))) (setq Hatch (addVar Hatch)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "HELIX") (-4 . "AND>"))) (setq Helix (addVar Helix)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "LEADER") (-4 . "AND>"))) (setq Leader (addVar Leader)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "MESH") (-4 . "AND>"))) (setq Mesh (addVar Mesh)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "MLINE") (-4 . "AND>"))) (setq MLine (addVar MLine)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "MTEXT") (-4 . "AND>"))) (setq MText (addVar MText)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "MULTILEADER") (-4 . "AND>"))) (setq Multileader (addVar Multileader)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "POLYLINE") (-4 . "AND>"))) (setq PolyfaceMesh (addVar PolyfaceMesh)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "POSITION_MAKER") (-4 . "AND>"))) (setq PositionMaker (addVar PositionMaker)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "REGION") (-4 . "AND>"))) (setq Region (addVar Region)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "SECTION_OBJECT") (-4 . "AND>"))) (setq SectionObject (addVar SectionObject)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "SURFACE") (-4 . "AND>"))) (setq Surface (addVar Surface)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "TABLE") (-4 . "AND>"))) (setq Table (addVar Table)))
  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "VIEWPORT") (-4 . "AND>"))) (setq Viewport (addVar Viewport)))
;  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (0 . "POLYLINE") (-4 . "AND>"))) (setq 3DPolyline (addVar 3DPolyine))) ; Covered by Polyface Mesh 
;  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (-1 . <Entity name: 7ffffb1baa0>) (-4 . "AND>"))) (setq AlignedConstraintParameter (addVar AlignedConstraintParameter))) ; Does not comply to any symbol
;  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (-1 . <Entity name: 7ffffb62600>) (-4 . "AND>"))) (setq Array (addVar Array))) ; Covered by Block Reference
;  (if (ssget "X" '((8 . "*-P-*") (0 . "INSERT"))) (setq BlockReference (addVar BlockReference)))
;  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (-1 . <Entity name: 7ffffb7be90>) (-4 . "AND>"))) (setq ExternalReference (addVar ExternalReference))) ; Covered by Block Reference
;  (if (ssget "X" '((8 . "*-P-*") (-4 . "<AND") (-1 . <Entity name: 7ffffb0cdf0>) (-4 . "AND>"))) (setq Parameter (addVar Parameter))) ; Does not comply to any symbol

  (setq string "")
  (foreach a (list Layer0 Non3DSolid 3DSolid Dimension Attribute AttributeDefinition BlockReference Hatch Helix Leader Mesh MLine MText Multileader PolyfaceMesh PositionMaker Region SectionObject Surface Table Viewport)
	(cond
      ((and (> (cadr a) 0) (= string "")) (setq string (strcat (car a))))
      ((and (> (cadr a) 0) (/= string "")) (setq string (strcat string ", " (car a))))
    )
  )
  (writeFile string)
  (saveFile string)
)

;; Prints or outputs the current date and time
;; bOutput = T - Output the string to the Command prompt
;; bOutput = nil - Return a formatted string MM/DD/YYYY  HH:MM:SS
;; Usage: (CurDate T)
;; Usage: (CurDate nil)
(defun StartDate (bOuput / cdate_val YYYY M D HH MM SS)
  ; Get the current date/time
  (setq cdate_val (rtos (getvar "CDATE") 2 6))

  ; Break up the string into its separate parts
  (setq YYYY (substr cdate_val 1 4)
        M    (substr cdate_val 5 2)
        D    (substr cdate_val 7 2)
        HH   (substr cdate_val 10 2)
        MM   (substr cdate_val 12 2)
        SS   (substr cdate_val 14 2)
  )
  (setq file (open "DESTINATION/Log_File.txt" "a"))
  (write-line (strcat "Start Time"
                      "\nDate:" M "/" D "/" YYYY
                      "\nTime:" HH ":" MM ":" SS
					  "\n"
					  "\n------------------------------------------------------------------------------------------"
					  "\n"
              ) file)
  (close file)
)

;; Prints or outputs the current date and time
;; bOutput = T - Output the string to the Command prompt
;; bOutput = nil - Return a formatted string MM/DD/YYYY  HH:MM:SS
;; Usage: (CurDate T)
;; Usage: (CurDate nil)
(defun EndDate (bOuput / cdate_val YYYY M D HH MM SS)
  ; Get the current date/time
  (setq cdate_val (rtos (getvar "CDATE") 2 6))

  ; Break up the string into its separate parts
  (setq YYYY (substr cdate_val 1 4)
        M    (substr cdate_val 5 2)
        D    (substr cdate_val 7 2)
        HH   (substr cdate_val 10 2)
        MM   (substr cdate_val 12 2)
        SS   (substr cdate_val 14 2)
  )

  (setq runTime (duration (atoi D) (atoi HH) (atoi MM) (atoi SS)))
    
  (if (setq f (vl-directory-files "DESTINATION/Errors" "*.dwg")) 
    (setq errors (length f))
	(if (= errors nil) (setq errors 0))
  )
  
  (if (setq g (vl-directory-files "DESTINATION/Checked" "*.dwg"))
    (setq processed (+ (length g) errors))
  )
  
  (setq file (open "DESTINATION/Log_File.txt" "a"))
  (write-line (strcat "\n"
					  "\n------------------------------------------------------------------------------------------"
					  "\nEnd Time"
                      "\nDate:" M "/" D "/" YYYY
                      "\nTime:" HH ":" MM ":" SS
					  "\nDuration:" (itoa (abs (car runTime))) ":" (itoa (abs (cadr runTime))) ":" (itoa (abs (caddr runTime)))
					  "\nNumber of Processed Drawings:" (itoa processed)
					  "\nNumber of Errors:" (itoa errors)
              ) file)
  (close file)
)

;; duration: Calculates the duration of the test
(defun duration (D1 HH1 MM1 SS1)
  (setq f (open "DESTINATION/Log_File.txt" "r")) ;-Opens the file to read
  (setq dataline (read-line f))                ;-Reads the header
  (setq dataline (read-line f))                ;-Reads the 1st data line

  (setq lst2 (cdr (LM:str->lst dataline "/")))
  (setq DAYS (abs (- (atoi (car lst2)) D1)))
  
  (setq dataline (read-line f))                ;-Reads the 2nd data line
  (setq lst (cdr (LM:str->lst dataline ":")))
  (close f)                                    ;-Close the file
  (setq HH2 (+ (* DAYS 24) (- HH1 (atoi (car lst)))))
  (setq MM2 (- (atoi (cadr lst)) MM1))
  (setq SS2 (- (atoi (caddr lst)) SS1))
  (setq runtime2 (list HH2 MM2 SS2))
)

;; String to List  -  Lee Mac
;; Separates a string using a given delimiter
;; str - [str] String to process
;; del - [str] Delimiter by which to separate the string
;; Returns: [lst] List of strings
;; Used in duration
(defun LM:str->lst ( str del / len lst pos )
    (setq len (1+ (strlen del)))
    (while (setq pos (vl-string-search del str))
        (setq lst (cons (substr str 1 pos) lst)
              str (substr str (+ pos len))
        )
    )
    (reverse (cons str lst))
)
