# yourviewer
Yourviewer is an RFC function designed to work with <a href="https://www.poloniumsoftware.com/itistable">ItIsTable application</a>

Below you can find how simple script from ItIsTable can be processed directly in the SAP system.

*  ItIsTable script
*  ----------------
*  [OBJECT]
*  IEQ PROG
*  [DEVCLASS]
*  ICP *DEMOS*
*
*  Criteria table in YourViewer rfc function
*  ---------------------------------------------------
*  | SELNAME  | KIND | SIGN | OPTION | LOW     | HIGH|
*  ---------------------------------------------------
*  | 00000001 |    |    |    | OBJECT   |     |
*  | 00000001 | P  | I  | EQ | PROG     |     |
*  | 00000002 |    |    |    | DEVCLASS |     |
*  | 00000002 | P  | I  | CP | *DEMOS*  |     |
*  ---------------------------------------------------

DATA: criteria     TYPE rsparams_tt,
      criteria_row LIKE LINE OF criteria,
      results      TYPE stringtab.

criteria_row-selname = '00000001'.
criteria_row-low = 'OBJECT'.
APPEND criteria_row TO criteria.
criteria_row-kind = 'P'.
criteria_row-sign = 'I'.
criteria_row-option = 'EQ'.
criteria_row-low = 'PROG'.
APPEND criteria_row TO criteria.
CLEAR criteria_row.

criteria_row-selname = '00000002'.
criteria_row-low = 'DEVCLASS'.
APPEND criteria_row TO criteria.
criteria_row-kind = 'P'.
criteria_row-sign = 'I'.
criteria_row-option = 'CP'.
criteria_row-low = '*DEMOS*'.
APPEND criteria_row TO criteria.
CLEAR criteria_row.

" Get 10 rows from TADIR for specified criteria
CALL FUNCTION 'YOURVIEWER'
  EXPORTING
    db_table                = 'TADIR'
    columns_list_to_display = 'PGMID|OBJECT|OBJ_NAME'
    max_number_of_rows      = 10
    criteria_table          = criteria
  IMPORTING
    dataset                 = results.

*  " Returns something like that in the results table
*  R3TR|PROG|ABAP_OBJECTS_ENJOY_0
*  R3TR|PROG|ABAP_OBJECTS_ENJOY_1
*  R3TR|PROG|ABAP_OBJECTS_ENJOY_2
*  R3TR|PROG|ABAP_OBJECTS_ENJOY_3
*  R3TR|PROG|ABAP_OBJECTS_ENJOY_4
*  R3TR|PROG|ABAP_OBJECTS_ENJOY_5
*  R3TR|PROG|DEMO_ABAP_OBJECTS
*  R3TR|PROG|DEMO_ABAP_OBJECTS_EVENTS
*  R3TR|PROG|DEMO_ABAP_OBJECTS_GENERAL
*  R3TR|PROG|DEMO_ABAP_OBJECTS_METHODS

" Just get 1k rows from BSEG
CALL FUNCTION 'YOURVIEWER'
  EXPORTING
    db_table = 'BSEG'
  IMPORTING
    dataset  = results.
