" Help for ranges handling - class def.
CLASS lcl_range DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: co_sign_component   TYPE string VALUE 'SIGN',
               co_sign_data_type   TYPE string VALUE 'DDSIGN',
               co_option_component TYPE string VALUE 'OPTION',
               co_option_data_type TYPE string VALUE 'DDOPTION',
               co_low_component    TYPE string VALUE 'LOW',
               co_high_component   TYPE string VALUE 'HIGH'.

    " Add new range line
    CLASS-METHODS add_range_line IMPORTING iv_sign   TYPE ddsign
                                           iv_option TYPE ddoption
                                           iv_low    TYPE any
                                           iv_high   TYPE any OPTIONAL
                                 CHANGING  ct_range  TYPE ANY TABLE.

    " Get range from selection screen parameters
    CLASS-METHODS get_range_from_sap IMPORTING iv_parameter_name TYPE rsscr_name
                                               it_parameters     TYPE rsparams_tt
                                     CHANGING  ct_range          TYPE ANY TABLE.
ENDCLASS.

" Help for ranges handling - class imp.
CLASS lcl_range IMPLEMENTATION.
  METHOD add_range_line.

    DATA: range_line TYPE REF TO data,
          line_type  TYPE REF TO cl_abap_structdescr,
          table_type TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <range>     TYPE any,
                   <component> TYPE any.

    table_type ?= cl_abap_typedescr=>describe_by_data( ct_range ).
    line_type  ?= table_type->get_table_line_type( ).

    " Create range line
    CREATE DATA range_line TYPE HANDLE line_type.
    ASSIGN range_line->* TO <range>.

    " Set values for sign,option, low and high.
    ASSIGN COMPONENT co_sign_component OF STRUCTURE <range> TO <component>.
    IF sy-subrc EQ 0.
      MOVE iv_sign TO <component>.
    ENDIF.
    ASSIGN COMPONENT co_option_component OF STRUCTURE <range> TO <component>.
    IF sy-subrc EQ 0.
      MOVE iv_option TO <component>.
    ENDIF.
    ASSIGN COMPONENT co_low_component OF STRUCTURE <range> TO <component>.
    IF sy-subrc EQ 0.
      MOVE iv_low TO <component>.
    ENDIF.
    IF iv_high IS SUPPLIED AND iv_high IS NOT INITIAL.
      ASSIGN COMPONENT co_high_component OF STRUCTURE <range> TO <component>.
      IF sy-subrc EQ 0.
        MOVE iv_high TO <component>.
      ENDIF.
    ENDIF.

    " Insert only unique rows
    INSERT <range> INTO TABLE ct_range.
    SORT ct_range.
    DELETE ADJACENT DUPLICATES FROM ct_range.

  ENDMETHOD.

  METHOD get_range_from_sap.
    FIELD-SYMBOLS <parameter> LIKE LINE OF it_parameters.

    LOOP AT it_parameters ASSIGNING <parameter> WHERE selname EQ iv_parameter_name AND
                                                      kind    IS NOT INITIAL AND
                                                      sign    IS NOT INITIAL AND
                                                      option  IS NOT INITIAL.
      " Insert new unique row into range
      add_range_line( EXPORTING iv_sign   = <parameter>-sign
                                iv_option = <parameter>-option
                                iv_low    = <parameter>-low
                                iv_high   = <parameter>-high
                      CHANGING  ct_range  = ct_range ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

FUNCTION yourviewer .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DB_TABLE) TYPE  ALTTAB
*"     VALUE(ONLY_DATA) TYPE  BOOLE_D DEFAULT 'X'
*"     VALUE(COLUMNS_LIST_TO_DISPLAY) TYPE  STRING OPTIONAL
*"     VALUE(MAX_NUMBER_OF_ROWS) TYPE  INT4 DEFAULT 100
*"     VALUE(CRITERIA_TABLE) TYPE  RSPARAMS_TT OPTIONAL
*"     VALUE(ADDITIONAL_COMMAND) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     VALUE(FIELDS_DEFINITION) TYPE  DFIES_TABLE
*"     VALUE(FIELDS_LIST) TYPE  STRING
*"     VALUE(KEY_FIELDS_LIST) TYPE  STRING
*"     VALUE(ROWS_NUMBER) TYPE  INT4
*"     VALUE(DATASET) TYPE  STRINGTAB
*"     VALUE(DATA_ORIGIN) TYPE  STRING
*"     VALUE(DATE_TIME) TYPE  STRING
*"     VALUE(PROCESSING_TIME_MS) TYPE  INT4
*"----------------------------------------------------------------------
  TYPES: BEGIN OF range_list_s,
           id    TYPE tvarv_val,
           range TYPE REF TO data,
         END OF range_list_s,
  range_list_t TYPE STANDARD TABLE OF range_list_s.

  CONSTANTS: final_line_length TYPE i           VALUE 512,
             data_separator    TYPE c           VALUE `|`,
             dash              TYPE c           VALUE `-`,
             underscore        TYPE c           VALUE `_`,
             event_type        TYPE dbglevtype  VALUE `FUNC`,
             event_name        TYPE dbglevent   VALUE `YOURVIEWER`.

  DATA: columns_list          TYPE STANDARD TABLE OF string,
        range_components_row  TYPE cl_abap_structdescr=>component,
        range_components_tab  TYPE cl_abap_structdescr=>component_table,
        range_list_row        TYPE range_list_s,
        range_list_tab        TYPE range_list_t,
        main_criteria_tab     TYPE rsparams_tt,
        rtts_components_row   TYPE abap_compdescr,
        call_stack            TYPE sys_callst,
        call_stack_row        TYPE sys_calls,
        range_half_template   LIKE range_components_tab,
        criteria_tab          LIKE criteria_table,
        criteria_row          LIKE LINE OF criteria_tab,
        temp_columns_list     LIKE columns_list,
        field_info            TYPE dfies,
        fields_info           TYPE dfies_tab,
        timer                 TYPE REF TO if_abap_runtime,
        range_tab_type        TYPE REF TO cl_abap_tabledescr,
        final_tab_type        TYPE REF TO cl_abap_tabledescr,
        final_structure_type  TYPE REF TO cl_abap_structdescr,
        range_row_type        TYPE REF TO cl_abap_datadescr,
        final_row_type        TYPE REF TO cl_abap_datadescr,
        range_vector_type     TYPE REF TO cl_abap_datadescr,
        range_data            TYPE REF TO data,
        final_data            TYPE REF TO data,
        vector_of_ranges      TYPE REF TO data,
        first_value           TYPE abap_bool,
        sql_where_clause      TYPE string,
        temp_string           TYPE string,
        final_string          TYPE string,
        final_line            TYPE string,
        column_name           TYPE string,
        us_time               TYPE p LENGTH 16,
        text_offset           TYPE i,
        text_length           TYPE i,
        start_time            TYPE i,
        end_time              TYPE i.

  FIELD-SYMBOLS: <range_table> TYPE STANDARD TABLE,
                 <final_table> TYPE STANDARD TABLE,
                 <range>       TYPE REF TO data,
                 <range_list>  LIKE range_list_row,
                 <final_row>   TYPE any,
                 <field>       TYPE any,
                 <x>           TYPE any.

  " Get only fields definition
  IF db_table IS NOT INITIAL AND only_data EQ abap_false.
    final_structure_type ?= cl_abap_typedescr=>describe_by_name( db_table ).
    APPEND LINES OF final_structure_type->get_ddic_field_list( ) TO fields_definition.
    RETURN.
  ENDIF.

  " Prepare time measurement
  timer = cl_abap_runtime=>create_hr_timer( ).
  start_time = timer->get_runtime( ).

  " Call internal command
  IF additional_command IS NOT INITIAL.

    " Get calls stack
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        et_callstack = call_stack.

    IF call_stack IS NOT INITIAL.
      READ TABLE call_stack INTO call_stack_row WITH KEY eventtype = event_type eventname = event_name.
      IF sy-subrc EQ 0.
        TRANSLATE additional_command TO UPPER CASE.
        PERFORM (additional_command) IN PROGRAM (call_stack_row-progname) IF FOUND
        USING db_table data_separator columns_list_to_display max_number_of_rows criteria_table additional_command
        CHANGING fields_list key_fields_list rows_number dataset.
      ENDIF.
    ENDIF.

    " Final spec.
    end_time = timer->get_runtime( ).
    us_time = end_time - start_time.
    processing_time_ms = us_time / 1000.
    CONCATENATE sy-sysid underscore sy-mandt INTO data_origin.
    CONCATENATE sy-datum underscore sy-uzeit INTO date_time.

    RETURN.
  ENDIF.

  " We need tabular object's name
  IF db_table IS INITIAL.
    RETURN.
  ENDIF.

  " Prepare columns list
  TRANSLATE columns_list_to_display TO UPPER CASE.
  SPLIT columns_list_to_display AT data_separator INTO TABLE columns_list.
  DELETE columns_list WHERE table_line IS INITIAL.

  " Prepare criteria
  " main_criteria_tab has only names of components
  " criteria_tab has values for this components
  criteria_tab = criteria_table.
  LOOP AT criteria_tab INTO criteria_row WHERE kind IS INITIAL AND sign IS INITIAL AND option IS INITIAL.
    APPEND criteria_row TO main_criteria_tab.
  ENDLOOP.
  DELETE criteria_tab WHERE kind IS INITIAL AND sign IS INITIAL AND option IS INITIAL.

  " Build range tables from tables with values of criteria
  IF criteria_tab IS NOT INITIAL.

    " Sign and option have always the same type
    " we can remember that as template
    range_components_row-name = lcl_range=>co_sign_component.
    range_components_row-type ?= cl_abap_datadescr=>describe_by_name( lcl_range=>co_sign_data_type ).
    APPEND range_components_row TO range_components_tab.
    range_components_row-name = lcl_range=>co_option_component.
    range_components_row-type ?= cl_abap_datadescr=>describe_by_name( lcl_range=>co_option_data_type ).
    APPEND range_components_row TO range_components_tab.
    range_half_template = range_components_tab.

    LOOP AT main_criteria_tab INTO criteria_row.

      " Build rest of the range (low + high)
      CONCATENATE db_table dash criteria_row-low INTO temp_string.
      range_components_row-name = lcl_range=>co_low_component.
      range_components_row-type ?= cl_abap_datadescr=>describe_by_name( temp_string ).
      APPEND range_components_row TO range_components_tab.
      range_components_row-name = lcl_range=>co_high_component.
      APPEND range_components_row TO range_components_tab.
      range_row_type ?= cl_abap_structdescr=>create( p_components = range_components_tab ).
      range_tab_type = cl_abap_tabledescr=>create( p_line_type = range_row_type ).
      CREATE DATA range_data TYPE HANDLE range_tab_type.
      ASSIGN range_data->* TO <range_table>.

      " Retrive from criteria values for range table
      CALL METHOD lcl_range=>get_range_from_sap
        EXPORTING
          iv_parameter_name = criteria_row-selname
          it_parameters     = criteria_tab
        CHANGING
          ct_range          = <range_table>.

      " Add new range to list of ranges
      range_list_row-id = criteria_row-low.
      range_list_row-range = range_data.
      APPEND range_list_row TO range_list_tab.

      " Prepare first part of new range
      range_components_tab = range_half_template.
    ENDLOOP.

    " Prepare type of range's vector
    CLEAR range_components_tab.
    LOOP AT range_list_tab ASSIGNING <range_list>.
      range_components_row-name = <range_list>-id.
      range_components_row-type ?= cl_abap_datadescr=>describe_by_data( <range_list>-range ).
      APPEND range_components_row TO range_components_tab.
    ENDLOOP.

    " Create range's vector data object and assign it to <x> field symbol.
    range_vector_type ?= cl_abap_structdescr=>create( p_components = range_components_tab ).
    CREATE DATA vector_of_ranges TYPE HANDLE range_vector_type.
    ASSIGN vector_of_ranges->* TO <x>.

    " Build sql where clause using refernces to our <x> vector.
    LOOP AT range_list_tab ASSIGNING <range_list>.
      ASSIGN COMPONENT <range_list>-id OF STRUCTURE <x> TO <range>.
      <range> = <range_list>-range.
      CONCATENATE <range_list>-id ` IN <X>-` <range_list>-id `->* AND ` INTO temp_string.
      CONCATENATE sql_where_clause temp_string INTO sql_where_clause RESPECTING BLANKS.
    ENDLOOP.

    " Remove last AND from sql clause
    sy-fdpos = strlen( sql_where_clause ) - 4.
    sql_where_clause = sql_where_clause(sy-fdpos).
  ENDIF.

  " Create tabular data object for final dataset
  final_row_type ?= cl_abap_tabledescr=>describe_by_name( db_table ).
  final_tab_type = cl_abap_tabledescr=>create( p_line_type = final_row_type ).
  CREATE DATA final_data TYPE HANDLE final_tab_type.
  ASSIGN final_data->* TO <final_table>.

  " App. ItIsTable did not ask for specific columns so we will show everything.
  final_structure_type ?= final_tab_type->get_table_line_type( ).
  IF columns_list_to_display IS INITIAL.
    LOOP AT final_structure_type->components INTO rtts_components_row.
      IF fields_list IS INITIAL.
        CONCATENATE fields_list rtts_components_row-name INTO fields_list.
      ELSE.
        CONCATENATE fields_list rtts_components_row-name INTO fields_list SEPARATED BY data_separator.
      ENDIF.
      APPEND rtts_components_row-name TO temp_columns_list.
    ENDLOOP.
  ENDIF.

  " Get data from db table
  SELECT *
    BYPASSING BUFFER
    UP TO max_number_of_rows ROWS
    INTO TABLE <final_table>
    FROM (db_table)
    WHERE (sql_where_clause).

  " Number of retrieved rows, source system, date and time of dataset
  rows_number = sy-dbcnt.
  CONCATENATE sy-sysid underscore sy-mandt INTO data_origin.
  CONCATENATE sy-datum underscore sy-uzeit INTO date_time.

  LOOP AT <final_table> ASSIGNING <final_row>.

    " Prepare final columns list
    AT FIRST.
      LOOP AT columns_list INTO column_name.
        ASSIGN COMPONENT column_name OF STRUCTURE <final_row> TO <field>.
        CHECK sy-subrc EQ 0.
        IF fields_list IS INITIAL.
          CONCATENATE fields_list column_name INTO fields_list.
        ELSE.
          CONCATENATE fields_list column_name INTO fields_list SEPARATED BY data_separator.
        ENDIF.
        APPEND column_name TO temp_columns_list.
      ENDLOOP.
      columns_list = temp_columns_list.
    ENDAT.

    " Separate data with passed data separator
    " Each occurence of data separator in dataset will be removed!
    first_value = abap_false.
    LOOP AT columns_list INTO column_name.
      ASSIGN COMPONENT column_name OF STRUCTURE <final_row> TO <field>.
      CHECK sy-subrc EQ 0.
      temp_string = <field>.
      CONDENSE temp_string.
      REPLACE ALL OCCURRENCES OF data_separator IN temp_string WITH ``.
      IF first_value EQ abap_false.
        final_string = temp_string.
        first_value = abap_true.
      ELSE.
        CONCATENATE final_string temp_string INTO final_string SEPARATED BY data_separator.
      ENDIF.
    ENDLOOP.

    APPEND final_string TO dataset.
    CLEAR final_string.
  ENDLOOP.

  " There was no data but fields list must be filled
  IF dataset IS INITIAL AND columns_list_to_display IS NOT INITIAL.
    fields_list = columns_list_to_display.
  ENDIF.

  " Get keys
  fields_info = final_structure_type->get_ddic_field_list( ).
  LOOP AT fields_info INTO field_info WHERE keyflag EQ abap_true.
    IF key_fields_list IS INITIAL.
      key_fields_list = field_info-fieldname.
    ELSE.
      CONCATENATE key_fields_list field_info-fieldname
      INTO key_fields_list SEPARATED BY data_separator.
    ENDIF.
  ENDLOOP.

  " Calculate processing time in [us]
  end_time = timer->get_runtime( ).
  us_time = end_time - start_time.
  processing_time_ms = us_time / 1000.

ENDFUNCTION.

" Subprogram for ItIsTable internal use.
" It generates list of tabular objects that can be used in ItIsTable app.
FORM iit_get_tabular_objects USING db_table                TYPE alttab
                                   data_separator          TYPE char1
                                   columns_list_to_display TYPE string
                                   max_number_of_rows      TYPE int4
                                   criteria_table          TYPE rsparams_tt
                                   additional_command      TYPE string
                             CHANGING fields_list          TYPE string
                                      key_fields_list      TYPE string
                                      rows_number          TYPE int4
                                      dataset              TYPE stringtab.
  TYPES: BEGIN OF dd02l_row,
    tabname   TYPE tadir-obj_name,
    tabclass  TYPE dd02l-tabclass,
    viewclass TYPE dd02l-viewclass,
  END OF dd02l_row,
  dd02l_tab TYPE HASHED TABLE OF dd02l_row WITH UNIQUE KEY tabname.

  TYPES: BEGIN OF final_row,
    obj_name   TYPE dd02t-tabname,
    devclass   TYPE tadir-devclass,
    masterlang TYPE tadir-masterlang,
  END OF final_row,
  final_tab TYPE HASHED TABLE OF final_row WITH UNIQUE KEY obj_name.

  TYPES: BEGIN OF dd02t_row,
    tabname TYPE dd02t-tabname,
    ddtext  TYPE dd02t-ddtext,
  END OF dd02t_row,
  dd02t_tab TYPE HASHED TABLE OF dd02t_row WITH UNIQUE KEY tabname.

  TYPES: BEGIN OF csv_row,
    object_type TYPE string,
    object_name TYPE string,
    object_text TYPE string,
    package     TYPE string,
  END OF csv_row.

  DATA: tables TYPE dd02l_tab,
        views  TYPE dd02l_tab,
        repo_tables TYPE final_tab,
        repo_views  TYPE final_tab,
        csv_row     TYPE csv_row,
        csv_line    TYPE string,
        tables_eng_texts TYPE dd02t_tab,
        tables_org_texts TYPE dd02t_tab,
        views_eng_texts  TYPE dd02t_tab,
        views_org_texts  TYPE dd02t_tab.

  FIELD-SYMBOLS: <final_row> TYPE final_row,
                 <dd02l_row> TYPE dd02l_row,
                 <obj_text>  TYPE dd02t_row,
                 <message>   TYPE bapiret2.

  " Transparent, cluster and pooled tables
  SELECT tabname tabclass
    FROM dd02l
    INTO TABLE tables
    WHERE as4local EQ 'A' AND
          tabclass IN ('TRANSP','CLUSTER','POOL') AND
          actflag  EQ ''.

  IF sy-subrc EQ 0.

    " Table's package and orginal language
    SELECT obj_name devclass masterlang
      FROM tadir
      INTO TABLE repo_tables
      FOR ALL ENTRIES IN tables
      WHERE pgmid    EQ 'R3TR' AND
            object   EQ 'TABL' AND
            obj_name EQ tables-tabname.

    IF sy-subrc EQ 0.

      " Table's english description
      SELECT tabname ddtext
        FROM dd02t
        INTO TABLE tables_eng_texts
        FOR ALL ENTRIES IN repo_tables
        WHERE tabname    EQ repo_tables-obj_name AND
              ddlanguage EQ 'EN'                 AND
              as4local   EQ 'A'.

      " Table's orginal description
      SELECT tabname ddtext
        FROM dd02t
        INTO TABLE tables_org_texts
        FOR ALL ENTRIES IN repo_tables
        WHERE tabname    EQ repo_tables-obj_name   AND
              ddlanguage EQ repo_tables-masterlang AND
              ddlanguage NE 'EN' AND
              as4local   EQ 'A'.
    ENDIF.
  ENDIF.

  " Prepare final data
  LOOP AT repo_tables ASSIGNING <final_row>.
    UNASSIGN: <dd02l_row>, <obj_text>.
    CLEAR: csv_row, csv_line.

    READ TABLE tables ASSIGNING <dd02l_row> WITH KEY tabname = <final_row>-obj_name.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    " English or orginal description
    READ TABLE tables_eng_texts ASSIGNING <obj_text> WITH KEY tabname = <final_row>-obj_name.
    IF sy-subrc NE 0.
      READ TABLE tables_org_texts ASSIGNING <obj_text> WITH KEY tabname = <final_row>-obj_name.
    ENDIF.

    " Object type, T - transparent, C - cluester, P - pool
    csv_row-object_type = <dd02l_row>-tabclass(1).
    csv_row-object_name = <dd02l_row>-tabname.
    csv_row-package     = <final_row>-devclass.
    IF <obj_text> IS ASSIGNED.
      csv_row-object_text = <obj_text>-ddtext.
    ENDIF.
    CONCATENATE csv_row-object_type csv_row-object_name csv_row-package csv_row-object_text
    INTO csv_line SEPARATED BY data_separator.
    APPEND csv_line TO dataset.
  ENDLOOP.
  CLEAR: tables,repo_tables,tables_eng_texts,tables_org_texts.

  " Database views (D) and Projection views (P)
  SELECT tabname tabclass viewclass
    FROM dd02l
    INTO TABLE views
    WHERE as4local  EQ 'A' AND
          tabclass  EQ 'VIEW' AND
          actflag   EQ '' AND
          viewclass IN ('D','P').

  IF sy-subrc EQ 0.

    " View's package and orginal language
    SELECT obj_name devclass masterlang
      FROM tadir
      INTO TABLE repo_views
      FOR ALL ENTRIES IN views
      WHERE pgmid    EQ 'R3TR' AND
            object   EQ 'VIEW' AND
            obj_name EQ views-tabname.

    IF sy-subrc EQ 0.

      " View's english description
      SELECT tabname ddtext
        FROM dd02t
        INTO TABLE views_eng_texts
        FOR ALL ENTRIES IN repo_views
        WHERE tabname    EQ repo_views-obj_name AND
              ddlanguage EQ 'EN'                AND
              as4local   EQ 'A'.

      " View's orginal description
      SELECT tabname ddtext
        FROM dd02t
        INTO TABLE views_org_texts
        FOR ALL ENTRIES IN repo_views
        WHERE tabname    EQ repo_views-obj_name   AND
              ddlanguage EQ repo_views-masterlang AND
              ddlanguage NE 'EN' AND
              as4local   EQ 'A'.
    ENDIF.
  ENDIF.

  " Prepare final data
  LOOP AT repo_views ASSIGNING <final_row>.
    UNASSIGN: <dd02l_row>, <obj_text>.
    CLEAR: csv_row, csv_line.

    READ TABLE views ASSIGNING <dd02l_row> WITH KEY tabname = <final_row>-obj_name.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    " English or orginal description
    READ TABLE views_eng_texts ASSIGNING <obj_text> WITH KEY tabname = <final_row>-obj_name.
    IF sy-subrc NE 0.
      READ TABLE views_org_texts ASSIGNING <obj_text> WITH KEY tabname = <final_row>-obj_name.
    ENDIF.

    " Object type, VP - Projection view, VD - Database view
    CONCATENATE <dd02l_row>-tabclass(1) <dd02l_row>-viewclass INTO csv_row-object_type.
    csv_row-object_name = <dd02l_row>-tabname.
    csv_row-package     = <final_row>-devclass.
    IF <obj_text> IS ASSIGNED.
      csv_row-object_text = <obj_text>-ddtext.
    ENDIF.
    CONCATENATE csv_row-object_type csv_row-object_name csv_row-package csv_row-object_text
    INTO csv_line SEPARATED BY data_separator.
    APPEND csv_line TO dataset.
  ENDLOOP.

  " Number of tabulars
  DESCRIBE TABLE dataset LINES rows_number.

  " Columns
  CONCATENATE 'TYPE' 'NAME' 'PACKAGE' 'DESC' INTO fields_list SEPARATED BY data_separator.
  key_fields_list = 'NAME'.

ENDFORM.
