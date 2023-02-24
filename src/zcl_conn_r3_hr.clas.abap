class ZCL_CONN_R3_HR definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_s_conn,
        rfc    TYPE  rfcdest,
        dbco   TYPE  dbcon_name,
        schema TYPE  string,
        mandt  TYPE  mandt,
        syst   TYPE  sysysid,
      END OF ty_s_conn .

  data V_CONN type TY_S_CONN .

  methods DELETE_TABLE
    importing
      value(IP_TABNAME) type TABNAME
      value(IT_DATA) type STANDARD TABLE
    returning
      value(R_OK) type ABAP_BOOL .
  methods DELETE_TABLE_RFC
    importing
      value(IP_TABNAME) type TABNAME
      value(IT_DATA) type STANDARD TABLE
    returning
      value(R_OK) type ABAP_BOOL
    exceptions
      TABLE_NOT_AVAILABLE
      TABLE_WITHOUT_DATA
      OPTION_NOT_VALID
      NOT_AUTHORIZED
      RFC_OTHERS
      RFC_NOT_AVAILABLE .
  methods GET_TABLE
    importing
      value(IP_TABNAME) type TABNAME
      value(IP_OPT_WHERE) type ESH_T_CO_RFCRT_OPTIONS optional
      value(IP_SELECTION) type GUSL_T_SELECTION optional
    changing
      value(IT_DATA) type STANDARD TABLE optional
    returning
      value(R_OK) type ABAP_BOOL .
  methods GET_TABLE_RFC
    importing
      value(IP_TABNAME) type TABNAME
      value(IP_OPT_WHERE) type ESH_T_CO_RFCRT_OPTIONS optional
      value(IP_SELECTION) type GUSL_T_SELECTION optional
    changing
      value(IT_DATA) type STANDARD TABLE
    returning
      value(R_OK) type ABAP_BOOL
    exceptions
      TABLE_NOT_AVAILABLE
      TABLE_WITHOUT_DATA
      OPTION_NOT_VALID
      NOT_AUTHORIZED
      RFC_OTHERS
      RFC_NOT_AVAILABLE .
  methods GET_TABLE_SQL
    importing
      value(IP_TABNAME) type TABNAME
      value(IP_OPT_WHERE) type ESH_T_CO_RFCRT_OPTIONS optional
      value(IP_SELECTION) type GUSL_T_SELECTION optional
    changing
      value(IT_DATA) type STANDARD TABLE
    returning
      value(R_OK) type ABAP_BOOL .
  methods SHOW_MESSAGES
    importing
      value(IP_EXIT) type ABAP_BOOL default ABAP_TRUE
      value(IP_ON_ERROR) type ABAP_BOOL default ABAP_TRUE .
  methods UPDATE_TABLE
    importing
      value(IP_TABNAME) type TABNAME
      value(IT_DATA) type STANDARD TABLE
      value(IT_DELETE_DATA) type STANDARD TABLE optional
    returning
      value(R_OK) type ABAP_BOOL .
  methods UPDATE_TABLE_RFC
    importing
      value(IP_TABNAME) type TABNAME
      value(IT_DATA) type STANDARD TABLE
      value(IT_DELETE_DATA) type STANDARD TABLE optional
    returning
      value(R_OK) type ABAP_BOOL
    exceptions
      TABLE_NOT_AVAILABLE
      TABLE_WITHOUT_DATA
      OPTION_NOT_VALID
      NOT_AUTHORIZED
      RFC_OTHERS
      RFC_NOT_AVAILABLE .
  class-methods ADD_FIELD_2_SELECTION
    importing
      value(IP_FIELDNAME) type STRING
      value(IP_SIGN) type STRING default 'I'
      value(IP_OPTION) type STRING default 'EQ'
      value(IP_LOW) type ANY
      value(IP_HIGH) type ANY optional
    changing
      value(IOT_SELECTION) type GUSL_T_SELECTION .
  class-methods ADD_RANGE_2_SELECTION
    importing
      value(IP_FIELDNAME) type STRING
      value(IT_RANGE) type STANDARD TABLE
    changing
      value(IOT_SELECTION) type GUSL_T_SELECTION .
  class-methods SELECTION_2_WHERE
    importing
      value(IP_TABNAME) type TABNAME
      value(IP_SELECTION) type GUSL_T_SELECTION
    returning
      value(RT_WHERE) type RSDS_WHERE_TAB .
  class-methods SELECTION_2_WHERE_SQL
    importing
      value(IP_TABNAME) type TABNAME
      value(IP_SELECTION) type GUSL_T_SELECTION
    returning
      value(R_WHERE) type STRING .
  methods CONSTRUCTOR
    importing
      value(IP_DESTINO) type CHAR2 optional
      value(IP_REPID) type SY-REPID default SY-REPID
      value(IP_TCODE) type SY-TCODE default SY-TCODE
      value(IO_LOGGER) type ref to ZCL_ALOG_MSG_LOGGER_BASE optional .
  class-methods CLASS_CONSTRUCTOR .
  methods CLEAR_TABLE
    importing
      value(IP_TABNAME) type TABNAME
    returning
      value(R_OK) type ABAP_BOOL .
  methods CLEAR_TABLE_SQL
    importing
      value(IP_TABNAME) type TABNAME
    returning
      value(R_OK) type ABAP_BOOL .
  methods GET_TABLE_SRQL
    importing
      value(IP_TABNAME) type TABNAME
      value(IP_WHERE) type STRING optional
      value(IT_SELECTION) type ZSRQLSOQUERY_TT optional
      value(IT_FIELDS) type FIELDNAME_TAB optional
      value(IP_DDIC_STRUCTURE) type STRUKNAME optional
    returning
      value(OR_DATA) type ref to DATA .
  methods GET_MESSAGES
    returning
      value(RT_MESSAGES) type BAPIRET2_TAB .
protected section.
private section.

  data V_TCODE type SY-TCODE .
  data V_REPID type SY-REPID .
  class-data V_CONN_2_HR type TY_S_CONN .
  class-data V_CONN_2_R3 type TY_S_CONN .
  data T_CONN_BAPIRET2 type BAPIRET2_TAB .
  data O_SQL type ref to ZCL_GA_SQL .
  data O_LOGGER type ref to ZCL_ALOG_MSG_LOGGER_BASE .

  methods ADD_LOG
    importing
      value(IP_TABNAME) type TABNAME
      value(IP_TECH) type ZBCDBCOCONN_TABS-TECH
      value(IP_MODO) type ZBCDBCOCONN_TABS-MODO .
  methods ADD_MESSAGE
    importing
      value(IP_TYPE) type BAPIRET2-TYPE
      value(IP_ID) type BAPIRET2-ID default 'ZP'
      value(IP_NO) type BAPIRET2-NUMBER default 999
      value(IP_MESS) type BAPIRET2-MESSAGE optional
      value(IP_VAR1) type BAPIRET2-MESSAGE_V1 optional
      value(IP_VAR2) type BAPIRET2-MESSAGE_V2 optional
      value(IP_VAR3) type BAPIRET2-MESSAGE_V3 optional
      value(IP_VAR4) type BAPIRET2-MESSAGE_V4 optional .
  methods CHECK_RFC_EXISTS
    importing
      value(RFC) type RS38L-NAME
    returning
      value(R_OK) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_CONN_R3_HR IMPLEMENTATION.


  METHOD add_field_2_selection.
    DATA: ls_selection TYPE gusl_s_selection.
    DATA: ls_range     TYPE gusl_s_range.
    FIELD-SYMBOLS <selection> TYPE gusl_s_selection.

    TRANSLATE: ip_fieldname TO UPPER CASE,
               ip_sign TO UPPER CASE,
               ip_option TO UPPER CASE.

    READ TABLE iot_selection ASSIGNING <selection>
    WITH KEY fieldname = ip_fieldname.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO iot_selection ASSIGNING <selection>.
      <selection>-fieldname = ip_fieldname.
    ENDIF.
*    ls_range-sign = ip_sign.
*    ls_range-option = ip_option.
*    ls_range-low = ip_low.
*    ls_range-high = ip_high.
*    APPEND ls_range TO <selection>-t_range.
    <selection>-t_range = VALUE #( BASE <selection>-t_range
                                   ( sign = ip_sign
                                     option = ip_option
                                     low = ip_low
                                     high = ip_high
                                   )
                                  ).

  ENDMETHOD.


  METHOD add_log.
    DATA: ls_conn TYPE zbcdbcoconn_tabs.
    ls_conn-tabname = ip_tabname.
    ls_conn-systori = sy-sysid.
    ls_conn-mandtori  = sy-mandt.
    ls_conn-systdes = v_conn-syst.
    ls_conn-mandtdes  = v_conn-mandt.
    ls_conn-report = v_repid.
    ls_conn-tcode = v_tcode.
    ls_conn-tech  = ip_tech.
    ls_conn-modo  = ip_modo.
    MODIFY zbcdbcoconn_tabs FROM ls_conn.
  ENDMETHOD.


  METHOD add_message.
    t_conn_bapiret2 = VALUE #( BASE t_conn_bapiret2
      (
      type = ip_type
      id = ip_id
      number = ip_no
      message = ip_mess
      message_v1 = ip_var1
      message_v2 = ip_var2
      message_v3 = ip_var3
      message_v4 = ip_var4
      )
    ).
  ENDMETHOD.


  METHOD ADD_RANGE_2_SELECTION.
    DATA: ls_selection TYPE gusl_s_selection,
          ls_range     TYPE gusl_s_range.
    FIELD-SYMBOLS: <lf_field> TYPE any.
    ls_selection-fieldname = to_upper( ip_fieldname ).

    LOOP AT it_range ASSIGNING FIELD-SYMBOL(<lf_aux>).
      CLEAR ls_range.
*      MOVE-CORRESPONDING ls_aux TO ls_range.
      UNASSIGN <lf_field>.
      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <lf_aux> TO <lf_field>.
      IF <lf_field> IS ASSIGNED.
        ls_range-sign = <lf_field>.
      ENDIF.
      UNASSIGN <lf_field>.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <lf_aux> TO <lf_field>.
      IF <lf_field> IS ASSIGNED.
        ls_range-option = <lf_field>.
      ENDIF.
      UNASSIGN <lf_field>.
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <lf_aux> TO <lf_field>.
      IF <lf_field> IS ASSIGNED.
        ls_range-low = <lf_field>.
      ENDIF.
      UNASSIGN <lf_field>.
      ASSIGN COMPONENT 'HIGH' OF STRUCTURE <lf_aux> TO <lf_field>.
      IF <lf_field> IS ASSIGNED.
        ls_range-high = <lf_field>.
      ENDIF.


      APPEND ls_range TO ls_selection-t_range.
    ENDLOOP.

    IF ls_selection-t_range IS NOT INITIAL.
      APPEND ls_selection TO iot_selection.
    ENDIF.


  ENDMETHOD.


  METHOD check_rfc_exists.
    r_ok = abap_true.
    CALL FUNCTION 'FUNCTION_EXISTS' DESTINATION v_conn-rfc
      EXPORTING
        funcname           = rfc
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      r_ok = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD class_constructor.

    DATA: ld_valor TYPE zhrhard-valor.
    SELECT SINGLE valor INTO ld_valor
    FROM zhrhard
    WHERE programa = 'ZIN_NOHR_2_HR'
    AND parametro = 'VULSHR2HR'.
    IF sy-subrc = 0.
      SPLIT ld_valor AT '-' INTO v_conn_2_hr-dbco
      v_conn_2_hr-mandt
      v_conn_2_hr-rfc
      v_conn_2_hr-syst
      v_conn_2_hr-schema.
    ELSE.
      CASE sy-sysid.
        WHEN 'TST'.
          v_conn_2_hr-dbco = 'GANDBHRD'.
          v_conn_2_hr-mandt = '020'.
          v_conn_2_hr-rfc = 'HRD'.
          v_conn_2_hr-syst = 'HRD'.
          v_conn_2_hr-schema = 'hrd'.
        WHEN 'QGA'.
          v_conn_2_hr-dbco = 'GANDBHRQ'.
          v_conn_2_hr-mandt = '020'.
          v_conn_2_hr-rfc = 'HRQ'.
          v_conn_2_hr-syst = 'HRQ'.
          v_conn_2_hr-schema = 'hrq'.
        WHEN 'SSI'.
          v_conn_2_hr-dbco = 'GANDBHRP'.
          v_conn_2_hr-mandt = '020'.
          v_conn_2_hr-rfc = 'HRP'.
          v_conn_2_hr-syst = 'HRP'.
          v_conn_2_hr-schema = 'hrp'.
      ENDCASE.
    ENDIF.


    SELECT SINGLE valor INTO ld_valor
    FROM zhrhard
    WHERE programa = 'ZIN_HR_2_NOHR'
    AND parametro = 'VULSHR2NOHR'.
    IF sy-subrc = 0.
      SPLIT ld_valor AT '-' INTO v_conn_2_r3-dbco
      v_conn_2_r3-mandt
      v_conn_2_r3-rfc
      v_conn_2_r3-syst
      v_conn_2_r3-schema.
    ELSE.
      CASE sy-sysid.
        WHEN 'HRD'.
          v_conn_2_r3-dbco = 'GANDBTST'.
          v_conn_2_r3-mandt = '010'.
          v_conn_2_r3-rfc = 'TST'.
          v_conn_2_r3-syst = 'TST'.
          v_conn_2_r3-schema = 'tst'.
        WHEN 'HRQ'.
          v_conn_2_r3-dbco = 'GANDBQGA'.
          v_conn_2_r3-mandt = '010'.
          v_conn_2_r3-rfc = 'QGA'.
          v_conn_2_r3-syst = 'QGA'.
          v_conn_2_r3-schema = 'qga'.
        WHEN 'HRP'.
          v_conn_2_r3-dbco = 'GANDBSSI'.
          v_conn_2_r3-mandt = '010'.
          v_conn_2_r3-rfc = 'SSI'.
          v_conn_2_r3-syst = 'SSI'.
          v_conn_2_r3-schema = 'ssi'.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD clear_table.
    r_ok = clear_table_sql( ip_tabname ).
    CHECK r_ok = abap_false.
    add_message( ip_type = 'E' ip_var1 = 'Actualización tabla'  ip_var2 = CONV #( ip_tabname ) ip_var3 = 'OK RFC'  ).
    add_log( ip_tabname = ip_tabname ip_tech = 'RFC' ip_modo = 'UPD' ).
  ENDMETHOD.


  METHOD clear_table_sql.
    DATA: ld_stmt TYPE string,
          ld_from TYPE string.

    r_ok = abap_false.

    ld_from = ip_tabname.
    ld_stmt = |DELETE FROM { ld_from }|.
    r_ok = o_sql->execute( EXPORTING ip_stmt = ld_stmt ).
    IF r_ok = abap_true.
      add_message( ip_type = 'S' ip_var1 = 'Borrado'  ip_var2 = CONV #( ip_tabname ) ip_var3 = 'OK SQL'  ).
      add_log( ip_tabname = ip_tabname ip_tech = 'SQL' ip_modo = 'CLR' ).
    ELSE.
      add_message( ip_type = 'E' ip_var1 = 'Error Borrado tabla' ip_var2 = |{ ip_tabname }| ).
      add_message( ip_type = 'E' ip_var1 = |SQL { ld_stmt }| ip_var2 = |en { v_conn-dbco } { v_conn-mandt }| ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    CASE ip_destino.
      WHEN 'HR'.
        v_conn = v_conn_2_hr.
      WHEN 'R3'.
        v_conn = v_conn_2_r3.
      WHEN space.
        CASE sy-sysid.
          WHEN 'TST' OR 'QGA' OR 'SSI'.
            v_conn = v_conn_2_hr.
          WHEN 'HRD' OR 'HRQ' OR 'HRP'.
            v_conn = v_conn_2_r3.
        ENDCASE.
    ENDCASE.
    v_repid = ip_repid.
    v_tcode = ip_tcode.
    IF io_logger IS BOUND.
      o_sql = NEW zcl_ga_sql( ip_dbco = v_conn-dbco io_logger = io_logger ).
    ELSE.
      o_sql = NEW zcl_ga_sql( v_conn-dbco ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_table.

*    r_ok = delete_table_rfc( ip_tabname = to_upper( ip_tabname ) it_data = it_data ).
    CALL METHOD delete_table_rfc
      EXPORTING
        ip_tabname = to_upper( ip_tabname )
        it_data    = it_data
      RECEIVING
        r_ok       = r_ok
      EXCEPTIONS
        OTHERS     = 1.

    CHECK r_ok = abap_false.

  ENDMETHOD.


  METHOD delete_table_rfc.
    r_ok = abap_false.

    DATA: ld_fname TYPE rs38l_fnam.

    CONCATENATE 'Z_DEL_' ip_tabname '_RFC' INTO ld_fname.
    IF check_rfc_exists( ld_fname ) = abap_false.
      add_message( ip_type = 'W' ip_var1 = 'Borrado tabla' ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'RFC_NOT_AVAILABLE' ).
      RAISE rfc_not_available.
    ENDIF.
    CALL FUNCTION ld_fname DESTINATION v_conn-rfc
      TABLES
        data                = it_data
      EXCEPTIONS
        table_not_available = 1
        table_without_data  = 2
        option_not_valid    = 3
        not_authorized      = 4
        OTHERS              = 5.
    CASE sy-subrc.
      WHEN 1.
        add_message( ip_type = 'W' ip_var1 = 'Borrado tabla' ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'TABLE_NOT_AVAILABLE' ).
        RAISE table_not_available.
      WHEN 2.
        add_message( ip_type = 'W' ip_var1 = 'Borrado tabla'  ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'TABLE_WITHOUT_DATA' ).
        RAISE table_without_data.
      WHEN 3.
        add_message( ip_type = 'W' ip_var1 = 'Borrado tabla'  ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'OPTION_NOT_VALID' ).
        RAISE option_not_valid.
      WHEN 4.
        add_message( ip_type = 'W' ip_var1 = 'Borrado tabla'  ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'NOT_AUTHORIZED' ).
        RAISE not_authorized.
      WHEN 5.
        add_message( ip_type = 'W' ip_var1 = 'Borrado tabla'  ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'RFC_NOT_AVAILABLE' ).
        RAISE rfc_not_available.
      WHEN 0.
        r_ok = abap_true.
    ENDCASE.
    CHECK r_ok = abap_true.
    add_message( ip_type = 'S' ip_var1 = 'Borrado tabla'  ip_var2 = CONV #( ip_tabname ) ip_var3 = 'OK RFC'  ).
    add_log( ip_tabname = ip_tabname ip_tech = 'RFC' ip_modo = 'DEL' ).
  ENDMETHOD.


  method GET_MESSAGES.
    rt_messages = me->t_conn_bapiret2.
  endmethod.


  METHOD get_table.

*    r_ok = get_table_rfc( EXPORTING ip_tabname = ip_tabname ip_opt_where = ip_opt_where ip_selection = ip_selection
*                          CHANGING it_data = it_data ).
    CALL METHOD get_table_rfc
      EXPORTING
        ip_tabname   = ip_tabname
        ip_opt_where = ip_opt_where
        ip_selection = ip_selection
      CHANGING
        it_data      = it_data
      RECEIVING
        r_ok         = r_ok
      EXCEPTIONS
        OTHERS       = 1.
    CHECK r_ok = abap_false.


*    r_ok = get_table_sql( EXPORTING ip_tabname = ip_tabname ip_opt_where = ip_opt_where ip_selection = ip_selection
*                          CHANGING it_data = it_data ).
    CALL METHOD get_table_sql
      EXPORTING
        ip_tabname   = ip_tabname
        ip_opt_where = ip_opt_where
        ip_selection = ip_selection
      CHANGING
        it_data      = it_data
      RECEIVING
        r_ok         = r_ok
      EXCEPTIONS
        OTHERS       = 1.


  ENDMETHOD.


  METHOD get_table_rfc.
    DATA: lt_options TYPE esh_t_co_rfcrt_options,
          ls_option  TYPE rfc_db_opt,
          lt_where   TYPE rsds_where_tab,
          ls_where   TYPE rsdswhere,
          ld_tabname TYPE tabname.

    r_ok = abap_false.
    lt_options = ip_opt_where.
    ld_tabname = ip_tabname.
    TRANSLATE ld_tabname TO UPPER CASE.
    DATA: ld_fname TYPE rs38l_fnam.

    CONCATENATE 'Z_GET_' ld_tabname '_RFC' INTO ld_fname.
    IF check_rfc_exists( ld_fname ) = abap_false.
      add_message( ip_type = 'W' ip_var1 = 'Lectura tabla' ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'RFC_NOT_AVAILABLE' ).
      RAISE rfc_not_available.
    ENDIF.

    IF ip_selection[] IS NOT INITIAL.
      lt_where = selection_2_where(
        EXPORTING
          ip_selection = ip_selection
          ip_tabname = ld_tabname ).
      LOOP AT lt_where INTO ls_where.
        AT FIRST.
          IF lt_options[] IS NOT INITIAL.
            ls_option-text = 'AND'.
            APPEND ls_option TO lt_options.
          ENDIF.
        ENDAT.
        MOVE ls_where-line TO ls_option.
        APPEND ls_option TO lt_options.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION ld_fname DESTINATION v_conn-rfc
      TABLES
        options             = lt_options
        data                = it_data
      EXCEPTIONS
        table_not_available = 1
        table_without_data  = 2
        option_not_valid    = 3
        not_authorized      = 4
        OTHERS              = 5.
*        rc = sy-subrc.

    CASE sy-subrc.
      WHEN 1.
        add_message( ip_type = 'W' ip_var1 = 'Lectura tabla' ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'TABLE_NOT_AVAILABLE' ).
        RAISE table_not_available.
      WHEN 2.
        add_message( ip_type = 'W' ip_var1 = 'Lectura tabla' ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'TABLE_WITHOUT_DATA' ).
        RAISE table_without_data.
      WHEN 3.
        add_message( ip_type = 'W' ip_var1 = 'Lectura tabla' ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'OPTION_NOT_VALID' ).
        RAISE option_not_valid.
      WHEN 4.
        add_message( ip_type = 'W' ip_var1 = 'Lectura tabla' ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'NOT_AUTHORIZED' ).
        RAISE not_authorized.
      WHEN 5.
        add_message( ip_type = 'W' ip_var1 = 'Lectura tabla' ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'RFC_NOT_AVAILABLE' ).
        RAISE rfc_not_available.
      WHEN 0.
        r_ok = abap_true.
    ENDCASE.
    CHECK r_ok = abap_true.
    add_message( ip_type = 'S' ip_var1 = 'Lectura tabla' ip_var2 = CONV #( ip_tabname ) ip_var3 = 'OK RFC'  ).
    add_log( ip_tabname = ip_tabname ip_tech = 'RFC' ip_modo = 'READ' ).

  ENDMETHOD.


  METHOD get_table_sql.
    DATA: ld_where_opt TYPE string,
          ld_where_sel TYPE string.
    DATA: ls_selection TYPE gusl_s_selection,
          lt_selection TYPE gusl_t_selection,
          ls_ra_mandt  TYPE gusl_s_range.
    DATA: ld_fname TYPE dd03l-fieldname VALUE 'MANDT'.



    LOOP AT ip_opt_where INTO DATA(ls_where).
      ld_where_opt = |{ ld_where_opt } { ls_where-text }|.
    ENDLOOP.
    lt_selection = ip_selection.


    SELECT SINGLE fieldname INTO ld_fname
    FROM dd03l WHERE fieldname = ld_fname.
    IF sy-subrc = 0.
      READ TABLE ip_selection TRANSPORTING NO FIELDS
      WITH KEY fieldname = ld_fname.
      IF sy-subrc <> 0.
        ls_selection-fieldname = ld_fname.
        ls_ra_mandt-sign = 'I'.
        ls_ra_mandt-option = 'EQ'.
        ls_ra_mandt-low = v_conn-mandt.
        APPEND ls_ra_mandt TO ls_selection-t_range.
        APPEND ls_selection TO lt_selection.
      ENDIF.
    ENDIF.

    ld_where_sel = selection_2_where_sql( ip_tabname = ip_tabname ip_selection = lt_selection ).

    DATA:ld_stmt  TYPE string,
         ld_table TYPE string,
         ld_error TYPE string.

    CONCATENATE v_conn-schema '.' ip_tabname INTO ld_table.
    CONCATENATE
    'select * from' ld_table
    INTO ld_stmt SEPARATED BY space.
    IF ld_where_sel IS NOT INITIAL OR ld_where_opt IS NOT INITIAL.
      CONCATENATE ld_stmt 'where' INTO ld_stmt SEPARATED BY space.
    ENDIF.
    IF ld_where_sel IS NOT INITIAL.
      CONCATENATE ld_stmt ld_where_sel INTO ld_stmt SEPARATED BY space.
    ENDIF.
    IF ld_where_opt IS NOT INITIAL.
      IF ld_where_sel IS NOT INITIAL.
        CONCATENATE ld_stmt 'and' INTO ld_stmt SEPARATED BY space.
      ENDIF.
      CONCATENATE ld_stmt ld_where_opt INTO ld_stmt SEPARATED BY space.
    ENDIF.
    TRY.
        DATA(lo_ref) = REF #( it_data ) .
        r_ok = o_sql->query( EXPORTING ip_query = ld_stmt
                              CHANGING or_result = lo_ref
                            ).
        add_message( ip_type = 'S' ip_var1 = 'Lectura tabla' ip_var2 = CONV #( ip_tabname ) ip_var3 = 'OK SQL' ).
        add_log( ip_tabname = ip_tabname ip_tech = 'SQL' ip_modo = 'READ' ).
      CATCH cx_sql_exception INTO DATA(lx_sql).
        r_ok = abap_false.
        ld_error = lx_sql->sql_message.
      CATCH cx_root INTO DATA(lx_error).
        r_ok = abap_false.
        ld_error = lx_error->get_longtext( ).

    ENDTRY.
    CHECK r_ok = abap_false.
    add_message( ip_type = 'E' ip_var1 = 'Error lectura tabala' ip_var2 = |{ ip_tabname }| ).
    add_message( ip_type = 'E' ip_var1 = |{ ld_error }| ip_var2 = |en { v_conn-dbco } { v_conn-mandt }| ).


  ENDMETHOD.


  METHOD get_table_srql.
    DATA(lo_srql) = zcl_srqlquery=>new( destination = v_conn-rfc table = CONV #( ip_tabname ) ).
    IF it_fields IS NOT INITIAL AND ip_ddic_structure IS INITIAL.
      lo_srql->prepare_result( fields = it_fields ).
    ELSEIF it_fields IS INITIAL AND ip_ddic_structure IS NOT INITIAL.
      lo_srql->prepare_result( ddic_structure = ip_ddic_structure ).
    ENDIF.

    lo_srql->prepare_statement( select_option = it_selection string = ip_where ).
    lo_srql->execute( ).
    or_data = lo_srql->get_result_data( ).

  ENDMETHOD.


  METHOD selection_2_where.
    DATA: lt_where_clause TYPE rsds_twhere,
          ls_where_clause TYPE rsds_where,
          ls_trange       TYPE rsds_range,
          lt_trange       TYPE rsds_trange.

    ls_trange-tablename = ip_tabname.
    ls_trange-frange_t  = ip_selection.
    APPEND ls_trange TO lt_trange.

    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING
        field_ranges  = lt_trange
      IMPORTING
        where_clauses = lt_where_clause.

    READ TABLE lt_where_clause INTO ls_where_clause INDEX 1.
    rt_where = ls_where_clause-where_tab.
  ENDMETHOD.


  METHOD selection_2_where_sql.
*    DATA: lt_where_clause TYPE rsds_twhere,
*          ls_where_clause TYPE rsds_where,
*          ls_trange       TYPE rsds_range,
*          lt_trange       TYPE rsds_trange.
*
*    ls_trange-tablename = ip_tabname.
*    ls_trange-frange_t  = ip_selection.
*    APPEND ls_trange TO lt_trange.
*
*    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
*      EXPORTING
*        field_ranges  = lt_trange
*      IMPORTING
*        where_clauses = lt_where_clause.
*
*    READ TABLE lt_where_clause INTO ls_where_clause INDEX 1.
*    rt_where = ls_where_clause-where_tab.
    DATA: ls_selection TYPE gusl_s_selection.
    DATA: ld_renderer TYPE REF TO if_rsmds_condition_renderer.
    DATA: ld_dim0     TYPE REF TO if_rsmds_dimension.
    DATA: ld_universe TYPE REF TO cl_rsmds_universe.
    DATA: ld_rset TYPE REF TO cl_rsmds_set.
    DATA: ld_rseti TYPE REF TO cl_rsmds_set.
    DATA: ld_incl_null  TYPE rsmds_boolean VALUE rsmds_c_boolean-false.
    DATA: ld_acc_gen_cp  TYPE rsmds_boolean VALUE rsmds_c_boolean-false.
    DATA: ld_string TYPE string.
    DATA: rx_message TYPE REF TO cx_rsmds_message.

*  break-point.
*    TRY.


*    ld_universe = cl_rsmds_ddic_universe=>create_by_tabname( ip_tabname ).
    CALL METHOD cl_rsmds_ddic_universe=>create_by_tabname
      EXPORTING
        i_tabname    = ip_tabname
      RECEIVING
        r_r_universe = ld_universe
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
*      CATCH not_found.
*
*    ENDTRY.
    ld_rseti = cl_rsmds_set=>get_universal_set( ).
    LOOP AT ip_selection INTO ls_selection.
      TRY.
          ld_dim0 = ld_universe->get_dimension_by_name( ls_selection-fieldname ).
          ld_rset = ld_dim0->create_set_from_ranges(
          i_t_ranges                = ls_selection-t_range
          i_final                   = rsmds_c_boolean-true
          i_accept_general_patterns = ld_acc_gen_cp           ).
        CATCH cx_rsmds_input_invalid INTO rx_message.
          MESSAGE rx_message TYPE 'I'.
*        LEAVE PROGRAM.
      ENDTRY.
      ld_rseti = ld_rseti->intersect( ld_rset ).
    ENDLOOP.
    ld_string = ld_rseti->to_string(
    i_r_renderer = ld_renderer
    i_include_null_conditions = ld_incl_null ).
    r_where = ld_string.
  ENDMETHOD.


  METHOD show_messages.
    IF ip_on_error = abap_true AND NOT line_exists( t_conn_bapiret2[ type = 'E' ] ).
      RETURN.
    ENDIF.
    CALL FUNCTION 'SUSR_DISPLAY_LOG'
      EXPORTING
        display_in_popup = 'X'
      TABLES
        it_log_bapiret2  = t_conn_bapiret2
      EXCEPTIONS
        parameter_error  = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF ip_exit IS NOT INITIAL.
      LEAVE PROGRAM.
    ENDIF.
  ENDMETHOD.


  METHOD update_table.
*    CATCH SYSTEM-EXCEPTIONS
*    r_ok = update_table_rfc( exporting ip_tabname = ip_tabname it_data = it_data it_delete_data = it_delete_data ).
    CALL METHOD update_table_rfc
      EXPORTING
        ip_tabname     = ip_tabname
        it_data        = it_data
        it_delete_data = it_delete_data
      RECEIVING
        r_ok           = r_ok
      EXCEPTIONS
        OTHERS         = 1.
    CHECK r_ok = abap_false.
  ENDMETHOD.


  METHOD update_table_rfc.
    r_ok = abap_false.

    DATA(ld_tabname) = ip_tabname.
    TRANSLATE ld_tabname TO UPPER CASE.
    DATA: ld_fname TYPE rs38l_fnam.

    CONCATENATE 'Z_UPD_' ld_tabname '_RFC' INTO ld_fname.
    IF check_rfc_exists( ld_fname ) = abap_false.
      add_message( ip_type = 'E' ip_var1 = 'Actualización tabla' ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'RFC_NOT_AVAILABLE' ).
      RAISE rfc_not_available.
    ENDIF.
    CALL FUNCTION ld_fname DESTINATION v_conn-rfc
      TABLES
        data                = it_data
        rem_data            = it_delete_data
      EXCEPTIONS
        table_not_available = 1
        table_without_data  = 2
        option_not_valid    = 3
        not_authorized      = 4
        OTHERS              = 5.
    CASE sy-subrc.
      WHEN 1.
        add_message( ip_type = 'W' ip_var1 = 'Actualización tabla' ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'TABLE_NOT_AVAILABLE' ).
        RAISE table_not_available.
      WHEN 2.
        add_message( ip_type = 'W' ip_var1 = 'Actualización tabla'  ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'TABLE_WITHOUT_DATA' ).
        RAISE table_without_data.
      WHEN 3.
        add_message( ip_type = 'W' ip_var1 = 'Actualización tabla'  ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'OPTION_NOT_VALID' ).
        RAISE option_not_valid.
      WHEN 4.
        add_message( ip_type = 'W' ip_var1 = 'Actualización tabla'  ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'NOT_AUTHORIZED' ).
        RAISE not_authorized.
      WHEN 5.
        add_message( ip_type = 'W' ip_var1 = 'Actualización tabla'  ip_var2 = CONV #( ip_tabname ) ip_var3 = 'NOK RFC' ip_var4 = 'RFC_NOT_AVAILABLE' ).
        RAISE rfc_not_available.
      WHEN 0.
        r_ok = abap_true.
    ENDCASE.
    CHECK r_ok = abap_true.
    add_message( ip_type = 'S' ip_var1 = 'Actualización tabla'  ip_var2 = CONV #( ip_tabname ) ip_var3 = 'OK RFC'  ).
    add_log( ip_tabname = ip_tabname ip_tech = 'RFC' ip_modo = 'UPD' ).
  ENDMETHOD.
ENDCLASS.
