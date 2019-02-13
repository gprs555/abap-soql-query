*&---------------------------------------------------------------------*
*& Report zabap_soql_query_results
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_soql_query_results.

PARAMETERS: p_token  TYPE string DEFAULT space  LOWER CASE, "NO-DISPLAY,
            p_insurl TYPE string DEFAULT space  LOWER CASE, "NO-DISPLAY,
            p_query  TYPE string DEFAULT space  LOWER CASE. "NO-DISPLAY.

CLASS lcl_main DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:  t_line     TYPE STANDARD TABLE OF line.
    CLASS-METHODS exec
      RETURNING
        VALUE(ro_main) TYPE REF TO lcl_main.

    METHODS: query IMPORTING iv_access_token TYPE string
                             iv_instance_url TYPE string
                             iv_query_uri    TYPE string
                   RAISING   zcx_abap_soql.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: mo_http_client TYPE REF TO if_http_client,
                mo_rest_client TYPE REF TO cl_rest_http_client.
    DATA: mv_dataelement TYPE rollname,
          mo_structdescr TYPE REF TO cl_abap_structdescr,
          mo_tabledescr  TYPE REF TO cl_abap_tabledescr,
          mt_component   TYPE abap_component_tab,
          ms_component   TYPE LINE OF abap_component_tab,
          mr_range       TYPE REF TO data,
          mr_range_line  TYPE REF TO data.

    METHODS:show_output  IMPORTING it_fieldname TYPE t_line
                         CHANGING  ct_output    TYPE STANDARD TABLE,
      get_fieldname_list IMPORTING iv_query      TYPE string
                         EXPORTING rt_fieldnames TYPE t_line.


ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD exec.
    CREATE OBJECT ro_main.
  ENDMETHOD.

  METHOD query.

*   Create HTTP Client
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = iv_instance_url
        ssl_id             = 'ANONYM' "#EC NOTEXT
      IMPORTING
        client             = mo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_soql
        EXPORTING
          textid = zcx_abap_soql=>zcx_create_http_client.
    ENDIF.

    " set http protocol version
    mo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    mo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.
    mo_rest_client = NEW cl_rest_http_client( mo_http_client ).
    cl_http_utility=>set_request_uri( request = mo_http_client->request uri = |{ iv_query_uri }| ).

    " generate http request header
    DATA(lo_request) = mo_rest_client->if_rest_client~create_request_entity( ).
    lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_request->set_header_field( iv_name = 'Accept' iv_value = if_rest_media_type=>gc_appl_json ). "#EC NOTEXT
    CONCATENATE 'Bearer' iv_access_token INTO DATA(lv_access_token) SEPARATED BY space. "#EC NOTEXT
    lo_request->set_header_field( iv_name = 'Authorization' iv_value = lv_access_token ). "#EC NOTEXT

    TRY.
        mo_rest_client->if_rest_resource~get( ).
      CATCH cx_rest_client_exception INTO DATA(lo_exception).
        DATA(lv_msg) = `HTTP GET failed: ` && lo_exception->get_text( ).  "#EC NOTEXT
        RAISE EXCEPTION TYPE zcx_abap_soql
          EXPORTING
            textid  = zcx_abap_soql=>zcx_http_error
            err_msg = lv_msg.
    ENDTRY.

    " get and check REST call response
    DATA(lo_response) = mo_rest_client->if_rest_client~get_response_entity( ).
    DATA(lv_http_status)  = lo_response->get_header_field( '~status_code' ).
    DATA(lv_reason)       = lo_response->get_header_field( '~status_reason' ).
    DATA(lv_content_type) = lo_response->get_header_field( 'content-type' ).  "#EC NEEDED
    DATA(response) = lo_response->get_string_data( ). "#EC NEEDED
    DATA: lv_message TYPE string.
    IF lv_http_status EQ 200.
      lv_message = `OK`.                                    "#EC NOTEXT
    ELSE. " http error status
      lv_message = `HTTP status ` && lv_http_status && ` (` && lv_reason && `)`. "#EC NOTEXT
      RAISE EXCEPTION TYPE zcx_abap_soql
        EXPORTING
          textid  = zcx_abap_soql=>zcx_http_error
          err_msg = lv_message.
    ENDIF.

    DATA(lv_request_body) = lo_response->get_string_data( ).

    DATA lo_parser TYPE REF TO /ui5/cl_json_parser.         "#EC NOTEXT
    CREATE OBJECT lo_parser.
    lo_parser->parse( lv_request_body ).

    DATA: iv_node TYPE string.
    iv_node = '/records'.
    DATA(lt_entries)  = lo_parser->array_values( iv_node ).
    me->get_fieldname_list( EXPORTING iv_query = iv_query_uri
                            IMPORTING rt_fieldnames = DATA(lt_fieldnames) ).

    FIELD-SYMBOLS: <range_line> TYPE any,
                   <range>      TYPE STANDARD TABLE.

    mv_dataelement = 'STRING'.
    LOOP AT lt_fieldnames INTO DATA(ls_fieldnames).
      TRANSLATE ls_fieldnames-line TO UPPER CASE.
      ms_component-name = ls_fieldnames-line.
      ms_component-type ?= cl_abap_elemdescr=>describe_by_name( mv_dataelement ).
      INSERT ms_component INTO TABLE mt_component.
    ENDLOOP.
    " Create type descriptors
    mo_structdescr ?= cl_abap_structdescr=>create( mt_component ).
    mo_tabledescr  ?= cl_abap_tabledescr=>create( mo_structdescr ).
    " Create usable variables
    CREATE DATA mr_range TYPE HANDLE mo_tabledescr.
    ASSIGN mr_range->* TO <range>.
    CREATE DATA mr_range_line TYPE HANDLE mo_structdescr.
    ASSIGN mr_range_line->* TO <range_line>.

    FIELD-SYMBOLS: <value> TYPE any.
*---------------------------------------------------------------
    DATA lv_index_str TYPE string.
    DATA lv_path      TYPE string.
    DATA lt_tdings TYPE TABLE OF string.
    FIELD-SYMBOLS <obj_name> TYPE string.

    DATA: lt_results TYPE match_result_tab.
    DATA: lv_pattern TYPE string.
    CONSTANTS : lc_wd TYPE char2 VALUE `\b`. "word boundary

    LOOP AT lt_entries INTO DATA(ls_entry). "#EC NEEDED
      lv_index_str = sy-tabix.
      CONDENSE lv_index_str.
      DATA(index_host) = iv_node && '/'  && lv_index_str. "/records/NN
      CONDENSE index_host.
      lt_tdings = lo_parser->members( index_host ).

      LOOP AT lt_tdings ASSIGNING <obj_name>.
        CONCATENATE iv_node '/' lv_index_str '/' <obj_name> INTO lv_path. "/records/NN/XXXXX
        CONDENSE lv_path.

        CHECK lo_parser->value( lv_path ) IS NOT INITIAL.
        LOOP AT lt_fieldnames INTO ls_fieldnames.
          CLEAR lv_pattern.
          lv_pattern = |({ lc_wd }{ ls_fieldnames-line })|.

          FIND ALL OCCURRENCES OF REGEX lv_pattern IN  lv_path  RESULTS lt_results.
          IF lt_results IS NOT INITIAL.
            TRANSLATE ls_fieldnames-line TO UPPER CASE.
            ASSIGN COMPONENT ls_fieldnames-line OF STRUCTURE <range_line> TO <value>.
            <value> =  lo_parser->value( lv_path ).
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      APPEND <range_line> TO <range>.
    ENDLOOP.
    show_output(  EXPORTING it_fieldname = lt_fieldnames CHANGING ct_output  = <range> ).

  ENDMETHOD.


  METHOD get_fieldname_list.
    DATA: lt_text TYPE STANDARD TABLE OF line.
    DATA: lv_query TYPE string.
    lv_query = iv_query.
    SPLIT lv_query AT `?q=SELECT+` INTO TABLE lt_text.
    DELETE lt_text INDEX 1.

    CLEAR lv_query.
    CONCATENATE LINES OF lt_text INTO lv_query.
    SPLIT lv_query AT `+FROM` INTO TABLE lt_text.
    READ TABLE lt_text INTO DATA(ls_text) INDEX 1.
    IF sy-subrc EQ 0.
      SPLIT ls_text AT `,` INTO TABLE rt_fieldnames.
    ENDIF.
  ENDMETHOD.

  METHOD show_output.

    DATA lr_table       TYPE REF TO cl_salv_table.
    DATA lr_functions   TYPE REF TO cl_salv_functions.
    DATA lr_display     TYPE REF TO cl_salv_display_settings.
    DATA lr_columns     TYPE REF TO cl_salv_columns_table.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lr_table
                                CHANGING t_table = ct_output  ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.

    lr_functions = lr_table->get_functions( ).
    lr_functions->set_all( abap_true ).
    lr_columns = lr_table->get_columns( ).

    DATA: lt_component TYPE abap_component_tab,
          ls_component TYPE LINE OF abap_component_tab.

    LOOP AT it_fieldname INTO DATA(ls_fieldname).
      TRANSLATE ls_fieldname-line TO UPPER CASE.
      ls_component-name = ls_fieldname-line.
      ls_component-type ?= cl_abap_elemdescr=>describe_by_name( `STRING` ).
      INSERT ls_component INTO TABLE lt_component.
    ENDLOOP.

    DATA: lo_column     TYPE REF TO cl_salv_column.
    DATA: lo_cols       TYPE REF TO cl_salv_columns.
    DATA: col_name(30), col_desc(20).

    lo_cols = lr_table->get_columns( ). "get columns object
    LOOP AT lt_component INTO ls_component."get Individual Column Names
      TRY.
          col_name = ls_component-name.
          lo_column = lo_cols->get_column( col_name ).
          READ TABLE it_fieldname INDEX sy-tabix INTO ls_fieldname.
          col_desc = ls_fieldname-line.
          lo_column->set_medium_text( col_desc ).
        CATCH cx_salv_not_found.  "#EC NO_HANDLER
      ENDTRY.
    ENDLOOP.


    DATA: lv_title TYPE lvc_title.
    lv_title  =  'Query Result(s)'. "#EC NOTEXT
    DATA(lr_display_settings) = lr_table->get_display_settings( ).
    lr_display_settings->set_list_header( lv_title ).
    lr_columns->set_optimize( ).
    lr_display = lr_table->get_display_settings( ).
    lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
    lr_table->display( ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  IF p_token IS NOT INITIAL AND p_insurl IS NOT INITIAL AND p_query IS NOT INITIAL.
    TRY.
        lcl_main=>exec( )->query( iv_access_token = p_token
                                  iv_instance_url = p_insurl
                                  iv_query_uri = p_query ).
      CATCH zcx_abap_soql INTO DATA(lcx_abap_soql).
        MESSAGE lcx_abap_soql->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDIF.
