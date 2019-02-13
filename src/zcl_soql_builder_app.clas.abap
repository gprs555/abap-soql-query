CLASS zcl_soql_builder_app DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_abap_soql_builder.

    DATA: mo_http_client TYPE REF TO if_http_client,
          mo_rest_client TYPE REF TO cl_rest_http_client.

    CLASS-METHODS:
      get_apps_instance IMPORTING is_credentials TYPE zif_abap_soql_builder=>ty_credentials
                        RETURNING VALUE(ro_apps) TYPE REF TO zcl_soql_builder_app,
      progress_indicator.
    METHODS: constructor IMPORTING is_credentials TYPE zif_abap_soql_builder=>ty_credentials,

      get_sobjects  RETURNING VALUE(rt_sobjects) TYPE zif_abap_soql_builder=>tty_sobjects
                    RAISING   zcx_abap_soql,

      get_fieldnames IMPORTING iv_describe_url  TYPE string
                     RETURNING VALUE(rt_fields) TYPE zif_abap_soql_builder=>tty_fields
                     RAISING   zcx_abap_soql,

      get_instance_url RETURNING VALUE(rv_instance_url) TYPE string,
      get_access_token RETURNING VALUE(rv_access_token) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    "static class reference to hold the existing object reference
    CLASS-DATA: mo_apps TYPE REF TO zcl_soql_builder_app.

    DATA: mo_auth                 TYPE REF TO zcl_soql_builder_authtoken,
          mv_service_instance_url TYPE string,
          mv_service_access_token TYPE string.
ENDCLASS.


CLASS zcl_soql_builder_app IMPLEMENTATION.
  METHOD constructor.
    " authentication
    mo_auth = NEW zcl_soql_builder_authtoken( is_credentials  ).
    TRY.
        mo_auth->authentication( ).
      CATCH zcx_abap_soql INTO DATA(lcx_abap_soql).
        MESSAGE lcx_abap_soql->get_text( ) TYPE 'E'.
    ENDTRY.
    mv_service_instance_url = mo_auth->get_instance_url( ).
    mv_service_access_token = mo_auth->get_access_token( ).
  ENDMETHOD.

  METHOD get_apps_instance.
    IF mo_apps IS NOT BOUND.
      CREATE OBJECT mo_apps
        EXPORTING
          is_credentials = is_credentials.
    ENDIF.
    ro_apps = mo_apps.
  ENDMETHOD.

  METHOD get_sobjects.

*   HTTP Client Object
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = mv_service_instance_url
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

    cl_http_utility=>set_request_uri( request = mo_http_client->request
                                          uri = |/services/data/v44.0/sobjects| ).
    " generate http request header
    DATA(lo_request) = mo_rest_client->if_rest_client~create_request_entity( ).
    lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
    lo_request->set_header_field( iv_name = 'Accept' iv_value = if_rest_media_type=>gc_appl_json ). "#EC NOTEXT
    CONCATENATE 'Bearer' mv_service_access_token INTO DATA(lv_access_token) SEPARATED BY space.
    lo_request->set_header_field( iv_name = 'Authorization' iv_value = lv_access_token ).

    TRY.
        mo_rest_client->if_rest_resource~get( ).
      CATCH cx_rest_client_exception INTO DATA(lo_exception).
        DATA(lv_msg) = `HTTP GET failed: ` && lo_exception->get_text( ).
        RAISE EXCEPTION TYPE zcx_abap_soql
          EXPORTING
            textid  = zcx_abap_soql=>zcx_http_error
            err_msg = lv_msg.
    ENDTRY.

    " get and check REST call response
    DATA(lo_response) = mo_rest_client->if_rest_client~get_response_entity( ).
    DATA(lv_http_status)  = lo_response->get_header_field( '~status_code' ).
    DATA(lv_reason)       = lo_response->get_header_field( '~status_reason' ).
    DATA(lv_content_type) = lo_response->get_header_field( 'content-type' ).
    DATA(response)        = lo_response->get_string_data( ).
    DATA: lv_message TYPE string.
    IF lv_http_status EQ 200. "
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
    DATA: lt_data TYPE STANDARD TABLE OF zif_abap_soql_builder=>ty_sobjects,
          ls_data TYPE zif_abap_soql_builder=>ty_sobjects.
    DATA(lt_entries)  = lo_parser->array_values( '/sobjects' ).


    DATA lv_index_str TYPE string.
    LOOP AT lt_entries INTO DATA(ls_entry).
      lv_index_str = sy-tabix.
      CONDENSE lv_index_str.
      "Name
      DATA(lv_path)  = '/sobjects/' && lv_index_str && '/name'.
      DATA(lv_value) = lo_parser->value( lv_path ).
      ls_data-name   = lv_value.
      "Path
      lv_path  = '/sobjects/' && lv_index_str && '/label'.
      lv_value = lo_parser->value( lv_path ).
      ls_data-label = lv_value.
      "describe_url
      lv_path =  '/sobjects/' && lv_index_str && '/urls/describe'.
      lv_value = lo_parser->value( lv_path ).
      ls_data-describe_url = lv_value.
      "sobject_url
      lv_path =  '/sobjects/' && lv_index_str && '/urls/sobject'.
      lv_value = lo_parser->value( lv_path ).
      ls_data-sobject_url = lv_value.
      APPEND ls_data TO lt_data.
    ENDLOOP.

    rt_sobjects = lt_data.

  ENDMETHOD.

  METHOD get_fieldnames.

    cl_http_utility=>set_request_uri( request = mo_http_client->request uri = iv_describe_url ).
    TRY.
        mo_rest_client->if_rest_resource~get( ).
      CATCH cx_rest_client_exception INTO DATA(lo_exception).
        DATA(lv_msg) = `HTTP GET failed: ` && lo_exception->get_text( ).
        RAISE EXCEPTION TYPE zcx_abap_soql
          EXPORTING
            textid  = zcx_abap_soql=>zcx_http_error
            err_msg = lv_msg.
    ENDTRY.
    " get and check REST call response
    DATA(lo_response)     = mo_rest_client->if_rest_client~get_response_entity( ).
    DATA(lv_http_status)  = lo_response->get_header_field( '~status_code' ).
    DATA(lv_reason)       = lo_response->get_header_field( '~status_reason' ).
    DATA(lv_content_type) = lo_response->get_header_field( 'content-type' ).

    DATA: lv_message TYPE string.
    IF lv_http_status EQ 200."OK
      lv_message = `OK`.                                    "#EC NOTEXT
    ELSE. " Error
      lv_message = `HTTP status ` && lv_http_status && ` (` && lv_reason && `)`. "#EC NOTEXT
    ENDIF.

    DATA(lv_request_body)  = lo_response->get_string_data( ).
    DATA lo_parser TYPE REF TO /ui5/cl_json_parser.         "#EC NOTEXT
    CREATE OBJECT lo_parser.
    lo_parser->parse( lv_request_body ).

    DATA: lt_fields TYPE zif_abap_soql_builder=>tty_fields,
          ls_fields LIKE LINE OF lt_fields.

    DATA(lv_entries)  = lo_parser->array_values( '/fields' ).

    DATA lv_entry     TYPE string.
    DATA lv_index_str TYPE string.
    DATA lv_tdings TYPE TABLE OF string.

    LOOP AT lv_entries INTO lv_entry.
      lv_index_str = sy-tabix.
      CONDENSE lv_index_str.
      "Name
      DATA(lv_path) = '/fields/' && lv_index_str && '/name'.
      DATA(lv_value) = lo_parser->value( lv_path ).
      ls_fields-name = lv_value.
      "Label
      lv_path =  '/fields/' && lv_index_str && '/label'.
      lv_value = lo_parser->value( lv_path ).
      ls_fields-label = lv_value.

      APPEND ls_fields TO lt_fields.
    ENDLOOP.

    rt_fields = lt_fields.

  ENDMETHOD.

  METHOD get_access_token.
    rv_access_token = me->mv_service_access_token.
  ENDMETHOD.

  METHOD get_instance_url.
    rv_instance_url = me->mv_service_instance_url.
  ENDMETHOD.

  METHOD progress_indicator.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 95
        text       = 'Getting the data'.
  ENDMETHOD.

ENDCLASS.
