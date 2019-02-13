CLASS zcl_soql_builder_authtoken DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abap_soql_builder.

    METHODS:
      constructor IMPORTING is_credentials TYPE zif_abap_soql_builder=>ty_credentials,
      authentication RAISING zcx_abap_soql,
      get_access_token RETURNING VALUE(rv_access_token) TYPE string,
      get_instance_url RETURNING VALUE(rv_instance_url) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mv_instance_url TYPE string,
          mv_query_params TYPE string,
          mv_access_token TYPE string.
    DATA: mo_http_client TYPE REF TO if_http_client.
    DATA: mo_rest_client TYPE REF TO if_rest_client.
    METHODS:

      http IMPORTING iv_url                TYPE string
           RETURNING VALUE(ro_http_client) TYPE REF TO if_http_client
           RAISING   zcx_abap_soql,

      setup_credentials IMPORTING iv_grant_type          TYPE string
                                  iv_client_id           TYPE string
                                  iv_client_secret       TYPE string
                                  iv_user_name           TYPE string
                                  iv_password            TYPE string
                        RETURNING VALUE(rv_query_params) TYPE string.
ENDCLASS.



CLASS zcl_soql_builder_authtoken IMPLEMENTATION.
  METHOD http.
    IF ( mo_http_client IS NOT BOUND ).
*   Create HTTP Client
      CALL METHOD cl_http_client=>create_by_url
        EXPORTING
          url                = iv_url
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

    ENDIF.
    ro_http_client = mo_http_client.
  ENDMETHOD.



  METHOD get_access_token.
    rv_access_token = mv_access_token.
  ENDMETHOD.

  METHOD get_instance_url.
    rv_instance_url = mv_instance_url.
  ENDMETHOD.

  METHOD authentication.

    TRY.
        mo_http_client = http( 'https://login.salesforce.com' ).
      CATCH zcx_abap_soql INTO DATA(lcx_abap_soql).
        MESSAGE lcx_abap_soql->get_text( ) TYPE 'E'.
    ENDTRY.

    " set http protocol version
    mo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    mo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.
    cl_http_utility=>set_request_uri( request = mo_http_client->request
                                      uri = |/services/oauth2/token?{ mv_query_params }| ).

    DATA(lo_rest_client) = NEW cl_rest_http_client( mo_http_client ).

    " generate http request header
    DATA(lo_request) = lo_rest_client->if_rest_client~create_request_entity( ).
    lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_www_form_url_encoded ).
    lo_request->set_header_field( iv_name = 'Accept' iv_value = if_rest_media_type=>gc_appl_json ). "#EC NOTEXT

    " perform HTTP POST request
    TRY.
        lo_rest_client->if_rest_resource~post( lo_request ).
      CATCH cx_rest_client_exception INTO DATA(lo_exception).
        DATA(lv_msg) = `HTTP POST failed: ` && lo_exception->get_text( ).
        RAISE EXCEPTION TYPE zcx_abap_soql
          EXPORTING
            textid  = zcx_abap_soql=>zcx_http_error
            err_msg = lv_msg.
    ENDTRY.

    " get and check REST call response
    DATA(lo_response)       = lo_rest_client->if_rest_client~get_response_entity( ).
    DATA(lv_http_status)    = lo_response->get_header_field( '~status_code' ).
    DATA(lv_reason)         = lo_response->get_header_field( '~status_reason' ).
    DATA(lv_content_type)   = lo_response->get_header_field( 'content-type' ).
    DATA(response)          = lo_response->get_string_data( ).

    DATA: lv_message TYPE string.
    IF lv_http_status EQ 200.
      lv_message = `OK`.
    ELSE.
      lv_message = `HTTP status ` && lv_http_status && ` (` && lv_reason && `)`. "#EC NOTEXT
      RAISE EXCEPTION TYPE zcx_abap_soql
        EXPORTING
          textid  = zcx_abap_soql=>zcx_http_error
          err_msg = lv_message.
    ENDIF.

    DATA: ls_request TYPE zif_abap_soql_builder=>ty_request.
    DATA(lv_response_body) = lo_response->get_string_data( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_response_body CHANGING data = ls_request ).

    mv_access_token = ls_request-access_token.
    mv_instance_url = ls_request-instance_url.
  ENDMETHOD.

  METHOD setup_credentials.

    rv_query_params = cl_http_utility=>fields_to_string(
      fields      = VALUE #( ( name = 'grant_type' value = iv_grant_type )
                             ( name = 'client_id' value = iv_client_id  )
                             ( name = 'client_secret' value = iv_client_secret )
                             ( name = 'username'   value = iv_user_name )
                             ( name = 'password'   value = iv_password ) ) "{password}{securitytoken}
    ).

  ENDMETHOD.

  METHOD constructor.

    mv_query_params = setup_credentials( iv_grant_type    = is_credentials-grant_type
                                         iv_client_id     = is_credentials-client_id
                                         iv_client_secret = is_credentials-client_secret
                                         iv_user_name     = is_credentials-user_name
                                         iv_password      = is_credentials-password ).

  ENDMETHOD.

ENDCLASS.
