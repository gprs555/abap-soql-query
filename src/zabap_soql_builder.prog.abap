*&---------------------------------------------------------------------*
*& Report zabap_soql_builder
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_soql_builder.


TYPE-POOLS: icon.

CLASS lcl_detail DEFINITION DEFERRED.
CLASS lcl_field DEFINITION DEFERRED.


DATA: ok_code               TYPE sy-ucomm,
      ms_variant            TYPE disvariant,
      ms_stable             TYPE lvc_s_stbl,
      ms_detail_layout      TYPE lvc_s_layo,
      ms_field_layout       TYPE lvc_s_layo,
      mt_detail_fcat        TYPE lvc_t_fcat,
      mt_field_fcat         TYPE lvc_t_fcat,
      mt_hidden_code        TYPE ui_functions,
      mo_gui_area           TYPE REF TO cl_gui_custom_container,
      mo_gui_splitter       TYPE REF TO cl_gui_splitter_container,
      mo_gui_detail         TYPE REF TO cl_gui_container,
      mo_gui_detail_grid    TYPE REF TO cl_gui_alv_grid,
      mo_gui_field          TYPE REF TO cl_gui_container,
      mo_gui_field_grid     TYPE REF TO cl_gui_alv_grid,

      mo_gui_output         TYPE REF TO cl_gui_container,
      mo_info_screen        TYPE REF TO cl_gui_textedit,
      mo_detail_application TYPE REF TO lcl_detail,
      mo_field_application  TYPE REF TO lcl_field.

DATA: ms_current_detail     TYPE zif_abap_soql_builder=>ty_sobjects,
      mt_detail             TYPE zif_abap_soql_builder=>tty_sobjects,
      mv_field_query        TYPE string,
      mt_field              TYPE zif_abap_soql_builder=>tty_fields.
DATA: mo_soql_builder       TYPE REF TO zcl_soql_builder_app.


*----------------------------------------------------------------------*
*       CLASS lcl_detail DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_detail DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      handle_hotspot_click
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING es_row_no e_column_id.

ENDCLASS.                    "lcl_detail DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_detail IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_detail IMPLEMENTATION.

  METHOD handle_hotspot_click.

    IF es_row_no IS NOT INITIAL.
      READ TABLE mt_detail INDEX es_row_no-row_id INTO ms_current_detail.
      CASE e_column_id.
        WHEN 'NAME'.
          TRY.
              mt_field = mo_soql_builder->get_fieldnames( ms_current_detail-describe_url ).
            CATCH zcx_abap_soql INTO DATA(lcx_abap_soql).
              MESSAGE lcx_abap_soql->get_text( ) TYPE 'E'.
          ENDTRY.
          mo_gui_field_grid->set_frontend_layout( is_layout = ms_field_layout ).
          mo_gui_field_grid->refresh_table_display( ).

          DATA: lt_trace_tab TYPE STANDARD TABLE OF line.
          CALL METHOD mo_info_screen->set_text_as_r3table
            EXPORTING
              table = lt_trace_tab.
      ENDCASE.
    ENDIF.

  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "lcl_detail IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_field DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_field DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      handle_toolbar
                    FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_hotspot_click
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING es_row_no e_column_id.


ENDCLASS.                    "lcl_field DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_field IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_field IMPLEMENTATION.

  METHOD handle_toolbar.

    DATA: ls_toolbar  TYPE stb_button.
* Run query
    CLEAR ls_toolbar.
    MOVE 'RUN_QUERY'     TO ls_toolbar-function.
    MOVE icon_test       TO ls_toolbar-icon.
    MOVE 'RUN QUERY'     TO ls_toolbar-text.
    MOVE 'RUN QUERY'     TO ls_toolbar-quickinfo.
    IF mt_detail IS INITIAL.
      ls_toolbar-disabled = abap_true.
    ENDIF.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.                    "handle_toolbar


  METHOD handle_user_command.

** Handle command
    CASE e_ucomm.
      WHEN 'RUN_QUERY'.

        DATA(lv_access_token) = mo_soql_builder->get_access_token( ).
        DATA(lv_instance_url) = mo_soql_builder->get_instance_url( ).
        SUBMIT zabap_soql_query_results WITH p_token  = lv_access_token
                                        WITH p_insurl = lv_instance_url
                                        WITH p_query  = mv_field_query
                                        AND RETURN.      "#EC CI_SUBMIT
    ENDCASE.

  ENDMETHOD.                    " handle_user_command


  METHOD handle_hotspot_click.

    IF es_row_no IS NOT INITIAL.
      DATA: lt_text TYPE STANDARD TABLE OF line.
      DATA: lt_results TYPE match_result_tab.
      DATA: lv_pattern TYPE string.
      CONSTANTS : lc_wd TYPE char2 VALUE `\b`. "word boundary

      CALL METHOD mo_info_screen->get_text_as_r3table
        IMPORTING
          table = lt_text.

      cl_gui_cfw=>flush( ).

      READ TABLE mt_field INDEX es_row_no-row_id INTO DATA(ls_field).
      CASE e_column_id.
        WHEN 'NAME'.
          DATA: lt_trace_tab TYPE STANDARD TABLE OF line.
          DATA: wa_trace_tab LIKE LINE OF lt_trace_tab.
          REFRESH :lt_trace_tab.

          IF lt_text[] IS INITIAL.
            wa_trace_tab-line =  |SELECT|.
            APPEND wa_trace_tab TO lt_trace_tab.
            wa_trace_tab-line =  |{ ls_field-name }|.
            APPEND wa_trace_tab TO lt_trace_tab.
            wa_trace_tab-line =  |FROM { ms_current_detail-name }|.
            APPEND wa_trace_tab TO lt_trace_tab.
          ELSE.
            DATA(lv_count) = lines( lt_text ).
            DATA: ls_text LIKE LINE OF lt_text.

            CONCATENATE LINES OF lt_text INTO DATA(lv_querystring) SEPARATED BY space.
            CLEAR lv_pattern.
            lv_pattern = |({ lc_wd }{ ls_field-name })|.
            FIND ALL OCCURRENCES OF REGEX lv_pattern IN  lv_querystring  RESULTS lt_results.
            IF lt_results IS NOT INITIAL.
              RETURN.
            ENDIF.

            LOOP AT lt_text INTO ls_text.
              REPLACE ALL OCCURRENCES OF ',' IN ls_text-line WITH space.
              CONDENSE ls_text-line.
              IF sy-tabix = lv_count.
                wa_trace_tab-line =  |{ ls_field-name }|.
                APPEND wa_trace_tab TO lt_trace_tab.
                wa_trace_tab-line = ls_text-line .
                APPEND wa_trace_tab TO lt_trace_tab.
                EXIT.
              ENDIF.

              IF sy-tabix = 1.
                wa_trace_tab-line = ls_text-line.
              ELSE.
                wa_trace_tab-line =  |{ ls_text-line },|.
              ENDIF.

              APPEND wa_trace_tab TO lt_trace_tab.

            ENDLOOP.
          ENDIF.

          CLEAR mv_field_query.
          CONCATENATE LINES OF lt_trace_tab INTO mv_field_query SEPARATED BY space.
          REPLACE ALL OCCURRENCES OF ` ` IN mv_field_query WITH `+`.
          REPLACE ALL OCCURRENCES OF `,+` IN mv_field_query WITH `,`.
          CONDENSE mv_field_query NO-GAPS.
          IF mv_field_query IS NOT INITIAL.
            REFRESH: lt_text.
            SPLIT ms_current_detail-sobject_url AT 'sobjects' INTO TABLE lt_text. "#EC NOTEXT
            READ TABLE lt_text INTO ls_text INDEX 1.
            IF sy-subrc EQ 0.
              DATA(lv_query_uri) = |{ ls_text-line }query/?q=|.
            ENDIF.
            mv_field_query = |{ lv_query_uri }{ mv_field_query } |.
          ENDIF.
          CALL METHOD mo_info_screen->set_text_as_r3table( table = lt_trace_tab ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.                    "handle_double_click

ENDCLASS.                    "lcl_field IMPLEMENTATION



*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'LIST'.
  SET TITLEBAR 'TITLE'.

  IF mo_detail_application IS INITIAL.

    ms_stable-row = ms_stable-col = abap_true.
    mo_detail_application  = NEW #( ).
    mo_field_application = NEW #( ).

  ENDIF.

  IF mo_gui_area IS INITIAL.

    mo_gui_area = NEW #( container_name = 'GUI_AREA' ).
    mo_gui_splitter = NEW #( parent  = mo_gui_area
                             rows    = 1
                             columns = 3 ).

    mo_gui_detail =  mo_gui_splitter->get_container( EXPORTING row = 1 column = 1 ).
    mo_gui_field = mo_gui_splitter->get_container( EXPORTING row = 1 column = 2 ).
    mo_gui_output =  mo_gui_splitter->get_container( EXPORTING row = 1 column = 3 ).

    DATA: lv_line_length TYPE i VALUE 254.
    mo_info_screen = NEW #( parent                     = mo_gui_output
                            wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
                            wordwrap_position          = lv_line_length
                            wordwrap_to_linebreak_mode = cl_gui_textedit=>true ).

    mo_info_screen->set_toolbar_mode( toolbar_mode = cl_gui_textedit=>false ).
    mo_info_screen->set_statusbar_mode( statusbar_mode = cl_gui_textedit=>false ).
    mo_info_screen->set_font_fixed( mode = cl_gui_textedit=>true ).
    mo_info_screen->set_readonly_mode( cl_gui_textedit=>true ).

    mo_gui_detail_grid  = NEW #( i_parent = mo_gui_detail ).
    mo_gui_field_grid   = NEW #( i_parent = mo_gui_field ).

    SET HANDLER mo_detail_application->handle_hotspot_click  FOR mo_gui_detail_grid.

    SET HANDLER mo_field_application->handle_toolbar        FOR mo_gui_field_grid.
    SET HANDLER mo_field_application->handle_user_command   FOR mo_gui_field_grid.
    SET HANDLER mo_field_application->handle_hotspot_click  FOR mo_gui_field_grid.

    PERFORM hide_toolbar.

    PERFORM create_layout.

    PERFORM get_data.

    MOVE sy-repid TO ms_variant-report.

***** Detail Grid
    CALL METHOD mo_gui_detail_grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = mt_hidden_code
        is_layout            = ms_detail_layout
        i_save               = 'A'
        is_variant           = ms_variant
      CHANGING
        it_fieldcatalog      = mt_detail_fcat
        it_outtab            = mt_detail.

****** Field Grid
    CALL METHOD mo_gui_field_grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = mt_hidden_code
        is_layout            = ms_field_layout
        i_save               = 'A'
        is_variant           = ms_variant
      CHANGING
        it_fieldcatalog      = mt_field_fcat
        it_outtab            = mt_field.

  ENDIF.

ENDMODULE.                 " status_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  user_command_0100 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT .

  CASE ok_code.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.                 " user_command_0100 INPUT


*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
FORM get_data.

  DATA: ls_credentials                TYPE zif_abap_soql_builder=>ty_credentials.

  CLEAR mt_detail.

  ls_credentials-grant_type    = 'password'.
  ls_credentials-client_id     = '<client_id>'.
  ls_credentials-client_secret = '<client_secret>'.
  ls_credentials-user_name     = '<user_name>'.
  ls_credentials-password      = '<password>'.

  mo_soql_builder = zcl_soql_builder_app=>get_apps_instance( ls_credentials ).
  zcl_soql_builder_app=>progress_indicator( ).
  TRY.
      DATA(lt_sobjects) = mo_soql_builder->get_sobjects( ).
    CATCH zcx_abap_soql INTO DATA(lcx_abap_soql).
      MESSAGE lcx_abap_soql->get_text( ) TYPE 'E'.
  ENDTRY.

  APPEND LINES OF lt_sobjects TO mt_detail.

ENDFORM.                    " get_data


*&---------------------------------------------------------------------*
*&      Form  create_layout
*&---------------------------------------------------------------------*
FORM create_layout.

  DATA: lv_index    TYPE i,
        ls_gui_fcat TYPE lvc_s_fcat.

* HEADER Layout
  ms_detail_layout-grid_title = 'Select Object'.            "#EC NOTEXT
  ms_detail_layout-sel_mode   = 'A'.

  lv_index = 1.
  CLEAR ls_gui_fcat.
  ls_gui_fcat-col_pos   = lv_index.
  ls_gui_fcat-fieldname = 'NAME'.                           "#EC NOTEXT
  ls_gui_fcat-coltext   = 'Name'.                           "#EC NOTEXT
  ls_gui_fcat-hotspot   = abap_true.
  ls_gui_fcat-col_opt   = abap_true.
  ls_gui_fcat-outputlen = 18.
  APPEND ls_gui_fcat TO mt_detail_fcat.

  lv_index = lv_index + 1.
  CLEAR ls_gui_fcat.
  ls_gui_fcat-col_pos   = lv_index.
  ls_gui_fcat-fieldname = 'LABEL'.                          "#EC NOTEXT
  ls_gui_fcat-coltext   = 'Label'.                          "#EC NOTEXT
  ls_gui_fcat-outputlen = 20.
  ls_gui_fcat-col_opt   = abap_true.
  APPEND ls_gui_fcat TO mt_detail_fcat.

  lv_index = lv_index + 1.
  CLEAR ls_gui_fcat.
  ls_gui_fcat-col_pos   = lv_index.
  ls_gui_fcat-fieldname = 'SOBJECT_URL'.                    "#EC NOTEXT
  ls_gui_fcat-coltext   = 'SObject URL'.                    "#EC NOTEXT
  ls_gui_fcat-hotspot   = abap_true.
  ls_gui_fcat-outputlen = 30.
  ls_gui_fcat-no_out    = abap_true.

  APPEND ls_gui_fcat TO mt_detail_fcat.

  lv_index = lv_index + 1.
  CLEAR ls_gui_fcat.
  ls_gui_fcat-col_pos   = lv_index.
  ls_gui_fcat-fieldname = 'DESCRIBE_URL'.                   "#EC NOTEXT
  ls_gui_fcat-coltext   = 'Describe URL'.                   "#EC NOTEXT
  ls_gui_fcat-outputlen = 60.
  ls_gui_fcat-no_out    = abap_true.
  APPEND ls_gui_fcat TO mt_detail_fcat.


* Field Layout
  ms_field_layout-grid_title = 'Select Field(s)'.           "#EC NOTEXT
  ms_field_layout-sel_mode   = 'A'.
  ms_field_layout-col_opt    = 'X'.
  ms_field_layout-cwidth_opt  = 'X'.

  lv_index = 1.
  CLEAR ls_gui_fcat.
  ls_gui_fcat-col_pos   = lv_index.
  ls_gui_fcat-fieldname = 'NAME'.                           "#EC NOTEXT
  ls_gui_fcat-coltext   = 'API Name'.                       "#EC NOTEXT
  ls_gui_fcat-hotspot   = abap_true.
  ls_gui_fcat-outputlen = 20.
  ls_gui_fcat-col_opt   = 'X'.
  APPEND ls_gui_fcat TO mt_field_fcat.

  lv_index = lv_index + 1.
  CLEAR ls_gui_fcat.
  ls_gui_fcat-col_pos   = lv_index.
  ls_gui_fcat-fieldname = 'LABEL'.                          "#EC NOTEXT
  ls_gui_fcat-coltext   = 'Label'.                          "#EC NOTEXT
  ls_gui_fcat-outputlen = 25.
  ls_gui_fcat-col_opt   = 'X'.
  APPEND ls_gui_fcat TO mt_field_fcat.


ENDFORM.                    " create_layout
*&---------------------------------------------------------------------*
*&      Form  hide_toolbar
*&---------------------------------------------------------------------*
FORM hide_toolbar .

* Hidden Codes
  APPEND cl_gui_alv_grid=>mc_fc_col_optimize          TO mt_hidden_code.
  APPEND cl_gui_alv_grid=>mc_fc_help                  TO mt_hidden_code.
  APPEND cl_gui_alv_grid=>mc_fc_col_invisible         TO mt_hidden_code.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy              TO mt_hidden_code.
  APPEND cl_gui_alv_grid=>mc_fc_unfix_columns         TO mt_hidden_code.
  APPEND cl_gui_alv_grid=>mc_fc_subtot                TO mt_hidden_code.
  APPEND cl_gui_alv_grid=>mc_fc_sum                   TO mt_hidden_code.
  APPEND cl_gui_alv_grid=>mc_fc_print                 TO mt_hidden_code.
  APPEND cl_gui_alv_grid=>mc_fc_views                 TO mt_hidden_code.
  APPEND cl_gui_alv_grid=>mc_fc_graph                 TO mt_hidden_code.
  APPEND cl_gui_alv_grid=>mc_fc_info                  TO mt_hidden_code.
  APPEND cl_gui_alv_grid=>mc_mb_sum                   TO mt_hidden_code.

ENDFORM.                    " hide_toolbar

START-OF-SELECTION.
  CALL SCREEN 100.
