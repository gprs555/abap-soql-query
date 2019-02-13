class ZCX_ABAP_SOQL definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  constants ZCX_CREATE_HTTP_CLIENT type SOTR_CONC value '080027ECD3A21ED989E215371B428073' ##NO_TEXT.
  constants ZCX_HTTP_ERROR type SOTR_CONC value '080027ECD3A21ED98BB3DE615AE3CE1D' ##NO_TEXT.
  data ERR_MSG type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !ERR_MSG type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ABAP_SOQL IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->ERR_MSG = ERR_MSG .
  endmethod.
ENDCLASS.
