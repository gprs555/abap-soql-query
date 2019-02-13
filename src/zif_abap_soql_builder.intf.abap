INTERFACE zif_abap_soql_builder
  PUBLIC .
  TYPES:
    BEGIN OF ty_credentials,
      grant_type    TYPE string,
      client_id     TYPE string,
      client_secret TYPE string,
      user_name     TYPE string,
      password      TYPE string,
    END OF ty_credentials,

    BEGIN OF ty_request,
      access_token TYPE string,
      instance_url TYPE string,
      id           TYPE string,
      issued_at    TYPE string,
      signature    TYPE string,
    END OF ty_request,


    BEGIN OF ty_sobjects,
      name         TYPE string,
      label        TYPE string,
      sobject_url  TYPE string,
      describe_url TYPE string,
    END OF ty_sobjects,

    BEGIN OF ty_fields,
      name  TYPE string,
      label TYPE string,
    END OF ty_fields.


  TYPES: tty_sobjects TYPE STANDARD TABLE OF ty_sobjects WITH EMPTY KEY,
         tty_fields   TYPE STANDARD TABLE OF ty_fields WITH EMPTY KEY.

ENDINTERFACE.
