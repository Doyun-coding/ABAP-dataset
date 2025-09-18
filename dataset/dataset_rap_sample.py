import json

datasets =[]

safe_templates = [
    r"""CLASS zcl_rap100_gen_data_sol DEFINITION
PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS zcl_rap100_gen_data_sol IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    DATA:
      group_id   TYPE string VALUE 'sol',
      attachment TYPE /dmo/attachment,
      file_name  TYPE /dmo/filename,
      mime_type  TYPE /dmo/mime_type.

*   clear data
    DELETE FROM zrap100_atravsol.
*    DELETE FROM zrap100_dtravsol.

    "insert travel demo data
    INSERT zrap100_atravsol  FROM (
        SELECT
          FROM /dmo/travel AS travel
          FIELDS
            travel~travel_id        AS travel_id,
            travel~agency_id        AS agency_id,
            travel~customer_id      AS customer_id,
            travel~begin_date       AS begin_date,
            travel~end_date         AS end_date,
            travel~booking_fee      AS booking_fee,
            travel~total_price      AS total_price,
            travel~currency_code    AS currency_code,
            travel~description      AS description,
            CASE travel~status    "[N(New) | P(Planned) | B(Booked) | X(Cancelled)]
              WHEN 'N' THEN 'O'
              WHEN 'P' THEN 'O'
              WHEN 'B' THEN 'A'
              ELSE 'X'
            END                     AS overall_status,
            @attachment             AS attachment,
            @mime_type              AS mime_type,
            @file_name              AS file_name,
            travel~createdby        AS created_by,
            travel~createdat        AS created_at,
            travel~lastchangedby    AS last_changed_by,
            travel~lastchangedat    AS last_changed_at,
            travel~lastchangedat    AS local_last_changed_at
            ORDER BY travel_id UP TO 10 ROWS
      ).
    COMMIT WORK.
    out->write( |[RAP100] Demo data generated for table ZRAP100_ATRAV{ group_id }. | ).
  ENDMETHOD.
ENDCLASS.""",
    r"""class ZRAP100_BP_TRAVELTP_SOL definition
  public
  abstract
  final
  for behavior of ZRAP100_R_TRAVELTP_SOL .

public section.
protected section.
private section.
ENDCLASS.



CLASS ZRAP100_BP_TRAVELTP_SOL IMPLEMENTATION.
ENDCLASS.""",
    r"""CLASS zrap100_cl_eml_SOL DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      is_draft  TYPE if_abap_behv=>t_xflag VALUE if_abap_behv=>mk-on,  "draft: '01'
      is_active TYPE if_abap_behv=>t_xflag VALUE if_abap_behv=>mk-off. "active: '00'

    CLASS-DATA:
      travel_id      TYPE /dmo/travel_id,        "travel id
      instance_state TYPE if_abap_behv=>t_xflag, "instance state (draft or active)
      console_output TYPE REF TO if_oo_adt_classrun_out.

    METHODS:
      read_travel,
      update_travel,
      create_travel,
      delete_travel,
      activate_travel_draft,
      discard_travel_draft.

ENDCLASS.""",
    r"""CLASS zrap100_cl_eml_SOL IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.console_output = out.

    "specify the operation to be executed
    DATA(execute) = 1.

    "READ a Travel BO entity instance
    IF execute = 1 OR execute = 55.
      travel_id      = '00000001'.
      instance_state = is_active.
      read_travel( ).
    ENDIF.

    "UPDATE a Travel BO entity instance
    IF execute = 2 OR execute = 55.
      travel_id      = '00000000'.
      instance_state = is_active.
      update_travel( ).
    ENDIF.

    "CREATE a Travel BO entity instance
    IF execute = 3 OR execute = 55.
      instance_state = is_active.
      create_travel( ).
    ENDIF.

    "DELETE a Travel BO entity instance
    IF execute = 4 OR execute = 55.
      travel_id      = '00000000'.
      instance_state = is_active.
      delete_travel( ).
    ENDIF.

    "ACTIVATE a draft Travel BO entity instance
    IF execute = 5 OR execute = 55.
      travel_id      = '00000000'.
      activate_travel_draft( ).
    ENDIF.

    "DISCARD a draft Travel BO entity instance
    IF execute = 6 OR execute = 55.
      travel_id      = '00000000'.
      discard_travel_draft( ).
    ENDIF.

  ENDMETHOD.


  METHOD read_travel.
    "declare internal table using derived type
    DATA travels TYPE TABLE FOR READ IMPORT ZRAP100_R_TravelTP_SOL.

    "fill in data for READ request
    travels = VALUE #( ( TravelID = travel_id  %is_draft = instance_state ) ).

    "read from transactional buffer
    READ ENTITIES OF ZRAP100_R_TravelTP_SOL
      ENTITY Travel
        ALL FIELDS
        WITH travels
    RESULT DATA(lt_travels_read)
    FAILED DATA(failed)
    REPORTED DATA(reported).

    "console output
    console_output->write( |Exercise 9.2: READ a Travel BO entity instance | ).
    console_output->write( lt_travels_read ).
    IF failed IS NOT INITIAL.
      console_output->write( |- Cause for failed read = { failed-travel[ 1 ]-%fail-cause } | ).
    ENDIF.
    console_output->write( |--------------------------------------------- | ).
  ENDMETHOD.


  METHOD update_travel.
    "update the transactional buffer
    MODIFY ENTITIES OF ZRAP100_R_TravelTP_SOL
      ENTITY Travel
        UPDATE FIELDS ( Description )
          WITH VALUE #( ( %is_draft   = instance_state
                          TravelID    = travel_id
                          Description = | Vacation { cl_abap_context_info=>get_system_time( ) } |
                      ) )
      FAILED DATA(failed)
      REPORTED DATA(reported).

    console_output->write( |Exercise 9.3: UPDATE a Travel BO entity instance| ).
    console_output->write( |- TravelID = { travel_id } / Description was updated | ).
    IF failed IS NOT INITIAL.
      console_output->write( |- Cause for failed update = { failed-travel[ 1 ]-%fail-cause } | ).
    ENDIF.
    console_output->write( |--------------------------------------------- | ).
  ENDMETHOD.


  METHOD create_travel.
    "create in the transactional buffer
    MODIFY ENTITIES OF ZRAP100_R_TravelTP_SOL
      ENTITY travel
        CREATE FIELDS ( CustomerID AgencyID BeginDate EndDate Description )
          WITH VALUE #( ( %cid        = 'create_travel'
                          %is_draft   = instance_state
                          CustomerID  = '15'
                          AgencyID    = '070042'
                          BeginDate   = cl_abap_context_info=>get_system_date( )
                          EndDate     = cl_abap_context_info=>get_system_date( ) + 10
                          Description = | ABAP DevDays { cl_abap_context_info=>get_system_time(  ) } |
                      ) )

      MAPPED DATA(mapped)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    "persist changes
    COMMIT ENTITIES
      RESPONSE OF ZRAP100_R_TravelTP_SOL
      FAILED DATA(failed_commit)
      REPORTED DATA(reported_commit).

    "console output
    console_output->write( |Exercise 9.4: CREATE a new Travel BO entity instance| ).
    console_output->write( mapped-travel ).
    IF failed IS NOT INITIAL.
      console_output->write( |- Cause for failed create: { failed-travel[ 1 ]-%fail-cause } | ).
    ELSEIF failed_commit IS NOT INITIAL.
      console_output->write( |- Cause for failed commit: { failed_commit-travel[ 1 ]-%fail-cause } | ).
    ENDIF.
    console_output->write( |--------------------------------------------- | ).
  ENDMETHOD.


 METHOD delete_travel.
    "delete in the transactional buffer
    MODIFY ENTITIES OF ZRAP100_R_TravelTP_SOL
      ENTITY travel
        DELETE FROM
          VALUE
             #( ( TravelID = travel_id  %is_draft   = instance_state ) )
     FAILED DATA(failed)
     REPORTED DATA(reported).

    "persist changes
    COMMIT ENTITIES
      RESPONSE OF ZRAP100_R_TravelTP_SOL
      FAILED     DATA(failed_commit)
      REPORTED   DATA(reported_commit).

    "console output
    console_output->write( |Exercise 9.5: DELETE a Travel BO entity instance | ).
    console_output->write( |- TravelID = { travel_id } | ).
    IF failed IS NOT INITIAL.
      console_output->write( |- Cause for failed delete: { failed-travel[ 1 ]-%fail-cause } | ).
    ELSEIF failed_commit IS NOT INITIAL.
      console_output->write( |- Cause for failed commit: { failed_commit-travel[ 1 ]-%fail-cause } | ).
    ENDIF.
    console_output->write( |--------------------------------------------- | ).
  ENDMETHOD.


  METHOD activate_travel_draft.

    "activate travel instance in the transactional buffer
    MODIFY ENTITIES OF ZRAP100_R_TravelTP_SOL
      ENTITY Travel
        EXECUTE Activate FROM
        VALUE #( ( %cid = 'activate_draft_travel'  %key-TravelID = travel_id ) )
     MAPPED DATA(mapped)
     FAILED DATA(failed)
     REPORTED DATA(reported).

    "persist changes
    COMMIT ENTITIES
      RESPONSE OF ZRAP100_R_TravelTP_SOL
      FAILED DATA(failed_commit)
      REPORTED DATA(reported_commit).

    "console output
    console_output->write( |Exercise 9.6: ACTIVATE a draft Travel BO entity instance | ).
    console_output->write( |- TravelID = { travel_id } | ).
    IF failed IS NOT INITIAL.
      console_output->write( |- Cause for failed activate: { failed-travel[ 1 ]-%fail-cause } | ).
    ELSEIF failed_commit IS NOT INITIAL.
      console_output->write( |- Cause for failed commit: { failed_commit-travel[ 1 ]-%fail-cause } | ).
    ENDIF.
    console_output->write( |--------------------------------------------- | ).
  ENDMETHOD.


  METHOD discard_travel_draft.
    "activate travel instance in the transactional buffer
    MODIFY ENTITIES OF ZRAP100_R_TravelTP_SOL
      ENTITY Travel
        EXECUTE Discard FROM
        VALUE #( ( %key-TravelID = travel_id ) )
     MAPPED DATA(mapped)
     FAILED DATA(failed)
     REPORTED DATA(reported).

    COMMIT ENTITIES
      RESPONSE OF ZRAP100_R_TravelTP_SOL
      FAILED DATA(failed_commit)
      REPORTED DATA(reported_commit).

    console_output->write( |Exercise 9.7: DISCARD a draft Travel BO entity instance | ).
    console_output->write( |- TravelID = { travel_id } | ).
    IF failed IS NOT INITIAL.
      console_output->write( |- Cause for failed discard: { failed-travel[ 1 ]-%fail-cause } | ).
    ELSEIF failed_commit IS NOT INITIAL.
      console_output->write( |- Cause for failed commit: { failed_commit-travel[ 1 ]-%fail-cause } | ).
    ENDIF.
    console_output->write( |--------------------------------------------- | ).
  ENDMETHOD.

ENDCLASS.""",
    r"""class ZRAP630_CL_VH_PRODUCT_SOL definition
  public
  final
  create public .

public section.

  interfaces IF_RAP_QUERY_PROVIDER .

  types:
    T_PRODUCTS TYPE STANDARD TABLE OF ZRAP630I_VH_PRODUCT_SOL WITH  EMPTY KEY .

  data:
    PRODUCTY TYPE t_products READ-ONLY .

  methods GET_PRODUCTS
    returning
      value(PRODUCTS) type t_products .
protected section.
private section.
ENDCLASS.""",
    r"""CLASS ZRAP630_CL_VH_PRODUCT_SOL IMPLEMENTATION.


METHOD GET_PRODUCTS.
"
  products = VALUE #(
  ( Product = 'ZPRINTER01' ProductText = 'Printer Professional ABC' Price = '500.00 ' Currency = 'EUR' ProductGroup = 'L001' BaseUnit = 'ST'  )
  ( Product = 'ZPRINTER02' ProductText = 'Printer Platinum' Price = '800.00 ' Currency = 'EUR' ProductGroup = 'L001' BaseUnit = 'ST'  )
  ( Product = 'D001' ProductText = 'Mobile Phone' Price = '850.00 ' Currency = 'EUR' ProductGroup = 'L001' BaseUnit = 'ST'  )
  ( Product = 'D002' ProductText = 'Table PC' Price = '900.00 ' Currency = 'EUR' ProductGroup = 'L001' BaseUnit = 'ST'  )
  ( Product = 'D003' ProductText = 'Office Table' Price = '599.00 ' Currency = 'EUR' ProductGroup = 'L001' BaseUnit = 'ST'  )
  ( Product = 'D004' ProductText = 'Office Chair' Price = '449.00 ' Currency = 'EUR' ProductGroup = 'L001' BaseUnit = 'ST'  )
  ( Product = 'D005' ProductText = 'Developer Notebook' Price = '3150.00 ' Currency = 'EUR' ProductGroup = 'L001' BaseUnit = 'ST'  )
  ( Product = 'D006' ProductText = 'Mouse' Price = '79.00 ' Currency = 'EUR' ProductGroup = 'L001' BaseUnit = 'ST'  )
  ( Product = 'D007' ProductText = 'Headset' Price = '159.00 ' Currency = 'EUR' ProductGroup = 'L001' BaseUnit = 'ST'  )
  ( Product = 'D008' ProductText = 'Keyboard' Price = '39.00 ' Currency = 'EUR' ProductGroup = 'L001' BaseUnit = 'ST'  )
   ).
ENDMETHOD.


METHOD IF_RAP_QUERY_PROVIDER~SELECT.
  DATA business_data TYPE TABLE OF ZRAP630I_VH_Product_SOL.
  DATA business_data_line TYPE ZRAP630I_VH_Product_SOL .
  DATA(skip)    = io_request->get_paging( )->get_offset( ).
  DATA(requested_fields)  = io_request->get_requested_elements( ).
  DATA(sort_order)    = io_request->get_sort_elements( ).
  TRY.
    DATA(filter_condition) = io_request->get_filter( )->get_as_sql_string( ).
    business_data = get_products(  ).
    SELECT * FROM @business_data AS implementation_types
       WHERE (filter_condition) INTO TABLE @business_data.
    io_response->set_total_number_of_records( lines( business_data ) ).
    io_response->set_data( business_data ).
    CATCH cx_root INTO DATA(exception).
    DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_longtext( ).
    DATA(exception_t100_key) = cl_message_helper=>get_latest_t100_exception( exception )->t100key.
    RAISE EXCEPTION TYPE zrap630_cx_demo_exception
        EXPORTING
          textid   = VALUE scx_t100key( msgid = exception_t100_key-msgid
          msgno = exception_t100_key-msgno
          attr1 = exception_t100_key-attr1
          attr2 = exception_t100_key-attr2
          attr3 = exception_t100_key-attr3
          attr4 = exception_t100_key-attr4 )
    previous = exception.
  ENDTRY.

ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zrap630_cx_demo_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_rap_query_provider
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL .

    CONSTANTS:

      BEGIN OF query_failed,
        msgid TYPE symsgid VALUE 'ZDMO_CM_RAP_GEN_MSG',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF query_failed
      .


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZRAP630_CX_DEMO_EXCEPTION IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZRAP630BP_R_SHOPTP_SOL definition
  public
  abstract
  final
  for behavior of ZRAP630R_SHOPTP_SOL .

public section.
protected section.
private section.
ENDCLASS.



CLASS ZRAP630BP_R_SHOPTP_SOL IMPLEMENTATION.
ENDCLASS.""",
    r"""CLASS LHC_SHOP DEFINITION INHERITING FROM CL_ABAP_BEHAVIOR_HANDLER.
  PRIVATE SECTION.
    METHODS:
      GET_GLOBAL_AUTHORIZATIONS FOR GLOBAL AUTHORIZATION
        IMPORTING
           REQUEST requested_authorizations FOR Shop
        RESULT result,
      CALCULATEORDERID FOR DETERMINE ON SAVE
        IMPORTING
          KEYS FOR  Shop~CalculateOrderID .
ENDCLASS.""",
    r"""CLASS LHC_SHOP IMPLEMENTATION.
  METHOD GET_GLOBAL_AUTHORIZATIONS.
  ENDMETHOD.
  METHOD CALCULATEORDERID.
  READ ENTITIES OF ZRAP630R_ShopTP_SOL IN LOCAL MODE
    ENTITY Shop
      ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(entities).
  DELETE entities WHERE OrderID IS NOT INITIAL.
  Check entities is not initial.
  "Dummy logic to determine object_id
  SELECT MAX( ORDER_ID ) FROM ZRAP630_ASHOPSOL INTO @DATA(max_object_id).
  "Add support for draft if used in modify
  "SELECT SINGLE FROM FROM ZRAP630SH00D_SOL FIELDS MAX( OrderID ) INTO @DATA(max_orderid_draft). "draft table
  "if max_orderid_draft > max_object_id
  " max_object_id = max_orderid_draft.
  "ENDIF.
  MODIFY ENTITIES OF ZRAP630R_ShopTP_SOL IN LOCAL MODE
    ENTITY Shop
      UPDATE FIELDS ( OrderID )
        WITH VALUE #( FOR entity IN entities INDEX INTO i (
        %tky          = entity-%tky
        OrderID     = max_object_id + i
  ) ).
  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_abap_file_uploader DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: tablename TYPE string.
    DATA: filename TYPE string.
    DATA: fileext TYPE string.
    DATA: dataoption TYPE string.
    DATA: filedata TYPE string.

    METHODS: get_input_field_value IMPORTING name         TYPE string
                                             dataref      TYPE data
                                   RETURNING VALUE(value) TYPE string.
    METHODS: get_html RETURNING VALUE(ui_html) TYPE string.

ENDCLASS.""",
    r"""CLASS zcl_abap_file_uploader  IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.

    CASE request->get_method(  ).

      WHEN CONV string( if_web_http_client=>get ).

        DATA(sap_table_request) = request->get_header_field( 'sap-table-request' ).
        IF sap_table_request IS INITIAL.
          response->set_text( get_html(   ) ).
        ELSE.
          DATA(name_filter) = xco_cp_abap_repository=>object_name->get_filter(
                               xco_cp_abap_sql=>constraint->contains_pattern( to_upper( sap_table_request ) && '%' )  ).
          DATA(objects) = xco_cp_abap_repository=>objects->tabl->where( VALUE #(
                              ( name_filter ) ) )->in( xco_cp_abap=>repository  )->get(  ).

          DATA(res) = `[`.
          LOOP AT objects INTO DATA(object).
            res &&= |\{ "TABLE_NAME": "{ object->name }" \}|.
            IF sy-tabix NE lines( objects ).
              res &&= `,`.
            ENDIF.
          ENDLOOP.
          res &&= `]`.
          response->set_text( res ).
        ENDIF.

      WHEN CONV string( if_web_http_client=>post ).

* the request comes in with metadata around the actual file data,
* extract the filename and fileext from this metadata as well as the raw file data.
        SPLIT request->get_text(  )  AT cl_abap_char_utilities=>cr_lf INTO TABLE DATA(content).
        READ TABLE content REFERENCE INTO DATA(content_item) INDEX 2.
        IF sy-subrc = 0.

          SPLIT content_item->* AT ';' INTO TABLE DATA(content_dis).
          READ TABLE content_dis REFERENCE INTO DATA(content_dis_item) INDEX 3.
          IF sy-subrc = 0.
            SPLIT content_dis_item->* AT '=' INTO DATA(fn) filename.
            REPLACE ALL OCCURRENCES OF `"` IN filename WITH space.
            CONDENSE filename NO-GAPS.
            SPLIT filename AT '.' INTO filename fileext.
          ENDIF.

        ENDIF.

        DELETE content FROM 1 TO 4.  " Get rid of the first 4 lines
        DELETE content FROM ( lines( content ) - 8 ) TO lines( content ).  " get rid of the last 9 lines

        LOOP AT content REFERENCE INTO content_item.  " put it all back together again humpdy dumpdy....
          filedata = filedata && content_item->*.
        ENDLOOP.

* Unpack input field values such as tablename, dataoption, etc.
        DATA(ui_data) = request->get_form_field(  `filetoupload-data` ).
        DATA(ui_dataref) = /ui2/cl_json=>generate( json = ui_data ).
        IF ui_dataref IS BOUND.
          ASSIGN ui_dataref->* TO FIELD-SYMBOL(<ui_dataref>).
          tablename = me->get_input_field_value( name = `TABLENAME` dataref = <ui_dataref> ).
          dataoption = me->get_input_field_value( name = `DATAOPTION` dataref = <ui_dataref> ).
        ENDIF.

* Check table name is valid.
        IF xco_cp_abap_repository=>object->tabl->database_table->for(
                             iv_name =  CONV #( tablename ) )->exists(  ) = abap_false
          OR tablename IS INITIAL.
          response->set_status( i_code = if_web_http_status=>bad_request
                                i_reason = |Table name { tablename } not valid or does not exist| ).
          response->set_text( |Table name { tablename } not valid or does not exist| ).
          RETURN.
        ENDIF.

* Check file extension is valid, only json today.
        IF fileext <> `json`.
          response->set_status( i_code = if_web_http_status=>bad_request
                                i_reason = `File type not supported` ).
          response->set_text( `File type not supported` ).
          RETURN.
        ENDIF.

* Load the data to the table via dynamic internal table
        DATA: dynamic_table TYPE REF TO data.
        FIELD-SYMBOLS: <table_structure> TYPE table.

        TRY.
            CREATE DATA dynamic_table TYPE TABLE OF (tablename).
            ASSIGN dynamic_table->* TO <table_structure>.
          CATCH cx_sy_create_data_error INTO DATA(cd_exception).
            response->set_status( i_code = if_web_http_status=>bad_request
                                 i_reason = cd_exception->get_text(  ) ).
            response->set_text( cd_exception->get_text(  )  ).
            RETURN.
        ENDTRY.

        /ui2/cl_json=>deserialize( EXPORTING json = filedata
                                   pretty_name = /ui2/cl_json=>pretty_mode-none
                                   CHANGING data = <table_structure> ).

        IF dataoption = `1`.  "if replace, delete the data from the table first
          DELETE FROM (tablename).
        ENDIF.

        TRY.
            INSERT (tablename) FROM TABLE @<table_structure>.
            IF sy-subrc = 0.
              response->set_status( i_code = if_web_http_status=>ok
                                    i_reason = `Table updated successfully` ).
              response->set_text( `Table updated successfully` ).
            ENDIF.
          CATCH cx_sy_open_sql_db INTO DATA(db_exception).
            response->set_status( i_code = if_web_http_status=>bad_request
                                 i_reason = db_exception->get_text(  ) ).
            response->set_text( db_exception->get_text(  )  ).
            RETURN.
        ENDTRY.

    ENDCASE.

  ENDMETHOD.

  METHOD get_input_field_value.

    FIELD-SYMBOLS: <value> TYPE data,
                   <field> TYPE any.

    ASSIGN COMPONENT name  OF STRUCTURE dataref TO <field>.
    IF <field> IS ASSIGNED.
      ASSIGN <field>->* TO <value>.
      value = condense( <value> ).
    ENDIF.

  ENDMETHOD.

  METHOD get_html.
    ui_html = "".
  ENDMETHOD.

ENDCLASS.""",
    r"""CLASS zcl_dsag_bo_impl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    interfaces:
      if_rap_query_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS zcl_dsag_bo_impl IMPLEMENTATION.
  METHOD if_rap_query_provider~select.
    if not io_request->is_data_requested(  ).
      return.
    endif.

    data(lo_filter) = io_request->get_filter(  ).
    try.
      data(lt_range) = lo_filter->get_as_ranges(  ).
    catch cx_rap_query_filter_no_range.
    endtry.

    if not line_exists( lt_range[ name = 'ID' ] ).
      "Need select parameter!
      return.
    endif.

    data rt_table type table of zi_dsag_bill_order.
    loop at lt_range[ name = 'ID' ]-range assigning field-symbol(<ls_range>).
      data(lv_id) = <ls_range>-low.
      select single * from zdsag_billdoc WHERE
        id = @lv_id INTO @DATA(ls_billdoc).

      data:
        lv_sum_excl_vat  type p LENGTH 15 DECIMALS 2 VALUE 0,
        lv_sum_vat       type p LENGTH 15 DECIMALS 2 VALUE 0,
        lv_sum_all       type p LENGTH 15 DECIMALS 2 VALUE 0,
        lv_currency      type c LENGTH 5.

      select _item~amount, _product~vat, _product~price, _product~currency from zdsag_billitem as _item
        join zdsag_product as _product on
          _item~product = _product~id
        where
          billdoc = _item~billdoc
        into TABLE @DATA(lt_bill_items).

      loop at lt_bill_items ASSIGNING FIELD-SYMBOL(<ls_bill_item>).
        if lv_currency is INITIAL.
          lv_currency = <ls_bill_item>-currency.
        endif.

        data lv_vat type p LENGTH 15 DECIMALS 2.
        data lv_price type p LENGTH 15 DECIMALS 2.
        lv_price = <ls_bill_item>-price * <ls_bill_item>-amount.
        lv_vat =  lv_price * ( <ls_bill_item>-vat / 100  ).

        lv_sum_excl_vat = lv_sum_excl_vat + lv_price.
        lv_sum_vat      = lv_sum_vat + lv_vat + lv_price.
        lv_sum_all      = lv_sum_all + lv_price + lv_vat.
      ENDLOOP.

      insert value zi_dsag_bill_order(
        created_at      = ls_billdoc-created_at
        id              = ls_billdoc-id
        payment_method  = ls_billdoc-payment_method
        receiver_id     = ls_billdoc-receiver
        sum_all         = lv_sum_all
        sum_excl_vat    = lv_sum_excl_vat
        sum_vat         = lv_sum_vat
      ) into table rt_table.
    endloop.

    io_response->set_data( rt_table ).
    io_response->set_total_number_of_records( lines( rt_table ) ).
  ENDMETHOD.

ENDCLASS.""",
    r"""CLASS zcl_dsag_execute_fdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_DSAG_EXECUTE_FDP IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    try.
      "Initialize Template Store Client
      data(lo_store) = new ZCL_FP_TMPL_STORE_CLIENT(
        iv_name = 'restapi'
        iv_service_instance_name = 'SAP_COM_0276'
      ).
      "Initialize Template Store Client (using custom comm scenario)
      "data(lo_store) = new ZCL_FP_TMPL_STORE_CLIENT(
      "  iv_service_instance_name = 'ZADSTEMPLSTORE'
      "  iv_use_destination_service = abap_false
      ").
      out->write( 'Template Store Client initialized' ).
      "Initialize class with service definition
      data(lo_fdp_util) = cl_fp_fdp_services=>get_instance( 'ZDSAG_BILLING_SRV_DEF' ).
      out->write( 'Dataservice initialized' ).

      try.
        lo_store->get_schema_by_name( iv_form_name = 'DSAG_DEMO' ).
        out->write( 'Schema found in form' ).
      catch zcx_fp_tmpl_store_error into data(lo_tmpl_error).
        out->write( 'No schema in form found' ).
        if lo_tmpl_error->mv_http_status_code = 404.
          "Upload service definition
          lo_store->set_schema(
            iv_form_name = 'DSAG_DEMO'
            is_data = value #( note = '' schema_name = 'schema' xsd_schema = lo_fdp_util->get_xsd(  )  )
          ).
        else.
          out->write( lo_tmpl_error->get_longtext(  ) ).
        ENDIF.
      endtry.
      "Get initial select keys for service
      data(lt_keys)     = lo_fdp_util->get_keys( ).
      lt_keys[ name = 'ID' ]-value = '1'.

      data(lv_xml) = lo_fdp_util->read_to_xml( lt_keys ).
      out->write( 'Service data retrieved' ).

      data(ls_template) = lo_store->get_template_by_name(
        iv_get_binary     = abap_true
        iv_form_name      = 'DSAG_DEMO'
        iv_template_name  = 'TEMPLATE'
      ).
      out->write( 'Form Template retrieved' ).

      cl_fp_ads_util=>render_4_pq(
        EXPORTING
          iv_locale       = 'en_US'
          iv_pq_name      = 'PRINT_QUEUE'
          iv_xml_data     = lv_xml
          iv_xdp_layout   = ls_template-xdp_template
          is_options      = value #(
            trace_level = 4 "Use 0 in production environment
          )
        IMPORTING
          ev_trace_string = data(lv_trace)
          ev_pdl          = data(lv_pdf)
      ).
      out->write( 'Output was generated' ).

      cl_print_queue_utils=>create_queue_item_by_data(
        iv_qname = 'PRINT_QUEUE'
        iv_print_data = lv_pdf
        iv_name_of_main_doc = 'DSAG DEMO Output'
      ).
      out->write( 'Output was sent to print queue' ).

    catch cx_fp_fdp_error zcx_fp_tmpl_store_error cx_fp_ads_util into data(lo_err).
      out->write( 'Exception occurred.' ).
    endtry.
    out->write( 'Finished processing.' ).
  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_dsag_fill_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS zcl_dsag_fill_data IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    data:
      lt_bill_doc   type STANDARD TABLE OF zdsag_billdoc,
      lt_bill_item  type STANDARD TABLE OF zdsag_billitem,
      lt_receiver   type STANDARD TABLE OF zdsag_receiver,
      lt_product    type STANDARD TABLE OF zdsag_product.

    delete from zdsag_billdoc.
    delete from zdsag_billitem.
    delete from zdsag_product.
    delete from zdsag_receiver.

    append value zdsag_receiver(
      client          = 100
      id              = 1
      country         = 'USA'
      zip             = 'New York, NY 10023'
      street          = '123 Sesame Street'
      name            = 'Cookie Monster'
    ) to lt_receiver.


    append value zdsag_billdoc(
      client          = 100
      created_at      = 20220523164722
      id              = 1
      payment_method  = 'Cash'
      receiver        = 1
    ) to lt_bill_doc.

    append value zdsag_billitem(
      client          = 100
      billdoc         = 1
      id              = 1
      amount          = 10000
      product         = 1
    ) to lt_bill_item.

    append value zdsag_billitem(
      client          = 100
      billdoc         = 1
      id              = 2
      amount          = 1
      product         = 2
    ) to lt_bill_item.

    append value zdsag_product(
      client          = 100
      name            = 'Cookie'
      currency        = 'EUR'
      id              = 1
      price           = '5'
      vat             = 7
    ) to lt_product.

    append value zdsag_product(
      client          = 100
      name            = 'Versandkosten'
      currency        = 'EUR'
      id              = 2
      price           = '100'
      vat             = 19
    ) to lt_product.

    insert zdsag_billdoc  FROM TABLE @lt_bill_doc.
    insert zdsag_billitem FROM TABLE @lt_bill_item.
    insert zdsag_product  FROM TABLE @lt_product.
    insert zdsag_receiver FROM TABLE @lt_receiver.

  ENDMETHOD.

ENDCLASS.""",
    r"""CLASS zcl_dsag_prod_ean_calc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    interfaces IF_SADL_EXIT_CALC_ELEMENT_READ.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS zcl_dsag_prod_ean_calc IMPLEMENTATION.
  METHOD if_sadl_exit_calc_element_read~calculate.
    data(lo_generator) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed(  )
      min = 0
      max = 9
    ).

    LOOP AT it_requested_calc_elements INTO DATA(lv_virtual_field_name).
      LOOP AT ct_calculated_data ASSIGNING FIELD-SYMBOL(<ls_calculation_structure>).
        ASSIGN COMPONENT lv_virtual_field_name OF STRUCTURE <ls_calculation_structure> TO FIELD-SYMBOL(<lv_virtual_field_value>).
        IF lv_virtual_field_name = 'EAN'.
          data lv_ean type string VALUE ''.
          clear lv_ean.


          do 10 times.
            lv_ean = lv_ean && lo_generator->get_next(  ).
          enddo.

          <lv_virtual_field_value> = lv_ean.

        ELSE.
          "Data Initialization for this type is not implemented yet
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~get_calculation_info.

  ENDMETHOD.

ENDCLASS.""",
    r"""CLASS zcl_fp_tmpl_store_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      mo_http_destination     type ref to if_http_destination,
      mv_client               type ref to if_web_http_client.
    TYPES :
      BEGIN OF ty_schema_body,
        xsd_Schema              type xstring,
        schema_Name             type c LENGTH 30,
        note                    type c LENGTH 280,
      END OF ty_schema_body,
      BEGIN OF ty_schema_body_in,
        xsd_Schema              type string,
        schema_Name             type c LENGTH 30,
        note                    type c LENGTH 280,
      END OF ty_schema_body_in,
      BEGIN OF ty_template_body,
        xdp_Template            type xstring,
        template_Name           type c LENGTH 30,
        description             type c LENGTH 280,
        note                    type c LENGTH 280,
        locale                  type c LENGTH 6,
        language                type c LENGTH 280,
        master_Language         type c LENGTH 280,
        business_Area           type c LENGTH 280,
        business_Department     type c LENGTH 280,
      END OF ty_template_body,
      BEGIN OF ty_template_body_in,
        xdp_Template            type string,
        template_Name           type c LENGTH 30,
        description             type c LENGTH 280,
        note                    type c LENGTH 280,
        locale                  type c LENGTH 6,
        language                type c LENGTH 280,
        master_Language         type c LENGTH 280,
        business_Area           type c LENGTH 280,
        business_Department     type c LENGTH 280,
      END OF ty_template_body_in,
      tt_templates type STANDARD TABLE OF ty_template_body WITH KEY template_Name,
      BEGIN OF ty_form_body,
        form_Name               type c LENGTH 30,
        description             type c LENGTH 280,
        note                    type c LENGTH 30,
      END OF ty_form_body,
      tt_forms type STANDARD TABLE OF ty_form_body WITH KEY form_Name,
      BEGIN OF ty_version_history,
        version_Object_Id       type string,
        version_Number          type string,
        is_Latest_Version       type abap_boolean,
        last_Modification_Date  type string,
      END OF ty_version_history,
      tt_versions type STANDARD TABLE OF ty_version_history WITH KEY version_object_id.
    METHODS:
      constructor
        IMPORTING
          iv_use_destination_service type abap_boolean DEFAULT abap_true
          iv_name                    type string OPTIONAL
          iv_service_instance_name   type string
        RAISING
          zcx_fp_tmpl_store_error,
      list_forms
        IMPORTING
          iv_limit  type i DEFAULT 10
          iv_offset type i DEFAULT 0
        RETURNING VALUE(rt_forms) type tt_forms
        RAISING
          zcx_fp_tmpl_store_error,
      get_form_by_name
        IMPORTING
          iv_name type string
        RETURNING VALUE(rs_form) type ty_form_body
        RAISING
           zcx_fp_tmpl_store_error,
      list_templates
        IMPORTING
          iv_form_name            type string
          iv_locale               type string OPTIONAL
          iv_language             type string OPTIONAL
          iv_template_name        type string OPTIONAL
          iv_master_language      type string OPTIONAL
          iv_business_area        type string OPTIONAL
          iv_business_department  type string OPTIONAL
          iv_limit                type i default 10
          iv_offset               type i default 0
        RETURNING VALUE(rt_templates) type tt_templates
        RAISING
           zcx_fp_tmpl_store_error,
      get_template_history_by_name
        IMPORTING
          iv_form_name      type string
          iv_template_name  type string
        RETURNING VALUE(rt_versions) type tt_versions
        RAISING
           zcx_fp_tmpl_store_error,
      get_template_by_name
        IMPORTING
          iv_get_binary    type abap_boolean default abap_false
          iv_form_name     type string
          iv_template_name type string
        RETURNING VALUE(rs_template) type ty_template_body
        RAISING
           zcx_fp_tmpl_store_error,
      get_template_by_id
        IMPORTING
          iv_form_name       type string
          iv_object_id  type string
        RETURNING VALUE(rs_template) type ty_template_body
        RAISING
           zcx_fp_tmpl_store_error,
      get_schema_history_by_name
        IMPORTING
          iv_form_name      type string
        RETURNING VALUE(rt_versions) type tt_versions
        RAISING
           zcx_fp_tmpl_store_error,
      get_schema_by_name
        IMPORTING
          iv_get_binary        type abap_boolean default abap_false
          iv_form_name         type string
        RETURNING VALUE(rs_schema) type ty_schema_body
        RAISING
          zcx_fp_tmpl_store_error,
      get_schema_by_id
        IMPORTING
          iv_form_name type string
          iv_object_id type string
        RETURNING VALUE(rs_schema) type ty_schema_body
        RAISING
           zcx_fp_tmpl_store_error,
      set_form
        IMPORTING
          iv_form_name type string
          is_form     type ty_form_body
        RAISING
           zcx_fp_tmpl_store_error,
      set_template
        IMPORTING
          iv_template_name type string
          iv_form_name type string
          is_template  type ty_template_body
        RAISING
           zcx_fp_tmpl_store_error,
      set_schema
        IMPORTING
          iv_form_name type string
          is_data      type ty_schema_body
        RAISING
          zcx_fp_tmpl_store_error,
      delete_form
        IMPORTING
          iv_form_name type string
        RAISING
           zcx_fp_tmpl_store_error,
      delete_template_in_form
        IMPORTING
          iv_form_name      type string
          iv_template_name  type string
        RAISING
           zcx_fp_tmpl_store_error,
      delete_schema_in_form
        IMPORTING
          iv_form_name    type string
        RAISING
          zcx_fp_tmpl_store_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    data:
      mv_use_dest_srv type abap_boolean,
      mv_name type string,
      mv_instance_name type string.
    methods:
      __close_request,
      __conv_path
        IMPORTING
          iv_path type string
        RETURNING VALUE(rv_path) type string,
      __get_request
        returning value(ro_request) type ref to if_web_http_request,
      __json2abap
        importing
          ir_input_data type data
        changing
          cr_abap_data  type data,
      __execute
        IMPORTING
           i_method type if_web_http_client=>method
           i_expect type i DEFAULT 200
        RETURNING VALUE(ro_response) type ref to if_web_http_response
        RAISING
          zcx_fp_tmpl_store_error.

ENDCLASS.""",
    r"""CLASS ZCL_FP_TMPL_STORE_CLIENT IMPLEMENTATION.


  METHOD constructor.
    mv_use_dest_srv = iv_use_destination_service.
    mv_instance_name = iv_service_instance_name.
    mv_name = iv_name.
      ENDMETHOD.


  METHOD delete_form.
    DATA(lo_request) = __get_request( ).
    lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }| ) ).
    data(lo_response) = __execute(
      i_method = if_web_http_client=>delete
      i_expect = 200
    ).
    __close_request(  ).
  ENDMETHOD.


  METHOD delete_schema_in_form.
    data(ls_schema) = me->get_schema_by_name(
      iv_form_name = iv_form_name
      iv_get_binary = abap_false
    ).

    DATA(lo_request) = __get_request( ).
    lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }/schema/{ ls_schema-schema_name }| ) ).
    lo_request->set_query( |allVersions=true| ).
    data(lo_response) = __execute(
      i_method = if_web_http_client=>delete
      i_expect = 200
    ).
    __close_request(  ).
  ENDMETHOD.


  METHOD delete_template_in_form.
    DATA(lo_request) = __get_request( ).
    lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }| ) ).
    data(lo_response) = __execute(
      i_method = if_web_http_client=>delete
      i_expect = 200
    ).
    __close_request(  ).
  ENDMETHOD.


  METHOD get_form_by_name.
    DATA(lo_request) = __get_request( ).
    lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_name }| ) ).
    lo_request->set_query( |formData| ).
    data(lo_response) = __execute(
      i_method = if_web_http_client=>get
      i_expect = 200
    ).

    DATA lr_data type ref to data.
    lr_data = /ui2/cl_json=>generate(
      json = lo_response->get_text( )
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    if lr_data is bound.
      assign lr_data->* to FIELD-SYMBOL(<data>).
      __json2abap(
        EXPORTING
          ir_input_data = <data>
        CHANGING
          cr_abap_data = rs_form
      ).

    endif.
    __close_request(  ).
  ENDMETHOD.


  METHOD get_schema_by_id.
    DATA(lo_request) = __get_request( ).
    lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }/schema/{ iv_object_id }| ) ).
    lo_request->set_query( |select=xsdSchema,schemaData&isObjectId=true| ).

    data(lo_response) = __execute(
      i_method = if_web_http_client=>get
      i_expect = 200
    ).

    DATA(lv_json_response) = lo_response->get_text( ).
    DATA lr_data type ref to data.
    lr_data = /ui2/cl_json=>generate(
      json = lv_json_response
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    if lr_data is bound.
      assign lr_data->* to FIELD-SYMBOL(<data>).
      __json2abap(
        EXPORTING
          ir_input_data = <data>
        CHANGING
          cr_abap_data = rs_schema
      ).
    endif.
    __close_request(  ).
  ENDMETHOD.


  METHOD get_schema_by_name.
    DATA(lo_request) = __get_request( ).
    lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }| ) ).
    if iv_get_binary = abap_true.
      lo_request->set_query( |select=schemaData,xsdSchema| ).
    else.
      lo_request->set_query( |select=schemaData| ).
    endif.

    data(lo_response) = __execute(
      i_method = if_web_http_client=>get
      i_expect = 200
    ).

    DATA(lv_json_response) = lo_response->get_text( ).
    DATA lr_data type ref to data.
    lr_data = /ui2/cl_json=>generate(
      json = lv_json_response
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    if lr_data is bound.
      assign lr_data->* to FIELD-SYMBOL(<data>).

      assign component 'SCHEMA' of structure <data> to FIELD-SYMBOL(<schema>).

      if <schema> is assigned.
        __json2abap(
          EXPORTING
            ir_input_data = <schema>->*
          CHANGING
            cr_abap_data = rs_schema
        ).
      else.
        raise EXCEPTION type zcx_fp_tmpl_store_error
          EXPORTING
            mv_http_status_code = 404
            mv_http_reason = 'No schema maintained for form'
            textid = zcx_fp_tmpl_store_error=>data_error.
      endif.

    endif.
    __close_request(  ).
  ENDMETHOD.


  METHOD get_schema_history_by_name.
    data(ls_schema) = me->get_schema_by_name(
      iv_form_name = iv_form_name
      iv_get_binary = abap_false
    ).
    DATA(lo_request) = __get_request( ).
    lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }/schema/{ ls_schema-schema_name }| ) ).
    lo_request->set_query( |select=schemaData,schemaVersions| ).

    data(lo_response) = __execute(
      i_method = if_web_http_client=>get
      i_expect = 200
    ).

    DATA(lv_json_response) = lo_response->get_text( ).
    DATA lr_data type ref to data.
    lr_data = /ui2/cl_json=>generate(
      json = lv_json_response
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    if lr_data is bound.
      field-SYMBOLS: <versions> type STANDARD TABLE.

      assign lr_data->* to FIELD-SYMBOL(<data>).
      assign component `VERSIONS` of structure <data> to <versions>.

      loop at <versions> ASSIGNING FIELD-SYMBOL(<version>).
        data ls_version type ty_version_history.

        __json2abap(
          EXPORTING
            ir_input_data = <version>->*
          CHANGING
            cr_abap_data = ls_version
        ).

        append ls_version to rt_versions.
      endloop.

    endif.
    __close_request(  ).
  ENDMETHOD.


  METHOD get_template_by_id.
    DATA(lo_request) = __get_request( ).
    lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }/templates/{ iv_object_id }| ) ).
    lo_request->set_query( |select=xdpTemplate,templateData&isObjectId=true| ).

    data(lo_response) = __execute(
      i_method = if_web_http_client=>get
      i_expect = 200
    ).

    DATA(lv_json_response) = lo_response->get_text( ).
    DATA lr_data type ref to data.
    lr_data = /ui2/cl_json=>generate(
      json = lv_json_response
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    if lr_data is bound.
      assign lr_data->* to FIELD-SYMBOL(<data>).
      __json2abap(
          EXPORTING
            ir_input_data = <data>->*
          CHANGING
            cr_abap_data = rs_template
      ).
    endif.
    __close_request(  ).
  ENDMETHOD.


  METHOD get_template_by_name.

    DATA(lo_request) = __get_request( ).
    lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }/templates/{ iv_template_name }| ) ).
    if iv_get_binary = abap_true.
      lo_request->set_query( |select=xdpTemplate,templateData| ).
    else.
      lo_request->set_query( |select=templateData| ).
    endif.
    data(lo_response) = __execute(
      i_method = if_web_http_client=>get
      i_expect = 200
    ).

    DATA(lv_json_response) = lo_response->get_text( ).
    DATA lr_data type ref to data.
    lr_data = /ui2/cl_json=>generate(
      json = lv_json_response
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    if lr_data is bound.
      assign lr_data->* to FIELD-SYMBOL(<data>).
      __json2abap(
          EXPORTING
            ir_input_data = <data>
          CHANGING
            cr_abap_data = rs_template
      ).
    endif.
    __close_request(  ).
  ENDMETHOD.


  METHOD get_template_history_by_name.
    DATA(lo_request) = __get_request( ).
    lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }/templates/{ iv_template_name }| ) ).
    lo_request->set_query( |select=templateData,templateVersions| ).

    data(lo_response) = __execute(
      i_method = if_web_http_client=>get
      i_expect = 200
    ).

    DATA(lv_json_response) = lo_response->get_text( ).
    DATA lr_data type ref to data.
    lr_data = /ui2/cl_json=>generate(
      json = lv_json_response
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    if lr_data is bound.
      field-SYMBOLS: <versions> type STANDARD TABLE.

      assign lr_data->* to FIELD-SYMBOL(<data>).
      assign component `VERSIONS` of structure <data> to <versions>.

      loop at <versions> ASSIGNING FIELD-SYMBOL(<version>).
        data ls_version type ty_version_history.

        __json2abap(
          EXPORTING
            ir_input_data = <version>->*
          CHANGING
            cr_abap_data = ls_version
        ).

        append ls_version to rt_versions.
      endloop.

    endif.
    __close_request(  ).
  ENDMETHOD.


  METHOD list_forms.
    DATA(lo_request) = __get_request( ).
    lo_request->set_uri_path( __conv_path( |/v1/forms| ) ).
    lo_request->set_query( |limit={ iv_limit }&offset={ iv_offset }&select=formData| ).

    data(lo_response) = __execute(
      i_method = if_web_http_client=>get
      i_expect = 200
    ).

    DATA(lv_json_response) = lo_response->get_text( ).
    DATA lr_data type ref to data.
    lr_data = /ui2/cl_json=>generate(
      json = lv_json_response
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    if lr_data is bound.
      field-symbols:
         <data> type any table.

      assign lr_data->* to <data>.

      loop at <data> assigning FIELD-SYMBOL(<form>).
        data ls_form type ty_form_body.

        __json2abap(
          EXPORTING
            ir_input_data = <form>->*
          CHANGING
            cr_abap_data = ls_form
        ).

        append ls_form to rt_forms.
      endloop.
    endif.
    __close_request(  ).
  ENDMETHOD.


  METHOD list_templates.
    DATA(lo_request) = __get_request( ).
    data(lv_query) = |select=templateData&limit={ iv_limit }&offset={ iv_offset }|.
    if iv_business_area is not initial.
      lv_query = lv_query && |&businessArea={ iv_business_area }|.
    endif.

    if iv_business_department is not initial.
      lv_query = lv_query && |&businessDepartment={ iv_business_department }|.
    endif.

    if iv_language is not initial.
      lv_query = lv_query && |&language={ iv_language }|.
    endif.

    if iv_locale is not initial.
      lv_query = lv_query && |&locale={ iv_locale }|.
    endif.

    if iv_master_language is not initial.
      lv_query = lv_query && |&masterLanguage={ iv_master_language }|.
    endif.

    if iv_template_name is not initial.
      lv_query = lv_query && |&templateName={ iv_template_name }|.
    endif.

    lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }/templates| ) ).
    lo_request->set_query( |limit={ iv_limit }&offset={ iv_offset }&select=formData| ).

    data(lo_response) = __execute(
      i_method = if_web_http_client=>get
      i_expect = 200
    ).

    DATA(lv_json_response) = lo_response->get_text( ).
    DATA lr_data type ref to data.
    lr_data = /ui2/cl_json=>generate(
      json = lv_json_response
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    if lr_data is bound.
      field-symbols: <data> type any table.

      assign lr_data->* to <data>.

      loop at <data> assigning field-symbol(<template>).
        data ls_template type ty_template_body.

        __json2abap(
          exporting
            ir_input_data = <template>->*
          CHANGING
            cr_abap_data = ls_template
        ).

        append ls_template to rt_templates.

      endloop.
    endif.
    __close_request(  ).
  ENDMETHOD.


  METHOD set_form.
    data(lv_exists) = abap_false.

    try.
      me->get_form_by_name( iv_name = iv_form_name ).
      lv_exists = abap_true.
    catch zcx_fp_tmpl_store_error into data(lo_data_error).
      if lo_data_error->mv_http_status_code <> 404.
        raise exception lo_data_error.
      ENDIF.
    endtry.

    DATA(lo_request) = __get_request( ).
    if lv_exists = abap_true.
      lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }| ) ).
    else.
      lo_request->set_uri_path( __conv_path( |/v1/forms| ) ).
    ENDIF.

    DATA(lv_json) = /ui2/cl_json=>serialize(
      data = is_form
      compress = abap_true
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    lo_request->append_text(
        EXPORTING
          data   = lv_json
    ).

    if lv_exists = abap_true.
      __execute(
        i_method = if_web_http_client=>put
      ).
    else.
      __execute(
        i_method = if_web_http_client=>post
      ).
    endif.
    __close_request(  ).
  ENDMETHOD.


  METHOD set_schema.
    data(lv_exists) = abap_false.

    try.
      data(ls_schema) = me->get_schema_by_name(
        iv_form_name = iv_form_name
        iv_get_binary = abap_false
      ).
      lv_exists = abap_true.
    catch zcx_fp_tmpl_store_error into data(lo_data_error).
      if lo_data_error->mv_http_status_code <> 404.
        raise exception lo_data_error.
      ENDIF.
    endtry.

    DATA(lo_request) = __get_request( ).
    if lv_exists = abap_true.
      lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }/schema/{ ls_schema-schema_name }| ) ).
    else.
      lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }/schema| ) ).
    ENDIF.

    data(ls_body) = value ty_schema_body_in(
      note = is_data-note
      schema_name = is_data-schema_name
      xsd_schema = cl_web_http_utility=>encode_base64( cl_web_http_utility=>decode_utf8( is_data-xsd_schema ) )
    ).

    DATA(lv_json) = /ui2/cl_json=>serialize(
      data = ls_body
      compress = abap_true
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    lo_request->append_text(
        EXPORTING
          data   = lv_json
    ).

    if lv_exists = abap_true.
      __execute(
        i_method = if_web_http_client=>put
      ).
    else.
      __execute(
        i_method = if_web_http_client=>post
      ).
    endif.
    __close_request(  ).
  ENDMETHOD.


  METHOD set_template.
    data(lv_exists) = abap_false.

    try.
      data(lv_template) = me->get_template_by_name(
        iv_form_name = iv_form_name
        iv_get_binary = abap_false
        iv_template_name = iv_template_name
      ).
      lv_exists = abap_true.
    catch zcx_fp_tmpl_store_error into data(lo_data_error).
      if lo_data_error->mv_http_status_code <> 404.
        raise exception lo_data_error.
      ENDIF.
    endtry.

    DATA(lo_request) = __get_request( ).
    if lv_exists = abap_true.
      lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }/templates/{ iv_template_name }| ) ).
    else.
      lo_request->set_uri_path( __conv_path( |/v1/forms/{ iv_form_name }/templates| ) ).
    ENDIF.

    data(ls_body) = value ty_template_body_in(
      note = is_template-note
      business_area = is_template-business_area
      business_department = is_template-business_department
      description = is_template-description
      language = is_template-language
      locale = is_template-locale
      master_language = is_template-master_language
      template_name = is_template-template_name
      xdp_template = cl_web_http_utility=>encode_base64( cl_web_http_utility=>decode_utf8( is_template-xdp_template ) )
    ).

    DATA(lv_json) = /ui2/cl_json=>serialize(
      data = ls_body
      compress = abap_true
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    lo_request->append_text(
        EXPORTING
          data   = lv_json
    ).

    if lv_exists = abap_true.
      __execute(
        i_method = if_web_http_client=>put
      ).
    else.
      __execute(
        i_method = if_web_http_client=>post
      ).
    endif.
    __close_request(  ).
  ENDMETHOD.


  METHOD __execute.
    try.
      ro_response = mv_client->execute( i_method = i_method ).
      if ro_response->get_status(  )-code <> i_expect.
        RAISE EXCEPTION type zcx_fp_tmpl_store_error
          EXPORTING
            textid = zcx_fp_tmpl_store_error=>data_error
            mv_http_status_code = ro_response->get_status(  )-code
            mv_http_reason = ro_response->get_status(  )-reason.
      ENDIF.
    catch cx_web_http_client_error into data(lo_http_error).
      RAISE EXCEPTION type zcx_fp_tmpl_store_error
        EXPORTING
          textid = zcx_fp_tmpl_store_error=>http_client_error
          mv_http_reason = lo_http_error->get_longtext( ).
    endtry.
  ENDMETHOD.


  METHOD __json2abap.
    data(lo_input_struct)   = cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( p_data = ir_input_data ) ).
    data(lo_target_struct)  = cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( p_data = cr_abap_data ) ).


    LOOP at lo_input_struct->components ASSIGNING FIELD-SYMBOL(<ls_component>).
      if line_exists( lo_target_struct->components[ name = <ls_component>-name ] ).
        assign component <ls_component>-name of structure ir_input_data to FIELD-SYMBOL(<field_in_data>).
        assign component <ls_component>-name of structure cr_abap_data to FIELD-SYMBOL(<field_out_data>).

        if lo_target_struct->components[ name = <ls_component>-name ]-type_kind = cl_abap_typedescr=>typekind_xstring.
          <field_out_data> = cl_web_http_utility=>decode_x_base64( <field_in_data>->* ).
        else.
          <field_out_data> = <field_in_data>->*.
        endif.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD __get_request.
    try.
      if mv_client is BOUND.
        mv_client->close(  ).
      endif.
      IF mv_use_dest_srv = abap_true.
        mo_http_destination = cl_http_destination_provider=>create_by_cloud_destination(
          i_service_instance_name = CONV #( mv_instance_name )
          i_name                  = mv_name
          i_authn_mode            = if_a4c_cp_service=>service_specific
        ).
      ELSE.
        mo_http_destination = cl_http_destination_provider=>create_by_comm_arrangement(
          comm_scenario           = CONV #( mv_instance_name )
        ).
      ENDIF.
      mv_client = cl_web_http_client_manager=>create_by_http_destination( mo_http_destination ).
    catch cx_web_http_client_error cx_http_dest_provider_error .
    endtry.

    ro_request = mv_client->get_http_request( ).
    ro_request->set_header_fields( VALUE #(
      ( name = 'Accept' value = 'application/json, text/plain, */*'  )
      ( name = 'Content-Type' value = 'application/json;charset=utf-8'  )
    ) ).
  ENDMETHOD.

  METHOD __CONV_PATH.
    rv_path = iv_path.
    if mv_use_dest_srv = abap_false.
      SHIFT rv_path left.
    endif.
  ENDMETHOD.

  METHOD __close_request.

  ENDMETHOD.

ENDCLASS.""",
    r"""CLASS zcx_fp_tmpl_store_error DEFINITION
  public
  inheriting from CX_STATIC_CHECK
  create public .

  PUBLIC SECTION.
    interfaces IF_T100_DYN_MSG .
    interfaces IF_T100_MESSAGE .

    CONSTANTS:
      begin of SETUP_NOT_COMPLETE,
        msgid type symsgid value 'Z_TMPL_STORE',
        msgno type symsgno value '001',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of SETUP_NOT_COMPLETE,
      begin of DATA_ERROR,
        msgid type symsgid value 'Z_TMPL_STORE',
        msgno type symsgno value '002',
        attr1 type scx_attrname value 'MV_HTTP_STATUS_CODE',
        attr2 type scx_attrname value 'MV_HTTP_REASON',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of DATA_ERROR,
      begin of HTTP_CLIENT_ERROR,
        msgid type symsgid value 'Z_TMPL_STORE',
        msgno type symsgno value '003',
        attr1 type scx_attrname value 'MV_HTTP_REASON',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of HTTP_CLIENT_ERROR.

    data MV_HTTP_STATUS_CODE type I .
    data MV_HTTP_REASON type STRING .
    methods CONSTRUCTOR
      importing
        !TEXTID like IF_T100_MESSAGE=>T100KEY optional
        !PREVIOUS like PREVIOUS optional
        !MV_HTTP_STATUS_CODE type I optional
        !MV_HTTP_REASON type STRING optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_fp_tmpl_store_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD SUPER->CONSTRUCTOR
      EXPORTING
        PREVIOUS = PREVIOUS.
    me->MV_HTTP_STATUS_CODE = MV_HTTP_STATUS_CODE .
    me->MV_HTTP_REASON = MV_HTTP_REASON .
    clear me->textid.
    if textid is initial.
      IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
    else.
      IF_T100_MESSAGE~T100KEY = TEXTID.
    endif.

  ENDMETHOD.

ENDCLASS.""",
    r"""class /VPCOE/CL_ADJUST_API_EXAMPLE definition
  public
  final
  create public .

public section.

  interfaces /VPCOE/IF_ADJ_DATA_RETRIEVAL .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.
""",
    r"""CLASS /vpcoe/cl_fb_notification DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /vpcoe/if_notifictn_processing .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS /VPCOE/CL_FB_NOTIFICATION IMPLEMENTATION.
      METHOD /vpcoe/if_notifictn_processing~notification_change_body.

    cv_subject = |VPCoE:{ is_payload-service_id }, { is_payload-response_status }( { is_payload-response_message } )|.

    ct_message = VALUE #( ( line = `***This is an automated generated email***` )
                          ( line = `Hello Colleagues,` )
                          ( line = |Sending process was failed for { is_payload-service_id }.| )
                          ( line = |Response status: { is_payload-response_status }.| )
                          ( line = |Response message: { is_payload-response_message }.| )
                          ( line = |Session ID: { is_payload-session_id }.| ) ).

    IF io_log IS BOUND AND io_log->check( ).
      INSERT VALUE #( line = `Messages from the Log:` ) INTO TABLE ct_message.

      io_log->get_messages(
        IMPORTING
          et_messages = DATA(lt_log_msgs) ).

      LOOP AT lt_log_msgs ASSIGNING FIELD-SYMBOL(<ls_log_msg>).
        IF <ls_log_msg>-id IS NOT INITIAL.
          MESSAGE ID <ls_log_msg>-id TYPE <ls_log_msg>-type NUMBER <ls_log_msg>-number
             WITH <ls_log_msg>-message_v1 <ls_log_msg>-message_v2 <ls_log_msg>-message_v3 <ls_log_msg>-message_v4
                INTO /vpcoe/cl_rdp_log=>sv_msg_text.
          INSERT VALUE #( line = /vpcoe/cl_rdp_log=>sv_msg_text ) INTO TABLE ct_message.
        ELSE.
          INSERT VALUE #( line = <ls_log_msg>-message ) INTO TABLE ct_message.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
    METHOD /vpcoe/if_notifictn_processing~notification_change_sender.
  ENDMETHOD.
    METHOD /vpcoe/if_notifictn_processing~notification_overwrite.
  ENDMETHOD.
ENDCLASS.""",
    r"""class /VPCOE/CL_PCKCOMP_BOM_EXMPL definition
  public
  inheriting from /VPCOE/CL_UPH_PROC_BASE_BOM
  final
  create public .

public section.

  methods /VPCOE/IF_UPH_ENTITY_BOM_PROC~MAP_BOM_DATA
    redefinition .
protected section.

  types:
    BEGIN OF lty_class,
        class_id TYPE clint,
      END OF lty_class .
  types:
    ltty_class TYPE STANDARD TABLE OF lty_class WITH KEY class_id .

  constants MC_RELEVANT_CLASS_NAME type STRING value 'ZMCL_PACKELEM_ATTR' ##NO_TEXT.
  data MR_TCLA_MULTOBJ type ref to ABAP_BOOL .

  methods MAP_BOM_ITEMS
    importing
      !IT_BOM_ITEMS type /VPCOE/T_UPH_WRAP_BOM_ITEM
      !IV_PREVIOUS_ITEM_AMOUNT type I optional
      !IO_ACT_PACKCOMP type ref to /VPCOE/CL_UPH_ENT_PCKG_CMP_HDR .
  methods IS_VALID_PACKCOMP_ITEM
    importing
      !IV_BOM_MATERIAL type MATNR
      !IV_BOM_MATL_TYPE type MTART
    returning
      value(RV_RESULT) type ABAP_BOOL .
private section.

  methods DETERMINE_MATCLAS_VALIDITY
    importing
      !IV_MATERIAL type MATNR
      !IV_BOM_ITEM_VALID_FROM type DATUV
    returning
      value(RV_RESULT) type DATUV .
ENDCLASS.""",
    r"""CLASS /VPCOE/CL_PCKCOMP_BOM_EXMPL IMPLEMENTATION.
      METHOD /vpcoe/if_uph_entity_bom_proc~map_bom_data.
    DATA lo_packcomp TYPE REF TO /vpcoe/cl_uph_ent_pckg_cmp_hdr.

    " loop all BOM header data
    LOOP AT it_bom_data INTO DATA(lo_bom_data).

      CLEAR lo_packcomp.

      DATA(ls_cmp_hdr_data) = VALUE /vpcoe/s_uph_ent_pack_cmp_hdr( source = ms_parameters-source_id ).
      ls_cmp_hdr_data-displayid           = |{ lo_bom_data->get_material( ) }-{
                                               lo_bom_data->get_plant( ) }-{
                                               lo_bom_data->get_bom_usage( ) }-{
                                               lo_bom_data->get_bom_alternative_number( ) }-{
                                               lo_bom_data->get_valid_from( ) }|.

      ls_cmp_hdr_data-description         = |{ lo_bom_data->get_material_description( ) }|.
      ls_cmp_hdr_data-basequantity        = lo_bom_data->get_base_amount( ).
      ls_cmp_hdr_data-baseunitofmeasureid = lo_bom_data->get_base_uom( ).
      ls_cmp_hdr_data-consumersalesunit   = lo_bom_data->get_material_base_uom( ).

      DATA(lt_products) = VALUE /vpcoe/t_uph_entity_data( ( NEW /vpcoe/cl_uph_ent_pckg_product(
                                                                      is_data = VALUE #( productid  = lo_bom_data->get_material( )
                                                                                         valid_from = lo_bom_data->get_valid_from( )
                                                                                         valid_to   = lo_bom_data->get_valid_to( )
                                                                                         supplier   = ''
                                                                                         business_process_direction = 'ALL' ) ) ) ).

      lo_packcomp = NEW /vpcoe/cl_uph_ent_pckg_cmp_hdr( is_cmp_hdr_data  = ls_cmp_hdr_data
                                                        it_cmp_item_data = VALUE #( )
                                                        it_products      = lt_products ).

      " map BOM items
      map_bom_items( it_bom_items    = lo_bom_data->get_items( )
                     io_act_packcomp = lo_packcomp ).

      IF lo_packcomp->get_items( ) IS NOT INITIAL.

        INSERT lo_packcomp INTO TABLE rt_entity_data.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
    METHOD determine_matclas_validity.

    TYPES:
      BEGIN OF ltys_datuv,
        datuv TYPE datuv,
      END OF ltys_datuv,
      lty_t_datuv TYPE STANDARD TABLE OF ltys_datuv WITH DEFAULT KEY.

    DATA lt_result TYPE lty_t_datuv.

    "Check if multiple objects is active
    IF mr_tcla_multobj IS NOT BOUND.

      SELECT SINGLE multobj FROM tcla WHERE klart = '001' INTO @DATA(lv_multobj).
      mr_tcla_multobj = NEW abap_bool(  ).
      mr_tcla_multobj->* = lv_multobj.
    ENDIF.

    IF mr_tcla_multobj->* = abap_true.

      SELECT SINGLE cuobj FROM inob
        WHERE klart = '001' AND obtab  = 'MARA' AND objek = @iv_material
      INTO @DATA(lv_internal_matnr).                    "#EC CI_NOORDER

      IF lv_internal_matnr IS NOT INITIAL.
        SELECT DISTINCT ausp~datuv FROM ausp
         INNER JOIN inob ON ausp~objek = inob~cuobj
         INNER JOIN kssk ON kssk~objek = inob~cuobj AND kssk~mafid = 'O' AND kssk~klart = '001'
         INNER JOIN ksml ON ausp~atinn = ksml~imerk AND ksml~lkenz = ''
         INNER JOIN klah ON ksml~clint = klah~clint AND klah~clint = kssk~clint
          WHERE ausp~klart = '001'
             AND ausp~lkenz = ''
             AND ausp~objek = @lv_internal_matnr
             AND klah~class = @mc_relevant_class_name
             AND ausp~datuv <= @iv_bom_item_valid_from
           ORDER BY ausp~datuv DESCENDING
           INTO TABLE @lt_result.
      ENDIF.

    ELSE.

      SELECT DISTINCT ausp~datuv FROM ausp
      INNER JOIN kssk ON kssk~objek = ausp~objek AND kssk~mafid = 'O' AND kssk~klart = '001'
      INNER JOIN ksml ON ausp~atinn = ksml~imerk AND ksml~lkenz = ''
      INNER JOIN klah ON ksml~clint = klah~clint AND klah~clint = kssk~clint
      WHERE ausp~klart = '001'
        AND ausp~lkenz = ''
        AND ausp~objek = @iv_material
        AND klah~class = @mc_relevant_class_name
        AND ausp~datuv <= @iv_bom_item_valid_from
      ORDER BY ausp~datuv DESCENDING
      INTO TABLE @lt_result.

    ENDIF.

    IF lt_result IS NOT INITIAL.
      DATA(lv_datuv) = lt_result[ 1 ]-datuv.
      rv_result = COND #( WHEN lv_datuv = '00000000' THEN '00010101' ELSE lv_datuv ).
    ELSE.
      rv_result = iv_bom_item_valid_from.
    ENDIF.

  ENDMETHOD.
  METHOD is_valid_packcomp_item.

    TYPES:
      BEGIN OF ltys_clint,
        clint TYPE clint,
      END OF ltys_clint,
      lty_t_clint TYPE STANDARD TABLE OF ltys_clint WITH DEFAULT KEY.

    DATA: lt_result TYPE lty_t_clint.

    IF iv_bom_matl_type = 'VERP'.

      "Check class type multiple object customizing is active
      IF mr_tcla_multobj IS NOT BOUND.

        SELECT SINGLE multobj FROM tcla WHERE klart = '001' INTO @DATA(lv_multobj).
        mr_tcla_multobj = NEW abap_bool(  ).
        mr_tcla_multobj->* = lv_multobj.
      ENDIF.

      "If yes, set Internal ID from table INOB as matnr.
      IF mr_tcla_multobj->* = abap_true.

        SELECT SINGLE cuobj FROM inob
          WHERE
            klart = '001' AND obtab = 'MARA' AND objek = @iv_bom_material
          INTO @DATA(lv_internal_matnr).                "#EC CI_NOORDER

        IF lv_internal_matnr IS NOT INITIAL.

          SELECT DISTINCT kssk~clint FROM kssk
           INNER JOIN klah ON kssk~clint = klah~clint
           WHERE
            klah~class = @mc_relevant_class_name AND kssk~objek = @lv_internal_matnr
            INTO TABLE @lt_result.
        ENDIF.

      ELSE.

        SELECT DISTINCT kssk~clint FROM kssk
        INNER JOIN klah ON kssk~clint = klah~clint
        WHERE
        klah~class = @mc_relevant_class_name AND kssk~objek = @iv_bom_material
          INTO TABLE @lt_result.

      ENDIF.

      IF lt_result IS NOT INITIAL.
        rv_result = abap_true.
      ELSE.
        rv_result = abap_false.
      ENDIF.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.
    METHOD map_bom_items.
    " Recursive mapping of BOM items to packaging compositions and composition components

    LOOP AT it_bom_items INTO DATA(lo_bom_item).

      DATA(lv_item_category) = lo_bom_item->get_item_category( ).
      DATA(lv_material) = lo_bom_item->get_material( ).
      DATA(lv_material_type) = lo_bom_item->get_material_type( ).

      IF lv_item_category <> 'L' OR lv_material IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(lt_bom_sub_items) = lo_bom_item->get_items( ).

      IF lt_bom_sub_items IS NOT INITIAL.

        " go map sub BOM items
        map_bom_items( it_bom_items            = lt_bom_sub_items
                       io_act_packcomp         = io_act_packcomp ).
      ELSE.

        IF  is_valid_packcomp_item( iv_bom_material = lo_bom_item->get_material( ) iv_bom_matl_type = lv_material_type ).

          DATA(ls_packcomp_item_data) = VALUE /vpcoe/s_uph_ent_pack_cmp_item( ).

          ls_packcomp_item_data-packagingelementdisplayid = |{ lv_material }|.
          " get the correct material classification assignment valid from date
          DATA(lv_packcomp_item_valid_from) = determine_matclas_validity( iv_material            = lv_material
                                                                          iv_bom_item_valid_from = lo_bom_item->get_item_valid_from( ) ).

          ls_packcomp_item_data-packagingelementversion = |{ lv_packcomp_item_valid_from(4) }-{ lv_packcomp_item_valid_from+4(2) }-{ lv_packcomp_item_valid_from+6(2) }|.

          IF lo_bom_item->is_alternative_item( ).
            ls_packcomp_item_data-quantity = lo_bom_item->get_item_calculated_amount( ) * ( lo_bom_item->get_alt_item_usage_probability( ) / 100 ).
          ELSE.
            ls_packcomp_item_data-quantity = lo_bom_item->get_item_calculated_amount( ).
          ENDIF.

          ls_packcomp_item_data-quantityunitofmeasureid   = lo_bom_item->get_item_uom( ).

          DATA(lv_levelcode)                              = lo_bom_item->get_item_sort_string( ).
          ls_packcomp_item_data-levelcode                 = COND #( WHEN lv_levelcode IS INITIAL
                                                                    THEN '10'
                                                                    ELSE lv_levelcode ).

          ls_packcomp_item_data-eprgroup  = ''.
          ls_packcomp_item_data-wwfgroup  = ''.
          ls_packcomp_item_data-usage     = ''.

          " check dimension of item UoM
          CALL FUNCTION 'DIMENSION_CHECK'
            EXPORTING
              dimid                  = 'AAAADL'
              msehi                  = lo_bom_item->get_item_uom( )
            EXCEPTIONS
              dimension_check_failed = 1
              unit_not_valid         = 2
              OTHERS                 = 3.
          IF sy-subrc = 0.
            " set count since item UoM has no dimension
            ls_packcomp_item_data-count  = lo_bom_item->get_item_amount( ).
          ENDIF.

          DATA(lo_packcomp_item) = NEW /vpcoe/cl_uph_ent_pckg_cmp_itm( is_data = ls_packcomp_item_data ).

          DATA(lt_cmp_item_data) = io_act_packcomp->get_items( ).
          INSERT lo_packcomp_item INTO TABLE lt_cmp_item_data.
          io_act_packcomp->set_items( lt_cmp_item_data ).

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS lcl_exmpl_pckg_fee DEFINITION CREATE PUBLIC
   INHERITING FROM /vpcoe/cl_pckf_proc_pckg_fee.

  PUBLIC SECTION.
    METHODS /vpcoe/if_pckf_entity_proc~transfer_package REDEFINITION.

  PRIVATE SECTION.

    CONSTANTS:
      mc_condition_type_zrdp   TYPE kscha VALUE 'ZRDP',
      mc_condition_type_zesp   TYPE kscha VALUE 'ZESP',
      mc_condition_type_zest   TYPE kscha VALUE 'ZEST',
      mc_condition_type_zcon   TYPE kscha VALUE 'ZCON',
      mc_condition_type_zese   TYPE kscha VALUE 'ZESE',
      mc_procdir_inbound       TYPE /vpcoe/pckf_proc_drctn VALUE 'INBOUND',
      mc_procdir_outbound      TYPE /vpcoe/pckf_proc_drctn VALUE 'OUTBOUND',

      mc_kc_sales_country      TYPE string VALUE 'SD-01',
      mc_kc_sales_general      TYPE string VALUE 'SD-02',
      mc_kc_sales_material     TYPE string VALUE 'SD-03',
      mc_kc_sales_additional   TYPE string VALUE 'SD-04',
      mc_kc_purch_material     TYPE string VALUE 'MM-01',
      mc_kc_purch_general      TYPE string VALUE 'MM-02',
      mc_kc_customer_independent        TYPE string VALUE 'SD-05',
      mc_kc_customer_dependent    TYPE string VALUE 'SD-06'.

    DATA:
      mv_test_mode TYPE abap_bool.

    METHODS upsert_pricing_condition
      IMPORTING
                !iv_condition_type     TYPE kscha DEFAULT mc_condition_type_zrdp
                !ir_ent_pckg_fee       TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee
                !is_ent_org_data       TYPE /vpcoe/s_pckf_ent_org_data OPTIONAL
                !is_ent_supplier       TYPE /vpcoe/s_pckf_ent_suplr OPTIONAL
                !iv_fee_qty            TYPE kbetr OPTIONAL
                !iv_use_additional_fee TYPE abap_bool DEFAULT abap_false
                !iv_customer           TYPE string OPTIONAL
      RETURNING VALUE(rv_error)        TYPE abap_bool.

    METHODS pricing_condition_exists
      IMPORTING
                !iv_condition_type TYPE kscha
                !ir_ent_pckg_fee   TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee
                !is_ent_org_data   TYPE /vpcoe/s_pckf_ent_org_data OPTIONAL
                !is_ent_supplier   TYPE /vpcoe/s_pckf_ent_suplr OPTIONAL
                !iv_customer       TYPE string OPTIONAL
      RETURNING VALUE(rv_exists)   TYPE abap_bool.

    METHODS update_pricing_condition
      IMPORTING
                !iv_condition_type     TYPE kscha
                !ir_ent_pckg_fee       TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee
                !is_ent_org_data       TYPE /vpcoe/s_pckf_ent_org_data OPTIONAL
                !is_ent_supplier       TYPE /vpcoe/s_pckf_ent_suplr OPTIONAL
                !iv_fee_qty            TYPE kbetr OPTIONAL
                !iv_use_additional_fee TYPE abap_bool
                !iv_customer           TYPE string OPTIONAL
      RETURNING VALUE(rv_error)        TYPE abap_bool.

    METHODS create_pricing_condition
      IMPORTING
                !iv_condition_type     TYPE kscha
                !ir_ent_pckg_fee       TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee
                !is_ent_org_data       TYPE /vpcoe/s_pckf_ent_org_data OPTIONAL
                !is_ent_supplier       TYPE /vpcoe/s_pckf_ent_suplr OPTIONAL
                !iv_fee_qty            TYPE kbetr OPTIONAL
                !iv_use_additional_fee TYPE abap_bool
                !iv_customer            TYPE string OPTIONAL
      RETURNING VALUE(rv_error)        TYPE abap_bool.

    METHODS update_mclass_es_plastictax
      IMPORTING
                !ir_ent_pckg_fee TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee
      RETURNING VALUE(rv_error)  TYPE abap_bool.

    METHODS get_key_combination
      IMPORTING !ir_ent_pckg_fee          TYPE REF TO /vpcoe/cl_pckf_ent_pckg_fee
                !is_ent_org_data          TYPE /vpcoe/s_pckf_ent_org_data OPTIONAL
                !is_ent_supplier          TYPE /vpcoe/s_pckf_ent_suplr OPTIONAL
                !iv_condition_type        TYPE kscha OPTIONAL
      EXPORTING
                et_messages               TYPE /vpcoe/t_uph_msg
      RETURNING VALUE(rv_key_combination) TYPE string.

    METHODS date_to_external
      IMPORTING
                !iv_date         TYPE dats
      RETURNING VALUE(rv_result) TYPE bdc_fval.

    METHODS convert_to_external
      IMPORTING
                !iv_data         TYPE any
                !iv_max_decimals TYPE i OPTIONAL
      RETURNING VALUE(rv_result) TYPE bdc_fval.

    METHODS determ_purchase_org
      IMPORTING
                !is_ent_org_data TYPE /vpcoe/s_pckf_ent_org_data
      RETURNING VALUE(rv_result) TYPE ekorg.

ENDCLASS.""",
    r"""CLASS lcl_exmpl_pckg_fee IMPLEMENTATION.

  METHOD /vpcoe/if_pckf_entity_proc~transfer_package.

    DATA: lv_error              TYPE abap_bool,
          lv_description        TYPE string,
          lt_customer_exemption TYPE TABLE OF /vpcoe/s_pckf_custom_exemp.

    "Get test mode parameter value
    /vpcoe/if_pckf_entity_proc~get_parameter_value( EXPORTING iv_name = /vpcoe/if_pckf_entity_proc=>gc_parameters-test_mode IMPORTING ev_value = mv_test_mode ).

    "Transfer the packaging fee entities to pricing conditions
    LOOP AT it_entity_data INTO DATA(lr_entity_data).

      DATA(lr_pckf_data) = CAST /vpcoe/cl_pckf_ent_pckg_fee( lr_entity_data ).

      DATA(lt_orgdata) = lr_pckf_data->get_organizationdata( ).
      DATA(lt_suppliers) = lr_pckf_data->get_suppliers( ).

      IF lr_pckf_data->get_conai_extension( ) IS NOT INITIAL.
        lv_description = |{ lr_pckf_data->get_conai_extension( )-packagingcode }-{ lr_pckf_data->get_conai_extension( )-reportfractionname }|.
      ELSE.
        lv_description = ''.
      ENDIF.

      CLEAR lv_error.

      DATA(lt_customer_exemption_row) = NEW /vpcoe/cl_cstm_exemp_cache( )->/vpcoe/if_pckf_cache~get_entities( iv_entity_type = /vpcoe/if_pckf_entity_data=>gc_entities-gc_ent_cstm_exemp ).
      LOOP AT lt_customer_exemption_row ASSIGNING FIELD-SYMBOL(<ls_customer_exemption>).
        DATA(ls_customer_exemption) = CAST /vpcoe/cl_pckf_ent_cstm_exemp( <ls_customer_exemption> ).
        lt_customer_exemption = VALUE #( BASE lt_customer_exemption ( ls_customer_exemption->get_data( ) ) ).
      ENDLOOP.
      SORT lt_customer_exemption.
      DELETE ADJACENT DUPLICATES FROM lt_customer_exemption COMPARING ALL FIELDS.

      CASE lr_pckf_data->get_business_process_direction( ).

        WHEN mc_procdir_outbound.

          "Process outbound packaging fee (SD sales)

          "Organizational data exists?
          IF lt_orgdata IS INITIAL.
            lv_error = upsert_pricing_condition( ir_ent_pckg_fee = lr_pckf_data ).

            IF lv_error = abap_true.
              rv_error_flg = abap_true.
            ENDIF.
          ELSE.

            LOOP AT lt_orgdata INTO DATA(ls_orgdata).
              lv_error = upsert_pricing_condition( ir_ent_pckg_fee = lr_pckf_data is_ent_org_data = ls_orgdata ).

              IF lv_error = abap_true.
                rv_error_flg = abap_true.
              ENDIF.

              IF lr_pckf_data->get_extension( ) IS NOT INITIAL.
                lv_error = upsert_pricing_condition( iv_condition_type     = mc_condition_type_zesp
                                                     ir_ent_pckg_fee       = lr_pckf_data
                                                     is_ent_org_data       = ls_orgdata
                                                     iv_fee_qty            = lr_pckf_data->get_extension( )-inhouseproductionchargeablefee
                                                     iv_use_additional_fee = abap_true ).

                IF lv_error = abap_true.
                  rv_error_flg = abap_true.
                ENDIF.

                lv_error = upsert_pricing_condition( iv_condition_type     = mc_condition_type_zest
                                                     ir_ent_pckg_fee       = lr_pckf_data
                                                     is_ent_org_data       = ls_orgdata
                                                     iv_fee_qty            = lr_pckf_data->get_extension( )-inhouseprodnstatisticalfee
                                                     iv_use_additional_fee = abap_true ).
              ENDIF.

              IF lr_pckf_data->get_conai_extension( ) IS NOT INITIAL.


                IF line_exists( lt_customer_exemption[ report_fraction = lr_pckf_data->get_conai_extension( )-reportfraction
                                                       report_category = lr_pckf_data->get_reportcategoryid( ) ] ).
                  "create customer dependent pricing condition
                  LOOP AT lt_customer_exemption  ASSIGNING FIELD-SYMBOL(<ls_exemption>)
                    WHERE report_fraction = lr_pckf_data->get_conai_extension( )-reportfraction
                      AND report_category = lr_pckf_data->get_reportcategoryid( ).

                    lv_error = upsert_pricing_condition(
                             iv_condition_type     = mc_condition_type_zese
                             ir_ent_pckg_fee       = lr_pckf_data
                             is_ent_org_data       = ls_orgdata
                             iv_fee_qty            = CONV #( <ls_exemption>-exemption_percent )
                             iv_customer           = <ls_exemption>-customer
                             iv_use_additional_fee = abap_false ).
                  ENDLOOP.

                ELSE.
                  "create customer independent pricing condition
                  lv_error = upsert_pricing_condition(
                             iv_condition_type = mc_condition_type_zcon
                             ir_ent_pckg_fee   = lr_pckf_data
                             is_ent_org_data   = ls_orgdata
                             iv_fee_qty        = lr_pckf_data->get_conai_extension( )-feepertonne
                             iv_use_additional_fee = abap_false ).
                ENDIF.

                IF lv_error = abap_true.
                  rv_error_flg = abap_true.
                ENDIF.
              ENDIF.

            ENDLOOP.
          ENDIF.

          "Set Spain Plastic tax characteristics at material classification (if needed)
          lv_error = update_mclass_es_plastictax( ir_ent_pckg_fee = lr_pckf_data ).

          IF lv_error = abap_true.
            rv_error_flg = abap_true.
          ENDIF.

        WHEN mc_procdir_inbound.

          "Process inbound packaging fee (MM purchasing)

          "the Company Code from Org data is used to determine the purchasing organization
          LOOP AT lt_orgdata INTO ls_orgdata.

            "No supplier data exists
            IF lt_suppliers IS INITIAL.

              lv_error = upsert_pricing_condition( ir_ent_pckg_fee = lr_pckf_data is_ent_org_data = ls_orgdata ).
              IF lv_error = abap_true.
                rv_error_flg = abap_true.
              ENDIF.

            ELSE.

              "Supplier data exists
              LOOP AT lt_suppliers REFERENCE INTO DATA(lr_supplier).

                LOOP AT lt_orgdata INTO ls_orgdata.
                  lv_error = upsert_pricing_condition( ir_ent_pckg_fee = lr_pckf_data is_ent_org_data = ls_orgdata is_ent_supplier = lr_supplier->* ).

                  IF lv_error = abap_true.
                    rv_error_flg = abap_true.
                  ENDIF.
                ENDLOOP.

              ENDLOOP.

              "In case of fallback fee, write in addition without supplier
              IF lr_pckf_data->is_fallback_fee( ) = abap_true.

*                lv_error = upsert_pricing_condition( ir_ent_pckg_fee = lr_pckf_data is_ent_org_data = ls_orgdata ).
                IF lv_error = abap_true.
                  rv_error_flg = abap_true.
                ENDIF.

              ENDIF.

            ENDIF.

            IF lr_pckf_data->get_conai_extension( ) IS NOT INITIAL.


                IF line_exists( lt_customer_exemption[ report_fraction = lr_pckf_data->get_conai_extension( )-reportfraction
                                                       report_category = lr_pckf_data->get_reportcategoryid( ) ] ).
                  "create customer dependent pricing condition
                  LOOP AT lt_customer_exemption  ASSIGNING <ls_exemption>
                    WHERE report_fraction = lr_pckf_data->get_conai_extension( )-reportfraction
                      AND report_category = lr_pckf_data->get_reportcategoryid( ).

                  lv_error = upsert_pricing_condition(
                             iv_condition_type     = mc_condition_type_zese
                             ir_ent_pckg_fee       = lr_pckf_data
                             is_ent_org_data       = ls_orgdata
                             iv_fee_qty            = CONV #( <ls_exemption>-exemption_percent )
                             iv_use_additional_fee = abap_false ).
                  ENDLOOP.

                ELSE.
                  "create customer independent pricing condition
                  lv_error = upsert_pricing_condition(
                             iv_condition_type = mc_condition_type_zcon
                             ir_ent_pckg_fee   = lr_pckf_data
                             is_ent_org_data   = ls_orgdata
                             iv_fee_qty        = lr_pckf_data->get_conai_extension( )-feepertonne
                             iv_use_additional_fee = abap_false ).
                ENDIF.

                IF lv_error = abap_true.
                  rv_error_flg = abap_true.
                ENDIF.
              ENDIF.
          ENDLOOP.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD upsert_pricing_condition.

    DATA lv_error TYPE abap_bool.

    CLEAR rv_error.

    IF pricing_condition_exists( iv_condition_type = iv_condition_type ir_ent_pckg_fee = ir_ent_pckg_fee is_ent_org_data = is_ent_org_data is_ent_supplier = is_ent_supplier ) = abap_true.
      lv_error = update_pricing_condition( iv_condition_type = iv_condition_type ir_ent_pckg_fee = ir_ent_pckg_fee is_ent_org_data = is_ent_org_data is_ent_supplier = is_ent_supplier iv_fee_qty = iv_fee_qty iv_use_additional_fee = iv_use_additional_fee
                                           iv_customer = iv_customer ).
    ELSE.
      lv_error = create_pricing_condition( iv_condition_type = iv_condition_type ir_ent_pckg_fee = ir_ent_pckg_fee is_ent_org_data = is_ent_org_data is_ent_supplier = is_ent_supplier iv_fee_qty = iv_fee_qty iv_use_additional_fee = iv_use_additional_fee
                                           iv_customer = iv_customer ).
    ENDIF.

    IF lv_error = abap_true.
      rv_error = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD pricing_condition_exists.

    DATA lt_bdcdata               TYPE TABLE OF bdcdata.
    DATA lt_bdcmsgcoll            TYPE TABLE OF bdcmsgcoll.
    DATA lv_tcode TYPE tcode.

    CLEAR rv_exists.

    DATA(lv_keycombination) = get_key_combination( iv_condition_type = iv_condition_type ir_ent_pckg_fee = ir_ent_pckg_fee is_ent_org_data = is_ent_org_data is_ent_supplier = is_ent_supplier ).
    IF lv_keycombination IS INITIAL.
      RETURN.
    ENDIF.

    lv_tcode = COND #( WHEN ir_ent_pckg_fee->get_business_process_direction( ) EQ mc_procdir_outbound THEN 'VK13' ELSE 'MEK3' ).

    APPEND VALUE #( program  = 'SAPMV13A' dynpro   = '0100' dynbegin = 'X' ) TO lt_bdcdata.
    APPEND VALUE #( fnam = 'RV13A-KSCHL' fval = iv_condition_type ) TO lt_bdcdata.
    APPEND VALUE #( program  = 'SAPLV14A' dynpro   = '0100' dynbegin = 'X' ) TO lt_bdcdata.

    CASE lv_keycombination.
      WHEN mc_kc_sales_country.

        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(01)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(03)' fval = '' ) TO lt_bdcdata.


        APPEND VALUE #( program  = 'RV13A999' dynpro   = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_org_data-salesorganization ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = is_ent_org_data-distributionchannel ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F003' fval = is_ent_org_data-division ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F004' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F005-LOW' fval = ir_ent_pckg_fee->get_reportcategorycountry( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro = '1999' dynbegin = 'X' ) TO lt_bdcdata.

      WHEN mc_kc_sales_general.

        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(02)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'RV13A004' dynpro   = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_org_data-salesorganization ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = is_ent_org_data-distributionchannel ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F003-LOW' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro = '1004' dynbegin = 'X' ) TO lt_bdcdata.

      WHEN mc_kc_sales_material.

        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(03)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(03)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'RV13A005' dynpro   = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_org_data-salesorganization ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = is_ent_org_data-distributionchannel ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F004-LOW' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro = '1005' dynbegin = 'X' ) TO lt_bdcdata.

      WHEN mc_kc_sales_additional.

        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'RV13A004' dynpro = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_org_data-salesorganization ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = is_ent_org_data-distributionchannel ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F003-LOW' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'SAPMV13A' dynpro = '1004' dynbegin = 'X' ) TO lt_bdcdata.

      WHEN mc_kc_purch_material.

        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(01)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = '' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'RV13A018' dynpro   = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_supplier-supplier ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F003' fval = determ_purchase_org( is_ent_org_data = is_ent_org_data ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F004-LOW' fval = '0' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro = '1018' dynbegin = 'X' ) TO lt_bdcdata.

      WHEN mc_kc_purch_general.

        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(02)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'RV13A049' dynpro   = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = determ_purchase_org( is_ent_org_data = is_ent_org_data ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = '0' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F003-LOW' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro = '1049' dynbegin = 'X' ) TO lt_bdcdata.
      WHEN mc_kc_customer_independent.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'RV13A820' dynpro = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_org_data-companycode ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002-LOW' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'SAPMV13A' dynpro = '1820' dynbegin = 'X' ) TO lt_bdcdata.
      WHEN mc_kc_customer_dependent.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'RV13A971' dynpro = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_org_data-companycode ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F003-LOW' fval = iv_customer ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'SAPMV13A' dynpro = '1971' dynbegin = 'X' ) TO lt_bdcdata.
    ENDCASE.

    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '/ENDE' ) TO lt_bdcdata.

    CALL TRANSACTION lv_tcode
           USING lt_bdcdata
           MODE   'N'
           MESSAGES INTO lt_bdcmsgcoll.

    IF sy-subrc = 0.

      rv_exists = abap_true.

      "check if there is message: no condition exists
      READ TABLE lt_bdcmsgcoll WITH KEY msgid = 'VK' msgnr = '021' TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        rv_exists = abap_false.
      ENDIF.

    ELSE.
      rv_exists = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD create_pricing_condition.

    DATA lt_bdcdata               TYPE TABLE OF bdcdata.
    DATA lt_bdcmsgcoll            TYPE TABLE OF bdcmsgcoll.
    DATA lv_tcode TYPE tcode.
    DATA lv_fee TYPE kbetr.

    CLEAR rv_error.

    DATA(lv_keycombination) = get_key_combination( iv_condition_type = iv_condition_type ir_ent_pckg_fee = ir_ent_pckg_fee is_ent_org_data = is_ent_org_data is_ent_supplier = is_ent_supplier ).

    IF lv_keycombination IS INITIAL.
      rv_error = abap_true.

      "mark packaging fee entry as failed
      ir_ent_pckg_fee->set_failed( abap_true ).

      RETURN.
    ENDIF.

    lv_tcode = COND #( WHEN ir_ent_pckg_fee->get_business_process_direction( ) EQ mc_procdir_outbound THEN 'VK11' ELSE 'MEK1' ).
    lv_fee   = COND #( WHEN iv_use_additional_fee EQ abap_true THEN iv_fee_qty ELSE ir_ent_pckg_fee->get_totalfee( ) ).

    APPEND VALUE #( program  = 'SAPMV13A' dynpro   = '0100' dynbegin = 'X' ) TO lt_bdcdata.
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ANTA' ) TO lt_bdcdata.
    APPEND VALUE #( fnam = 'RV13A-KSCHL' fval = iv_condition_type ) TO lt_bdcdata.

    APPEND VALUE #( program  = 'SAPLV14A' dynpro   = '0100' dynbegin = 'X' ) TO lt_bdcdata.

    CASE lv_keycombination.
      WHEN mc_kc_sales_country.
        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(01)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(03)' fval = '' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro   = '1999' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-VKORG' fval = is_ent_org_data-salesorganization ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-VTWEG' fval = is_ent_org_data-distributionchannel ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-SPART' fval = is_ent_org_data-division ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-MATNR' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-LAND1(01)' fval = ir_ent_pckg_fee->get_reportcategorycountry( ) ) TO lt_bdcdata.

      WHEN mc_kc_sales_general.
        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(02)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro   = '1004' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-VKORG' fval = is_ent_org_data-salesorganization ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-VTWEG' fval = is_ent_org_data-distributionchannel ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-MATNR(01)' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.

      WHEN mc_kc_sales_material.
        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(03)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(03)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro   = '1005' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-VKORG' fval = is_ent_org_data-salesorganization ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-VTWEG' fval = is_ent_org_data-distributionchannel ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-MATNR(01)' fval = ir_ent_pckg_fee->get_productid( ) )  TO lt_bdcdata.

      WHEN mc_kc_sales_additional.

        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'SAPMV13A' dynpro = '1004' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-VKORG' fval = is_ent_org_data-salesorganization ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-VTWEG' fval = is_ent_org_data-distributionchannel ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-MATNR(01)' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.

      WHEN mc_kc_purch_material.
        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(01)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = '' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro   = '1018' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-LIFNR' fval = is_ent_supplier-supplier ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-EKORG' fval = determ_purchase_org( is_ent_org_data = is_ent_org_data ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-MATNR' fval = ir_ent_pckg_fee->get_productid( ) )  TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-ESOKZ(01)' fval = '0' ) TO lt_bdcdata.

        IF is_ent_supplier-is_excluded_from_report_config = abap_true.
          lv_fee = 0.
        ENDIF.

      WHEN mc_kc_purch_general.
        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(02)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro   = '1049' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-EKORG' fval = determ_purchase_org( is_ent_org_data = is_ent_org_data ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-ESOKZ' fval = '0' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-MATNR(01)' fval = ir_ent_pckg_fee->get_productid( ) )  TO lt_bdcdata.
      WHEN mc_kc_customer_independent.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'SAPMV13A' dynpro = '1820' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-BUKRS' fval = is_ent_org_data-companycode ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-MATNR(01)' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
      WHEN mc_kc_customer_dependent.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'SAPMV13A' dynpro = '1971' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-BUKRS' fval = is_ent_org_data-companycode ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-MATNR' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'KOMG-KUNNR(01)' fval = iv_customer ) TO lt_bdcdata.

    ENDCASE.

    APPEND VALUE #( fnam = 'RV13A-DATAB(01)' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
    APPEND VALUE #( fnam = 'RV13A-DATBI(01)' fval = date_to_external( ir_ent_pckg_fee->get_validto( ) ) ) TO lt_bdcdata.
    APPEND VALUE #( fnam = 'KONP-KBETR(01)' fval = convert_to_external( iv_data = lv_fee iv_max_decimals = 2 ) ) TO lt_bdcdata.
    APPEND VALUE #( fnam = 'KONP-KONWA(01)' fval = convert_to_external( ir_ent_pckg_fee->get_currency( ) ) ) TO lt_bdcdata.
    APPEND VALUE #( fnam = 'KONP-KPEIN(01)' fval = convert_to_external( iv_data = ir_ent_pckg_fee->get_referencequantity( ) iv_max_decimals = 0 ) ) TO lt_bdcdata.
    APPEND VALUE #( fnam = 'KONP-KMEIN(01)' fval = convert_to_external( iv_data = 'EA' ) ) TO lt_bdcdata.

    IF mv_test_mode = abap_true.
      APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '/EABR' ) TO lt_bdcdata.
    ELSE.
      APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=SICH' ) TO lt_bdcdata.
    ENDIF.

    DATA(lv_mode) = COND #( WHEN mv_test_mode = abap_true THEN 'E' ELSE 'N' ).

    CALL TRANSACTION lv_tcode
           USING lt_bdcdata
           MODE  lv_mode
           MESSAGES INTO lt_bdcmsgcoll.

    IF sy-subrc = 0.
      rv_error = abap_false.
    ELSE.
      rv_error = abap_true.

      "mark packaging fee entry as failed
      ir_ent_pckg_fee->set_failed( abap_true ).
      ir_ent_pckg_fee->set_messages( lt_bdcmsgcoll ).

    ENDIF.

  ENDMETHOD.

  METHOD update_pricing_condition.

    DATA lt_bdcdata               TYPE TABLE OF bdcdata.
    DATA lt_bdcmsgcoll            TYPE TABLE OF bdcmsgcoll.
    DATA lv_tcode TYPE tcode.
    DATA lv_fee TYPE kbetr.

    CLEAR rv_error.

    DATA(lv_keycombination) = get_key_combination( iv_condition_type = iv_condition_type ir_ent_pckg_fee = ir_ent_pckg_fee is_ent_org_data = is_ent_org_data is_ent_supplier = is_ent_supplier ).

    IF lv_keycombination IS INITIAL.
      rv_error = abap_true.

      "mark packaging fee entry as failed
      ir_ent_pckg_fee->set_failed( abap_true ).

      RETURN.
    ENDIF.

    lv_tcode = COND #( WHEN ir_ent_pckg_fee->get_business_process_direction( ) EQ mc_procdir_outbound THEN 'VK12' ELSE 'MEK2' ).
    lv_fee   = COND #( WHEN iv_use_additional_fee EQ abap_true THEN iv_fee_qty ELSE ir_ent_pckg_fee->get_totalfee( ) ).

    APPEND VALUE #( program  = 'SAPMV13A' dynpro   = '0100' dynbegin = 'X' ) TO lt_bdcdata.
    APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ANTA' ) TO lt_bdcdata.
    APPEND VALUE #( fnam = 'RV13A-KSCHL' fval = iv_condition_type ) TO lt_bdcdata.
    APPEND VALUE #( program  = 'SAPLV14A' dynpro   = '0100' dynbegin = 'X' ) TO lt_bdcdata.

    CASE lv_keycombination.
      WHEN mc_kc_sales_country.
        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(01)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(03)' fval = '' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'RV13A999' dynpro   = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_org_data-salesorganization ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = is_ent_org_data-distributionchannel ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F003' fval = is_ent_org_data-division ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F004' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F005-LOW' fval = ir_ent_pckg_fee->get_reportcategorycountry( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.
        APPEND VALUE #( program  = 'SAPMV13A' dynpro   = '1999' dynbegin = 'X' ) TO lt_bdcdata.

      WHEN mc_kc_sales_general.
        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(02)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'RV13A004' dynpro   = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_org_data-salesorganization ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = is_ent_org_data-distributionchannel ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F003-LOW' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro   = '1004' dynbegin = 'X' ) TO lt_bdcdata.

      WHEN mc_kc_sales_material.
        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(03)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(03)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'RV13A005' dynpro   = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_org_data-salesorganization ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = is_ent_org_data-distributionchannel ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F004-LOW' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro = '1005' dynbegin = 'X' ) TO lt_bdcdata.

      WHEN mc_kc_sales_additional.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'RV13A004' dynpro = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_org_data-salesorganization ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = is_ent_org_data-distributionchannel ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F003-LOW' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'SAPMV13A' dynpro = '1004' dynbegin = 'X' ) TO lt_bdcdata.

      WHEN mc_kc_purch_material.
        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(01)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = '' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'RV13A018' dynpro   = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_supplier-supplier )  TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = ir_ent_pckg_fee->get_productid( ) )  TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F003' fval = determ_purchase_org( is_ent_org_data = is_ent_org_data ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F004-LOW' fval = '0' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro   = '1018' dynbegin = 'X' ) TO lt_bdcdata.

        IF is_ent_supplier-is_excluded_from_report_config = abap_true.
          lv_fee = 0.
        ENDIF.

      WHEN mc_kc_purch_general.
        APPEND VALUE #( fnam = 'BDC_CURSOR' fval = 'RV130-SELKZ(02)' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = '' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(02)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'RV13A049' dynpro   = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = determ_purchase_org( is_ent_org_data = is_ent_org_data ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = '0' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F003-LOW' fval = ir_ent_pckg_fee->get_productid( ) )  TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program  = 'SAPMV13A' dynpro   = '1049' dynbegin = 'X' ) TO lt_bdcdata.

      WHEN mc_kc_customer_independent.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'RV13A820' dynpro = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_org_data-companycode ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002-LOW' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.
        APPEND VALUE #( program = 'SAPMV13A' dynpro = '1820' dynbegin = 'X' ) TO lt_bdcdata.
      WHEN mc_kc_customer_dependent.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = 'WEIT' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'RV130-SELKZ(01)' fval = 'X' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'RV13A971' dynpro = '1000' dynbegin = 'X' ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F001' fval = is_ent_org_data-companycode ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F002' fval = ir_ent_pckg_fee->get_productid( ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'F003-LOW' fval = iv_customer ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'SEL_DATE' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
        APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=ONLI' ) TO lt_bdcdata.

        APPEND VALUE #( program = 'SAPMV13A' dynpro = '1971' dynbegin = 'X' ) TO lt_bdcdata.

    ENDCASE.

    APPEND VALUE #( fnam = 'RV13A-DATAB(01)' fval = date_to_external( ir_ent_pckg_fee->get_validfrom( ) ) ) TO lt_bdcdata.
    APPEND VALUE #( fnam = 'RV13A-DATBI(01)' fval = date_to_external( ir_ent_pckg_fee->get_validto( ) ) ) TO lt_bdcdata.
    APPEND VALUE #( fnam = 'KONP-KBETR(01)' fval = convert_to_external( iv_data = lv_fee iv_max_decimals = 2 ) ) TO lt_bdcdata.

    IF mv_test_mode = abap_true.
      APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '/EABR' ) TO lt_bdcdata.
    ELSE.
      APPEND VALUE #( fnam = 'BDC_OKCODE' fval = '=SICH' ) TO lt_bdcdata.
    ENDIF.

    DATA(lv_mode) = COND #( WHEN mv_test_mode = abap_true THEN 'E' ELSE 'N' ).

    CALL TRANSACTION lv_tcode
           USING lt_bdcdata
           MODE   lv_mode
           MESSAGES INTO lt_bdcmsgcoll.

    IF sy-subrc = 0.
      rv_error = abap_false.
    ELSE.
      rv_error = abap_true.

      "mark packaging fee entry as failed
      ir_ent_pckg_fee->set_failed( abap_true ).
      ir_ent_pckg_fee->set_messages( lt_bdcmsgcoll ).

    ENDIF.

  ENDMETHOD.

  METHOD update_mclass_es_plastictax.

    DATA: lt_characteristics  TYPE /vpcoe/if_pckf_matclass_dac=>gtyt_characteristic_value,
          lv_changenum        TYPE aennr,
          lv_changedesc       TYPE aetxt,
          lv_changenum_exists TYPE abap_bool,
          lv_changenum_valid  TYPE abap_bool,
          lv_error            TYPE abap_bool,
          lv_ecn_date         TYPE c LENGTH 6,
          lt_messages         TYPE /vpcoe/t_uph_msg.

    CLEAR rv_error.

    DATA(ls_extension) = ir_ent_pckg_fee->get_extension( ).
    DATA(lv_reference_quantity) = ir_ent_pckg_fee->get_referencequantity( ).

    CHECK ls_extension IS NOT INITIAL.

    IF lv_reference_quantity <> 0.

      APPEND INITIAL LINE TO lt_characteristics REFERENCE INTO DATA(lr_characteristics).
      lr_characteristics->characteristic = 'ZRDP_ES_PLASTIC'.
      lr_characteristics->value = ls_extension-plasticweightinkg / lv_reference_quantity.
      lr_characteristics->data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-num.

      APPEND INITIAL LINE TO lt_characteristics REFERENCE INTO lr_characteristics.
      lr_characteristics->characteristic = 'ZRDP_ES_NONRECYCLED_PLASTIC'.
      lr_characteristics->value = ls_extension-nonrecycledplasticinkg / lv_reference_quantity.
      lr_characteristics->data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-num.
    ENDIF.

    LOOP AT ls_extension-exemptions INTO DATA(ls_exemption).
      APPEND INITIAL LINE TO lt_characteristics REFERENCE INTO lr_characteristics.
      lr_characteristics->characteristic = 'ZRDP_ES_EXEMPTION'.
      lr_characteristics->value = ls_exemption-code.
      lr_characteristics->data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-char.
    ENDLOOP.

    APPEND INITIAL LINE TO lt_characteristics REFERENCE INTO lr_characteristics.
    lr_characteristics->characteristic = 'ZRDP_ES_PRODORDER_CHECK'.
    IF ls_extension-productionordercheck = abap_true.
      lr_characteristics->value = 'Y'.
    ELSEIF ls_extension-productionordercheck = abap_undefined.
      lr_characteristics->value = ''.
    ELSE.
      lr_characteristics->value = 'N'.
    ENDIF.
    lr_characteristics->data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-char.

    APPEND INITIAL LINE TO lt_characteristics REFERENCE INTO lr_characteristics.
    lr_characteristics->characteristic = 'ZRDP_ES_PACKMAT_TAX'.
    lr_characteristics->value = ls_extension-taxforpackagingasmaterial.
    lr_characteristics->data_type = /vpcoe/if_pckf_matclass_dac=>cs_characteristic_type-num.

    DATA(lv_valid_from) = ir_ent_pckg_fee->get_validfrom( ).

    WRITE lv_valid_from TO lv_ecn_date YYMMDD.
    lv_changedesc = |ESPTX_{ lv_ecn_date }|.

    mo_matclass_dac->check_change_number(
      EXPORTING
        iv_description = lv_changedesc
      IMPORTING
        ev_changenum   = lv_changenum
        ev_exists      = lv_changenum_exists
        ev_valid       = lv_changenum_valid
    ).

    IF mv_test_mode = abap_true.
      RETURN.
    ENDIF.

    IF lv_changenum_exists = abap_false.

      mo_matclass_dac->create_change_number(
        EXPORTING
          iv_valid_from  = lv_valid_from
          iv_description = lv_changedesc
          iv_commit      = abap_true
        IMPORTING
          ev_changenum   = lv_changenum
          ev_failed      = lv_error
          et_messages    = lt_messages
      ).

      IF lv_error = abap_true.

        mo_logger->add_messages( lt_messages ).

        "mark packaging fee entry as failed
        ir_ent_pckg_fee->set_failed( abap_true ).

        rv_error = abap_true.
        RETURN.
      ENDIF.

    ELSEIF lv_changenum_valid = abap_false.
      rv_error = abap_true.
      RETURN.
    ENDIF.

    mo_matclass_dac->update_classification(
      EXPORTING
        iv_material        = ir_ent_pckg_fee->get_productid( )
        iv_changenum       = lv_changenum
        iv_class           = 'ZRDP_ES_PLASTICTAX'
        it_characteristics = lt_characteristics
        iv_commit          = abap_true
      IMPORTING
        ev_failed          = lv_error
        et_messages        = lt_messages
    ).

    IF lv_error = abap_true.

      mo_logger->add_messages( lt_messages ).

      "mark packaging fee entry as failed
      ir_ent_pckg_fee->set_failed( abap_true ).

      rv_error = abap_true.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD get_key_combination.

    CLEAR rv_key_combination.

    IF ir_ent_pckg_fee->get_productid( ) IS INITIAL.
      RETURN.
    ENDIF.

    IF ir_ent_pckg_fee->get_validfrom( ) IS INITIAL OR ir_ent_pckg_fee->get_validto( ) IS INITIAL.
      RETURN.
    ENDIF.

    CASE ir_ent_pckg_fee->get_business_process_direction( ).
      WHEN mc_procdir_outbound.

        IF is_ent_org_data IS NOT INITIAL.
          IF iv_condition_type = mc_condition_type_zest OR iv_condition_type = mc_condition_type_zesp.
            rv_key_combination = mc_kc_sales_additional.
            RETURN.
          ENDIF.
          IF iv_condition_type = mc_condition_type_zcon.
            rv_key_combination = mc_kc_customer_independent.
            RETURN.
          ENDIF.
          IF iv_condition_type = mc_condition_type_zese.
            rv_key_combination = mc_kc_customer_dependent.
            RETURN.
          ENDIF.
          IF ir_ent_pckg_fee->get_reportcategorycountry( ) IS NOT INITIAL
              AND is_ent_org_data-distributionchannel IS NOT INITIAL
              AND is_ent_org_data-division IS NOT INITIAL.

            rv_key_combination = mc_kc_sales_country.

          ELSEIF is_ent_org_data-distributionchannel IS NOT INITIAL.

            rv_key_combination = mc_kc_sales_general.

          ENDIF.

        ENDIF.

      WHEN mc_procdir_inbound.

        IF iv_condition_type = mc_condition_type_zcon or iv_condition_type = mc_condition_type_zese.
          rv_key_combination = mc_kc_customer_independent.
          RETURN.
        ENDIF.

        IF is_ent_supplier IS NOT INITIAL.
          rv_key_combination = mc_kc_purch_material.
        ELSE.
          rv_key_combination = mc_kc_purch_general.
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD date_to_external.
    CLEAR rv_result.

    IF iv_date IS NOT INITIAL.

      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
          date_internal = iv_date
        IMPORTING
          date_external = rv_result.

    ENDIF.

  ENDMETHOD.

  METHOD convert_to_external.

    CLEAR rv_result.
    IF iv_data IS NOT INITIAL.
      IF iv_max_decimals IS NOT SUPPLIED.
        WRITE iv_data TO rv_result LEFT-JUSTIFIED NO-GROUPING.
      ELSE.
        WRITE iv_data TO rv_result LEFT-JUSTIFIED NO-GROUPING DECIMALS iv_max_decimals.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD determ_purchase_org.
* Determine purchasing organziation by given company code and lookup into assignment table
    CLEAR rv_result.

    CHECK is_ent_org_data-companycode IS NOT INITIAL.

    SELECT ekorg FROM t024e WHERE bukrs = @is_ent_org_data-companycode INTO TABLE @DATA(lt_t024e).
    IF sy-subrc = 0.
      IF lt_t024e IS NOT INITIAL.
        "return the first assigned purchasing organization
        rv_result = lt_t024e[ 1 ].
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.""",
    r"""CLASS zcl_gen_rfc_tier2_proxy DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA function_modules TYPE cl_aco_metadata_provider=>t_functions READ-ONLY.
    DATA function_module TYPE cl_aco_metadata_provider=>t_function READ-ONLY.

    DATA wrapper_class_name TYPE sxco_class_name  READ-ONLY.
    DATA wrapper_interface_name TYPE sxco_class_name  READ-ONLY.
    DATA wrapper_factory_class_name TYPE sxco_class_name  READ-ONLY.

    DATA package_name TYPE sxco_package READ-ONLY.

    DATA namespace TYPE string READ-ONLY.

    DATA transport_request TYPE sxco_transport READ-ONLY.

    DATA wrapper_interface_code TYPE rswsourcet READ-ONLY.
    DATA wrapper_class_code TYPE rswsourcet READ-ONLY.
    DATA wrapper_factory_class_code TYPE rswsourcet READ-ONLY.

    DATA methods_code_definition TYPE rswsourcet READ-ONLY .
    DATA methods_code_implementation TYPE rswsourcet READ-ONLY.

    INTERFACES if_oo_adt_classrun .


    METHODS constructor
      IMPORTING
                i_package_name                 TYPE sxco_package
                i_transport_request            TYPE sxco_transport OPTIONAL
                i_generate_intf_and_fact_class TYPE abap_bool DEFAULT abap_true
                i_wrapper_class_name           TYPE sxco_class_name
                i_wrapper_interface_name       TYPE sxco_class_name  OPTIONAL
                i_wrapper_factory_class_name   TYPE sxco_class_name  OPTIONAL
                i_function_modules             TYPE cl_aco_metadata_provider=>t_functions
                i_overwrite_objects            TYPE abap_bool DEFAULT abap_true
      RAISING   cx_abap_invalid_value.


    METHODS read_aco_proxy_cls_src_code
*      IMPORTING remove_class_method_statements  TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(aco_proxy_class_src_code) TYPE rswsourcet.

    METHODS get_wrapper_class_code
      IMPORTING I_aco_proxy_class_src_code  TYPE rswsourcet
      RETURNING VALUE(r_wrapper_class_code) TYPE rswsourcet..

    METHODS get_wrapper_interface_code
      IMPORTING I_aco_proxy_class_src_code      TYPE rswsourcet
      RETURNING VALUE(r_wrapper_interface_code) TYPE rswsourcet.

    METHODS get_private_methods_code
      IMPORTING I_aco_proxy_class_src_code    TYPE rswsourcet
      EXPORTING
                r_methods_definition_code     TYPE rswsourcet
                r_methods_implementation_code TYPE rswsourcet.

    METHODS get_wrapper_factory_class_code
      RETURNING VALUE(r_wrapper__factory_class_code) TYPE rswsourcet..

    METHODS generate_wrapper_objects
      IMPORTING i_demo_mode             TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(r_exception_text) TYPE string.

    METHODS generate_wrapper_interface.

    METHODS generate_factory_class.

    METHODS update_wrapper_objects_code
      IMPORTING
        i_object_type TYPE trobjtype
        i_object_name TYPE trobj_name
        i_source_code TYPE rswsourcet.


    METHODS generate_aco_proxy_class
      IMPORTING
                i_function_modules  TYPE cl_aco_metadata_provider=>t_functions
                I_proxy_class_name  TYPE sxco_class_name
                i_package_name      TYPE sxco_package
                i_transport_request TYPE sxco_transport
      RETURNING VALUE(success)      TYPE abap_bool
      RAISING   cx_aco_exception.


    METHODS get_namespace
      IMPORTING i_package_name     TYPE string
      RETURNING VALUE(r_namespace) TYPE string.


    METHODS get_unique_object_name
      IMPORTING i_short_object_name  TYPE sxco_class_name
*                i_namespace         TYPE string
                i_object_type        TYPE trobjtype
      RETURNING VALUE(r_object_name) TYPE sxco_class_name.


    METHODS release_class_and_interface
      RAISING cx_abap_api_state.

    METHODS create_transport RETURNING VALUE(r_transport) TYPE sxco_transport.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA generate_intf_and_fact_class TYPE abap_bool.
    DATA overwrite_objects TYPE abap_bool.


ENDCLASS.""",
    r"""CLASS zcl_gen_rfc_tier2_proxy IMPLEMENTATION.

  METHOD constructor.

    IF NOT xco_abap_repository=>object->devc->for(  i_package_name  )->exists( ).
      RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | package { i_package_name } does not exist |.
    ENDIF.

    package_name  = i_package_name .
    namespace = get_namespace( CONV #( package_name ) ).

    IF i_transport_request IS INITIAL.
      "create transport checks if the selected package records changes
      transport_request = create_transport( ) .
    ELSE.
      transport_request = i_transport_request .
    ENDIF.

    generate_intf_and_fact_class = i_generate_intf_and_fact_class.

    overwrite_objects = i_overwrite_objects.

    IF i_overwrite_objects = abap_false.
      IF i_generate_intf_and_fact_class = abap_true.
        IF xco_abap_repository=>object->clas->for(  i_wrapper_class_name  )->exists( ).
          RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | Class { i_wrapper_class_name } does already exist |.
        ELSEIF xco_abap_repository=>object->intf->for(  i_wrapper_interface_name  )->exists( ).
          RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | Interface { i_wrapper_interface_name } does already exist |.
        ELSEIF xco_abap_repository=>object->clas->for(  i_wrapper_factory_class_name  )->exists( ).
          RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | Class { i_wrapper_factory_class_name } does already exist |.
        ENDIF.
      ELSE.
        IF xco_abap_repository=>object->clas->for(  i_wrapper_class_name  )->exists( ).
          RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | Class { i_wrapper_class_name } does already exist |.
        ENDIF.
      ENDIF.
    ENDIF.

    wrapper_class_name = i_wrapper_class_name     .
    wrapper_interface_name = i_wrapper_interface_name   .
    wrapper_factory_class_name = i_wrapper_factory_class_name  .

    LOOP AT i_function_modules INTO DATA(function_module).

      SELECT SINGLE * FROM tfdir INTO @DATA(function_module_info) WHERE funcname = @function_module-functionname.

      IF function_module_info IS INITIAL.
        RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | Function module { function_module-functionname } does not exist.|.
      ENDIF.

      IF function_module_info-fmode IS NOT INITIAL.
        APPEND function_module TO function_modules.
      ELSE.
        RAISE EXCEPTION TYPE cx_abap_invalid_value EXPORTING value = | Function module { function_module-functionname } is not remote enabled.|.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD generate_wrapper_objects.
    TRY.
        generate_aco_proxy_class(
          i_function_modules  = function_modules
          i_proxy_class_name  = wrapper_class_name
          i_package_name      = package_name
          i_transport_request = transport_request
        ).
      CATCH cx_aco_exception INTO DATA(aco_exception).
        r_exception_text = |ACO error: { aco_exception->get_text(  ) }|.
        RETURN.
    ENDTRY.

    read_aco_proxy_cls_src_code(
*      EXPORTING
*        remove_class_method_statements = abap_true
      RECEIVING
        aco_proxy_class_src_code       = DATA(aco_proxy_class_code)
    ).

    IF aco_proxy_class_code IS INITIAL.
      r_exception_text = |No source code found: { wrapper_class_name }|.
      RETURN.
    ENDIF.


    IF generate_intf_and_fact_class = abap_true.

      get_wrapper_interface_code(
        EXPORTING
          i_aco_proxy_class_src_code = aco_proxy_class_code
        RECEIVING
          r_wrapper_interface_code   = wrapper_interface_code
      ).

      get_private_methods_code(
        EXPORTING
          i_aco_proxy_class_src_code = aco_proxy_class_code
        IMPORTING
          r_methods_definition_code  = methods_code_definition
          r_methods_implementation_code = methods_code_implementation
      ).

      get_wrapper_factory_class_code(
        RECEIVING
          r_wrapper__factory_class_code = wrapper_factory_class_code
      ).

    ENDIF.

    get_wrapper_class_code(
      EXPORTING
        i_aco_proxy_class_src_code = aco_proxy_class_code
      RECEIVING
        r_wrapper_class_code       = wrapper_class_code
    ).

    IF i_demo_mode = abap_false.

      TRY.

          IF generate_intf_and_fact_class = abap_true.

            IF xco_abap_repository=>object->intf->for(  wrapper_interface_name  )->exists( ) = abap_False.
              generate_wrapper_interface( ).
            ELSE.
              ASSERT overwrite_objects = abap_true.
            ENDIF.

            IF xco_abap_repository=>object->clas->for(  wrapper_factory_class_name  )->exists( ) = abap_False.
              generate_factory_class( ).
            ELSE.
              ASSERT overwrite_objects = abap_true.
            ENDIF.

            update_wrapper_objects_code(
              i_object_type = 'INTF'
              i_object_name = CONV #( wrapper_interface_name )
              i_source_code = wrapper_interface_code
            ).

          ENDIF.

          update_wrapper_objects_code(
            i_object_type = 'CLAS'
            i_object_name = CONV #( wrapper_class_name )
            i_source_code = wrapper_class_code
          ).

          IF generate_intf_and_fact_class = abap_true.

            update_wrapper_objects_code(
              i_object_type = 'CLAS'
              i_object_name = CONV #( wrapper_factory_class_name )
              i_source_code = wrapper_factory_class_code
            ).

          ENDIF.

        CATCH cx_oo_class_scan_error  INTO DATA(update_wrapper_code_exc).
          r_exception_text = |cl_oo_factory error: { update_wrapper_code_exc->get_text(  ) }|.
          RETURN.
      ENDTRY.

      TRY.
          release_class_and_interface(  ).
        CATCH  cx_abap_api_state   INTO DATA(api_state_exception).
          r_exception_text = |api_state error: { api_state_exception->get_text(  ) }|.
          RETURN.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    package_name     = 'TEST_AF_GENERATED_OBJECTS_001'.
    transport_request = ''.

    DATA number TYPE i VALUE 106.

    DATA(project_name) = 'af_wrapper_t'.

    function_module-functionname = to_upper( 'bapi_epm_product_get_detail' ).
    APPEND function_module TO function_modules.
    function_module-functionname = to_upper( 'bapi_epm_product_get_list' ).
    APPEND function_module TO function_modules.

    wrapper_class_name = to_upper( namespace && 'cl_wrap_'  && project_name ).
    wrapper_interface_name = to_upper( namespace && 'if_wrap_'  && project_name ).
    wrapper_factory_class_name = to_upper( namespace && 'cl_fact_'  && project_name ) .


    wrapper_class_name = get_unique_object_name(
                           i_short_object_name = wrapper_class_name
                           i_object_type       = 'CLAS'
                         ).

    wrapper_interface_name = get_unique_object_name(
                               i_short_object_name = wrapper_interface_name
                               i_object_type       = 'INTF'
                             )                    .
    wrapper_factory_class_name = get_unique_object_name(
                     i_short_object_name = wrapper_factory_class_name
                     i_object_type       = 'CLAS'
                   ).

    out->write( wrapper_class_name ).
    out->write( wrapper_interface_name ).
    out->write( wrapper_factory_class_name ).

    out->write( 'finished' ).

  ENDMETHOD.

  METHOD generate_aco_proxy_class.

    cl_aco_static_proxy=>create_static_proxy_by_rfc(
               EXPORTING
                 function_names         = i_function_modules
                 proxy_name             = i_proxy_class_name
                 destination_name       = 'NONE'
                 devclass               = i_package_name
                 trkorr                 = i_transport_request
                 classic_exceptions     = abap_false
                 bapi_exceptions        = abap_false
                 generate_inactive      = abap_false
                 destination_by_constructor = abap_false
                 do_not_create_released_type = abap_true
             ).
    success = abap_true.

  ENDMETHOD.

  METHOD read_aco_proxy_cls_src_code.

    FIELD-SYMBOLS <source_code_line> TYPE string.

    SELECT SINGLE  * FROM i_abapobjectdirectoryentry INTO @DATA(obj_entry)
                               WHERE abapobject = @wrapper_class_name
                               .

    DATA(package) = obj_entry-abappackage.

    IF package IS INITIAL.
      EXIT.
    ENDIF.

    "read source code of generated proxy class
    DATA(ref_proxy_class_name) = cl_oo_factory=>create_instance( )->create_clif_source( to_upper( wrapper_class_name ) ).
    ref_proxy_class_name->get_source( IMPORTING source = aco_proxy_class_src_code ).

  ENDMETHOD.

  METHOD get_wrapper_factory_class_code.


    APPEND |CLASS { wrapper_factory_class_name } DEFINITION | TO r_wrapper__factory_class_code.
    APPEND |PUBLIC | TO r_wrapper__factory_class_code.
    APPEND |FINAL | TO r_wrapper__factory_class_code.
    APPEND |CREATE PRIVATE . | TO r_wrapper__factory_class_code.

    APPEND |   PUBLIC SECTION. | TO r_wrapper__factory_class_code.


    APPEND |     CLASS-METHODS create_instance | TO r_wrapper__factory_class_code.
    APPEND |       RETURNING VALUE(result) TYPE REF TO { wrapper_interface_name }.    |            TO r_wrapper__factory_class_code.
    APPEND |   PROTECTED SECTION. |   TO r_wrapper__factory_class_code.
    APPEND |   PRIVATE SECTION. |  TO r_wrapper__factory_class_code.
    APPEND |     METHODS constructor. |  TO r_wrapper__factory_class_code.
    APPEND |ENDCLASS. |  TO r_wrapper__factory_class_code.

    APPEND | CLASS { wrapper_factory_class_name } IMPLEMENTATION.  | TO r_wrapper__factory_class_code.

    APPEND |  METHOD create_instance. | TO r_wrapper__factory_class_code.

    APPEND |   result = NEW { wrapper_class_name }(  ). | TO r_wrapper__factory_class_code.
    APPEND |  ENDMETHOD.| TO r_wrapper__factory_class_code.

    APPEND |  METHOD constructor. | TO r_wrapper__factory_class_code.
    APPEND |  ENDMETHOD. | TO r_wrapper__factory_class_code.

    APPEND | ENDCLASS. | TO r_wrapper__factory_class_code.


  ENDMETHOD.

  METHOD get_wrapper_interface_code.

    DATA add_code TYPE abap_bool.
    DATA source_code_line LIKE LINE OF i_aco_proxy_class_src_code.

    APPEND |INTERFACE { wrapper_interface_name }| TO r_wrapper_interface_code.
    APPEND | PUBLIC . | TO r_wrapper_interface_code.

    LOOP AT i_aco_proxy_class_src_code  INTO source_code_line.

      DATA(result_last_statement)  = find( val = source_code_line sub  = |PROTECTED SECTION.| case = abap_false ).
      DATA(result_first_statement) = find( val = source_code_line sub  = |INTERFACES if_aco_proxy| case = abap_false ).
      DATA(result_class_methods)   = find( val = source_code_line sub  = |CLASS-METHODS| case = abap_false ).
      DATA(result_if_aco_proxy)   = find( val = source_code_line sub  = |if_aco_proxy| case = abap_false ).

      IF result_class_methods <> -1.
        source_code_line = replace( val = source_code_line
                                    sub = |CLASS-METHODS|
                                    with = |METHODS| ).
      ENDIF.


      IF result_first_statement <> -1.
        add_code = abap_true.
      ENDIF.

      IF result_last_statement <> -1.
        APPEND |ENDINTERFACE.| TO r_wrapper_interface_code.
        EXIT.
      ENDIF.

      "skip if_aco_proxy statement
      IF add_code = abap_true AND result_if_aco_proxy = -1.
        APPEND source_code_line TO r_wrapper_interface_code.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_wrapper_class_code.

    DATA add_code TYPE abap_bool.
    DATA source_code_line LIKE LINE OF i_aco_proxy_class_src_code.

    IF generate_intf_and_fact_class = abap_false.

      LOOP AT i_aco_proxy_class_src_code  INTO source_code_line.

        " add a final statement before the CREATE PUBLIC statement.
        DATA(result_create_public_statement) = find( val = source_code_line sub  = |CREATE PUBLIC| case = abap_false ).
        DATA(result_if_aco_proxy)   = find( val = source_code_line sub  = |if_aco_proxy| case = abap_false ).

        IF result_create_public_statement <> -1.
          APPEND 'FINAL' TO r_wrapper_class_code.
          source_code_line =   replace( val = source_code_line
                                        sub =  |CREATE PUBLIC|
                                        with = |CREATE PRIVATE| ).
        ENDIF.

        "remove DESTINATION _dest_ statements
        DATA(result_destination_statement) = find( val = source_code_line sub  = |DESTINATION _dest_| case = abap_false ).
*        source_code_line = to_upper( source_code_line ).
        IF result_destination_statement <> -1.
          source_code_line =   replace( val = source_code_line
                                                 sub = |DESTINATION _dest_|
                                                 with = |DESTINATION space| ).
        ENDIF.
        IF result_if_aco_proxy = -1.
          APPEND source_code_line TO r_wrapper_class_code.
        ENDIF.
      ENDLOOP.

      RETURN.

    ENDIF.

    CLEAR r_wrapper_class_code.

    APPEND |CLASS { wrapper_class_name } DEFINITION| TO r_wrapper_class_code.
    APPEND |PUBLIC  | TO r_wrapper_class_code.
    APPEND |FINAL  | TO r_wrapper_class_code.
    APPEND |CREATE PUBLIC . | TO r_wrapper_class_code.
    APPEND |PUBLIC SECTION. | TO r_wrapper_class_code.
    APPEND |INTERFACES { wrapper_interface_name }.| TO r_wrapper_class_code.
    APPEND |PROTECTED SECTION. | TO r_wrapper_class_code.
    APPEND |PRIVATE SECTION. | TO r_wrapper_class_code.

    "add private methods code

    LOOP AT methods_code_definition INTO DATA(method_code_line).
      APPEND method_code_line TO r_wrapper_class_code.
    ENDLOOP.
    APPEND |.| TO r_wrapper_class_code.
    APPEND |ENDCLASS.| TO r_wrapper_class_code.
    APPEND | | TO r_wrapper_class_code.

    LOOP AT i_aco_proxy_class_src_code  INTO source_code_line.

      DATA(result_method_statement) = find( val = source_code_line sub  = |METHOD| case = abap_false ).
      DATA(result_first_statement) = find( val = source_code_line sub  = |CLASS { wrapper_class_name } IMPLEMENTATION.| case = abap_false ).

      "remove DESTINATION _dest_
      result_destination_statement = find( val = source_code_line sub  = |DESTINATION _dest_| case = abap_false ).

      IF result_first_statement <> -1.
        add_code = abap_true.
      ENDIF.

      "add interface name to method name
*      IF result_method_statement <> -1 .
*        source_code_line = to_upper( source_code_line ).
*        source_code_line =   replace( val = source_code_line
*                                      sub = |METHOD |
*                                      with = |METHOD { wrapper_interface_name }~| ).
*      ENDIF.

      IF result_destination_statement <> -1.
        source_code_line =   replace( val = source_code_line
                                               sub = |DESTINATION _dest_|
                                               with = |DESTINATION space| ).
      ENDIF.

      IF add_code = abap_true.
        APPEND source_code_line TO r_wrapper_class_code.
        IF result_first_statement <> -1.
          LOOP AT methods_code_implementation INTO DATA(methods_code_impl_line).
            APPEND methods_code_impl_line TO r_wrapper_class_code.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.

  METHOD update_wrapper_objects_code.

    DATA(ref) = cl_oo_factory=>create_instance( )->create_clif_source( to_upper(  i_object_name  ) ).

    TRY.
        ref->lock( ).
        ref->set_source( source = i_source_code ).
        ref->save( ).
        ref->unlock( ).
      CATCH  cx_oo_access_permission cx_oo_class_scan_error INTO DATA(access_permission_exc).
*    WRITE : / |error occured: { access_permission_exc->get_text(  ) }|.
*        EXIT.
    ENDTRY.

*    DATA objects TYPE STANDARD TABLE OF dwinactiv .
*
*    objects =  VALUE #( ( object =  i_object_type  obj_name = i_object_name uname = sy-uname ) ).
*
*    CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
*      TABLES
*        objects                = objects
*      EXCEPTIONS
*        excecution_error       = 1
*        cancelled              = 2
*        insert_into_corr_error = 3
*        OTHERS                 = 4.

    IF sy-subrc <> 0.
*  WRITE : / |error occured when activating class { cls_name }. SY-SUBRC = { sy-subrc } |.
*      EXIT.
    ENDIF.
  ENDMETHOD.



  METHOD generate_wrapper_interface.

    DATA  lo_put_operation TYPE REF TO if_xco_gen_intf_o_put  .

    IF transport_request IS INITIAL.
      lo_put_operation = xco_generation=>environment->local->for-intf->create_put_operation( ).
    ELSE.
      lo_put_operation = xco_generation=>environment->transported( transport_request )->for-intf->create_put_operation(  ).
    ENDIF.

    DATA(lo_form_specification) = lo_put_operation->add_object( wrapper_interface_name
      )->set_package( package_name
      )->create_form_specification( ).

    lo_form_specification->set_short_description( 'Sample interface' ) ##NO_TEXT.


    lo_put_operation->execute(  ).

  ENDMETHOD.

  METHOD generate_factory_class.

    DATA  lo_put_operation TYPE REF TO if_xco_gen_clas_o_put  .

    IF transport_request IS INITIAL.
      lo_put_operation = xco_generation=>environment->local->for-clas->create_put_operation( ).
    ELSE.
      lo_put_operation = xco_generation=>environment->transported( transport_request )->for-clas->create_put_operation(  ).
    ENDIF.

    DATA(lo_form_specification) = lo_put_operation->add_object( wrapper_factory_class_name
      )->set_package( package_name
      )->create_form_specification( ).

    lo_form_specification->set_short_description( 'Factory class' ) ##NO_TEXT.


    lo_put_operation->execute(  ).
  ENDMETHOD.

  METHOD get_namespace.

    IF i_package_name = '$TMP'.
      r_namespace = 'Z'.
      EXIT.
    ENDIF.

    FIND ALL OCCURRENCES OF '/'
       IN i_package_name
       IGNORING CASE             " case insensitive
       RESULTS DATA(result). " TYPE match_result_tab

    IF lines( result ) = 2.
      CHECK result[ 1 ]-offset = 0.
      r_namespace = substring( val = i_package_name  len = result[ 2 ]-offset + 1 ).
      EXIT.
    ENDIF.

    DATA(first_character_package) = substring( val = i_package_name  off = 0 len = 1 ).
    DATA(package_name_length) = strlen( i_package_name ).

    IF first_character_package = 'Y'.
      r_namespace = 'Y'.
      EXIT.
    ENDIF.

    IF first_character_package =  'Z'.
      r_namespace = 'Z'.
      EXIT.
    ENDIF.

    IF package_name_length > strlen( 'TEST_' ).
      IF substring( val = i_package_name  len = strlen( 'TEST_' )   ) = 'TEST_' .
        r_namespace = 'Z'.
        EXIT.
      ENDIF.
    ENDIF.

    r_namespace = ''.


  ENDMETHOD.

  METHOD get_unique_object_name.

    DATA is_valid_repo_object_name TYPE abap_bool VALUE abap_false.
    DATA unique_number TYPE i VALUE 0.
    DATA unique_hex_number TYPE xstring .
    DATA unique_hex_number_string TYPE c LENGTH 2.

    "Generate a short class name that provides us the option to add two characters to suggest a unique name
*    DATA short_class_name TYPE c LENGTH 28.

*    short_class_name = i_namespace && 'CL_WRAP_' && i_fugr_name.

    WHILE is_valid_repo_object_name = abap_false AND unique_number < 255 .

      unique_hex_number = CONV xstring( unique_number ).

      IF unique_hex_number = 00.
        unique_hex_number_string = ''.
      ELSE.
        unique_hex_number_string = unique_hex_number.
      ENDIF.

      r_object_name = i_short_object_name && unique_hex_number_string.

      IF  i_object_type   = 'CLAS'.
        IF NOT xco_abap_repository=>object->clas->for( r_object_name )->exists( ).
          is_valid_repo_object_name = abap_true.
        ELSE.
          unique_number += 1.
        ENDIF.
      ELSEIF i_object_type   = 'INTF'.
        IF NOT xco_abap_repository=>object->intf->for( r_object_name )->exists( ).
          is_valid_repo_object_name = abap_true.
        ELSE.
          unique_number += 1.
        ENDIF.
      ELSE.
        ASSERT 1 = 0.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.




  METHOD release_class_and_interface.

    IF generate_intf_and_fact_class = abap_true.

      DATA(api_state_wrapper_fact_class) = cl_abap_api_state=>create_instance(
                api_key = VALUE #(
                object_type     = 'CLAS'
                object_name     = to_upper( wrapper_factory_class_name )
                ) ).

      api_state_wrapper_fact_class->release(
        EXPORTING
          release_contract         = 'C1'
          use_in_cloud_development = abap_true
          use_in_key_user_apps     = abap_false
          request                  = transport_request
      ).


      DATA(api_state_wrapper_interface) = cl_abap_api_state=>create_instance(
                api_key = VALUE #(
                object_type     = 'INTF'
                object_name     = to_upper( wrapper_interface_name )
                ) ).

      api_state_wrapper_interface->release(
        EXPORTING
          release_contract         = 'C1'
          use_in_cloud_development = abap_true
          use_in_key_user_apps     = abap_false
          request                  = transport_request
      ).

    ELSE.
      DATA(api_state_wrapper_class) = cl_abap_api_state=>create_instance(
                api_key = VALUE #(
                object_type     = 'CLAS'
                object_name     = to_upper( wrapper_class_name )
                ) ).

      api_state_wrapper_class->release(
        EXPORTING
          release_contract         = 'C1'
          use_in_cloud_development = abap_true
          use_in_key_user_apps     = abap_false
          request                  = transport_request
      ).
    ENDIF.

  ENDMETHOD.

  METHOD create_transport.
    DATA(xco_package) = xco_abap_repository=>object->devc->for(  package_name  ).
    DATA(record_object_changes) = xco_package->read( )-property-record_object_changes.
    IF record_object_changes = abap_true.
      DATA(lo_transport_target) = xco_package->read( )-property-transport_layer->get_transport_target( ).
      DATA(new_transport_object) = xco_cp_cts=>transports->workbench( lo_transport_target->value  )->create_request( |Wrapper class: { wrapper_class_name } | ).
      r_transport = new_transport_object->value.
    ELSE.
      r_transport = ''.
    ENDIF.
  ENDMETHOD.

  METHOD get_private_methods_code.
    DATA add_code TYPE abap_bool.
    DATA result_in_interface TYPE abap_bool.

    DATA source_code_line LIKE LINE OF i_aco_proxy_class_src_code.

*    APPEND |INTERFACE { wrapper_interface_name }| TO r_methods_definition_code.
*    APPEND | PUBLIC . | TO r_wrapper_interface_code.

    LOOP AT i_aco_proxy_class_src_code  INTO source_code_line.

      DATA(result_last_statement)  = find( val = source_code_line sub  = |.| case = abap_false ).
      DATA(result_first_statement) = find( val = source_code_line sub  = |CLASS-METHODS| case = abap_false ).
      DATA(result_type_statement)   = find( val = source_code_line sub  = |TYPE | case = abap_false ).
      DATA(result_endclass_statement) = find( val = source_code_line sub  = |ENDCLASS.| case = abap_false ).

      DATA(result_class_methods)   = find( val = source_code_line sub  = |CLASS-METHODS| case = abap_false ).

      DATA(result_destination_statement) = find( val = source_code_line sub  = |_dest_| case = abap_false ).

      DATA(result_none_statement) = find( val = source_code_line sub  = |'NONE'| case = abap_false ).

      " !prheader               TYPE ZIF_WRAP_TEST_4714~bapimereqheader OPTIONAL

      DATA(result_exclamation_mark) = find( val = source_code_line sub  = |!| case = abap_false ).


      IF result_class_methods <> -1.
        source_code_line = replace( val = source_code_line
                                    sub = |CLASS-METHODS|
                                    with = |METHODS| ).
        DATA(source_code_line_impl) = replace( val = source_code_line
                                    sub = |METHODS |
                                    with = |METHOD { wrapper_interface_name }~| ).
        DATA(method_name) = replace( val = source_code_line
                                    sub = |METHODS |
                                    with = || ).
        CONDENSE method_name NO-GAPS.
        source_code_line_impl =  source_code_line_impl &&  '.'.
*        APPEND |.| TO r_methods_implementation_code.
        APPEND source_code_line_impl TO r_methods_implementation_code.
        APPEND |  "add call to private method { method_name }| TO r_methods_implementation_code.
        APPEND |  "e.g. me->{ method_name }( ... ) | TO r_methods_implementation_code.
        APPEND |RAISE EXCEPTION TYPE cx_method_not_implemented.| TO r_methods_implementation_code.
        APPEND |ENDMETHOD.|     TO r_methods_implementation_code.
      ENDIF.

      IF result_endclass_statement <> -1.
        EXIT.
      ENDIF.
      DATA string1 TYPE string.
      DATA string2 TYPE string.
      DATA pos TYPE i.
      CLEAR pos.
      IF result_exclamation_mark <> -1.

        SPLIT source_code_line AT '!' INTO string1 string2.
        SPLIT string2 AT space INTO TABLE DATA(source_code_line_tab).

        LOOP AT source_code_line_tab INTO DATA(single_statement).
          pos += 1.
          IF single_statement = 'TYPE'.
            DATA(pos_type) = pos.
          ENDIF.
        ENDLOOP.

        DATA(type_of) = source_code_line_tab[ pos_type + 1 ].

        "REPLACE '!' IN type_of WITH ''.

        LOOP AT wrapper_interface_code INTO DATA(interface_code_line).
          CLEAR result_in_interface.
          DATA(result_is_in_interface) = find( val = interface_code_line sub  = type_of case = abap_false ).
          DATA(first_methods_statement) = find( val = interface_code_line sub  = 'METHODS' case = abap_false ).
          "only search in types statements
          IF first_methods_statement <> -1.
            EXIT.
          ENDIF.
          IF result_is_in_interface <> -1.
            result_in_interface = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF result_first_statement <> -1.
        add_code = abap_true.
      ENDIF.

      IF add_code = abap_true AND result_last_statement <> -1.
        add_code = abap_false.
      ENDIF.

      IF result_type_statement <> -1 AND
         result_in_interface = abap_true AND
         result_destination_statement = -1
         .

        "certain function modules such as SATC_CI_GET_RESULT use
        "built in types such as 'I'
        "searching for 'I' by name is not reliable

        CASE to_upper( type_of ).
            "built in numeric types
          WHEN 'I' OR 'B' OR 'S' OR 'INT8' OR 'P' OR 'F'.
            "built in character type
          WHEN 'C' OR 'N'  OR 'STRING'.
            "built in date type
          WHEN 'D' OR 'T'.
            "not a built in type
          WHEN OTHERS.
            source_code_line = replace( val = source_code_line
                                        sub = |TYPE |
                                        with = |TYPE { wrapper_interface_name }~| ).
        ENDCASE.
      ENDIF.

      IF result_destination_statement <> -1.
        source_code_line =   replace( val = source_code_line
                                               sub = |DESTINATION _dest_|
                                               with = |DESTINATION space| ).
      ENDIF.

      IF result_none_statement <> -1.
        source_code_line =   replace( val = source_code_line
                                               sub = |DESTINATION 'NONE'|
                                               with = |DESTINATION space| ).
      ENDIF.

      IF add_code = abap_true.

        IF result_class_methods <> -1.
          APPEND '.' TO r_methods_definition_code.
        ENDIF.
        APPEND source_code_line TO r_methods_definition_code.
      ENDIF.



    ENDLOOP.
    APPEND '.' TO r_methods_definition_code.
  ENDMETHOD.

ENDCLASS.""",
    r""" function_type-funcname  NO INTERVALS .

PARAMETERS package TYPE tadir-devclass.

PARAMETERS : yes_intf RADIOBUTTON GROUP rad1 DEFAULT 'X',
             no_intf  RADIOBUTTON GROUP rad1.

PARAMETERS : wrapclas TYPE sxco_class_name DEFAULT 'ZCL_WRAP_TEST'.
PARAMETERS : wrapfact TYPE sxco_class_name DEFAULT 'ZCL_FACT_TEST'.
PARAMETERS : wrapintf TYPE sxco_class_name DEFAULT 'ZIF_WRAP_TEST'.

*PARAMETERS : demomode TYPE abap_bool DEFAULT abap_true.
DATA demomode TYPE abap_bool VALUE abap_false.

"overwrite existing objects
PARAMETERS : patchobj TYPE abap_bool DEFAULT abap_true.

LOOP AT s_func INTO DATA(function_module_sel_option).
  "transaction aco_proxy only supports remote enabled function modules
  SELECT SINGLE * FROM tfdir INTO @DATA(function_module_info) WHERE funcname = @function_module_sel_option-low.
  IF function_module_info-fmode IS NOT INITIAL.
    APPEND to_upper( function_module_sel_option-low ) TO function_modules.
  ELSE.
    WRITE : / |function module { function_module_sel_option-low } has been skipped, because it is not remote-enabled. |.
  ENDIF.
ENDLOOP.

TRY.
    DATA(tier2_rfc_proxy_generator) = NEW zcl_gen_rfc_tier2_proxy(
      i_package_name                 = package
      i_transport_request            = ''
      i_generate_intf_and_fact_class = yes_intf
      i_wrapper_class_name           = wrapclas
      i_wrapper_interface_name       = wrapintf
      i_wrapper_factory_class_name   = wrapfact
      i_function_modules             = function_modules
      i_overwrite_objects            = patchobj
    ).

  CATCH cx_abap_invalid_value INTO DATA(invalid_parameter).
    WRITE : / 'Exception occured:'.
    WRITE : / invalid_parameter->get_text(  ).
    EXIT.
ENDTRY.

DATA(exception_text) = tier2_rfc_proxy_generator->generate_wrapper_objects( demomode ).

IF exception_text IS NOT INITIAL.
  WRITE : 'Exception occured generating objects:'.
  WRITE : exception_text.
  EXIT.
ENDIF.

DATA objects TYPE STANDARD TABLE OF dwinactiv .

IF yes_intf = abap_true.
  objects =  VALUE #(
                       ( object = 'INTF' obj_name = wrapintf uname = sy-uname )
                       ( object = 'CLAS' obj_name = wrapclas uname = sy-uname )
                       ( object = 'CLAS' obj_name = wrapfact uname = sy-uname )
                         ).
ELSE.
  objects =  VALUE #(
                      ( object = 'CLAS' obj_name = wrapclas uname = sy-uname )
                         ).
ENDIF.

IF demomode = abap_False.

  CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
    TABLES
      objects                = objects
    EXCEPTIONS
      excecution_error       = 1
      cancelled              = 2
      insert_into_corr_error = 3
      OTHERS                 = 4.

  IF sy-subrc <> 0.
    WRITE : / |error occured when activating classes. SY-SUBRC = { sy-subrc } |.
    EXIT.
  ELSE.
    WRITE : / |Generation finished:|.
    IF yes_intf = abap_true.
      WRITE : / |{ wrapclas }, { wrapfact } and { wrapintf }|.
    ELSE.
      WRITE : / |{ wrapclas }|.
    ENDIF.
  ENDIF.

ELSE.

  WRITE : / '***************************'.
  WRITE : / 'methods_code_definition'.
  WRITE : / '***************************'.

  LOOP AT tier2_rfc_proxy_generator->methods_code_definition INTO source_code_line.
    WRITE : / source_code_line.
  ENDLOOP.

  WRITE : / '***************************'.
  WRITE : / 'methods_code_implementation'.
  WRITE : / '***************************'.

  LOOP AT tier2_rfc_proxy_generator->methods_code_implementation INTO source_code_line.
    WRITE : / source_code_line.
  ENDLOOP.

  WRITE : / '***************************'.
  WRITE : / 'wrapper_factory_class_code'.
  WRITE : / '***************************'.

  LOOP AT tier2_rfc_proxy_generator->wrapper_factory_class_code INTO source_code_line.
    WRITE : / source_code_line.
  ENDLOOP.

  WRITE : / '***************************'.
  WRITE : / 'wrapper_class_code'.
  WRITE : / '***************************'.

  LOOP AT tier2_rfc_proxy_generator->wrapper_class_code INTO source_code_line.
    WRITE : / source_code_line.
  ENDLOOP.

  WRITE : / '***************************'.
  WRITE : / 'wrapper_interface_code'.
  WRITE : / '***************************'.


  LOOP AT tier2_rfc_proxy_generator->wrapper_interface_code INTO source_code_line.
    WRITE : / source_code_line.
  ENDLOOP.

ENDIF.""",
    r"""CLASS zcl_conversion_ext_int DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_conversion_ext_int .

    CLASS-DATA mo_instance TYPE REF TO zcl_conversion_ext_int READ-ONLY.

    CLASS-METHODS:
      "! Factory method to create a new conversion for BAPI consumption instance
      "! @parameter ro_instance | Conversion for BAPI consumption instance
      "! @raising ZCX_conversion_ext_int | Conversion for BAPI consumption exception
      get_instance
        RETURNING
          VALUE(ro_instance) TYPE REF TO zif_conversion_ext_int "cl_conversion_ext_int
        RAISING
          zcx_conversion_ext_int.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.""",
    r"""CLASS zcl_conversion_ext_int IMPLEMENTATION.

  METHOD zif_conversion_ext_int~currency_amount_ext_to_int.

    CALL FUNCTION 'CURRENCY_AMOUNT_BAPI_TO_SAP'
      EXPORTING
        currency              = currency
        bapi_amount           = bapi_amount
      IMPORTING
        sap_amount            = sap_amount
      EXCEPTIONS
        bapi_amount_incorrect = 1.
    CASE sy-subrc .
      WHEN 0.
      WHEN 1.
        RAISE EXCEPTION TYPE ZCX_conversion_ext_int
          EXPORTING
            textid = ZCX_conversion_ext_int=>bapi_amount_incorrect.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_conversion_ext_int~currency_amount_int_to_ext.
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = currency
        sap_amount  = sap_amount
      IMPORTING
        bapi_amount = bapi_amount.
  ENDMETHOD.


  METHOD zif_conversion_ext_int~currency_code_ext_to_int.
*  DATA unique TYPE calltrans2.
    CALL FUNCTION 'CURRENCY_CODE_ISO_TO_SAP'
      EXPORTING
        iso_code  = iso_code
      IMPORTING
        sap_code  = sap_code
        unique    = is_unique
      EXCEPTIONS
        not_found = 1.

    CASE sy-subrc.
      WHEN 0.
      WHEN 1.
        RAISE EXCEPTION TYPE ZCX_conversion_ext_int
          EXPORTING
            textid = ZCX_conversion_ext_int=>value_not_found
            attr1  = |{ iso_code }|.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_conversion_ext_int~currency_code_int_to_ext.

    CALL FUNCTION 'CURRENCY_CODE_SAP_TO_ISO'
      EXPORTING
        sap_code  = sap_code
*       no_msg_repetition =
*       no_msg_output     =
      IMPORTING
        iso_code  = iso_code
*       not_allowed =
*       msg_out   =
      EXCEPTIONS
        not_found = 1.
    CASE sy-subrc.
      WHEN 0.
      WHEN 1.
        RAISE EXCEPTION TYPE ZCX_conversion_ext_int
          EXPORTING
            textid = ZCX_conversion_ext_int=>value_not_found
            attr1  = |{ sap_code }|.
    ENDCASE.


  ENDMETHOD.


  METHOD zif_conversion_ext_int~language_code_ext_to_int.

    CALL FUNCTION 'LANGUAGE_CODE_ISO_TO_SAP'
      EXPORTING
        iso_code  = iso_code
      IMPORTING
        sap_code  = sap_code
      EXCEPTIONS
        not_found = 1.

    CASE sy-subrc.
      WHEN 0.
      WHEN 1.
        RAISE EXCEPTION TYPE ZCX_conversion_ext_int
          EXPORTING
            textid = ZCX_conversion_ext_int=>value_not_found
            attr1  = |{ iso_code }|.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_conversion_ext_int~language_code_int_to_ext.

    CALL FUNCTION 'LANGUAGE_CODE_SAP_TO_ISO'
      EXPORTING
        sap_code  = sap_code
      IMPORTING
        iso_code  = iso_code
      EXCEPTIONS
        not_found = 1.
    CASE sy-subrc.
      WHEN 0.
      WHEN 1.
        RAISE EXCEPTION TYPE ZCX_conversion_ext_int
          EXPORTING
            textid = ZCX_conversion_ext_int=>value_not_found
            attr1  = |{ sap_code }|.
    ENDCASE.


  ENDMETHOD.


  METHOD zif_conversion_ext_int~unit_of_measure_ext_to_int.

    CALL FUNCTION 'cl_abap_unit_assert=>fail( msg    = |sap_code { lv_sap_code } should not exist| ).UNIT_OF_MEASURE_ISO_TO_SAP'
      EXPORTING
        iso_code  = iso_code
      IMPORTING
        sap_code  = sap_code
        unique    = unique
      EXCEPTIONS
        not_found = 1.
    CASE sy-subrc.
      WHEN 0.
      WHEN 1.
        RAISE EXCEPTION TYPE ZCX_conversion_ext_int
          EXPORTING
            textid = ZCX_conversion_ext_int=>value_not_found
            attr1  = |{ sap_code }|.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_conversion_ext_int~unit_of_measure_int_to_ext.

    CALL FUNCTION 'UNIT_OF_MEASURE_SAP_TO_ISO'
      EXPORTING
        sap_code    = sap_code
      IMPORTING
        iso_code    = iso_code
      EXCEPTIONS
        not_found   = 1
        no_iso_code = 2.
    CASE sy-subrc.
      WHEN 0.
      WHEN 1.
        RAISE EXCEPTION TYPE ZCX_conversion_ext_int
          EXPORTING
            textid = ZCX_conversion_ext_int=>value_not_found
            attr1  = |{ sap_code }|.
      WHEN 2.
        RAISE EXCEPTION TYPE ZCX_conversion_ext_int
          EXPORTING
            textid = ZCX_conversion_ext_int=>no_iso_code_maintained
            attr1  = |{ iso_code }|.
    ENDCASE.

  ENDMETHOD.



  METHOD get_instance.

    IF mo_instance IS INITIAL.
      mo_instance = NEW zcl_conversion_ext_int( ).
    ENDIF.

    ro_instance = mo_instance.

  ENDMETHOD.

ENDCLASS.""",
    r"""CLASS zcx_conversion_ext_int DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check "cx_xco_runtime_exception
  FINAL
  CREATE PUBLIC .

*CX_STATIC_CHECK and implementing the interface IF_T100_DYN_MSG.

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg.

    CONSTANTS:
      gc_msgid TYPE symsgid VALUE 'ZCM_CONV_EXT_INT',

      BEGIN OF bapi_amount_incorrect,
        msgid TYPE symsgid VALUE 'CM_CONV_EXT_INT',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF bapi_amount_incorrect
      ,
      BEGIN OF value_not_found,
        msgid TYPE symsgid VALUE 'ZCM_CONV_EXT_INT',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF value_not_found
      ,
      BEGIN OF no_iso_code_maintained,
        msgid TYPE symsgid VALUE 'ZCM_CONV_EXT_INT',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF  no_iso_code_maintained
      ,
      BEGIN OF other_error,
        msgid TYPE symsgid VALUE 'ZCM_CONV_EXT_INT',
        msgno TYPE symsgno VALUE '030',
        attr1 TYPE scx_attrname VALUE 'attr1',
        attr2 TYPE scx_attrname VALUE 'attr2',
        attr3 TYPE scx_attrname VALUE 'attr3',
        attr4 TYPE scx_attrname VALUE 'attr4',
      END OF  other_error
      .


    DATA attr1 TYPE string.
    DATA attr2 TYPE string.

    CLASS-METHODS class_constructor .
    METHODS constructor
      IMPORTING
        !textid LIKE if_t100_message=>t100key OPTIONAL
*        !previous   LIKE previous OPTIONAL
        !attr1  TYPE string OPTIONAL
        !attr2  TYPE string OPTIONAL .


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS zcx_conversion_ext_int IMPLEMENTATION.

  METHOD class_constructor.
  ENDMETHOD.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor(
*      textid   = textid
*      previous =
    ).

    me->attr1 = attr1.
    me->attr2 = attr2.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.

ENDCLASS.""",
    r"""INTERFACE zif_conversion_ext_int
  PUBLIC .

  METHODS:
    currency_code_ext_to_int
      IMPORTING
                VALUE(iso_code)  TYPE isocd
      EXPORTING
                VALUE(sap_code)  TYPE waers_curc
                VALUE(is_unique) TYPE abap_bool
      RAISING   ZCX_conversion_ext_int ,

    currency_code_int_to_ext
      IMPORTING
                VALUE(sap_code) TYPE waers_curc
      EXPORTING
                VALUE(iso_code) TYPE isocd
      RAISING   ZCX_conversion_ext_int ,

    currency_amount_ext_to_int
      IMPORTING
                VALUE(currency)    TYPE waers_curc
                VALUE(bapi_amount) TYPE bapicurr_d
      EXPORTING
                VALUE(sap_amount)  TYPE bapicurr_d
      RAISING   ZCX_conversion_ext_int ,
      
    currency_amount_int_to_ext
      IMPORTING
                VALUE(currency)    TYPE waers_curc
                VALUE(sap_amount)  TYPE bapicurr_d
      EXPORTING
                VALUE(bapi_amount) TYPE bapicurr_d
      RAISING   ZCX_conversion_ext_int ,

    language_code_ext_to_int
      IMPORTING
                VALUE(iso_code) TYPE laiso
      EXPORTING
                VALUE(sap_code) TYPE spras
      RAISING   ZCX_conversion_ext_int ,

    language_code_int_to_ext
      IMPORTING
                VALUE(sap_code) TYPE spras
      EXPORTING
                VALUE(iso_code) TYPE laiso
      RAISING   ZCX_conversion_ext_int ,

    unit_of_measure_ext_to_int
      IMPORTING
                VALUE(iso_code) TYPE isocd_unit
      EXPORTING
                VALUE(sap_code) TYPE msehi
                VALUE(unique)   TYPE xfeld
      RAISING   ZCX_conversion_ext_int ,

    unit_of_measure_int_to_ext
      IMPORTING
                VALUE(sap_code) TYPE msehi
      EXPORTING
                VALUE(iso_code) TYPE isocd_unit
      RAISING   ZCX_conversion_ext_int.

ENDINTERFACE.""",
    r"""class ZBP_PRA_MF_C_MUSICFESTTP definition
  public
  abstract
  final
  for behavior of ZPRA_MF_C_MUSICFESTIVALTP .

public section.
protected section.
private section.
ENDCLASS.



CLASS ZBP_PRA_MF_C_MUSICFESTTP IMPLEMENTATION.
ENDCLASS.""",
    r"""class ZBP_PRA_MF_C_VISITORTP definition
  public
  abstract
  final
  for behavior of ZPRA_MF_C_VISITORTP .

public section.
protected section.
private section.
ENDCLASS.



CLASS ZBP_PRA_MF_C_VISITORTP IMPLEMENTATION.
ENDCLASS.""",
    r"""class ZBP_PRA_MF_R_MUSICFESTIVAL definition
  public
  abstract
  final
  for behavior of ZPRA_MF_R_MUSICFESTIVAL .

public section.
protected section.
private section.
ENDCLASS.



CLASS ZBP_PRA_MF_R_MUSICFESTIVAL IMPLEMENTATION.
ENDCLASS.""",
    r"""CLASS ltc_validation_methods DEFINITION DEFERRED FOR TESTING.
CLASS ltc_action_methods DEFINITION DEFERRED FOR TESTING.
CLASS ltcl_determination_methods DEFINITION DEFERRED FOR TESTING.
CLASS ltc_authorization_methods DEFINITION DEFERRED FOR TESTING.

CLASS lhc_zpra_mf_r_musicfestival DEFINITION INHERITING FROM cl_abap_behavior_handler
FRIENDS ltc_validation_methods ltc_action_methods ltcl_determination_methods ltc_authorization_methods.

  PRIVATE SECTION.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING
      REQUEST requested_authorizations FOR MusicFestival
      RESULT result.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR MusicFestival RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR MusicFestival RESULT result.

    "Validations
    METHODS validateMandatoryValue FOR VALIDATE ON SAVE
      IMPORTING keys FOR MusicFestival~validateMandatoryValue.
    METHODS validateMaxVisitors FOR VALIDATE ON SAVE
      IMPORTING keys FOR MusicFestival~validateMaxVisitors.
    METHODS validateDate FOR VALIDATE ON SAVE
      IMPORTING keys FOR MusicFestival~validateDate.

    "Determinations
    METHODS determineStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR MusicFestival~determineStatus.
    METHODS determineAvailableSeats FOR DETERMINE ON MODIFY
      IMPORTING keys FOR MusicFestival~determineAvailableSeats.

    "Actions
    METHODS calculateFreeVisitorSeats FOR MODIFY
      IMPORTING keys FOR ACTION MusicFestival~calculateFreeVisitorSeats.
    METHODS cancel FOR MODIFY
      IMPORTING keys FOR ACTION MusicFestival~cancel RESULT result.
    METHODS publish FOR MODIFY
      IMPORTING keys FOR ACTION MusicFestival~publish RESULT result.
    METHODS createproject FOR MODIFY
      IMPORTING keys FOR ACTION MusicFestival~CrProj RESULT result.
    METHODS generateSampleData FOR MODIFY
      IMPORTING keys FOR ACTION MusicFestival~generateSampleData.

ENDCLASS.""",
    r"""CLASS lhc_zpra_mf_r_musicfestival IMPLEMENTATION.
  METHOD get_global_authorizations.
    IF requested_authorizations-%create EQ if_abap_behv=>mk-on.
*     check create authorization
      AUTHORITY-CHECK OBJECT 'ZPRA_MF_AO' ID 'ACTVT' FIELD '01'.
      result-%create = COND #( WHEN sy-subrc = 0 THEN
      if_abap_behv=>auth-allowed ELSE
      if_abap_behv=>auth-unauthorized ).
    ENDIF.

    IF requested_authorizations-%update EQ if_abap_behv=>mk-on.
*     check update authorization
      AUTHORITY-CHECK OBJECT 'ZPRA_MF_AO' ID 'ACTVT' FIELD '02'.
      result-%update = COND #( WHEN sy-subrc = 0 THEN
      if_abap_behv=>auth-allowed ELSE
      if_abap_behv=>auth-unauthorized ).
    ENDIF.

    IF requested_authorizations-%delete EQ if_abap_behv=>mk-on.
*     check delete authorization
      AUTHORITY-CHECK OBJECT 'ZPRA_MF_AO' ID 'ACTVT' FIELD '06'.
      result-%delete = COND #( WHEN sy-subrc = 0 THEN
      if_abap_behv=>auth-allowed ELSE
      if_abap_behv=>auth-unauthorized ).
    ENDIF.

  ENDMETHOD.

  METHOD get_instance_authorizations.
    DATA: update_requested TYPE abap_bool.

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
    ENTITY MusicFestival
    FIELDS ( Uuid ) WITH CORRESPONDING #( keys )
    RESULT DATA(events)
    FAILED failed.

    CHECK events IS NOT INITIAL.

    update_requested = COND #( WHEN requested_authorizations-%update = if_abap_behv=>mk-on OR
                                    requested_authorizations-%delete = if_abap_behv=>mk-on OR
                                    requested_authorizations-%action-publish = if_abap_behv=>mk-on
                                    THEN
                                    abap_true ELSE abap_false ).

    LOOP AT events ASSIGNING FIELD-SYMBOL(<lfs_events>).
      IF update_requested = abap_true.
        "check authorization
        AUTHORITY-CHECK OBJECT 'ZPRA_MF_AO'
        ID 'ACTVT' FIELD '02'.
        IF sy-subrc NE 0.
          APPEND VALUE #( %tky            = <lfs_events>-%tky
                          %update         = if_abap_behv=>auth-unauthorized
                          %delete         = if_abap_behv=>auth-unauthorized
                          %action-edit    = if_abap_behv=>auth-unauthorized
                          %action-publish = if_abap_behv=>auth-unauthorized
                          %action-crproj  = if_abap_behv=>auth-unauthorized ) TO result.
        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_instance_features.
    DATA: music_festivals TYPE TABLE FOR READ RESULT ZPRA_MF_R_MusicFestival.

    " Logic to enable create button only when status is Published
    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
         FIELDS ( Status project_id  )
         WITH CORRESPONDING #( keys )
      RESULT music_festivals
      FAILED DATA(read_failed).

    DATA(music_festival) = VALUE #( music_festivals[ 1 ] OPTIONAL ).

    result = VALUE #( FOR ls_event IN music_festivals
                      ( %tky = ls_event-%tky
                        %features-%assoc-_Visits = COND #(
                          WHEN ls_event-Status = zcl_pra_mf_enum_mf_status=>published
                            THEN if_abap_behv=>fc-o-enabled
                            ELSE if_abap_behv=>fc-o-disabled )

                        %features-%action-publish = COND #(
                          WHEN ls_event-%is_draft = if_abap_behv=>mk-off
                            THEN if_abap_behv=>fc-o-enabled
                            ELSE if_abap_behv=>fc-o-disabled )

                        %features-%delete = COND #(
                          WHEN ls_event-Status = zcl_pra_mf_enum_mf_status=>published OR
                               ls_event-Status = zcl_pra_mf_enum_mf_status=>fully_booked
                            THEN if_abap_behv=>fc-o-disabled
                            ELSE if_abap_behv=>fc-o-enabled )

                        %features-%action-cancel = COND #(
                          WHEN ls_event-%is_draft = if_abap_behv=>mk-off
                            THEN if_abap_behv=>fc-o-enabled
                            ELSE if_abap_behv=>fc-o-disabled )

                        %features-%action-CrProj = COND #(
                          WHEN ls_event-Status = zcl_pra_mf_enum_mf_status=>published AND
                               music_festival-project_id IS INITIAL
                            THEN if_abap_behv=>fc-o-enabled
                            ELSE if_abap_behv=>fc-o-disabled ) ) ).

  ENDMETHOD.

  METHOD validateMandatoryValue.

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        FIELDS ( Title EventDateTime MaxVisitorsNumber )
        WITH CORRESPONDING #( keys )
        RESULT DATA(events).

    LOOP AT events REFERENCE INTO DATA(event).

      INSERT VALUE #( %tky = event->%tky %state_area = zcm_pra_mf_messages=>state_area-validate_event ) INTO TABLE reported-musicfestival.

      IF event->Title IS INITIAL OR
         event->EventDateTime IS INITIAL OR
         event->MaxVisitorsNumber IS INITIAL.

        INSERT VALUE #( %tky = event->%tky ) INTO TABLE failed-musicfestival.
        INSERT VALUE #(
          %tky = event->%tky
          %state_area = zcm_pra_mf_messages=>state_area-validate_event
          "Fill in all mandatory fields to proceed.
          %msg = NEW zcm_pra_mf_messages( textid = zcm_pra_mf_messages=>event_mandatory_value_missing
                                          severity = if_abap_behv_message=>severity-error )
          %element-Title = COND #( WHEN event->Title IS INITIAL
                                   THEN if_abap_behv=>mk-on
                                   ELSE if_abap_behv=>mk-off )
          %element-EventDateTime = COND #( WHEN event->EventDateTime IS INITIAL
                                           THEN if_abap_behv=>mk-on
                                           ELSE if_abap_behv=>mk-off )
          %element-MaxVisitorsNumber = COND #( WHEN event->MaxVisitorsNumber IS INITIAL
                                               THEN if_abap_behv=>mk-on
                                               ELSE if_abap_behv=>mk-off ) ) INTO TABLE reported-musicfestival.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD validateMaxVisitors.

    DATA: booked_visitors TYPE TABLE FOR READ RESULT ZPRA_MF_R_MusicFestival\\Visits.

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        FIELDS ( Uuid FreeVisitorSeats MaxVisitorsNumber )
        WITH CORRESPONDING #( keys )
        RESULT DATA(events)
      ENTITY MusicFestival BY \_Visits
        FIELDS ( Uuid ParentUuid Status )
        WITH CORRESPONDING #( keys )
        RESULT DATA(event_visits).

    LOOP AT events REFERENCE INTO DATA(event).

      INSERT VALUE #( %tky = event->%tky
                      %state_area = zcm_pra_mf_messages=>state_area-validate_visitors )
        INTO TABLE reported-musicfestival.

      IF event->MaxVisitorsNumber <= 0.
        INSERT VALUE #( %tky = event->%tky ) INTO TABLE failed-musicfestival.
        INSERT VALUE #( %tky                       = event->%tky
                        %state_area                = zcm_pra_mf_messages=>state_area-validate_visitors
                        "Maximum visitors must be greater than zero.
                        %msg                       = NEW zcm_pra_mf_messages( textid   = zcm_pra_mf_messages=>max_visitor_zero_negative
                                                                              severity = if_abap_behv_message=>severity-error )
                        %element-MaxVisitorsNumber = if_abap_behv=>mk-on )
          INTO TABLE reported-musicfestival.
        CONTINUE.
      ENDIF.

      booked_visitors = VALUE #( FOR visit IN event_visits
                                 WHERE ( ParentUuid = event->uuid
                                 AND     Status     = zcl_pra_mf_enum_visit_status=>booked )
                                 ( visit ) ).
      IF lines( booked_visitors ) > event->MaxVisitorsNumber.

        INSERT VALUE #( %tky = event->%tky ) INTO TABLE failed-musicfestival.
        INSERT VALUE #( %tky                       = event->%tky
                        %state_area                = zcm_pra_mf_messages=>state_area-validate_visitors
                        "Maximum visitors must be equal to or greater than booked visitors.
                        %msg                       = NEW zcm_pra_mf_messages( textid   = zcm_pra_mf_messages=>max_visitors_less_than_booked
                                                                              severity = if_abap_behv_message=>severity-error )
                        %element-MaxVisitorsNumber = if_abap_behv=>mk-on ) INTO TABLE reported-musicfestival.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD validateDate.

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        FIELDS ( EventDateTime )
        WITH CORRESPONDING #( keys )
        RESULT DATA(events).

    LOOP AT events REFERENCE INTO DATA(event).

      INSERT VALUE #( %tky = event->%tky %state_area = zcm_pra_mf_messages=>state_area-validate_date ) INTO TABLE reported-musicfestival.

      IF event->EventDateTime IS NOT INITIAL AND event->EventDateTime < utclong_current( ).

        INSERT VALUE #( %tky = event->%tky ) INTO TABLE failed-musicfestival.
        INSERT VALUE #( %tky                   = event->%tky
                        %state_area            = zcm_pra_mf_messages=>state_area-validate_date
                        "Event date and time must be in the future.
                        %msg                   = NEW zcm_pra_mf_messages( textid   = zcm_pra_mf_messages=>event_datetime_invalid
                                                                          severity = if_abap_behv_message=>severity-error )
                        %element-EventDateTime = if_abap_behv=>mk-on ) INTO TABLE reported-musicfestival.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD determineStatus.

    DATA: booked_visitors TYPE TABLE FOR READ RESULT ZPRA_MF_R_MusicFestival\\Visits.

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        FIELDS ( Status MaxVisitorsNumber FreeVisitorSeats )
        WITH CORRESPONDING #( keys )
        RESULT DATA(events)
      ENTITY MusicFestival BY \_Visits
        FIELDS ( ParentUuid Status )
        WITH CORRESPONDING #( keys )
        RESULT DATA(event_visits).

    LOOP AT events REFERENCE INTO DATA(event).

      booked_visitors = VALUE #( FOR visit IN event_visits
                                 WHERE ( ParentUuid = event->uuid
                                 AND     Status     = zcl_pra_mf_enum_visit_status=>booked )
                                 ( visit ) ).

      event->Status = COND #( WHEN event->Status IS INITIAL
                              THEN zcl_pra_mf_enum_mf_status=>in_preparation
                              WHEN event->Status = zcl_pra_mf_enum_mf_status=>fully_booked AND event->MaxVisitorsNumber <> lines( booked_visitors )
                              THEN zcl_pra_mf_enum_mf_status=>published
                              WHEN event->MaxVisitorsNumber > 0 AND event->MaxVisitorsNumber = lines( booked_visitors )
                              THEN zcl_pra_mf_enum_mf_status=>fully_booked
                              ELSE abap_off ).
    ENDLOOP.

    DELETE events WHERE Status IS INITIAL.
    CHECK lines( events ) > 0.

    MODIFY ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
      UPDATE FIELDS ( Status )
      WITH CORRESPONDING #( events ).

  ENDMETHOD.

  METHOD determineAvailableSeats.

    MODIFY ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        EXECUTE calculateFreeVisitorSeats
        FROM CORRESPONDING #( keys ).

  ENDMETHOD.

  METHOD calculateFreeVisitorSeats.

    DATA: booked_visitors TYPE TABLE FOR READ RESULT ZPRA_MF_R_MusicFestival\\Visits.

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        FIELDS ( Uuid MaxVisitorsNumber FreeVisitorSeats )
        WITH CORRESPONDING #( keys )
      RESULT DATA(events)
      ENTITY MusicFestival BY \_Visits
        FIELDS ( Uuid ParentUuid Status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(event_visits).

    LOOP AT events REFERENCE INTO DATA(event).
      booked_visitors = VALUE #( FOR visit IN event_visits
                                 WHERE ( ParentUuid = event->uuid
                                 AND     Status     = zcl_pra_mf_enum_visit_status=>booked )
                                 ( visit ) ).

      event->FreeVisitorSeats = event->MaxVisitorsNumber - lines( booked_visitors ).
    ENDLOOP.

    MODIFY ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        UPDATE FIELDS ( FreeVisitorSeats )
        WITH VALUE #( FOR updated_event IN events
                      ( %tky             = updated_event-%tky
                        FreeVisitorSeats = updated_event-FreeVisitorSeats ) ).

  ENDMETHOD.

  METHOD cancel.

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        FIELDS ( Status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(events).

    MODIFY ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        UPDATE FIELDS ( Status )
        WITH VALUE #( FOR event IN events
                      ( %tky   = event-%tky
                        Status = zcl_pra_mf_enum_mf_status=>cancelled ) ).

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        FIELDS ( Status )
        WITH CORRESPONDING #( keys )
        RESULT DATA(updated_events).

    result = VALUE #( FOR event IN updated_events
                      ( %tky   = event-%tky
                        %param = event ) ).
  ENDMETHOD.

  METHOD publish.

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        FIELDS ( Uuid Status FreeVisitorSeats Title )
        WITH CORRESPONDING #( keys )
        RESULT DATA(events).

    LOOP AT events REFERENCE INTO DATA(event).

      IF event->Status =  zcl_pra_mf_enum_mf_status=>published OR
         event->Status =  zcl_pra_mf_enum_mf_status=>fully_booked.

        INSERT VALUE #( %tky = event->%tky ) INTO TABLE failed-musicfestival.
        INSERT VALUE #(
          %tky = event->%tky
          "The festival 'Title' has already been published.
          %msg = NEW zcm_pra_mf_messages( textid = zcm_pra_mf_messages=>event_already_published
                                          severity = if_abap_behv_message=>severity-error
                                          title = event->Title )
        %op-%action-publish = if_abap_behv=>mk-on
        %element-Status = if_abap_behv=>mk-on


           ) INTO TABLE reported-musicfestival.
      ENDIF.
    ENDLOOP.

    MODIFY ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        UPDATE FIELDS ( Status )
        WITH VALUE #( FOR mf_event IN events
                       ( %tky   = mf_event-%tky
                         Status = COND #( WHEN mf_event-FreeVisitorSeats = 0
                                          THEN zcl_pra_mf_enum_mf_status=>fully_booked
                                          ELSE zcl_pra_mf_enum_mf_status=>published ) ) ).

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        FIELDS ( Status )
        WITH CORRESPONDING #( keys )
        RESULT DATA(events_after_update).

    result = VALUE #( FOR event_updated IN events_after_update
                      ( %tky   = event_updated-%tky
                        %param = event_updated ) ).

  ENDMETHOD.

  METHOD createproject.

    DATA ls_project_data TYPE zcl_pra_mf_scm_ent_proj=>tys_a_enterprise_project_type.

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        ALL FIELDS
        WITH CORRESPONDING #( keys )
        RESULT DATA(lt_event_data).

    IF lt_event_data IS NOT INITIAL.

      DATA(ls_mf_data) = VALUE #( lt_event_data[ 1 ] OPTIONAL ).

      DATA(lo_project) = NEW zcl_pra_mf_ent_proj_outb_integ( ).

      ls_project_data-project = ls_mf_data-Title.
      ls_project_data-project_description = ls_mf_data-Description.

      CONVERT UTCLONG
      ls_mf_data-EventDateTime
      INTO DATE DATA(lv_event_date)
      TIME DATA(lv_event_time)
      TIME ZONE 'UTC'.

      ls_project_data-project_start_date = lv_event_date.
      ls_project_data-project_end_date = lv_event_date.


      lo_project->create_entproject( EXPORTING Is_project_data = ls_project_data
                                     IMPORTING et_message = DATA(lt_proj_message) ).

      IF lt_proj_message IS NOT INITIAL.

        INSERT VALUE #( %tky = ls_mf_data-%tky ) INTO TABLE failed-musicfestival.
        INSERT VALUE #(
          %tky = ls_mf_data-%tky
          "The festival 'Title' has already been published.
          %msg = NEW zcm_pra_mf_messages( textid = zcm_pra_mf_messages=>error_in_proj_creation
                                          severity = if_abap_behv_message=>severity-error
                                          title = ls_mf_data-Title )
        %op-%action-crproj = if_abap_behv=>mk-on
        %element-Status = if_abap_behv=>mk-on

           ) INTO TABLE reported-musicfestival.


      ELSEIF lt_proj_message IS INITIAL.

        DATA ls_mf TYPE zpra_mf_a_mf.

        ls_mf-project_id = |MF_| && |{ to_upper( ls_project_data-project ) }|.

        ls_mf-uuid = ls_mf_data-Uuid.

        MODIFY ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
          ENTITY MusicFestival
          UPDATE SET FIELDS
          WITH VALUE #( FOR entity IN lt_event_data
                        ( %tky       = entity-%tky
                          project_id = ls_mf-project_id ) ).

      ENDIF.

    ENDIF.


    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        FIELDS ( Status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_updated_entities).

    result = VALUE #( FOR ls_updated_entities IN lt_updated_entities
                      ( %tky   = ls_updated_entities-%tky
                        %param = ls_updated_entities ) ).

  ENDMETHOD.


  METHOD generateSampleData.
    DATA create_visitors TYPE TABLE FOR CREATE ZPRA_MF_R_Visitor.
    DATA create_music_fests TYPE TABLE FOR CREATE ZPRA_MF_R_MusicFestival.
    DATA cba_music_fest_visits TYPE TABLE FOR CREATE ZPRA_MF_R_MusicFestival\_Visits.
    DATA action_music_fest_publish TYPE TABLE FOR ACTION IMPORT ZPRA_MF_R_MusicFestival~publish.
    DATA action_visits_book TYPE TABLE FOR ACTION IMPORT ZPRA_MF_R_MusicFestival\\Visits~book.

    " first create Visitors, so that they can be added in Music Fests as Visits
    create_visitors = VALUE #(
                        ( %cid = `visitor01` Name = `Kenji Tanaka`     Email = `kenji.tanaka@pra.ondemand.com` )
                        ( %cid = `visitor02` Name = `Suresh Kumar`     Email = `Suresh Kumar` )
                        ( %cid = `visitor03` Name = `Jamal Adebayo`    Email = `jamal.adebayo@pra.ondemand.com` )
                        ( %cid = `visitor04` Name = `Shreya Reddy`     Email = `shreya.reddy@pra.ondemand.com` )
                        ( %cid = `visitor05` Name = `Miguel Rodriguez` Email = `miguel.rodriguez@pra.ondemand.com` )
                        ( %cid = `visitor06` Name = `Naomi Chen`       Email = `naomi.chen@pra.ondemand.com` )
                        ( %cid = `visitor07` Name = `Sofia Rossi`      Email = `sofia.rossi@pra.ondemand.com` )
                        ( %cid = `visitor08` Name = `Liam Johnson`     Email = `liam.johnson@pra.ondemand.com` )
                        ( %cid = `visitor09` Name = `Emma Brown`       Email = `emma.brown@pra.ondemand.com` )
                        ( %cid = `visitor10` Name = `Noah Davis`       Email = `noah.davis@pra.ondemand.com` )
                        ( %cid = `visitor11` Name = `Lukas Schneider`  Email = `lukas.schneider@pra.ondemand.com` ) ) ##NO_TEXT.
    MODIFY ENTITY ZPRA_MF_R_Visitor
      CREATE
      FIELDS ( Name Email )
      WITH create_visitors
      MAPPED   DATA(visitors_mapping)
      FAILED   DATA(visitors_failed)
      REPORTED DATA(visitors_reported).

    " create Music Festivals and Visits using (CreateByAssociation), Publish selected Music Fests, & Book Visits
    create_music_fests = VALUE #(
                          VisitorsFeeAmount   = `99`
                          VisitorsFeeCurrency = `USD`
                          EventDateTime       = utclong_add(  val   = utclong_current( )
                                                              days  = 30 )
                          ( %cid              = `mf01`
                            Title             = `Tango Tales Buenos Aires`
                            Description       = `Experience the passionate and intricate world of Argentine Tango.`
                            MaxVisitorsNumber = `25` )
                          ( %cid              = `mf02`
                            Title             = `Sakura Spring Kyoto`
                            Description       = `Celebrate the ephemeral beauty of cherry blossoms in ancient Kyoto.`
                            MaxVisitorsNumber = `5` )
                          ( %cid              = `mf03`
                            Title             = `Mediterranean Melodies Athens`
                            Description       = `Enjoy the soulful sounds and rhythms of the Mediterranean coast.`
                            MaxVisitorsNumber = `50` )
                          ( %cid              = `mf04`
                            Title             = `Stage of Words New York`
                            Description       = `Welcome to a stage in New York where words reign supreme`
                            MaxVisitorsNumber = `10` )
                          ( %cid              = `mf05`
                            Title             = `Rhythm of Rajasthan`
                            Description       = `Immerse yourself in the vibrant folk music and dance of Rajasthan.`
                            MaxVisitorsNumber = `20` ) ) ##NO_TEXT.

    cba_music_fest_visits = VALUE #( ( %cid_ref = `mf02`
                                      %target  = VALUE #(
                                                    ( %cid = `mf02_1` VisitorUuid = visitors_mapping-visitor[ 2 ]-uuid )
                                                    ( %cid = `mf02_2` VisitorUuid = visitors_mapping-visitor[ 3 ]-uuid )
                                                    ( %cid = `mf02_3` VisitorUuid = visitors_mapping-visitor[ 4 ]-uuid )
                                                    ( %cid = `mf02_4` VisitorUuid = visitors_mapping-visitor[ 5 ]-uuid )
                                                    ( %cid = `mf02_5` VisitorUuid = visitors_mapping-visitor[ 6 ]-uuid ) ) )
                                    ( %cid_ref = `mf03`
                                      %target  = VALUE #( ( %cid        = `mf03_1`
                                                            VisitorUuid = visitors_mapping-visitor[ 1 ]-uuid ) ) )
                                    ( %cid_ref = `mf04`
                                      %target  = VALUE #( ( %cid = `mf04_1` VisitorUuid = visitors_mapping-visitor[ 7 ]-uuid )
                                                          ( %cid = `mf04_2` VisitorUuid = visitors_mapping-visitor[ 8 ]-uuid ) ) ) ).
    action_music_fest_publish = VALUE #( ( %cid_ref = `mf02` )
                                        ( %cid_ref = `mf03` )
                                        ( %cid_ref = `mf04` ) ).
    action_visits_book = VALUE #( ( %cid_ref = `mf02_1` )
                                  ( %cid_ref = `mf02_2` )
                                  ( %cid_ref = `mf02_3` )
                                  ( %cid_ref = `mf02_4` )
                                  ( %cid_ref = `mf02_5` )
                                  ( %cid_ref = `mf03_1` )
                                  ( %cid_ref = `mf04_1` )
                                  ( %cid_ref = `mf04_2` ) ).

    MODIFY ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        CREATE
          FIELDS ( Title Description EventDateTime MaxVisitorsNumber VisitorsFeeAmount VisitorsFeeCurrency )
          WITH create_music_fests
        EXECUTE publish FROM action_music_fest_publish
        CREATE BY \_Visits
          FIELDS ( VisitorUuid )
          WITH cba_music_fest_visits
      ENTITY Visits
        EXECUTE book FROM action_visits_book
      MAPPED mapped
      FAILED failed
      REPORTED reported.

  ENDMETHOD.

ENDCLASS.""",
    r"""CLASS ltc_validation_methods DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_zpra_mf_r_musicfestival,               " the class to be tested
      cds_test_environment TYPE REF TO if_cds_test_environment,  " cds test double framework
      sql_test_environment TYPE REF TO if_osql_test_environment. " abap sql test double framework

    CLASS-METHODS:
      " setup test double framework
      class_setup,
      " stop test doubles
      class_teardown.

    METHODS:
      " reset test doubles
      setup,
      " rollback any changes
      teardown,

      validateDate            FOR TESTING,
      validateMandatoryValue  FOR TESTING,
      validateMaxVisitors     FOR TESTING.

ENDCLASS.""",
    r"""CLASS ltc_validation_methods IMPLEMENTATION.


  METHOD class_setup.
    " Create the class under Test
    " The class is abstract but can be constructed with the FOR TESTING
    CREATE OBJECT class_under_test FOR TESTING.
    " Create test doubles for database dependencies
    " The EML READ operation will then also access the test doubles
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds( i_for_entities = VALUE #(
        ( i_for_entity = 'ZPRA_MF_R_MUSICFESTIVAL' )
        ( i_for_entity = 'ZPRA_MF_R_VISITOR' )
        ( i_for_entity = 'ZPRA_MF_R_VISIT' ) ) ).
    cds_test_environment->enable_double_redirection( ).
    sql_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #(
        ( 'zpra_mf_d_mf' )
        ( 'zpra_mf_d_vst' )
        ( 'zpra_mf_d_vstr' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    " stop mocking
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " clear the content of the test double per test
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    " Clean up any involved entity
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD validateDate.
    DATA mf_mock_data TYPE STANDARD TABLE OF zpra_mf_a_mf.
    TYPES: BEGIN OF ty_entity_key,
             uuid TYPE sysuuid_x16,
           END OF ty_entity_key.

    DATA: failed      TYPE RESPONSE FOR FAILED LATE ZPRA_MF_R_MusicFestival,
          reported    TYPE RESPONSE FOR REPORTED LATE ZPRA_MF_R_MusicFestival,
          entity_keys TYPE STANDARD TABLE OF ty_entity_key.

    mf_mock_data = VALUE #( ( uuid = 'DEC190889AC21FE08191A45962D04217' event_date_time = '2025-01-01T00:00:00.0000000' )
                            ( uuid = 'DEC190889AC21FE08191A45962D04218' event_date_time = '2028-01-01T00:00:00.0000000' ) ).

    " insert test data into the cds test doubles
    cds_test_environment->insert_test_data( i_data = mf_mock_data ).

    " specify test entity keys
    entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04218' ) ).

    " execute the validation
    class_under_test->validateDate(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        failed   = failed
        reported = reported
    ).

    " Expect no failures and messages for the valid event date
    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    " As it a valid future date, expect the event to be returned
    cl_abap_unit_assert=>assert_not_initial( msg = 'reported' act = reported ).

    CLEAR entity_keys.

    " specify test entity keys
    entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04217' ) ).


    " execute the validation
    class_under_test->validateDate(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        failed   = failed
        reported = reported
    ).

    " Check the validation message for past event date
    cl_abap_unit_assert=>assert_not_initial( msg = 'Failed' act = failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'Failed Uuid' act = failed-musicfestival[ 1 ]-Uuid exp = 'DEC190889AC21FE08191A45962D04217' ).
    cl_abap_unit_assert=>assert_equals( act = reported-musicfestival[ 3 ]-%msg->if_t100_message~t100key
                                        exp = zcm_pra_mf_messages=>event_datetime_invalid ).

  ENDMETHOD.

  METHOD validateMandatoryValue.

    DATA mf_mock_data TYPE STANDARD TABLE OF zpra_mf_a_mf.
    " call the method to be tested
    TYPES: BEGIN OF ty_entity_key,
             uuid TYPE sysuuid_x16,
           END OF ty_entity_key.

    DATA: failed      TYPE RESPONSE FOR FAILED LATE ZPRA_MF_R_MusicFestival,
          reported    TYPE RESPONSE FOR REPORTED LATE ZPRA_MF_R_MusicFestival,
          entity_keys TYPE STANDARD TABLE OF ty_entity_key.

    mf_mock_data = VALUE #( ( uuid = 'DEC190889AC21FE08191A45962D04217' event_date_time = '2028-01-01T00:00:00.0000000' title = 'Event 1' max_visitors_number = '10' )
                            ( uuid = 'DEC190889AC21FE08191A45962D04218' event_date_time = '2028-01-01T00:00:00.0000000' ) ).

    " insert test data into the cds test doubles
    cds_test_environment->insert_test_data( i_data = mf_mock_data ).


    " specify test entity keys
    entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04217' ) ).

    " execute the validation
    class_under_test->validateMandatoryValue(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        failed   = failed
        reported = reported
    ).

    " Expect no failures and messages
    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_not_initial( msg = 'reported' act = reported ).

    CLEAR entity_keys.

    " specify test entity keys
    entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04218' ) ).


    " execute the validation
    class_under_test->validateMandatoryValue(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        failed   = failed
        reported = reported
    ).

    " Check the validation message for missing mandatory fields
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals( act = failed-musicfestival[ 1 ]-Uuid
                                        exp = 'DEC190889AC21FE08191A45962D04218' ).
    cl_abap_unit_assert=>assert_equals( act = reported-musicfestival[ 3 ]-%msg->if_t100_message~t100key
                                        exp = zcm_pra_mf_messages=>event_mandatory_value_missing ).

  ENDMETHOD.

  METHOD validateMaxVisitors.

    DATA: mf_mock_data   TYPE STANDARD TABLE OF zpra_mf_a_mf,
          vstr_mock_data TYPE STANDARD TABLE OF zpra_mf_a_vstr,
          vst_mock_data  TYPE STANDARD TABLE OF zpra_mf_a_vst.
    " call the method to be tested
    TYPES: BEGIN OF ty_entity_key,
             uuid TYPE sysuuid_x16,
           END OF ty_entity_key.

    DATA: failed      TYPE RESPONSE FOR FAILED LATE ZPRA_MF_R_MusicFestival,
          reported    TYPE RESPONSE FOR REPORTED LATE ZPRA_MF_R_MusicFestival,
          entity_keys TYPE STANDARD TABLE OF ty_entity_key.

    vstr_mock_data = VALUE #( ( uuid = 'DEC190889AC21FE08191A45962D04210' name = 'visitor1' )
                              ( uuid = 'DEC190889AC21FE08191A45962D04211' name = 'visitor2' )
                              ( uuid = 'DEC190889AC21FE08191A45962D04212' name = 'visitor3' ) ).

    mf_mock_data = VALUE #( ( uuid = 'DEC190889AC21FE08191A45962D04217' event_date_time = '2028-01-01T00:00:00.0000000' title = 'Event 1' max_visitors_number = 0 )
                            ( uuid = 'DEC190889AC21FE08191A45962D04218' event_date_time = '2028-01-01T00:00:00.0000000' title = 'Event 2' max_visitors_number = '2' )
                            ( uuid = 'DEC190889AC21FE08191A45962D04219' event_date_time = '2028-01-01T00:00:00.0000000' title = 'Event 3' max_visitors_number = '4' ) ).

    vst_mock_data = VALUE #( ( uuid = 'DEC190889AC21FE08191A45962D04213' parent_uuid = 'DEC190889AC21FE08191A45962D04218' visitor_uuid = 'DEC190889AC21FE08191A45962D04210' status = 'B' )
                             ( uuid = 'DEC190889AC21FE08191A45962D04214' parent_uuid = 'DEC190889AC21FE08191A45962D04218' visitor_uuid = 'DEC190889AC21FE08191A45962D04211' status = 'B' )
                             ( uuid = 'DEC190889AC21FE08191A45962D04215' parent_uuid = 'DEC190889AC21FE08191A45962D04218' visitor_uuid = 'DEC190889AC21FE08191A45962D04212' status = 'B' )
                             ( uuid = 'DEC190889AC21FE08191A45962D04223' parent_uuid = 'DEC190889AC21FE08191A45962D04219' visitor_uuid = 'DEC190889AC21FE08191A45962D04210' status = 'B' )
                             ( uuid = 'DEC190889AC21FE08191A45962D04224' parent_uuid = 'DEC190889AC21FE08191A45962D04219' visitor_uuid = 'DEC190889AC21FE08191A45962D04211' status = 'B' )
                             ( uuid = 'DEC190889AC21FE08191A45962D04225' parent_uuid = 'DEC190889AC21FE08191A45962D04219' visitor_uuid = 'DEC190889AC21FE08191A45962D04212' status = 'B' ) ).

    " insert test data into the cds test doubles
    cds_test_environment->insert_test_data( i_data = mf_mock_data ).
    cds_test_environment->insert_test_data( i_data = vst_mock_data ).
    cds_test_environment->insert_test_data( i_data = vstr_mock_data ).


    " specify test entity keys
    entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04219' ) ).

    " execute the validation
    class_under_test->validateMaxVisitors(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        failed   = failed
        reported = reported
    ).

    " Expect no failures and messages
    cl_abap_unit_assert=>assert_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_not_initial( msg = 'reported' act = reported ).

    CLEAR entity_keys.

    " specify test entity keys
    entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04218' ) ).


    " execute the validation
    class_under_test->validateMaxVisitors(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        failed   = failed
        reported = reported
    ).
    " Check the validation message for maximum number of visitors
    cl_abap_unit_assert=>assert_not_initial( msg = 'failed' act = failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'Failed Uuid' act = failed-musicfestival[ 1 ]-Uuid exp = 'DEC190889AC21FE08191A45962D04218' ).
    cl_abap_unit_assert=>assert_equals( act = reported-musicfestival[ 3 ]-%msg->if_t100_message~t100key
                                        exp = zcm_pra_mf_messages=>max_visitors_less_than_booked ).

    CLEAR entity_keys.

    " specify test entity keys
    entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04217' ) ).


    " execute the validation
    class_under_test->validateMaxVisitors(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        failed   = failed
        reported = reported
    ).

    " Check the validation message for negative or zero for maximum visitors
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_equals( act = failed-musicfestival[ 2 ]-Uuid
                                        exp = 'DEC190889AC21FE08191A45962D04217' ).
    cl_abap_unit_assert=>assert_equals( act = reported-musicfestival[ 3 ]-%msg->if_t100_message~t100key
                                        exp = zcm_pra_mf_messages=>max_visitors_less_than_booked ).

  ENDMETHOD.


ENDCLASS.

**************************************************************
*  Local class to test actions in behavior implementations   *
**************************************************************
"! @testing BDEF:ZPRA_MF_R_MUSICFESTIVAL
CLASS ltc_action_methods DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_zpra_mf_r_musicfestival,               " the class to be tested
      cds_test_environment TYPE REF TO if_cds_test_environment.  " cds test double framework

    CLASS-METHODS:
      " setup test double framework
      class_setup,
      " stop test doubles
      class_teardown.

    METHODS:
      " reset test doubles
      setup,
      " rollback any changes
      teardown,

      publish        FOR TESTING RAISING cx_static_check,
      cancel         FOR TESTING RAISING cx_static_check,
      generate_data  FOR TESTING RAISING cx_static_check.

ENDCLASS.""",
    r"""CLASS ltc_action_methods DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_zpra_mf_r_musicfestival,               " the class to be tested
      cds_test_environment TYPE REF TO if_cds_test_environment.  " cds test double framework

    CLASS-METHODS:
      " setup test double framework
      class_setup,
      " stop test doubles
      class_teardown.

    METHODS:
      " reset test doubles
      setup,
      " rollback any changes
      teardown,

      publish        FOR TESTING RAISING cx_static_check,
      cancel         FOR TESTING RAISING cx_static_check,
      generate_data  FOR TESTING RAISING cx_static_check.

ENDCLASS.""",
    r"""CLASS ltc_action_methods IMPLEMENTATION.

  METHOD class_setup.

    " Create the class under Test
    " The class is abstract but can be constructed with the FOR TESTING
    CREATE OBJECT class_under_test FOR TESTING.
    " Create test doubles for database dependencies
    " The EML READ operation will then also access the test doubles
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds( i_for_entities = VALUE #(
        ( i_for_entity = 'ZPRA_MF_R_MUSICFESTIVAL' )
        ( i_for_entity = 'ZPRA_MF_R_VISITOR' )
        ( i_for_entity = 'ZPRA_MF_R_VISIT' ) ) ).
    cds_test_environment->enable_double_redirection( ).

  ENDMETHOD.

  METHOD class_teardown.
    " stop mocking
    cds_test_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " clear the content of the test double per test
    cds_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    " Clean up any involved entity
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD publish.

    DATA mf_mock_data TYPE STANDARD TABLE OF zpra_mf_a_mf.

    mf_mock_data = VALUE #( ( uuid = 'DEC190889AC21FE08191A45962D04211' title = 'Event 1' max_visitors_number = 2 free_visitor_seats = 0 status = 'I' )
                            ( uuid = 'DEC190889AC21FE08191A45962D04212' title = 'Event 2' max_visitors_number = 2 free_visitor_seats = 1 status = 'I' )
                            ( uuid = 'DEC190889AC21FE08191A45962D04213' title = 'Event 3' max_visitors_number = 4 free_visitor_seats = 0 status = 'F' ) ).

    " insert test data into the cds test doubles
    cds_test_environment->insert_test_data( i_data = mf_mock_data ).

    " call the method to be tested
    TYPES: BEGIN OF ty_entity_key,
             uuid TYPE sysuuid_x16,
           END OF ty_entity_key.


    DATA: result      TYPE TABLE    FOR ACTION RESULT zpra_mf_r_musicfestival\\MusicFestival~publish,
          mapped      TYPE RESPONSE FOR MAPPED EARLY zpra_mf_r_musicfestival,
          failed      TYPE RESPONSE FOR FAILED EARLY zpra_mf_r_musicfestival,
          reported    TYPE RESPONSE FOR REPORTED EARLY zpra_mf_r_musicfestival,
          entity_keys TYPE STANDARD TABLE OF ty_entity_key.

    " specify test entity keys
    entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04211' ) ).

    " execute the action
    class_under_test->publish(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        result   = result
        mapped   = mapped
        failed   = failed
        reported = reported
    ).

    " expect input keys and output keys to be same and Status
    DATA exp LIKE result.
    exp = VALUE #(  ( Uuid = 'DEC190889AC21FE08191A45962D04211'  %param-Status = 'F' ) ).


    " current result; copy only fields of interest - i.e. Uuid and Status
    DATA act_fb LIKE result.

    act_fb = CORRESPONDING #( result MAPPING Uuid = Uuid
                                (  %param = %param MAPPING Status      = Status
                                EXCEPT * )
                            EXCEPT * ).

    cl_abap_unit_assert=>assert_equals( msg = 'Action Publish - Status - Fully Booked' exp = exp act = act_fb ).

    " additionally check by reading entity state
    READ ENTITY zpra_mf_r_musicfestival
    FIELDS ( Uuid Status ) WITH CORRESPONDING #( entity_keys )
    RESULT DATA(read_result).

    act_fb = VALUE #( FOR t IN read_result ( Uuid          = t-Uuid
                                             %param-Status = t-Status ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'Action Publish - Status - Fully Booked - Read result' exp = exp act = act_fb ).

    CLEAR entity_keys.

    " specify test entity keys
    entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04212' ) ).

    " execute the action
    class_under_test->publish(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        result   = result
        mapped   = mapped
        failed   = failed
        reported = reported
    ).

    " expect input keys and output keys to be same and Status
    DATA exp_publish LIKE result.
    exp_publish = VALUE #(  ( Uuid = 'DEC190889AC21FE08191A45962D04212'  %param-Status = 'P' ) ).


    " current result; copy only fields of interest - i.e. Uuid and Status
    DATA act_publish LIKE result.

    act_publish = CORRESPONDING #( result MAPPING Uuid = Uuid
                                (  %param = %param MAPPING Status      = Status
                                EXCEPT * )
                            EXCEPT * ).

    cl_abap_unit_assert=>assert_equals( msg = 'Action Publish - Status - Published' exp = exp_publish act = act_publish ).

    " additionally check by reading entity state
    READ ENTITY zpra_mf_r_musicfestival
    FIELDS ( Uuid Status ) WITH CORRESPONDING #( entity_keys )
    RESULT DATA(read_result_publish).

    act_publish = VALUE #( FOR t IN read_result_publish ( Uuid          = t-Uuid
                                                          %param-Status = t-Status ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'Action Publish - Status - Published - Read result' exp = exp_publish act = act_publish ).


  ENDMETHOD.

  METHOD cancel.

    DATA mf_mock_data TYPE STANDARD TABLE OF zpra_mf_a_mf.

    mf_mock_data = VALUE #( ( uuid = 'DEC190889AC21FE08191A45962D04211' title = 'Event 1' max_visitors_number = 2 free_visitor_seats = 0 status = 'I' ) ).

    " insert test data into the cds test doubles
    cds_test_environment->insert_test_data( i_data = mf_mock_data ).

    " call the method to be tested
    TYPES: BEGIN OF ty_entity_key,
             uuid TYPE sysuuid_x16,
           END OF ty_entity_key.


    DATA: result      TYPE TABLE    FOR ACTION RESULT zpra_mf_r_musicfestival\\MusicFestival~cancel,
          mapped      TYPE RESPONSE FOR MAPPED EARLY zpra_mf_r_musicfestival,
          failed      TYPE RESPONSE FOR FAILED EARLY zpra_mf_r_musicfestival,
          reported    TYPE RESPONSE FOR REPORTED EARLY zpra_mf_r_musicfestival,
          entity_keys TYPE STANDARD TABLE OF ty_entity_key.

    " specify test entity keys
    entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04211' ) ).

    " execute the action
    class_under_test->cancel(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        result   = result
        mapped   = mapped
        failed   = failed
        reported = reported
    ).


    " expect input keys and output keys to be same and Status
    DATA exp LIKE result.
    exp = VALUE #(  ( Uuid = 'DEC190889AC21FE08191A45962D04211'  %param-Status = 'C' ) ).


    " current result; copy only fields of interest - i.e. Uuid and Status
    DATA act LIKE result.

    act = CORRESPONDING #( result MAPPING Uuid = Uuid
                                (  %param = %param MAPPING Status      = Status
                                EXCEPT * )
                            EXCEPT * ).

    cl_abap_unit_assert=>assert_equals( msg = 'Action Cancel - Status - Cancel' exp = exp act = act ).

    " additionally check by reading entity state
    READ ENTITY zpra_mf_r_musicfestival
    FIELDS ( Uuid Status ) WITH CORRESPONDING #( entity_keys )
    RESULT DATA(read_result).

    act = VALUE #( FOR t IN read_result ( Uuid          = t-Uuid
                                          %param-Status = t-Status ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'Action Cancel - Status - Cancel - Read result' exp = exp act = act ).

  ENDMETHOD.

  METHOD generate_data.

 TYPES: BEGIN OF ty_entity_key,
             uuid TYPE sysuuid_x16,
           END OF ty_entity_key.


    DATA: entity_keys TYPE STANDARD TABLE OF ty_entity_key,
          lt_action  TYPE TABLE FOR ACTION IMPORT ZPRA_MF_R_MusicFestival~generateSampleData.

  lt_action = VALUE #( ( %cid = 'Root1' ) ).
  entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04211' ) ).


  MODIFY ENTITY zpra_mf_r_musicfestival
    EXECUTE generatesampledata FROM lt_action
    MAPPED   DATA(mapped_generate_sample_data)
    FAILED   DATA(failed_generate_sample_data)
    REPORTED DATA(reported_generate_sample_data).


    DATA(lv_visitor_count) = lines( mapped_generate_sample_data-visits ).
    DATA(lv_music_fest_count) = lines( mapped_generate_sample_data-musicfestival ).
*
    cl_abap_unit_assert=>assert_equals( msg = 'Action Generate Sample Data - Visitor count'  exp = 8  act = lv_visitor_count ).
    cl_abap_unit_assert=>assert_equals( msg = 'Action Generate Sample Data - Music Festival count'  exp = 5  act = lv_music_fest_count ).
  ENDMETHOD.

ENDCLASS.""",
    r"""CLASS ltcl_determination_methods DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_zpra_mf_r_musicfestival,               " the class to be tested
      cds_test_environment TYPE REF TO if_cds_test_environment,  " cds test double framework
      sql_test_environment TYPE REF TO if_osql_test_environment. " abap sql test double framework

    CLASS-METHODS:
      " setup test double framework
      class_setup,
      " stop test doubles
      class_teardown.

    METHODS:
      " reset test doubles
      setup,
      " rollback any changes
      teardown,

      determineStatus          FOR TESTING RAISING cx_static_check,
      determineAvailableSeats  FOR TESTING RAISING cx_static_check.
ENDCLASS.""",
    r"""CLASS ltcl_determination_methods IMPLEMENTATION.

  METHOD class_setup.

    " Create the class under Test
    " The class is abstract but can be constructed with the FOR TESTING
    CREATE OBJECT class_under_test FOR TESTING.
    " Create test doubles for database dependencies
    " The EML READ operation will then also access the test doubles
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds( i_for_entities = VALUE #(
        ( i_for_entity = 'ZPRA_MF_R_MUSICFESTIVAL' )
        ( i_for_entity = 'ZPRA_MF_R_VISITOR' )
        ( i_for_entity = 'ZPRA_MF_R_VISIT' ) ) ).
    cds_test_environment->enable_double_redirection( ).
    sql_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #(
        ( 'zpra_mf_d_mf' )
        ( 'zpra_mf_d_vst' )
        ( 'zpra_mf_d_vstr' ) ) ).

  ENDMETHOD.

  METHOD class_teardown.
    " stop mocking
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " clear the content of the test double per test
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    " Clean up any involved entity
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD determineStatus.

    DATA mf_mock_data TYPE STANDARD TABLE OF zpra_mf_a_mf.

    mf_mock_data = VALUE #( ( uuid = 'DEC190889AC21FE08191A45962D04211' title = 'Event 1' max_visitors_number = 2 free_visitor_seats = 2 )
                            ( uuid = 'DEC190889AC21FE08191A45962D04212' title = 'Event 2' max_visitors_number = 2 free_visitor_seats = 0 status = 'P' )
                            ( uuid = 'DEC190889AC21FE08191A45962D04213' title = 'Event 3' max_visitors_number = 4 free_visitor_seats = 2 status = 'F' ) ).

    " insert test data into the cds test doubles
    cds_test_environment->insert_test_data( i_data = mf_mock_data ).

    " call the method to be tested
    TYPES: BEGIN OF ty_entity_key,
             uuid TYPE sysuuid_x16,
           END OF ty_entity_key.


    DATA: reported    TYPE RESPONSE FOR REPORTED LATE zpra_mf_r_musicfestival,
          entity_keys TYPE STANDARD TABLE OF ty_entity_key.

    " specify test entity keys
    entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04211' ) ).

    " execute the determination
    class_under_test->determineStatus(
      EXPORTING
        keys     = CORRESPONDING #( entity_keys )
      CHANGING
        reported = reported
    ).

    cl_abap_unit_assert=>assert_initial( msg = 'reported' act = reported ).

    " additionally check by reading entity state
    READ ENTITY zpra_mf_r_musicfestival
    FIELDS ( Uuid Status ) WITH CORRESPONDING #( entity_keys )
    RESULT DATA(lt_read_status).


    " expect input keys and output keys to be same and Status
    DATA exp_inprogress LIKE lt_read_status.
    exp_inprogress = VALUE #(  ( Uuid = 'DEC190889AC21FE08191A45962D04211' Status = 'I' ) ).


    " current result; copy only fields of interest - i.e. Uuid and Status
    DATA act_inprogress LIKE lt_read_status.

    act_inprogress = CORRESPONDING #( lt_read_status MAPPING Uuid = Uuid
                                 Status      = Status
                                EXCEPT * ).

    cl_abap_unit_assert=>assert_equals( msg = 'Status Determination - Status - In Progress' exp = exp_inprogress act = act_inprogress ).

  ENDMETHOD.

  METHOD determineAvailableSeats.

    TYPES: BEGIN OF ty_entity_key,
             uuid TYPE sysuuid_x16,
           END OF ty_entity_key.

    DATA: reported             TYPE RESPONSE FOR REPORTED  zpra_mf_r_musicfestival,
          failed               TYPE RESPONSE FOR FAILED  zpra_mf_r_musicfestival,
          mapped               TYPE RESPONSE FOR MAPPED  zpra_mf_r_musicfestival,
          lt_music_festival TYPE TABLE FOR READ RESULT  zpra_mf_r_musicfestival,
          entity_keys          TYPE STANDARD TABLE OF ty_entity_key.

    MODIFY ENTITIES OF zpra_mf_r_musicfestival
     ENTITY MusicFestival
       CREATE SET FIELDS WITH
         VALUE #(     ( %cid              = 'ROOT1'
                        MaxVisitorsNumber = 2
                        FreeVisitorSeats  = 0
                  ) )
       CREATE BY \_Visits SET FIELDS WITH
         VALUE #(     ( %cid_ref          = 'ROOT1'
                        %target           = VALUE #( (
                                     %cid        = 'VISITS1'
                                     VisitorUuid = 'DEC190889AC21FE08191A45962D04210'

                                     ) )
                  ) )
       ENTITY Visits
        EXECUTE book
        FROM VALUE #( ( %cid_ref          = 'VISITS1' ) )

        MAPPED mapped
        FAILED failed
        REPORTED reported.


    LOOP AT mapped-musicfestival ASSIGNING FIELD-SYMBOL(<fs_mapped>).
      entity_keys = VALUE #( (  uuid =  <fs_mapped>-uuid ) ).

      READ ENTITIES OF zpra_mf_r_musicfestival IN LOCAL MODE
      ENTITY MusicFestival
      FIELDS ( FreeVisitorSeats )
      WITH CORRESPONDING #( entity_keys )
      RESULT lt_music_festival.
    ENDLOOP.
    READ TABLE lt_music_festival ASSIGNING FIELD-SYMBOL(<ls_music_festival>) INDEX 1.
    cl_abap_unit_assert=>assert_equals( msg = 'Free Seats calculated for an event' exp = 1 act = <ls_music_festival>-FreeVisitorSeats ).
  ENDMETHOD.


ENDCLASS.""",
    r"""CLASS ltc_authorization_methods DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_zpra_mf_r_musicfestival,               " the class to be tested
      cds_test_environment TYPE REF TO if_cds_test_environment,  " cds test double framework
      sql_test_environment TYPE REF TO if_osql_test_environment. " abap sql test double framework

    CLASS-METHODS:
      " setup test double framework
      class_setup,
      " stop test doubles
      class_teardown.

    METHODS:
      " reset test doubles
      setup,
      " roll back any changes
      teardown,
      get_global_authorizations FOR TESTING,
      get_instance_authorizations FOR TESTING.

    TYPES s_reported_early TYPE RESPONSE FOR REPORTED EARLY ZPRA_MF_R_MusicFestival.
    TYPES s_failed_early TYPE RESPONSE FOR FAILED EARLY ZPRA_MF_R_MusicFestival.

    DATA : ms_reported_early TYPE s_reported_early.
    DATA : ms_failed_early   TYPE s_failed_early.
ENDCLASS.""",
    r"""CLASS ltc_authorization_methods IMPLEMENTATION.


  METHOD class_setup.
    " Create the class under Test
    " The class is abstract but can be constructed with the FOR TESTING
    CREATE OBJECT class_under_test FOR TESTING.
    " Create test doubles for database dependencies
    " The EML READ operation will then also access the test doubles
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds( i_for_entities = VALUE #(
        ( i_for_entity = 'ZPRA_MF_R_MUSICFESTIVAL' )
        ( i_for_entity = 'ZPRA_MF_R_VISITOR' )
        ( i_for_entity = 'ZPRA_MF_R_VISIT' ) ) ).
    cds_test_environment->enable_double_redirection( ).
    sql_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #(
        ( 'zpra_mf_d_mf' )
        ( 'zpra_mf_d_vst' )
        ( 'zpra_mf_d_vstr' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    " stop mocking
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " clear the content of the test double per test
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    " Clean up any involved entity
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD get_global_authorizations.
    DATA ls_requested_authorizations TYPE STRUCTURE FOR GLOBAL AUTHORIZATION REQUEST ZPRA_MF_R_MusicFestival\\MusicFestival.
    DATA lt_result TYPE STRUCTURE FOR GLOBAL AUTHORIZATION RESULT ZPRA_MF_R_MusicFestival\\MusicFestival.

    ls_requested_authorizations-%delete = if_abap_behv=>mk-on.

    class_under_test->get_global_authorizations(
        EXPORTING
            requested_authorizations = ls_requested_authorizations
        CHANGING
            result   = lt_result
            reported = ms_reported_early ).

  ENDMETHOD.

  METHOD get_instance_authorizations.


    DATA lt_entity_keys TYPE TABLE FOR AUTHORIZATION KEY ZPRA_MF_R_MusicFestival\\MusicFestival.
    DATA ls_requested_authorizations TYPE STRUCTURE FOR AUTHORIZATION REQUEST ZPRA_MF_R_MusicFestival\\MusicFestival.
    DATA lt_result TYPE TABLE FOR AUTHORIZATION RESULT ZPRA_MF_R_MusicFestival\\MusicFestival.

    " specify test entity keys
    lt_entity_keys = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04218' ) ).

    lt_result = VALUE #( (  uuid = 'DEC190889AC21FE08191A45962D04218'  ) ).

    class_under_test->get_instance_authorizations(
      EXPORTING
        keys                     = lt_entity_keys
        requested_authorizations = ls_requested_authorizations
      CHANGING
        result                   = lt_result
        failed                   = ms_failed_early
        reported                 = ms_reported_early
    ).
  ENDMETHOD.

ENDCLASS.""",
    r"""class ZBP_PRA_MF_R_VISITOR definition
  public
  abstract
  final
  for behavior of ZPRA_MF_R_VISITOR .

public section.
protected section.
private section.
ENDCLASS.



CLASS ZBP_PRA_MF_R_VISITOR IMPLEMENTATION.
ENDCLASS.""",
    r"""CLASS zbp_pra_mf_r_visits DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zpra_mf_r_musicfestival.
ENDCLASS.



CLASS ZBP_PRA_MF_R_VISITS IMPLEMENTATION.
ENDCLASS.""",
    r"""CLASS ltcl_methods DEFINITION DEFERRED FOR TESTING.

CLASS lhc_ZPRA_MF_BP_R_Visits DEFINITION INHERITING FROM cl_abap_behavior_handler
FRIENDS ltcl_methods .

  PRIVATE SECTION.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Visits RESULT result.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Visits RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR visits RESULT result.

    "Actions
    METHODS book FOR MODIFY
      IMPORTING keys FOR ACTION Visits~book RESULT result.
    METHODS cancel FOR MODIFY
      IMPORTING keys FOR ACTION Visits~cancel RESULT result.

    "Determinations
    METHODS determineStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Visits~determineStatus.
    METHODS determineAvailableSeats FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Visits~determineAvailableSeats.

ENDCLASS.""",
    r"""CLASS lhc_ZPRA_MF_BP_R_Visits IMPLEMENTATION.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_instance_features.

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY Visits
         FIELDS ( Status )
         WITH CORRESPONDING #( keys )
      RESULT DATA(visits)
      FAILED failed.

    result = VALUE #( FOR visit IN visits
                      ( %tky   = visit-%tky

                        %action-book = COND #( WHEN visit-%is_draft = if_abap_behv=>mk-on
                                               THEN COND #( WHEN visit-Status = zcl_pra_mf_enum_visit_status=>booked
                                                            THEN if_abap_behv=>fc-o-disabled
                                                            ELSE if_abap_behv=>fc-o-enabled )
                                               ELSE if_abap_behv=>fc-o-disabled )

                       %action-cancel = COND #( WHEN visit-%is_draft = if_abap_behv=>mk-on
                                                THEN COND #( WHEN visit-Status = zcl_pra_mf_enum_visit_status=>cancelled
                                                             THEN if_abap_behv=>fc-o-disabled
                                                             ELSE if_abap_behv=>fc-o-enabled )
                                               ELSE if_abap_behv=>fc-o-disabled )

                       %delete       = COND #( WHEN ( visit-Status = zcl_pra_mf_enum_visit_status=>cancelled
                                                   OR visit-Status = zcl_pra_mf_enum_visit_status=>pending )
                                               THEN if_abap_behv=>fc-o-enabled
                                               ELSE if_abap_behv=>fc-o-disabled ) ) ).

  ENDMETHOD.

  METHOD book.
    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
    ENTITY Visits
       FIELDS ( Status )
       WITH CORRESPONDING #( keys )
    RESULT DATA(visits).

    CHECK lines( visits ) > 0.

    MODIFY ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY Visits
        UPDATE FIELDS ( Status )
        WITH VALUE #( FOR visit IN visits
                      ( %tky   = visit-%tky
                        Status = zcl_pra_mf_enum_visit_status=>booked ) ).

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
    ENTITY Visits
       FIELDS ( Status )
       WITH CORRESPONDING #( keys )
    RESULT DATA(visits_after_update).

    result = VALUE #( FOR updated_visit IN visits_after_update
                      ( %tky   = updated_visit-%tky
                        %param = updated_visit ) ).


  ENDMETHOD.

  METHOD cancel.
    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
   ENTITY Visits
      FIELDS ( Status )
      WITH CORRESPONDING #( keys )
   RESULT DATA(visits).

    CHECK lines( visits ) > 0.

    MODIFY ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY Visits
        UPDATE FIELDS ( Status )
        WITH VALUE #( FOR visit IN visits
                      ( %tky   = visit-%tky
                        Status = zcl_pra_mf_enum_visit_status=>cancelled ) ).

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
    ENTITY Visits
       FIELDS ( Status )
       WITH CORRESPONDING #( keys )
    RESULT DATA(visits_after_update).

    result = VALUE #( FOR updated_visit IN visits_after_update
                      ( %tky   = updated_visit-%tky
                        %param = updated_visit ) ).

  ENDMETHOD.

  METHOD determineStatus.
    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY Visits
         FIELDS ( Status )
         WITH CORRESPONDING #( keys )
      RESULT DATA(visits).

    DELETE visits WHERE Status IS NOT INITIAL.

    CHECK lines( visits ) > 0.

    MODIFY ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY Visits
        UPDATE FIELDS ( Status )
        WITH VALUE #( FOR visit IN visits
                      ( %tky   = visit-%tky
                        Status = zcl_pra_mf_enum_visit_status=>pending ) ).

  ENDMETHOD.

  METHOD determineAvailableSeats.

    READ ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
        ENTITY Visits
           FIELDS ( ParentUuid Status )
           WITH CORRESPONDING #( keys )
        RESULT DATA(visits).

    DELETE visits WHERE Status = zcl_pra_mf_enum_visit_status=>pending.
    CHECK lines( visits ) > 0.

    SORT visits ASCENDING BY ParentUuid.
    DELETE ADJACENT DUPLICATES FROM visits COMPARING ParentUuid.

    MODIFY ENTITIES OF ZPRA_MF_R_MusicFestival IN LOCAL MODE
      ENTITY MusicFestival
        EXECUTE calculateFreeVisitorSeats
        FROM VALUE #( FOR visit IN visits
                      ( uuid      = visit-ParentUuid
                        %is_draft = visit-%is_draft ) ).
  ENDMETHOD.


ENDCLASS.""",
    r"""CLASS ltcl_methods DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      class_under_test     TYPE REF TO lhc_ZPRA_MF_BP_R_Visits,               " the class to be tested
      cds_test_environment TYPE REF TO if_cds_test_environment,  " cds test double framework
      sql_test_environment TYPE REF TO if_osql_test_environment. " abap sql test double framework

    CLASS-METHODS:
      " setup test double framework
      class_setup,
      " stop test doubles
      class_teardown.

    METHODS:
      " reset test doubles
      setup,
      " rollback any changes
      teardown,
      determineStatus         FOR TESTING RAISING cx_static_check,
      determineAvailableSeats FOR TESTING RAISING cx_static_check,
      actionBook              FOR TESTING RAISING cx_static_check,
      actionCancel            FOR TESTING RAISING cx_static_check.

ENDCLASS.""",
    r"""CLASS ltcl_methods IMPLEMENTATION.

  METHOD class_setup.
    CREATE OBJECT class_under_test FOR TESTING.
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds( i_for_entities = VALUE #(
        ( i_for_entity = 'ZPRA_MF_R_MUSICFESTIVAL' )
        ( i_for_entity = 'ZPRA_MF_R_VISITOR' )
        ( i_for_entity = 'ZPRA_MF_R_VISIT' ) ) ).
    cds_test_environment->enable_double_redirection( ).
    sql_test_environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #(
        ( 'zpra_mf_d_mf' )
        ( 'zpra_mf_d_vst' )
        ( 'zpra_mf_d_vstr' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    " stop mocking
    cds_test_environment->destroy( ).
    sql_test_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " clear the content of the test double per test
    cds_test_environment->clear_doubles( ).
    sql_test_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    " Clean up any involved entity
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD determineAvailableSeats.

  ENDMETHOD.

  METHOD determineStatus.

  ENDMETHOD.

  METHOD actionBook.

  ENDMETHOD.

  METHOD actionCancel.
  
  ENDMETHOD.

ENDCLASS.""",
    r"""CLASS zcl_pra_mf_calc_mf_elements DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_sadl_exit_calc_element_read.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS calculate_event_status_ind
      IMPORTING status                    TYPE zpra_mf_c_musicfestivaltp-status
      RETURNING VALUE(status_criticality) TYPE zpra_mf_c_musicfestivaltp-statuscriticality.
ENDCLASS.""",
    r"""CLASS zcl_pra_mf_calc_mf_elements IMPLEMENTATION.

  METHOD if_sadl_exit_calc_element_read~calculate.

    DATA events TYPE STANDARD TABLE OF ZPRA_MF_C_MusicFestivalTP WITH DEFAULT KEY.
    events = CORRESPONDING #( it_original_data ).

    LOOP AT it_requested_calc_elements REFERENCE INTO DATA(req_calc_elements).

      CASE req_calc_elements->*.

        WHEN 'BOOKEDSEATS'.

          LOOP AT events REFERENCE INTO DATA(event).
            event->BookedSeats = event->MaxVisitorsNumber - event->FreeVisitorSeats.
          ENDLOOP.

        WHEN 'STATUSCRITICALITY'.

          LOOP AT events REFERENCE INTO event.
            event->StatusCriticality = zcl_pra_mf_calc_mf_elements=>calculate_event_status_ind( event->status ).
          ENDLOOP.

      ENDCASE.
    ENDLOOP.

    ct_calculated_data = CORRESPONDING #( events ).

  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~get_calculation_info.

    CLEAR: et_requested_orig_elements.

    IF iv_entity EQ `ZPRA_MF_C_MUSICFESTIVALTP`.
      IF line_exists( it_requested_calc_elements[ table_line = `BOOKEDSEATS` ] ).
        INSERT `MAXVISITORSNUMBER` INTO TABLE et_requested_orig_elements.
        INSERT `FREEVISITORSEATS` INTO TABLE et_requested_orig_elements.
      ENDIF.

      IF line_exists( it_requested_calc_elements[ table_line = `STATUSCRITICALITY` ] ).
        INSERT `STATUS` INTO TABLE et_requested_orig_elements.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD calculate_event_status_ind.

    CASE status.
      WHEN zcl_pra_mf_enum_mf_status=>cancelled.
        status_criticality = zcl_pra_mf_enum_criticality=>negative.
      WHEN zcl_pra_mf_enum_mf_status=>fully_booked.
        status_criticality = zcl_pra_mf_enum_criticality=>critical.
      WHEN zcl_pra_mf_enum_mf_status=>published.
        status_criticality = zcl_pra_mf_enum_criticality=>positive.
      WHEN OTHERS.
        status_criticality = zcl_pra_mf_enum_criticality=>neutral.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.""",
    r"""CLASS zcl_pra_mf_calc_visit_elements DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_sadl_exit_calc_element_read.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS calculate_status_criticality
      IMPORTING status                    TYPE zpra_mf_c_visittp-status
      RETURNING VALUE(status_criticality) TYPE zpra_mf_c_visittp-statuscriticality.
ENDCLASS.""",
    r"""CLASS zcl_pra_mf_calc_visit_elements IMPLEMENTATION.

  METHOD if_sadl_exit_calc_element_read~calculate.

    DATA visits TYPE STANDARD TABLE OF zpra_mf_c_visittp WITH DEFAULT KEY.
    visits = CORRESPONDING #( it_original_data ).

    LOOP AT it_requested_calc_elements REFERENCE INTO DATA(req_calc_elements).

      CASE req_calc_elements->*.

        WHEN 'STATUSCRITICALITY'.

          LOOP AT visits REFERENCE INTO DATA(visit).
            visit->StatusCriticality = zcl_pra_mf_calc_visit_elements=>calculate_status_criticality( visit->status ).
          ENDLOOP.

      ENDCASE.

    ENDLOOP.

    ct_calculated_data = CORRESPONDING #( visits ).

  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~get_calculation_info.

    IF iv_entity EQ 'ZPRA_MF_C_VISITTP'.
      LOOP AT it_requested_calc_elements ASSIGNING FIELD-SYMBOL(<requested_calc_elements>).
        CASE <requested_calc_elements>.
          WHEN 'STATUSCRITICALITY'.
            APPEND 'STATUS' TO et_requested_orig_elements.
        ENDCASE.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD calculate_status_criticality.

    CASE status.
      WHEN zcl_pra_mf_enum_visit_status=>cancelled.
        status_criticality = zcl_pra_mf_enum_criticality=>negative.
      WHEN zcl_pra_mf_enum_visit_status=>pending.
        status_criticality = zcl_pra_mf_enum_criticality=>critical.
      WHEN zcl_pra_mf_enum_visit_status=>booked.
        status_criticality = zcl_pra_mf_enum_criticality=>positive.
      WHEN OTHERS.
        status_criticality = zcl_pra_mf_enum_criticality=>neutral.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.""",
    r"""CLASS zcl_pra_mf_ent_proj_outb_integ DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES BEGIN OF ty_business_data.
    INCLUDE TYPE zcl_pra_mf_scm_ent_proj=>tys_a_enterprise_project_type.
    TYPES to_enterprise_project_el_2 TYPE zcl_pra_mf_scm_ent_proj=>tyt_a_enterprise_project_ele_2.
    TYPES END OF ty_business_data.

    METHODS get_clientproxy
      EXPORTING
                et_message             TYPE bapirettab
      RETURNING VALUE(ro_client_proxy) TYPE REF TO /iwbep/if_cp_client_proxy.

    METHODS create_entproject
      IMPORTING
        Is_project_data TYPE zcl_pra_mf_scm_ent_proj=>tys_a_enterprise_project_type
      EXPORTING
        es_project_data TYPE zcl_pra_mf_scm_ent_proj=>tys_a_enterprise_project_type
        et_message      TYPE bapirettab.

    INTERFACES if_oo_adt_classrun .
    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_PRA_MF_ENT_PROJ_OUTB_INTEG IMPLEMENTATION.


  METHOD create_entproject.

    TYPES BEGIN OF ty_business_data.
    INCLUDE TYPE zcl_pra_mf_scm_ent_proj=>tys_a_enterprise_project_type.
    TYPES to_enterprise_project_el_2 TYPE zcl_pra_mf_scm_ent_proj=>tyt_a_enterprise_project_ele_2.
    TYPES END OF ty_business_data.

    DATA:
      lo_http_client    TYPE REF TO if_web_http_client,
      lt_header_pro     TYPE TABLE OF string,
      ls_business_data  TYPE ty_business_data,
      lo_client_proxy   TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request        TYPE REF TO /iwbep/if_cp_request_create,
      lo_response       TYPE REF TO /iwbep/if_cp_response_create,
      lo_resource       TYPE REF TO /iwbep/if_cp_resource_entity,
      lo_filter_factory TYPE REF TO /iwbep/if_cp_filter_factory,
      lt_gl_pro         TYPE TABLE OF string,
      lo_filter_node_1  TYPE REF TO /iwbep/if_cp_filter_node.


    APPEND 'PROFIT_CENTER'               TO lt_header_pro.
    APPEND 'PROJECT'                     TO lt_header_pro.
    APPEND 'PROJECT_DESCRIPTION'         TO lt_header_pro.
    APPEND 'PROJECT_END_DATE'            TO lt_header_pro.
    APPEND 'PROJECT_PROFILE_CODE'        TO lt_header_pro.
    APPEND 'PROJECT_START_DATE'          TO lt_header_pro.
    APPEND 'RESPONSIBLE_COST_CENTER'     TO lt_header_pro.
    APPEND 'PROJECT_ELEMENT'               TO lt_gl_pro.
    APPEND 'PROJECT_ELEMENT_DESCRIPT_2'    TO lt_gl_pro.
    APPEND 'PLANNED_START_DATE'            TO lt_gl_pro.
    APPEND 'PLANNED_END_DATE'              TO lt_gl_pro.

    ls_business_data = VALUE #( profit_center               = 'YB900'
                                project                     = |MF_| && |{ is_project_data-project }|
                                project_description         = is_project_data-project_description
                                project_end_date            = is_project_data-project_end_date
                                project_profile_code        = 'YP02'
                                project_start_date          = is_project_data-project_start_date
                                responsible_cost_center     = 'CC_CON1' ).

    TRY.

        lo_request = get_clientproxy( )->create_resource_for_entity_set( 'A_ENTERPRISE_PROJECT' )->create_request_for_create( ).
        DATA(lo_data_description_node) = lo_request->create_data_descripton_node( ).

        lo_data_description_node->set_properties( lt_header_pro  ).

        DATA(lo_item_child) = lo_data_description_node->add_child( 'TO_ENTERPRISE_PROJECT_EL_2' ).
        lo_item_child->set_properties( lt_gl_pro ).
        lo_request->set_deep_business_data( is_business_data = ls_business_data
                                            io_data_description = lo_data_description_node ).

      CATCH /iwbep/cx_gateway INTO DATA(lo_exception).
        et_message = VALUE #( ( type = 'E' id = 'ZPRA_MF_MSG_CLS' number = '009' ) ).
    ENDTRY.

    TRY.
        " Execute the request
        lo_response = lo_request->execute( ).

      CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
        et_message = VALUE #( ( type = 'E' id = 'ZPRA_MF_MSG_CLS' number = '009' ) ).
      CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
        et_message = VALUE #( ( type = 'E' id = 'ZPRA_MF_MSG_CLS' number = '009' ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_clientproxy.

    DATA:
      lo_http_client  TYPE REF TO if_web_http_client,
      lo_client_proxy TYPE REF TO /iwbep/if_cp_client_proxy.

    TRY.
        "  Get the destination of remote system; Create http client
        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                                    comm_scenario  = 'ZPRA_MF_CS_ENT_PROJ'
                                                    comm_system_id = 'TEST_SAP_COM_0308_PRA_2'
                                                     service_id     = 'ZPRA_MF_OUT_ENT_PROJ_REST'
    ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).

        "create client proxy
        ro_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
          EXPORTING is_proxy_model_key       = VALUE #( repository_id       = 'DEFAULT'
                                                        proxy_model_id      = 'ZCL_PRA_MF_SCM_ENT_PROJ'
                                                        proxy_model_version = '001' )
                    io_http_client             = lo_http_client
                    iv_relative_service_root   = '/sap/opu/odata/sap/API_ENTERPRISE_PROJECT_SRV;v=0002/'  " = the service endpoint in the service binding in PRV' ).
                    ).

      CATCH cx_http_dest_provider_error INTO DATA(lx_prov_error).
        et_message = VALUE #( ( type = 'E' id = 'ZPRA_MF_MSG_CLS' number = '009' ) ).
      CATCH /iwbep/cx_gateway INTO DATA(lx_cx_gateway).
        et_message = VALUE #( ( type = 'E' id = 'ZPRA_MF_MSG_CLS' number = '009' ) ).
      CATCH cx_web_http_client_error INTO DATA(lx_http_client).
        et_message = VALUE #( ( type = 'E' id = 'ZPRA_MF_MSG_CLS' number = '009' ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DATA ls_project_data TYPE zcl_pra_mf_scm_ent_proj=>tys_a_enterprise_project_type.

        create_entproject( EXPORTING is_project_data = ls_project_data IMPORTING es_project_data  = ls_project_data ) .

  ENDMETHOD.


  METHOD if_rap_query_provider~select.
  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_pra_mf_enum_criticality DEFINITION PUBLIC ABSTRACT FINAL.

  PUBLIC SECTION.
    CONSTANTS:
      neutral  TYPE int4 VALUE 0,
      negative TYPE int4 VALUE 1,
      critical TYPE int4 VALUE 2,
      positive TYPE int4 VALUE 3.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_pra_mf_enum_criticality IMPLEMENTATION.
ENDCLASS.""",
    r"""CLASS zcl_pra_mf_enum_mf_status DEFINITION PUBLIC ABSTRACT FINAL.

  PUBLIC SECTION.
    CONSTANTS:
      in_preparation TYPE ZPRA_MF_C_MusicFestivalTP-Status VALUE 'I',
      cancelled      TYPE ZPRA_MF_C_MusicFestivalTP-Status VALUE 'C',
      published      TYPE ZPRA_MF_C_MusicFestivalTP-Status VALUE 'P',
      fully_booked   TYPE ZPRA_MF_C_MusicFestivalTP-Status VALUE 'F'.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_pra_mf_enum_mf_status IMPLEMENTATION.
ENDCLASS.""",
    r"""CLASS zcl_pra_mf_enum_visit_status DEFINITION PUBLIC ABSTRACT FINAL.

  PUBLIC SECTION.
    CONSTANTS:
      pending   TYPE ZPRA_MF_C_VisitTP-Status VALUE 'P',
      cancelled TYPE ZPRA_MF_C_VisitTP-Status VALUE 'C',
      booked    TYPE ZPRA_MF_C_VisitTP-Status VALUE 'B'.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_pra_mf_enum_visit_status IMPLEMENTATION.
ENDCLASS.""",
    r"""CLASS zcl_pra_mf_fetch_proj DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS zcl_pra_mf_fetch_proj IMPLEMENTATION.


  METHOD if_rap_query_provider~select.

    TYPES : BEGIN OF lty_data,
              Projectid   TYPE c LENGTH 24,
              ProjectName TYPE c LENGTH 40,
              StartDate   TYPE c LENGTH 8,
              EndDate     TYPE c LENGTH 8,
              CostCenter  TYPE c LENGTH 10,
              Status      TYPE c LENGTH 10,
              Nav         TYPE c LENGTH 120,
            END OF lty_data.

    TYPES : BEGIN OF lty_range,
              sign   TYPE c LENGTH 1,
              option TYPE c LENGTH 2,
              low    TYPE c LENGTH 24,
              high   TYPE c LENGTH 24,
            END OF lty_range,
            ltt_range TYPE STANDARD TABLE OF lty_range WITH EMPTY KEY.

    DATA:
      lo_http_client  TYPE REF TO if_web_http_client,
      lo_client_proxy TYPE REF TO /iwbep/if_cp_client_proxy,
      lt_data         TYPE TABLE OF zcl_pra_mf_scm_ent_proj=>tys_a_enterprise_project_type,
      lt_data1        TYPE TABLE OF lty_data,
      lo_request      TYPE REF TO /iwbep/if_cp_request_read_list,
      lr_cscn         TYPE if_com_scenario_factory=>ty_query-cscn_id_range.

    CONSTANTS : lc_project TYPE string VALUE 'ui#EnterpriseProject-planProject?EnterpriseProject='.

    CLEAR : lr_cscn.
    " find Communication Arrangement by scenario ID
    lr_cscn = VALUE #( ( sign = 'I' option = 'EQ' low = 'ZPRA_MF_CS_ENT_PROJ' ) ).

    DATA(lo_factory) = cl_com_arrangement_factory=>create_instance( ).lo_factory->query_ca(
  EXPORTING
    is_query = VALUE #( cscn_id_range = lr_cscn )
  IMPORTING
    et_com_arrangement = DATA(lt_ca) ).

    IF lt_ca IS INITIAL.
      io_response->set_data( it_data = lt_data1 ).
      io_response->set_total_number_of_records( 0 ).
      EXIT.
    ELSE.
      READ TABLE lt_ca ASSIGNING FIELD-SYMBOL(<fs_ca>) INDEX 1.
      IF sy-subrc EQ 0.
        DATA(lt_inb_services)  = <fs_ca>->get_inbound_services( ).
        DATA(lt_outb_services) = <fs_ca>->get_outbound_services( ).

        IF lt_outb_services IS NOT INITIAL.
          DATA(lv_url) = lt_outb_services[ 1 ]-url.
          lv_url = lv_url && lc_project.
        ENDIF.
      ENDIF.

    ENDIF.

    TRY.
        "  Get the destination of remote system; Create http client
        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                                    comm_scenario  = 'ZPRA_MF_CS_ENT_PROJ'
                                                    comm_system_id = 'TEST_SAP_COM_0308_PRA_2'
                                                     service_id     = 'ZPRA_MF_OUT_ENT_PROJ_REST'
    ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).

        "create client proxy
        lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
          EXPORTING is_proxy_model_key       = VALUE #( repository_id       = 'DEFAULT'
                                                        proxy_model_id      = 'ZCL_PRA_MF_SCM_ENT_PROJ'
                                                        proxy_model_version = '001' )
                    io_http_client             = lo_http_client
                    iv_relative_service_root   = '/sap/opu/odata/sap/API_ENTERPRISE_PROJECT_SRV;v=0002/'  " = the service endpoint in the service binding in PRV' ).
                    ).

      CATCH cx_http_dest_provider_error INTO DATA(lx_prov_error).
        MESSAGE e009(zpra_mf_msg_cls) INTO DATA(lv_msg).
      CATCH /iwbep/cx_gateway INTO DATA(lx_cx_gateway).
        MESSAGE e009(zpra_mf_msg_cls) INTO lv_msg.
      CATCH cx_web_http_client_error INTO DATA(lx_http_client).
        MESSAGE e009(zpra_mf_msg_cls) INTO lv_msg.
    ENDTRY.

    TRY.
        lo_request = lo_client_proxy->create_resource_for_entity_set( 'A_ENTERPRISE_PROJECT' )->create_request_for_read( ).

        lo_request->set_filter(
                  io_filter_node = lo_request->create_filter_factory( )->create_by_range(
                    iv_property_path = 'PROJECT'
                    it_range         = VALUE ltt_range( ( sign = 'I' option = 'CP' low = 'MF_*' high = '' ) )
                  )
                ).

        lo_request->execute( ).


        DATA(lo_response) = lo_request->get_response( ).

        lo_response->get_business_data( IMPORTING et_business_data = lt_data ).

      CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
        MESSAGE e009(zpra_mf_msg_cls) INTO lv_msg.
      CATCH /iwbep/cx_gateway INTO DATA(lo_exception).
        MESSAGE e009(zpra_mf_msg_cls) INTO lv_msg.
    ENDTRY.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
      lt_data1 = VALUE #( BASE lt_data1 (
                                          projectid = <lfs_data>-project
                                          projectname = <lfs_data>-project_description
                                          startdate = <lfs_data>-project_start_date
                                          enddate = <lfs_data>-project_end_date
                                          costcenter = <lfs_data>-responsible_cost_center
                                          status = COND #( WHEN <lfs_data>-processing_status EQ '00' THEN 'Created' )
                                          Nav = lv_url && <lfs_data>-project

                                           ) ).
    ENDLOOP.
    io_response->set_data( it_data = lt_data1 ).
    io_response->set_total_number_of_records( CONV int8( lines( lt_data1 ) ) ).

  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcm_pra_mf_messages DEFINITION
  PUBLIC
     INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .
    INTERFACES if_abap_behv_message .

    CONSTANTS:
      gc_msgid TYPE symsgid VALUE 'ZPRA_MF_MSG_CLS',

      BEGIN OF state_area,
        validate_event    TYPE string VALUE 'VALIDATE_EVENT',
        validate_visitors TYPE String VALUE 'VALIDATE_VISITORS',
        validate_date     TYPE string VALUE 'VALIDATE_DATE',
        validate_publish_action TYPE string VALUE 'VALIDATE_PUBLISH_ACTION',
      END OF state_area,

      BEGIN OF max_visitor_zero_negative,
        msgid TYPE symsgid VALUE 'ZPRA_MF_MSG_CLS',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF max_visitor_zero_negative,

      BEGIN OF max_visitors_less_than_booked,
        msgid TYPE symsgid VALUE 'ZPRA_MF_MSG_CLS',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF max_visitors_less_than_booked,

      BEGIN OF event_datetime_invalid,
        msgid TYPE symsgid VALUE 'ZPRA_MF_MSG_CLS',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF event_datetime_invalid,

      BEGIN OF event_mandatory_value_missing,
        msgid TYPE symsgid VALUE 'ZPRA_MF_MSG_CLS',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF event_mandatory_value_missing,

      BEGIN OF event_already_published,
        msgid TYPE symsgid VALUE 'ZPRA_MF_MSG_CLS',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'TITLE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF event_already_published,

      BEGIN OF error_in_proj_creation,
        msgid TYPE symsgid VALUE 'ZPRA_MF_MSG_CLS',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'TITLE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_in_proj_creation.


    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        attr1    TYPE string OPTIONAL
        attr2    TYPE string OPTIONAL
        attr3    TYPE string OPTIONAL
        attr4    TYPE string OPTIONAL
        title    TYPE zpra_mf_title OPTIONAL
        previous LIKE previous OPTIONAL
        severity TYPE if_abap_behv_message=>t_severity OPTIONAL
        uname    TYPE syuname OPTIONAL.

    DATA:
      mv_attr1 TYPE string,
      mv_attr2 TYPE string,
      mv_attr3 TYPE string,
      mv_attr4 TYPE string,
      title TYPE zpra_mf_title,
      mv_uname TYPE syuname.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS zcm_pra_mf_messages IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    me->mv_attr1                 = attr1.
    me->mv_attr2                 = attr2.
    me->mv_attr3                 = attr3.
    me->mv_attr4                 = attr4.
    me->mv_uname                 = uname.
    me->title                    = title.

    if_abap_behv_message~m_severity = severity.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_pra_mf_scope_pg_sp_tmplt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_PRA_MF_SCOPE_PG_SP_TMPLT IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DATA(lo_scope_api) = cl_aps_bc_scope_change_api=>create_instance( ).

    lo_scope_api->scope(
      EXPORTING
        it_object_scope = VALUE #(
                            pgmid = if_aps_bc_scope_change_api=>gc_tadir_pgmid-r3tr
                            scope_state = if_aps_bc_scope_change_api=>gc_scope_state-on

                            " Space template
                            ( object = if_aps_bc_scope_change_api=>gc_tadir_object-uist obj_name = 'ZPRA_MF_LST' )

                            " Page template
                            ( object = if_aps_bc_scope_change_api=>gc_tadir_object-uipg obj_name = 'ZPRA_MF_LPT' )

                          )
        iv_simulate = abap_false
        iv_force = abap_false
      IMPORTING et_object_result = DATA(lt_results)
        et_message = DATA(lt_messages)
    ).

    " Check if the operation was successful
    IF lt_messages IS NOT INITIAL.
      " Print success message
      out->write( 'Scope operation completed successfully.' ) ##NO_TEXT.
    ELSE.
      " Optionally handle errors
      out->write( 'Scope operation encountered errors.' ) ##NO_TEXT.
    ENDIF.

  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_gtt_ae_factory DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_gtt_ae_factory .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_AE_FACTORY IMPLEMENTATION.


  METHOD zif_gtt_ae_factory~get_ae_filler.
  ENDMETHOD.


  METHOD zif_gtt_ae_factory~get_ae_parameters.
    ro_ae_parameters = NEW zcl_gtt_ae_parameters(
      iv_appsys               = iv_appsys
      is_event_type           = is_event_type
      it_all_appl_tables      = it_all_appl_tables
      it_event_type_cntl_tabs = it_event_type_cntl_tabs
      it_events               = it_events ).
  ENDMETHOD.


  METHOD zif_gtt_ae_factory~get_ae_processor.
    DATA: lo_ae_parameters TYPE REF TO zif_gtt_ae_parameters,
          lo_ae_filler     TYPE REF TO zif_gtt_ae_filler.

    lo_ae_parameters = zif_gtt_ae_factory~get_ae_parameters(
      iv_appsys               = iv_appsys
      is_event_type           = is_event_type
      it_all_appl_tables      = it_all_appl_tables
      it_event_type_cntl_tabs = it_event_type_cntl_tabs
      it_events               = it_events ).

    lo_ae_filler = zif_gtt_ae_factory~get_ae_filler(
      io_ae_parameters = lo_ae_parameters ).

    ro_ae_processor = NEW zcl_gtt_ae_processor(
      is_definition    = is_definition
      io_ae_parameters = lo_ae_parameters
      io_ae_filler     = lo_ae_filler ).
  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_gtt_ae_parameters DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_gtt_ae_parameters .

    METHODS constructor
      IMPORTING
        !iv_appsys               TYPE /saptrx/applsystem
        !is_event_type           TYPE /saptrx/evtypes
        !it_all_appl_tables      TYPE trxas_tabcontainer
        !it_event_type_cntl_tabs TYPE trxas_eventtype_tabs
        !it_events               TYPE trxas_evt_ctabs .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_appsys TYPE /saptrx/applsystem .
    DATA ms_event_type TYPE /saptrx/evtypes .
    DATA mr_all_appl_tables TYPE REF TO data .
    DATA mr_event_type_cntl_tabs TYPE REF TO data .
    DATA mr_events TYPE REF TO data .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_AE_PARAMETERS IMPLEMENTATION.


  METHOD constructor.

    mv_appsys               = iv_appsys.
    ms_event_type           = is_event_type.
    mr_all_appl_tables      = REF #( it_all_appl_tables ).
    mr_event_type_cntl_tabs = REF #( it_event_type_cntl_tabs ).
    mr_events               = REF #( it_events ).

  ENDMETHOD.


  METHOD zif_gtt_ae_parameters~get_appl_table.
    TRY.
        FIELD-SYMBOLS: <lt_all_appl_tables> TYPE trxas_tabcontainer.

        ASSIGN mr_all_appl_tables->* TO <lt_all_appl_tables>.

        rr_data   = <lt_all_appl_tables>[ tabledef = iv_tabledef ]-tableref.

      CATCH cx_sy_itab_line_not_found.
        MESSAGE e008(/saptrx/asc)
          WITH iv_tabledef
               ms_event_type-eventdatafunc
          INTO DATA(lv_dummy).

        zcl_gtt_tools=>throw_exception(
          iv_textid = zif_gtt_ef_constants=>cs_errors-stop_processing ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_gtt_ae_parameters~get_appsys.
    rv_appsys   = mv_appsys.
  ENDMETHOD.


  METHOD zif_gtt_ae_parameters~get_events.
    rr_data     = mr_events.
  ENDMETHOD.


  METHOD zif_gtt_ae_parameters~get_event_type.
    rs_event_type   = ms_event_type.
  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_gtt_ae_performer DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS check_relevance
      IMPORTING
        !is_definition           TYPE zif_gtt_ef_types=>ts_definition
        !io_ae_factory           TYPE REF TO zif_gtt_ae_factory
        !iv_appsys               TYPE /saptrx/applsystem
        !is_event_type           TYPE /saptrx/evtypes
        !it_all_appl_tables      TYPE trxas_tabcontainer
        !it_event_type_cntl_tabs TYPE trxas_eventtype_tabs OPTIONAL
        !is_events               TYPE trxas_evt_ctab_wa
      RETURNING
        VALUE(rv_result)         TYPE zif_gtt_ef_types=>tv_condition
      RAISING
        cx_udm_message .
    CLASS-METHODS get_event_data
      IMPORTING
        !is_definition           TYPE zif_gtt_ef_types=>ts_definition
        !io_ae_factory           TYPE REF TO zif_gtt_ae_factory
        !iv_appsys               TYPE /saptrx/applsystem
        !is_event_type           TYPE /saptrx/evtypes
        !it_all_appl_tables      TYPE trxas_tabcontainer
        !it_event_type_cntl_tabs TYPE trxas_eventtype_tabs
        !it_events               TYPE trxas_evt_ctabs
      CHANGING
        !ct_eventid_map          TYPE trxas_evtid_evtcnt_map
        !ct_trackingheader       TYPE zif_gtt_ae_types=>tt_trackingheader
        !ct_tracklocation        TYPE zif_gtt_ae_types=>tt_tracklocation
        !ct_trackreferences      TYPE zif_gtt_ae_types=>tt_trackreferences
        !ct_trackparameters      TYPE zif_gtt_ae_types=>tt_trackparameters
      RAISING
        cx_udm_message .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_AE_PERFORMER IMPLEMENTATION.


  METHOD check_relevance.

    DATA: lt_events TYPE trxas_evt_ctabs.

    lt_events = VALUE #( ( is_events ) ).

    DATA(lo_ae_processor) = io_ae_factory->get_ae_processor(
      is_definition           = is_definition
      io_ae_factory           = io_ae_factory
      iv_appsys               = iv_appsys
      is_event_type           = is_event_type
      it_all_appl_tables      = it_all_appl_tables
      it_event_type_cntl_tabs = it_event_type_cntl_tabs
      it_events               = lt_events ).

    lo_ae_processor->check_events( ).

    rv_result = lo_ae_processor->check_relevance( ).

  ENDMETHOD.


  METHOD get_event_data.

    DATA(lo_ae_processor) = io_ae_factory->get_ae_processor(
      is_definition           = is_definition
      io_ae_factory           = io_ae_factory
      iv_appsys               = iv_appsys
      is_event_type           = is_event_type
      it_all_appl_tables      = it_all_appl_tables
      it_event_type_cntl_tabs = it_event_type_cntl_tabs
      it_events               = it_events ).

    lo_ae_processor->check_events( ).

    lo_ae_processor->get_event_data(
      CHANGING
        ct_eventid_map     = ct_eventid_map
        ct_trackingheader  = ct_trackingheader
        ct_tracklocation   = ct_tracklocation
        ct_trackreferences = ct_trackreferences
        ct_trackparameters = ct_trackparameters ).

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_AE_PROCESSOR definition
  public
  create public .

public section.

  interfaces ZIF_GTT_AE_PROCESSOR .

  methods CONSTRUCTOR
    importing
      !IS_DEFINITION type ZIF_GTT_EF_TYPES=>TS_DEFINITION
      !IO_AE_PARAMETERS type ref to ZIF_GTT_AE_PARAMETERS
      !IO_AE_FILLER type ref to ZIF_GTT_AE_FILLER .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_definition TYPE zif_gtt_ef_types=>ts_definition .
    DATA mo_ae_parameters TYPE REF TO zif_gtt_ae_parameters .
    DATA mo_ae_filler TYPE REF TO zif_gtt_ae_filler .

    METHODS add_technical_records
      IMPORTING
        !it_eventid_map     TYPE trxas_evtid_evtcnt_map
      CHANGING
        !ct_trackparameters TYPE zif_gtt_ae_types=>tt_trackparameters
      RAISING
        cx_udm_message .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_AE_PROCESSOR IMPLEMENTATION.


  METHOD add_technical_records.

    DATA: lt_eventid    TYPE STANDARD TABLE OF /saptrx/evtcnt.

    lt_eventid  = VALUE #( FOR ls_eventid_map IN it_eventid_map
                           ( ls_eventid_map-evtcnt ) ).

    SORT lt_eventid.
    DELETE ADJACENT DUPLICATES FROM lt_eventid.

    LOOP AT lt_eventid ASSIGNING FIELD-SYMBOL(<lv_eventid>).
      ct_trackparameters    = VALUE #( BASE ct_trackparameters
        (
          evtcnt      = <lv_eventid>
          param_name  = zif_gtt_ef_constants=>cs_system_fields-actual_technical_timezone
          param_value = zcl_gtt_tools=>get_system_time_zone( )
        )
        (
          evtcnt      = <lv_eventid>
          param_name  = zif_gtt_ef_constants=>cs_system_fields-actual_technical_datetime
          param_value = zcl_gtt_tools=>get_system_date_time( )
        )
        (
          evtcnt      = <lv_eventid>
          param_name  = zif_gtt_ef_constants=>cs_system_fields-reported_by
          param_value = sy-uname
        ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    ms_definition    = is_definition.
    mo_ae_parameters = io_ae_parameters.
    mo_ae_filler     = io_ae_filler.

  ENDMETHOD.


  METHOD zif_gtt_ae_processor~check_events.
    DATA: lr_events TYPE REF TO data,
          lv_dummy  TYPE char100.

    FIELD-SYMBOLS: <lt_events>  TYPE trxas_evt_ctabs.

    lr_events  = mo_ae_parameters->get_events( ).

    ASSIGN lr_events->* TO <lt_events>.

    LOOP AT <lt_events> ASSIGNING FIELD-SYMBOL(<ls_events>).
      IF <ls_events>-maintabdef <> ms_definition-maintab.
        MESSAGE e087(/saptrx/asc)
          WITH <ls_events>-maintabdef
               mo_ae_parameters->get_event_type( )-eventdatafunc
               <ls_events>-eventtype
               mo_ae_parameters->get_appsys( )
          INTO lv_dummy.

        zcl_gtt_tools=>throw_exception(
          iv_textid = zif_gtt_ef_constants=>cs_errors-table_determination ).

      ELSEIF ms_definition-mastertab IS NOT INITIAL AND
            <ls_events>-mastertabdef <> ms_definition-mastertab.
        MESSAGE e088(/saptrx/asc)
          WITH <ls_events>-mastertabdef
               mo_ae_parameters->get_event_type( )-eventdatafunc
               <ls_events>-eventtype
               mo_ae_parameters->get_appsys( )
          INTO lv_dummy.

        zcl_gtt_tools=>throw_exception(
          iv_textid = zif_gtt_ef_constants=>cs_errors-table_determination ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_gtt_ae_processor~check_relevance.
    DATA(lr_events)  = mo_ae_parameters->get_events( ).

    FIELD-SYMBOLS <lt_events>  TYPE trxas_evt_ctabs.

    ASSIGN lr_events->* TO <lt_events>.

    rv_result = zif_gtt_ef_constants=>cs_condition-false.

    LOOP AT <lt_events> ASSIGNING FIELD-SYMBOL(<ls_events>).
      rv_result   = mo_ae_filler->check_relevance( is_events = <ls_events> ).

      IF rv_result = zif_gtt_ef_constants=>cs_condition-true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_gtt_ae_processor~get_event_data.
    DATA: lt_eventid_map     TYPE trxas_evtid_evtcnt_map,
          lt_trackingheader  TYPE zif_gtt_ae_types=>tt_trackingheader,
          lt_tracklocation   TYPE zif_gtt_ae_types=>tt_tracklocation,
          lt_trackreferences TYPE zif_gtt_ae_types=>tt_trackreferences,
          lt_trackparameters TYPE zif_gtt_ae_types=>tt_trackparameters.

    DATA(lr_events)    = mo_ae_parameters->get_events( ).

    FIELD-SYMBOLS <lt_events>  TYPE trxas_evt_ctabs.

    ASSIGN lr_events->* TO <lt_events>.

    LOOP AT <lt_events> ASSIGNING FIELD-SYMBOL(<ls_events>)
      WHERE maintabdef = ms_definition-maintab.

      mo_ae_filler->get_event_data(
        EXPORTING
          is_events          = <ls_events>
        CHANGING
          ct_eventid_map     = lt_eventid_map
          ct_trackingheader  = lt_trackingheader
          ct_tracklocation   = lt_tracklocation
          ct_trackreferences = lt_trackreferences
          ct_trackparameters = lt_trackparameters
      ).
    ENDLOOP.

    add_technical_records(
      EXPORTING
        it_eventid_map     = lt_eventid_map
      CHANGING
        ct_trackparameters = lt_trackparameters ).

    " Add all the changes to result tables in the end of the method,
    " so that in case of exceptions there will be no inconsistent data in them
    ct_eventid_map[]      = VALUE #( BASE ct_eventid_map
                                     ( LINES OF lt_eventid_map ) ).
    ct_trackingheader[]   = VALUE #( BASE ct_trackingheader
                                     ( LINES OF lt_trackingheader ) ).
    ct_tracklocation[]    = VALUE #( BASE ct_tracklocation
                                     ( LINES OF lt_tracklocation ) ).
    ct_trackreferences[]  = VALUE #( BASE ct_trackreferences
                                     ( LINES OF lt_trackreferences ) ).
    ct_trackparameters[]  = VALUE #( BASE ct_trackparameters
                                     ( LINES OF lt_trackparameters ) ).
  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_CTP_SND definition
  public
  abstract
  create public .

public section.

  methods SEND_IDOC_DATA
    exporting
      !ET_BAPIRET type BAPIRET2_T
    raising
      CX_UDM_MESSAGE .
protected section.

  data MV_APPSYS type LOGSYS .
  data MT_EVTYPE type ZIF_GTT_CTP_TYPES=>TT_EVTYPE .
  data MT_AOTYPE type ZIF_GTT_CTP_TYPES=>TT_AOTYPE .
  data MT_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TT_IDOC_DATA .
  data MT_IDOC_EVT_DATA type ZIF_GTT_CTP_TYPES=>TT_IDOC_EVT_DATA .

  methods GET_AOTYPE_RESTRICTION_ID
  abstract
    returning
      value(RV_RST_ID) type ZGTT_RST_ID .
  methods GET_AOTYPE_RESTRICTIONS
    exporting
      !ET_AOTYPE type ZIF_GTT_CTP_TYPES=>TT_AOTYPE_RST .
  methods GET_EVTYPE_RESTRICTION_ID
  abstract
    returning
      value(RV_RST_ID) type ZGTT_RST_ID .
  methods GET_EVTYPE_RESTRICTIONS
    exporting
      !ET_EVTYPE type ZIF_GTT_CTP_TYPES=>TT_EVTYPE_RST .
  methods GET_OBJECT_TYPE
  abstract
    returning
      value(RV_OBJTYPE) type /SAPTRX/TRK_OBJ_TYPE .
  methods FILL_EVT_IDOC_TRXSERV
    importing
      !IS_EVTYPE type ZIF_GTT_CTP_TYPES=>TS_EVTYPE
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_EVT_DATA .
  methods FILL_IDOC_TRXSERV
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA .
  methods INITIATE
    importing
      !I_INITIATE_AOT type BOOLE_D default 'X'
      !I_INITIATE_EVT type BOOLE_D default 'X'
    raising
      CX_UDM_MESSAGE .
  methods INITIATE_AOTYPES
    raising
      CX_UDM_MESSAGE .
  methods INITIATE_EVTYPES
    raising
      CX_UDM_MESSAGE .
  class-methods IS_EXTRACTOR_EXIST
    importing
      !IV_TRK_OBJ_TYPE type CLIKE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods IS_GTT_ENABLED
    importing
      !IT_TRK_OBJ_TYPE type ZIF_GTT_CTP_TYPES=>TT_TRK_OBJ_TYPE
    returning
      value(RV_RESULT) type ABAP_BOOL .
private section.
ENDCLASS.



CLASS ZCL_GTT_CTP_SND IMPLEMENTATION.


  METHOD FILL_EVT_IDOC_TRXSERV.
    SELECT SINGLE *
       INTO cs_idoc_data-trxserv
       FROM /saptrx/trxserv
       WHERE trx_server_id = is_evtype-trxservername.
  ENDMETHOD.


  METHOD FILL_IDOC_TRXSERV.

    SELECT SINGLE *
      INTO cs_idoc_data-trxserv
      FROM /saptrx/trxserv
      WHERE trx_server_id = is_aotype-server_name.

  ENDMETHOD.


  METHOD GET_AOTYPE_RESTRICTIONS.

    DATA(rst_id)  = get_aotype_restriction_id( ).

    SELECT rst_option AS option,
           rst_sign   AS sign,
           rst_low    AS low,
           rst_high   AS high
      INTO CORRESPONDING FIELDS OF TABLE @et_aotype
      FROM zgtt_aotype_rst
      WHERE rst_id = @rst_id.

    IF sy-subrc <> 0.
      CLEAR: et_aotype[].
    ENDIF.

  ENDMETHOD.


  METHOD GET_EVTYPE_RESTRICTIONS.
    DATA(rst_id)  = get_evtype_restriction_id( ).

    SELECT rst_option AS option,
           rst_sign   AS sign,
           rst_low    AS low,
           rst_high   AS high
      INTO CORRESPONDING FIELDS OF TABLE @et_evtype
      FROM zgtt_evtype_rst
      WHERE rst_id = @rst_id.

    IF sy-subrc <> 0.
      CLEAR: et_evtype[].
    ENDIF.
  ENDMETHOD.


  METHOD INITIATE.

    " Get current logical system
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = mv_appsys
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.

    IF sy-subrc <> 0.
      MESSAGE e007(zgtt) INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
    IF i_initiate_aot = abap_true.
      initiate_aotypes( ).
    ENDIF.
    IF i_initiate_evt = abap_true.
      initiate_evtypes( ).
    ENDIF.
  ENDMETHOD.


  METHOD initiate_aotypes.

    DATA: lt_aotype_rst TYPE zif_gtt_ctp_types=>tt_aotype_rst.

    DATA(lv_objtype)  = get_object_type(  ).

    get_aotype_restrictions(
      IMPORTING
        et_aotype = lt_aotype_rst ).

    " Prepare AOT list
    IF lt_aotype_rst IS NOT INITIAL.
      SELECT trk_obj_type  AS obj_type
             aotype        AS aot_type
             trxservername AS server_name
        INTO TABLE mt_aotype
        FROM /saptrx/aotypes
        WHERE trk_obj_type  = lv_objtype
          AND aotype       IN lt_aotype_rst
          AND torelevant    = abap_true.

      IF sy-subrc <> 0.
        MESSAGE e008(zgtt) INTO DATA(lv_dummy).
        " zcl_gtt_spof_tools=>throw_exception( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD initiate_evtypes.

    DATA: lt_evtype_rst TYPE zif_gtt_ctp_types=>tt_evtype_rst.
    DATA(lv_objtype)  = get_object_type(  ).

    get_evtype_restrictions(
      IMPORTING
        et_evtype = lt_evtype_rst ).

    " Prepare AOT list
    IF lt_evtype_rst IS NOT INITIAL.
      SELECT trk_obj_type evtype
             trxservername eventdatafunc
        INTO TABLE mt_evtype
        FROM /saptrx/evtypes
        WHERE trk_obj_type  = lv_objtype
          AND evtype       IN lt_evtype_rst
          AND torelevant    = abap_true.

      IF sy-subrc <> 0.
        MESSAGE e008(zgtt) INTO DATA(lv_dummy).
        " zcl_gtt_spof_tools=>throw_exception( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD IS_EXTRACTOR_EXIST.

    DATA: lv_trk_obj_type  TYPE /saptrx/aotypes-trk_obj_type.

    SELECT SINGLE trk_obj_type
      INTO lv_trk_obj_type
      FROM /saptrx/aotypes
      WHERE trk_obj_type = iv_trk_obj_type
        AND torelevant   = abap_true.

    rv_result   = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD IS_GTT_ENABLED.

    DATA: lv_extflag      TYPE flag.

    rv_result   = abap_false.

    " Check package dependent BADI disabling
    CALL FUNCTION 'GET_R3_EXTENSION_SWITCH'
      EXPORTING
        i_structure_package = zif_gtt_ef_constants=>cv_structure_pkg
      IMPORTING
        e_active            = lv_extflag
      EXCEPTIONS
        not_existing        = 1
        object_not_existing = 2
        no_extension_object = 3
        OTHERS              = 4.

    IF sy-subrc = 0 AND lv_extflag = abap_true.
*     Check if any tracking server defined
      CALL FUNCTION '/SAPTRX/EVENT_MGR_CHECK'
        EXCEPTIONS
          no_event_mgr_available = 1
          OTHERS                 = 2.

      "Check whether at least 1 active extractor exists for every object
      IF sy-subrc = 0.
        rv_result = boolc( it_trk_obj_type[] IS NOT INITIAL ).

        LOOP AT it_trk_obj_type ASSIGNING FIELD-SYMBOL(<lv_trk_obj_type>).
          IF is_extractor_exist( iv_trk_obj_type = <lv_trk_obj_type> ) = abap_false.
            rv_result   = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD send_idoc_data.

    DATA: lt_bapiret1 TYPE bapiret2_t,
          lt_bapiret2 TYPE bapiret2_t.

    LOOP AT mt_idoc_data ASSIGNING FIELD-SYMBOL(<ls_idoc_data>).
      CLEAR: lt_bapiret1[], lt_bapiret2[].

      /saptrx/cl_send_idocs=>send_idoc_ehpost01(
        EXPORTING
          it_control      = <ls_idoc_data>-control
          it_info         = <ls_idoc_data>-info
          it_tracking_id  = <ls_idoc_data>-tracking_id
          it_exp_event    = <ls_idoc_data>-exp_event
          is_trxserv      = <ls_idoc_data>-trxserv
          iv_appsys       = <ls_idoc_data>-appsys
          it_appobj_ctabs = <ls_idoc_data>-appobj_ctabs
          iv_upd_task     = 'X'
        IMPORTING
          et_bapireturn   = lt_bapiret1 ).

      " when GTT.2 version
      IF /saptrx/cl_send_idocs=>st_idoc_data[] IS NOT INITIAL.
        /saptrx/cl_send_idocs=>send_idoc_gttmsg01(
          IMPORTING
            et_bapireturn = lt_bapiret2 ).
      ENDIF.

      " collect messages, if it is necessary
      IF et_bapiret IS REQUESTED.
        et_bapiret    = VALUE #( BASE et_bapiret
                                 ( LINES OF lt_bapiret1 )
                                 ( LINES OF lt_bapiret2 ) ).
      ENDIF.
    ENDLOOP.

    " Process event
    LOOP AT mt_idoc_evt_data ASSIGNING FIELD-SYMBOL(<ls_idoc_evt_data>).
      CLEAR: lt_bapiret1[], lt_bapiret2[].
      /saptrx/cl_send_idocs=>send_idoc_evmsta02(
        EXPORTING
          is_trxserv        = <ls_idoc_evt_data>-trxserv
          it_evm_header     = <ls_idoc_evt_data>-evt_header
          it_evm_locationid = <ls_idoc_evt_data>-evt_locationid
          it_evm_parameters = <ls_idoc_evt_data>-evt_parameters
          it_evt_ctabs      = <ls_idoc_evt_data>-evt_ctabs
          it_eventid_map    = <ls_idoc_evt_data>-eventid_map
          it_evm_reference  = <ls_idoc_evt_data>-evt_reference
        IMPORTING
          et_bapireturn     = lt_bapiret1
      ).

      " when GTT.2 version
      IF /saptrx/cl_send_idocs=>st_idoc_data[] IS NOT INITIAL.
        /saptrx/cl_send_idocs=>send_idoc_gttmsg01(
          IMPORTING
            et_bapireturn = lt_bapiret2 ).
      ENDIF.

      " collect messages, if it is necessary
      IF et_bapiret IS REQUESTED.
        et_bapiret    = VALUE #( BASE et_bapiret
                                 ( LINES OF lt_bapiret1 )
                                 ( LINES OF lt_bapiret2 ) ).
      ENDIF.
    ENDLOOP.

    CLEAR:
      mt_idoc_data,
      mt_idoc_evt_data.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_EF_PARAMETERS definition
  public
  create public .

public section.

  interfaces ZIF_GTT_EF_PARAMETERS .

  methods CONSTRUCTOR
    importing
      !IV_APPSYS type /SAPTRX/APPLSYSTEM
      !IS_APP_OBJ_TYPES type /SAPTRX/AOTYPES
      !IT_ALL_APPL_TABLES type TRXAS_TABCONTAINER
      !IT_APP_TYPE_CNTL_TABS type TRXAS_APPTYPE_TABS optional
      !IT_APP_OBJECTS type TRXAS_APPOBJ_CTABS .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_appsys TYPE /saptrx/applsystem .
    DATA ms_app_obj_types TYPE /saptrx/aotypes .
    DATA mr_all_appl_tables TYPE REF TO data .
    DATA mr_app_type_cntl_tabs TYPE REF TO data .
    DATA mr_app_objects TYPE REF TO data .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_EF_PARAMETERS IMPLEMENTATION.


  METHOD constructor.

    mv_appsys               = iv_appsys.
    ms_app_obj_types        = is_app_obj_types.
    mr_all_appl_tables      = REF #( it_all_appl_tables ).
    IF it_app_type_cntl_tabs IS SUPPLIED.
      mr_app_type_cntl_tabs = REF #( it_app_type_cntl_tabs ).
    ENDIF.
    mr_app_objects          = REF #( it_app_objects ).

  ENDMETHOD.


  METHOD zif_gtt_ef_parameters~get_appl_table.

    TRY.
        FIELD-SYMBOLS: <lt_all_appl_tables>   TYPE trxas_tabcontainer.

        ASSIGN mr_all_appl_tables->* TO <lt_all_appl_tables>.

        rr_data   = <lt_all_appl_tables>[ tabledef = iv_tabledef ]-tableref.

      CATCH cx_sy_itab_line_not_found.
        MESSAGE e008(/saptrx/asc)
          WITH iv_tabledef
               ms_app_obj_types-aotype
          INTO DATA(lv_dummy).

        zcl_gtt_tools=>throw_exception(
          iv_textid = zif_gtt_ef_constants=>cs_errors-stop_processing ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_gtt_ef_parameters~get_appsys.
    rv_appsys           = mv_appsys.
  ENDMETHOD.


  METHOD zif_gtt_ef_parameters~get_app_objects.
    rr_data             = mr_app_objects.
  ENDMETHOD.


  METHOD zif_gtt_ef_parameters~get_app_obj_types.
    rs_app_obj_types    = ms_app_obj_types.
  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_EF_PERFORMER definition
  public
  create public .

public section.

  class-methods CHECK_RELEVANCE
    importing
      !IS_DEFINITION type ZIF_GTT_EF_TYPES=>TS_DEFINITION
      !IO_TP_FACTORY type ref to ZIF_GTT_TP_FACTORY
      !IV_APPSYS type /SAPTRX/APPLSYSTEM
      !IS_APP_OBJ_TYPES type /SAPTRX/AOTYPES
      !IT_ALL_APPL_TABLES type TRXAS_TABCONTAINER
      !IT_APP_TYPE_CNTL_TABS type TRXAS_APPTYPE_TABS optional
      !IT_APP_OBJECTS type TRXAS_APPOBJ_CTABS
    returning
      value(RV_RESULT) type SY-BINPT
    raising
      CX_UDM_MESSAGE .
  class-methods GET_APP_OBJ_TYPE_ID
    importing
      !IS_DEFINITION type ZIF_GTT_EF_TYPES=>TS_DEFINITION
      !IO_TP_FACTORY type ref to ZIF_GTT_TP_FACTORY
      !IV_APPSYS type /SAPTRX/APPLSYSTEM
      !IS_APP_OBJ_TYPES type /SAPTRX/AOTYPES
      !IT_ALL_APPL_TABLES type TRXAS_TABCONTAINER
      !IT_APP_TYPE_CNTL_TABS type TRXAS_APPTYPE_TABS optional
      !IT_APP_OBJECTS type TRXAS_APPOBJ_CTABS
    returning
      value(RV_APPOBJID) type /SAPTRX/AOID
    raising
      CX_UDM_MESSAGE .
  class-methods GET_CONTROL_DATA
    importing
      !IS_DEFINITION type ZIF_GTT_EF_TYPES=>TS_DEFINITION
      !IO_TP_FACTORY type ref to ZIF_GTT_TP_FACTORY
      !IV_APPSYS type /SAPTRX/APPLSYSTEM
      !IS_APP_OBJ_TYPES type /SAPTRX/AOTYPES
      !IT_ALL_APPL_TABLES type TRXAS_TABCONTAINER
      !IT_APP_TYPE_CNTL_TABS type TRXAS_APPTYPE_TABS
      !IT_APP_OBJECTS type TRXAS_APPOBJ_CTABS
    changing
      !CT_CONTROL_DATA type ZIF_GTT_EF_TYPES=>TT_CONTROL_DATA
    raising
      CX_UDM_MESSAGE .
  class-methods GET_PLANNED_EVENTS
    importing
      !IS_DEFINITION type ZIF_GTT_EF_TYPES=>TS_DEFINITION
      !IO_TP_FACTORY type ref to ZIF_GTT_TP_FACTORY
      !IV_APPSYS type /SAPTRX/APPLSYSTEM
      !IS_APP_OBJ_TYPES type /SAPTRX/AOTYPES
      !IT_ALL_APPL_TABLES type TRXAS_TABCONTAINER
      !IT_APP_TYPE_CNTL_TABS type TRXAS_APPTYPE_TABS
      !IT_APP_OBJECTS type TRXAS_APPOBJ_CTABS
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
      !CT_MEASRMNTDATA type ZIF_GTT_EF_TYPES=>TT_MEASRMNTDATA
      !CT_INFODATA type ZIF_GTT_EF_TYPES=>TT_INFODATA
    raising
      CX_UDM_MESSAGE .
  class-methods GET_TRACK_ID_DATA
    importing
      !IS_DEFINITION type ZIF_GTT_EF_TYPES=>TS_DEFINITION
      !IO_TP_FACTORY type ref to ZIF_GTT_TP_FACTORY
      !IV_APPSYS type /SAPTRX/APPLSYSTEM
      !IS_APP_OBJ_TYPES type /SAPTRX/AOTYPES
      !IT_ALL_APPL_TABLES type TRXAS_TABCONTAINER
      !IT_APP_TYPE_CNTL_TABS type TRXAS_APPTYPE_TABS
      !IT_APP_OBJECTS type TRXAS_APPOBJ_CTABS
    exporting
      !ET_TRACK_ID_DATA type ZIF_GTT_EF_TYPES=>TT_TRACK_ID_DATA
    raising
      CX_UDM_MESSAGE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GTT_EF_PERFORMER IMPLEMENTATION.


  METHOD check_relevance.

    DATA: lo_ef_processor   TYPE REF TO zif_gtt_ef_processor.

    " get instance of extractor function processor
    lo_ef_processor = io_tp_factory->get_ef_processor(
      is_definition         = is_definition
      io_bo_factory         = io_tp_factory
      iv_appsys             = iv_appsys
      is_app_obj_types      = is_app_obj_types
      it_all_appl_tables    = it_all_appl_tables
      it_app_type_cntl_tabs = it_app_type_cntl_tabs
      it_app_objects        = it_app_objects ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " check relevance
    rv_result = lo_ef_processor->check_relevance( ).

  ENDMETHOD.


  METHOD get_app_obj_type_id.
    DATA: lo_ef_processor   TYPE REF TO zif_gtt_ef_processor.

    " get instance of extractor function processor
    lo_ef_processor = io_tp_factory->get_ef_processor(
      is_definition         = is_definition
      io_bo_factory         = io_tp_factory
      iv_appsys             = iv_appsys
      is_app_obj_types      = is_app_obj_types
      it_all_appl_tables    = it_all_appl_tables
      it_app_type_cntl_tabs = it_app_type_cntl_tabs
      it_app_objects        = it_app_objects
    ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " fill control data from business object data
    rv_appobjid   = lo_ef_processor->get_app_obj_type_id( ).
  ENDMETHOD.


  METHOD get_control_data.

    DATA: lo_ef_processor   TYPE REF TO zif_gtt_ef_processor.

    " get instance of extractor function processor
    lo_ef_processor = io_tp_factory->get_ef_processor(
      is_definition         = is_definition
      io_bo_factory         = io_tp_factory
      iv_appsys             = iv_appsys
      is_app_obj_types      = is_app_obj_types
      it_all_appl_tables    = it_all_appl_tables
      it_app_type_cntl_tabs = it_app_type_cntl_tabs
      it_app_objects        = it_app_objects
    ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " fill control data from business object data
    lo_ef_processor->get_control_data(
      CHANGING
        ct_control_data = ct_control_data[]
    ).


  ENDMETHOD.


  METHOD get_planned_events.

    DATA: lo_ef_processor   TYPE REF TO zif_gtt_ef_processor.

    " get instance of extractor function processor
    lo_ef_processor = io_tp_factory->get_ef_processor(
      is_definition         = is_definition
      io_bo_factory         = io_tp_factory
      iv_appsys             = iv_appsys
      is_app_obj_types      = is_app_obj_types
      it_all_appl_tables    = it_all_appl_tables
      it_app_type_cntl_tabs = it_app_type_cntl_tabs
      it_app_objects        = it_app_objects
    ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " fill planned events data
    lo_ef_processor->get_planned_events(
      CHANGING
        ct_expeventdata = ct_expeventdata
        ct_measrmntdata = ct_measrmntdata
        ct_infodata     = ct_infodata
    ).


  ENDMETHOD.


  METHOD get_track_id_data.

    DATA: lo_ef_processor   TYPE REF TO zif_gtt_ef_processor.

    CLEAR: et_track_id_data[].

    " get instance of extractor function processor
    lo_ef_processor = io_tp_factory->get_ef_processor(
      is_definition         = is_definition
      io_bo_factory         = io_tp_factory
      iv_appsys             = iv_appsys
      is_app_obj_types      = is_app_obj_types
      it_all_appl_tables    = it_all_appl_tables
      it_app_type_cntl_tabs = it_app_type_cntl_tabs
      it_app_objects        = it_app_objects
    ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " fill controll data from PO Header data
    lo_ef_processor->get_track_id_data(
      IMPORTING
        et_track_id_data = et_track_id_data
    ).


  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_EF_PROCESSOR definition
  public
  create public .

public section.

  interfaces ZIF_GTT_EF_PROCESSOR .

  methods CONSTRUCTOR
    importing
      !IO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS
      !IO_BO_READER type ref to ZIF_GTT_TP_READER
      !IO_PE_FILLER type ref to ZIF_GTT_PE_FILLER
      !IS_DEFINITION type ZIF_GTT_EF_TYPES=>TS_DEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_ef_parameters TYPE REF TO zif_gtt_ef_parameters .
    DATA mo_tp_reader TYPE REF TO zif_gtt_tp_reader .
    DATA mo_pe_filler TYPE REF TO zif_gtt_pe_filler .
    DATA ms_definition TYPE zif_gtt_ef_types=>ts_definition .

    METHODS add_struct_to_control_data
      IMPORTING
        !ir_bo_data      TYPE REF TO data
        !iv_appobjid     TYPE /saptrx/aoid
      CHANGING
        !ct_control_data TYPE zif_gtt_ef_types=>tt_control_data
      RAISING
        cx_udm_message .
    METHODS add_sys_attr_to_control_data
      IMPORTING
        !iv_appobjid     TYPE /saptrx/aoid
      CHANGING
        !ct_control_data TYPE zif_gtt_ef_types=>tt_control_data
      RAISING
        cx_udm_message .
ENDCLASS.



CLASS ZCL_GTT_EF_PROCESSOR IMPLEMENTATION.


  METHOD add_struct_to_control_data.

    DATA: lt_fields       TYPE cl_abap_structdescr=>component_table,
          ls_control_data TYPE zif_gtt_ef_types=>ts_control_data,
          lr_mapping      TYPE REF TO data,
          lv_dummy        TYPE char100.

    FIELD-SYMBOLS: <ls_bo_data>   TYPE any,
                   <ls_mapping>   TYPE any,
                   <lt_value>     TYPE ANY TABLE,
                   <lv_value>     TYPE any,
                   <lv_paramname> TYPE any.

    ASSIGN ir_bo_data->* TO <ls_bo_data>.

    IF <ls_bo_data> IS ASSIGNED.
      " get fields list of the structure, which provided by reader class
      lt_fields = CAST cl_abap_structdescr(
                    cl_abap_typedescr=>describe_by_data(
                      p_data = <ls_bo_data> )
                  )->get_components( ).

      " assign mapping table to use it in converting of field names into external format
      lr_mapping  = mo_tp_reader->get_mapping_structure( ).
      ASSIGN lr_mapping->* TO <ls_mapping>.

      IF <ls_mapping> IS ASSIGNED.
        " fill generic parameters
        ls_control_data-appsys      = mo_ef_parameters->get_appsys( ).
        ls_control_data-appobjtype  = mo_ef_parameters->get_app_obj_types( )-aotype.
        ls_control_data-language    = sy-langu.
        ls_control_data-appobjid    = iv_appobjid.

        " walk around fields list and copy values one by one
        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
          ASSIGN COMPONENT <ls_fields>-name OF STRUCTURE <ls_bo_data> TO <lv_value>.
          ASSIGN COMPONENT <ls_fields>-name OF STRUCTURE <ls_mapping> TO <lv_paramname>.

          IF <lv_value> IS ASSIGNED AND <lv_paramname> IS ASSIGNED.
            ls_control_data-paramname   = <lv_paramname>.

            " cycled copy for table values
            IF zcl_gtt_tools=>is_table( iv_value = <lv_value> ) = abap_true.
              ASSIGN <lv_value> TO <lt_value>.

              CLEAR: ls_control_data-paramindex.

              LOOP AT <lt_value> ASSIGNING <lv_value>.
                ADD 1 TO ls_control_data-paramindex.
                ls_control_data-value = zcl_gtt_tools=>get_pretty_value(
                  iv_value = <lv_value> ).

                APPEND ls_control_data TO ct_control_data.
              ENDLOOP.

              " add clearing value, when multivalues table is empty
              IF <lt_value>[] IS INITIAL AND
                  mo_tp_reader->get_field_parameter(
                    iv_field_name = <lv_paramname>
                    iv_parameter  = zif_gtt_ef_constants=>cs_parameter_id-key_field
                  ) = abap_true.

                ls_control_data-paramindex  = 1.
                ls_control_data-value       = ''.
                APPEND ls_control_data TO ct_control_data.
              ENDIF.

              " simple copy for usual values
            ELSEIF <lv_value> IS NOT INITIAL OR
                   mo_tp_reader->get_field_parameter(
                     iv_field_name = <lv_paramname>
                     iv_parameter  = zif_gtt_ef_constants=>cs_parameter_id-no_empty_tag
                   ) = abap_false.

              ls_control_data-paramindex  = 0.
              ls_control_data-value = zcl_gtt_tools=>get_pretty_value(
                iv_value = <lv_value> ).
              APPEND ls_control_data TO ct_control_data.
            ELSEIF ( zcl_gtt_tools=>is_date( iv_value = <lv_value> ) = abap_true OR
                     zcl_gtt_tools=>is_timestamp( iv_value = <lv_value> ) = abap_true ).

              ls_control_data-paramindex  = 0.
              ls_control_data-value       = ''.
              APPEND ls_control_data TO ct_control_data.
            ENDIF.
          ELSEIF <lv_value> IS NOT ASSIGNED.
            MESSAGE e001(zgtt) WITH <ls_fields>-name 'data table' INTO lv_dummy ##NO_TEXT.
            zcl_gtt_tools=>throw_exception( ).
          ELSE.
            MESSAGE e001(zgtt) WITH <ls_fields>-name 'mapping table' INTO lv_dummy ##NO_TEXT.
            zcl_gtt_tools=>throw_exception( ).
          ENDIF.
        ENDLOOP.
      ELSE.
        MESSAGE e002(zgtt) WITH 'mapping table' INTO lv_dummy ##NO_TEXT.
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt) WITH 'data table' INTO lv_dummy ##NO_TEXT.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD add_sys_attr_to_control_data.

    DATA: ls_control_data TYPE zif_gtt_ef_types=>ts_control_data.

    ls_control_data-appsys      = mo_ef_parameters->get_appsys( ).
    ls_control_data-appobjtype  = mo_ef_parameters->get_app_obj_types( )-aotype.
    ls_control_data-language    = sy-langu.
    ls_control_data-appobjid    = iv_appobjid.

    ls_control_data-paramname   = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_timezone.
    ls_control_data-value       = zcl_gtt_tools=>get_system_time_zone( ).
    APPEND ls_control_data TO ct_control_data.

    ls_control_data-paramname   = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_datetime.
    ls_control_data-value       = zcl_gtt_tools=>get_system_date_time( ).
    APPEND ls_control_data TO ct_control_data.

    ls_control_data-paramname   = zif_gtt_ef_constants=>cs_system_fields-actual_technical_timezone.
    ls_control_data-value       = zcl_gtt_tools=>get_system_time_zone( ).
    APPEND ls_control_data TO ct_control_data.

    ls_control_data-paramname   = zif_gtt_ef_constants=>cs_system_fields-actual_technical_datetime.
    ls_control_data-value       = zcl_gtt_tools=>get_system_date_time( ).
    APPEND ls_control_data TO ct_control_data.

    ls_control_data-paramname   = zif_gtt_ef_constants=>cs_system_fields-reported_by.
    ls_control_data-value       = sy-uname.
    APPEND ls_control_data TO ct_control_data.

  ENDMETHOD.


  METHOD constructor.

    mo_ef_parameters    = io_ef_parameters.
    mo_tp_reader        = io_bo_reader.
    mo_pe_filler        = io_pe_filler.
    ms_definition       = is_definition.

  ENDMETHOD.


  METHOD zif_gtt_ef_processor~check_app_objects.

    DATA: lr_app_objects TYPE REF TO data,
          lv_dummy       TYPE char100.

    FIELD-SYMBOLS: <lt_app_objects>  TYPE trxas_appobj_ctabs.

    lr_app_objects  = mo_ef_parameters->get_app_objects( ).

    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    LOOP AT <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>).
      IF <ls_app_objects>-maintabdef <> ms_definition-maintab.
        MESSAGE e087(/saptrx/asc)
          WITH <ls_app_objects>-maintabdef
               mo_ef_parameters->get_app_obj_types( )-controldatafunc
               zif_gtt_ef_constants=>cv_aot
               mo_ef_parameters->get_appsys( )
          INTO lv_dummy.

        zcl_gtt_tools=>throw_exception(
          iv_textid = zif_gtt_ef_constants=>cs_errors-table_determination ).

      ELSEIF ms_definition-mastertab IS NOT INITIAL AND
            <ls_app_objects>-mastertabdef <> ms_definition-mastertab.
        MESSAGE e088(/saptrx/asc)
          WITH <ls_app_objects>-maintabdef
               mo_ef_parameters->get_app_obj_types( )-controldatafunc
               zif_gtt_ef_constants=>cv_aot
               mo_ef_parameters->get_appsys( )
          INTO lv_dummy.

        zcl_gtt_tools=>throw_exception(
          iv_textid = zif_gtt_ef_constants=>cs_errors-table_determination ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_gtt_ef_processor~check_relevance.
    DATA(lr_app_objects)  = mo_ef_parameters->get_app_objects( ).
    FIELD-SYMBOLS <lt_app_objects>  TYPE trxas_appobj_ctabs.

    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    rv_result = zif_gtt_ef_constants=>cs_condition-false.

    LOOP AT <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>).
      rv_result   = mo_tp_reader->check_relevance( is_app_object = <ls_app_objects> ).

      IF rv_result = zif_gtt_ef_constants=>cs_condition-false AND
         mo_pe_filler IS BOUND.
        rv_result = mo_pe_filler->check_relevance( is_app_objects = <ls_app_objects> ).
      ENDIF.

      IF rv_result = zif_gtt_ef_constants=>cs_condition-true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_gtt_ef_processor~get_app_obj_type_id.
    DATA: lr_app_objects   TYPE REF TO data,
          lt_track_id_data TYPE zif_gtt_ef_types=>tt_track_id_data,
          lr_bo_data       TYPE REF TO data.

    FIELD-SYMBOLS: <lt_app_objects>   TYPE trxas_appobj_ctabs.


    lr_app_objects  = mo_ef_parameters->get_app_objects( ).
    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    READ TABLE <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>)
      INDEX 1.

    IF sy-subrc = 0.
      rv_appobjid = mo_tp_reader->get_app_obj_type_id(
        is_app_object = <ls_app_objects> ).

    ELSE.
      MESSAGE e012(zgtt) INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_gtt_ef_processor~get_control_data.
    DATA: lt_control_data TYPE zif_gtt_ef_types=>tt_control_data,
          lr_app_objects  TYPE REF TO data,
          lr_bo_data      TYPE REF TO data.

    FIELD-SYMBOLS: <lt_app_objects>   TYPE trxas_appobj_ctabs.

    lr_app_objects  = mo_ef_parameters->get_app_objects( ).
    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    LOOP AT <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>)
      WHERE maintabdef = ms_definition-maintab.

      lr_bo_data = mo_tp_reader->get_data(
        EXPORTING
          is_app_object = <ls_app_objects> ).

      add_struct_to_control_data(
        EXPORTING
          ir_bo_data      = lr_bo_data
          iv_appobjid     = <ls_app_objects>-appobjid
        CHANGING
          ct_control_data = lt_control_data ).

      add_sys_attr_to_control_data(
        EXPORTING
          iv_appobjid     = <ls_app_objects>-appobjid
        CHANGING
          ct_control_data = lt_control_data ).
    ENDLOOP.

    " Add all the changes to result tables in the end of the method,
    " so that in case of exceptions there will be no inconsistent data in them
    IF lt_control_data[] IS NOT INITIAL.
      ct_control_data[] = VALUE #( BASE ct_control_data
                                   ( LINES OF lt_control_data ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_gtt_ef_processor~get_planned_events.

    DATA: lt_expeventdata TYPE zif_gtt_ef_types=>tt_expeventdata,
          lt_measrmntdata TYPE zif_gtt_ef_types=>tt_measrmntdata,
          lt_infodata     TYPE zif_gtt_ef_types=>tt_infodata,
          lr_app_objects  TYPE REF TO data.

    FIELD-SYMBOLS: <lt_app_objects>   TYPE trxas_appobj_ctabs.

    lr_app_objects  = mo_ef_parameters->get_app_objects( ).
    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    LOOP AT <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>)
      WHERE maintabdef = ms_definition-maintab.

      mo_pe_filler->get_planed_events(
        EXPORTING
          is_app_objects  = <ls_app_objects>
        CHANGING
          ct_expeventdata = lt_expeventdata
          ct_measrmntdata = lt_measrmntdata
          ct_infodata     = lt_infodata ).
    ENDLOOP.

    " Add all the changes to result tables in the end of the method,
    " so that in case of exceptions there will be no inconsistent data in them
    IF lt_expeventdata[] IS NOT INITIAL.
      ct_expeventdata[] = VALUE #( BASE ct_expeventdata
                                   ( LINES OF lt_expeventdata ) ).
    ENDIF.
    IF lt_measrmntdata[] IS NOT INITIAL.
      ct_measrmntdata[] = VALUE #( BASE ct_measrmntdata
                                   ( LINES OF lt_measrmntdata ) ).
    ENDIF.
    IF lt_expeventdata[] IS NOT INITIAL.
      lt_infodata[] = VALUE #( BASE ct_infodata
                                   ( LINES OF lt_infodata ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_gtt_ef_processor~get_track_id_data.
    DATA: lr_app_objects   TYPE REF TO data,
          lt_track_id_data TYPE zif_gtt_ef_types=>tt_track_id_data,
          lr_bo_data       TYPE REF TO data.

    FIELD-SYMBOLS: <lt_app_objects>   TYPE trxas_appobj_ctabs.

    CLEAR: et_track_id_data[].

    lr_app_objects  = mo_ef_parameters->get_app_objects( ).
    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    LOOP AT <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>)
      WHERE maintabdef = ms_definition-maintab.

      mo_tp_reader->get_track_id_data(
        EXPORTING
          is_app_object    = <ls_app_objects>
        IMPORTING
          et_track_id_data = lt_track_id_data
      ).

      et_track_id_data  = VALUE #( BASE et_track_id_data
                                   ( LINES OF lt_track_id_data ) ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_gtt_tp_factory DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_gtt_tp_factory .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_TP_FACTORY IMPLEMENTATION.


  METHOD zif_gtt_tp_factory~get_ef_parameters.

    ro_ef_parameters = NEW zcl_gtt_ef_parameters(
      iv_appsys             = iv_appsys
      is_app_obj_types      = is_app_obj_types
      it_all_appl_tables    = it_all_appl_tables
      it_app_type_cntl_tabs = it_app_type_cntl_tabs
      it_app_objects        = it_app_objects ).

  ENDMETHOD.


  METHOD zif_gtt_tp_factory~get_ef_processor.
    DATA:
      lo_ef_parameters TYPE REF TO zif_gtt_ef_parameters,
      lo_bo_reader     TYPE REF TO zif_gtt_tp_reader,
      lo_pe_filler     TYPE REF TO zif_gtt_pe_filler.

    lo_ef_parameters = zif_gtt_tp_factory~get_ef_parameters(
      iv_appsys             = iv_appsys
      is_app_obj_types      = is_app_obj_types
      it_all_appl_tables    = it_all_appl_tables
      it_app_type_cntl_tabs = it_app_type_cntl_tabs
      it_app_objects        = it_app_objects ).

    lo_bo_reader = zif_gtt_tp_factory~get_tp_reader(
      io_ef_parameters = lo_ef_parameters ).

    " filler is not obligatory
    TRY.
        lo_pe_filler = zif_gtt_tp_factory~get_pe_filler(
          io_ef_parameters = lo_ef_parameters
          io_bo_reader     = lo_bo_reader ).
      CATCH cx_udm_message.
    ENDTRY.

    ro_ef_processor = NEW zcl_gtt_ef_processor(
      io_ef_parameters = lo_ef_parameters
      io_bo_reader     = lo_bo_reader
      io_pe_filler     = lo_pe_filler
      is_definition    = is_definition
    ).
  ENDMETHOD.


  METHOD zif_gtt_tp_factory~get_pe_filler.
  ENDMETHOD.


  METHOD zif_gtt_tp_factory~get_tp_reader.

  ENDMETHOD.
ENDCLASS.""",
    r"""DATA:  BEGIN OF STATUS_ZGTT_AOTYPE_RST               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGTT_AOTYPE_RST               .
CONTROLS: TCTRL_ZGTT_AOTYPE_RST
            TYPE TABLEVIEW USING SCREEN '2001'.""",
    r"""DATA:  BEGIN OF STATUS_ZGTT_EVTYPE_RST               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGTT_EVTYPE_RST               .
CONTROLS: TCTRL_ZGTT_EVTYPE_RST
            TYPE TABLEVIEW USING SCREEN '2002'.""",
    r"""TABLES: *ZGTT_AOTYPE_RST.""",
    r"""INTERFACE zif_gtt_ae_factory
  PUBLIC .


  METHODS get_ae_filler
    IMPORTING
      !io_ae_parameters   TYPE REF TO zif_gtt_ae_parameters
    RETURNING
      VALUE(ro_ae_filler) TYPE REF TO zif_gtt_ae_filler
    RAISING
      cx_udm_message .
  METHODS get_ae_parameters
    IMPORTING
      !iv_appsys               TYPE /saptrx/applsystem
      !is_event_type           TYPE /saptrx/evtypes
      !it_all_appl_tables      TYPE trxas_tabcontainer
      !it_event_type_cntl_tabs TYPE trxas_eventtype_tabs OPTIONAL
      !it_events               TYPE trxas_evt_ctabs
    RETURNING
      VALUE(ro_ae_parameters)  TYPE REF TO zif_gtt_ae_parameters
    RAISING
      cx_udm_message .
  METHODS get_ae_processor
    IMPORTING
      !is_definition           TYPE zif_gtt_ef_types=>ts_definition
      !io_ae_factory           TYPE REF TO zif_gtt_ae_factory
      !iv_appsys               TYPE /saptrx/applsystem
      !is_event_type           TYPE /saptrx/evtypes
      !it_all_appl_tables      TYPE trxas_tabcontainer
      !it_event_type_cntl_tabs TYPE trxas_eventtype_tabs OPTIONAL
      !it_events               TYPE trxas_evt_ctabs
    RETURNING
      VALUE(ro_ae_processor)   TYPE REF TO zif_gtt_ae_processor
    RAISING
      cx_udm_message .
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_ae_filler
  PUBLIC .


  METHODS check_relevance
    IMPORTING
      !is_events       TYPE trxas_evt_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE zif_gtt_ef_types=>tv_condition
    RAISING
      cx_udm_message .
  METHODS get_event_data
    IMPORTING
      !is_events          TYPE trxas_evt_ctab_wa
    CHANGING
      !ct_eventid_map     TYPE trxas_evtid_evtcnt_map
      !ct_trackingheader  TYPE zif_gtt_ae_types=>tt_trackingheader
      !ct_tracklocation   TYPE zif_gtt_ae_types=>tt_tracklocation
      !ct_trackreferences TYPE zif_gtt_ae_types=>tt_trackreferences
      !ct_trackparameters TYPE zif_gtt_ae_types=>tt_trackparameters
    RAISING
      cx_udm_message .
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_ae_parameters
  PUBLIC .


  METHODS get_appsys
    RETURNING
      VALUE(rv_appsys) TYPE /saptrx/applsystem .
  METHODS get_event_type
    RETURNING
      VALUE(rs_event_type) TYPE /saptrx/evtypes .
  METHODS get_appl_table
    IMPORTING
      !iv_tabledef   TYPE clike
    RETURNING
      VALUE(rr_data) TYPE REF TO data
    RAISING
      cx_udm_message .
  METHODS get_events
    RETURNING
      VALUE(rr_data) TYPE REF TO data
    RAISING
      cx_udm_message .
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_ae_processor
  PUBLIC .


  METHODS check_events
    RAISING
      cx_udm_message .
  METHODS check_relevance
    RETURNING
      VALUE(rv_result) TYPE zif_gtt_ef_types=>tv_condition
    RAISING
      cx_udm_message .
  METHODS get_event_data
    CHANGING
      !ct_eventid_map     TYPE trxas_evtid_evtcnt_map
      !ct_trackingheader  TYPE zif_gtt_ae_types=>tt_trackingheader
      !ct_tracklocation   TYPE zif_gtt_ae_types=>tt_tracklocation
      !ct_trackreferences TYPE zif_gtt_ae_types=>tt_trackreferences
      !ct_trackparameters TYPE zif_gtt_ae_types=>tt_trackparameters
    RAISING
      cx_udm_message .
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_ae_types
  PUBLIC .

  TYPES ts_trackingheader TYPE /saptrx/bapi_evm_header .
  TYPES:
    tt_trackingheader TYPE STANDARD TABLE OF ts_trackingheader .
  TYPES ts_tracklocation TYPE /saptrx/bapi_evm_locationid .
  TYPES:
    tt_tracklocation TYPE STANDARD TABLE OF ts_tracklocation .
  TYPES ts_trackreferences TYPE /saptrx/bapi_evm_reference .
  TYPES:
    tt_trackreferences TYPE STANDARD TABLE OF ts_trackreferences .
  TYPES ts_trackparameters TYPE /saptrx/bapi_evm_parameters .
  TYPES:
    tt_trackparameters TYPE STANDARD TABLE OF ts_trackparameters .
ENDINTERFACE.""",
    r"""interface ZIF_GTT_CTP_TYPES
  public .


  types:
    BEGIN OF ts_evtype,
      trk_obj_type  TYPE /saptrx/trk_obj_type,
      evtype        TYPE /saptrx/evtype,
      trxservername TYPE /saptrx/trxservername,
      eventdatafunc TYPE /saptrx/treventdatafunc,
      evtcnt        TYPE /saptrx/evtcnt,
    END OF ts_evtype .
  types:
    tt_evtype TYPE STANDARD TABLE OF ts_evtype WITH EMPTY KEY .
  types:
    BEGIN OF ts_aotype,
      obj_type    TYPE /saptrx/trk_obj_type,
      aot_type    TYPE /saptrx/aotype,
      server_name TYPE /saptrx/trxservername,
    END OF ts_aotype .
  types:
    tt_aotype TYPE STANDARD TABLE OF ts_aotype WITH EMPTY KEY .
  types:
    tt_trxas_appobj_ctab TYPE STANDARD TABLE OF trxas_appobj_ctab_wa
                                WITH EMPTY KEY .
  types:
    tt_trxas_evt_ctab TYPE STANDARD TABLE OF trxas_evt_ctab_wa
                                WITH EMPTY KEY .
  types:
    tt_aotype_rst  TYPE RANGE OF /saptrx/aotype .
  types:
    tt_evtype_rst  TYPE RANGE OF /saptrx/evtype .
  types:
    tt_trk_obj_type TYPE STANDARD TABLE OF /saptrx/trk_obj_type
                           WITH EMPTY KEY .
  types:
    BEGIN OF ts_idoc_data,
      control      TYPE /saptrx/bapi_trk_control_tab,
      info         TYPE /saptrx/bapi_trk_info_tab,
      tracking_id  TYPE /saptrx/bapi_trk_trkid_tab,
      exp_event    TYPE /saptrx/bapi_trk_ee_tab,
      trxserv      TYPE /saptrx/trxserv,
      appsys       TYPE logsys,
      appobj_ctabs TYPE tt_trxas_appobj_ctab,
    END OF ts_idoc_data .
  types:
    tt_idoc_data TYPE STANDARD TABLE OF ts_idoc_data
                   WITH EMPTY KEY .
  types:
    tt_evm_header     TYPE STANDARD TABLE OF /saptrx/bapi_evm_header WITH EMPTY KEY .
  types:
    tt_evm_locationid TYPE STANDARD TABLE OF /saptrx/bapi_evm_locationid WITH EMPTY KEY .
  types:
    tt_evm_parameters TYPE STANDARD TABLE OF /saptrx/bapi_evm_parameters WITH EMPTY KEY .
  types:
    tt_evm_reference     TYPE STANDARD TABLE OF /saptrx/bapi_evm_reference WITH EMPTY KEY .
  types:
    tt_trxas_evt_ctabs TYPE STANDARD TABLE
                         OF trxas_evt_ctab_wa WITH EMPTY KEY .
  types:
    BEGIN OF ts_idoc_evt_data,
      evt_header     TYPE tt_evm_header,
      evt_locationid TYPE tt_evm_locationid,
      evt_parameters TYPE tt_evm_parameters,
      evt_reference  TYPE tt_evm_reference,
      evt_ctabs      TYPE tt_trxas_evt_ctabs,
      trxserv        TYPE /saptrx/trxserv,
      eventid_map    TYPE trxas_evtid_evtcnt_map,
    END OF ts_idoc_evt_data .
  types:
    tt_idoc_evt_data TYPE STANDARD TABLE OF ts_idoc_evt_data
                   WITH EMPTY KEY .
endinterface.""",
    r"""interface ZIF_GTT_EF_CONSTANTS
  public .


  constants:
    BEGIN OF cs_trxcod, " tracking id type
      po_number         TYPE /saptrx/trxcod VALUE 'FT1_PO',
      po_position       TYPE /saptrx/trxcod VALUE 'FT1_PO_ITEM',
      dl_number         TYPE /saptrx/trxcod VALUE 'FT1_IN_DELIVERY',
      dl_position       TYPE /saptrx/trxcod VALUE 'FT1_IN_DELIVERY_ITEM',
      sh_number         TYPE /saptrx/trxcod VALUE 'FT1_SHIPMENT',
      sh_resource       TYPE /saptrx/trxcod VALUE 'FT1_RESOURCE',
      fu_number         TYPE /saptrx/trxcod VALUE 'FT1_FREIGHT_UNIT',
      sales_order       TYPE /saptrx/trxcod VALUE 'FT1_SALES_ORDER',
      sales_order_item  TYPE /saptrx/trxcod VALUE 'FT1_SALES_ORDER_ITEM',
      out_delivery      TYPE /saptrx/trxcod VALUE 'FT1_OUT_DELIVERY',
      out_delivery_item TYPE /saptrx/trxcod VALUE 'FT1_ODLV_ITEM',
    END OF cs_trxcod .
  constants:
    BEGIN OF cs_milestone,

      po_confirmation     TYPE /saptrx/appl_event_tag VALUE 'CONFIRMATION',
      po_goods_receipt    TYPE /saptrx/appl_event_tag VALUE 'GOODS_RECEIPT',
      po_planned_delivery TYPE /saptrx/appl_event_tag VALUE 'PO_PLANNED_DLV',
      po_itm_completed    TYPE /saptrx/appl_event_tag VALUE 'PO_ITEM_COMPLETED',
      po_deletion         TYPE /saptrx/appl_event_tag VALUE 'DELETION',
      po_undeletion       TYPE /saptrx/appl_event_tag VALUE 'UNDELETION',

      dl_item_completed   TYPE /saptrx/appl_event_tag VALUE 'IDLV_IT_COMPLETED',
      dl_put_away         TYPE /saptrx/appl_event_tag VALUE 'PUT_AWAY',
      dl_packing          TYPE /saptrx/appl_event_tag VALUE 'PACKING',
      dl_goods_receipt    TYPE /saptrx/appl_event_tag VALUE 'GOODS_RECEIPT',
      dl_pod              TYPE /saptrx/appl_event_tag VALUE 'SHP_POD',
      dl_planned_delivery TYPE /saptrx/appl_event_tag VALUE 'IDLV_PLANNED_DLV',

      fu_completed        TYPE /saptrx/appl_event_tag VALUE 'FU_COMPLETED',

      sh_check_in         TYPE /saptrx/appl_event_tag VALUE 'CHECK_IN',
      sh_load_start       TYPE /saptrx/appl_event_tag VALUE 'LOAD_BEGIN',
      sh_load_end         TYPE /saptrx/appl_event_tag VALUE 'LOAD_END',
      sh_departure        TYPE /saptrx/appl_event_tag VALUE 'DEPARTURE',
      sh_arrival          TYPE /saptrx/appl_event_tag VALUE 'ARRIV_DEST',
      sh_pod              TYPE /saptrx/appl_event_tag VALUE 'POD',

      so_item_completed   TYPE /saptrx/appl_event_tag VALUE 'SO_ITEM_COMPLETED',
      so_planned_dlv      TYPE /saptrx/appl_event_tag VALUE 'SO_PLANNED_DLV',
      dlv_hd_completed    TYPE /saptrx/appl_event_tag VALUE 'ODLV_HD_COMPLETED',
      dlv_item_completed  TYPE /saptrx/appl_event_tag VALUE 'ODLV_IT_COMPLETED',
      dlv_item_pod        TYPE /saptrx/appl_event_tag VALUE 'ODLV_ITEM_POD',
      goods_issue         TYPE /saptrx/appl_event_tag VALUE 'GOODS_ISSUE',
      departure           TYPE /saptrx/appl_event_tag VALUE 'DEPARTURE',
      arriv_dest          TYPE /saptrx/appl_event_tag VALUE 'ARRIV_DEST',
      pod                 TYPE /saptrx/appl_event_tag VALUE 'POD',
      picking             TYPE /saptrx/appl_event_tag VALUE 'PICKING',
      packing             TYPE /saptrx/appl_event_tag VALUE 'PACKING',
      odlv_planned_dlv    TYPE /saptrx/appl_event_tag VALUE 'ODLV_PLANNED_DLV',
    END OF cs_milestone .
  constants:
    BEGIN OF cs_event_param,
      quantity           TYPE /saptrx/paramname VALUE 'QUANTITY',
      delivery_completed TYPE /saptrx/paramname VALUE 'DELIVERY_COMPLETED',
      full_volume_mode   TYPE /saptrx/paramname VALUE 'FULL_VOLUME_MODE',
      reversal           TYPE /saptrx/paramname VALUE 'REVERSAL_INDICATOR',
      location_id        TYPE /saptrx/paramname VALUE 'LOCATION_ID',
      location_type      TYPE /saptrx/paramname VALUE 'LOCATION_TYPE',
    END OF cs_event_param .
  constants:
    BEGIN OF cs_trk_obj_type,
      esc_purord TYPE /saptrx/trk_obj_type VALUE 'ESC_PURORD',
      esc_deliv  TYPE /saptrx/trk_obj_type VALUE 'ESC_DELIV',
      esc_shipmt TYPE /saptrx/trk_obj_type VALUE 'ESC_SHIPMT',
      tms_tor    TYPE /saptrx/trk_obj_type VALUE 'TMS_TOR',
      esc_sorder TYPE /saptrx/trk_obj_type VALUE 'ESC_SORDER',
    END OF cs_trk_obj_type .
  constants:
    BEGIN OF cs_system_fields,
      actual_bisiness_timezone  TYPE /saptrx/paramname VALUE 'ACTUAL_BUSINESS_TIMEZONE',
      actual_bisiness_datetime  TYPE /saptrx/paramname VALUE 'ACTUAL_BUSINESS_DATETIME',
      actual_technical_timezone TYPE /saptrx/paramname VALUE 'ACTUAL_TECHNICAL_TIMEZONE',
      actual_technical_datetime TYPE /saptrx/paramname VALUE 'ACTUAL_TECHNICAL_DATETIME',
      reported_by               TYPE /saptrx/paramname VALUE 'REPORTED_BY',
    END OF cs_system_fields .
  constants:
    BEGIN OF cs_errors,
      wrong_parameter     TYPE sotr_conc VALUE '1216f03004ce11ebbf450050c2490048',
      cdata_determination TYPE sotr_conc VALUE '1216f03004ce11ebbf460050c2490048',
      table_determination TYPE sotr_conc VALUE '1216f03004ce11ebbf470050c2490048',
      stop_processing     TYPE sotr_conc VALUE '1216f03004ce11ebbf480050c2490048',
    END OF cs_errors .
  constants:
    BEGIN OF cs_condition,
      true  TYPE zif_gtt_ef_types=>tv_condition VALUE 'T',
      false TYPE zif_gtt_ef_types=>tv_condition VALUE 'F',
    END OF cs_condition .
  constants:
    BEGIN OF cs_change_mode,
      insert    TYPE updkz_d VALUE 'I',
      update    TYPE updkz_d VALUE 'U',
      delete    TYPE updkz_d VALUE 'D',
      undefined TYPE updkz_d VALUE 'T',
    END OF cs_change_mode .
  constants:
    BEGIN OF cs_parameter_id,
      key_field    TYPE zif_gtt_ef_types=>tv_parameter_id VALUE 1,
      no_empty_tag TYPE zif_gtt_ef_types=>tv_parameter_id VALUE 2,
    END OF cs_parameter_id .
  constants CV_AOT type STRING value 'AOT' ##NO_TEXT.
  constants CV_STRUCTURE_PKG type DEVCLASS value '/SAPTRX/SCEM_AI_R3' ##NO_TEXT.
  constants:
    BEGIN OF cs_loc_types,
      businesspartner  TYPE /saptrx/loc_id_type VALUE 'BusinessPartner' ##NO_TEXT,
      customer         TYPE /saptrx/loc_id_type VALUE 'Customer' ##NO_TEXT,
      logisticlocation TYPE /saptrx/loc_id_type VALUE 'LogisticLocation' ##NO_TEXT,
      plant            TYPE /saptrx/loc_id_type VALUE 'Plant' ##NO_TEXT,
      shippingpoint    TYPE /saptrx/loc_id_type VALUE 'ShippingPoint' ##NO_TEXT,
      supplier         TYPE /saptrx/loc_id_type VALUE 'Supplier' ##NO_TEXT,
    END OF cs_loc_types .
  constants:
    BEGIN OF cs_date_types,
      timestamp TYPE char20 VALUE 'TIMESTAMP',
    END OF cs_date_types .
  constants:
    BEGIN OF cs_logs,
*               log_name TYPE balnrext VALUE '',
      BEGIN OF object,
        shipment_ctp TYPE balobj_d VALUE 'SAPTRX',
        delivery_ctp TYPE balobj_d VALUE 'SAPTRX',
        tor_ctp      TYPE balobj_d VALUE 'SAPTRX',
      END OF object,
      BEGIN OF subobject,
        shipment_ctp TYPE balsubobj VALUE 'APPSYS',
        delivery_ctp TYPE balsubobj VALUE 'APPSYS',
        tor_ctp      TYPE balsubobj VALUE 'APPSYS',
      END OF subobject,
    END OF cs_logs .
* combinations of address indicator
  CONSTANTS:
    vbpa_addr_ind_man_all TYPE char04 VALUE 'BCEF',
    shp_addr_ind_man_all  TYPE char03 VALUE 'BCD'.
endinterface.""",
    r"""INTERFACE zif_gtt_ef_parameters
  PUBLIC .


  METHODS get_appsys
    RETURNING
      VALUE(rv_appsys) TYPE /saptrx/applsystem .
  METHODS get_app_obj_types
    RETURNING
      VALUE(rs_app_obj_types) TYPE /saptrx/aotypes .
  METHODS get_app_objects
    RETURNING
      VALUE(rr_data) TYPE REF TO data .
  METHODS get_appl_table
    IMPORTING
      !iv_tabledef   TYPE clike
    RETURNING
      VALUE(rr_data) TYPE REF TO data
    RAISING
      cx_udm_message .
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_ef_processor
  PUBLIC .


  METHODS check_app_objects
    RAISING
      cx_udm_message .
  METHODS check_relevance
    RETURNING
      VALUE(rv_result) TYPE zif_gtt_ef_types=>tv_condition
    RAISING
      cx_udm_message .
  METHODS get_app_obj_type_id
    RETURNING
      VALUE(rv_appobjid) TYPE /saptrx/aoid
    RAISING
      cx_udm_message .
  METHODS get_control_data
    CHANGING
      !ct_control_data TYPE zif_gtt_ef_types=>tt_control_data
    RAISING
      cx_udm_message .
  METHODS get_track_id_data
    EXPORTING
      !et_track_id_data TYPE zif_gtt_ef_types=>tt_track_id_data
    RAISING
      cx_udm_message .
  METHODS get_planned_events
    CHANGING
      !ct_expeventdata TYPE zif_gtt_ef_types=>tt_expeventdata
      !ct_measrmntdata TYPE zif_gtt_ef_types=>tt_measrmntdata
      !ct_infodata     TYPE zif_gtt_ef_types=>tt_infodata
    RAISING
      cx_udm_message .
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_ef_types
  PUBLIC .


  TYPES ts_control_data TYPE /saptrx/control_data .
  TYPES:
    tt_control_data TYPE STANDARD TABLE OF ts_control_data .
  TYPES ts_track_id_data TYPE /saptrx/track_id_data .
  TYPES:
    tt_track_id_data TYPE STANDARD TABLE OF ts_track_id_data .
  TYPES ts_strucdatadef TYPE /saptrx/strucdatadef .
  TYPES:
    tt_strucdatadef TYPE STANDARD TABLE OF ts_strucdatadef .
  TYPES ts_expeventdata TYPE /saptrx/exp_events .
  TYPES:
    tt_expeventdata TYPE STANDARD TABLE OF ts_expeventdata .
  TYPES ts_measrmntdata TYPE /saptrx/measr_data .
  TYPES:
    tt_measrmntdata TYPE STANDARD TABLE OF ts_measrmntdata .
  TYPES ts_infodata TYPE /saptrx/info_data .
  TYPES:
    tt_infodata TYPE STANDARD TABLE OF ts_infodata .
  TYPES:
    BEGIN OF ts_definition,
      maintab   TYPE /saptrx/strucdatadef,
      mastertab TYPE /saptrx/strucdatadef,
    END OF ts_definition .
  TYPES tv_parameter_id TYPE i .
  TYPES tv_parameter_value TYPE char50 .
  TYPES tv_condition TYPE sy-binpt .
  TYPES tv_field_name TYPE char20 .
  TYPES:
    tt_field_name TYPE STANDARD TABLE OF tv_field_name
                              WITH EMPTY KEY .
  TYPES tv_currency_amnt TYPE bapicurr_d .
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_pe_filler
  PUBLIC .


  METHODS check_relevance
    IMPORTING
      !is_app_objects  TYPE trxas_appobj_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE zif_gtt_ef_types=>tv_condition
    RAISING
      cx_udm_message .
  METHODS get_planed_events
    IMPORTING
      !is_app_objects  TYPE trxas_appobj_ctab_wa
    CHANGING
      !ct_expeventdata TYPE zif_gtt_ef_types=>tt_expeventdata
      !ct_measrmntdata TYPE zif_gtt_ef_types=>tt_measrmntdata
      !ct_infodata     TYPE zif_gtt_ef_types=>tt_infodata
    RAISING
      cx_udm_message .
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_tp_factory
  PUBLIC .


  METHODS get_tp_reader
    IMPORTING
      !io_ef_parameters   TYPE REF TO zif_gtt_ef_parameters
    RETURNING
      VALUE(ro_bo_reader) TYPE REF TO zif_gtt_tp_reader .
  METHODS get_ef_parameters
    IMPORTING
      !iv_appsys              TYPE /saptrx/applsystem
      !is_app_obj_types       TYPE /saptrx/aotypes
      !it_all_appl_tables     TYPE trxas_tabcontainer
      !it_app_type_cntl_tabs  TYPE trxas_apptype_tabs OPTIONAL
      !it_app_objects         TYPE trxas_appobj_ctabs
    RETURNING
      VALUE(ro_ef_parameters) TYPE REF TO zif_gtt_ef_parameters .
  METHODS get_ef_processor
    IMPORTING
      !is_definition         TYPE zif_gtt_ef_types=>ts_definition
      !io_bo_factory         TYPE REF TO zif_gtt_tp_factory
      !iv_appsys             TYPE /saptrx/applsystem
      !is_app_obj_types      TYPE /saptrx/aotypes
      !it_all_appl_tables    TYPE trxas_tabcontainer
      !it_app_type_cntl_tabs TYPE trxas_apptype_tabs
      !it_app_objects        TYPE trxas_appobj_ctabs
    RETURNING
      VALUE(ro_ef_processor) TYPE REF TO zif_gtt_ef_processor
    RAISING
      cx_udm_message .
  METHODS get_pe_filler
    IMPORTING
      !io_ef_parameters   TYPE REF TO zif_gtt_ef_parameters
      !io_bo_reader       TYPE REF TO zif_gtt_tp_reader
    RETURNING
      VALUE(ro_pe_filler) TYPE REF TO zif_gtt_pe_filler
    RAISING
      cx_udm_message .
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_tp_reader
  PUBLIC .


  METHODS check_relevance
    IMPORTING
      !is_app_object   TYPE trxas_appobj_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE zif_gtt_ef_types=>tv_condition
    RAISING
      cx_udm_message .
  METHODS get_app_obj_type_id
    IMPORTING
      !is_app_object     TYPE trxas_appobj_ctab_wa
    RETURNING
      VALUE(rv_appobjid) TYPE /saptrx/aoid
    RAISING
      cx_udm_message .
  METHODS get_data
    IMPORTING
      !is_app_object TYPE trxas_appobj_ctab_wa
    RETURNING
      VALUE(rr_data) TYPE REF TO data
    RAISING
      cx_udm_message .
  METHODS get_data_old
    IMPORTING
      !is_app_object TYPE trxas_appobj_ctab_wa
    RETURNING
      VALUE(rr_data) TYPE REF TO data
    RAISING
      cx_udm_message .
  METHODS get_field_parameter
    IMPORTING
      !iv_field_name   TYPE clike
      !iv_parameter    TYPE zif_gtt_ef_types=>tv_parameter_id
    RETURNING
      VALUE(rv_result) TYPE zif_gtt_ef_types=>tv_parameter_value
    RAISING
      cx_udm_message .
  METHODS get_mapping_structure
    RETURNING
      VALUE(rr_data) TYPE REF TO data .
  METHODS get_track_id_data
    IMPORTING
      !is_app_object    TYPE trxas_appobj_ctab_wa
    EXPORTING
      !et_track_id_data TYPE zif_gtt_ef_types=>tt_track_id_data
    RAISING
      cx_udm_message .
ENDINTERFACE.""",
    r"""class ZCL_GTT_MIA_AE_FACTORY_DLH_GR definition
  public
  inheriting from ZCL_GTT_AE_FACTORY
  create public .

public section.

  methods ZIF_GTT_AE_FACTORY~GET_AE_FILLER
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_AE_FACTORY_DLH_GR IMPLEMENTATION.


  METHOD ZIF_GTT_AE_FACTORY~GET_AE_FILLER.

    ro_ae_filler = NEW zcl_gtt_mia_ae_filler_dlh_gr(
      io_ae_parameters = io_ae_parameters ).

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_CTP_DAT_TOR_TO_DLH definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IT_DELIVERY_CHNG type ZIF_GTT_MIA_CTP_TYPES=>TT_DELIVERY_CHNG
      !IT_TOR_ROOT type /SCMTMS/T_EM_BO_TOR_ROOT
      !IT_TOR_ITEM type /SCMTMS/T_EM_BO_TOR_ITEM .
  methods GET_DELIVERIES
    returning
      value(RR_DELIVERIES) type ref to DATA .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_delivery TYPE zif_gtt_mia_ctp_types=>tt_delivery .

    METHODS fill_delivery_data
      IMPORTING
        !it_delivery_chng TYPE zif_gtt_mia_ctp_types=>tt_delivery_chng
        !it_tor_root      TYPE /scmtms/t_em_bo_tor_root
        !it_tor_item      TYPE /scmtms/t_em_bo_tor_item
      CHANGING
        !ct_delivery      TYPE zif_gtt_mia_ctp_types=>tt_delivery .
    METHODS init_delivery_headers
      IMPORTING
        !it_delivery_chng TYPE zif_gtt_mia_ctp_types=>tt_delivery_chng
        !it_tor_root      TYPE /scmtms/t_em_bo_tor_root
        !it_tor_item      TYPE /scmtms/t_em_bo_tor_item .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_CTP_DAT_TOR_TO_DLH IMPLEMENTATION.


  METHOD constructor.

    IF it_delivery_chng[] IS NOT INITIAL.
      init_delivery_headers(
        EXPORTING
          it_delivery_chng = it_delivery_chng
          it_tor_root      = it_tor_root
          it_tor_item      = it_tor_item ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_delivery_data.

    DATA: lt_likp        TYPE HASHED TABLE OF zif_gtt_mia_app_types=>ts_likpvb
                           WITH UNIQUE KEY vbeln,
          lt_lips        TYPE SORTED TABLE OF zif_gtt_mia_app_types=>ts_lipsvb
                           WITH UNIQUE KEY vbeln posnr,
          ls_lips        TYPE zif_gtt_mia_app_types=>ts_lipsvb,
          lv_base_btd_id TYPE /scmtms/base_btd_id.

    IF ct_delivery[] IS NOT INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_likp
        FROM likp
        FOR ALL ENTRIES IN ct_delivery
        WHERE vbeln = ct_delivery-vbeln.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_lips
        FROM lips
        FOR ALL ENTRIES IN ct_delivery
        WHERE vbeln = ct_delivery-vbeln.

      LOOP AT ct_delivery ASSIGNING FIELD-SYMBOL(<ls_delivery>).
        " copy selected delivery header
        <ls_delivery>-likp  = VALUE #( lt_likp[ KEY primary_key
                                                COMPONENTS vbeln = <ls_delivery>-vbeln ]
                                         DEFAULT VALUE #( vbeln = <ls_delivery>-vbeln ) ).

        " copy selected delivery items
        LOOP AT lt_lips ASSIGNING FIELD-SYMBOL(<ls_lips>)
          USING KEY primary_key
          WHERE vbeln = <ls_delivery>-vbeln.

          APPEND <ls_lips> TO <ls_delivery>-lips.
        ENDLOOP.

        " when data is not stored in DB yet, just take it from TOR tables
        IF sy-subrc <> 0.
          lv_base_btd_id  = |{ <ls_delivery>-vbeln ALPHA = IN }|.

          LOOP AT it_tor_item ASSIGNING FIELD-SYMBOL(<ls_tor_item>)
            WHERE base_btd_tco = zif_gtt_mia_ctp_tor_constants=>cv_base_btd_tco_inb_dlv
              AND base_btd_id  = lv_base_btd_id
              AND base_btditem_id IS NOT INITIAL.

            ASSIGN it_tor_root[ node_id = <ls_tor_item>-parent_node_id ]
              TO FIELD-SYMBOL(<ls_tor_root>).

            IF sy-subrc = 0 AND
               <ls_tor_root>-tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit.

              ls_lips-vbeln   = |{ <ls_tor_item>-base_btd_id ALPHA = IN }|.
              ls_lips-posnr   = |{ <ls_tor_item>-base_btditem_id ALPHA = IN }|.

              " check whether DLV item already exists
              READ TABLE <ls_delivery>-lips
                WITH KEY vbeln = ls_lips-vbeln
                         posnr = ls_lips-posnr
                         TRANSPORTING NO FIELDS
                         BINARY SEARCH.

              " no row -> add it
              IF sy-subrc <> 0.
                INSERT ls_lips INTO <ls_delivery>-lips INDEX sy-tabix.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD get_deliveries.

    rr_deliveries   = REF #( mt_delivery ).

  ENDMETHOD.


  METHOD init_delivery_headers.

    DATA: ls_dlv_head TYPE zif_gtt_mia_ctp_types=>ts_delivery.

    CLEAR: mt_delivery[].

    " collect DLV Headers from DLV Items
    LOOP AT it_delivery_chng ASSIGNING FIELD-SYMBOL(<ls_delivery_chng>).
      READ TABLE mt_delivery TRANSPORTING NO FIELDS
        WITH KEY vbeln  = <ls_delivery_chng>-vbeln
        BINARY SEARCH.

      IF sy-subrc <> 0.
        ls_dlv_head-vbeln = <ls_delivery_chng>-vbeln.
        INSERT ls_dlv_head INTO mt_delivery INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    " enrich DLV Headers with data from DB and TOR internal tables
    fill_delivery_data(
      EXPORTING
        it_delivery_chng = it_delivery_chng
        it_tor_root      = it_tor_root
        it_tor_item      = it_tor_item
      CHANGING
        ct_delivery      = mt_delivery ).

    LOOP AT mt_delivery ASSIGNING FIELD-SYMBOL(<ls_delivery>).
      TRY.
          <ls_delivery>-fu_relevant   = zcl_gtt_mia_tm_tools=>is_fu_relevant(
                                          it_lips = CORRESPONDING #( <ls_delivery>-lips ) ).
        CATCH cx_udm_message.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_CTP_SHIPMENT_DATA definition
  public
  create public .

public section.

  types:
    BEGIN OF ts_likpdl,
        tknum TYPE vttk-tknum,
        vbeln TYPE likp-vbeln,
        updkz TYPE likpvb-updkz,
      END OF ts_likpdl .
  types:
    BEGIN OF ts_vbfaex,
        tknum TYPE vttk-tknum.
        INCLUDE TYPE vbfavb.
    TYPES: END OF ts_vbfaex .
  types:
    BEGIN OF ts_likpex,
        tknum TYPE vttk-tknum.
        INCLUDE TYPE likp.
    TYPES: END OF ts_likpex .
  types:
    BEGIN OF ts_stops,
        tknum    TYPE vttk-tknum,
        stops    TYPE zif_gtt_mia_app_types=>tt_stops,
        watching TYPE zif_gtt_mia_app_types=>tt_dlv_watch_stops,
      END OF ts_stops .
  types TS_VTTKVB type VTTKVB .
  types TS_LIPS type LIPS .
  types TS_EE_REL type ZGTT_MIA_EE_REL .
  types:
    tt_vttkvb_srt TYPE SORTED TABLE OF ts_vttkvb
                             WITH UNIQUE KEY tknum .
  types:
    tt_vttpvb_srt TYPE SORTED TABLE OF vttpvb
                             WITH NON-UNIQUE KEY tknum tpnum .
  types:
    tt_vttsvb_srt TYPE SORTED TABLE OF vttsvb
                             WITH NON-UNIQUE KEY tknum tsnum .
  types:
    tt_vtspvb_srt TYPE SORTED TABLE OF vtspvb
                             WITH UNIQUE KEY tknum tsnum tpnum
                             WITH NON-UNIQUE SORTED KEY vtts
                               COMPONENTS tknum tsnum .
  types:
    tt_vbfaex_srt TYPE SORTED TABLE OF ts_vbfaex
                             WITH NON-UNIQUE KEY tknum vbelv vbeln .
  types:
    tt_likpex_srt TYPE SORTED TABLE OF ts_likpex
                             WITH UNIQUE KEY tknum vbeln .
  types:
    tt_likpdl_srt TYPE SORTED TABLE OF ts_likpdl
                             WITH UNIQUE KEY vbeln tknum .
  types:
    tt_lips_srt   TYPE SORTED TABLE OF ts_lips
                             WITH UNIQUE KEY vbeln posnr .
  types:
    tt_stops_srt  TYPE SORTED TABLE OF ts_stops
                             WITH UNIQUE KEY tknum .
  types:
    tt_ee_rel_srt TYPE SORTED TABLE OF ts_ee_rel
                             WITH UNIQUE KEY appobjid .
  types:
    BEGIN OF MESH ts_shipment_merge,
        vttk     TYPE tt_vttkvb_srt
                 ASSOCIATION vttp TO vttp ON tknum = tknum
                 ASSOCIATION vttp_dlt TO vttp_dlt ON tknum = tknum,
        vttp     TYPE tt_vttpvb_srt,
        vttp_dlt TYPE tt_vttpvb_srt,
        vtts     TYPE tt_vttsvb_srt,
        vtts_dlt TYPE tt_vttsvb_srt
                 ASSOCIATION vtsp TO vtsp ON tknum = tknum
                                         AND tsnum = tsnum,
        vtsp     TYPE tt_vtspvb_srt
                 ASSOCIATION vttp TO vttp ON tknum = tknum
                                         AND tpnum = tpnum,
        vtsp_dlt TYPE tt_vtspvb_srt
                 ASSOCIATION vttp TO vttp ON tknum = tknum
                                         AND tpnum = tpnum,
        vbfa     TYPE tt_vbfaex_srt,
        likp     TYPE tt_likpex_srt
                 ASSOCIATION vbfa TO vbfa ON tknum = tknum
                                         AND vbelv = vbeln
                 ASSOCIATION likp_dlt TO likp_dlt ON vbeln = vbeln
                 ASSOCIATION lips TO lips ON vbeln = vbeln,
        lips     TYPE tt_lips_srt,
        likp_dlt TYPE tt_likpdl_srt,
        ee_rel   TYPE tt_ee_rel_srt,
      END OF MESH ts_shipment_merge .

  methods CONSTRUCTOR
    importing
      !IS_SHIPMENT type CXSHIPMENT
    raising
      CX_UDM_MESSAGE .
  methods GET_DATA
    returning
      value(RR_SHIP) type ref to DATA .
  methods GET_STOPS
    returning
      value(RR_STOPS) type ref to DATA .
  PROTECTED SECTION.
private section.

  types:
    tt_tknum   TYPE RANGE OF tknum .

  data MS_SHIP type TS_SHIPMENT_MERGE .
  data MT_STOPS type TT_STOPS_SRT .

  methods GET_VTTK_MERGED_DATA
    importing
      !IS_SHIPMENT type CXSHIPMENT
    exporting
      !ET_VTTK type TT_VTTKVB_SRT
    raising
      CX_UDM_MESSAGE .
  methods GET_VTTP_MERGED_DATA
    importing
      !IS_SHIPMENT type CXSHIPMENT
      !IT_TKNUM type TT_TKNUM
    exporting
      !ET_VTTP_FULL type TT_VTTPVB_SRT
      !ET_VTTP_DELTA type TT_VTTPVB_SRT
    raising
      CX_UDM_MESSAGE .
  methods GET_VTTS_MERGED_DATA
    importing
      !IS_SHIPMENT type CXSHIPMENT
      !IT_TKNUM type TT_TKNUM
    exporting
      !ET_VTTS_FULL type TT_VTTSVB_SRT
      !ET_VTTS_DELTA type TT_VTTSVB_SRT
    raising
      CX_UDM_MESSAGE .
  methods GET_VTSP_MERGED_DATA
    importing
      !IS_SHIPMENT type CXSHIPMENT
      !IT_TKNUM type TT_TKNUM
    exporting
      !ET_VTSP_FULL type TT_VTSPVB_SRT
      !ET_VTSP_DELTA type TT_VTSPVB_SRT
    raising
      CX_UDM_MESSAGE .
  methods GET_LIKP_DELTA_DATA
    importing
      !IS_SHIP type TS_SHIPMENT_MERGE
      !IT_TKNUM type TT_TKNUM
      !IS_SHIPMENT type CXSHIPMENT optional
    exporting
      !ET_LIKP_DELTA type TT_LIKPDL_SRT
    raising
      CX_UDM_MESSAGE .
  methods GET_VBFA_AND_LIKP_DATA
    importing
      !IS_SHIP type TS_SHIPMENT_MERGE
    exporting
      !ET_VBFA type TT_VBFAEX_SRT
      !ET_LIKP type TT_LIKPEX_SRT
    raising
      CX_UDM_MESSAGE .
  methods GET_LIPS_DATA
    importing
      !IS_SHIP type TS_SHIPMENT_MERGE
    exporting
      !ET_LIPS type TT_LIPS_SRT
    raising
      CX_UDM_MESSAGE .
  methods GET_EE_REL_DATA
    importing
      !IS_SHIP type TS_SHIPMENT_MERGE
    exporting
      !ET_EE_REL type TT_EE_REL_SRT
    raising
      CX_UDM_MESSAGE .
  methods INIT_SHIPMENT_DATA
    importing
      !IS_SHIPMENT type CXSHIPMENT
    exporting
      !ES_SHIP type TS_SHIPMENT_MERGE
    raising
      CX_UDM_MESSAGE .
  methods INIT_STOPS_DATA
    importing
      !IS_SHIPMENT type CXSHIPMENT
      !IS_SHIP type TS_SHIPMENT_MERGE
    exporting
      !ET_STOPS type TT_STOPS_SRT
    raising
      CX_UDM_MESSAGE .
  methods IS_VTTS_CHANGED
    importing
      !IS_SHIPMENT type CXSHIPMENT
      !IS_VTTS type VTTSVB
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_CTP_SHIPMENT_DATA IMPLEMENTATION.


  METHOD constructor.

    init_shipment_data(
      EXPORTING
        is_shipment = is_shipment
      IMPORTING
        es_ship     = ms_ship ).

    init_stops_data(
      EXPORTING
        is_shipment = is_shipment
        is_ship     = ms_ship
      IMPORTING
        et_stops    = mt_stops ).

  ENDMETHOD.


  METHOD get_data.

    rr_ship   = REF #( ms_ship ).

  ENDMETHOD.


  METHOD get_ee_rel_data.

    DATA: lt_appobjid TYPE STANDARD TABLE OF ts_ee_rel-appobjid.

    CLEAR: et_ee_rel.

    lt_appobjid   = VALUE #( FOR ls_likp IN is_ship-likp
                             ( zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
                                 ir_likp = REF #( ls_likp ) ) ) ).

    lt_appobjid   = VALUE #( BASE lt_appobjid
                             FOR ls_lips IN is_ship-lips
                             ( zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
                                 ir_lips = REF #( ls_lips ) ) ) ).

    IF lt_appobjid[] IS NOT INITIAL.
      SELECT *
        INTO TABLE et_ee_rel
        FROM zgtt_mia_ee_rel
        FOR ALL ENTRIES IN lt_appobjid
        WHERE appobjid = lt_appobjid-table_line.
    ENDIF.

  ENDMETHOD.


  METHOD get_likp_delta_data.

    DATA:
      ls_likp_delta TYPE ts_likpdl,
      lt_updkz      TYPE RANGE OF updkz_d,
      lv_likp_flg   TYPE flag,
      lt_vttp       TYPE vttpvb_tab.

    CLEAR: et_likp_delta[].

    FIELD-SYMBOLS: <ls_vttp> TYPE vttpvb,
                   <ls_vtsp> TYPE vtspvb,
                   <ls_vtts> TYPE vttsvb.

    LOOP AT is_ship-vttp_dlt ASSIGNING <ls_vttp>
      WHERE updkz = zif_gtt_ef_constants=>cs_change_mode-insert
         OR updkz = zif_gtt_ef_constants=>cs_change_mode-delete.

      IF NOT line_exists( et_likp_delta[ KEY primary_key
                                         COMPONENTS tknum = <ls_vttp>-tknum
                                                    vbeln = <ls_vttp>-vbeln ] ).
        ls_likp_delta = CORRESPONDING #( <ls_vttp> ).
        INSERT ls_likp_delta INTO TABLE et_likp_delta.
      ENDIF.
    ENDLOOP.

    LOOP AT is_ship-vtsp_dlt ASSIGNING <ls_vtsp>
      WHERE updkz = zif_gtt_ef_constants=>cs_change_mode-insert
         OR updkz = zif_gtt_ef_constants=>cs_change_mode-delete.
      ASSIGN is_ship-vtsp_dlt\vttp[ <ls_vtsp> ] TO <ls_vttp>.

      IF sy-subcs = 0.
        IF NOT line_exists( et_likp_delta[ KEY primary_key
                                           COMPONENTS tknum = <ls_vttp>-tknum
                                                      vbeln = <ls_vttp>-vbeln ] ).
          ls_likp_delta = CORRESPONDING #( <ls_vttp> ).
          INSERT ls_likp_delta INTO TABLE et_likp_delta.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT is_ship-vtts_dlt ASSIGNING <ls_vtts>.
      LOOP AT is_ship-vtts_dlt\vtsp[ <ls_vtts> ] ASSIGNING <ls_vtsp>.
        ASSIGN is_ship-vtsp_dlt\vttp[ <ls_vtsp> ] TO <ls_vttp>.

        IF sy-subcs = 0.
          IF NOT line_exists( et_likp_delta[ KEY primary_key
                                             COMPONENTS tknum = <ls_vttp>-tknum
                                                        vbeln = <ls_vttp>-vbeln ] ).
            ls_likp_delta = CORRESPONDING #( <ls_vttp> ).
            INSERT ls_likp_delta INTO TABLE et_likp_delta.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

*   Additional logic for add planned destination arrival event
*   1.Check the check-in date and time in shipment is changed or not
*   2.If Shipment Item(VTTP) contains newly insert or delete item,all of the delivery should be send to GTT again
*   because we need to generate new planned destination arrival event for these delivery
    lt_updkz  = VALUE #(
      (
        sign = 'I'
        option = 'EQ'
        low = zif_gtt_ef_constants=>cs_change_mode-insert
      )
      (
        sign = 'I'
        option = 'EQ'
        low = zif_gtt_ef_constants=>cs_change_mode-delete
      ) ).

    APPEND LINES OF is_shipment-new_vttp TO lt_vttp.
    APPEND LINES OF is_shipment-old_vttp TO lt_vttp.
    LOOP AT lt_vttp TRANSPORTING NO FIELDS
      WHERE tknum IN it_tknum
        AND updkz IN lt_updkz.
      lv_likp_flg = abap_true.
      EXIT.
    ENDLOOP.

    LOOP AT lt_vttp INTO DATA(ls_vttp)
      WHERE tknum IN it_tknum.

      READ TABLE is_shipment-new_vttk INTO DATA(ls_vttk_new)
        WITH KEY tknum = ls_vttp-tknum.

      READ TABLE is_shipment-old_vttk INTO DATA(ls_vttk_old)
        WITH KEY tknum = ls_vttp-tknum.
      IF ls_vttk_new-dpreg <> ls_vttk_old-dpreg OR ls_vttk_new-upreg <> ls_vttk_old-upreg
        OR lv_likp_flg = abap_true.
        IF NOT line_exists( et_likp_delta[ KEY primary_key
                                           COMPONENTS tknum = ls_vttp-tknum
                                                      vbeln = ls_vttp-vbeln ] ).
          ls_likp_delta = CORRESPONDING #( ls_vttp ).
          INSERT ls_likp_delta INTO TABLE et_likp_delta.
        ENDIF.
      ENDIF.
      CLEAR:
        ls_vttk_new,
        ls_vttk_old,
        ls_likp_delta.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_lips_data.

    DATA: ls_lips   TYPE lips.

    CLEAR: et_lips[].

    IF is_ship-likp[] IS NOT INITIAL.
      SELECT * INTO ls_lips
        FROM lips
        FOR ALL ENTRIES IN is_ship-likp
        WHERE vbeln = is_ship-likp-vbeln.

        READ TABLE is_ship-likp INTO DATA(ls_likp) WITH KEY vbeln = ls_lips-vbeln.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        IF zcl_gtt_tools=>is_appropriate_dl_item( ir_likp = REF #( ls_likp ) ir_lips = REF #( ls_lips ) ) = abap_true.
          INSERT ls_lips INTO TABLE et_lips.
        ENDIF.
      ENDSELECT.
    ENDIF.

  ENDMETHOD.


  METHOD get_stops.

    rr_stops  = REF #( mt_stops ).

  ENDMETHOD.


  METHOD get_vbfa_and_likp_data.

    DATA: ls_comwa6 TYPE vbco6,
          lt_vbfas  TYPE STANDARD TABLE OF vbfas,
          lv_vbeln  TYPE likp-vbeln,
          ls_vbfa   TYPE ts_vbfaex,
          ls_likp   TYPE ts_likpex,
          ls_vttk   TYPE vttk.

    CLEAR: et_vbfa[], et_likp[].

    LOOP AT is_ship-likp_dlt ASSIGNING FIELD-SYMBOL(<ls_likp_dlt>).
      CLEAR: ls_comwa6, lt_vbfas.
      MOVE-CORRESPONDING <ls_likp_dlt> TO ls_comwa6.

      CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
        EXPORTING
          comwa         = ls_comwa6
        TABLES
          vbfa_tab      = lt_vbfas
        EXCEPTIONS
          no_vbfa       = 1
          no_vbuk_found = 2
          OTHERS        = 3.

      IF sy-subrc = 0.
        LOOP AT lt_vbfas ASSIGNING FIELD-SYMBOL(<ls_vbfas>)
          WHERE vbtyp_n = zif_gtt_mia_app_constants=>cs_vbtyp-shipment
            AND vbtyp_v = zif_gtt_mia_app_constants=>cs_vbtyp-delivery.

          SELECT SINGLE *
            INTO ls_vttk
            FROM vttk
            WHERE tknum = <ls_vbfas>-vbeln.

          IF sy-subrc = 0 AND
             zcl_gtt_mia_sh_tools=>is_appropriate_type(
               ir_vttk = REF #( ls_vttk ) ) = abap_true.

            SELECT SINGLE *
              INTO CORRESPONDING FIELDS OF ls_likp
              FROM likp
              WHERE vbeln = <ls_vbfas>-vbelv.

            IF sy-subrc = 0 AND
               zcl_gtt_tools=>is_appropriate_dl_type(
                 ir_likp = REF #( ls_likp ) ) = abap_true.

              ls_vbfa       = CORRESPONDING #( <ls_vbfas> ).
              ls_vbfa-tknum = <ls_likp_dlt>-tknum.

              INSERT ls_vbfa INTO TABLE et_vbfa.

              IF NOT line_exists( et_likp[ KEY primary_key
                                           COMPONENTS tknum = <ls_likp_dlt>-tknum
                                                      vbeln = ls_likp-vbeln ] ).
                ls_likp-tknum = <ls_likp_dlt>-tknum.
                INSERT ls_likp INTO TABLE et_likp.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    LOOP AT is_ship-vttp_dlt ASSIGNING FIELD-SYMBOL(<ls_vttp_dlt>).
      " DELETED
      IF <ls_vttp_dlt>-updkz = zif_gtt_ef_constants=>cs_change_mode-delete.
        DELETE et_vbfa
          WHERE vbeln = <ls_vttp_dlt>-tknum
            AND vbelv = <ls_vttp_dlt>-vbeln.

        " ADDED
      ELSEIF <ls_vttp_dlt>-updkz = zif_gtt_ef_constants=>cs_change_mode-insert AND
             NOT line_exists( et_vbfa[ KEY primary_key
                                       COMPONENTS tknum = <ls_vttp_dlt>-tknum
                                                  vbeln = <ls_vttp_dlt>-tknum
                                                  vbelv = <ls_vttp_dlt>-vbeln ] ).

        SELECT SINGLE *
          INTO CORRESPONDING FIELDS OF ls_likp
          FROM likp
          WHERE vbeln = <ls_vttp_dlt>-vbeln.

        IF sy-subrc = 0 AND
               zcl_gtt_tools=>is_appropriate_dl_type(
                 ir_likp = REF #( ls_likp ) ) = abap_true.

          ls_vbfa-tknum = <ls_vttp_dlt>-tknum.
          ls_vbfa-vbelv = <ls_vttp_dlt>-vbeln.
          ls_vbfa-vbeln = <ls_vttp_dlt>-tknum.
          INSERT ls_vbfa INTO TABLE et_vbfa.

          IF NOT line_exists( et_likp[ KEY primary_key
                                       COMPONENTS tknum = <ls_vttp_dlt>-tknum
                                                  vbeln = ls_likp-vbeln ] ).
            ls_likp-tknum   = <ls_vttp_dlt>-tknum.
            INSERT ls_likp INTO TABLE et_likp.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_vtsp_merged_data.

    FIELD-SYMBOLS: <ls_vtsp>  TYPE vtspvb.
    CLEAR: et_vtsp_delta[].

    et_vtsp_full  = is_shipment-new_vtsp[].

    LOOP AT is_shipment-old_vtsp ASSIGNING <ls_vtsp>
      WHERE updkz = zif_gtt_ef_constants=>cs_change_mode-delete.

      et_vtsp_full  = VALUE #( BASE et_vtsp_full
                               ( <ls_vtsp> ) ).
    ENDLOOP.

    LOOP AT et_vtsp_full ASSIGNING <ls_vtsp>
      WHERE tknum IN it_tknum
        AND ( updkz = zif_gtt_ef_constants=>cs_change_mode-insert
           OR updkz = zif_gtt_ef_constants=>cs_change_mode-delete ).

      et_vtsp_delta   = VALUE #( BASE et_vtsp_delta
                                 ( <ls_vtsp> ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_vttk_merged_data.

    CLEAR: et_vttk[].

    " collect all the current shipments
    LOOP AT is_shipment-new_vttk ASSIGNING FIELD-SYMBOL(<ls_vttk_new>).
      IF zcl_gtt_mia_sh_tools=>is_appropriate_type(
           ir_vttk = REF #( <ls_vttk_new> ) ) = abap_true.

        et_vttk   = VALUE #( BASE et_vttk
                             ( <ls_vttk_new> ) ).
      ENDIF.
    ENDLOOP.

    " add deleted shipments
    LOOP AT is_shipment-old_vttk ASSIGNING FIELD-SYMBOL(<ls_vttk_old>)
      WHERE updkz = zif_gtt_ef_constants=>cs_change_mode-delete.

      IF zcl_gtt_mia_sh_tools=>is_appropriate_type(
           ir_vttk = REF #( <ls_vttk_old> ) ) = abap_true.

        et_vttk   = VALUE #( BASE et_vttk
                             ( <ls_vttk_old> ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_vttp_merged_data.

    FIELD-SYMBOLS: <ls_vttp>  TYPE vttpvb.
    CLEAR: et_vttp_delta[].

    et_vttp_full  = is_shipment-new_vttp[].

    LOOP AT is_shipment-old_vttp ASSIGNING <ls_vttp>
      WHERE updkz = zif_gtt_ef_constants=>cs_change_mode-delete.

      et_vttp_full  = VALUE #( BASE et_vttp_full
                               ( <ls_vttp> ) ).
    ENDLOOP.

    LOOP AT et_vttp_full ASSIGNING <ls_vttp>
      WHERE tknum IN it_tknum
        AND ( updkz = zif_gtt_ef_constants=>cs_change_mode-insert
           OR updkz = zif_gtt_ef_constants=>cs_change_mode-delete ).

      et_vttp_delta   = VALUE #( BASE et_vttp_delta
                                 ( <ls_vttp> ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_vtts_merged_data.

    FIELD-SYMBOLS: <ls_vtts>  TYPE vttsvb.
    CLEAR: et_vtts_delta[].

    et_vtts_full  = is_shipment-new_vtts[].

    LOOP AT is_shipment-old_vtts ASSIGNING <ls_vtts>
      WHERE updkz = zif_gtt_ef_constants=>cs_change_mode-delete.

      et_vtts_full  = VALUE #( BASE et_vtts_full
                               ( <ls_vtts> ) ).
    ENDLOOP.

    LOOP AT et_vtts_full ASSIGNING <ls_vtts>
      WHERE tknum IN it_tknum.

      IF is_vtts_changed( is_shipment = is_shipment
                          is_vtts     = <ls_vtts> ) = abap_true.

        et_vtts_delta   = VALUE #( BASE et_vtts_delta
                                   ( <ls_vtts> ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD init_shipment_data.

    DATA: lt_tknum    TYPE tt_tknum.

    CLEAR: es_ship.

    get_vttk_merged_data(
      EXPORTING
        is_shipment = is_shipment
      IMPORTING
        et_vttk     = es_ship-vttk ).

    lt_tknum    = VALUE #( FOR ls_vttk IN es_ship-vttk
                           ( low    = ls_vttk-tknum
                             sign   = 'I'
                             option = 'EQ' ) ).

    get_vttp_merged_data(
      EXPORTING
        is_shipment   = is_shipment
        it_tknum      = lt_tknum
      IMPORTING
        et_vttp_full  = es_ship-vttp
        et_vttp_delta = es_ship-vttp_dlt ).

    get_vtts_merged_data(
      EXPORTING
        is_shipment   = is_shipment
        it_tknum      = lt_tknum
      IMPORTING
        et_vtts_full  = es_ship-vtts
        et_vtts_delta = es_ship-vtts_dlt ).

    get_vtsp_merged_data(
      EXPORTING
        is_shipment   = is_shipment
        it_tknum      = lt_tknum
      IMPORTING
        et_vtsp_full  = es_ship-vtsp
        et_vtsp_delta = es_ship-vtsp_dlt ).

    get_likp_delta_data(
      EXPORTING
        is_ship       = es_ship
        it_tknum      = lt_tknum
        is_shipment   = is_shipment
      IMPORTING
        et_likp_delta = es_ship-likp_dlt ).

    get_vbfa_and_likp_data(
      EXPORTING
        is_ship       = es_ship
      IMPORTING
        et_vbfa       = es_ship-vbfa
        et_likp       = es_ship-likp ).

    get_lips_data(
      EXPORTING
        is_ship       = es_ship
      IMPORTING
        et_lips       = es_ship-lips ).

    get_ee_rel_data(
      EXPORTING
        is_ship       = es_ship
      IMPORTING
        et_ee_rel     = es_ship-ee_rel ).

  ENDMETHOD.


  METHOD init_stops_data.

    DATA: lt_stops    TYPE zif_gtt_mia_app_types=>tt_stops,
          lt_watching TYPE zif_gtt_mia_app_types=>tt_dlv_watch_stops.

    FIELD-SYMBOLS: <ls_stops> TYPE ts_stops.

    CLEAR: et_stops[].

    "initiate table
    LOOP AT is_ship-vttk ASSIGNING FIELD-SYMBOL(<ls_vttk>).
      et_stops    = VALUE #( BASE et_stops
                             ( tknum = <ls_vttk>-tknum ) ).
    ENDLOOP.

    LOOP AT is_ship-vbfa ASSIGNING FIELD-SYMBOL(<ls_vbfa>).
      CLEAR: lt_stops[], lt_watching[].

      IF <ls_vbfa>-vbeln = <ls_vbfa>-tknum.
        zcl_gtt_mia_sh_tools=>get_stops_from_shipment(
          EXPORTING
            iv_tknum              = <ls_vbfa>-tknum
            it_vtts               = is_shipment-new_vtts
            it_vtsp               = is_shipment-new_vtsp
            it_vttp               = is_shipment-new_vttp
          IMPORTING
            et_stops              = lt_stops
            et_dlv_watching_stops = lt_watching ).

      ELSE.
        zcl_gtt_mia_sh_tools=>get_stops_from_shipment(
          EXPORTING
            iv_tknum              = <ls_vbfa>-vbeln
          IMPORTING
            et_stops              = lt_stops
            et_dlv_watching_stops = lt_watching ).
      ENDIF.

      READ TABLE et_stops ASSIGNING <ls_stops>
        WITH TABLE KEY tknum = <ls_vbfa>-tknum.

      IF sy-subrc = 0.
        <ls_stops>-stops    = VALUE #( BASE <ls_stops>-stops
                                       ( LINES OF lt_stops ) ).

        <ls_stops>-watching = VALUE #( BASE <ls_stops>-watching
                                       ( LINES OF lt_watching ) ).
      ELSE.
        MESSAGE e005(zgtt) WITH |{ <ls_vbfa>-tknum }| 'STOPS' INTO DATA(lv_dummy).
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ENDLOOP.

    LOOP AT et_stops ASSIGNING <ls_stops>.
      SORT <ls_stops>-stops    BY stopid loccat.
      SORT <ls_stops>-watching BY vbeln stopid loccat.

      DELETE ADJACENT DUPLICATES FROM <ls_stops>-stops
        COMPARING stopid loccat.
      DELETE ADJACENT DUPLICATES FROM <ls_stops>-watching
        COMPARING vbeln stopid loccat.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_vtts_changed.

    IF is_vtts-updkz = zif_gtt_ef_constants=>cs_change_mode-insert OR
       is_vtts-updkz = zif_gtt_ef_constants=>cs_change_mode-delete.
      rv_result   = abap_true.
    ELSEIF ( is_vtts-updkz = zif_gtt_ef_constants=>cs_change_mode-update OR
             is_vtts-updkz = zif_gtt_ef_constants=>cs_change_mode-undefined ).

      READ TABLE is_shipment-old_vtts ASSIGNING FIELD-SYMBOL(<ls_vtts_old>)
        WITH KEY tknum = is_vtts-tknum
                 tsnum = is_vtts-tsnum
                 BINARY SEARCH.

      rv_result = boolc(
         sy-subrc = 0 ).
    ELSE.
      rv_result   = abap_false.
    ENDIF.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_CTP_SND_SH_TO_DLH definition
  public
  inheriting from ZCL_GTT_CTP_SND
  create private .

public section.

  types TT_TKNUM TYPE TABLE OF vttk-tknum.
  types:
    BEGIN OF ts_loc_info,
      loctype      TYPE char20,
      locid        TYPE char10,
      locaddrnum   TYPE adrnr,
      locindicator TYPE char1,
    END OF ts_loc_info .
  types:
    tt_loc_info type STANDARD TABLE OF ts_loc_info .
  types:
    BEGIN OF ts_address_info,
      locid     type char20,
      loctype   type char30,
      addr1     TYPE addr1_data,
      email     TYPE ad_smtpadr,
      telephone TYPE char50,
    END OF ts_address_info .
  types:
    tt_address_info type TABLE OF ts_address_info .

  class-methods GET_INSTANCE
    returning
      value(RO_SENDER) type ref to ZCL_GTT_MIA_CTP_SND_SH_TO_DLH
    raising
      CX_UDM_MESSAGE .
  methods PREPARE_IDOC_DATA
    importing
      !IO_SHIP_DATA type ref to ZCL_GTT_MIA_CTP_SHIPMENT_DATA
    raising
      CX_UDM_MESSAGE .
  PROTECTED SECTION.

    METHODS get_aotype_restriction_id
        REDEFINITION .
    METHODS get_object_type
        REDEFINITION .
    METHODS get_evtype_restriction_id
        REDEFINITION .
private section.

  constants:
    BEGIN OF cs_mapping,
        vbeln                 TYPE /saptrx/paramname VALUE 'YN_DL_DELEVERY',
        shp_count             TYPE /saptrx/paramname VALUE 'YN_SHP_LINE_COUNT',
        shp_tknum             TYPE /saptrx/paramname VALUE 'YN_SHP_NO',
        shp_fstop             TYPE /saptrx/paramname VALUE 'YN_SHP_FIRST_STOP',
        shp_lstop             TYPE /saptrx/paramname VALUE 'YN_SHP_LAST_STOP',
        shp_lstop_rec_loc     TYPE /saptrx/paramname VALUE 'YN_SHP_LAST_STOP_REC_LOC',
        shp_lstop_rec_loc_typ TYPE /saptrx/paramname VALUE 'YN_SHP_LAST_STOP_REC_LOC_TYP',
        pod_relevant          TYPE /saptrx/paramname VALUE 'YN_DL_POD_RELEVANT',
        otl_locid             TYPE /saptrx/paramname VALUE 'GTT_OTL_LOCID',
        otl_loctype           TYPE /saptrx/paramname VALUE 'GTT_OTL_LOCTYPE',
        otl_timezone          TYPE /saptrx/paramname VALUE 'GTT_OTL_TIMEZONE',
        otl_longitude         TYPE /saptrx/paramname VALUE 'GTT_OTL_LONGITUDE',
        otl_latitude          TYPE /saptrx/paramname VALUE 'GTT_OTL_LATITUDE',
        otl_unlocode          TYPE /saptrx/paramname VALUE 'GTT_OTL_UNLOCODE',
        otl_iata_code         TYPE /saptrx/paramname VALUE 'GTT_OTL_IATA_CODE',
        otl_description       TYPE /saptrx/paramname VALUE 'GTT_OTL_DESCRIPTION',
        otl_country_code      TYPE /saptrx/paramname VALUE 'GTT_OTL_COUNTRY_CODE',
        otl_city_name         TYPE /saptrx/paramname VALUE 'GTT_OTL_CITY_NAME',
        otl_region_code       TYPE /saptrx/paramname VALUE 'GTT_OTL_REGION_CODE',
        otl_house_number      TYPE /saptrx/paramname VALUE 'GTT_OTL_HOUSE_NUMBER',
        otl_street_name       TYPE /saptrx/paramname VALUE 'GTT_OTL_STREET_NAME',
        otl_postal_code       TYPE /saptrx/paramname VALUE 'GTT_OTL_POSTAL_CODE',
        otl_email_address     TYPE /saptrx/paramname VALUE 'GTT_OTL_EMAIL_ADDRESS',
        otl_phone_number      TYPE /saptrx/paramname VALUE 'GTT_OTL_PHONE_NUMBER',
      END OF cs_mapping .

  methods FILL_IDOC_APPOBJ_CTABS
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_LIKP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIKPEX
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_CONTROL_DATA
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_SHIP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_SHIPMENT_MERGE
      !IS_LIKP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIKPEX
      !IS_STOPS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_STOPS
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_EXP_EVENT
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_SHIP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_SHIPMENT_MERGE
      !IS_LIKP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIKPEX
      !IS_STOPS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_STOPS
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_TRACKING_ID
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_SHIP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_SHIPMENT_MERGE
      !IS_LIKP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIKPEX
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods GET_SHIPMENT_STOP
    importing
      !IS_STOPS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_STOPS
      !IS_VBFA type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_VBFAEX
      !IV_ARRIVAL type ABAP_BOOL
    exporting
      !ES_STOP type ZIF_GTT_MIA_APP_TYPES=>TS_STOPS
    returning
      value(RV_STOPID_TXT) type ZIF_GTT_MIA_APP_TYPES=>TV_STOPID
    raising
      CX_UDM_MESSAGE .
  methods IS_POD_RELEVANT_DELIVERY
    importing
      !IS_SHIP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_SHIPMENT_MERGE
      !IS_LIKP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIKPEX
      !IT_STOPS type ZIF_GTT_MIA_APP_TYPES=>TT_STOPS
    returning
      value(RV_RESULT) type ZIF_GTT_EF_TYPES=>TV_CONDITION
    raising
      CX_UDM_MESSAGE .
  methods IS_POD_RELEVANT_STOP
    importing
      !IS_SHIP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_SHIPMENT_MERGE
      !IS_LIKP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIKPEX
      !IS_STOPS type ZIF_GTT_MIA_APP_TYPES=>TS_STOPS
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  methods FILL_ONE_TIME_LOCATION
    importing
      !IV_VBELN type VBELN_VL
      !IT_TKNUM type TT_TKNUM
      !IT_VTTK type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TT_VTTKVB_SRT
      !IT_VTTS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TT_VTTSVB_SRT
    exporting
      !ET_CONTROL_DATA type /SAPTRX/BAPI_TRK_CONTROL_TAB .
  methods PREPARE_CURRENT_LOC_DATA
    importing
      !IT_TKNUM type TT_TKNUM
      !IT_VTTKVB type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TT_VTTKVB_SRT
      !IT_VTTSVB type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TT_VTTSVB_SRT
    exporting
      !ET_ADDRESS_INFO type TT_ADDRESS_INFO .
  methods PREPARE_RELEVANT_LOC_DATA
    importing
      !IT_TKNUM type TT_TKNUM
      !IT_VTTKVB type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TT_VTTKVB_SRT
    exporting
      !ET_ADDRESS_INFO type TT_ADDRESS_INFO .
  methods PREPARE_DELIVERY_LOC_DATA
    importing
      !IV_VBELN type VBELN_VL
    exporting
      !ET_ADDRESS_INFO type TT_ADDRESS_INFO .
  methods MERGE_LOCATION_DATA
    importing
      !IT_ADDR_INFO_CUR type TT_ADDRESS_INFO
      !IT_ADDR_INFO_REL type TT_ADDRESS_INFO
      !IT_ADDR_INFO_DLV type TT_ADDRESS_INFO
    exporting
      !ET_ADDR_INFO_ALL type TT_ADDRESS_INFO .
  methods PREPARE_CONTROL_DATA
    importing
      !IT_ADDR_INFO type TT_ADDRESS_INFO
    exporting
      !ET_CONTROL_DATA type /SAPTRX/BAPI_TRK_CONTROL_TAB .
  methods ADD_PLANNED_SOURCE_ARRIVAL
    importing
      !IT_EXPEVENTDATA type /SAPTRX/BAPI_TRK_EE_TAB
      !IS_SHIP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_SHIPMENT_MERGE
    exporting
      !ET_EXPEVENTDATA type /SAPTRX/BAPI_TRK_EE_TAB
    raising
      CX_UDM_MESSAGE .
ENDCLASS.



CLASS ZCL_GTT_MIA_CTP_SND_SH_TO_DLH IMPLEMENTATION.


  METHOD fill_idoc_appobj_ctabs.

    cs_idoc_data-appobj_ctabs = VALUE #( BASE cs_idoc_data-appobj_ctabs (
      trxservername = cs_idoc_data-trxserv-trx_server_id
      appobjtype    = is_aotype-aot_type
      appobjid      = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
                        ir_likp = REF #( is_likp ) )
    ) ).

  ENDMETHOD.


  METHOD fill_idoc_control_data.

    DATA: lt_control TYPE /saptrx/bapi_trk_control_tab,
          lv_count   TYPE i VALUE 0,
          lt_tknum   TYPE tt_tknum.

    " DLV Head data (obligatory)
    lt_control  = VALUE #(
      (
        paramname = cs_mapping-vbeln
        value     = zcl_gtt_mia_dl_tools=>get_formated_dlv_number(
                      ir_likp = REF #( is_likp ) )
      )
      (
        paramname = cs_mapping-pod_relevant
        value     = is_pod_relevant_delivery(
                      is_ship  = is_ship
                      is_likp  = is_likp
                      it_stops = is_stops-stops )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_timezone
        value     = zcl_gtt_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_technical_timezone
        value     = zcl_gtt_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_technical_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-reported_by
        value     = sy-uname
      )
    ).

    LOOP AT is_ship-likp\vbfa[ is_likp ] ASSIGNING FIELD-SYMBOL(<ls_vbfa>).
      ADD 1 TO lv_count.

      get_shipment_stop(
        EXPORTING
          is_stops   = is_stops
          is_vbfa    = <ls_vbfa>
          iv_arrival = abap_true
        IMPORTING
          es_stop    = DATA(ls_lstop) ).

      lt_control  = VALUE #( BASE lt_control
        (
          paramindex = lv_count
          paramname  = cs_mapping-shp_count
          value      = |{ lv_count }|
        )
        (
          paramindex = lv_count
          paramname  = cs_mapping-shp_tknum
          value      = zcl_gtt_mia_sh_tools=>get_formated_sh_number(
                         ir_vttk = NEW vttk( tknum = <ls_vbfa>-vbeln ) )
        )
        (
          paramindex = lv_count
          paramname  = cs_mapping-shp_fstop
          value      = get_shipment_stop(
                         EXPORTING
                           is_stops   = is_stops
                           is_vbfa    = <ls_vbfa>
                           iv_arrival = abap_false )
        )
        (
          paramindex = lv_count
          paramname  = cs_mapping-shp_lstop
          value      = ls_lstop-stopid_txt
        )
        (
          paramindex = lv_count
          paramname  = cs_mapping-shp_lstop_rec_loc
          value      = zcl_gtt_tools=>get_pretty_location_id(
                         iv_locid   = ls_lstop-locid
                         iv_loctype = ls_lstop-loctype )
        )
        (
          paramindex = lv_count
          paramname  = cs_mapping-shp_lstop_rec_loc_typ
          value      = ls_lstop-loctype
        )
      ).
*     Note down the shipment number
      APPEND <ls_vbfa>-vbeln TO lt_tknum.
    ENDLOOP.

    IF sy-subrc <> 0.
      lt_control  = VALUE #( BASE lt_control (
          paramindex = '1'
          paramname  = cs_mapping-shp_count
          value      = ''
      ) ).
    ENDIF.

*   Support one time location
    fill_one_time_location(
      EXPORTING
        iv_vbeln        = is_likp-vbeln
        it_tknum        = lt_tknum
        it_vttk         = is_ship-vttk
        it_vtts         = is_ship-vtts
      IMPORTING
        et_control_data = DATA(lt_location_data) ).
    APPEND LINES OF lt_location_data TO lt_control.
    CLEAR lt_location_data.

    " fill technical data into all control data records
    LOOP AT lt_control ASSIGNING FIELD-SYMBOL(<ls_control>).
      <ls_control>-appsys     = mv_appsys.
      <ls_control>-appobjtype = is_aotype-aot_type.
      <ls_control>-appobjid = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
        ir_likp = REF #( is_likp ) ).
    ENDLOOP.

    cs_idoc_data-control  = VALUE #( BASE cs_idoc_data-control
                                     ( LINES OF lt_control ) ).

  ENDMETHOD.


  METHOD fill_idoc_exp_event.

    DATA: lt_exp_event     TYPE /saptrx/bapi_trk_ee_tab,
          lt_exp_event_dlv TYPE /saptrx/bapi_trk_ee_tab,
          lv_milestonenum  TYPE /saptrx/seq_num VALUE 1,
          lv_tknum         TYPE tknum.

    zcl_gtt_mia_ctp_tools=>get_delivery_head_planned_evt(
      EXPORTING
        iv_appsys    = mv_appsys
        is_aotype    = is_aotype
        is_likp      = CORRESPONDING #( is_likp )
        it_lips      = CORRESPONDING #( is_ship-lips )
      IMPORTING
        et_exp_event = lt_exp_event_dlv ).

    LOOP AT lt_exp_event_dlv TRANSPORTING NO FIELDS
      WHERE ( milestone = zif_gtt_ef_constants=>cs_milestone-sh_arrival OR
              milestone = zif_gtt_ef_constants=>cs_milestone-sh_departure OR
              milestone = zif_gtt_ef_constants=>cs_milestone-sh_pod ).
      DELETE lt_exp_event_dlv.
    ENDLOOP.

    LOOP AT is_stops-watching ASSIGNING FIELD-SYMBOL(<ls_watching>)
      WHERE vbeln = is_likp-vbeln.

      IF lv_tknum <> <ls_watching>-stopid(10).
        lv_tknum        = <ls_watching>-stopid(10).
        lv_milestonenum = 1.
      ENDIF.

      READ TABLE is_stops-stops ASSIGNING FIELD-SYMBOL(<ls_stops>)
        WITH KEY stopid = <ls_watching>-stopid
                 loccat = <ls_watching>-loccat.

      IF sy-subrc = 0.
        " Departure / Arrival
        lt_exp_event = VALUE #( BASE lt_exp_event (
            milestone         = COND #( WHEN <ls_watching>-loccat = zif_gtt_mia_app_constants=>cs_loccat-departure
                                          THEN zif_gtt_ef_constants=>cs_milestone-sh_departure
                                          ELSE zif_gtt_ef_constants=>cs_milestone-sh_arrival )
            locid2            = <ls_stops>-stopid_txt
            loctype           = <ls_stops>-loctype
            locid1            = zcl_gtt_tools=>get_pretty_location_id(
                                  iv_locid   = <ls_stops>-locid
                                  iv_loctype = <ls_stops>-loctype )
            evt_exp_datetime  = <ls_stops>-pln_evt_datetime
            evt_exp_tzone     = <ls_stops>-pln_evt_timezone
            milestonenum      = lv_milestonenum
        ) ).
        ADD 1 TO lv_milestonenum.

        " POD
        IF <ls_stops>-loccat  = zif_gtt_mia_app_constants=>cs_loccat-arrival AND
           <ls_stops>-loctype = zif_gtt_ef_constants=>cs_loc_types-shippingpoint AND
           is_pod_relevant_stop( is_ship  = is_ship
                                 is_likp  = is_likp
                                 is_stops = <ls_stops> ) = abap_true.

          lt_exp_event = VALUE #( BASE lt_exp_event (
              milestone         = zif_gtt_ef_constants=>cs_milestone-sh_pod
              locid2            = <ls_stops>-stopid_txt
              loctype           = <ls_stops>-loctype
              locid1            = zcl_gtt_tools=>get_pretty_location_id(
                                    iv_locid   = <ls_stops>-locid
                                    iv_loctype = <ls_stops>-loctype )
              evt_exp_datetime  = <ls_stops>-pln_evt_datetime
              evt_exp_tzone     = <ls_stops>-pln_evt_timezone
              milestonenum      = lv_milestonenum
          ) ).
          ADD 1 TO lv_milestonenum.
        ENDIF.
      ELSE.
        MESSAGE e005(zgtt)
          WITH |{ <ls_watching>-stopid }{ <ls_watching>-loccat }| 'STOPS'
          INTO DATA(lv_dummy).
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ENDLOOP.

    " fill sequence number in DLV events
    lv_milestonenum = zcl_gtt_tools=>get_next_sequence_id(
      it_expeventdata = lt_exp_event ).

    LOOP AT lt_exp_event_dlv ASSIGNING FIELD-SYMBOL(<ls_exp_event_dlv>).
      <ls_exp_event_dlv>-milestonenum   = lv_milestonenum.
      ADD 1 TO lv_milestonenum.
    ENDLOOP.

    lt_exp_event    = VALUE #( BASE lt_exp_event
                                ( LINES OF lt_exp_event_dlv ) ).

    add_planned_source_arrival(
      EXPORTING
        it_expeventdata = lt_exp_event
        is_ship         = is_ship
      IMPORTING
        et_expeventdata = DATA(lt_expeventdata) ).
    APPEND LINES OF lt_expeventdata TO lt_exp_event.

    IF lt_exp_event[] IS INITIAL.
      lt_exp_event = VALUE #( (
          milestone         = ''
          locid2            = ''
          loctype           = ''
          locid1            = ''
          evt_exp_datetime  = '000000000000000'
          evt_exp_tzone     = ''
      ) ).
    ENDIF.

    LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
      <ls_exp_event>-appsys         = mv_appsys.
      <ls_exp_event>-appobjtype     = is_aotype-aot_type.
      <ls_exp_event>-appobjid = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
        ir_likp = REF #( is_likp ) ).
      <ls_exp_event>-language       = sy-langu.
      IF <ls_exp_event>-evt_exp_tzone IS INITIAL.
        <ls_exp_event>-evt_exp_tzone  = zcl_gtt_tools=>get_system_time_zone(  ).
      ENDIF.
    ENDLOOP.

    cs_idoc_data-exp_event = VALUE #( BASE cs_idoc_data-exp_event
                                      ( LINES OF lt_exp_event ) ).

  ENDMETHOD.


  METHOD fill_idoc_tracking_id.

    "another tip is that: for tracking ID type 'SHIPMENT_ORDER' of delivery header,
    "and for tracking ID type 'RESOURCE' of shipment header,
    "DO NOT enable START DATE and END DATE
    DATA:
      lt_tracking_id TYPE /saptrx/bapi_trk_trkid_tab,
      lv_shptrxcod   TYPE /saptrx/trxcod,
      lv_dlvhdtrxcod TYPE /saptrx/trxcod.

    lv_shptrxcod = zif_gtt_ef_constants=>cs_trxcod-sh_number.
    lv_dlvhdtrxcod = zif_gtt_ef_constants=>cs_trxcod-dl_number.

    " Delivery Header
    lt_tracking_id    = VALUE #( (
      trxcod      = lv_dlvhdtrxcod
      trxid       = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
                      ir_likp = REF #( is_likp ) )
      timzon      = zcl_gtt_tools=>get_system_time_zone( )
    ) ).

    " Shipment
    LOOP AT is_ship-likp\likp_dlt[ is_likp ] ASSIGNING FIELD-SYMBOL(<ls_likp_dlt>).
      IF <ls_likp_dlt>-updkz = zif_gtt_ef_constants=>cs_change_mode-insert OR
         <ls_likp_dlt>-updkz = zif_gtt_ef_constants=>cs_change_mode-delete.
        lt_tracking_id    = VALUE #( BASE lt_tracking_id (
          trxcod      = lv_shptrxcod
          trxid       = zcl_gtt_mia_sh_tools=>get_tracking_id_sh_header(
                          ir_vttk = REF #( <ls_likp_dlt> ) )              "<ls_likp_dlt>-tknum
          action      = COND #( WHEN <ls_likp_dlt>-updkz = zif_gtt_ef_constants=>cs_change_mode-delete
                                  THEN zif_gtt_ef_constants=>cs_change_mode-delete )
        ) ).
      ENDIF.
    ENDLOOP.

    " Fill general data
    LOOP AT lt_tracking_id ASSIGNING FIELD-SYMBOL(<ls_tracking_id>).
      <ls_tracking_id>-appsys     = mv_appsys.
      <ls_tracking_id>-appobjtype = is_aotype-aot_type.
      <ls_tracking_id>-appobjid = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
        ir_likp = REF #( is_likp ) ).
    ENDLOOP.

    cs_idoc_data-tracking_id = VALUE #( BASE cs_idoc_data-tracking_id
                                        ( LINES OF lt_tracking_id ) ).

  ENDMETHOD.


  METHOD get_aotype_restriction_id.

    rv_rst_id   = 'SH_TO_IDLH'.

  ENDMETHOD.


  METHOD get_evtype_restriction_id.
    CLEAR: rv_rst_id .
  ENDMETHOD.


  METHOD get_instance.

    DATA(lt_trk_obj_type) = VALUE zif_gtt_ctp_types=>tt_trk_obj_type(
       ( zif_gtt_ef_constants=>cs_trk_obj_type-esc_shipmt )
       ( zif_gtt_ef_constants=>cs_trk_obj_type-esc_deliv )
    ).

    IF is_gtt_enabled( it_trk_obj_type = lt_trk_obj_type ) = abap_true.
      ro_sender  = NEW #( ).

      ro_sender->initiate( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_object_type.

    rv_objtype  = zif_gtt_ef_constants=>cs_trk_obj_type-esc_deliv.

  ENDMETHOD.


  METHOD get_shipment_stop.
    DATA: lv_stopid TYPE zif_gtt_mia_app_types=>tv_stopid.

    CLEAR: es_stop.

    LOOP AT is_stops-watching ASSIGNING FIELD-SYMBOL(<ls_watching>)
      WHERE vbeln       = is_vbfa-vbelv
        AND stopid(10)  = is_vbfa-vbeln.

      rv_stopid_txt = <ls_watching>-stopid_txt.
      lv_stopid     = <ls_watching>-stopid.

      IF iv_arrival = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_stopid IS NOT INITIAL AND
       es_stop IS REQUESTED.
      READ TABLE is_stops-stops
        INTO es_stop
        WITH KEY stopid = lv_stopid
                 loccat = COND #( WHEN iv_arrival = abap_true
                                    THEN zif_gtt_mia_app_constants=>cs_loccat-arrival
                                    ELSE zif_gtt_mia_app_constants=>cs_loccat-departure ).
      IF sy-subrc <> 0.
        MESSAGE e011(zgtt) WITH lv_stopid is_stops-tknum INTO DATA(lv_dummy).
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_pod_relevant_delivery.

    DATA: lt_vstel TYPE RANGE OF likp-vstel.

    rv_result   = zif_gtt_ef_constants=>cs_condition-false.

    " prepare list of POD relevant shipping point
    LOOP AT is_ship-likp ASSIGNING FIELD-SYMBOL(<ls_likp>).
      IF lt_vstel IS INITIAL OR
         <ls_likp>-vstel NOT IN lt_vstel.

        READ TABLE is_ship-ee_rel ASSIGNING FIELD-SYMBOL(<ls_ee_rel>)
          WITH TABLE KEY appobjid = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
                                      ir_likp = REF #( <ls_likp> ) ).

        IF sy-subrc = 0 AND
           <ls_ee_rel>-z_pdstk = abap_true.

          lt_vstel  = VALUE #( BASE lt_vstel (
            low       = <ls_likp>-vstel
            option    = 'EQ'
            sign      = 'I'
          ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    " check whether shipment has any stops with POD Relevant shipping point
    IF lt_vstel[] IS NOT INITIAL.
      LOOP AT it_stops ASSIGNING FIELD-SYMBOL(<ls_stops>)
        WHERE locid    IN lt_vstel
          AND loccat   = zif_gtt_mia_app_constants=>cs_loccat-arrival
          AND loctype  = zif_gtt_ef_constants=>cs_loc_types-shippingpoint.

        rv_result   = zif_gtt_ef_constants=>cs_condition-true.
        EXIT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD is_pod_relevant_stop.

    CLEAR: rv_result.

    LOOP AT is_ship-likp ASSIGNING FIELD-SYMBOL(<ls_likp>).
      IF is_stops-locid   = <ls_likp>-vstel AND
         is_stops-loccat  = zif_gtt_mia_app_constants=>cs_loccat-arrival AND
         is_stops-loctype = zif_gtt_ef_constants=>cs_loc_types-shippingpoint.

        READ TABLE is_ship-ee_rel ASSIGNING FIELD-SYMBOL(<ls_ee_rel>)
          WITH TABLE KEY appobjid = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
                                      ir_likp = REF #( <ls_likp> ) ).

        IF sy-subrc = 0 AND
           <ls_ee_rel>-z_pdstk = abap_true.

          rv_result   = abap_true.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_idoc_data.

    " prepare DLV Header list (likp or vtrlk_tab)
    " fill DLV Header
    "   control data
    "     YN_SHP_LINE_COUNT
    "     YN_SHP_NO
    "     YN_SHP_FIRST_STOP
    "     YN_SHP_LAST_STOP
    "     YN_SHP_LINE_COUNT
    "     ACTUAL_BUSINESS_TIMEZONE
    "     ACTUAL_BUSINESS_DATETIME
    "   tracking ID
    "     send DLV Header TID
    "     add/delete Shipment TID
    "   planned events
    "     DEPARTURE
    "     ARRIV_DEST
    "     POD

    DATA: ls_idoc_data    TYPE zif_gtt_ctp_types=>ts_idoc_data.

    DATA(lr_ship)   = io_ship_data->get_data( ).
    DATA(lr_stops)  = io_ship_data->get_stops( ).

    FIELD-SYMBOLS: <ls_ship>  TYPE zcl_gtt_mia_ctp_shipment_data=>ts_shipment_merge,
                   <lt_stops> TYPE zcl_gtt_mia_ctp_shipment_data=>tt_stops_srt.

    ASSIGN lr_ship->*  TO <ls_ship>.
    ASSIGN lr_stops->* TO <lt_stops>.

    LOOP AT mt_aotype ASSIGNING FIELD-SYMBOL(<ls_aotype>).
      CLEAR: ls_idoc_data.

      ls_idoc_data-appsys   = mv_appsys.

      fill_idoc_trxserv(
        EXPORTING
          is_aotype    = <ls_aotype>
        CHANGING
          cs_idoc_data = ls_idoc_data ).

      LOOP AT <ls_ship>-likp ASSIGNING FIELD-SYMBOL(<ls_likp>).
        READ TABLE <lt_stops> ASSIGNING FIELD-SYMBOL(<ls_stops>)
          WITH TABLE KEY tknum = <ls_likp>-tknum.

        IF sy-subrc = 0.
          fill_idoc_appobj_ctabs(
            EXPORTING
              is_aotype    = <ls_aotype>
              is_likp      = <ls_likp>
            CHANGING
              cs_idoc_data = ls_idoc_data ).

          fill_idoc_control_data(
            EXPORTING
              is_aotype    = <ls_aotype>
              is_ship      = <ls_ship>
              is_likp      = <ls_likp>
              is_stops     = <ls_stops>
            CHANGING
              cs_idoc_data = ls_idoc_data ).

          fill_idoc_exp_event(
            EXPORTING
              is_aotype    = <ls_aotype>
              is_ship      = <ls_ship>
              is_likp      = <ls_likp>
              is_stops     = <ls_stops>
            CHANGING
              cs_idoc_data = ls_idoc_data ).

          fill_idoc_tracking_id(
            EXPORTING
              is_aotype    = <ls_aotype>
              is_ship      = <ls_ship>
              is_likp      = <ls_likp>
            CHANGING
              cs_idoc_data = ls_idoc_data ).
        ELSE.
          MESSAGE e005(zgtt) WITH |{ <ls_likp>-tknum }| 'STOPS'
            INTO DATA(lv_dummy).
          zcl_gtt_tools=>throw_exception( ).
        ENDIF.

      ENDLOOP.

      IF ls_idoc_data-appobj_ctabs[] IS NOT INITIAL AND
         ls_idoc_data-control[] IS NOT INITIAL AND
         ls_idoc_data-tracking_id[] IS NOT INITIAL.
        APPEND ls_idoc_data TO mt_idoc_data.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_one_time_location.

    DATA:
      lt_addr_info_rel TYPE tt_address_info,
      lt_addr_info_cur TYPE tt_address_info,
      lt_addr_info_dlv TYPE tt_address_info,
      lt_addr_info_all TYPE tt_address_info.

    CLEAR et_control_data.

*   Step 1. Get current procced shipment location information
    prepare_current_loc_data(
      EXPORTING
        it_tknum        = it_tknum
        it_vttkvb       = it_vttk
        it_vttsvb       = it_vtts
      IMPORTING
        et_address_info = lt_addr_info_cur ).

*   Step 2. Get relevant shipment location information
    prepare_relevant_loc_data(
      EXPORTING
        it_tknum        = it_tknum
        it_vttkvb       = it_vttk
      IMPORTING
        et_address_info = lt_addr_info_rel ).

*   Step 3. Get delivery location information
    prepare_delivery_loc_data(
      EXPORTING
        iv_vbeln        = iv_vbeln
      IMPORTING
        et_address_info = lt_addr_info_dlv ).

*   Step 4. Merge the location information
    merge_location_data(
      EXPORTING
        it_addr_info_cur = lt_addr_info_cur
        it_addr_info_rel = lt_addr_info_rel
        it_addr_info_dlv = lt_addr_info_dlv
      IMPORTING
        et_addr_info_all = lt_addr_info_all ).

*   Step 5. Generate the location control data
    prepare_control_data(
      EXPORTING
        it_addr_info    = lt_addr_info_all
      IMPORTING
        et_control_data = et_control_data ).

  ENDMETHOD.


  METHOD merge_location_data.

    DATA:
      lt_addr_info_cur TYPE tt_address_info,
      lt_addr_info_rel TYPE tt_address_info,
      lt_addr_info_dlv TYPE tt_address_info.

    CLEAR et_addr_info_all.

    lt_addr_info_cur = it_addr_info_cur.
    lt_addr_info_rel = it_addr_info_rel.
    lt_addr_info_dlv = it_addr_info_dlv.

    LOOP AT lt_addr_info_cur INTO DATA(ls_loc_info_curr).
      READ TABLE lt_addr_info_rel INTO DATA(ls_addr_info_rel)
        WITH KEY locid   = ls_loc_info_curr-locid
                 loctype = ls_loc_info_curr-loctype.
      IF sy-subrc = 0.
        DELETE lt_addr_info_rel WHERE locid   = ls_loc_info_curr-locid
                                  AND loctype = ls_loc_info_curr-loctype.
      ENDIF.

      READ TABLE lt_addr_info_dlv INTO DATA(ls_addr_info_dlv)
        WITH KEY locid   = ls_loc_info_curr-locid
                 loctype = ls_loc_info_curr-loctype.
      IF sy-subrc = 0.
        DELETE lt_addr_info_dlv WHERE locid   = ls_loc_info_curr-locid
                                  AND loctype = ls_loc_info_curr-loctype.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_addr_info_rel INTO ls_addr_info_rel.
      CLEAR ls_addr_info_dlv.
      READ TABLE lt_addr_info_dlv INTO ls_addr_info_dlv
        WITH KEY locid   = ls_addr_info_rel-locid
                 loctype = ls_addr_info_rel-loctype.
      IF sy-subrc = 0.
        DELETE lt_addr_info_dlv WHERE locid   = ls_addr_info_rel-locid
                                  AND loctype = ls_addr_info_rel-loctype.
      ENDIF.
    ENDLOOP.

    APPEND LINES OF lt_addr_info_cur TO et_addr_info_all.
    APPEND LINES OF lt_addr_info_rel TO et_addr_info_all.
    APPEND LINES OF lt_addr_info_dlv TO et_addr_info_all.

  ENDMETHOD.


  METHOD prepare_control_data.

    DATA:
      lv_paramindex   TYPE /saptrx/indexcounter,
      ls_control_data TYPE /saptrx/control_data.

    CLEAR:et_control_data.

    LOOP AT it_addr_info INTO DATA(ls_addr_info).

      lv_paramindex = lv_paramindex + 1.

*     Location ID
      ls_control_data-paramindex = lv_paramindex.
      ls_control_data-paramname = cs_mapping-otl_locid.
      ls_control_data-value = ls_addr_info-locid.
      IF ls_addr_info-loctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
        ls_control_data-value = |{ ls_addr_info-locid ALPHA = OUT }|.
      ENDIF.
      APPEND ls_control_data TO et_control_data.

*     Location Type
      ls_control_data-paramindex = lv_paramindex.
      ls_control_data-paramname = cs_mapping-otl_loctype.
      ls_control_data-value = ls_addr_info-loctype.
      APPEND ls_control_data TO et_control_data.

*     Time Zone
      ls_control_data-paramindex = lv_paramindex.
      ls_control_data-paramname = cs_mapping-otl_timezone.
      ls_control_data-value = ls_addr_info-addr1-time_zone.
      APPEND ls_control_data TO et_control_data.

*     Description
      ls_control_data-paramindex = lv_paramindex.
      ls_control_data-paramname = cs_mapping-otl_description.
      ls_control_data-value = ls_addr_info-addr1-name1.
      APPEND ls_control_data TO et_control_data.

*     Country Code
      ls_control_data-paramindex = lv_paramindex.
      ls_control_data-paramname = cs_mapping-otl_country_code.
      ls_control_data-value = ls_addr_info-addr1-country.
      APPEND ls_control_data TO et_control_data.

*     City Name
      ls_control_data-paramindex = lv_paramindex.
      ls_control_data-paramname = cs_mapping-otl_city_name.
      ls_control_data-value = ls_addr_info-addr1-city1.
      APPEND ls_control_data TO et_control_data.

*     Region Code
      ls_control_data-paramindex = lv_paramindex.
      ls_control_data-paramname = cs_mapping-otl_region_code.
      ls_control_data-value = ls_addr_info-addr1-region.
      APPEND ls_control_data TO et_control_data.

*     House Number
      ls_control_data-paramindex = lv_paramindex.
      ls_control_data-paramname = cs_mapping-otl_house_number.
      ls_control_data-value = ls_addr_info-addr1-house_num1.
      APPEND ls_control_data TO et_control_data.

*     Street Name
      ls_control_data-paramindex = lv_paramindex.
      ls_control_data-paramname = cs_mapping-otl_street_name.
      ls_control_data-value = ls_addr_info-addr1-street.
      APPEND ls_control_data TO et_control_data.

*     Postal Code
      ls_control_data-paramindex = lv_paramindex.
      ls_control_data-paramname = cs_mapping-otl_postal_code.
      ls_control_data-value = ls_addr_info-addr1-post_code1.
      APPEND ls_control_data TO et_control_data.

*     Email Address
      ls_control_data-paramindex = lv_paramindex.
      ls_control_data-paramname = cs_mapping-otl_email_address.
      ls_control_data-value = ls_addr_info-email.
      APPEND ls_control_data TO et_control_data.

*     Phone Number
      ls_control_data-paramindex = lv_paramindex.
      ls_control_data-paramname = cs_mapping-otl_phone_number.
      ls_control_data-value = ls_addr_info-telephone.
      APPEND ls_control_data TO et_control_data.

    ENDLOOP.

    IF et_control_data IS INITIAL.
      ls_control_data-paramindex = 1.
      ls_control_data-paramname = cs_mapping-otl_locid.
      ls_control_data-value = ''.
      APPEND ls_control_data TO et_control_data.
    ENDIF.

  ENDMETHOD.


  METHOD prepare_current_loc_data.

    DATA:
      lt_loc_info_tmp  TYPE tt_loc_info,
      lt_loc_info_curr TYPE tt_loc_info,
      ls_address_info  TYPE ts_address_info,
      lt_addr_info_cur TYPE tt_address_info,
      ls_loc_addr      TYPE addr1_data,
      lv_loc_email     TYPE ad_smtpadr,
      lv_loc_tel       TYPE char50,
      lt_vttsvb        TYPE vttsvb_tab,
      ls_loc_addr_tmp  TYPE addr1_data,
      lv_loc_email_tmp TYPE ad_smtpadr,
      lv_loc_tel_tmp   TYPE char50.

    CLEAR et_address_info.

    APPEND LINES OF it_vttsvb TO lt_vttsvb.

    LOOP AT it_tknum INTO DATA(ls_tknum).
      CLEAR:
        lt_loc_info_tmp.
      READ TABLE it_vttkvb TRANSPORTING NO FIELDS
        WITH KEY tknum = ls_tknum.
      IF sy-subrc = 0.
*       Current procced shipment
        zcl_gtt_tools=>get_location_info(
          EXPORTING
            iv_tknum    = ls_tknum
            it_vttsvb   = lt_vttsvb
          IMPORTING
            et_loc_info = lt_loc_info_tmp ).
        APPEND LINES OF lt_loc_info_tmp TO lt_loc_info_curr.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_loc_info_curr INTO DATA(ls_loc_info_curr).
      CLEAR:
        ls_loc_addr,
        lv_loc_email,
        lv_loc_tel,
        ls_loc_addr_tmp,
        lv_loc_email_tmp,
        lv_loc_tel_tmp.

      IF ls_loc_info_curr-locaddrnum CN '0 ' AND ls_loc_info_curr-locindicator CA zif_gtt_ef_constants=>shp_addr_ind_man_all.
        zcl_gtt_tools=>get_address_from_memory(
          EXPORTING
            iv_addrnumber = ls_loc_info_curr-locaddrnum
          IMPORTING
            es_addr       = ls_loc_addr
            ev_email      = lv_loc_email
            ev_telephone  = lv_loc_tel ).

        zcl_gtt_tools=>get_address_detail_by_loctype(
          EXPORTING
            iv_loctype   = ls_loc_info_curr-loctype
            iv_locid     = ls_loc_info_curr-locid
          IMPORTING
            es_addr      = ls_loc_addr_tmp
            ev_email     = lv_loc_email_tmp
            ev_telephone = lv_loc_tel_tmp ).

        IF ls_loc_addr <> ls_loc_addr_tmp
          OR lv_loc_email <> lv_loc_email_tmp
          OR lv_loc_tel <> lv_loc_tel_tmp.
          ls_address_info-locid = ls_loc_info_curr-locid.
          ls_address_info-loctype = ls_loc_info_curr-loctype.
          ls_address_info-addr1 = ls_loc_addr.
          ls_address_info-email = lv_loc_email.
          ls_address_info-telephone = lv_loc_tel.
          APPEND ls_address_info TO et_address_info.
        ENDIF.
        CLEAR:
          ls_address_info.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_delivery_loc_data.

    DATA:
      lv_posnr        TYPE vbpa-posnr VALUE '000000',
      ls_vbpa         TYPE vbpavb,
      ls_address_info TYPE ts_address_info,
      ls_loc_addr     TYPE addr1_data,
      lv_loc_email    TYPE ad_smtpadr,
      lv_loc_tel      TYPE char50.

    CLEAR: et_address_info.

    CALL FUNCTION 'SD_VBPA_SINGLE_READ'
      EXPORTING
        i_vbeln          = iv_vbeln
        i_posnr          = lv_posnr
        i_parvw          = zif_gtt_mia_app_constants=>cs_parvw-supplier
      IMPORTING
        e_vbpavb         = ls_vbpa
      EXCEPTIONS
        record_not_found = 1
        OTHERS           = 2.

    IF sy-subrc = 0.
      IF ls_vbpa-adrnr CN '0 ' AND ls_vbpa-adrda CA zif_gtt_ef_constants=>vbpa_addr_ind_man_all.
        zcl_gtt_tools=>get_address_from_db(
          EXPORTING
            iv_addrnumber = ls_vbpa-adrnr
          IMPORTING
            es_addr       = ls_loc_addr
            ev_email      = lv_loc_email
            ev_telephone  = lv_loc_tel ).

        ls_address_info-locid = ls_vbpa-lifnr.
        ls_address_info-loctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
        ls_address_info-addr1 = ls_loc_addr.
        ls_address_info-email = lv_loc_email.
        ls_address_info-telephone = lv_loc_tel.
        APPEND ls_address_info TO et_address_info.
        CLEAR:
          ls_address_info.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD prepare_relevant_loc_data.

    DATA:
      lt_relevant_shp  TYPE tt_tknum,
      lt_tknum_range   TYPE STANDARD TABLE OF range_c10,
      lt_vttsvb        TYPE vttsvb_tab,
      lt_loc_info      TYPE tt_loc_info,
      ls_address_info  TYPE ts_address_info,
      ls_loc_addr      TYPE addr1_data,
      lv_loc_email     TYPE ad_smtpadr,
      lv_loc_tel       TYPE char50,
      ls_loc_addr_tmp  TYPE addr1_data,
      lv_loc_email_tmp TYPE ad_smtpadr,
      lv_loc_tel_tmp   TYPE char50.

    CLEAR et_address_info.

    LOOP AT it_tknum INTO DATA(ls_tknum).
      READ TABLE it_vttkvb TRANSPORTING NO FIELDS
        WITH KEY tknum = ls_tknum.
      IF sy-subrc <> 0.
        APPEND ls_tknum TO lt_relevant_shp.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_relevant_shp INTO ls_tknum.
      lt_tknum_range = VALUE #( BASE lt_tknum_range (
        sign   = 'I'
        option = 'EQ'
        low    = ls_tknum ) ).
    ENDLOOP.

    CHECK lt_tknum_range IS NOT INITIAL.
    CALL FUNCTION 'ST_STAGES_READ'
      TABLES
        i_tknum_range = lt_tknum_range
        c_xvttsvb     = lt_vttsvb
      EXCEPTIONS
        no_shipments  = 1
        OTHERS        = 2.

    zcl_gtt_tools=>get_location_info(
      EXPORTING
        it_vttsvb   = lt_vttsvb
      IMPORTING
        et_loc_info = lt_loc_info ).

    LOOP AT lt_loc_info INTO DATA(ls_loc_info).
      CLEAR:
        ls_loc_addr,
        lv_loc_email,
        lv_loc_tel,
        ls_loc_addr_tmp,
        lv_loc_email_tmp,
        lv_loc_tel_tmp.

      IF ls_loc_info-locaddrnum CN '0 ' AND ls_loc_info-locindicator CA zif_gtt_ef_constants=>shp_addr_ind_man_all.
        zcl_gtt_tools=>get_address_from_db(
          EXPORTING
            iv_addrnumber = ls_loc_info-locaddrnum
          IMPORTING
            es_addr       = ls_loc_addr
            ev_email      = lv_loc_email
            ev_telephone  = lv_loc_tel ).

        zcl_gtt_tools=>get_address_detail_by_loctype(
          EXPORTING
            iv_loctype   = ls_loc_info-loctype
            iv_locid     = ls_loc_info-locid
          IMPORTING
            es_addr      = ls_loc_addr_tmp
            ev_email     = lv_loc_email_tmp
            ev_telephone = lv_loc_tel_tmp ).

        IF ls_loc_addr <> ls_loc_addr_tmp
          OR lv_loc_email <> lv_loc_email_tmp
          OR lv_loc_tel <> lv_loc_tel_tmp.
          ls_address_info-locid = ls_loc_info-locid.
          ls_address_info-loctype = ls_loc_info-loctype.
          ls_address_info-addr1 = ls_loc_addr.
          ls_address_info-email = lv_loc_email.
          ls_address_info-telephone = lv_loc_tel.
          APPEND ls_address_info TO et_address_info.
        ENDIF.
        CLEAR:
          ls_address_info.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD add_planned_source_arrival.

    DATA:
      lv_length       TYPE i,
      lv_seq          TYPE char04,
      lv_tknum        TYPE vttk-tknum,
      lv_ltl_flag     TYPE flag,
      ls_eventdata    TYPE /saptrx/bapi_trk_exp_events,
      lt_vttk         TYPE vttkvb_tab,
      lt_vttp         TYPE vttpvb_tab,
      lv_exp_datetime TYPE /saptrx/event_exp_datetime.

    CLEAR et_expeventdata.

    lt_vttk = CORRESPONDING #( is_ship-vttk ).
    lt_vttp = CORRESPONDING #( is_ship-vttp ).

    LOOP AT it_expeventdata INTO DATA(ls_expeventdata)
      WHERE milestone = zif_gtt_ef_constants=>cs_milestone-sh_departure.
      CLEAR:
        lv_length,
        lv_tknum,
        lv_seq.

      lv_length = strlen( ls_expeventdata-locid2 ) - 4.
      IF lv_length >= 0.
        lv_tknum = ls_expeventdata-locid2+0(lv_length).
        lv_tknum = |{ lv_tknum ALPHA = IN }|.
        lv_seq = ls_expeventdata-locid2+lv_length(4).
        zcl_gtt_tools=>check_ltl_shipment(
          EXPORTING
            iv_tknum    = lv_tknum
            it_vttk     = lt_vttk
            it_vttp     = lt_vttp
          IMPORTING
            ev_ltl_flag = lv_ltl_flag
            es_vttk     = DATA(ls_vttk) ).

        IF lv_ltl_flag = abap_true AND lv_seq = '0001'. "only for source location of the shipment
          lv_exp_datetime = zcl_gtt_tools=>get_local_timestamp(
            iv_date = ls_vttk-dpreg     "Planned date of check-in
            iv_time = ls_vttk-upreg ).  "Planned check-in time
          ls_eventdata = ls_expeventdata.
          ls_eventdata-milestonenum = 0.
          ls_eventdata-milestone = zif_gtt_ef_constants=>cs_milestone-sh_arrival.
          ls_eventdata-evt_exp_datetime = lv_exp_datetime.
          ls_eventdata-evt_exp_tzone = zcl_gtt_tools=>get_system_time_zone( ).
          APPEND ls_eventdata TO et_expeventdata.
          CLEAR ls_eventdata.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_CTP_SND_SH_TO_DLI IMPLEMENTATION.


  METHOD fill_idoc_appobj_ctabs.

    cs_idoc_data-appobj_ctabs = VALUE #( BASE cs_idoc_data-appobj_ctabs (
      trxservername = cs_idoc_data-trxserv-trx_server_id
      appobjtype    = is_aotype-aot_type
      appobjid      = get_delivery_item_tracking_id(
                        is_lips = is_lips )
    ) ).

  ENDMETHOD.


  METHOD fill_idoc_control_data.

    DATA: lt_control    TYPE /saptrx/bapi_trk_control_tab .

    " DLV Item key data (obligatory)
    lt_control  = VALUE #(
      (
        paramname = cs_mapping-vbeln
        value     = zcl_gtt_mia_dl_tools=>get_formated_dlv_number(
                      ir_likp = REF #( is_lips ) )
      )
      (
        paramname = cs_mapping-posnr
        value     = zcl_gtt_mia_dl_tools=>get_formated_dlv_item(
                      ir_lips = REF #( is_lips ) )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_timezone
        value     = zcl_gtt_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_technical_timezone
        value     = zcl_gtt_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_technical_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
    ).

    " fill technical data into all control data records
    LOOP AT lt_control ASSIGNING FIELD-SYMBOL(<ls_control>).
      <ls_control>-appsys     = mv_appsys.
      <ls_control>-appobjtype = is_aotype-aot_type.
      <ls_control>-appobjid = get_delivery_item_tracking_id(
        is_lips = is_lips ).
    ENDLOOP.

    cs_idoc_data-control  = VALUE #( BASE cs_idoc_data-control
                                     ( LINES OF lt_control ) ).

  ENDMETHOD.


  METHOD fill_idoc_exp_event.

    DATA: lt_exp_event     TYPE /saptrx/bapi_trk_ee_tab,
          lt_exp_event_dlv TYPE /saptrx/bapi_trk_ee_tab,
          lv_milestonenum  TYPE /saptrx/seq_num VALUE 1,
          lv_tknum         TYPE tknum.

    " delivery item events
    zcl_gtt_mia_ctp_tools=>get_delivery_item_planned_evt(
      EXPORTING
        iv_appsys    = mv_appsys
        is_aotype    = is_aotype
        is_likp      = CORRESPONDING #( is_likp )
        is_lips      = CORRESPONDING #( is_lips )
      IMPORTING
        et_exp_event = lt_exp_event_dlv ).

    LOOP AT lt_exp_event_dlv TRANSPORTING NO FIELDS
      WHERE ( milestone = zif_gtt_ef_constants=>cs_milestone-sh_arrival OR
              milestone = zif_gtt_ef_constants=>cs_milestone-sh_departure OR
              milestone = zif_gtt_ef_constants=>cs_milestone-sh_pod ).
      DELETE lt_exp_event_dlv.
    ENDLOOP.

    " shipment events
    LOOP AT is_stops-watching ASSIGNING FIELD-SYMBOL(<ls_watching>)
      WHERE vbeln = is_lips-vbeln.

      IF lv_tknum <> <ls_watching>-stopid(10).
        lv_tknum        = <ls_watching>-stopid(10).
        lv_milestonenum = 1.
      ENDIF.

      READ TABLE is_stops-stops ASSIGNING FIELD-SYMBOL(<ls_stops>)
        WITH KEY stopid = <ls_watching>-stopid
                 loccat = <ls_watching>-loccat.
      IF sy-subrc = 0.
        " Departure / Arrival
        lt_exp_event = VALUE #( BASE lt_exp_event (
            milestone         = COND #( WHEN <ls_watching>-loccat = zif_gtt_mia_app_constants=>cs_loccat-departure
                                          THEN zif_gtt_ef_constants=>cs_milestone-sh_departure
                                          ELSE zif_gtt_ef_constants=>cs_milestone-sh_arrival )
            locid2            = <ls_stops>-stopid_txt
            loctype           = <ls_stops>-loctype
            locid1            = zcl_gtt_tools=>get_pretty_location_id(
                                  iv_locid   = <ls_stops>-locid
                                  iv_loctype = <ls_stops>-loctype )
            evt_exp_datetime  = <ls_stops>-pln_evt_datetime
            evt_exp_tzone     = <ls_stops>-pln_evt_timezone
            milestonenum      = lv_milestonenum
        ) ).
        ADD 1 TO lv_milestonenum.

        " POD
        IF <ls_stops>-loccat  = zif_gtt_mia_app_constants=>cs_loccat-arrival AND
           <ls_stops>-loctype = zif_gtt_ef_constants=>cs_loc_types-plant AND
           is_pod_relevant( is_ship  = is_ship
                            is_lips  = is_lips
                            is_stops = <ls_stops> ) = abap_true.

          lt_exp_event = VALUE #( BASE lt_exp_event (
              milestone         = zif_gtt_ef_constants=>cs_milestone-sh_pod
              locid2            = <ls_stops>-stopid_txt
              loctype           = <ls_stops>-loctype
              locid1            = zcl_gtt_tools=>get_pretty_location_id(
                                    iv_locid   = <ls_stops>-locid
                                    iv_loctype = <ls_stops>-loctype )
              evt_exp_datetime  = <ls_stops>-pln_evt_datetime
              evt_exp_tzone     = <ls_stops>-pln_evt_timezone
              milestonenum      = lv_milestonenum
          ) ).

          ADD 1 TO lv_milestonenum.

        ENDIF.
      ELSE.
        MESSAGE e005(zgtt)
          WITH |{ <ls_watching>-stopid }{ <ls_watching>-loccat }| 'STOPS'
          INTO DATA(lv_dummy).
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ENDLOOP.

    " fill sequence number in DLV events
    lv_milestonenum = zcl_gtt_tools=>get_next_sequence_id(
      it_expeventdata = lt_exp_event ).

    LOOP AT lt_exp_event_dlv ASSIGNING FIELD-SYMBOL(<ls_exp_event_dlv>).
      <ls_exp_event_dlv>-milestonenum   = lv_milestonenum.
      ADD 1 TO lv_milestonenum.
    ENDLOOP.

    lt_exp_event    = VALUE #( BASE lt_exp_event
                                ( LINES OF lt_exp_event_dlv ) ).

    IF lt_exp_event[] IS INITIAL.
      lt_exp_event = VALUE #( (
          milestone         = ''
          locid2            = ''
          loctype           = ''
          locid1            = ''
          evt_exp_datetime  = '000000000000000'
          evt_exp_tzone     = ''
      ) ).
    ENDIF.

    LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
      <ls_exp_event>-appsys         = mv_appsys.
      <ls_exp_event>-appobjtype     = is_aotype-aot_type.
      <ls_exp_event>-appobjid = get_delivery_item_tracking_id(
        is_lips = is_lips ).
      <ls_exp_event>-language       = sy-langu.
      IF <ls_exp_event>-evt_exp_tzone IS INITIAL.
        <ls_exp_event>-evt_exp_tzone  = zcl_gtt_tools=>get_system_time_zone(  ).
      ENDIF.
    ENDLOOP.

    cs_idoc_data-exp_event = VALUE #( BASE cs_idoc_data-exp_event
                                      ( LINES OF lt_exp_event ) ).

  ENDMETHOD.


  METHOD fill_idoc_tracking_id.

*    "another tip is that: for tracking ID type 'SHIPMENT_ORDER' of delivery header,
*    "and for tracking ID type 'RESOURCE' of shipment header,
*    "DO NOT enable START DATE and END DATE

    DATA:lv_dlvittrxcod     TYPE /saptrx/trxcod.

    lv_dlvittrxcod = zif_gtt_ef_constants=>cs_trxcod-dl_position.

    " Delivery Item
    cs_idoc_data-tracking_id    = VALUE #( BASE cs_idoc_data-tracking_id (
      appsys      = mv_appsys
      appobjtype  = is_aotype-aot_type
      appobjid    = get_delivery_item_tracking_id(
                      is_lips = is_lips )
      trxcod      = lv_dlvittrxcod
      trxid       = get_delivery_item_tracking_id(
                      is_lips = is_lips )
      timzon      = zcl_gtt_tools=>get_system_time_zone( )
    ) ).

  ENDMETHOD.


  METHOD get_aotype_restriction_id.

    rv_rst_id   = 'SH_TO_IDLI'.

  ENDMETHOD.


  METHOD get_delivery_item_tracking_id.

    rv_tracking_id = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
      ir_lips = REF #( is_lips ) ).

  ENDMETHOD.


  METHOD get_evtype_restriction_id.
    CLEAR: rv_rst_id.
  ENDMETHOD.


  METHOD get_instance.

    DATA(lt_trk_obj_type) = VALUE zif_gtt_ctp_types=>tt_trk_obj_type(
       ( zif_gtt_ef_constants=>cs_trk_obj_type-esc_shipmt )
       ( zif_gtt_ef_constants=>cs_trk_obj_type-esc_deliv )
    ).

    IF is_gtt_enabled( it_trk_obj_type = lt_trk_obj_type ) = abap_true.
      ro_sender  = NEW #( ).

      ro_sender->initiate( ).

    ENDIF.

  ENDMETHOD.


  METHOD get_object_type.

    rv_objtype  = zif_gtt_ef_constants=>cs_trk_obj_type-esc_deliv.

  ENDMETHOD.


  METHOD get_shipment_stop.

    LOOP AT is_stops-watching ASSIGNING FIELD-SYMBOL(<ls_watching>)
      WHERE vbeln       = is_vbfa-vbelv
        AND stopid(10)  = is_vbfa-vbeln.
      rv_stop   = <ls_watching>-stopid.

      IF iv_arrival = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_pod_relevant.

    CLEAR: rv_result.

    IF is_stops-locid   = is_lips-werks AND
       is_stops-loctype = zif_gtt_ef_constants=>cs_loc_types-plant.

      READ TABLE is_ship-ee_rel ASSIGNING FIELD-SYMBOL(<ls_ee_rel>)
        WITH TABLE KEY appobjid = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
                                      ir_lips = REF #( is_lips ) ).

      rv_result = boolc( sy-subrc = 0 AND
                         <ls_ee_rel>-z_pdstk = abap_true ).
    ENDIF.

  ENDMETHOD.


  METHOD prepare_idoc_data.

    " fill DLV Item
    "   control data
    "     YN_DLV_NO
    "     YN_DLV_ITEM_NO
    "     ACTUAL_BUSINESS_TIMEZONE
    "     ACTUAL_BUSINESS_DATETIME
    "   planned events
    "     PUTAWAY
    "     PACKING
    "     GOODS_RECEIPT
    "     DEPARTURE
    "     ARRIV_DEST
    "     POD
    DATA: ls_idoc_data    TYPE zif_gtt_ctp_types=>ts_idoc_data.

    DATA(lr_ship)   = io_ship_data->get_data( ).
    DATA(lr_stops)  = io_ship_data->get_stops( ).

    FIELD-SYMBOLS: <ls_ship>  TYPE zcl_gtt_mia_ctp_shipment_data=>ts_shipment_merge,
                   <lt_stops> TYPE zcl_gtt_mia_ctp_shipment_data=>tt_stops_srt.

    ASSIGN lr_ship->*  TO <ls_ship>.
    ASSIGN lr_stops->* TO <lt_stops>.

    LOOP AT mt_aotype ASSIGNING FIELD-SYMBOL(<ls_aotype>).
      CLEAR: ls_idoc_data.

      ls_idoc_data-appsys   = mv_appsys.

      fill_idoc_trxserv(
        EXPORTING
          is_aotype    = <ls_aotype>
        CHANGING
          cs_idoc_data = ls_idoc_data ).

      LOOP AT <ls_ship>-likp ASSIGNING FIELD-SYMBOL(<ls_likp>).
        READ TABLE <lt_stops> ASSIGNING FIELD-SYMBOL(<ls_stops>)
          WITH TABLE KEY tknum = <ls_likp>-tknum.

        IF sy-subrc = 0.
          LOOP AT <ls_ship>-likp\lips[ <ls_likp> ] ASSIGNING FIELD-SYMBOL(<ls_lips>).
            fill_idoc_appobj_ctabs(
              EXPORTING
                is_aotype    = <ls_aotype>
                is_lips      = <ls_lips>
              CHANGING
                cs_idoc_data = ls_idoc_data ).

            fill_idoc_control_data(
              EXPORTING
                is_aotype    = <ls_aotype>
                is_ship      = <ls_ship>
                is_likp      = <ls_likp>
                is_lips      = <ls_lips>
                is_stops     = <ls_stops>
              CHANGING
                cs_idoc_data = ls_idoc_data ).

            fill_idoc_exp_event(
              EXPORTING
                is_aotype    = <ls_aotype>
                is_ship      = <ls_ship>
                is_likp      = <ls_likp>
                is_lips      = <ls_lips>
                is_stops     = <ls_stops>
              CHANGING
                cs_idoc_data = ls_idoc_data ).

            fill_idoc_tracking_id(
              EXPORTING
                is_aotype    = <ls_aotype>
                is_ship      = <ls_ship>
                is_likp      = <ls_likp>
                is_lips      = <ls_lips>
              CHANGING
                cs_idoc_data = ls_idoc_data ).
          ENDLOOP.
        ELSE.
          MESSAGE e005(zgtt) WITH |{ <ls_likp>-tknum }| 'STOPS'
            INTO DATA(lv_dummy).
          zcl_gtt_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.

      IF ls_idoc_data-appobj_ctabs[] IS NOT INITIAL AND
         ls_idoc_data-control[] IS NOT INITIAL AND
         ls_idoc_data-tracking_id[] IS NOT INITIAL.
        APPEND ls_idoc_data TO mt_idoc_data.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_gtt_mia_ctp_snd_sh_to_dli DEFINITION
  PUBLIC
  INHERITING FROM zcl_gtt_ctp_snd
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_sender) TYPE REF TO zcl_gtt_mia_ctp_snd_sh_to_dli
      RAISING
        cx_udm_message .
    METHODS prepare_idoc_data
      IMPORTING
        !io_ship_data TYPE REF TO zcl_gtt_mia_ctp_shipment_data
      RAISING
        cx_udm_message .
  PROTECTED SECTION.

    METHODS get_aotype_restriction_id
        REDEFINITION .
    METHODS get_object_type
        REDEFINITION .
    METHODS get_evtype_restriction_id
        REDEFINITION .
private section.

  constants:
    BEGIN OF cs_mapping,
        vbeln TYPE /saptrx/paramname VALUE 'YN_DLV_NO',
        posnr TYPE /saptrx/paramname VALUE 'YN_DLV_ITEM_NO',
      END OF cs_mapping .

  methods FILL_IDOC_APPOBJ_CTABS
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_LIPS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIPS
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_CONTROL_DATA
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_SHIP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_SHIPMENT_MERGE
      !IS_LIKP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIKPEX
      !IS_LIPS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIPS
      !IS_STOPS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_STOPS
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_EXP_EVENT
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_SHIP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_SHIPMENT_MERGE
      !IS_LIKP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIKPEX
      !IS_LIPS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIPS
      !IS_STOPS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_STOPS
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_TRACKING_ID
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_SHIP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_SHIPMENT_MERGE
      !IS_LIKP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIKPEX
      !IS_LIPS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIPS
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods GET_SHIPMENT_STOP
    importing
      !IS_STOPS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_STOPS
      !IS_VBFA type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_VBFAEX
      !IV_ARRIVAL type ABAP_BOOL
    returning
      value(RV_STOP) type ZIF_GTT_MIA_APP_TYPES=>TV_STOPID .
  class-methods GET_DELIVERY_ITEM_TRACKING_ID
    importing
      !IS_LIPS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIPS
    returning
      value(RV_TRACKING_ID) type /SAPTRX/TRXID
    raising
      CX_UDM_MESSAGE .
  methods IS_POD_RELEVANT
    importing
      !IS_SHIP type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_SHIPMENT_MERGE
      !IS_LIPS type ZCL_GTT_MIA_CTP_SHIPMENT_DATA=>TS_LIPS
      !IS_STOPS type ZIF_GTT_MIA_APP_TYPES=>TS_STOPS
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_CTP_SND_TOR_TO_DLH definition
  public
  inheriting from ZCL_GTT_CTP_SND
  create private .

public section.

  class-methods GET_INSTANCE
    returning
      value(RO_SENDER) type ref to ZCL_GTT_MIA_CTP_SND_TOR_TO_DLH
    raising
      CX_UDM_MESSAGE .
  methods PREPARE_IDOC_DATA
    importing
      !IO_DL_HEAD_DATA type ref to ZCL_GTT_MIA_CTP_DAT_TOR_TO_DLH
    raising
      CX_UDM_MESSAGE .
  PROTECTED SECTION.

    METHODS get_aotype_restriction_id
        REDEFINITION .
    METHODS get_object_type
        REDEFINITION .
    METHODS get_evtype_restriction_id
        REDEFINITION .
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF cs_mapping,
        vbeln        TYPE /saptrx/paramname VALUE 'YN_DLV_NO',
        fu_relevant  TYPE /saptrx/paramname VALUE 'YN_DL_FU_RELEVANT',
        pod_relevant TYPE /saptrx/paramname VALUE 'YN_DL_POD_RELEVANT',
      END OF cs_mapping .

    METHODS fill_idoc_appobj_ctabs
      IMPORTING
        !is_aotype    TYPE zif_gtt_ctp_types=>ts_aotype
        !is_likp      TYPE zif_gtt_mia_ctp_types=>ts_delivery
      CHANGING
        !cs_idoc_data TYPE zif_gtt_ctp_types=>ts_idoc_data
      RAISING
        cx_udm_message.
    METHODS fill_idoc_control_data
      IMPORTING
        !is_aotype    TYPE zif_gtt_ctp_types=>ts_aotype
        !is_delivery  TYPE zif_gtt_mia_ctp_types=>ts_delivery
      CHANGING
        !cs_idoc_data TYPE zif_gtt_ctp_types=>ts_idoc_data
      RAISING
        cx_udm_message .
    METHODS fill_idoc_exp_event
      IMPORTING
        !is_aotype    TYPE zif_gtt_ctp_types=>ts_aotype
        !is_delivery  TYPE zif_gtt_mia_ctp_types=>ts_delivery
      CHANGING
        !cs_idoc_data TYPE zif_gtt_ctp_types=>ts_idoc_data
      RAISING
        cx_udm_message .
    METHODS fill_idoc_tracking_id
      IMPORTING
        !is_aotype    TYPE zif_gtt_ctp_types=>ts_aotype
        !is_delivery  TYPE zif_gtt_mia_ctp_types=>ts_delivery
      CHANGING
        !cs_idoc_data TYPE zif_gtt_ctp_types=>ts_idoc_data
      RAISING
        cx_udm_message .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_CTP_SND_TOR_TO_DLH IMPLEMENTATION.


  METHOD fill_idoc_appobj_ctabs.

    cs_idoc_data-appobj_ctabs = VALUE #( BASE cs_idoc_data-appobj_ctabs (
      trxservername = cs_idoc_data-trxserv-trx_server_id
      appobjtype    = is_aotype-aot_type
      appobjid      = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
                        ir_likp = REF #( is_likp ) )
    ) ).

  ENDMETHOD.


  METHOD fill_idoc_control_data.

    DATA: lt_control TYPE /saptrx/bapi_trk_control_tab,
          lv_count   TYPE i VALUE 0.

    " PO Item key data (obligatory)
    lt_control  = VALUE #(
      (
        paramname = cs_mapping-vbeln
        value     = zcl_gtt_mia_dl_tools=>get_formated_dlv_number(
                      ir_likp = REF #( is_delivery ) )
      )
      (
        paramname = cs_mapping-fu_relevant
        value     = is_delivery-fu_relevant
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_timezone
        value     = zcl_gtt_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_technical_timezone
        value     = zcl_gtt_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_technical_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-reported_by
        value     = sy-uname
      )
    ).

    " fill technical data into all control data records
    LOOP AT lt_control ASSIGNING FIELD-SYMBOL(<ls_control>).
      <ls_control>-appsys     = mv_appsys.
      <ls_control>-appobjtype = is_aotype-aot_type.
      <ls_control>-appobjid = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
        ir_likp = REF #( is_delivery ) ).
    ENDLOOP.

    cs_idoc_data-control  = VALUE #( BASE cs_idoc_data-control
                                     ( LINES OF lt_control ) ).

  ENDMETHOD.


  METHOD fill_idoc_exp_event.

    DATA: lt_exp_event TYPE /saptrx/bapi_trk_ee_tab.

    " do not retrieve planned events when DLV is not stored in DB yet
    " (this is why all the fields of LIKP are empty, except VBELN)
    IF is_delivery-likp-lfart IS NOT INITIAL AND
       is_delivery-lips[] IS NOT INITIAL.

      TRY.
          zcl_gtt_mia_ctp_tools=>get_delivery_head_planned_evt(
            EXPORTING
              iv_appsys    = mv_appsys
              is_aotype    = is_aotype
              is_likp      = CORRESPONDING #( is_delivery-likp )
              it_lips      = CORRESPONDING #( is_delivery-lips )
            IMPORTING
              et_exp_event = lt_exp_event ).

          IF lt_exp_event[] IS INITIAL.
            lt_exp_event = VALUE #( (
                milestone         = ''
                locid2            = ''
                loctype           = ''
                locid1            = ''
                evt_exp_datetime  = '000000000000000'
                evt_exp_tzone     = ''
            ) ).
          ENDIF.

          LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
            <ls_exp_event>-appsys         = mv_appsys.
            <ls_exp_event>-appobjtype     = is_aotype-aot_type.
            <ls_exp_event>-appobjid = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
              ir_likp = REF #( is_delivery ) ).
            <ls_exp_event>-language       = sy-langu.
            IF <ls_exp_event>-evt_exp_tzone IS INITIAL.
              <ls_exp_event>-evt_exp_tzone  = zcl_gtt_tools=>get_system_time_zone( ).
            ENDIF.
          ENDLOOP.

          cs_idoc_data-exp_event = VALUE #( BASE cs_idoc_data-exp_event
                                            ( LINES OF lt_exp_event ) ).

        CATCH cx_udm_message INTO DATA(lo_udm_message).
          " do not throw exception when DLV is not stored in DB yet
          " (this is why all the fields of LIKP are empty, except VBELN)
          IF is_delivery-likp-lfart IS NOT INITIAL.
            RAISE EXCEPTION TYPE cx_udm_message
              EXPORTING
                textid   = lo_udm_message->textid
                previous = lo_udm_message->previous
                m_msgid  = lo_udm_message->m_msgid
                m_msgty  = lo_udm_message->m_msgty
                m_msgno  = lo_udm_message->m_msgno
                m_msgv1  = lo_udm_message->m_msgv1
                m_msgv2  = lo_udm_message->m_msgv2
                m_msgv3  = lo_udm_message->m_msgv3
                m_msgv4  = lo_udm_message->m_msgv4.
          ENDIF.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD fill_idoc_tracking_id.

    DATA:lv_dlvhdtrxcod     TYPE /saptrx/trxcod.

    lv_dlvhdtrxcod = zif_gtt_ef_constants=>cs_trxcod-dl_number.

    " Delivery Header
    cs_idoc_data-tracking_id  = VALUE #( BASE cs_idoc_data-tracking_id (
      appsys      = mv_appsys
      appobjtype  = is_aotype-aot_type
      appobjid    = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
                      ir_likp = REF #( is_delivery ) )
      trxcod      = lv_dlvhdtrxcod
      trxid       = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
                      ir_likp = REF #( is_delivery ) )
      timzon      = zcl_gtt_tools=>get_system_time_zone( )
    ) ).

  ENDMETHOD.


  METHOD get_aotype_restriction_id.

    rv_rst_id   = 'FU_TO_IDLH'.

  ENDMETHOD.


  METHOD get_evtype_restriction_id.
    CLEAR: rv_rst_id.
  ENDMETHOD.


  METHOD get_instance.

    DATA(lt_trk_obj_type) = VALUE zif_gtt_ctp_types=>tt_trk_obj_type(
       ( zif_gtt_ef_constants=>cs_trk_obj_type-tms_tor )
       ( zif_gtt_ef_constants=>cs_trk_obj_type-esc_deliv )
    ).

    IF is_gtt_enabled( it_trk_obj_type = lt_trk_obj_type ) = abap_true.
      ro_sender  = NEW #( ).

      ro_sender->initiate( ).
    ELSE.
      MESSAGE e006(zgtt) INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_object_type.

    rv_objtype  = zif_gtt_ef_constants=>cs_trk_obj_type-esc_deliv.

  ENDMETHOD.


  METHOD prepare_idoc_data.

    DATA: ls_idoc_data    TYPE zif_gtt_ctp_types=>ts_idoc_data.

    DATA(lr_dlv)   = io_dl_head_data->get_deliveries( ).

    FIELD-SYMBOLS: <lt_dlv>  TYPE zif_gtt_mia_ctp_types=>tt_delivery.

    ASSIGN lr_dlv->*  TO <lt_dlv>.

    LOOP AT mt_aotype ASSIGNING FIELD-SYMBOL(<ls_aotype>).
      CLEAR: ls_idoc_data.

      ls_idoc_data-appsys   = mv_appsys.

      fill_idoc_trxserv(
        EXPORTING
          is_aotype    = <ls_aotype>
        CHANGING
          cs_idoc_data = ls_idoc_data ).

      LOOP AT <lt_dlv> ASSIGNING FIELD-SYMBOL(<ls_dlv>).

        IF <ls_dlv>-likp-lfart IS INITIAL. " Skip cross TP if DLV not stored in DB
          CONTINUE.
        ENDIF.

        fill_idoc_appobj_ctabs(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_likp      = <ls_dlv>
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_control_data(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_delivery  = <ls_dlv>
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_exp_event(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_delivery  = <ls_dlv>
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_tracking_id(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_delivery  = <ls_dlv>
          CHANGING
            cs_idoc_data = ls_idoc_data ).
      ENDLOOP.

      IF ls_idoc_data-appobj_ctabs[] IS NOT INITIAL AND
         ls_idoc_data-control[] IS NOT INITIAL AND
         ls_idoc_data-tracking_id[] IS NOT INITIAL.
        APPEND ls_idoc_data TO mt_idoc_data.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r""" GET_INSTANCE
    returning
      value(RO_SENDER) type ref to ZCL_GTT_MIA_CTP_SND_TOR_TO_DLI
    raising
      CX_UDM_MESSAGE .
  methods PREPARE_IDOC_DATA
    importing
      !IO_DL_ITEM_DATA type ref to ZCL_GTT_MIA_CTP_DAT_TOR_TO_DLI
    raising
      CX_UDM_MESSAGE .
  PROTECTED SECTION.

    METHODS get_aotype_restriction_id
        REDEFINITION .
    METHODS get_object_type
        REDEFINITION .
    METHODS get_evtype_restriction_id
        REDEFINITION .
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF cs_mapping,
        vbeln            TYPE /saptrx/paramname VALUE 'YN_DL_DELEVERY',
        posnr            TYPE /saptrx/paramname VALUE 'YN_DL_DELEVERY_ITEM',
        fu_lineno        TYPE /saptrx/paramname VALUE 'YN_DL_FU_LINE_COUNT',
        fu_freightunit   TYPE /saptrx/paramname VALUE 'YN_DL_FU_NO',
        fu_itemnumber    TYPE /saptrx/paramname VALUE 'YN_DL_FU_ITEM_NO',
        fu_quantity      TYPE /saptrx/paramname VALUE 'YN_DL_FU_QUANTITY',
        fu_quantityuom   TYPE /saptrx/paramname VALUE 'YN_DL_FU_UNITS',
        fu_product_id    TYPE /saptrx/paramname VALUE 'YN_DL_FU_PRODUCT',
        fu_product_descr TYPE /saptrx/paramname VALUE 'YN_DL_FU_PRODUCT_DESCR',
      END OF cs_mapping .

    METHODS fill_idoc_appobj_ctabs
      IMPORTING
        !is_aotype    TYPE zif_gtt_ctp_types=>ts_aotype
        !is_lips      TYPE zif_gtt_mia_app_types=>ts_lipsvb
      CHANGING
        !cs_idoc_data TYPE zif_gtt_ctp_types=>ts_idoc_data
      RAISING
        cx_udm_message .
    METHODS fill_idoc_control_data
      IMPORTING
        !is_aotype    TYPE zif_gtt_ctp_types=>ts_aotype
        !is_likp      TYPE zif_gtt_mia_app_types=>ts_likpvb
        !is_lips      TYPE zif_gtt_mia_app_types=>ts_lipsvb
        !it_fu_list   TYPE zif_gtt_mia_ctp_types=>tt_fu_list
      CHANGING
        !cs_idoc_data TYPE zif_gtt_ctp_types=>ts_idoc_data
      RAISING
        cx_udm_message .
    METHODS fill_idoc_exp_event
      IMPORTING
        !is_aotype    TYPE zif_gtt_ctp_types=>ts_aotype
        !is_likp      TYPE zif_gtt_mia_app_types=>ts_likpvb
        !is_lips      TYPE zif_gtt_mia_app_types=>ts_lipsvb
      CHANGING
        !cs_idoc_data TYPE zif_gtt_ctp_types=>ts_idoc_data
      RAISING
        cx_udm_message .
    METHODS fill_idoc_tracking_id
      IMPORTING
        !is_aotype    TYPE zif_gtt_ctp_types=>ts_aotype
        !is_lips      TYPE zif_gtt_mia_app_types=>ts_lipsvb
        !it_fu_list   TYPE zif_gtt_mia_ctp_types=>tt_fu_list
      CHANGING
        !cs_idoc_data TYPE zif_gtt_ctp_types=>ts_idoc_data
      RAISING
        cx_udm_message .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_CTP_SND_TOR_TO_DLI IMPLEMENTATION.


  METHOD fill_idoc_appobj_ctabs.

    cs_idoc_data-appobj_ctabs = VALUE #( BASE cs_idoc_data-appobj_ctabs (
      trxservername = cs_idoc_data-trxserv-trx_server_id
      appobjtype    = is_aotype-aot_type
      appobjid      = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
                        ir_lips = REF #( is_lips ) )
    ) ).

  ENDMETHOD.


  METHOD fill_idoc_control_data.

    DATA: lt_control TYPE /saptrx/bapi_trk_control_tab,
          lv_count   TYPE i VALUE 0.
    DATA: lt_tmp_fu_list TYPE zif_gtt_mia_ctp_types=>tt_fu_list.

    " DL Item key data (obligatory)
    lt_control  = VALUE #(
      (
        paramname = cs_mapping-vbeln
        value     = zcl_gtt_mia_dl_tools=>get_formated_dlv_number(
                      ir_likp = REF #( is_likp ) )
      )
      (
        paramname = cs_mapping-posnr
        value     = zcl_gtt_mia_dl_tools=>get_formated_dlv_item(
                      ir_lips = REF #( is_lips ) )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_timezone
        value     = zcl_gtt_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_technical_timezone
        value     = zcl_gtt_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_technical_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-reported_by
        value     = sy-uname
      )
    ).

    DATA(lt_fu_list) = it_fu_list.

    " fill F.U. table
    LOOP AT it_fu_list ASSIGNING FIELD-SYMBOL(<ls_fu_list>).
      READ TABLE lt_fu_list INTO DATA(ls_fu_delete) WITH KEY tor_id = <ls_fu_list>-tor_id change_mode = /bobf/if_frw_c=>sc_modify_delete.
      IF ls_fu_delete IS NOT INITIAL.
        CLEAR ls_fu_delete.
        CONTINUE.
      ENDIF.
      APPEND <ls_fu_list> TO lt_tmp_fu_list.
      ADD 1 TO lv_count.

      lt_control  = VALUE #( BASE lt_control
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_lineno
          value       = zcl_gtt_tools=>get_pretty_value(
                          iv_value = lv_count )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_freightunit
          value       = zcl_gtt_mia_tm_tools=>get_formated_tor_id(
                          ir_data = REF #( <ls_fu_list> ) )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_itemnumber
          value       = zcl_gtt_mia_tm_tools=>get_formated_tor_item(
                          ir_data = REF #( <ls_fu_list> ) )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_quantity
          value       = zcl_gtt_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-quantity )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_quantityuom
          value       = zcl_gtt_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-quantityuom )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_product_id
          value       = zcl_gtt_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-product_id )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_product_descr
          value       = zcl_gtt_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-product_descr )
        )
      ).
    ENDLOOP.

    " add deletion sign in case of emtpy IT_FU_LIST table
    IF lt_tmp_fu_list IS INITIAL.
      lt_control  = VALUE #( BASE lt_control (
          paramindex = 1
          paramname  = cs_mapping-fu_lineno
          value      = ''
      ) ).
    ENDIF.

    " fill technical data into all control data records
    LOOP AT lt_control ASSIGNING FIELD-SYMBOL(<ls_control>).
      <ls_control>-appsys     = mv_appsys.
      <ls_control>-appobjtype = is_aotype-aot_type.
      <ls_control>-appobjid = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
        ir_lips = REF #( is_lips ) ).
    ENDLOOP.

    cs_idoc_data-control  = VALUE #( BASE cs_idoc_data-control
                                     ( LINES OF lt_control ) ).

  ENDMETHOD.


  METHOD fill_idoc_exp_event.

    DATA: lt_exp_event TYPE /saptrx/bapi_trk_ee_tab.

    " do not retrieve planned events when DLV is not stored in DB yet
    " (this is why all the fields of LIKP are empty, except VBELN)
    IF is_likp-lfart IS NOT INITIAL AND
       is_lips-pstyv IS NOT INITIAL.

      TRY.
          zcl_gtt_mia_ctp_tools=>get_delivery_item_planned_evt(
            EXPORTING
              iv_appsys    = mv_appsys
              is_aotype    = is_aotype
              is_likp      = is_likp
              is_lips      = is_lips
            IMPORTING
              et_exp_event = lt_exp_event ).

          IF lt_exp_event[] IS INITIAL.
            lt_exp_event = VALUE #( (
                milestone         = ''
                locid2            = ''
                loctype           = ''
                locid1            = ''
                evt_exp_datetime  = '000000000000000'
                evt_exp_tzone     = ''
            ) ).
          ENDIF.

          LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
            <ls_exp_event>-appsys         = mv_appsys.
            <ls_exp_event>-appobjtype     = is_aotype-aot_type.
            <ls_exp_event>-appobjid = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
              ir_lips = REF #( is_lips ) ).
            <ls_exp_event>-language       = sy-langu.
            IF <ls_exp_event>-evt_exp_tzone IS INITIAL.
              <ls_exp_event>-evt_exp_tzone  = zcl_gtt_tools=>get_system_time_zone(  ).
            ENDIF.
          ENDLOOP.

          cs_idoc_data-exp_event = VALUE #( BASE cs_idoc_data-exp_event
                                            ( LINES OF lt_exp_event ) ).

        CATCH cx_udm_message INTO DATA(lo_udm_message).
          RAISE EXCEPTION TYPE cx_udm_message
            EXPORTING
              textid   = lo_udm_message->textid
              previous = lo_udm_message->previous
              m_msgid  = lo_udm_message->m_msgid
              m_msgty  = lo_udm_message->m_msgty
              m_msgno  = lo_udm_message->m_msgno
              m_msgv1  = lo_udm_message->m_msgv1
              m_msgv2  = lo_udm_message->m_msgv2
              m_msgv3  = lo_udm_message->m_msgv3
              m_msgv4  = lo_udm_message->m_msgv4.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD fill_idoc_tracking_id.

    DATA:
      lv_dlvittrxcod     TYPE /saptrx/trxcod,
      lv_futrxcod        TYPE /saptrx/trxcod.

    DATA:
      lt_tracking_id TYPE /saptrx/bapi_trk_trkid_tab,
      lt_fu_id       TYPE zif_gtt_mia_ctp_types=>tt_fu_id.

    DATA:
      ls_fu_create TYPE zif_gtt_mia_ctp_types=>ts_fu_list,
      ls_fu_update TYPE zif_gtt_mia_ctp_types=>ts_fu_list,
      ls_fu_delete TYPE zif_gtt_mia_ctp_types=>ts_fu_list.

    lv_dlvittrxcod = zif_gtt_ef_constants=>cs_trxcod-dl_position.
    lv_futrxcod    = zif_gtt_ef_constants=>cs_trxcod-fu_number.

    " Delivery Item
    cs_idoc_data-tracking_id  = VALUE #( BASE cs_idoc_data-tracking_id (
      appsys      = mv_appsys
      appobjtype  = is_aotype-aot_type
      appobjid    = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
                      ir_lips = REF #( is_lips ) )
      trxcod      = lv_dlvittrxcod
      trxid       = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
                      ir_lips = REF #( is_lips ) )
      timzon      = zcl_gtt_tools=>get_system_time_zone( )
    ) ).

    lt_fu_id =  CORRESPONDING #( it_fu_list ).
    SORT lt_fu_id BY tor_id.
    DELETE ADJACENT DUPLICATES FROM lt_fu_id COMPARING tor_id.
    " Freight unit
    LOOP AT lt_fu_id  ASSIGNING FIELD-SYMBOL(<ls_fu_id>).

      CLEAR:ls_fu_create,ls_fu_update,ls_fu_delete.
      READ TABLE it_fu_list INTO ls_fu_create WITH KEY tor_id = <ls_fu_id>-tor_id change_mode = /bobf/if_frw_c=>sc_modify_create.
      READ TABLE it_fu_list INTO ls_fu_update WITH KEY tor_id = <ls_fu_id>-tor_id change_mode = /bobf/if_frw_c=>sc_modify_update.
      READ TABLE it_fu_list INTO ls_fu_delete WITH KEY tor_id = <ls_fu_id>-tor_id change_mode = /bobf/if_frw_c=>sc_modify_delete.

      IF ls_fu_create IS NOT INITIAL AND
         ls_fu_update IS INITIAL AND
         ls_fu_delete IS INITIAL.
        lt_tracking_id    = VALUE #( BASE lt_tracking_id (
          trxcod      = lv_futrxcod
          trxid       = zcl_gtt_mia_tm_tools=>get_formated_tor_id( ir_data = REF #( <ls_fu_id> ) )
        ) ).

      ENDIF.

      IF ls_fu_delete IS NOT INITIAL AND
         ls_fu_update IS INITIAL AND
         ls_fu_create IS INITIAL.
        lt_tracking_id    = VALUE #( BASE lt_tracking_id (
          trxcod      = lv_futrxcod
          trxid       = zcl_gtt_mia_tm_tools=>get_formated_tor_id( ir_data = REF #( <ls_fu_id> ) )
          action      = zif_gtt_ef_constants=>cs_change_mode-delete
        ) ).

      ENDIF.
    ENDLOOP.

*    " Fill general data
    LOOP AT lt_tracking_id ASSIGNING FIELD-SYMBOL(<ls_tracking_id>).
      <ls_tracking_id>-appsys     = mv_appsys.
      <ls_tracking_id>-appobjtype = is_aotype-aot_type.
      <ls_tracking_id>-appobjid = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
        ir_lips = REF #( is_lips ) ).
    ENDLOOP.

    cs_idoc_data-tracking_id = VALUE #( BASE cs_idoc_data-tracking_id
                                        ( LINES OF lt_tracking_id ) ).

  ENDMETHOD.


  METHOD get_aotype_restriction_id.

    rv_rst_id   = 'FU_TO_IDLI'.

  ENDMETHOD.


  METHOD get_evtype_restriction_id.
    CLEAR rv_rst_id.
  ENDMETHOD.


  METHOD get_instance.

    DATA(lt_trk_obj_type) = VALUE zif_gtt_ctp_types=>tt_trk_obj_type(
       ( zif_gtt_ef_constants=>cs_trk_obj_type-tms_tor )
       ( zif_gtt_ef_constants=>cs_trk_obj_type-esc_deliv )
    ).

    IF is_gtt_enabled( it_trk_obj_type = lt_trk_obj_type ) = abap_true.
      ro_sender  = NEW #( ).

      ro_sender->initiate( ).
    ELSE.
      MESSAGE e006(zgtt) INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_object_type.

    rv_objtype  = zif_gtt_ef_constants=>cs_trk_obj_type-esc_deliv.

  ENDMETHOD.


  METHOD prepare_idoc_data.

    DATA: ls_idoc_data    TYPE zif_gtt_ctp_types=>ts_idoc_data.

    DATA(lr_dlv_item)   = io_dl_item_data->get_delivery_items( ).

    FIELD-SYMBOLS: <lt_dlv_item>  TYPE zif_gtt_mia_ctp_types=>tt_delivery_item.

    ASSIGN lr_dlv_item->*  TO <lt_dlv_item>.

    LOOP AT mt_aotype ASSIGNING FIELD-SYMBOL(<ls_aotype>).
      CLEAR: ls_idoc_data.

      ls_idoc_data-appsys   = mv_appsys.

      fill_idoc_trxserv(
        EXPORTING
          is_aotype    = <ls_aotype>
        CHANGING
          cs_idoc_data = ls_idoc_data ).

      LOOP AT <lt_dlv_item> ASSIGNING FIELD-SYMBOL(<ls_dlv_fu>).

        " Skip for new created delivery item.
        IF <ls_dlv_fu>-lips-pstyv IS INITIAL.
          CONTINUE.
        ENDIF.
        fill_idoc_appobj_ctabs(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_lips      = <ls_dlv_fu>-lips
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_control_data(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_likp      = <ls_dlv_fu>-likp
            is_lips      = <ls_dlv_fu>-lips
            it_fu_list   = <ls_dlv_fu>-fu_list
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_exp_event(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_likp      = <ls_dlv_fu>-likp
            is_lips      = <ls_dlv_fu>-lips
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_tracking_id(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_lips      = <ls_dlv_fu>-lips
            it_fu_list   = <ls_dlv_fu>-fu_list
          CHANGING
            cs_idoc_data = ls_idoc_data ).
      ENDLOOP.

      IF ls_idoc_data-appobj_ctabs[] IS NOT INITIAL AND
         ls_idoc_data-control[] IS NOT INITIAL AND
         ls_idoc_data-tracking_id[] IS NOT INITIAL.
        APPEND ls_idoc_data TO mt_idoc_data.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_CTP_TOOLS definition
  public
  create public .

public section.

  class-methods GET_DELIVERY_HEAD_PLANNED_EVT
    importing
      !IV_APPSYS type LOGSYS
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_LIKP type ZIF_GTT_MIA_APP_TYPES=>TS_LIKPVB
      !IT_LIPS type ZIF_GTT_MIA_APP_TYPES=>TT_LIPSVB
    exporting
      !ET_EXP_EVENT type /SAPTRX/BAPI_TRK_EE_TAB
    raising
      CX_UDM_MESSAGE .
  class-methods GET_DELIVERY_ITEM_PLANNED_EVT
    importing
      !IV_APPSYS type LOGSYS
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_LIKP type ZIF_GTT_MIA_APP_TYPES=>TS_LIKPVB
      !IS_LIPS type ZIF_GTT_MIA_APP_TYPES=>TS_LIPSVB
    exporting
      !ET_EXP_EVENT type /SAPTRX/BAPI_TRK_EE_TAB
    raising
      CX_UDM_MESSAGE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_CTP_TOOLS IMPLEMENTATION.


  METHOD get_delivery_head_planned_evt.

    DATA: ls_app_obj_types      TYPE /saptrx/aotypes,
          lt_all_appl_tables    TYPE trxas_tabcontainer,
          lt_app_type_cntl_tabs TYPE trxas_apptype_tabs,
          ls_app_objects        TYPE trxas_appobj_ctab_wa,
          lt_app_objects        TYPE trxas_appobj_ctabs.

    CLEAR: et_exp_event[].

    ls_app_obj_types              = CORRESPONDING #( is_aotype ).

    ls_app_objects                = CORRESPONDING #( ls_app_obj_types ).
    ls_app_objects-appobjtype     = is_aotype-aot_type.
    ls_app_objects-appobjid       = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
                                      ir_likp = REF #( is_likp ) ).
    ls_app_objects-maintabref     = REF #( is_likp ).
    ls_app_objects-maintabdef     = zif_gtt_mia_app_constants=>cs_tabledef-dl_header_new.

    lt_app_objects                = VALUE #( ( ls_app_objects ) ).

    lt_all_appl_tables            = VALUE #( (
      tabledef    = zif_gtt_mia_app_constants=>cs_tabledef-dl_item_new
      tableref    = REF #( it_lips )
     ) ).

    CALL FUNCTION 'ZGTT_MIA_EE_DL_HDR'
      EXPORTING
        i_appsys                  = iv_appsys
        i_app_obj_types           = CORRESPONDING /saptrx/aotypes( is_aotype )
        i_all_appl_tables         = lt_all_appl_tables
        i_app_type_cntl_tabs      = lt_app_type_cntl_tabs
        i_app_objects             = lt_app_objects
      TABLES
        e_expeventdata            = et_exp_event
      EXCEPTIONS
        parameter_error           = 1
        exp_event_determ_error    = 2
        table_determination_error = 3
        stop_processing           = 4
        OTHERS                    = 5.

    IF sy-subrc <> 0.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_delivery_item_planned_evt.

    DATA: ls_app_obj_types      TYPE /saptrx/aotypes,
          lt_all_appl_tables    TYPE trxas_tabcontainer,
          lt_app_type_cntl_tabs TYPE trxas_apptype_tabs,
          ls_app_objects        TYPE trxas_appobj_ctab_wa,
          lt_app_objects        TYPE trxas_appobj_ctabs,
          lt_lips               TYPE zif_gtt_mia_app_types=>tt_lipsvb.

    CLEAR: et_exp_event[].

    ls_app_obj_types              = CORRESPONDING #( is_aotype ).

    ls_app_objects                = CORRESPONDING #( ls_app_obj_types ).
    ls_app_objects-appobjtype     = is_aotype-aot_type.
    ls_app_objects-appobjid       = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
                                      ir_lips = REF #( is_lips ) ).
    ls_app_objects-maintabref     = REF #( is_lips ).
    ls_app_objects-maintabdef     = zif_gtt_mia_app_constants=>cs_tabledef-dl_item_new.
    ls_app_objects-mastertabref   = REF #( is_likp ).
    ls_app_objects-mastertabdef   = zif_gtt_mia_app_constants=>cs_tabledef-dl_header_new.

    lt_app_objects                = VALUE #( ( ls_app_objects ) ).

    lt_lips                       = VALUE #( ( CORRESPONDING #( is_lips ) ) ).

    lt_all_appl_tables            = VALUE #( (
      tabledef    = zif_gtt_mia_app_constants=>cs_tabledef-dl_item_new
      tableref    = REF #( lt_lips )
     ) ).

    CALL FUNCTION 'ZGTT_MIA_EE_DL_ITEM'
      EXPORTING
        i_appsys                  = iv_appsys
        i_app_obj_types           = CORRESPONDING /saptrx/aotypes( is_aotype )
        i_all_appl_tables         = lt_all_appl_tables
        i_app_type_cntl_tabs      = lt_app_type_cntl_tabs
        i_app_objects             = lt_app_objects
      TABLES
        e_expeventdata            = et_exp_event
      EXCEPTIONS
        parameter_error           = 1
        exp_event_determ_error    = 2
        table_determination_error = 3
        stop_processing           = 4
        OTHERS                    = 5.

    IF sy-subrc <> 0.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_CTP_TOR_CHANGES definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IT_TOR_ROOT_SSTRING type /SCMTMS/T_EM_BO_TOR_ROOT
      !IT_TOR_ROOT_BEFORE_SSTRING type /SCMTMS/T_EM_BO_TOR_ROOT
      !IT_TOR_ITEM_SSTRING type /SCMTMS/T_EM_BO_TOR_ITEM
      !IT_TOR_ITEM_BEFORE_SSTRING type /SCMTMS/T_EM_BO_TOR_ITEM
      !IT_TOR_STOP_SSTRING type /SCMTMS/T_EM_BO_TOR_STOP
      !IT_TOR_STOP_ADDR_SSTRING type /SCMTMS/T_EM_BO_LOC_ADDR .
  methods GET_DELIVERY_ITEMS
    returning
      value(RR_DELIVERIES) type ref to DATA .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_tor_root TYPE /scmtms/t_em_bo_tor_root .
    DATA mt_tor_root_before TYPE /scmtms/t_em_bo_tor_root .
    DATA mt_tor_item TYPE /scmtms/t_em_bo_tor_item .
    DATA mt_tor_item_before TYPE /scmtms/t_em_bo_tor_item .
    DATA mt_tor_stop TYPE /scmtms/t_em_bo_tor_stop .
    DATA mt_tor_stop_before TYPE /scmtms/t_em_bo_tor_stop .
    DATA mt_tor_stop_addr TYPE /scmtms/t_em_bo_loc_addr .
    DATA mt_allowed_fu_type TYPE zif_gtt_mia_ctp_types=>tt_tor_type .
    DATA mt_deliveries TYPE zif_gtt_mia_ctp_types=>tt_delivery .
    DATA mt_delivery_item TYPE zif_gtt_mia_ctp_types=>tt_delivery_chng .

    METHODS add_delivery_item
      IMPORTING
        !is_tor_root      TYPE /scmtms/s_em_bo_tor_root
        !is_tor_item      TYPE /scmtms/s_em_bo_tor_item
        !iv_change_mode   TYPE /bobf/conf_change_mode
      CHANGING
        !ct_delivery_item TYPE zif_gtt_mia_ctp_types=>tt_delivery_chng .
    METHODS add_delivery_items_by_tor_root
      IMPORTING
        !is_tor_root      TYPE /scmtms/s_em_bo_tor_root
        !iv_change_mode   TYPE /bobf/conf_change_mode
      CHANGING
        !ct_delivery_item TYPE zif_gtt_mia_ctp_types=>tt_delivery_chng .
    METHODS convert_dlv_number
      IMPORTING
        !iv_dlv_num     TYPE clike
      RETURNING
        VALUE(rv_vbeln) TYPE vbeln_vl .
    METHODS get_allowed_fu_types
      RETURNING
        VALUE(rt_fu_type) TYPE zif_gtt_mia_ctp_types=>tt_tor_type .
    METHODS get_aotype_restrictions
      EXPORTING
        !et_aotype TYPE zif_gtt_mia_ctp_types=>tt_aotype_rst .
    METHODS get_dlv_item_based_on_tor_root
      CHANGING
        !ct_delivery_item TYPE zif_gtt_mia_ctp_types=>tt_delivery_chng .
    METHODS get_dlv_item_based_on_tor_item
      CHANGING
        !ct_delivery_item TYPE zif_gtt_mia_ctp_types=>tt_delivery_chng .
    METHODS get_dlv_item_based_on_tor_stop
      CHANGING
        !ct_delivery_item TYPE zif_gtt_mia_ctp_types=>tt_delivery_chng .
    METHODS get_tor_types
      EXPORTING
        !et_tor_types TYPE zif_gtt_mia_ctp_types=>tt_tor_type_rst .
    METHODS get_tor_stop_before
      RETURNING
        VALUE(rt_tor_stop_before) TYPE /scmtms/t_em_bo_tor_stop .
    METHODS init_deliveries .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_CTP_TOR_CHANGES IMPLEMENTATION.


  METHOD add_delivery_item.

    DATA:
      ls_delivery_item TYPE zif_gtt_mia_ctp_types=>ts_delivery_chng,
      lv_matnr         TYPE mara-matnr.

    ls_delivery_item-vbeln        = |{ is_tor_item-base_btd_id ALPHA = IN }|.
    ls_delivery_item-posnr        = is_tor_item-base_btditem_id.
    ls_delivery_item-tor_id       = is_tor_root-tor_id.
    ls_delivery_item-item_id      = is_tor_item-item_id.
    ls_delivery_item-quantity     = is_tor_item-qua_pcs_val.
    ls_delivery_item-product_descr = is_tor_item-item_descr.
    ls_delivery_item-base_uom_val  = is_tor_item-base_uom_val.
    ls_delivery_item-change_mode  = iv_change_mode.

    zcl_gtt_sof_toolkit=>convert_unit_output(
      EXPORTING
        iv_input  = is_tor_item-qua_pcs_uni
      RECEIVING
        rv_output = ls_delivery_item-quantityuom ).

    zcl_gtt_sof_toolkit=>convert_unit_output(
      EXPORTING
        iv_input  = is_tor_item-base_uom_uni
      RECEIVING
        rv_output = ls_delivery_item-base_uom_uni ).

    zcl_gtt_tools=>convert_matnr_to_external_frmt(
      EXPORTING
        iv_material = is_tor_item-product_id
      IMPORTING
        ev_result   = lv_matnr ).
    ls_delivery_item-product_id = lv_matnr.
    CLEAR lv_matnr.

    INSERT ls_delivery_item INTO TABLE ct_delivery_item.

  ENDMETHOD.


  METHOD add_delivery_items_by_tor_root.

    DATA: ls_delivery_item TYPE zif_gtt_mia_ctp_types=>ts_delivery_chng.

    LOOP AT mt_tor_item ASSIGNING FIELD-SYMBOL(<ls_tor_item>)
      USING KEY item_parent WHERE parent_node_id = is_tor_root-node_id.

      IF <ls_tor_item>-base_btd_tco = zif_gtt_mia_ctp_tor_constants=>cv_base_btd_tco_inb_dlv AND
         <ls_tor_item>-base_btd_id     IS NOT INITIAL AND
         <ls_tor_item>-base_btditem_id IS NOT INITIAL AND
         <ls_tor_item>-item_cat = /scmtms/if_tor_const=>sc_tor_item_category-product.
        add_delivery_item(
          EXPORTING
            is_tor_root      = is_tor_root
            is_tor_item      = <ls_tor_item>
            iv_change_mode   = iv_change_mode
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    mt_tor_root        = it_tor_root_sstring.
    mt_tor_root_before = it_tor_root_before_sstring.
    mt_tor_item        = it_tor_item_sstring.
    mt_tor_item_before = it_tor_item_before_sstring.
    mt_tor_stop        = it_tor_stop_sstring.
    mt_tor_stop_addr   = it_tor_stop_addr_sstring.
    mt_tor_stop_before = get_tor_stop_before( ).

    init_deliveries( ).

  ENDMETHOD.


  METHOD convert_dlv_number.

    rv_vbeln    = |{ iv_dlv_num ALPHA = IN }|.

  ENDMETHOD.


  METHOD get_allowed_fu_types.

    DATA: lt_aotype       TYPE zif_gtt_mia_ctp_types=>tt_aottype,
          lt_aotype_rst   TYPE zif_gtt_mia_ctp_types=>tt_aotype_rst,
          lt_tor_type_rst TYPE zif_gtt_mia_ctp_types=>tt_tor_type_rst.

    get_tor_types(
      IMPORTING
        et_tor_types = lt_tor_type_rst ).

    get_aotype_restrictions(
      IMPORTING
        et_aotype = lt_aotype_rst ).

    IF lt_tor_type_rst[] IS NOT INITIAL.
      SELECT type AS tor_type, aotype
        FROM /scmtms/c_torty
        INTO TABLE @lt_aotype
        WHERE type   IN @lt_tor_type_rst
          AND aotype IN @lt_aotype_rst.
    ELSE.
      CLEAR rt_fu_type.
    ENDIF.

  ENDMETHOD.


  METHOD get_aotype_restrictions.

    et_aotype = VALUE #( (
      low     = 'ZGTT_IDLV_HD_*'
      option  = 'CP'
      sign    = 'I'
    ) ).

  ENDMETHOD.


  METHOD get_delivery_items.

    rr_deliveries   = REF #( mt_delivery_item ).

  ENDMETHOD.


  METHOD get_dlv_item_based_on_tor_item.

    DATA ls_delivery_item TYPE zif_gtt_mia_ctp_types=>ts_delivery_chng.

    LOOP AT mt_tor_item ASSIGNING FIELD-SYMBOL(<ls_tor_item>)
      WHERE base_btd_tco = zif_gtt_mia_ctp_tor_constants=>cv_base_btd_tco_inb_dlv AND
            ( change_mode = /bobf/if_frw_c=>sc_modify_delete OR
              change_mode = /bobf/if_frw_c=>sc_modify_create ) AND
              base_btd_id IS NOT INITIAL AND
              base_btditem_id IS NOT INITIAL AND
             item_cat = /scmtms/if_tor_const=>sc_tor_item_category-product.

      ASSIGN mt_tor_root[ node_id = <ls_tor_item>-parent_node_id ] TO FIELD-SYMBOL(<ls_tor_root>).
      IF sy-subrc = 0 AND
         <ls_tor_root>-tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit.

        add_delivery_item(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            is_tor_item      = <ls_tor_item>
            iv_change_mode   = <ls_tor_item>-change_mode
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_dlv_item_based_on_tor_root.

    LOOP AT mt_tor_root ASSIGNING FIELD-SYMBOL(<ls_tor_root>)
      WHERE tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit
        AND base_btd_tco = zif_gtt_mia_ctp_tor_constants=>cv_base_btd_tco_inb_dlv.

      IF <ls_tor_root>-change_mode = /bobf/if_frw_c=>sc_modify_delete.
        add_delivery_items_by_tor_root(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = /bobf/if_frw_c=>sc_modify_delete
          CHANGING
            ct_delivery_item = ct_delivery_item ).
        CONTINUE.
      ENDIF.

      ASSIGN mt_tor_root_before[ node_id = <ls_tor_root>-node_id ] TO FIELD-SYMBOL(<ls_tor_root_before>).
      IF sy-subrc = 0.
        CHECK <ls_tor_root_before>-base_btd_tco <> zif_gtt_mia_ctp_tor_constants=>cv_base_btd_tco_inb_dlv.

        add_delivery_items_by_tor_root(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = /bobf/if_frw_c=>sc_modify_update
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ELSE.
        add_delivery_items_by_tor_root(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = /bobf/if_frw_c=>sc_modify_create
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD get_dlv_item_based_on_tor_stop.

    DATA lv_tabix TYPE sy-tabix.

    LOOP AT mt_tor_stop ASSIGNING FIELD-SYMBOL(<ls_tor_stop>)
      GROUP BY ( parent_node_id = <ls_tor_stop>-parent_node_id
                 group_size     = GROUP SIZE ) ASSIGNING FIELD-SYMBOL(<lt_group>).

      LOOP AT GROUP <lt_group> ASSIGNING FIELD-SYMBOL(<ls_tor_stop_current>).
        lv_tabix += 1.
        CHECK lv_tabix = <lt_group>-group_size.

        ASSIGN mt_tor_stop_before[ node_id = <ls_tor_stop_current>-node_id ] TO FIELD-SYMBOL(<ls_tor_stop_before>).
        CHECK ( sy-subrc = 0 AND <ls_tor_stop_current>-log_locid <> <ls_tor_stop_before>-log_locid ) OR sy-subrc <> 0.

        " Freight Unit destination was changed => send IDOC
        ASSIGN mt_tor_root[ node_id = <ls_tor_stop_current>-parent_node_id ] TO FIELD-SYMBOL(<ls_tor_root>).
        CHECK sy-subrc = 0 AND <ls_tor_root>-tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit.

        add_delivery_items_by_tor_root(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = /bobf/if_frw_c=>sc_modify_update
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDLOOP.

      CLEAR lv_tabix.
    ENDLOOP.


  ENDMETHOD.


  METHOD get_tor_stop_before.

    DATA: ls_tor_stop_before TYPE /scmtms/s_em_bo_tor_stop.

    /scmtms/cl_tor_helper_stop=>get_stop_sequence(
      EXPORTING
        it_root_key     = VALUE #( FOR <ls_tor_root> IN mt_tor_root ( key = <ls_tor_root>-node_id ) )
        iv_before_image = abap_true
      IMPORTING
        et_stop_seq_d   = DATA(lt_stop_seq_d) ).

    LOOP AT lt_stop_seq_d ASSIGNING FIELD-SYMBOL(<ls_stop_seq_d>).

      LOOP AT <ls_stop_seq_d>-stop_seq ASSIGNING FIELD-SYMBOL(<ls_stop_seq>).
        DATA(lv_tabix) = sy-tabix.
        MOVE-CORRESPONDING <ls_stop_seq> TO ls_tor_stop_before.

        ls_tor_stop_before-parent_node_id = <ls_stop_seq>-root_key.

        ASSIGN <ls_stop_seq_d>-stop_map[ tabix = lv_tabix ] TO FIELD-SYMBOL(<ls_stop_map>).
        ls_tor_stop_before-node_id = <ls_stop_map>-stop_key.

        INSERT ls_tor_stop_before INTO TABLE rt_tor_stop_before.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_tor_types.

    et_tor_types = VALUE #(
      FOR <ls_tor_root> IN mt_tor_root
        WHERE ( tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit )
        ( low     = <ls_tor_root>-tor_type
          option  = 'EQ'
          sign    = 'I' )
    ).

  ENDMETHOD.


  METHOD init_deliveries.

    CLEAR: mt_delivery_item.

    get_dlv_item_based_on_tor_root(
      CHANGING
        ct_delivery_item = mt_delivery_item ).

    get_dlv_item_based_on_tor_item(
      CHANGING
        ct_delivery_item = mt_delivery_item ).

    get_dlv_item_based_on_tor_stop(
      CHANGING
        ct_delivery_item = mt_delivery_item ).

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_DL_TOOLS definition
  public
  create public .

public section.

  class-methods CONVERT_QUANTITY_INTO_POUNITS
    importing
      !IV_QUANTITY_UOM type ANY
      !IR_LIPS type ref to DATA
    returning
      value(RV_QUANTITY_POU) type F
    raising
      CX_UDM_MESSAGE .
  class-methods GET_ADDRES_INFO
    importing
      !IV_ADDR_TYPE type AD_ADRTYPE default ZIF_GTT_MIA_APP_CONSTANTS=>CS_ADRTYPE-ORGANIZATION
      !IV_ADDR_NUMB type AD_ADDRNUM
    exporting
      !EV_ADDRESS type CLIKE
      !EV_EMAIL type CLIKE
      !EV_TELEPHONE type CLIKE
    raising
      CX_UDM_MESSAGE .
  class-methods GET_DOOR_DESCRIPTION
    importing
      !IV_LGNUM type LGNUM
      !IV_LGTOR type LGTOR
    returning
      value(RV_DESCR) type /SAPTRX/PARAMVAL200
    raising
      CX_UDM_MESSAGE .
  class-methods GET_DELIVERY_DATE
    importing
      !IR_DATA type ref to DATA
    returning
      value(RV_DATE) type /SAPTRX/EVENT_EXP_DATETIME
    raising
      CX_UDM_MESSAGE .
  class-methods GET_FORMATED_DLV_ITEM
    importing
      !IR_LIPS type ref to DATA
    returning
      value(RV_POSNR) type CHAR6
    raising
      CX_UDM_MESSAGE .
  class-methods GET_FORMATED_DLV_NUMBER
    importing
      !IR_LIKP type ref to DATA
    returning
      value(RV_VBELN) type VBELN_VL
    raising
      CX_UDM_MESSAGE .
  class-methods GET_FORMATED_PO_ITEM
    importing
      !IR_LIPS type ref to DATA
    returning
      value(RV_PO_ITEM) type CHAR20
    raising
      CX_UDM_MESSAGE .
  class-methods GET_NEXT_EVENT_COUNTER
    returning
      value(RV_EVTCNT) type /SAPTRX/EVTCNT .
  class-methods GET_PLANT_ADDRESS_NUMBER
    importing
      !IV_WERKS type WERKS_D
    returning
      value(EV_ADRNR) type ADRNR
    raising
      CX_UDM_MESSAGE .
  class-methods GET_STORAGE_LOCATION_TXT
    importing
      !IV_WERKS type WERKS_D
      !IV_LGORT type LGORT_D
    returning
      value(RV_LGOBE) type LGOBE
    raising
      CX_UDM_MESSAGE .
  class-methods GET_TRACKING_ID_DL_HEADER
    importing
      !IR_LIKP type ref to DATA
    returning
      value(RV_TRACK_ID) type /SAPTRX/TRXID
    raising
      CX_UDM_MESSAGE .
  class-methods GET_TRACKING_ID_DL_ITEM
    importing
      !IR_LIPS type ref to DATA
    returning
      value(RV_TRACK_ID) type /SAPTRX/TRXID
    raising
      CX_UDM_MESSAGE .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mv_evtcnt TYPE /saptrx/evtcnt VALUE zif_gtt_mia_app_constants=>cs_start_evtcnt-delivery ##NO_TEXT.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_DL_TOOLS IMPLEMENTATION.


  METHOD convert_quantity_into_pounits.

    DATA(lv_matnr)  = CONV matnr( zcl_gtt_tools=>get_field_of_structure(
                                       ir_struct_data = ir_lips
                                       iv_field_name  = 'MATNR' ) ).
    DATA(lv_vrkme)  = CONV vrkme( zcl_gtt_tools=>get_field_of_structure(
                                       ir_struct_data = ir_lips
                                       iv_field_name  = 'VRKME' ) ).

    CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
      EXPORTING
        input                = iv_quantity_uom
        matnr                = lv_matnr
        meinh                = lv_vrkme
      IMPORTING
        output               = rv_quantity_pou
      EXCEPTIONS
        conversion_not_found = 1
        input_invalid        = 2
        material_not_found   = 3
        meinh_not_found      = 4
        meins_missing        = 5
        no_meinh             = 6
        output_invalid       = 7
        overflow             = 8
        OTHERS               = 9.
    IF sy-subrc <> 0.
      CLEAR: rv_quantity_pou.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_addres_info.

    DATA: lt_address   TYPE szadr_printform_table,
          ls_addr_comp TYPE szadr_addr1_complete.


    IF ev_address IS REQUESTED.
      CLEAR: ev_address.

      CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
        EXPORTING
          address_type                   = iv_addr_type
          address_number                 = iv_addr_numb
        IMPORTING
          address_printform_table        = lt_address
        EXCEPTIONS
          address_blocked                = 1
          person_blocked                 = 2
          contact_person_blocked         = 3
          addr_to_be_formated_is_blocked = 4
          OTHERS                         = 5.

      IF sy-subrc = 0.
        LOOP AT lt_address ASSIGNING FIELD-SYMBOL(<ls_address>).
          ev_address  = COND #( WHEN ev_address IS INITIAL
                                  THEN <ls_address>-address_line
                                  ELSE |{ ev_address }${ <ls_address>-address_line }| ).
        ENDLOOP.
      ELSE.
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ENDIF.

    IF ev_email IS REQUESTED OR ev_telephone IS REQUESTED.
      CLEAR: ev_email, ev_telephone.

      CALL FUNCTION 'ADDR_GET_COMPLETE'
        EXPORTING
          addrnumber              = iv_addr_numb
        IMPORTING
          addr1_complete          = ls_addr_comp
        EXCEPTIONS
          parameter_error         = 1
          address_not_exist       = 2
          internal_error          = 3
          wrong_access_to_archive = 4
          address_blocked         = 5
          OTHERS                  = 6.

      IF sy-subrc = 0.
        ev_email      = VALUE #( ls_addr_comp-adsmtp_tab[ 1 ]-adsmtp-smtp_addr OPTIONAL ).
        ev_telephone  = VALUE #( ls_addr_comp-adtel_tab[ 1 ]-adtel-tel_number OPTIONAL ).
      ELSE.
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_delivery_date.

    rv_date = zcl_gtt_tools=>get_local_timestamp(
                iv_date = zcl_gtt_tools=>get_field_of_structure(
                            ir_struct_data = ir_data
                            iv_field_name  = 'LFDAT' )
                iv_time = zcl_gtt_tools=>get_field_of_structure(
                            ir_struct_data = ir_data
                            iv_field_name  = 'LFUHR' ) ).

  ENDMETHOD.


  METHOD get_formated_dlv_item.

    rv_posnr = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_lips
      iv_field_name  = 'POSNR' ).

    rv_posnr   = |{ rv_posnr ALPHA = IN }|.

  ENDMETHOD.


  METHOD get_formated_dlv_number.

    rv_vbeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_likp
      iv_field_name  = 'VBELN' ).

    rv_vbeln   = |{ rv_vbeln ALPHA = OUT }|.

  ENDMETHOD.


  METHOD get_formated_po_item.
    DATA(lv_ebeln)  = CONV ebeln( zcl_gtt_tools=>get_field_of_structure(
                                    ir_struct_data = ir_lips
                                    iv_field_name  = 'VGBEL' ) ).

    DATA(lv_vgpos)  = CONV vgpos( zcl_gtt_tools=>get_field_of_structure(
                                    ir_struct_data = ir_lips
                                    iv_field_name  = 'VGPOS' ) ).

    rv_po_item  = |{ lv_ebeln ALPHA = OUT }{ lv_vgpos+1(5) }|.

    CONDENSE rv_po_item NO-GAPS.
  ENDMETHOD.


  METHOD get_door_description.

    "concatenate T300T-LNUMT '/' T30BT-ltort using SY-LANGU and LIPSVB-LGNUM & LIPSVB-LGTOR
    DATA: ls_t300t TYPE t300t,
          lv_ltort TYPE t30bt-ltort.

    CLEAR: rv_descr.

    CALL FUNCTION 'T300T_SINGLE_READ'
      EXPORTING
        t300t_spras = sy-langu
        t300t_lgnum = iv_lgnum
      IMPORTING
        wt300t      = ls_t300t
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.

    IF sy-subrc = 0.
      SELECT SINGLE ltort
        INTO lv_ltort
        FROM t30bt
        WHERE spras = sy-langu
          AND lgnum = iv_lgnum
          AND lgtor = iv_lgtor.

      IF sy-subrc = 0.
        rv_descr    = |{ ls_t300t-lnumt }/{ lv_ltort }|.
      ELSE.
        MESSAGE e057(00) WITH iv_lgnum iv_lgtor '' 'T30BT'
          INTO DATA(lv_dummy).
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_next_event_counter.

    ADD 1 TO mv_evtcnt.

    rv_evtcnt = mv_evtcnt.

  ENDMETHOD.


  METHOD get_plant_address_number.

    DATA: ls_t001w TYPE t001w.

    CALL FUNCTION 'WCB_T001W_SINGLE_READ'
      EXPORTING
        i_werks   = iv_werks
      IMPORTING
        e_t001w   = ls_t001w
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc = 0.
      ev_adrnr    = ls_t001w-adrnr.
    ELSE.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_storage_location_txt.

    DATA: ls_t001l    TYPE t001l.

    CALL FUNCTION 'T001L_SINGLE_READ'
      EXPORTING
        t001l_werks = iv_werks
        t001l_lgort = iv_lgort
      IMPORTING
        wt001l      = ls_t001l
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.

    IF sy-subrc = 0.
      rv_lgobe    = ls_t001l-lgobe.
    ELSE.
      MESSAGE e058(00) WITH iv_werks iv_lgort '' 'T001L'
        INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_tracking_id_dl_header.

    DATA: lv_vbeln TYPE lips-vbeln.

    lv_vbeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_likp
      iv_field_name  = 'VBELN' ).

    rv_track_id   = |{ lv_vbeln ALPHA = OUT }|.

  ENDMETHOD.


  METHOD get_tracking_id_dl_item.

    DATA: lv_vbeln TYPE lips-vbeln,
          lv_posnr TYPE lips-posnr.

    lv_vbeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_lips
      iv_field_name  = 'VBELN' ).

    lv_posnr = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_lips
      iv_field_name  = 'POSNR' ).

    rv_track_id   = |{ lv_vbeln ALPHA = OUT }{ lv_posnr ALPHA = IN }|.

    CONDENSE rv_track_id NO-GAPS.
  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_EVENT_REL_DL_HD definition
  public
  inheriting from ZCL_GTT_MIA_EVENT_REL_DL_MAIN
  create public .

public section.
protected section.

  methods GET_FIELD_NAME
    redefinition .
  methods GET_OBJECT_STATUS
    redefinition .
  methods GET_OLD_APPOBJID
    redefinition .
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_EVENT_REL_DL_HD IMPLEMENTATION.


  METHOD GET_FIELD_NAME.

    CASE iv_milestone.
      WHEN zif_gtt_ef_constants=>cs_milestone-dl_put_away.
        rv_field_name   = COND #( WHEN iv_internal = abap_true
                                    THEN 'KOSTA'
                                    ELSE 'KOSTK' ).
      WHEN zif_gtt_ef_constants=>cs_milestone-dl_packing.
        rv_field_name   = COND #( WHEN iv_internal = abap_true
                                    THEN 'PKSTA'
                                    ELSE 'PKSTK' ).
      WHEN zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt.
        rv_field_name   = COND #( WHEN iv_internal = abap_true
                                    THEN 'WBSTA'
                                    ELSE 'WBSTK' ).
      WHEN zif_gtt_ef_constants=>cs_milestone-dl_pod.
        rv_field_name   = 'PDSTK'.
      WHEN OTHERS.
        MESSAGE e009(zgtt) WITH iv_milestone INTO DATA(lv_dummy).
        zcl_gtt_tools=>throw_exception( ).
    ENDCASE.

    IF iv_internal = abap_true.
      rv_field_name   = |Z_{ rv_field_name }|.
    ENDIF.

  ENDMETHOD.


  METHOD GET_OBJECT_STATUS.

*    TYPES: tt_vbuk  TYPE STANDARD TABLE OF vbuk.

    DATA: lv_dummy  TYPE char100.

    FIELD-SYMBOLS: <lt_vbuk>  TYPE shp_vl10_vbuk_t,
                   <ls_vbuk>  TYPE vbukvb,
                   <lv_value> TYPE any.

    DATA(lv_fname)  = get_field_name( iv_milestone = iv_milestone ).

    DATA(lv_vbeln)  = zcl_gtt_tools=>get_field_of_structure(
                        ir_struct_data = ms_app_objects-maintabref
                        iv_field_name  = 'VBELN' ).

    DATA(lr_vbuk)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef    = zif_gtt_mia_app_constants=>cs_tabledef-dl_hdr_status_new ).

    CLEAR rv_value.

    ASSIGN lr_vbuk->* TO <lt_vbuk>.

    IF <lt_vbuk> IS ASSIGNED.
      READ TABLE <lt_vbuk> ASSIGNING <ls_vbuk>
        WITH KEY vbeln  = lv_vbeln.

      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_vbuk> TO <lv_value>.
        IF <lv_value> IS ASSIGNED.
          rv_value  = <lv_value>.
        ELSE.
          MESSAGE e001(zgtt) WITH lv_fname 'VBUP' INTO lv_dummy.
          zcl_gtt_tools=>throw_exception( ).
        ENDIF.
      ELSE.
        MESSAGE e005(zgtt)
          WITH 'VBUK' lv_vbeln
          INTO lv_dummy.
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt) WITH 'VBUK' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_OLD_APPOBJID.
    rv_appobjid  = zcl_gtt_tools=>get_field_of_structure(
                     ir_struct_data = ms_app_objects-maintabref
                     iv_field_name  = 'VBELN' ).
  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_EVENT_REL_DL_IT definition
  public
  inheriting from ZCL_GTT_MIA_EVENT_REL_DL_MAIN
  create public .

public section.
protected section.

  methods GET_FIELD_NAME
    redefinition .
  methods GET_OBJECT_STATUS
    redefinition .
  methods GET_OLD_APPOBJID
    redefinition .
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_EVENT_REL_DL_IT IMPLEMENTATION.


  METHOD GET_FIELD_NAME.

    CASE iv_milestone.
      WHEN zif_gtt_ef_constants=>cs_milestone-dl_put_away.
        rv_field_name   = 'KOSTA'.
      WHEN zif_gtt_ef_constants=>cs_milestone-dl_packing.
        rv_field_name   = 'PKSTA'.
      WHEN zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt.
        rv_field_name   = 'WBSTA'.
      WHEN zif_gtt_ef_constants=>cs_milestone-dl_pod.
        rv_field_name   = COND #( WHEN iv_internal = abap_true
                                    THEN 'PDSTK'
                                    ELSE 'PDSTA' ).
      WHEN OTHERS.
        MESSAGE e009(zgtt) WITH iv_milestone INTO DATA(lv_dummy).
        zcl_gtt_tools=>throw_exception( ).
    ENDCASE.

    IF iv_internal = abap_true.
      rv_field_name   = |Z_{ rv_field_name }|.
    ENDIF.

  ENDMETHOD.


  METHOD GET_OBJECT_STATUS.

    TYPES: tt_vbup  TYPE STANDARD TABLE OF vbupvb.

    DATA: lv_dummy  TYPE char100.

    FIELD-SYMBOLS: <lt_vbup>  TYPE tt_vbup,
                   <ls_vbup>  TYPE vbupvb,
                   <lv_value> TYPE any.

    DATA(lv_fname)  = get_field_name( iv_milestone = iv_milestone ).

    DATA(lv_vbeln)  = zcl_gtt_tools=>get_field_of_structure(
                        ir_struct_data = ms_app_objects-maintabref
                        iv_field_name  = 'VBELN' ).

    DATA(lv_posnr)  = zcl_gtt_tools=>get_field_of_structure(
                        ir_struct_data = ms_app_objects-maintabref
                        iv_field_name  = 'POSNR' ).

    DATA(lr_vbup)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef    = zif_gtt_mia_app_constants=>cs_tabledef-dl_itm_status_new ).

    CLEAR rv_value.

    ASSIGN lr_vbup->* TO <lt_vbup>.

    IF <lt_vbup> IS ASSIGNED.
      READ TABLE <lt_vbup> ASSIGNING <ls_vbup>
        WITH KEY vbeln  = lv_vbeln
                 posnr  = lv_posnr.

      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_vbup> TO <lv_value>.
        IF <lv_value> IS ASSIGNED.
          rv_value  = <lv_value>.
        ELSE.
          MESSAGE e001(zgtt) WITH lv_fname 'VBUP' INTO lv_dummy.
          zcl_gtt_tools=>throw_exception( ).
        ENDIF.
      ELSE.
        MESSAGE e005(zgtt)
          WITH 'VBUP' |{ lv_vbeln }-{ lv_posnr }|
          INTO lv_dummy.
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt) WITH 'VBUP' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_OLD_APPOBJID.
    DATA: lv_vbeln TYPE lips-vbeln,
          lv_posnr TYPE lips-posnr.

    lv_vbeln  = zcl_gtt_tools=>get_field_of_structure(
                  ir_struct_data = ms_app_objects-maintabref
                  iv_field_name  = 'VBELN' ).

    lv_posnr  = zcl_gtt_tools=>get_field_of_structure(
                  ir_struct_data = ms_app_objects-maintabref
                  iv_field_name  = 'POSNR' ).

    rv_appobjid   = |{ lv_vbeln ALPHA = IN }{ lv_posnr ALPHA = IN }|.
  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_EVENT_REL_DL_MAIN definition
  public
  abstract
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA optional .
  methods INITIATE
    raising
      CX_UDM_MESSAGE .
  methods IS_ENABLED
    importing
      !IV_MILESTONE type CLIKE
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  methods UPDATE .
  PROTECTED SECTION.
    DATA mo_ef_parameters TYPE REF TO zif_gtt_ef_parameters .
    DATA ms_app_objects TYPE trxas_appobj_ctab_wa .
    DATA ms_relevance TYPE zgtt_mia_ee_rel .

    METHODS get_field_name
      ABSTRACT
      IMPORTING
        !iv_milestone        TYPE clike
        !iv_internal         TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_field_name) TYPE fieldname
      RAISING
        cx_udm_message .
    METHODS get_object_status
      ABSTRACT
      IMPORTING
        !iv_milestone   TYPE clike
      RETURNING
        VALUE(rv_value) TYPE /saptrx/paramval200
      RAISING
        cx_udm_message .

    METHODS get_old_appobjid ABSTRACT
      RETURNING
        VALUE(rv_appobjid) TYPE /saptrx/aoid
      RAISING
        cx_udm_message.

  PRIVATE SECTION.

    METHODS set_relevance
      IMPORTING
        !iv_milestone TYPE clike
        !iv_relevance TYPE clike
      RAISING
        cx_udm_message .
    METHODS recalc_relevance
      IMPORTING
        !iv_milestone TYPE clike
      RAISING
        cx_udm_message .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_EVENT_REL_DL_MAIN IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    mo_ef_parameters  = io_ef_parameters.
    ms_app_objects    = is_app_objects.

  ENDMETHOD.


  METHOD INITIATE.

    " read stored statuses
    SELECT SINGLE *
      INTO @ms_relevance
      FROM zgtt_mia_ee_rel
      WHERE appobjid  = @ms_app_objects-appobjid.

    " read stored statuses using appobjid with leading zero
    IF sy-subrc <> 0.
      TRY.
          DATA(lv_old_appobjid) = get_old_appobjid( ).

          SELECT SINGLE *
            INTO @ms_relevance
            FROM zgtt_mia_ee_rel
            WHERE appobjid  = @lv_old_appobjid.

        CATCH cx_udm_message.
      ENDTRY.
    ENDIF.

    " initiate statuses with initial values
    IF sy-subrc <> 0.
      CLEAR: ms_relevance.
      ms_relevance-appobjid = ms_app_objects-appobjid.

      " recalculate statuses
      recalc_relevance( iv_milestone = zif_gtt_ef_constants=>cs_milestone-dl_put_away ).
      recalc_relevance( iv_milestone = zif_gtt_ef_constants=>cs_milestone-dl_packing ).
      recalc_relevance( iv_milestone = zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt ).
      recalc_relevance( iv_milestone = zif_gtt_ef_constants=>cs_milestone-dl_pod ).
    ENDIF.

  ENDMETHOD.


  METHOD IS_ENABLED.

    DATA(lv_field_name) = get_field_name(
      iv_milestone = iv_milestone
      iv_internal  = abap_true ).

    ASSIGN COMPONENT lv_field_name OF STRUCTURE ms_relevance
      TO FIELD-SYMBOL(<lv_value>).

    IF <lv_value> IS ASSIGNED.
      rv_result   = <lv_value>.
    ELSE.
      MESSAGE e001(zgtt) WITH lv_field_name '' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD RECALC_RELEVANCE.

    " retrieve delivery item status field value
    DATA(lv_status)     = get_object_status( iv_milestone = iv_milestone ).

    " calculate relevance
    "   for shipment POD planning :
    "     status is <> 'empty' -> relevant
    "   for other delivery item level planned events:
    "     initial value is 'A' -> relevant
    DATA(lv_relevance)  = COND abap_bool(
      WHEN lv_status = zif_gtt_mia_app_constants=>cs_delivery_stat-not_relevant
        THEN abap_false
      WHEN lv_status = zif_gtt_mia_app_constants=>cs_delivery_stat-not_processed OR
           iv_milestone = zif_gtt_ef_constants=>cs_milestone-dl_pod
        THEN abap_true
        ELSE abap_undefined
    ).

    " update flag value if it has appropriate value
    IF lv_relevance <> abap_undefined.
      set_relevance(
        EXPORTING
          iv_milestone = iv_milestone
          iv_relevance = lv_relevance ).
    ENDIF.

  ENDMETHOD.


  METHOD SET_RELEVANCE.

    DATA(lv_fname_int) = get_field_name(
      iv_milestone = iv_milestone
      iv_internal  = abap_true ).

    ASSIGN COMPONENT lv_fname_int OF STRUCTURE ms_relevance
      TO FIELD-SYMBOL(<lv_flag>).

    IF <lv_flag> IS ASSIGNED.
      <lv_flag>   = iv_relevance.
    ELSE.
      MESSAGE e001(zgtt) WITH lv_fname_int '' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD UPDATE.

    CALL FUNCTION 'ZGTT_MIA_UPDATE_RELEVANCE_TAB'
      IN UPDATE TASK
      EXPORTING
        is_relevance = ms_relevance.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_LE_SHIPMENT definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BADI_LE_SHIPMENT .
protected section.
private section.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_LE_SHIPMENT IMPLEMENTATION.


  method IF_EX_BADI_LE_SHIPMENT~AT_SAVE.
  endmethod.


  METHOD if_ex_badi_le_shipment~before_update.
    CALL FUNCTION 'ZGTT_MIA_CTP_SH_TO_DL'
      EXPORTING
        is_shipment = im_shipments_before_update.
  ENDMETHOD.


  method IF_EX_BADI_LE_SHIPMENT~IN_UPDATE.
  endmethod.
ENDCLASS.""",
    r"""CLASS zcl_gtt_mia_pe_filler_dlh DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_gtt_pe_filler .

    METHODS constructor
      IMPORTING
        !io_ef_parameters TYPE REF TO zif_gtt_ef_parameters
        !io_bo_reader     TYPE REF TO zif_gtt_tp_reader .
  PROTECTED SECTION.
private section.

  data MO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS .
  data MO_BO_READER type ref to ZIF_GTT_TP_READER .

  methods ADD_GR_EVENT_WITH_MATCK_KEY
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
      !IR_LIPS_DATA type ref to DATA
      !IO_RELEVANCE type ref to ZCL_GTT_MIA_EVENT_REL_DL_MAIN
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    raising
      CX_UDM_MESSAGE .
  methods ADD_GOODS_RECEIPT_EVENT
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
      !IO_RELEVANCE type ref to ZCL_GTT_MIA_EVENT_REL_DL_MAIN
      !IV_MILESTONENUM type /SAPTRX/SEQ_NUM
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    raising
      CX_UDM_MESSAGE .
  methods ADD_SHIPMENT_EVENTS
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    raising
      CX_UDM_MESSAGE .
  methods ADD_ITEM_COMPLETED_BY_FU_EVENT
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
      !IR_LIPS_DATA type ref to DATA
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    raising
      CX_UDM_MESSAGE .
  methods ADD_PLANNED_DELIVERY_EVENT
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
      !IO_RELEVANCE type ref to ZCL_GTT_MIA_EVENT_REL_DL_MAIN
      !IV_MILESTONENUM type /SAPTRX/SEQ_NUM
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    raising
      CX_UDM_MESSAGE .
  methods IS_TIME_OF_DELIVERY_CHANGED
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  methods IS_FU_RELEVANT
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
      !IR_LIPS_DATA type ref to DATA
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  methods ADD_PLANNED_SOURCE_ARRIVAL
    importing
      !IT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    raising
      CX_UDM_MESSAGE .
ENDCLASS.



CLASS ZCL_GTT_MIA_PE_FILLER_DLH IMPLEMENTATION.


  METHOD add_goods_receipt_event.

    IF io_relevance->is_enabled(
         iv_milestone   = zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt ) = abap_true.

      DATA(lv_tzonrc) = zcl_gtt_tools=>get_field_of_structure(
            ir_struct_data = is_app_objects-maintabref
            iv_field_name  = 'TZONRC' ).

      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt
        evt_exp_tzone     = COND #( WHEN lv_tzonrc IS NOT INITIAL
                                   THEN lv_tzonrc
                                   ELSE zcl_gtt_tools=>get_system_time_zone( ) )
        evt_exp_datetime  = zcl_gtt_mia_dl_tools=>get_delivery_date(
                              ir_data = is_app_objects-maintabref )
        milestonenum      = iv_milestonenum
      ) ).
    ENDIF.

  ENDMETHOD.


  METHOD add_gr_event_with_matck_key.
    DATA: lv_vbeln     TYPE vbeln_vl.
    FIELD-SYMBOLS: <lt_lips_fs> TYPE zif_gtt_mia_app_types=>tt_lipsvb,
                   <ls_lips>    TYPE lipsvb.

    ASSIGN ir_lips_data->* TO <lt_lips_fs>.

    lv_vbeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_objects-maintabref
      iv_field_name  = 'VBELN' ).
    IF <lt_lips_fs> IS ASSIGNED
      AND io_relevance->is_enabled(
         iv_milestone   = zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt ) = abap_true.
      LOOP AT <lt_lips_fs> ASSIGNING <ls_lips>
        WHERE vbeln = lv_vbeln AND updkz <> zif_gtt_ef_constants=>cs_change_mode-delete.
        IF zcl_gtt_tools=>is_appropriate_dl_item(
             ir_likp = is_app_objects-maintabref
             ir_lips = REF #( <ls_lips> ) ) = abap_true.

          DATA(lv_tzonrc) = zcl_gtt_tools=>get_field_of_structure(
            ir_struct_data = is_app_objects-maintabref
            iv_field_name  = 'TZONRC' ).


          ct_expeventdata = VALUE #( BASE ct_expeventdata (
                  appsys            = mo_ef_parameters->get_appsys(  )
                  appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
                  language          = sy-langu
                  appobjid          = is_app_objects-appobjid
                  milestone         = zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt
                  evt_exp_tzone     = COND #( WHEN lv_tzonrc IS NOT INITIAL
                                             THEN lv_tzonrc
                                             ELSE zcl_gtt_tools=>get_system_time_zone( ) )
                  evt_exp_datetime  = zcl_gtt_mia_dl_tools=>get_delivery_date(
                                        ir_data = is_app_objects-maintabref )
                  locid1            = zcl_gtt_tools=>get_pretty_location_id(
                                        iv_locid   = <ls_lips>-werks
                                        iv_loctype = zif_gtt_ef_constants=>cs_loc_types-plant )
                  loctype           = zif_gtt_ef_constants=>cs_loc_types-plant
                  locid2            = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item( ir_lips = REF #( <ls_lips> ) )
                  milestonenum      = zcl_gtt_tools=>get_next_sequence_id(
                                      it_expeventdata = ct_expeventdata )
                ) ).
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD add_item_completed_by_fu_event.

    DATA: lv_vbeln     TYPE vbeln_vl.
    FIELD-SYMBOLS: <lt_lips_fs> TYPE zif_gtt_mia_app_types=>tt_lipsvb,
                   <ls_lips>    TYPE lipsvb.

    ASSIGN ir_lips_data->* TO <lt_lips_fs>.

    lv_vbeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_objects-maintabref
      iv_field_name  = 'VBELN' ).
    IF <lt_lips_fs> IS ASSIGNED.
      LOOP AT <lt_lips_fs> ASSIGNING <ls_lips>
        WHERE vbeln = lv_vbeln AND updkz <> zif_gtt_ef_constants=>cs_change_mode-delete.

        IF zcl_gtt_tools=>is_appropriate_dl_item(
             ir_likp = is_app_objects-maintabref
             ir_lips = REF #( <ls_lips> ) ) = abap_true.

           DATA(lv_tzonrc) = zcl_gtt_tools=>get_field_of_structure(
            ir_struct_data = is_app_objects-maintabref
            iv_field_name  = 'TZONRC' ).

          ct_expeventdata = VALUE #( BASE ct_expeventdata (
                  appsys            = mo_ef_parameters->get_appsys(  )
                  appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
                  language          = sy-langu
                  appobjid          = is_app_objects-appobjid
                  milestone         = zif_gtt_ef_constants=>cs_milestone-dl_item_completed
                  evt_exp_tzone     = COND #( WHEN lv_tzonrc IS NOT INITIAL
                                         THEN lv_tzonrc
                                         ELSE zcl_gtt_tools=>get_system_time_zone( ) )
                  evt_exp_datetime  = zcl_gtt_mia_dl_tools=>get_delivery_date(
                                        ir_data = is_app_objects-maintabref )
                  locid2            = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item( ir_lips = REF #( <ls_lips> ) )
                  milestonenum      = zcl_gtt_tools=>get_next_sequence_id(
                                      it_expeventdata = ct_expeventdata )
                ) ).
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD add_planned_delivery_event.

    IF zcl_gtt_tools=>is_appropriate_dl_type( ir_likp = is_app_objects-maintabref ) = abap_true.
      DATA(lv_plant) = CONV werks_d( zcl_gtt_tools=>get_field_of_structure(
                            ir_struct_data = is_app_objects-maintabref
                            iv_field_name  = 'WERKS' ) ).
      DATA(lv_tzonrc) = zcl_gtt_tools=>get_field_of_structure(
            ir_struct_data = is_app_objects-maintabref
            iv_field_name  = 'TZONRC' ).
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
                appsys            = mo_ef_parameters->get_appsys(  )
                appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
                language          = sy-langu
                appobjid          = is_app_objects-appobjid
                milestone         = zif_gtt_ef_constants=>cs_milestone-dl_planned_delivery
                evt_exp_tzone     = COND #( WHEN lv_tzonrc IS NOT INITIAL
                                         THEN lv_tzonrc
                                         ELSE zcl_gtt_tools=>get_system_time_zone( ) )
                evt_exp_datetime  = zcl_gtt_mia_dl_tools=>get_delivery_date(
                                      ir_data = is_app_objects-maintabref )
                locid1            = zcl_gtt_tools=>get_pretty_location_id(
                                      iv_locid   = lv_plant
                                      iv_loctype = zif_gtt_ef_constants=>cs_loc_types-plant )
                loctype           = zif_gtt_ef_constants=>cs_loc_types-plant
                milestonenum      = iv_milestonenum
              ) ).
    ENDIF.

  ENDMETHOD.


  METHOD add_shipment_events.

    DATA: lt_expeventdata  TYPE zif_gtt_ef_types=>tt_expeventdata.

    DATA(lv_vbeln)            = CONV vbeln_vl( zcl_gtt_tools=>get_field_of_structure(
                                                 ir_struct_data = is_app_objects-maintabref
                                                 iv_field_name  = 'VBELN' ) ).

    DATA(lo_sh_stops_events) = zcl_gtt_mia_sh_stops_events=>get_instance_for_delivery(
      iv_vbeln         = lv_vbeln
      iv_appobjid      = is_app_objects-appobjid
      is_app_objects   = is_app_objects
      io_ef_parameters = mo_ef_parameters ).

    lo_sh_stops_events->get_planned_events(
      IMPORTING
        et_exp_event = lt_expeventdata ).

    ct_expeventdata   = VALUE #( BASE ct_expeventdata
                                 ( LINES OF lt_expeventdata ) ).

  ENDMETHOD.


  METHOD constructor.

    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.

  ENDMETHOD.


  METHOD is_fu_relevant.

    DATA: lv_is_fu_rel TYPE abap_bool VALUE abap_false,
          lv_vbeln     TYPE vbeln_vl,
          lt_lips      TYPE zif_gtt_mia_app_types=>tt_lipsvb.

    FIELD-SYMBOLS: <lt_lips_fs> TYPE zif_gtt_mia_app_types=>tt_lipsvb,
                   <ls_lips>    TYPE lipsvb.

    ASSIGN ir_lips_data->* TO <lt_lips_fs>.

    lv_vbeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_objects-maintabref
      iv_field_name  = 'VBELN' ).
    IF <lt_lips_fs> IS ASSIGNED.
      " collect NEW records with appropriate item type
      LOOP AT <lt_lips_fs> ASSIGNING <ls_lips>
        WHERE vbeln = lv_vbeln AND updkz <> zif_gtt_ef_constants=>cs_change_mode-delete.

        IF zcl_gtt_tools=>is_appropriate_dl_item(
             ir_likp = is_app_objects-maintabref
             ir_lips = REF #( <ls_lips> ) ) = abap_true.
          APPEND <ls_lips> TO lt_lips.
        ENDIF.
      ENDLOOP.
      IF lt_lips IS NOT INITIAL.
        lv_is_fu_rel = zcl_gtt_mia_tm_tools=>is_fu_relevant(
          it_lips = CORRESPONDING #( lt_lips ) ).
      ENDIF.
    ENDIF.
    rv_result = lv_is_fu_rel.

  ENDMETHOD.


  METHOD is_time_of_delivery_changed.

    TYPES: tt_likp    TYPE STANDARD TABLE OF likpvb.

    DATA: lv_vbeln     TYPE likp-vbeln,
          lv_lfuhr_new TYPE lfuhr,
          lv_lfuhr_old TYPE lfuhr.

    lv_lfuhr_new = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_objects-maintabref
      iv_field_name  = 'LFUHR' ).

    DATA(lr_likp) = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-dl_header_old ).

    FIELD-SYMBOLS: <lt_likp> TYPE tt_likp.

    IF lr_likp IS BOUND.
      ASSIGN lr_likp->* TO <lt_likp>.

      IF <lt_likp> IS ASSIGNED.
        lv_vbeln = zcl_gtt_tools=>get_field_of_structure(
          ir_struct_data = is_app_objects-maintabref
          iv_field_name  = 'VBELN' ).

        READ TABLE <lt_likp> ASSIGNING FIELD-SYMBOL(<ls_likp>)
          WITH KEY vbeln = lv_vbeln.

        lv_lfuhr_old  = COND #( WHEN sy-subrc = 0 THEN <ls_likp>-lfuhr ).
      ENDIF.
    ENDIF.

    rv_result   = boolc( lv_lfuhr_new <> lv_lfuhr_old ).

  ENDMETHOD.


  METHOD zif_gtt_pe_filler~check_relevance.

    TYPES: tt_milestones    TYPE STANDARD TABLE OF /saptrx/appl_event_tag
                              WITH EMPTY KEY.

    rv_result = zif_gtt_ef_constants=>cs_condition-false.

    IF zcl_gtt_tools=>is_appropriate_dl_type( ir_likp = is_app_objects-maintabref ) = abap_true.

      IF is_time_of_delivery_changed( is_app_objects = is_app_objects ) = abap_true.
        rv_result = zif_gtt_ef_constants=>cs_condition-true.

      ELSE.
        DATA(lo_relevance_old)  = NEW zcl_gtt_mia_event_rel_dl_hd(
                                        io_ef_parameters = mo_ef_parameters
                                        is_app_objects   = VALUE #(
                                                             appobjid   = is_app_objects-appobjid ) ).

        DATA(lo_relevance_new) = NEW zcl_gtt_mia_event_rel_dl_hd(
          io_ef_parameters = mo_ef_parameters
          is_app_objects   = is_app_objects ).

        DATA(lv_milestone)      = zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt.

        rv_result = boolc( lo_relevance_old->is_enabled( iv_milestone = lv_milestone ) <>
                           lo_relevance_new->is_enabled( iv_milestone = lv_milestone ) ).

        rv_result = COND #( WHEN rv_result = abap_true
                              THEN zif_gtt_ef_constants=>cs_condition-true
                              ELSE zif_gtt_ef_constants=>cs_condition-false ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pe_filler~get_planed_events.

    DATA: lr_lips_data TYPE REF TO data.

    DATA(lo_relevance) = NEW zcl_gtt_mia_event_rel_dl_hd(
      io_ef_parameters = mo_ef_parameters
      is_app_objects   = is_app_objects ).

    " initiate relevance flags
    lo_relevance->initiate( ).

    " store calculated relevance flags
    lo_relevance->update( ).

    lr_lips_data = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-dl_item_new ).

    add_gr_event_with_matck_key(
      EXPORTING
        is_app_objects  = is_app_objects
        ir_lips_data    = lr_lips_data
        io_relevance    = lo_relevance
      CHANGING
        ct_expeventdata = ct_expeventdata
    ).

    add_planned_delivery_event(
      EXPORTING
        is_app_objects  = is_app_objects
        io_relevance    = lo_relevance
        iv_milestonenum = zcl_gtt_tools=>get_next_sequence_id(
                              it_expeventdata = ct_expeventdata )
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    IF is_fu_relevant( is_app_objects = is_app_objects
                       ir_lips_data   = lr_lips_data ) = abap_true.

      add_item_completed_by_fu_event(
        EXPORTING
          is_app_objects  = is_app_objects
          ir_lips_data    = lr_lips_data
        CHANGING
          ct_expeventdata = ct_expeventdata
      ).

    ELSE.

      add_shipment_events(
        EXPORTING
          is_app_objects  = is_app_objects
        CHANGING
          ct_expeventdata = ct_expeventdata ).

      add_goods_receipt_event(
        EXPORTING
          is_app_objects  = is_app_objects
          io_relevance    = lo_relevance
          iv_milestonenum = zcl_gtt_tools=>get_next_sequence_id(
                              it_expeventdata = ct_expeventdata )
        CHANGING
          ct_expeventdata = ct_expeventdata ).

      add_planned_source_arrival(
        EXPORTING
          it_expeventdata = ct_expeventdata
        CHANGING
          ct_expeventdata = ct_expeventdata ).

    ENDIF.

  ENDMETHOD.


  METHOD add_planned_source_arrival.

    DATA:
      lv_length       TYPE i,
      lv_seq          TYPE char04,
      lv_tknum        TYPE vttk-tknum,
      lv_ltl_flag     TYPE flag,
      ls_eventdata    TYPE zif_gtt_ef_types=>ts_expeventdata,
      lt_expeventdata TYPE zif_gtt_ef_types=>tt_expeventdata,
      lv_exp_datetime TYPE /saptrx/event_exp_datetime.

    lt_expeventdata = it_expeventdata.

    LOOP AT lt_expeventdata INTO DATA(ls_expeventdata)
      WHERE milestone = zif_gtt_ef_constants=>cs_milestone-departure.

      lv_length = strlen( ls_expeventdata-locid2 ) - 4.
      IF lv_length >= 0.
        lv_tknum = ls_expeventdata-locid2+0(lv_length).
        lv_tknum = |{ lv_tknum ALPHA = IN }|.
        lv_seq = ls_expeventdata-locid2+lv_length(4).

        zcl_gtt_tools=>check_ltl_shipment(
          EXPORTING
            iv_tknum    = lv_tknum
          IMPORTING
            ev_ltl_flag = lv_ltl_flag
            es_vttk     = DATA(ls_vttk) ).

        IF lv_ltl_flag = abap_true AND lv_seq = '0001'. "only for source location of the shipment
          lv_exp_datetime = zcl_gtt_tools=>get_local_timestamp(
            iv_date = ls_vttk-dpreg     "Planned date of check-in
            iv_time = ls_vttk-upreg ).  "Planned check-in time

          ls_eventdata = ls_expeventdata.
          ls_eventdata-milestonenum = 0.
          ls_eventdata-milestone = zif_gtt_ef_constants=>cs_milestone-arriv_dest.
          ls_eventdata-evt_exp_datetime = lv_exp_datetime.
          ls_eventdata-evt_exp_tzone = zcl_gtt_tools=>get_system_time_zone( ).
          APPEND ls_eventdata TO ct_expeventdata.
        ENDIF.
      ENDIF.
      CLEAR:
        lv_ltl_flag,
        ls_eventdata,
        lv_exp_datetime.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_gtt_mia_pe_filler_dli DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_gtt_pe_filler .

    METHODS constructor
      IMPORTING
        !io_ef_parameters TYPE REF TO zif_gtt_ef_parameters
        !io_bo_reader     TYPE REF TO zif_gtt_tp_reader .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_ef_parameters TYPE REF TO zif_gtt_ef_parameters .
    DATA mo_bo_reader TYPE REF TO zif_gtt_tp_reader .

    METHODS add_goods_receipt_event
      IMPORTING
        !is_app_objects  TYPE trxas_appobj_ctab_wa
        !io_relevance    TYPE REF TO zcl_gtt_mia_event_rel_dl_main
        !iv_milestonenum TYPE /saptrx/seq_num
      CHANGING
        !ct_expeventdata TYPE zif_gtt_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message .
    METHODS add_packing_event
      IMPORTING
        !is_app_objects  TYPE trxas_appobj_ctab_wa
        !io_relevance    TYPE REF TO zcl_gtt_mia_event_rel_dl_main
        !iv_milestonenum TYPE /saptrx/seq_num
      CHANGING
        !ct_expeventdata TYPE zif_gtt_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message .
    METHODS add_put_away_event
      IMPORTING
        !is_app_objects  TYPE trxas_appobj_ctab_wa
        !io_relevance    TYPE REF TO zcl_gtt_mia_event_rel_dl_main
        !iv_milestonenum TYPE /saptrx/seq_num
      CHANGING
        !ct_expeventdata TYPE zif_gtt_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message .
    METHODS is_time_of_delivery_changed
      IMPORTING
        !is_app_objects  TYPE trxas_appobj_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message .
    METHODS get_gr_event_matchkey
      IMPORTING
        !ir_struct_data    TYPE REF TO data
      RETURNING
        VALUE(rv_matchkey) TYPE char50
      RAISING
        cx_udm_message .
    METHODS is_fu_relevant
      IMPORTING
        !ir_struct_data  TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message .
    METHODS add_gr_event_wo_matchkey
      IMPORTING
        !is_app_objects  TYPE trxas_appobj_ctab_wa
        !io_relevance    TYPE REF TO zcl_gtt_mia_event_rel_dl_main
        !iv_milestonenum TYPE /saptrx/seq_num
      CHANGING
        !ct_expeventdata TYPE zif_gtt_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message .
    METHODS add_fu_completed_event
      IMPORTING
        !is_app_objects  TYPE trxas_appobj_ctab_wa
        !io_relevance    TYPE REF TO zcl_gtt_mia_event_rel_dl_main
      CHANGING
        !ct_expeventdata TYPE zif_gtt_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_PE_FILLER_DLI IMPLEMENTATION.


  METHOD add_fu_completed_event.

    DATA: lr_struct_data TYPE REF TO data,
          lt_fu_item     TYPE /scmtms/t_tor_item_tr_k.
    DATA: lt_torid TYPE zif_gtt_mia_ctp_types=>tt_fu_id,
          ls_torid TYPE zif_gtt_mia_ctp_types=>ts_fu_id.

    lr_struct_data = is_app_objects-maintabref.
    zcl_gtt_mia_tm_tools=>get_tor_items_for_dlv_items(
      EXPORTING
        it_lips    = VALUE #( ( vbeln = zcl_gtt_tools=>get_field_of_structure(
                                  ir_struct_data = lr_struct_data
                                  iv_field_name  = 'VBELN' )
                                posnr = zcl_gtt_tools=>get_field_of_structure(
                                  ir_struct_data = lr_struct_data
                                  iv_field_name  = 'POSNR' ) ) )
        iv_tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit
      IMPORTING
        et_fu_item = lt_fu_item ).

    LOOP AT lt_fu_item ASSIGNING FIELD-SYMBOL(<ls_fu_item>).
      zcl_gtt_mia_tm_tools=>get_tor_root(
        EXPORTING
          iv_key = <ls_fu_item>-parent_key
        IMPORTING
          es_tor = DATA(ls_tor_data)
      ).
      IF ls_tor_data-lifecycle = /scmtms/if_tor_status_c=>sc_root-lifecycle-v_canceled.
        CONTINUE.
      ENDIF.
      lt_torid  = VALUE #( BASE lt_torid (
         tor_id = ls_tor_data-tor_id
      ) ).

    ENDLOOP.

    SORT lt_torid BY tor_id.
    DELETE ADJACENT DUPLICATES FROM lt_torid  COMPARING tor_id.

    IF lt_torid IS NOT INITIAL.
      LOOP AT lt_torid INTO ls_torid.
        DATA(lv_tzonrc) = zcl_gtt_tools=>get_field_of_structure(
          ir_struct_data = is_app_objects-mastertabref
          iv_field_name  = 'TZONRC' ).

        ct_expeventdata = VALUE #( BASE ct_expeventdata (
               appsys            = mo_ef_parameters->get_appsys(  )
               appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
               language          = sy-langu
               appobjid          = is_app_objects-appobjid
               milestone         = zif_gtt_ef_constants=>cs_milestone-fu_completed
               evt_exp_tzone     = COND #( WHEN lv_tzonrc IS NOT INITIAL
                                   THEN lv_tzonrc
                                   ELSE zcl_gtt_tools=>get_system_time_zone( ) )
               evt_exp_datetime  = zcl_gtt_mia_dl_tools=>get_delivery_date(
                                     ir_data = is_app_objects-mastertabref )
               locid2            = zcl_gtt_mia_tm_tools=>get_formated_tor_id( ir_data = REF #( ls_torid )  )
               milestonenum      = zcl_gtt_tools=>get_next_sequence_id(
                                   it_expeventdata = ct_expeventdata )
             ) ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD add_goods_receipt_event.
    DATA: lv_werks  TYPE werks_d.

    IF io_relevance->is_enabled(
         iv_milestone   = zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt ) = abap_true.

      lv_werks = zcl_gtt_tools=>get_field_of_structure(
        ir_struct_data = is_app_objects-maintabref
        iv_field_name  = 'WERKS' ).

      DATA(lv_tzonrc) = zcl_gtt_tools=>get_field_of_structure(
            ir_struct_data = is_app_objects-mastertabref
            iv_field_name  = 'TZONRC' ).
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt
        evt_exp_tzone     = COND #( WHEN lv_tzonrc IS NOT INITIAL
                                   THEN lv_tzonrc
                                   ELSE zcl_gtt_tools=>get_system_time_zone( ) )
        evt_exp_datetime  = zcl_gtt_mia_dl_tools=>get_delivery_date(
                              ir_data = is_app_objects-mastertabref )
        locid1            = zcl_gtt_tools=>get_pretty_location_id(
                              iv_locid   = lv_werks
                              iv_loctype = zif_gtt_ef_constants=>cs_loc_types-plant )
        locid2            = get_gr_event_matchkey( ir_struct_data = is_app_objects-maintabref )
        loctype           = zif_gtt_ef_constants=>cs_loc_types-plant
        milestonenum      = iv_milestonenum
      ) ).
    ENDIF.

  ENDMETHOD.


  METHOD add_gr_event_wo_matchkey.

    IF io_relevance->is_enabled(
             iv_milestone   = zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt ) = abap_true.

      DATA(lv_tzonrc) = zcl_gtt_tools=>get_field_of_structure(
        ir_struct_data = is_app_objects-mastertabref
        iv_field_name  = 'TZONRC' ).
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
              appsys            = mo_ef_parameters->get_appsys(  )
              appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
              language          = sy-langu
              appobjid          = is_app_objects-appobjid
              milestone         = zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt
              evt_exp_tzone     = COND #( WHEN lv_tzonrc IS NOT INITIAL
                                   THEN lv_tzonrc
                                   ELSE zcl_gtt_tools=>get_system_time_zone( ) )
              evt_exp_datetime  = zcl_gtt_mia_dl_tools=>get_delivery_date(
                                    ir_data = is_app_objects-mastertabref )
              milestonenum      = iv_milestonenum
            ) ).
    ENDIF.

  ENDMETHOD.


  METHOD add_packing_event.
    DATA: lv_werks  TYPE werks_d.

    IF io_relevance->is_enabled(
         iv_milestone   = zif_gtt_ef_constants=>cs_milestone-dl_packing ) = abap_true.

      lv_werks = zcl_gtt_tools=>get_field_of_structure(
        ir_struct_data = is_app_objects-maintabref
        iv_field_name  = 'WERKS' ).

      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_ef_constants=>cs_milestone-dl_packing
        evt_exp_tzone     = zcl_gtt_tools=>get_system_time_zone( )
        locid1            = zcl_gtt_tools=>get_pretty_location_id(
                              iv_locid   = lv_werks
                              iv_loctype = zif_gtt_ef_constants=>cs_loc_types-plant )
        loctype           = zif_gtt_ef_constants=>cs_loc_types-plant
        milestonenum      = iv_milestonenum
      ) ).
    ENDIF.

  ENDMETHOD.


  METHOD add_put_away_event.
    DATA: lv_werks  TYPE werks_d.

    IF io_relevance->is_enabled(
         iv_milestone   = zif_gtt_ef_constants=>cs_milestone-dl_put_away ) = abap_true.

      lv_werks = zcl_gtt_tools=>get_field_of_structure(
        ir_struct_data = is_app_objects-maintabref
        iv_field_name  = 'WERKS' ).
      DATA(lv_tzonrc) = zcl_gtt_tools=>get_field_of_structure(
        ir_struct_data = is_app_objects-mastertabref
        iv_field_name  = 'TZONRC' ).

      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_ef_constants=>cs_milestone-dl_put_away
        evt_exp_tzone     = COND #( WHEN lv_tzonrc IS NOT INITIAL
                                   THEN lv_tzonrc
                                   ELSE zcl_gtt_tools=>get_system_time_zone( ) )
        evt_exp_datetime  = zcl_gtt_mia_dl_tools=>get_delivery_date(
                              ir_data = is_app_objects-mastertabref )
        locid1            = zcl_gtt_tools=>get_pretty_location_id(
                              iv_locid   = lv_werks
                              iv_loctype = zif_gtt_ef_constants=>cs_loc_types-plant )
        loctype           = zif_gtt_ef_constants=>cs_loc_types-plant
        milestonenum      = iv_milestonenum
      ) ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.

  ENDMETHOD.


  METHOD get_gr_event_matchkey.

    DATA(lv_vbeln) = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_struct_data
      iv_field_name  = 'VBELN' ).

    DATA(lv_posnr) = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_struct_data
      iv_field_name  = 'POSNR' ).

    rv_matchkey = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
      ir_lips = NEW lips( vbeln = lv_vbeln posnr = lv_posnr ) ).
  ENDMETHOD.


  METHOD is_fu_relevant.

    DATA: lt_lips TYPE zif_gtt_mia_app_types=>tt_lipsvb_key.
    rv_result = zcl_gtt_mia_tm_tools=>is_fu_relevant(
      it_lips = VALUE #( BASE lt_lips (
        vbeln             = zcl_gtt_tools=>get_field_of_structure(
                                  ir_struct_data = ir_struct_data
                                  iv_field_name  = 'VBELN' )
        posnr             = zcl_gtt_tools=>get_field_of_structure(
                                  ir_struct_data = ir_struct_data
                                  iv_field_name  = 'POSNR' )
      ) ) ).

  ENDMETHOD.


  METHOD is_time_of_delivery_changed.

    TYPES: tt_likp    TYPE STANDARD TABLE OF likpvb.

    DATA: lv_vbeln     TYPE likp-vbeln,
          lv_lfuhr_new TYPE lfuhr,
          lv_lfuhr_old TYPE lfuhr.

    lv_lfuhr_new = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_objects-mastertabref
      iv_field_name  = 'LFUHR' ).

    DATA(lr_likp) = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-dl_header_old ).

    FIELD-SYMBOLS: <lt_likp> TYPE tt_likp.

    IF lr_likp IS BOUND.
      ASSIGN lr_likp->* TO <lt_likp>.

      IF <lt_likp> IS ASSIGNED.
        lv_vbeln = zcl_gtt_tools=>get_field_of_structure(
          ir_struct_data = is_app_objects-mastertabref
          iv_field_name  = 'VBELN' ).

        READ TABLE <lt_likp> ASSIGNING FIELD-SYMBOL(<ls_likp>)
          WITH KEY vbeln = lv_vbeln.

        lv_lfuhr_old  = COND #( WHEN sy-subrc = 0 THEN <ls_likp>-lfuhr ).
      ENDIF.
    ENDIF.

    rv_result   = boolc( lv_lfuhr_new <> lv_lfuhr_old ).

  ENDMETHOD.


  METHOD zif_gtt_pe_filler~check_relevance.

    TYPES: tt_milestones    TYPE STANDARD TABLE OF /saptrx/appl_event_tag
                              WITH EMPTY KEY.

    rv_result = zif_gtt_ef_constants=>cs_condition-false.

    IF zcl_gtt_tools=>is_appropriate_dl_type( ir_likp = is_app_objects-mastertabref ) = abap_true AND
       zcl_gtt_tools=>is_appropriate_dl_item( ir_likp = is_app_objects-mastertabref ir_lips = is_app_objects-maintabref ) = abap_true.

      IF is_time_of_delivery_changed( is_app_objects = is_app_objects ) = abap_true.
        rv_result = zif_gtt_ef_constants=>cs_condition-true.

      ELSE.
        DATA(lo_relevance_old) = NEW zcl_gtt_mia_event_rel_dl_it(
          io_ef_parameters = mo_ef_parameters ).

        DATA(lo_relevance_new) = NEW zcl_gtt_mia_event_rel_dl_it(
          io_ef_parameters = mo_ef_parameters
          is_app_objects   = is_app_objects ).

        DATA(lt_milestones)     = VALUE tt_milestones(
          ( zif_gtt_ef_constants=>cs_milestone-dl_goods_receipt )
          ( zif_gtt_ef_constants=>cs_milestone-dl_packing )
          ( zif_gtt_ef_constants=>cs_milestone-dl_put_away )
        ).

        rv_result = zif_gtt_ef_constants=>cs_condition-false.

        LOOP AT lt_milestones ASSIGNING FIELD-SYMBOL(<lv_milestone>).
          IF lo_relevance_old->is_enabled( iv_milestone = <lv_milestone> ) <>
               lo_relevance_new->is_enabled( iv_milestone = <lv_milestone> ).

            rv_result = zif_gtt_ef_constants=>cs_condition-true.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pe_filler~get_planed_events.

    DATA(lo_relevance) = NEW zcl_gtt_mia_event_rel_dl_it(
      io_ef_parameters = mo_ef_parameters
      is_app_objects   = is_app_objects ).

    " initiate relevance flags
    lo_relevance->initiate( ).

    " store calculated relevance flags
    lo_relevance->update( ).

    add_put_away_event(
      EXPORTING
        is_app_objects  = is_app_objects
        io_relevance    = lo_relevance
        iv_milestonenum = zcl_gtt_tools=>get_next_sequence_id(
                            it_expeventdata = ct_expeventdata )
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    add_packing_event(
      EXPORTING
        is_app_objects  = is_app_objects
        io_relevance    = lo_relevance
        iv_milestonenum = zcl_gtt_tools=>get_next_sequence_id(
                            it_expeventdata = ct_expeventdata )
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    add_goods_receipt_event(
      EXPORTING
        is_app_objects  = is_app_objects
        io_relevance    = lo_relevance
        iv_milestonenum = zcl_gtt_tools=>get_next_sequence_id(
                            it_expeventdata = ct_expeventdata )
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    IF is_fu_relevant( ir_struct_data = is_app_objects-maintabref ) = abap_true.
      add_fu_completed_event(
        EXPORTING
          is_app_objects  = is_app_objects
          io_relevance    = lo_relevance
        CHANGING
          ct_expeventdata = ct_expeventdata ).
    ELSE.
      add_gr_event_wo_matchkey(
        EXPORTING
          is_app_objects  = is_app_objects
          io_relevance    = lo_relevance
          iv_milestonenum = zcl_gtt_tools=>get_next_sequence_id(
                              it_expeventdata = ct_expeventdata )
        CHANGING
          ct_expeventdata = ct_expeventdata ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_PE_FILLER_SHH definition
  public
  create public .

public section.

  interfaces ZIF_GTT_PE_FILLER .

  methods CONSTRUCTOR
    importing
      !IO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS
      !IO_BO_READER type ref to ZIF_GTT_TP_READER
    raising
      CX_UDM_MESSAGE .
  PROTECTED SECTION.
private section.

  types:
    tt_vbeln    TYPE RANGE OF lips-vbeln .
  types:
    tt_werks    TYPE RANGE OF lips-werks .
  types:
    tt_appobjid TYPE RANGE OF /saptrx/aoid .

  data MO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS .
  data MO_BO_READER type ref to ZIF_GTT_TP_READER .
  data MO_SH_DATA_OLD type ref to ZCL_GTT_MIA_SH_DATA_OLD .

  methods ADD_SHIPMENT_EVENTS
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
      !CT_MEASRMNTDATA type ZIF_GTT_EF_TYPES=>TT_MEASRMNTDATA
      !CT_INFODATA type ZIF_GTT_EF_TYPES=>TT_INFODATA
    raising
      CX_UDM_MESSAGE .
  methods ADD_STOPS_EVENTS
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
      !IV_MILESTONENUM type /SAPTRX/SEQ_NUM
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
      !CT_MEASRMNTDATA type ZIF_GTT_EF_TYPES=>TT_MEASRMNTDATA
      !CT_INFODATA type ZIF_GTT_EF_TYPES=>TT_INFODATA
    raising
      CX_UDM_MESSAGE .
  methods GET_SHIPPMENT_HEADER
    importing
      !IS_APP_OBJECT type TRXAS_APPOBJ_CTAB_WA
      !IR_VTTK type ref to DATA
    returning
      value(RR_VTTK) type ref to DATA
    raising
      CX_UDM_MESSAGE .
  methods IS_POD_RELEVANT
    importing
      !IS_STOPS type ZIF_GTT_MIA_APP_TYPES=>TS_STOPS
      !IT_VTTP type VTTPVB_TAB
      !IT_VTSP type VTSPVB_TAB
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  methods IS_STOP_CHANGED
    importing
      !IS_APP_OBJECT type TRXAS_APPOBJ_CTAB_WA
      !IT_FIELDS type ZIF_GTT_EF_TYPES=>TT_FIELD_NAME
    returning
      value(RV_RESULT) type ZIF_GTT_EF_TYPES=>TV_CONDITION
    raising
      CX_UDM_MESSAGE .
  methods GET_CORRESPONDING_DLV_ITEMS
    importing
      !IT_VBELN type TT_VBELN
      !IT_WERKS type TT_WERKS
    exporting
      !ET_APPOBJID type TT_APPOBJID
    raising
      CX_UDM_MESSAGE .
  methods GET_HEADER_FIELDS
    exporting
      !ET_FIELDS type ZIF_GTT_EF_TYPES=>TT_FIELD_NAME .
  methods GET_STOP_FIELDS
    exporting
      !ET_FIELDS type ZIF_GTT_EF_TYPES=>TT_FIELD_NAME .
  methods CHK_POD_RELEVANT_FOR_ODLV
    importing
      !IS_STOPS type ZIF_GTT_MIA_APP_TYPES=>TS_STOPS
      !IT_VTTP type VTTPVB_TAB
      !IT_VTSP type VTSPVB_TAB
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  methods ADD_PLANNED_SOURCE_ARRIVAL
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    raising
      CX_UDM_MESSAGE .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_PE_FILLER_SHH IMPLEMENTATION.


  METHOD ADD_SHIPMENT_EVENTS.

    FIELD-SYMBOLS: <ls_vttk>  TYPE vttkvb.

    DATA: lv_local_ts TYPE timestamp.

    ASSIGN is_app_objects-maintabref->* TO <ls_vttk>.

    IF <ls_vttk> IS ASSIGNED.
      " CHECK IN

      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_ef_constants=>cs_milestone-sh_check_in
        evt_exp_datetime  = zcl_gtt_tools=>get_local_timestamp(
                              iv_date = <ls_vttk>-dpreg
                              iv_time = <ls_vttk>-upreg )
        evt_exp_tzone     = zcl_gtt_tools=>get_system_time_zone( )
        milestonenum      = 1
      ) ).

      " LOAD START
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_ef_constants=>cs_milestone-sh_load_start
        evt_exp_datetime  = zcl_gtt_tools=>get_local_timestamp(
                              iv_date = <ls_vttk>-dplbg
                              iv_time = <ls_vttk>-uplbg )
        evt_exp_tzone     = zcl_gtt_tools=>get_system_time_zone( )
        milestonenum      = 2
      ) ).

      " LOAD END
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_ef_constants=>cs_milestone-sh_load_end
        evt_exp_datetime  = zcl_gtt_tools=>get_local_timestamp(
                              iv_date = <ls_vttk>-dplen
                              iv_time = <ls_vttk>-uplen )
        evt_exp_tzone     = zcl_gtt_tools=>get_system_time_zone( )
        milestonenum      = 3
      ) ).
    ENDIF.

  ENDMETHOD.


  METHOD ADD_STOPS_EVENTS.

    DATA(lv_tknum)        = CONV tknum( zcl_gtt_tools=>get_field_of_structure(
                                          ir_struct_data = is_app_objects-maintabref
                                          iv_field_name  = 'TKNUM' ) ).
    DATA(lv_abfer)        = CONV abfer( zcl_gtt_tools=>get_field_of_structure(
                                      ir_struct_data = is_app_objects-maintabref
                                      iv_field_name  = 'ABFER' ) ).

    DATA(lv_milestonenum) = iv_milestonenum.
    DATA: lt_stops    TYPE zif_gtt_mia_app_types=>tt_stops.

    FIELD-SYMBOLS: <lt_vttp> TYPE vttpvb_tab,
                   <lt_vtts> TYPE vttsvb_tab,
                   <lt_vtsp> TYPE vtspvb_tab.

    DATA(lr_vttp) = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_item_new ).
    DATA(lr_vtts) = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_stage_new ).
    DATA(lr_vtsp) = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_item_stage_new ).

    ASSIGN lr_vtts->* TO <lt_vtts>.
    ASSIGN lr_vttp->* TO <lt_vttp>.
    ASSIGN lr_vtsp->* TO <lt_vtsp>.

    IF <lt_vtts> IS ASSIGNED AND
       <lt_vtsp> IS ASSIGNED AND
       <lt_vttp> IS ASSIGNED.

      zcl_gtt_mia_sh_tools=>get_stops_from_shipment(
        EXPORTING
          iv_tknum = lv_tknum
          it_vtts  = <lt_vtts>
          it_vtsp  = <lt_vtsp>
          it_vttp  = <lt_vttp>
        IMPORTING
          et_stops = lt_stops ).

      " important for correct sequence number calculation
      SORT lt_stops BY stopid loccat.

      LOOP AT lt_stops ASSIGNING FIELD-SYMBOL(<ls_stops>).
        " DEPARTURE / ARRIVAL
        ct_expeventdata = VALUE #( BASE ct_expeventdata (
          appsys            = mo_ef_parameters->get_appsys(  )
          appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
          language          = sy-langu
          appobjid          = is_app_objects-appobjid
          milestone         = COND #( WHEN <ls_stops>-loccat = zif_gtt_mia_app_constants=>cs_loccat-departure
                                        THEN zif_gtt_ef_constants=>cs_milestone-sh_departure
                                        ELSE zif_gtt_ef_constants=>cs_milestone-sh_arrival )
          evt_exp_datetime  = <ls_stops>-pln_evt_datetime
          evt_exp_tzone     = <ls_stops>-pln_evt_timezone
          locid2            = <ls_stops>-stopid_txt
          loctype           = <ls_stops>-loctype
          locid1            = zcl_gtt_tools=>get_pretty_location_id(
                                iv_locid   = <ls_stops>-locid
                                iv_loctype = <ls_stops>-loctype )
          milestonenum      = lv_milestonenum
        ) ).

        ADD 1 TO lv_milestonenum.

        IF ( lv_abfer = zif_gtt_mia_app_constants=>cs_abfer-empty_inb_ship OR
             lv_abfer = zif_gtt_mia_app_constants=>cs_abfer-loaded_inb_ship ).

          IF is_pod_relevant( is_stops = <ls_stops>
                              it_vttp  = <lt_vttp>
                              it_vtsp  = <lt_vtsp> ) = abap_true.
            " POD
            ct_expeventdata = VALUE #( BASE ct_expeventdata (
              appsys            = mo_ef_parameters->get_appsys(  )
              appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
              language          = sy-langu
              appobjid          = is_app_objects-appobjid
              milestone         = zif_gtt_ef_constants=>cs_milestone-sh_pod
              evt_exp_datetime  = <ls_stops>-pln_evt_datetime
              evt_exp_tzone     = <ls_stops>-pln_evt_timezone
              locid2            = <ls_stops>-stopid_txt
              loctype           = <ls_stops>-loctype
              locid1            = zcl_gtt_tools=>get_pretty_location_id(
                                    iv_locid   = <ls_stops>-locid
                                    iv_loctype = <ls_stops>-loctype )
              milestonenum      = lv_milestonenum
            ) ).

            ADD 1 TO lv_milestonenum.
          ENDIF.

        ELSEIF ( lv_abfer = zif_gtt_mia_app_constants=>cs_abfer-empty_outb_ship OR
                 lv_abfer = zif_gtt_mia_app_constants=>cs_abfer-loaded_outb_ship ).

          IF chk_pod_relevant_for_odlv( is_stops = <ls_stops>
                              it_vttp  = <lt_vttp>
                              it_vtsp  = <lt_vtsp> ) = abap_true.
            " POD
            ct_expeventdata = VALUE #( BASE ct_expeventdata (
              appsys            = mo_ef_parameters->get_appsys(  )
              appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
              language          = sy-langu
              appobjid          = is_app_objects-appobjid
              milestone         = zif_gtt_ef_constants=>cs_milestone-sh_pod
              evt_exp_datetime  = <ls_stops>-pln_evt_datetime
              evt_exp_tzone     = <ls_stops>-pln_evt_timezone
              locid2            = <ls_stops>-stopid_txt
              loctype           = <ls_stops>-loctype
              locid1            = zcl_gtt_tools=>get_pretty_location_id(
                                    iv_locid   = <ls_stops>-locid
                                    iv_loctype = <ls_stops>-loctype )
              milestonenum      = lv_milestonenum
            ) ).

            ADD 1 TO lv_milestonenum.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'VTTS' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD CHK_POD_RELEVANT_FOR_ODLV.

    DATA:
      lv_locid TYPE zif_gtt_mia_app_types=>tv_locid,
      lv_pdstk TYPE pdstk,
      lv_vbeln TYPE vttp-vbeln.

    rv_result = abap_false.

    IF is_stops-loccat  = zif_gtt_mia_app_constants=>cs_loccat-arrival AND
       is_stops-loctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.

      "Get Outbound Delivery Numbers
      LOOP AT it_vtsp ASSIGNING FIELD-SYMBOL(<ls_vtsp>)
        WHERE tknum = is_stops-tknum
          AND tsnum = is_stops-tsnum.

        CLEAR:
          lv_locid,
          lv_pdstk,
          lv_vbeln.

        READ TABLE it_vttp ASSIGNING FIELD-SYMBOL(<ls_vttp>)
          WITH KEY tknum = <ls_vtsp>-tknum
                   tpnum = <ls_vtsp>-tpnum.
        IF sy-subrc = 0.

          SELECT SINGLE kunnr INTO lv_locid FROM likp WHERE vbeln = <ls_vttp>-vbeln.

          lv_vbeln = <ls_vttp>-vbeln.
          SHIFT lv_vbeln LEFT DELETING LEADING '0'.
          SELECT SINGLE z_pdstk INTO lv_pdstk FROM zgtt_mia_ee_rel WHERE ( appobjid = lv_vbeln OR appobjid = <ls_vttp>-vbeln ).

          IF lv_locid = is_stops-locid AND lv_pdstk = abap_true.
            rv_result = abap_true.
            EXIT.
          ENDIF.

        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.
    mo_sh_data_old = NEW zcl_gtt_mia_sh_data_old(
      io_ef_parameters = io_ef_parameters ).

  ENDMETHOD.


  METHOD GET_CORRESPONDING_DLV_ITEMS.

    DATA(lr_lips) = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_delivery_item ).

    FIELD-SYMBOLS: <lt_lips> TYPE vtrlp_tab.

    CLEAR: et_appobjid[].

    ASSIGN lr_lips->* TO <lt_lips>.

    IF <lt_lips> IS ASSIGNED.
      LOOP AT <lt_lips> ASSIGNING FIELD-SYMBOL(<ls_lips>)
        WHERE vbeln IN it_vbeln
          AND werks IN it_werks.

        et_appobjid = VALUE #( BASE et_appobjid (
                        low     = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
                                    ir_lips = REF #( <ls_lips> ) )
                        option  = 'EQ'
                        sign    = 'I'
                      ) ).
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'LIPS' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_HEADER_FIELDS.

    et_fields   = VALUE #( ( 'DPREG' ) ( 'UPREG' )
                           ( 'DPLBG' ) ( 'UPLBG' )
                           ( 'DPLEN' ) ( 'UPLEN' ) ).

  ENDMETHOD.


  METHOD GET_SHIPPMENT_HEADER.

    TYPES: tt_vttk TYPE STANDARD TABLE OF vttkvb.

    FIELD-SYMBOLS: <lt_vttk> TYPE tt_vttk.

    DATA(lv_tknum) = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_object-maintabref
      iv_field_name  = 'TKNUM' ).

    ASSIGN ir_vttk->* TO <lt_vttk>.
    IF <lt_vttk> IS ASSIGNED.
      READ TABLE <lt_vttk> ASSIGNING FIELD-SYMBOL(<ls_vttk>)
        WITH KEY tknum = lv_tknum.

      IF sy-subrc = 0.
        rr_vttk = REF #( <ls_vttk> ).
      ELSE.
        MESSAGE e005(zgtt) WITH 'VTTK OLD' lv_tknum
          INTO DATA(lv_dummy).
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt) WITH 'VTTK'
        INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_STOP_FIELDS.

    et_fields   = VALUE #( ( 'DPTBG' ) ( 'UPTBG' )
                           ( 'DPTEN' ) ( 'UPTEN' )
                           ( 'KUNNA' ) ( 'KUNNZ' )
                           ( 'VSTEL' ) ( 'VSTEZ' )
                           ( 'LIFNA' ) ( 'LIFNZ' )
                           ( 'WERKA' ) ( 'WERKZ' )
                           ( 'KNOTA' ) ( 'KNOTZ' ) ).

  ENDMETHOD.


  METHOD IS_POD_RELEVANT.

    DATA: lt_vbeln    TYPE RANGE OF lips-vbeln,
          lt_appobjid TYPE RANGE OF /saptrx/aoid,
          lv_locid    TYPE zif_gtt_mia_app_types=>tv_locid,
          lv_pdstk    TYPE pdstk.

    rv_result = abap_false.

    IF is_stops-loccat  = zif_gtt_mia_app_constants=>cs_loccat-arrival AND
       is_stops-loctype = zif_gtt_ef_constants=>cs_loc_types-shippingpoint.

      " get Inbound Delivery Numbers
      LOOP AT it_vtsp ASSIGNING FIELD-SYMBOL(<ls_vtsp>)
        WHERE tknum = is_stops-tknum
          AND tsnum = is_stops-tsnum.

        READ TABLE it_vttp ASSIGNING FIELD-SYMBOL(<ls_vttp>)
          WITH KEY tknum = <ls_vtsp>-tknum
                   tpnum = <ls_vtsp>-tpnum.

        IF sy-subrc = 0.
          lt_vbeln[]    = VALUE #( BASE lt_vbeln
                                  ( option = 'EQ'
                                    sign   = 'I'
                                    low    = <ls_vttp>-vbeln ) ).
        ENDIF.
      ENDLOOP.

      " get appobjid range (inbound deliveries for corresponding Plant)
      IF lt_vbeln[] IS NOT INITIAL.
        get_corresponding_dlv_items(
          EXPORTING
            it_vbeln    = lt_vbeln
            it_werks    = VALUE #( ( low    = is_stops-locid
                                     option = 'EQ'
                                     sign   = 'I'  ) )
          IMPORTING
            et_appobjid = lt_appobjid ).
      ENDIF.

      " get POD enabled flags for found DLV Items
      IF lt_appobjid[] IS NOT INITIAL.
        SELECT SINGLE z_pdstk                           "#EC CI_NOORDER
          INTO rv_result
          FROM zgtt_mia_ee_rel
          WHERE appobjid IN lt_appobjid
            AND z_pdstk   = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD IS_STOP_CHANGED.

    FIELD-SYMBOLS: <lt_vtts_new> TYPE zif_gtt_mia_app_types=>tt_vttsvb,
                   <lt_vtts_old> TYPE zif_gtt_mia_app_types=>tt_vttsvb.

    DATA(lv_tknum)    = CONV tknum( zcl_gtt_tools=>get_field_of_structure(
                                      ir_struct_data = is_app_object-maintabref
                                      iv_field_name  = 'TKNUM' ) ).

    DATA(lr_vtts_new) = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_stage_new ).
    DATA(lr_vtts_old) = mo_sh_data_old->get_vtts( ).

    rv_result   = zif_gtt_ef_constants=>cs_condition-false.

    ASSIGN lr_vtts_new->* TO <lt_vtts_new>.
    ASSIGN lr_vtts_old->* TO <lt_vtts_old>.

    IF <lt_vtts_new> IS ASSIGNED AND
       <lt_vtts_old> IS ASSIGNED.

      LOOP AT <lt_vtts_new> ASSIGNING FIELD-SYMBOL(<ls_vtts_new>)
        WHERE tknum = lv_tknum
          AND updkz IS NOT INITIAL.

        CASE <ls_vtts_new>-updkz.
          WHEN zif_gtt_ef_constants=>cs_change_mode-insert.
            rv_result   = zif_gtt_ef_constants=>cs_condition-true.

          WHEN zif_gtt_ef_constants=>cs_change_mode-update OR
               zif_gtt_ef_constants=>cs_change_mode-undefined.

            READ TABLE <lt_vtts_old> ASSIGNING FIELD-SYMBOL(<ls_vtts_old>)
              WITH KEY tknum  = <ls_vtts_new>-tknum
                       tsnum  = <ls_vtts_new>-tsnum.

            rv_result = zcl_gtt_tools=>are_fields_different(
              ir_data1  = REF #( <ls_vtts_new> )
              ir_data2  = REF #( <ls_vtts_old> )
              it_fields = it_fields ).
        ENDCASE.

        IF rv_result   = zif_gtt_ef_constants=>cs_condition-true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF rv_result   = zif_gtt_ef_constants=>cs_condition-false.
        LOOP AT <lt_vtts_old> TRANSPORTING NO FIELDS
          WHERE tknum = lv_tknum
            AND updkz = zif_gtt_ef_constants=>cs_change_mode-delete.

          rv_result   = zif_gtt_ef_constants=>cs_condition-true.
        ENDLOOP.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD ZIF_GTT_PE_FILLER~CHECK_RELEVANCE.

    DATA(lr_vttp) = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_item_new ).

    rv_result = zif_gtt_ef_constants=>cs_condition-false.

    " check the fields, used in PE extractor and not used in TP extractor
    IF zcl_gtt_mia_sh_tools=>is_appropriate_type( ir_vttk = is_app_objects-maintabref ) = abap_true AND
       zcl_gtt_mia_sh_tools=>is_delivery_assigned( ir_vttp = lr_vttp ) = abap_true.

      " check in, load start, load end
      get_header_fields(
        IMPORTING
          et_fields = DATA(lt_header_fields) ).

      rv_result = zcl_gtt_tools=>are_fields_different(
                    ir_data1  = is_app_objects-maintabref
                    ir_data2  = get_shippment_header(
                                  is_app_object = is_app_objects
                                  ir_vttk       = mo_sh_data_old->get_vttk( ) )
                    it_fields = lt_header_fields ).

      " departure, arrival
      IF rv_result = zif_gtt_ef_constants=>cs_condition-false.
        get_stop_fields(
          IMPORTING
            et_fields = DATA(lt_stop_fields) ).

        rv_result = is_stop_changed(
          is_app_object = is_app_objects
          it_fields     = lt_stop_fields ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pe_filler~get_planed_events.

    add_shipment_events(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata
        ct_measrmntdata = ct_measrmntdata
        ct_infodata     = ct_infodata ).

    add_stops_events(
      EXPORTING
        is_app_objects  = is_app_objects
        iv_milestonenum = zcl_gtt_tools=>get_next_sequence_id(
                            it_expeventdata = ct_expeventdata )
      CHANGING
        ct_expeventdata = ct_expeventdata
        ct_measrmntdata = ct_measrmntdata
        ct_infodata     = ct_infodata ).

    add_planned_source_arrival(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    IF NOT line_exists( ct_expeventdata[ appobjid = is_app_objects-appobjid ] ).
      " planned events DELETION
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = ''
        evt_exp_datetime  = '000000000000000'
        evt_exp_tzone     = ''
      ) ).
    ENDIF.

  ENDMETHOD.


  METHOD add_planned_source_arrival.

    FIELD-SYMBOLS:
      <ls_vttk> TYPE vttkvb,
      <lt_vttp> TYPE vttpvb_tab.

    DATA:
      lr_vttk         TYPE REF TO data,
      lr_vttp         TYPE REF TO data,
      lv_count        TYPE i,
      lv_ltl          TYPE flag,
      lv_tknum        TYPE vttk-tknum,
      lv_locid2       TYPE /saptrx/loc_id_2,
      ls_eventdata    TYPE zif_gtt_ef_types=>ts_expeventdata,
      lv_exp_datetime TYPE /saptrx/event_exp_datetime.

    lr_vttk = is_app_objects-maintabref.
    lr_vttp = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_item_new ).

    ASSIGN lr_vttk->* TO <ls_vttk>.
    ASSIGN lr_vttp->* TO <lt_vttp>.

    CHECK <ls_vttk> IS ASSIGNED AND <lt_vttp> IS ASSIGNED.

    lv_tknum = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = lr_vttk
      iv_field_name  = 'TKNUM' ).

    SHIFT lv_tknum LEFT DELETING LEADING '0'.
    CONCATENATE lv_tknum '0001' INTO lv_locid2.

    lv_count = lines( <lt_vttp> ).
    IF lv_count > 1 AND <ls_vttk>-vsart = '01'. "Truck
      lv_ltl = abap_true.
    ENDIF.

    CHECK lv_ltl = abap_true.

    lv_exp_datetime = zcl_gtt_tools=>get_local_timestamp(
      iv_date = <ls_vttk>-dpreg     "Planned date of check-in
      iv_time = <ls_vttk>-upreg ).  "Planned check-in time

    READ TABLE ct_expeventdata INTO DATA(ls_expeventdata)
      WITH KEY milestone = zif_gtt_ef_constants=>cs_milestone-sh_departure
               locid2    = lv_locid2.
    IF sy-subrc = 0.
      ls_eventdata = ls_expeventdata.
      ls_eventdata-milestonenum = 0.
      ls_eventdata-milestone = zif_gtt_ef_constants=>cs_milestone-sh_arrival.
      ls_eventdata-evt_exp_datetime = lv_exp_datetime.
      ls_eventdata-evt_exp_tzone = zcl_gtt_tools=>get_system_time_zone( ).
*     Add planned source arrival event for LTL shipment
      APPEND ls_eventdata TO ct_expeventdata.
    ENDIF.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_SH_DATA_OLD definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS
    raising
      CX_UDM_MESSAGE .
  methods GET_VTTK
    returning
      value(RR_VTTK) type ref to DATA .
  methods GET_VTTP
    returning
      value(RR_VTTP) type ref to DATA .
  methods GET_VTTS
    returning
      value(RR_VTTS) type ref to DATA .
  methods GET_VTSP
    returning
      value(RR_VTSP) type ref to DATA .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_ef_parameters TYPE REF TO zif_gtt_ef_parameters .
    DATA mt_vttk TYPE zif_gtt_mia_app_types=>tt_vttkvb .
    DATA mt_vttp TYPE zif_gtt_mia_app_types=>tt_vttpvb .
    DATA mt_vtts TYPE zif_gtt_mia_app_types=>tt_vttsvb .
    DATA mt_vtsp TYPE zif_gtt_mia_app_types=>tt_vtspvb .

    METHODS init
      RAISING
        cx_udm_message .
    METHODS init_vttk
      RAISING
        cx_udm_message .
    METHODS init_vttp
      RAISING
        cx_udm_message .
    METHODS init_vtts
      RAISING
        cx_udm_message .
    METHODS init_vtsp
      RAISING
        cx_udm_message .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_SH_DATA_OLD IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    mo_ef_parameters  = io_ef_parameters.

    init( ).

  ENDMETHOD.


  METHOD GET_VTSP.

    rr_vtsp   = REF #( mt_vtsp ).

  ENDMETHOD.


  METHOD GET_VTTK.

    rr_vttk   = REF #( mt_vttk ).

  ENDMETHOD.


  METHOD GET_VTTP.

    rr_vttp   = REF #( mt_vttp ).

  ENDMETHOD.


  METHOD GET_VTTS.

    rr_vtts   = REF #( mt_vtts ).

  ENDMETHOD.


  METHOD INIT.

    init_vttk( ).

    init_vttp( ).

    init_vtts( ).

    init_vtsp( ).

  ENDMETHOD.


  METHOD INIT_VTSP.

    FIELD-SYMBOLS: <lt_vtsp_new> TYPE zif_gtt_mia_app_types=>tt_vtspvb,
                   <lt_vtsp_old> TYPE zif_gtt_mia_app_types=>tt_vtspvb.

    DATA(lr_vtsp_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_item_stage_new ).
    DATA(lr_vtsp_old) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_item_stage_old ).

    ASSIGN lr_vtsp_new->* TO <lt_vtsp_new>.
    ASSIGN lr_vtsp_old->* TO <lt_vtsp_old>.

    IF <lt_vtsp_new> IS ASSIGNED AND
       <lt_vtsp_old> IS ASSIGNED.

      mt_vtsp   = <lt_vtsp_old>.
      SORT mt_vtsp BY tknum tsnum tpnum.

      LOOP AT <lt_vtsp_new> ASSIGNING FIELD-SYMBOL(<ls_vtsp_new>)
        WHERE updkz IS INITIAL.

        READ TABLE mt_vtsp
          WITH KEY tknum = <ls_vtsp_new>-tknum
                   tsnum = <ls_vtsp_new>-tsnum
                   tpnum = <ls_vtsp_new>-tpnum
          TRANSPORTING NO FIELDS
          BINARY SEARCH.

        IF sy-subrc <> 0.
          INSERT <ls_vtsp_new> INTO mt_vtsp INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'VTSP' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD INIT_VTTK.

    FIELD-SYMBOLS: <lt_vttk_new> TYPE zif_gtt_mia_app_types=>tt_vttkvb,
                   <lt_vttk_old> TYPE zif_gtt_mia_app_types=>tt_vttkvb.

    DATA(lr_vttk_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_header_new ).
    DATA(lr_vttk_old) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_header_old ).

    ASSIGN lr_vttk_new->* TO <lt_vttk_new>.
    ASSIGN lr_vttk_old->* TO <lt_vttk_old>.

    IF <lt_vttk_new> IS ASSIGNED AND
       <lt_vttk_old> IS ASSIGNED.

      mt_vttk   = <lt_vttk_old>.
      SORT mt_vttk BY tknum.

      LOOP AT <lt_vttk_new> ASSIGNING FIELD-SYMBOL(<ls_vttk_new>)
        WHERE updkz IS INITIAL.

        READ TABLE mt_vttk
          WITH KEY tknum = <ls_vttk_new>-tknum
          TRANSPORTING NO FIELDS
          BINARY SEARCH.

        IF sy-subrc <> 0.
          INSERT <ls_vttk_new> INTO mt_vttk INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'VTTK' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD INIT_VTTP.

    FIELD-SYMBOLS: <lt_vttp_new> TYPE zif_gtt_mia_app_types=>tt_vttpvb,
                   <lt_vttp_old> TYPE zif_gtt_mia_app_types=>tt_vttpvb.

    DATA(lr_vttp_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_item_new ).
    DATA(lr_vttp_old) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_item_old ).

    ASSIGN lr_vttp_new->* TO <lt_vttp_new>.
    ASSIGN lr_vttp_old->* TO <lt_vttp_old>.

    IF <lt_vttp_new> IS ASSIGNED AND
       <lt_vttp_old> IS ASSIGNED.

      mt_vttp   = <lt_vttp_old>.
      SORT mt_vttp BY tknum tpnum.

      LOOP AT <lt_vttp_new> ASSIGNING FIELD-SYMBOL(<ls_vttp_new>)
        WHERE updkz IS INITIAL.

        READ TABLE mt_vttp
          WITH KEY tknum = <ls_vttp_new>-tknum
                   tpnum = <ls_vttp_new>-tpnum
          TRANSPORTING NO FIELDS
          BINARY SEARCH.

        IF sy-subrc <> 0.
          INSERT <ls_vttp_new> INTO mt_vttp INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'VTTP' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD INIT_VTTS.

    FIELD-SYMBOLS: <lt_vtts_new> TYPE zif_gtt_mia_app_types=>tt_vttsvb,
                   <lt_vtts_old> TYPE zif_gtt_mia_app_types=>tt_vttsvb.

    DATA(lr_vtts_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_stage_new ).
    DATA(lr_vtts_old) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-sh_stage_old ).

    ASSIGN lr_vtts_new->* TO <lt_vtts_new>.
    ASSIGN lr_vtts_old->* TO <lt_vtts_old>.

    IF <lt_vtts_new> IS ASSIGNED AND
       <lt_vtts_old> IS ASSIGNED.

      mt_vtts   = <lt_vtts_old>.
      SORT mt_vtts BY tknum tsnum.

      LOOP AT <lt_vtts_new> ASSIGNING FIELD-SYMBOL(<ls_vtts_new>)
        WHERE updkz IS INITIAL.

        READ TABLE mt_vtts
          WITH KEY tknum = <ls_vtts_new>-tknum
                   tsnum = <ls_vtts_new>-tsnum
          TRANSPORTING NO FIELDS
          BINARY SEARCH.

        IF sy-subrc <> 0.
          INSERT <ls_vtts_new> INTO mt_vtts INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'VTTS' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_SH_STOPS_EVENTS definition
  public
  create private .

public section.

  types:
    BEGIN OF ts_event_info,
        appsys        TYPE /saptrx/applsystem,
        appobjtype    TYPE /saptrx/aotype,
        appobjid      TYPE /saptrx/aoid,
        language      TYPE spras,
        evt_exp_tzone TYPE /saptrx/timezone,
      END OF ts_event_info .

  class-methods GET_INSTANCE_FOR_DELIVERY
    importing
      !IV_APPOBJID type /SAPTRX/AOID
      !IV_VBELN type VBELN_VL
      !IV_POSNR type POSNR_VL default 0
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
      !IO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS
    returning
      value(RO_SH_STOPS_EVENTS) type ref to ZCL_GTT_MIA_SH_STOPS_EVENTS
    raising
      CX_UDM_MESSAGE .
  methods GET_PLANNED_EVENTS
    exporting
      !ET_EXP_EVENT type /SAPTRX/BAPI_TRK_EE_TAB
    raising
      CX_UDM_MESSAGE .
  PROTECTED SECTION.
private section.

  types:
    tt_tknum   TYPE STANDARD TABLE OF tknum .
  types:
    BEGIN OF ts_stops_info,
        tknum    TYPE vttk-tknum,
        stops    TYPE zif_gtt_mia_app_types=>tt_stops,
        watching TYPE zif_gtt_mia_app_types=>tt_dlv_watch_stops,
      END OF ts_stops_info .
  types:
    tt_stops_info TYPE STANDARD TABLE OF ts_stops_info .

  data MV_VBELN type VBELN_VL .
  data MV_POSNR type POSNR_VL .
  data MS_EVENT_INFO type TS_EVENT_INFO .
  data MT_STOPS_INFO type TT_STOPS_INFO .
  data MT_LIPS type ZIF_GTT_MIA_APP_TYPES=>TT_LIPSVB .
  data MT_LIKP type VA_LIKPVB_T .

  methods CONSTRUCTOR
    importing
      !IV_VBELN type VBELN_VL
      !IV_POSNR type POSNR_VL
      !IS_EVENT_INFO type TS_EVENT_INFO
      !IT_STOPS_INFO type TT_STOPS_INFO
      !IT_LIPS type ZIF_GTT_MIA_APP_TYPES=>TT_LIPSVB
      !IT_LIKP type VA_LIKPVB_T .
  class-methods GET_DELIVERY_ITEMS
    importing
      !IV_VBELN type VBELN_VL
      !IV_POSNR type POSNR_VL
      !IR_LIPS type ref to DATA
    exporting
      !ET_LIPS type ZIF_GTT_MIA_APP_TYPES=>TT_LIPSVB
    raising
      CX_UDM_MESSAGE .
  class-methods GET_EVENT_INFO
    importing
      !IV_APPOBJID type /SAPTRX/AOID
      !IO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS
    returning
      value(RS_EVENT_INFO) type TS_EVENT_INFO
    raising
      CX_UDM_MESSAGE .
  class-methods GET_SHIPMENTS_FOR_DELIVERY
    importing
      !IV_VBELN type VBELN_VL
    exporting
      !ET_TKNUM type TT_TKNUM .
  class-methods GET_STOPS_INFO_FOR_DELIVERY
    importing
      !IV_VBELN type VBELN_VL
    exporting
      !ET_STOPS_INFO type TT_STOPS_INFO .
  methods IS_POD_RELEVANT
    importing
      !IV_LOCID type CLIKE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods GET_DELIVERY_HEADER
    importing
      !IV_VBELN type VBELN_VL
      !IR_LIKP type DATA
    exporting
      !ET_LIKP type VA_LIKPVB_T
    raising
      CX_UDM_MESSAGE .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_SH_STOPS_EVENTS IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    mv_vbeln      = iv_vbeln.
    ms_event_info = is_event_info.
    mt_stops_info = it_stops_info.
    mt_lips       = it_lips.
    mt_likp       = it_likp.

  ENDMETHOD.


  METHOD GET_DELIVERY_ITEMS.

    DATA: lt_vbeln TYPE RANGE OF vbeln_vl,
          lt_posnr TYPE RANGE OF posnr_vl.

    CLEAR: et_lips[].

    FIELD-SYMBOLS: <lt_lips>  TYPE zif_gtt_mia_app_types=>tt_lipsvb.

    ASSIGN ir_lips->* TO <lt_lips>.
    IF <lt_lips> IS ASSIGNED.
      IF iv_vbeln IS NOT INITIAL.
        lt_vbeln    = VALUE #( ( low = iv_vbeln option = 'EQ' sign = 'I' ) ).
      ENDIF.
      IF iv_posnr IS NOT INITIAL.
        lt_posnr    = VALUE #( ( low = iv_posnr option = 'EQ' sign = 'I' ) ).
      ENDIF.

      LOOP AT <lt_lips> ASSIGNING FIELD-SYMBOL(<ls_lips>)
        WHERE vbeln IN lt_vbeln
          AND posnr IN lt_posnr.

        APPEND <ls_lips> TO et_lips.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'LIPS' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.


  ENDMETHOD.


  METHOD GET_EVENT_INFO.


    rs_event_info = VALUE #(
      appsys        = io_ef_parameters->get_appsys(  )
      appobjtype    = io_ef_parameters->get_app_obj_types( )-aotype
      appobjid      = iv_appobjid
      language      = sy-langu
      evt_exp_tzone = zcl_gtt_tools=>get_system_time_zone( )
    ).

  ENDMETHOD.


  METHOD get_instance_for_delivery.

    DATA: lt_stops_info TYPE tt_stops_info,
          lt_lips       TYPE zif_gtt_mia_app_types=>tt_lipsvb,
          lt_likp       TYPE va_likpvb_t.

    DATA(ls_event_info) = get_event_info(
      iv_appobjid      = iv_appobjid
      io_ef_parameters = io_ef_parameters ).

    get_stops_info_for_delivery(
      EXPORTING
        iv_vbeln      = iv_vbeln
      IMPORTING
        et_stops_info = lt_stops_info ).

    get_delivery_items(
      EXPORTING
        iv_vbeln = iv_vbeln
        iv_posnr = iv_posnr
        ir_lips  = io_ef_parameters->get_appl_table(
                     iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-dl_item_new )
      IMPORTING
        et_lips  = lt_lips ).

    get_delivery_header(
      EXPORTING
        iv_vbeln = iv_vbeln
        ir_likp  = is_app_objects-maintabref
      IMPORTING
        et_likp  = lt_likp ).

    ro_sh_stops_events = NEW zcl_gtt_mia_sh_stops_events(
      iv_vbeln      = iv_vbeln
      iv_posnr      = iv_posnr
      is_event_info = ls_event_info
      it_stops_info = lt_stops_info
      it_lips       = lt_lips
      it_likp       = lt_likp ).

  ENDMETHOD.


  METHOD GET_PLANNED_EVENTS.

    DATA: lt_exp_event    TYPE /saptrx/bapi_trk_ee_tab,
          lv_milestonenum TYPE /saptrx/seq_num.

    LOOP AT mt_stops_info ASSIGNING FIELD-SYMBOL(<ls_stops_info>).
      lv_milestonenum  = 1.

      LOOP AT <ls_stops_info>-watching ASSIGNING FIELD-SYMBOL(<ls_watching>)
        WHERE vbeln = mv_vbeln.

        READ TABLE <ls_stops_info>-stops ASSIGNING FIELD-SYMBOL(<ls_stops>)
          WITH KEY stopid = <ls_watching>-stopid
                   loccat = <ls_watching>-loccat.

        IF sy-subrc = 0.
          " Departure / Arrival
          lt_exp_event = VALUE #( BASE lt_exp_event (
              milestone         = COND #( WHEN <ls_watching>-loccat = zif_gtt_mia_app_constants=>cs_loccat-departure
                                            THEN zif_gtt_ef_constants=>cs_milestone-sh_departure
                                            ELSE zif_gtt_ef_constants=>cs_milestone-sh_arrival )
              locid2            = <ls_stops>-stopid_txt
              loctype           = <ls_stops>-loctype
              locid1            = zcl_gtt_tools=>get_pretty_location_id(
                                    iv_locid   = <ls_stops>-locid
                                    iv_loctype = <ls_stops>-loctype )
              evt_exp_datetime  = <ls_stops>-pln_evt_datetime
              evt_exp_tzone     = <ls_stops>-pln_evt_timezone
              milestonenum      = lv_milestonenum
          ) ).

          ADD 1 TO lv_milestonenum.

          " POD
          IF <ls_stops>-loccat  = zif_gtt_mia_app_constants=>cs_loccat-arrival AND
             <ls_stops>-loctype = zif_gtt_ef_constants=>cs_loc_types-shippingpoint AND
             is_pod_relevant( iv_locid = <ls_stops>-locid ) = abap_true.

            lt_exp_event = VALUE #( BASE lt_exp_event (
                milestone         = zif_gtt_ef_constants=>cs_milestone-sh_pod
                locid2            = <ls_stops>-stopid_txt
                loctype           = <ls_stops>-loctype
                locid1            = zcl_gtt_tools=>get_pretty_location_id(
                                      iv_locid   = <ls_stops>-locid
                                      iv_loctype = <ls_stops>-loctype )
                evt_exp_datetime  = <ls_stops>-pln_evt_datetime
                evt_exp_tzone     = <ls_stops>-pln_evt_timezone
                milestonenum      = lv_milestonenum
            ) ).

            ADD 1 TO lv_milestonenum.
          ENDIF.
        ELSE.
          MESSAGE e005(zgtt)
            WITH |{ <ls_watching>-stopid }{ <ls_watching>-loccat }| 'STOPS'
            INTO DATA(lv_dummy).
          zcl_gtt_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
      <ls_exp_event>-appsys         = ms_event_info-appsys.         "mv_appsys.
      <ls_exp_event>-appobjtype     = ms_event_info-appobjtype.     "is_aotype-aot_type.
      <ls_exp_event>-appobjid       = ms_event_info-appobjid.       "is_likp-vbeln.
      <ls_exp_event>-language       = ms_event_info-language.       "sy-langu.
      <ls_exp_event>-evt_exp_tzone  = ms_event_info-evt_exp_tzone.  "lcl_tools=>get_system_time_zone(  ).
    ENDLOOP.

    et_exp_event  = lt_exp_event.

  ENDMETHOD.


  METHOD GET_SHIPMENTS_FOR_DELIVERY.

    DATA: ls_comwa6 TYPE vbco6,
          lt_vbfas  TYPE STANDARD TABLE OF vbfas.

    CLEAR: et_tknum[].

    ls_comwa6-vbeln   = iv_vbeln.

    CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
      EXPORTING
        comwa         = ls_comwa6
      TABLES
        vbfa_tab      = lt_vbfas
      EXCEPTIONS
        no_vbfa       = 1
        no_vbuk_found = 2
        OTHERS        = 3.

    IF sy-subrc = 0.
      LOOP AT lt_vbfas ASSIGNING FIELD-SYMBOL(<ls_vbfas>)
        WHERE vbtyp_n = zif_gtt_mia_app_constants=>cs_vbtyp-shipment
          AND vbtyp_v = zif_gtt_mia_app_constants=>cs_vbtyp-delivery.

        APPEND <ls_vbfas>-vbeln TO et_tknum.
      ENDLOOP.

      SORT et_tknum.
      DELETE ADJACENT DUPLICATES FROM et_tknum.
    ENDIF.

  ENDMETHOD.


  METHOD GET_STOPS_INFO_FOR_DELIVERY.

    DATA: lt_tknum      TYPE tt_tknum,
          ls_stops_info TYPE ts_stops_info.

    CLEAR: et_stops_info.

    get_shipments_for_delivery(
      EXPORTING
        iv_vbeln = iv_vbeln
      IMPORTING
        et_tknum = lt_tknum ).

    LOOP AT lt_tknum ASSIGNING FIELD-SYMBOL(<lv_tknum>).
      CLEAR: ls_stops_info.

      ls_stops_info-tknum   = <lv_tknum>.

      zcl_gtt_mia_sh_tools=>get_stops_from_shipment(
        EXPORTING
          iv_tknum              = ls_stops_info-tknum
        IMPORTING
          et_stops              = ls_stops_info-stops
          et_dlv_watching_stops = ls_stops_info-watching ).

      " important for milestonenum (sequence number) calculation
      SORT ls_stops_info-watching BY stopid loccat.

      APPEND ls_stops_info TO et_stops_info.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_pod_relevant.

    CLEAR: rv_result.

    READ TABLE mt_likp TRANSPORTING NO FIELDS
      WITH KEY vbeln = mv_vbeln
               vstel = iv_locid.
    IF sy-subrc = 0.
      SELECT SINGLE z_pdstk INTO rv_result
        FROM zgtt_mia_ee_rel
        WHERE appobjid  = ms_event_info-appobjid.

      rv_result   = boolc( sy-subrc = 0 AND rv_result = abap_true ).
    ENDIF.

  ENDMETHOD.


  METHOD get_delivery_header.

    FIELD-SYMBOLS:
      <ls_likp> TYPE any.
    DATA:
      ls_likpvb TYPE likpvb.

    CLEAR et_likp.

    ASSIGN ir_likp->* TO <ls_likp>.
    IF <ls_likp> IS NOT ASSIGNED.
      MESSAGE e002(zgtt) WITH 'LIKP' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
    MOVE-CORRESPONDING <ls_likp> TO ls_likpvb.
    APPEND ls_likpvb TO et_likp.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_SH_TOOLS definition
  public
  create public .

public section.

  types:
    BEGIN OF ts_loc_info,
      loctype      TYPE zif_gtt_mia_app_types=>tv_loctype,
      locid        TYPE zif_gtt_mia_app_types=>tv_locid,
      locaddrnum   TYPE adrnr,
      locindicator TYPE char1,
    END OF ts_loc_info .
  types:
    tt_loc_info type STANDARD TABLE OF ts_loc_info .

  class-methods GET_CARRIER_REFERENCE_DOCUMENT
    importing
      !IS_VTTK type VTTKVB
    exporting
      !EV_REF_TYP type ZIF_GTT_MIA_APP_TYPES=>TV_CRDOC_REF_TYP
      !EV_REF_VAL type ZIF_GTT_MIA_APP_TYPES=>TV_CRDOC_REF_VAL .
  class-methods GET_FORMATED_SH_NUMBER
    importing
      !IR_VTTK type ref to DATA
    returning
      value(RV_TKNUM) type TKNUM
    raising
      CX_UDM_MESSAGE .
  class-methods GET_FORMATED_SH_STOPID
    importing
      !IV_TKNUM type TKNUM
      !IV_CNT type CLIKE
    returning
      value(RV_STOPID) type ZIF_GTT_MIA_APP_TYPES=>TV_STOPID .
  class-methods GET_NEXT_EVENT_COUNTER
    returning
      value(RV_EVTCNT) type /SAPTRX/EVTCNT .
  class-methods GET_TRACKING_ID_SH_HEADER
    importing
      !IR_VTTK type ref to DATA
    returning
      value(RV_TRACK_ID) type /SAPTRX/TRXID
    raising
      CX_UDM_MESSAGE .
  class-methods GET_STOPS_FROM_SHIPMENT
    importing
      !IV_TKNUM type TKNUM
      !IT_VTTS type VTTSVB_TAB optional
      !IT_VTSP type VTSPVB_TAB optional
      !IT_VTTP type VTTPVB_TAB optional
    exporting
      !ET_STOPS type ZIF_GTT_MIA_APP_TYPES=>TT_STOPS
      !ET_DLV_WATCHING_STOPS type ZIF_GTT_MIA_APP_TYPES=>TT_DLV_WATCH_STOPS .
  class-methods IS_APPROPRIATE_TYPE
    importing
      !IR_VTTK type ref to DATA
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  class-methods IS_DELIVERY_ASSIGNED
    importing
      !IR_VTTP type ref to DATA
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  class-methods IS_OBJECT_MODIFIED
    importing
      !IS_EVENTS type TRXAS_EVT_CTAB_WA
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods GET_SCAC_CODE
    importing
      !IV_PARTNER type BU_PARTNER
    returning
      value(RV_NUM) type /SAPTRX/PARAMVAL200 .
protected section.
private section.

  class-data MV_EVTCNT type /SAPTRX/EVTCNT value ZIF_GTT_MIA_APP_CONSTANTS=>CS_START_EVTCNT-SHIPMENT ##NO_TEXT.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_SH_TOOLS IMPLEMENTATION.


  METHOD get_carrier_reference_document.

    IF is_vttk-vsart = '01' AND
       is_vttk-tndr_trkid IS NOT INITIAL.
      ev_ref_typ  = 'BN'.
      ev_ref_val  = is_vttk-tndr_trkid.
    ELSEIF is_vttk-vsart = '04' AND
           is_vttk-tndr_trkid IS NOT INITIAL.
      ev_ref_typ  = 'T50'.
      ev_ref_val  = is_vttk-tndr_trkid.
    ELSEIF ( is_vttk-vsart = '05' OR is_vttk-vsart = '15' ) AND
             is_vttk-tndr_trkid IS NOT INITIAL.
      ev_ref_typ  = 'T55'.
      ev_ref_val  = is_vttk-tndr_trkid.
    ELSE.
      CLEAR: ev_ref_typ, ev_ref_val.
    ENDIF.

  ENDMETHOD.


  METHOD get_formated_sh_number.

    rv_tknum  = zcl_gtt_tools=>get_field_of_structure(
                  ir_struct_data = ir_vttk
                  iv_field_name  = 'TKNUM' ).

    rv_tknum  = |{ rv_tknum ALPHA = OUT }|.

  ENDMETHOD.


  METHOD get_formated_sh_stopid.
*    rv_stopid   = |{ iv_tknum ALPHA = OUT }{ iv_cnt ALPHA = OUT }|.
*    CONDENSE rv_stopid NO-GAPS.

    rv_stopid   = |{ iv_tknum ALPHA = OUT }{ iv_cnt }|.

    CONDENSE rv_stopid NO-GAPS.
  ENDMETHOD.


  METHOD get_next_event_counter.

    ADD 1 TO mv_evtcnt.

    rv_evtcnt = mv_evtcnt.

  ENDMETHOD.


  METHOD get_stops_from_shipment.

    DATA:
      ls_vttsvb            TYPE vttsvb,
      lt_vttsvb            TYPE STANDARD TABLE OF vttsvb,
      ls_vtsp              TYPE vtsp,
      lt_vtsp              TYPE STANDARD TABLE OF vtsp,
      ls_vtspvb            TYPE vtspvb,
      lt_vtspvb            TYPE STANDARD TABLE OF vtspvb,
      ls_vttpvb            TYPE vttpvb,
      lt_vttpvb            TYPE STANDARD TABLE OF vttpvb,
      ls_stop              TYPE zif_gtt_mia_app_types=>ts_stops,
      ls_dlv_watching_stop TYPE zif_gtt_mia_app_types=>ts_dlv_watch_stops,
*       Count
      lv_stopcnt           TYPE int4,
      lv_cnt               TYPE char04,
*       Source & Destination
      lv_desloctype        TYPE zif_gtt_mia_app_types=>tv_loctype,
      lv_deslocid          TYPE zif_gtt_mia_app_types=>tv_locid,
      lv_srcloctype        TYPE zif_gtt_mia_app_types=>tv_loctype,
      lv_srclocid          TYPE zif_gtt_mia_app_types=>tv_locid,
*       Timezone
      lv_tzone             TYPE timezone,
*       Door text
      lv_ltort             TYPE t30bt-ltort,
*       Warehouse text
      lv_lnumt             TYPE t300t-lnumt,
*       Warehouse text / door text
      lv_lgtratxt          TYPE char60,
      lv_loc_id_exsit      TYPE flag.

    DATA: lt_tknum_range TYPE STANDARD TABLE OF range_c10.

    lv_tzone = zcl_gtt_tools=>get_system_time_zone( ).

*   Read Stage Information
    IF it_vtts IS SUPPLIED.
      MOVE it_vtts TO lt_vttsvb.
    ELSE.
      lt_tknum_range = VALUE #( (
        sign   = 'I'
        option = 'EQ'
        low    = iv_tknum
      ) ).

      CALL FUNCTION 'ST_STAGES_READ'
        EXPORTING
          i_vtts_db_tab = 'VTTS'
          i_vtsp_db_tab = 'VTSP'
        TABLES
          i_tknum_range = lt_tknum_range
          c_xvttsvb     = lt_vttsvb.
    ENDIF.

    SORT lt_vttsvb BY tsrfo.

*   Fill source & destination
    LOOP AT lt_vttsvb INTO ls_vttsvb WHERE tknum = iv_tknum
                                       AND updkz <> 'D'.
      CLEAR lv_loc_id_exsit.
      IF ls_vttsvb-kunna IS NOT INITIAL.
        lv_srcloctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
        lv_srclocid   = ls_vttsvb-kunna.
        lv_loc_id_exsit  = abap_true.
      ELSEIF ls_vttsvb-vstel IS NOT INITIAL.
        lv_srcloctype = zif_gtt_ef_constants=>cs_loc_types-shippingpoint.
        lv_srclocid   = ls_vttsvb-vstel.
        lv_loc_id_exsit  = abap_true.
      ELSEIF ls_vttsvb-lifna IS NOT INITIAL.
        lv_srcloctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
        lv_srclocid   = ls_vttsvb-lifna.
        lv_loc_id_exsit  = abap_true.
      ELSEIF ls_vttsvb-werka IS NOT INITIAL.
        lv_srcloctype = zif_gtt_ef_constants=>cs_loc_types-plant.
        lv_srclocid   = ls_vttsvb-werka.
        lv_loc_id_exsit  = abap_true.
      ELSEIF ls_vttsvb-knota IS NOT INITIAL.
        lv_srcloctype = zif_gtt_ef_constants=>cs_loc_types-logisticlocation.
        lv_srclocid   = ls_vttsvb-knota.
        lv_loc_id_exsit  = abap_true.
      ENDIF.

*     Support one time location address(Source location id)
      IF lv_loc_id_exsit = abap_false
        AND ls_vttsvb-adrknza CA zif_gtt_ef_constants=>shp_addr_ind_man_all
        AND ls_vttsvb-adrna IS NOT INITIAL.
        lv_srcloctype = zif_gtt_ef_constants=>cs_loc_types-logisticlocation.
        lv_srclocid   = ls_vttsvb-adrna.
        SHIFT lv_srclocid LEFT DELETING LEADING '0'.
      ENDIF.

*     if current stage line's source = last stage line's destination, no change on stop id & stop count
      IF lv_srcloctype NE lv_desloctype OR lv_srclocid NE lv_deslocid.
        lv_stopcnt = lv_stopcnt + 1.
      ENDIF.

      CLEAR lv_loc_id_exsit.
      IF ls_vttsvb-kunnz IS NOT INITIAL.
        lv_desloctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
        lv_deslocid   = ls_vttsvb-kunnz.
        lv_loc_id_exsit  = abap_true.
      ELSEIF ls_vttsvb-vstez IS NOT INITIAL.
        lv_desloctype = zif_gtt_ef_constants=>cs_loc_types-shippingpoint.
        lv_deslocid   = ls_vttsvb-vstez.
        lv_loc_id_exsit  = abap_true.
      ELSEIF ls_vttsvb-lifnz IS NOT INITIAL.
        lv_desloctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
        lv_deslocid   = ls_vttsvb-lifnz.
        lv_loc_id_exsit  = abap_true.
      ELSEIF ls_vttsvb-werkz IS NOT INITIAL.
        lv_desloctype = zif_gtt_ef_constants=>cs_loc_types-plant.
        lv_deslocid   = ls_vttsvb-werkz.
        lv_loc_id_exsit  = abap_true.
      ELSEIF ls_vttsvb-knotz IS NOT INITIAL.
        lv_desloctype = zif_gtt_ef_constants=>cs_loc_types-logisticlocation.
        lv_deslocid   = ls_vttsvb-knotz.
        lv_loc_id_exsit  = abap_true.
      ENDIF.

*     Support one time location address(Destination location id)
      IF lv_loc_id_exsit = abap_false
        AND ls_vttsvb-adrknzz CA zif_gtt_ef_constants=>shp_addr_ind_man_all
        AND ls_vttsvb-adrnz IS NOT INITIAL.
        lv_desloctype = zif_gtt_ef_constants=>cs_loc_types-logisticlocation.
        lv_deslocid   = ls_vttsvb-adrnz.
        SHIFT lv_deslocid LEFT DELETING LEADING '0'.
      ENDIF.

      lv_cnt = lv_stopcnt.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_cnt
        IMPORTING
          output = lv_cnt.

      CONCATENATE iv_tknum lv_cnt INTO ls_stop-stopid.
      ls_stop-stopid_txt    = get_formated_sh_stopid(
                                iv_tknum = iv_tknum
                                iv_cnt   = lv_cnt ).
      ls_stop-stopcnt       = lv_stopcnt.
      ls_stop-loccat        = 'S'.
      ls_stop-loctype       = lv_srcloctype.
      ls_stop-locid         = lv_srclocid.
      ls_stop-kunablaz_txt  = ls_vttsvb-kunabla.
      ls_stop-lgnumaz       = ls_vttsvb-lgnuma.
      ls_stop-toraz         = ls_vttsvb-tora.
      ls_stop-tknum         = iv_tknum.
      ls_stop-tsnum         = ls_vttsvb-tsnum.
      ls_stop-tsrfo         = ls_vttsvb-tsrfo.
      IF ls_vttsvb-dptbg IS INITIAL.
        CLEAR ls_stop-pln_evt_datetime.
      ELSE.
        ls_stop-pln_evt_datetime  = |0{ ls_vttsvb-dptbg }{ ls_vttsvb-uptbg }|.
      ENDIF.
      ls_stop-pln_evt_timezone = lv_tzone.

      CLEAR ls_stop-lstelz_txt.
      SELECT SINGLE vtext INTO ls_stop-lstelz_txt FROM tvlat
                                                  WHERE spras = sy-langu
                                                  AND   vstel = ls_vttsvb-vstel
                                                  AND   lstel = ls_vttsvb-lstel.

      CLEAR ls_stop-lgortaz_txt.
      SELECT SINGLE lgobe INTO ls_stop-lgortaz_txt FROM t001l
                                                   WHERE werks = ls_vttsvb-werka
                                                   AND   lgort = ls_vttsvb-lgorta.

*  Warehouse door text: concatenate T300T-LNUMT '/' T30BT-ltort with LGNUM and LGTOR
      CLEAR lv_ltort.
      SELECT SINGLE ltort INTO lv_ltort FROM t30bt WHERE spras = sy-langu
                                                   AND   lgnum = ls_vttsvb-lgnuma
                                                   AND   lgtor = ls_vttsvb-tora.
      CLEAR lv_lnumt.
      SELECT SINGLE lnumt INTO lv_lnumt FROM t300t WHERE spras = sy-langu
                                                   AND   lgnum = ls_vttsvb-lgnuma.
      CLEAR ls_stop-lgtraz_txt.
      IF lv_ltort IS NOT INITIAL OR lv_lnumt IS NOT INITIAL.
        CONCATENATE lv_lnumt lv_ltort INTO ls_stop-lgtraz_txt SEPARATED BY '/'.
      ENDIF.


      APPEND ls_stop TO et_stops.

      lv_stopcnt = lv_stopcnt + 1.

      lv_cnt = lv_stopcnt.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_cnt
        IMPORTING
          output = lv_cnt.

      CONCATENATE iv_tknum lv_cnt INTO ls_stop-stopid.
      ls_stop-stopid_txt    = get_formated_sh_stopid(
                                iv_tknum = iv_tknum
                                iv_cnt   = lv_cnt ).
      ls_stop-stopcnt       = lv_stopcnt.
      ls_stop-loccat        = 'D'.
      ls_stop-loctype       = lv_desloctype.
      ls_stop-locid         = lv_deslocid.
      ls_stop-kunablaz_txt  = ls_vttsvb-kunablz.
      ls_stop-lgnumaz       = ls_vttsvb-lgnumz.
      ls_stop-toraz         = ls_vttsvb-torz.
      ls_stop-tknum         = iv_tknum.
      ls_stop-tsnum         = ls_vttsvb-tsnum.
      ls_stop-tsrfo         = ls_vttsvb-tsrfo.
      IF ls_vttsvb-dpten IS INITIAL.
        CLEAR ls_stop-pln_evt_datetime.
      ELSE.
        ls_stop-pln_evt_datetime  = |0{ ls_vttsvb-dpten }{ ls_vttsvb-upten }|.
      ENDIF.
      ls_stop-pln_evt_timezone = lv_tzone.

      CLEAR ls_stop-lstelz_txt.
      SELECT SINGLE vtext INTO ls_stop-lstelz_txt FROM tvlat
                                                  WHERE spras = sy-langu
                                                  AND   vstel = ls_vttsvb-vstez
                                                  AND   lstel = ls_vttsvb-lstez.

      CLEAR ls_stop-lgortaz_txt.
      SELECT SINGLE lgobe INTO ls_stop-lgortaz_txt FROM t001l
                                                   WHERE werks = ls_vttsvb-werkz
                                                   AND   lgort = ls_vttsvb-lgortz.

*  Warehouse door text: concatenate T300T-LNUMT '/' T30BT-ltort with LGNUM and LGTOR
      CLEAR lv_ltort.
      SELECT SINGLE ltort INTO lv_ltort FROM t30bt WHERE spras = sy-langu
                                                   AND   lgnum = ls_vttsvb-lgnumz
                                                   AND   lgtor = ls_vttsvb-torz.
      CLEAR lv_lnumt.
      SELECT SINGLE lnumt INTO lv_lnumt FROM t300t WHERE spras = sy-langu
                                                   AND   lgnum = ls_vttsvb-lgnumz.
      CLEAR ls_stop-lgtraz_txt.
      IF lv_ltort IS NOT INITIAL OR lv_lnumt IS NOT INITIAL.
        CONCATENATE lv_lnumt lv_ltort INTO ls_stop-lgtraz_txt SEPARATED BY '/'.
      ENDIF.

      APPEND ls_stop TO et_stops.

    ENDLOOP.

    CHECK et_stops IS NOT INITIAL.

*   Read Stage / Item relation Information
    IF it_vtsp IS SUPPLIED.
      MOVE it_vtsp TO lt_vtspvb.
    ELSE.
      lt_tknum_range = VALUE #( (
        sign   = 'I'
        option = 'EQ'
        low    = iv_tknum
      ) ).

      CALL FUNCTION 'ST_STAGES_READ'
        EXPORTING
          i_vtts_db_tab = 'VTTS'
          i_vtsp_db_tab = 'VTSP'
        TABLES
          i_tknum_range = lt_tknum_range
          c_xvtsp       = lt_vtsp
          c_xvtspvb     = lt_vtspvb.
    ENDIF.

    CHECK lt_vtspvb IS NOT INITIAL.

*   Read Item Information
    IF it_vttp IS SUPPLIED.
      MOVE it_vttp TO lt_vttpvb.
    ELSE.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_vttpvb
             FROM vttp
             WHERE tknum = iv_tknum.
    ENDIF.

    LOOP AT lt_vtspvb INTO ls_vtspvb WHERE tknum IS NOT INITIAL
                                       AND tpnum IS NOT INITIAL
                                       AND updkz <> 'D'.
      CLEAR ls_dlv_watching_stop.

      READ TABLE lt_vttpvb INTO ls_vttpvb WITH KEY tknum = ls_vtspvb-tknum
                                                   tpnum = ls_vtspvb-tpnum.
      ls_dlv_watching_stop-vbeln = ls_vttpvb-vbeln.
      LOOP AT et_stops INTO ls_stop
        WHERE tknum = ls_vtspvb-tknum
          AND tsnum = ls_vtspvb-tsnum.

        ls_dlv_watching_stop-stopid     = ls_stop-stopid.
        ls_dlv_watching_stop-stopid_txt = ls_stop-stopid_txt.
        ls_dlv_watching_stop-loccat     = ls_stop-loccat.
        APPEND ls_dlv_watching_stop TO et_dlv_watching_stops.
      ENDLOOP.
    ENDLOOP.

    SORT et_dlv_watching_stops BY vbeln stopid loccat.

  ENDMETHOD.


  METHOD get_tracking_id_sh_header.

    DATA: lv_tknum TYPE vttk-tknum.

    lv_tknum  = zcl_gtt_tools=>get_field_of_structure(
                  ir_struct_data = ir_vttk
                  iv_field_name  = 'TKNUM' ).

    rv_track_id   = |{ lv_tknum ALPHA = OUT }|.

  ENDMETHOD.


  METHOD is_appropriate_type.

    DATA: lv_abfer  TYPE tvtk-abfer.

    DATA(lv_shtyp)  = CONV shtyp( zcl_gtt_tools=>get_field_of_structure(
                                    ir_struct_data = ir_vttk
                                    iv_field_name  = 'SHTYP' ) ).

    SELECT SINGLE abfer
      INTO lv_abfer
      FROM tvtk
      WHERE shtyp = lv_shtyp.

    IF sy-subrc = 0.
      rv_result   = boolc( lv_abfer = zif_gtt_mia_app_constants=>cs_abfer-empty_inb_ship OR
                           lv_abfer = zif_gtt_mia_app_constants=>cs_abfer-loaded_inb_ship OR
                           lv_abfer = zif_gtt_mia_app_constants=>cs_abfer-empty_outb_ship OR
                           lv_abfer = zif_gtt_mia_app_constants=>cs_abfer-loaded_outb_ship ).
    ELSE.
      MESSAGE e057(00) WITH lv_shtyp '' '' 'TVTK'
        INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD is_delivery_assigned.

    TYPES: tt_vttp TYPE STANDARD TABLE OF vttpvb.

    FIELD-SYMBOLS: <lt_vttp> TYPE tt_vttp.

    ASSIGN ir_vttp->* TO <lt_vttp>.

    rv_result = boolc( <lt_vttp> IS ASSIGNED AND
                       <lt_vttp> IS NOT INITIAL ).

  ENDMETHOD.


  METHOD is_object_modified.

    rv_result   = boolc(
      is_events-update_indicator = zif_gtt_ef_constants=>cs_change_mode-insert OR
      is_events-update_indicator = zif_gtt_ef_constants=>cs_change_mode-update OR
      is_events-update_indicator = zif_gtt_ef_constants=>cs_change_mode-undefined
    ).

  ENDMETHOD.


  METHOD get_scac_code.

    DATA:
      lt_bpsc  TYPE TABLE OF /scmtms/cv_bpscac,
      lv_lines TYPE i.

    CLEAR:rv_num.
    CHECK iv_partner IS NOT INITIAL.

    SELECT *
      INTO TABLE @lt_bpsc
      FROM /scmtms/cv_bpscac
     WHERE partner = @iv_partner.

    lv_lines = lines( lt_bpsc ).

    IF lv_lines > 1.
      DELETE lt_bpsc WHERE scac_valfr > sy-datum OR scac_valto < sy-datum.
    ENDIF.

    READ TABLE lt_bpsc INTO DATA(ls_bpsc) WITH KEY type = 'BUP006'."Standard Carrier Alpha Code
    IF sy-subrc = 0.
      rv_num = zif_gtt_mia_app_constants=>cv_scac_prefix && ls_bpsc-scac.
    ENDIF.

  ENDMETHOD.
ENDCLASS.""",
    r"""lass ZCL_GTT_MIA_TM_TOOLS definition
  public
  create public .

public section.

  class-methods GET_FORMATED_TOR_ID
    importing
      !IR_DATA type ref to DATA
    returning
      value(RV_TOR_ID) type /SCMTMS/TOR_ID
    raising
      CX_UDM_MESSAGE .
  class-methods GET_FORMATED_TOR_ITEM
    importing
      !IR_DATA type ref to DATA
    returning
      value(RV_ITEM_ID) type /SCMTMS/ITEM_ID
    raising
      CX_UDM_MESSAGE .
  class-methods GET_TOR_ROOT_TOR_ID
    importing
      !IV_KEY type /BOBF/CONF_KEY
    returning
      value(RV_TOR_ID) type /SCMTMS/TOR_ID .
  class-methods GET_TOR_ITEMS_FOR_DLV_ITEMS
    importing
      !IT_LIPS type ZIF_GTT_MIA_APP_TYPES=>TT_LIPSVB_KEY
      !IV_TOR_CAT type /SCMTMS/TOR_CATEGORY optional
    exporting
      !ET_KEY type /BOBF/T_FRW_KEY
      !ET_FU_ITEM type /SCMTMS/T_TOR_ITEM_TR_K
    raising
      CX_UDM_MESSAGE .
  class-methods IS_FU_RELEVANT
    importing
      !IT_LIPS type ZIF_GTT_MIA_APP_TYPES=>TT_LIPSVB_KEY
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  class-methods GET_TOR_ROOT
    importing
      !IV_KEY type /BOBF/CONF_KEY
    exporting
      !ES_TOR type /SCMTMS/S_TOR_ROOT_K
    raising
      CX_UDM_MESSAGE .
  PROTECTED SECTION.
private section.

  class-methods GET_TOR_ITEM_SELPARAM_FOR_LIPS
    importing
      !IT_LIPS type ZIF_GTT_MIA_APP_TYPES=>TT_LIPSVB_KEY
    exporting
      !ET_SELPARAM type /BOBF/T_FRW_QUERY_SELPARAM .
  class-methods FILTER_TOR_ITEMS_BY_TOR_CAT
    importing
      !IV_TOR_CAT type /SCMTMS/TOR_CATEGORY
    changing
      !CT_KEY type /BOBF/T_FRW_KEY .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_TM_TOOLS IMPLEMENTATION.


  METHOD filter_tor_items_by_tor_cat.

    DATA: lo_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
          lt_tor_root TYPE /scmtms/t_tor_root_k,
          lv_tor_cat  TYPE /scmtms/tor_category.

    lo_srv_mgr    = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
                      /scmtms/if_tor_c=>sc_bo_key ).

    LOOP AT ct_key ASSIGNING FIELD-SYMBOL(<ls_key>).
      TRY.
          lo_srv_mgr->retrieve_by_association(
            EXPORTING
              iv_node_key             = /scmtms/if_tor_c=>sc_node-item_tr
              it_key                  = VALUE #( ( <ls_key> ) )
              iv_association          = /scmtms/if_tor_c=>sc_association-item_tr-to_parent
              iv_fill_data            = abap_true
            IMPORTING
              et_data                 = lt_tor_root ).
        CATCH /bobf/cx_frw_contrct_violation.
          CLEAR lt_tor_root.
      ENDTRY.

      lv_tor_cat  = VALUE #( lt_tor_root[ 1 ]-tor_cat OPTIONAL ).

      IF lv_tor_cat <> iv_tor_cat.
        DELETE ct_key.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_formated_tor_id.

    rv_tor_id   = zcl_gtt_tools=>get_field_of_structure(
                    ir_struct_data = ir_data
                    iv_field_name  = 'TOR_ID' ).

    rv_tor_id   = |{ rv_tor_id ALPHA = OUT }|.

  ENDMETHOD.


  METHOD get_formated_tor_item.

    rv_item_id   = zcl_gtt_tools=>get_field_of_structure(
                    ir_struct_data = ir_data
                    iv_field_name  = 'ITEM_ID' ).

  ENDMETHOD.


  METHOD get_tor_items_for_dlv_items.

    DATA: lo_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
          lo_message  TYPE REF TO /bobf/if_frw_message,
          lt_altkey   TYPE /scmtms/t_base_document_w_item,
          lt_selparam TYPE /bobf/t_frw_query_selparam,
          lt_key      TYPE /bobf/t_frw_key,
          lt_result   TYPE /bobf/t_frw_keyindex.

    CLEAR: et_key[], et_fu_item[].

    lo_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
      /scmtms/if_tor_c=>sc_bo_key ).

    lt_altkey     = VALUE #(
      FOR ls_lips IN it_lips
      ( base_btd_tco      = zif_gtt_mia_app_constants=>cs_base_btd_tco-inb_dlv
        base_btd_id       = |{ ls_lips-vbeln ALPHA = IN }|
        base_btditem_id   = |{ ls_lips-posnr ALPHA = IN }|
        base_btd_logsys   = zcl_gtt_tools=>get_logical_system( ) )
    ).

    get_tor_item_selparam_for_lips(
      EXPORTING
        it_lips     = it_lips
      IMPORTING
        et_selparam = lt_selparam ).

    TRY.
        lo_srv_mgr->convert_altern_key(
          EXPORTING
            iv_node_key   = /scmtms/if_tor_c=>sc_node-item_tr
            iv_altkey_key = /scmtms/if_tor_c=>sc_alternative_key-item_tr-base_document
            it_key        = lt_altkey
          IMPORTING
            et_key        = et_key
            et_result     = lt_result ).

        lo_srv_mgr->query(
          EXPORTING
            iv_query_key            = /scmtms/if_tor_c=>sc_query-item_tr-qdb_query_by_attributes
            it_selection_parameters = lt_selparam
          IMPORTING
            et_key                  = lt_key
        ).

        et_key  = VALUE #( BASE et_key
                           ( LINES OF lt_key ) ).

        et_key  = VALUE #( BASE et_key
                          FOR ls_result IN lt_result
                             ( key      = ls_result-key ) ) .


        DELETE et_key WHERE key IS INITIAL.

        SORT et_key BY key.
        DELETE ADJACENT DUPLICATES FROM et_key COMPARING key.

        IF iv_tor_cat IS SUPPLIED.
          filter_tor_items_by_tor_cat(
            EXPORTING
              iv_tor_cat = iv_tor_cat
            CHANGING
              ct_key     = et_key ).
        ENDIF.

        IF et_fu_item IS REQUESTED.
          lo_srv_mgr->retrieve(
            EXPORTING
              iv_node_key  = /scmtms/if_tor_c=>sc_node-item_tr
              it_key       = et_key
              iv_fill_data = abap_true
            IMPORTING
              et_data      = et_fu_item ).
        ENDIF.

      CATCH /bobf/cx_frw_contrct_violation.
    ENDTRY.

  ENDMETHOD.


  METHOD get_tor_item_selparam_for_lips.

    DATA: lv_base_btd_id      TYPE /scmtms/base_btd_id,
          lv_base_btd_item_id TYPE /scmtms/base_btd_item_id.

    CLEAR: et_selparam[].

    LOOP AT it_lips ASSIGNING FIELD-SYMBOL(<ls_lips>).
      lv_base_btd_id        = |{ <ls_lips>-vbeln ALPHA = IN }|.
      lv_base_btd_item_id   = |{ <ls_lips>-posnr ALPHA = IN }|.

      et_selparam = VALUE #( BASE et_selparam
        (
          attribute_name  = /scmtms/if_tor_c=>sc_query_attribute-item_tr-qdb_query_by_attributes-base_btd_id
          low             = lv_base_btd_id
          option          = 'EQ'
          sign            = 'I'
        )
        (
          attribute_name  = /scmtms/if_tor_c=>sc_query_attribute-item_tr-qdb_query_by_attributes-base_btditem_id
          low             = lv_base_btd_item_id
          option          = 'EQ'
          sign            = 'I'
        )
      ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_tor_root_tor_id.

    DATA: lo_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
          lt_tor_root TYPE /scmtms/t_tor_root_k.

    lo_srv_mgr    = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
                      /scmtms/if_tor_c=>sc_bo_key ).

    TRY.
        lo_srv_mgr->retrieve(
          EXPORTING
            iv_node_key             = /scmtms/if_tor_c=>sc_node-root
            it_key                  = VALUE #( ( key = iv_key ) )
            iv_fill_data            = abap_true
            it_requested_attributes = VALUE #( ( /scmtms/if_tor_c=>sc_node_attribute-root-tor_id  ) )
          IMPORTING
            et_data                 = lt_tor_root ).

        rv_tor_id   = VALUE #( lt_tor_root[ 1 ]-tor_id OPTIONAL ).

      CATCH /bobf/cx_frw_contrct_violation.
    ENDTRY.


  ENDMETHOD.


  METHOD is_fu_relevant.

    DATA: lt_key  TYPE /bobf/t_frw_key.

    get_tor_items_for_dlv_items(
      EXPORTING
        it_lips    = it_lips
        iv_tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit
      IMPORTING
        et_key     = lt_key ).

    rv_result   = boolc( lt_key[] IS NOT INITIAL ).

  ENDMETHOD.


  METHOD get_tor_root.
    DATA: lo_srv_mgr  TYPE REF TO /bobf/if_tra_service_manager,
          lt_tor_root TYPE /scmtms/t_tor_root_k.
    CLEAR es_tor.
    lo_srv_mgr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
      /scmtms/if_tor_c=>sc_bo_key ).

    TRY.
        lo_srv_mgr->retrieve(
          EXPORTING
            iv_node_key             = /scmtms/if_tor_c=>sc_node-root
            it_key                  = VALUE #( ( key = iv_key ) )
            iv_fill_data            = abap_true
            it_requested_attributes = VALUE #( ( /scmtms/if_tor_c=>sc_node_attribute-root-tor_id ) )
          IMPORTING
            et_data                 = lt_tor_root ).

        es_tor  = lt_tor_root[ 1 ].

      CATCH /bobf/cx_frw_contrct_violation.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_TP_FACTORY_DLH definition
  public
  inheriting from ZCL_GTT_TP_FACTORY
  create public .

public section.

  methods ZIF_GTT_TP_FACTORY~GET_PE_FILLER
    redefinition .
  methods ZIF_GTT_TP_FACTORY~GET_TP_READER
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_TP_FACTORY_DLH IMPLEMENTATION.


  METHOD zif_gtt_tp_factory~get_pe_filler.

    ro_pe_filler = NEW zcl_gtt_mia_pe_filler_dlh(
      io_ef_parameters = io_ef_parameters
      io_bo_reader     = io_bo_reader ).

  ENDMETHOD.


  METHOD ZIF_GTT_TP_FACTORY~GET_TP_READER.

    ro_bo_reader = NEW zcl_gtt_mia_tp_reader_dlh(
      io_ef_parameters = io_ef_parameters ).

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_TP_FACTORY_DLI definition
  public
  inheriting from ZCL_GTT_TP_FACTORY
  create public .

public section.

  methods ZIF_GTT_TP_FACTORY~GET_PE_FILLER
    redefinition .
  methods ZIF_GTT_TP_FACTORY~GET_TP_READER
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_TP_FACTORY_DLI IMPLEMENTATION.


  METHOD ZIF_GTT_TP_FACTORY~GET_PE_FILLER.

    ro_pe_filler = NEW zcl_gtt_mia_pe_filler_dli(
      io_ef_parameters = io_ef_parameters
      io_bo_reader     = io_bo_reader ).

  ENDMETHOD.


  METHOD ZIF_GTT_TP_FACTORY~GET_TP_READER.

    ro_bo_reader = NEW zcl_gtt_mia_tp_reader_dli(
      io_ef_parameters = io_ef_parameters ).

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_TP_FACTORY_SHH definition
  public
  inheriting from ZCL_GTT_TP_FACTORY
  create public .

public section.

  methods ZIF_GTT_TP_FACTORY~GET_PE_FILLER
    redefinition .
  methods ZIF_GTT_TP_FACTORY~GET_TP_READER
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_TP_FACTORY_SHH IMPLEMENTATION.


  METHOD ZIF_GTT_TP_FACTORY~GET_PE_FILLER.

    ro_pe_filler = NEW zcl_gtt_mia_pe_filler_shh(
      io_ef_parameters = io_ef_parameters
      io_bo_reader     = io_bo_reader ).

  ENDMETHOD.


  METHOD ZIF_GTT_TP_FACTORY~GET_TP_READER.

    ro_bo_reader = NEW zcl_gtt_mia_tp_reader_shh(
      io_ef_parameters = io_ef_parameters ).

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_MIA_TP_READER_DLH definition
  public
  create public .

public section.

  interfaces ZIF_GTT_TP_READER .

  methods CONSTRUCTOR
    importing
      !IO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS .
protected section.

  types tt_otl_locid TYPE STANDARD TABLE OF vbpa-lifnr WITH EMPTY KEY .
  types tt_otl_loctype TYPE STANDARD TABLE OF char30 WITH EMPTY KEY .
  types tt_otl_timezone TYPE STANDARD TABLE OF addr1_data-time_zone WITH EMPTY KEY .
  types tt_otl_description TYPE STANDARD TABLE OF addr1_data-name1 WITH EMPTY KEY .
  types tt_otl_country_code TYPE STANDARD TABLE OF addr1_data-country WITH EMPTY KEY .
  types tt_otl_city_name TYPE STANDARD TABLE OF addr1_data-city1 WITH EMPTY KEY .
  types tt_otl_region_code TYPE STANDARD TABLE OF addr1_data-region WITH EMPTY KEY .
  types tt_otl_house_number TYPE STANDARD TABLE OF addr1_data-house_num1 WITH EMPTY KEY .
  types tt_otl_street_name TYPE STANDARD TABLE OF addr1_data-street WITH EMPTY KEY .
  types tt_otl_postal_code TYPE STANDARD TABLE OF addr1_data-post_code1 WITH EMPTY KEY .
  types tt_otl_email_address TYPE STANDARD TABLE OF ad_smtpadr WITH EMPTY KEY .
  types tt_otl_phone_number TYPE STANDARD TABLE OF char50 WITH EMPTY KEY .

  TYPES:
    BEGIN OF ts_address_info,
      locid     type char20,
      loctype   type char30,
      addr1     TYPE addr1_data,
      email     TYPE ad_smtpadr,
      telephone TYPE char50,
    END OF ts_address_info.
  TYPES:
    tt_address_info type TABLE OF ts_address_info.
private section.

  types:
    BEGIN OF ts_dl_header,
      vbeln             TYPE likp-vbeln,
      lifnr             TYPE likp-lifnr,
      lifnr_lt          TYPE /saptrx/loc_id_type,
      recv_loc          TYPE likp-vstel,
      recv_loc_tp       TYPE /saptrx/loc_id_type,
      bldat             TYPE likp-bldat,
      lfdat             TYPE likp-lfdat,
      lfdat_ts          TYPE timestamp,
      erdat             TYPE timestamp,
      btgew             TYPE likp-btgew,
      ntgew             TYPE likp-ntgew,
      gewei             TYPE likp-gewei,
      volum             TYPE likp-volum,
      voleh             TYPE likp-voleh,
      lgnum             TYPE likp-lgnum,
      lgtor             TYPE likp-lgtor,
      lgnum_txt         TYPE /saptrx/paramval200,
      bolnr             TYPE likp-bolnr,
      proli             TYPE likp-proli,
      incov             TYPE likp-incov,
      inco1             TYPE likp-inco1,
      inco2_l           TYPE likp-inco2_l,
      fu_relev          TYPE abap_bool,
      lifex             TYPE likp-lifex,
      dlv_version       TYPE c LENGTH 4,
      ref_odlv_no       TYPE likp-lifex,
      otl_locid         TYPE tt_otl_locid,
      otl_loctype       TYPE tt_otl_loctype,
      otl_timezone      TYPE tt_otl_timezone,
      otl_description   TYPE tt_otl_description,
      otl_country_code  TYPE tt_otl_country_code,
      otl_city_name     TYPE tt_otl_city_name,
      otl_region_code   TYPE tt_otl_region_code,
      otl_house_number  TYPE tt_otl_house_number,
      otl_street_name   TYPE tt_otl_street_name,
      otl_postal_code   TYPE tt_otl_postal_code,
      otl_email_address TYPE tt_otl_email_address,
      otl_phone_number  TYPE tt_otl_phone_number,
    END OF ts_dl_header .

  constants:
    BEGIN OF cs_mapping,
      " Header section
      vbeln             TYPE /saptrx/paramname VALUE 'YN_DL_DELEVERY',
      lifnr             TYPE /saptrx/paramname VALUE 'YN_DL_VENDOR_ID',
      lifnr_lt          TYPE /saptrx/paramname VALUE 'YN_DL_VENDOR_LOC_TYPE',
      recv_loc          TYPE /saptrx/paramname VALUE 'YN_DL_RECEIVING_LOCATION',
      recv_loc_tp       TYPE /saptrx/paramname VALUE 'YN_DL_RECEIVING_LOC_TYPE',
      bldat             TYPE /saptrx/paramname VALUE 'YN_DL_DOCUMENT_DATE',
      lfdat             TYPE /saptrx/paramname VALUE 'YN_DL_PLANNED_DLV_DATE',
      lfdat_ts          TYPE /saptrx/paramname VALUE 'YN_DL_PLANNED_DLV_DATETIME',
      erdat             TYPE /saptrx/paramname VALUE 'YN_DL_CREATION_DATE',     "MIA
      btgew             TYPE /saptrx/paramname VALUE 'YN_DL_TOTAL_WEIGHT',
      ntgew             TYPE /saptrx/paramname VALUE 'YN_DL_NET_WEIGHT',
      gewei             TYPE /saptrx/paramname VALUE 'YN_DL_WEIGHT_UNITS',
      volum             TYPE /saptrx/paramname VALUE 'YN_DL_VOLUME',
      voleh             TYPE /saptrx/paramname VALUE 'YN_DL_VOLUME_UNITS',
      lgnum             TYPE /saptrx/paramname VALUE 'YN_DL_WAREHOUSE',
      lgnum_txt         TYPE /saptrx/paramname VALUE 'YN_DL_WAREHOUSE_DESC',
      lgtor             TYPE /saptrx/paramname VALUE 'YN_DL_DOOR',
      bolnr             TYPE /saptrx/paramname VALUE 'YN_DL_BILL_OF_LADING',
      proli             TYPE /saptrx/paramname VALUE 'YN_DL_DANGEROUS_GOODS',
      incov             TYPE /saptrx/paramname VALUE 'YN_DL_INCOTERMS_VERSION',
      inco1             TYPE /saptrx/paramname VALUE 'YN_DL_INCOTERMS',
      inco2_l           TYPE /saptrx/paramname VALUE 'YN_DL_INCOTERMS_LOCATION',
      fu_relev          TYPE /saptrx/paramname VALUE 'YN_DL_FU_RELEVANT',
      lifex             TYPE /saptrx/paramname VALUE 'YN_DL_ASN_NUMBER',
      dlv_version       TYPE /saptrx/paramname VALUE 'YN_DL_DELIVERY_VERSION',
      ref_odlv_no       TYPE /saptrx/paramname VALUE 'YN_DL_REFERENCE_ODLV_NO',
      otl_locid         TYPE /saptrx/paramname VALUE 'GTT_OTL_LOCID',
      otl_loctype       TYPE /saptrx/paramname VALUE 'GTT_OTL_LOCTYPE',
      otl_timezone      TYPE /saptrx/paramname VALUE 'GTT_OTL_TIMEZONE',
      otl_description   TYPE /saptrx/paramname VALUE 'GTT_OTL_DESCRIPTION',
      otl_country_code  TYPE /saptrx/paramname VALUE 'GTT_OTL_COUNTRY_CODE',
      otl_city_name     TYPE /saptrx/paramname VALUE 'GTT_OTL_CITY_NAME',
      otl_region_code   TYPE /saptrx/paramname VALUE 'GTT_OTL_REGION_CODE',
      otl_house_number  TYPE /saptrx/paramname VALUE 'GTT_OTL_HOUSE_NUMBER',
      otl_street_name   TYPE /saptrx/paramname VALUE 'GTT_OTL_STREET_NAME',
      otl_postal_code   TYPE /saptrx/paramname VALUE 'GTT_OTL_POSTAL_CODE',
      otl_email_address TYPE /saptrx/paramname VALUE 'GTT_OTL_EMAIL_ADDRESS',
      otl_phone_number  TYPE /saptrx/paramname VALUE 'GTT_OTL_PHONE_NUMBER',
    END OF cs_mapping .
  data MO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS .

  methods FILL_HEADER_FROM_LIKP_STRUCT
    importing
      !IR_LIKP type ref to DATA
    changing
      !CS_DL_HEADER type TS_DL_HEADER
    raising
      CX_UDM_MESSAGE .
  methods FILL_HEADER_FROM_LIPS_TABLE
    importing
      !IR_LIPS type ref to DATA
      !IV_VBELN type VBELN_VL
      !IR_LIKP type ref to DATA
    changing
      !CS_DL_HEADER type TS_DL_HEADER
    raising
      CX_UDM_MESSAGE .
  methods FILL_HEADER_LOCATION_TYPES
    changing
      !CS_DL_HEADER type TS_DL_HEADER .
  methods FORMAT_HEADER_LOCATION_IDS
    changing
      !CS_DL_HEADER type TS_DL_HEADER .
  methods GET_LIKP_STRUCT_OLD
    importing
      !IS_APP_OBJECT type TRXAS_APPOBJ_CTAB_WA
      !IV_VBELN type VBELN_VL
    returning
      value(RR_LIKP) type ref to DATA
    raising
      CX_UDM_MESSAGE .
  methods IS_OBJECT_CHANGED
    importing
      !IS_APP_OBJECT type TRXAS_APPOBJ_CTAB_WA
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  methods ADD_DELIVERY_ITEM_TRACKING
    importing
      !IS_APP_OBJECT type TRXAS_APPOBJ_CTAB_WA
    changing
      !CT_TRACK_ID_DATA type ZIF_GTT_EF_TYPES=>TT_TRACK_ID_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_ONE_TIME_LOCATION
    importing
      !IV_VBELN type VBELN_VL
      !IR_VBPA type ref to DATA
    changing
      !CS_DL_HEADER type TS_DL_HEADER
    raising
      CX_UDM_MESSAGE .
  methods FILL_ONE_TIME_LOCATION_OLD
    importing
      !IV_VBELN type VBELN_VL
      !IR_VBPA type ref to DATA
    changing
      !CS_DL_HEADER type TS_DL_HEADER
    raising
      CX_UDM_MESSAGE .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_MIA_TP_READER_DLH IMPLEMENTATION.


  METHOD ADD_DELIVERY_ITEM_TRACKING.
    DATA:
      lv_dlvittrxcod     TYPE /saptrx/trxcod,
      lr_lips_new        TYPE REF TO data,
      lv_vbeln           TYPE likp-vbeln.

    FIELD-SYMBOLS:
      <lt_lips_new> TYPE zif_gtt_mia_app_types=>tt_lipsvb,
      <ls_lips>     TYPE lipsvb.

    zcl_gtt_tools=>get_field_of_structure(
      EXPORTING
        ir_struct_data = is_app_object-maintabref
        iv_field_name  = 'VBELN'
      RECEIVING
        rv_value       = lv_vbeln ).

    lv_dlvittrxcod = zif_gtt_ef_constants=>cs_trxcod-dl_position.

    lr_lips_new = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-dl_item_new ).
    ASSIGN lr_lips_new->* TO <lt_lips_new>.
    " prepare positions list
    IF <lt_lips_new> IS ASSIGNED.
      " collect NEW records with appropriate item type
      LOOP AT <lt_lips_new> ASSIGNING <ls_lips>
        WHERE vbeln = lv_vbeln AND
              ( updkz = zif_gtt_ef_constants=>cs_change_mode-insert OR
                updkz = zif_gtt_ef_constants=>cs_change_mode-delete ).

        IF zcl_gtt_tools=>is_appropriate_dl_item(
             ir_likp = is_app_object-maintabref
             ir_lips = REF #( <ls_lips> ) ) = abap_true.
          ct_track_id_data = VALUE #( BASE ct_track_id_data (
                    appsys      = mo_ef_parameters->get_appsys( )
                    appobjtype  = is_app_object-appobjtype
                    appobjid    = is_app_object-appobjid
                    trxcod      = lv_dlvittrxcod
                    trxid       = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_item(
                                    ir_lips = NEW lips( vbeln = <ls_lips>-vbeln posnr = <ls_lips>-posnr ) )
                    timzon      = zcl_gtt_tools=>get_system_time_zone( )
                    action      = COND #( WHEN <ls_lips>-updkz = zif_gtt_ef_constants=>cs_change_mode-delete
                                          THEN zif_gtt_ef_constants=>cs_change_mode-delete )
                    msrid       = space
                  ) ).
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    mo_ef_parameters    = io_ef_parameters.

  ENDMETHOD.


  METHOD fill_header_from_likp_struct.

    FIELD-SYMBOLS: <ls_likp>  TYPE likpvb.

    DATA:
      lv_voleh TYPE likp-voleh,
      lv_gewei TYPE likp-gewei.

    ASSIGN ir_likp->* TO <ls_likp>.

    IF <ls_likp> IS ASSIGNED.
      MOVE-CORRESPONDING <ls_likp> TO cs_dl_header.

      cs_dl_header-vbeln = zcl_gtt_mia_dl_tools=>get_formated_dlv_number(
        ir_likp = ir_likp ).

      cs_dl_header-erdat = zcl_gtt_tools=>get_local_timestamp(
        iv_date = <ls_likp>-erdat
        iv_time = <ls_likp>-erzet ).
      cs_dl_header-erdat = zcl_gtt_tools=>convert_datetime_to_utc(
        iv_datetime = cs_dl_header-erdat
        iv_timezone = zcl_gtt_tools=>get_system_time_zone( ) ).

      cs_dl_header-lfdat_ts = zcl_gtt_tools=>get_local_timestamp(
        iv_date = <ls_likp>-lfdat
        iv_time = <ls_likp>-lfuhr ).

      cs_dl_header-lfdat_ts = zcl_gtt_tools=>convert_datetime_to_utc(
        iv_datetime = cs_dl_header-lfdat_ts
        iv_timezone = COND #( WHEN <ls_likp>-tzonrc IS NOT INITIAL
                                   THEN <ls_likp>-tzonrc
                                   ELSE zcl_gtt_tools=>get_system_time_zone( ) ) ).

      cs_dl_header-proli    = boolc( cs_dl_header-proli IS NOT INITIAL ).
      cs_dl_header-recv_loc = <ls_likp>-vstel."Shipping Point / Receiving Point
      CLEAR:cs_dl_header-lifex.
      IF <ls_likp>-spe_lifex_type = /spe/cl_dlv_doc=>c_lifex_type-vendor.  "External Identification from Vendor
        cs_dl_header-lifex = <ls_likp>-lifex.      "External ASN Id
      ELSEIF <ls_likp>-spe_lifex_type = /spe/cl_dlv_doc=>c_lifex_type-sto. "Stock Transfer: Number of Preceding Outbound Delivery
        cs_dl_header-ref_odlv_no = <ls_likp>-lifex."Reference Outbound Delivery No.
        SHIFT cs_dl_header-ref_odlv_no LEFT DELETING LEADING '0'.
      ENDIF.
      IF <ls_likp>-lgnum IS NOT INITIAL AND
         <ls_likp>-lgtor IS NOT INITIAL.

        TRY.
            cs_dl_header-lgnum_txt = zcl_gtt_mia_dl_tools=>get_door_description(
              EXPORTING
                iv_lgnum = <ls_likp>-lgnum
                iv_lgtor = <ls_likp>-lgtor ).
          CATCH cx_udm_message.
        ENDTRY.
      ENDIF.

      zcl_gtt_tools=>convert_unit_output(
        EXPORTING
          iv_input  = cs_dl_header-voleh
        RECEIVING
          rv_output = lv_voleh ).

      zcl_gtt_tools=>convert_unit_output(
        EXPORTING
          iv_input  = cs_dl_header-gewei
        RECEIVING
          rv_output = lv_gewei ).

      cs_dl_header-voleh = lv_voleh.
      cs_dl_header-gewei = lv_gewei.

    ELSE.
      MESSAGE e002(zgtt) WITH 'LIKP' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_header_from_lips_table.

    FIELD-SYMBOLS:
      <lt_lips> TYPE va_lipsvb_t,
      <ls_likp> TYPE likpvb.

    DATA:
      lv_dummy TYPE char100,
      lt_lips  TYPE va_lipsvb_t.

    ASSIGN ir_likp->* TO <ls_likp>.
    ASSIGN ir_lips->* TO <lt_lips>.

    IF <ls_likp> IS NOT ASSIGNED OR <lt_lips> IS NOT ASSIGNED.
      MESSAGE e002(zgtt) WITH 'LIKP LIPS' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

    LOOP AT <lt_lips> ASSIGNING FIELD-SYMBOL(<ls_lips>)
      WHERE vbeln = iv_vbeln.
      APPEND <ls_lips> TO lt_lips.
    ENDLOOP.

    zcl_gtt_tools=>check_tm_int_relevance(
      EXPORTING
        iv_ctrl_key = <ls_likp>-tm_ctrl_key
        it_lips     = lt_lips
      RECEIVING
        rv_relevant = cs_dl_header-fu_relev ).

  ENDMETHOD.


  METHOD FILL_HEADER_LOCATION_TYPES.

    cs_dl_header-lifnr_lt   = zif_gtt_ef_constants=>cs_loc_types-businesspartner.

    IF cs_dl_header-recv_loc IS NOT INITIAL.
      cs_dl_header-recv_loc_tp = zif_gtt_ef_constants=>cs_loc_types-shippingpoint.
    ENDIF.

  ENDMETHOD.


  METHOD FORMAT_HEADER_LOCATION_IDS.
    cs_dl_header-lifnr  = zcl_gtt_tools=>get_pretty_location_id(
                            iv_locid   = cs_dl_header-lifnr
                            iv_loctype = cs_dl_header-lifnr_lt ).

    cs_dl_header-recv_loc = zcl_gtt_tools=>get_pretty_location_id(
                            iv_locid   = cs_dl_header-recv_loc
                            iv_loctype = cs_dl_header-recv_loc_tp ).
  ENDMETHOD.


  METHOD GET_LIKP_STRUCT_OLD.

    " when header is unchanged, table 'DELIVERY_HEADER_OLD' is not populated
    " so maintab record is used as data source for header data
    TYPES: tt_likp TYPE STANDARD TABLE OF likpvb.

    FIELD-SYMBOLS: <lt_likp> TYPE tt_likp,
                   <ls_likp> TYPE likpvb.

    DATA(lr_likp)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-dl_header_old ).

    ASSIGN lr_likp->* TO <lt_likp>.

    IF <lt_likp> IS ASSIGNED.
      READ TABLE <lt_likp> ASSIGNING <ls_likp>
        WITH KEY vbeln = iv_vbeln.

      rr_likp   = COND #( WHEN sy-subrc = 0
                            THEN REF #( <ls_likp> )
                            ELSE is_app_object-maintabref ).
    ELSE.
      MESSAGE e002(zgtt) WITH 'LIKP' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD IS_OBJECT_CHANGED.

    rv_result   = zcl_gtt_tools=>is_object_changed(
                    is_app_object    = is_app_object
                    io_ef_parameters = mo_ef_parameters
                    iv_key_field     = 'VBELN'
                    iv_upd_field     = 'UPDKZ' ).

  ENDMETHOD.


  METHOD ZIF_GTT_TP_READER~CHECK_RELEVANCE.

    rv_result   = zif_gtt_ef_constants=>cs_condition-false.

    IF zcl_gtt_tools=>is_appropriate_dl_type( ir_likp = is_app_object-maintabref ) = abap_true AND
       is_object_changed( is_app_object = is_app_object ) = abap_true.

      CASE is_app_object-update_indicator.
        WHEN zif_gtt_ef_constants=>cs_change_mode-insert.
          rv_result   = zif_gtt_ef_constants=>cs_condition-true.
        WHEN zif_gtt_ef_constants=>cs_change_mode-update OR
             zif_gtt_ef_constants=>cs_change_mode-undefined.
          rv_result   = zcl_gtt_tools=>are_structures_different(
                          ir_data1  = zif_gtt_tp_reader~get_data(
                                        is_app_object = is_app_object )
                          ir_data2  = zif_gtt_tp_reader~get_data_old(
                                        is_app_object = is_app_object ) ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_GTT_TP_READER~GET_APP_OBJ_TYPE_ID.
    rv_appobjid   = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
                        ir_likp = is_app_object-maintabref ).
  ENDMETHOD.


  METHOD zif_gtt_tp_reader~get_data.

    FIELD-SYMBOLS: <ls_header> TYPE ts_dl_header.

    DATA:
      lr_lips  TYPE REF TO data.

    rr_data   = NEW ts_dl_header( ).

    ASSIGN rr_data->* TO <ls_header>.

    lr_lips = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-dl_item_new ).

    fill_header_from_likp_struct(
      EXPORTING
        ir_likp      = is_app_object-maintabref
      CHANGING
        cs_dl_header = <ls_header> ).

    fill_header_from_lips_table(
      EXPORTING
        ir_lips      = lr_lips
        iv_vbeln     = |{ <ls_header>-vbeln ALPHA = IN }|
        ir_likp      = is_app_object-maintabref
      CHANGING
        cs_dl_header = <ls_header> ).

    fill_header_location_types(
      CHANGING
        cs_dl_header = <ls_header> ).

    format_header_location_ids(
      CHANGING
        cs_dl_header = <ls_header> ).

*   Support one-time location(Supplier)
    fill_one_time_location(
      EXPORTING
        iv_vbeln     = |{ <ls_header>-vbeln ALPHA = IN }|
        ir_vbpa      = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-dl_partners_new )
      CHANGING
        cs_dl_header = <ls_header> ).

    IF <ls_header>-otl_locid IS INITIAL.
      APPEND INITIAL LINE TO <ls_header>-otl_locid.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_tp_reader~get_data_old.
    FIELD-SYMBOLS: <ls_header> TYPE ts_dl_header.

    DATA:
      lr_likp TYPE REF TO data,
      lr_lips TYPE REF TO data.

    DATA(lv_vbeln)  = CONV vbeln_vl( zcl_gtt_tools=>get_field_of_structure(
                                       ir_struct_data = is_app_object-maintabref
                                       iv_field_name  = 'VBELN' ) ).

    lr_likp = get_likp_struct_old(
      is_app_object = is_app_object
      iv_vbeln      = lv_vbeln ).

    lr_lips = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-dl_item_old ).

    rr_data   = NEW ts_dl_header( ).

    ASSIGN rr_data->* TO <ls_header>.

    fill_header_from_likp_struct(
      EXPORTING
        ir_likp      = lr_likp
      CHANGING
        cs_dl_header = <ls_header> ).

    fill_header_from_lips_table(
      EXPORTING
        ir_lips      = lr_lips
        iv_vbeln     = |{ <ls_header>-vbeln ALPHA = IN }|
        ir_likp      = lr_likp
      CHANGING
        cs_dl_header = <ls_header> ).

    fill_header_location_types(
      CHANGING
        cs_dl_header = <ls_header> ).

    format_header_location_ids(
      CHANGING
        cs_dl_header = <ls_header> ).

*   Support one-time location(Supplier)
    fill_one_time_location_old(
      EXPORTING
        iv_vbeln     = |{ <ls_header>-vbeln ALPHA = IN }|
        ir_vbpa      = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_mia_app_constants=>cs_tabledef-dl_partners_old )
      CHANGING
        cs_dl_header = <ls_header> ).

    IF <ls_header>-otl_locid IS INITIAL.
      APPEND INITIAL LINE TO <ls_header>-otl_locid.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_GTT_TP_READER~GET_FIELD_PARAMETER.

    CLEAR: rv_result.

  ENDMETHOD.


  METHOD ZIF_GTT_TP_READER~GET_MAPPING_STRUCTURE.

    rr_data   = REF #( cs_mapping ).

  ENDMETHOD.


  METHOD zif_gtt_tp_reader~get_track_id_data.
    " another tip is that: for tracking ID type 'SHIPMENT_ORDER' of delivery header,
    " and for tracking ID type 'RESOURCE' of shipment header,
    " DO NOT enable START DATE and END DATE

    DATA:lv_dlvhdtrxcod     TYPE /saptrx/trxcod.

    lv_dlvhdtrxcod = zif_gtt_ef_constants=>cs_trxcod-dl_number.
    CLEAR et_track_id_data.

    et_track_id_data = VALUE #( (
        appsys      = mo_ef_parameters->get_appsys( )
        appobjtype  = is_app_object-appobjtype
        appobjid    = is_app_object-appobjid
        trxcod      = lv_dlvhdtrxcod
        trxid       = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
                        ir_likp = is_app_object-maintabref )
        timzon      = zcl_gtt_tools=>get_system_time_zone( )
        msrid       = space
      ) ).

    add_delivery_item_tracking(
      EXPORTING
        is_app_object    = is_app_object
      CHANGING
        ct_track_id_data = et_track_id_data
    ).

  ENDMETHOD.


  METHOD fill_one_time_location.

    FIELD-SYMBOLS:
      <ls_likp> TYPE likpvb,
      <lt_vbpa> TYPE vbpavb_tab,
      <ls_vbpa> TYPE vbpavb.

    DATA:
      lv_dummy         TYPE char100,
      ls_loc_addr      TYPE addr1_data,
      lv_loc_email     TYPE ad_smtpadr,
      lv_loc_tel       TYPE char50,
      ls_address_info  TYPE ts_address_info,
      lt_address_info  TYPE tt_address_info,
      lt_vttsvb        TYPE vttsvb_tab,
      ls_loc_addr_tmp  TYPE addr1_data,
      lv_loc_email_tmp TYPE ad_smtpadr,
      lv_loc_tel_tmp   TYPE char50.

*   Get one-time location from shipment
    zcl_gtt_tools=>get_stage_by_delivery(
      EXPORTING
        iv_vbeln  = iv_vbeln
      IMPORTING
        et_vttsvb = lt_vttsvb ).

    zcl_gtt_tools=>get_location_info(
      EXPORTING
        it_vttsvb   = lt_vttsvb
      IMPORTING
        et_loc_info = DATA(lt_loc_info) ).

    LOOP AT lt_loc_info INTO DATA(ls_loc_info).
      CLEAR:
       ls_loc_addr,
       lv_loc_email,
       lv_loc_tel,
       ls_loc_addr_tmp,
       lv_loc_email_tmp,
       lv_loc_tel_tmp.

      IF ls_loc_info-locaddrnum CN '0 ' AND ls_loc_info-locindicator CA zif_gtt_ef_constants=>shp_addr_ind_man_all.

        zcl_gtt_tools=>get_address_from_db(
          EXPORTING
            iv_addrnumber = ls_loc_info-locaddrnum
          IMPORTING
            es_addr       = ls_loc_addr
            ev_email      = lv_loc_email
            ev_telephone  = lv_loc_tel ).

        zcl_gtt_tools=>get_address_detail_by_loctype(
          EXPORTING
            iv_loctype   = ls_loc_info-loctype
            iv_locid     = ls_loc_info-locid
          IMPORTING
            es_addr      = ls_loc_addr_tmp
            ev_email     = lv_loc_email_tmp
            ev_telephone = lv_loc_tel_tmp ).

        IF ls_loc_addr <> ls_loc_addr_tmp
          OR lv_loc_email <> lv_loc_email_tmp
          OR lv_loc_tel <> lv_loc_tel_tmp.
          ls_address_info-locid = ls_loc_info-locid.
          ls_address_info-loctype = ls_loc_info-loctype.
          ls_address_info-addr1 = ls_loc_addr.
          ls_address_info-email = lv_loc_email.
          ls_address_info-telephone = lv_loc_tel.
          APPEND ls_address_info TO lt_address_info.
        ENDIF.
        CLEAR:
          ls_address_info.
      ENDIF.
    ENDLOOP.

    CLEAR:ls_address_info.

*   Get one-time location from delivery itself(supplier)
    ASSIGN ir_vbpa->* TO <lt_vbpa>.
    IF <lt_vbpa> IS ASSIGNED.
      READ TABLE <lt_vbpa> ASSIGNING <ls_vbpa> WITH KEY vbeln = iv_vbeln
                                                        posnr = '000000'
                                                        parvw = zif_gtt_mia_app_constants=>cs_parvw-supplier.
      IF sy-subrc = 0 AND <ls_vbpa> IS ASSIGNED AND <ls_vbpa> IS NOT INITIAL
        AND <ls_vbpa>-adrnr CN '0 ' AND <ls_vbpa>-adrda CA zif_gtt_ef_constants=>vbpa_addr_ind_man_all.

*      For same one-Time location id and location type which exists in delivey and shipment,
*      use the shipment's address as one-Time location address
        READ TABLE lt_address_info TRANSPORTING NO FIELDS
          WITH KEY locid = |{ <ls_vbpa>-lifnr ALPHA = OUT }|
                   loctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
        IF sy-subrc <> 0.

          zcl_gtt_tools=>get_address_from_memory(
            EXPORTING
              iv_addrnumber = <ls_vbpa>-adrnr
            IMPORTING
              es_addr       = ls_loc_addr
              ev_email      = lv_loc_email
              ev_telephone  = lv_loc_tel ).

          ls_address_info-locid = |{ <ls_vbpa>-lifnr ALPHA = OUT }|.
          ls_address_info-loctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
          ls_address_info-addr1 = ls_loc_addr.
          ls_address_info-email = lv_loc_email.
          ls_address_info-telephone = lv_loc_tel.
          APPEND ls_address_info TO lt_address_info.
          CLEAR:
            ls_address_info.
        ENDIF.
      ENDIF.

    ELSE.
      MESSAGE e002(zgtt) WITH 'VBPA' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

    LOOP AT lt_address_info INTO ls_address_info.
      APPEND ls_address_info-locid TO cs_dl_header-otl_locid.
      APPEND ls_address_info-loctype TO cs_dl_header-otl_loctype.
      APPEND ls_address_info-addr1-time_zone TO cs_dl_header-otl_timezone.
      APPEND ls_address_info-addr1-name1 TO cs_dl_header-otl_description.
      APPEND ls_address_info-addr1-country TO cs_dl_header-otl_country_code.
      APPEND ls_address_info-addr1-city1 TO cs_dl_header-otl_city_name.
      APPEND ls_address_info-addr1-region TO cs_dl_header-otl_region_code.
      APPEND ls_address_info-addr1-house_num1 TO cs_dl_header-otl_house_number.
      APPEND ls_address_info-addr1-street TO cs_dl_header-otl_street_name.
      APPEND ls_address_info-addr1-post_code1 TO cs_dl_header-otl_postal_code.
      APPEND ls_address_info-email TO cs_dl_header-otl_email_address.
      APPEND ls_address_info-telephone TO cs_dl_header-otl_phone_number.
      CLEAR:
        ls_address_info.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_one_time_location_old.

    FIELD-SYMBOLS:
      <ls_likp> TYPE likpvb,
      <lt_vbpa> TYPE vbpavb_tab,
      <ls_vbpa> TYPE vbpavb.

    DATA:
      lv_dummy         TYPE char100,
      ls_loc_addr      TYPE addr1_data,
      lv_loc_email     TYPE ad_smtpadr,
      lv_loc_tel       TYPE char50,
      ls_address_info  TYPE ts_address_info,
      lt_address_info  TYPE tt_address_info,
      lt_vttsvb        TYPE vttsvb_tab,
      ls_loc_addr_tmp  TYPE addr1_data,
      lv_loc_email_tmp TYPE ad_smtpadr,
      lv_loc_tel_tmp   TYPE char50.

*   Get one-time location from shipment
    zcl_gtt_tools=>get_stage_by_delivery(
      EXPORTING
        iv_vbeln  = iv_vbeln
      IMPORTING
        et_vttsvb = lt_vttsvb ).

    zcl_gtt_tools=>get_location_info(
      EXPORTING
        it_vttsvb   = lt_vttsvb
      IMPORTING
        et_loc_info = DATA(lt_loc_info) ).

    LOOP AT lt_loc_info INTO DATA(ls_loc_info).
      CLEAR:
       ls_loc_addr,
       lv_loc_email,
       lv_loc_tel,
       ls_loc_addr_tmp,
       lv_loc_email_tmp,
       lv_loc_tel_tmp.

      IF ls_loc_info-locaddrnum CN '0 ' AND ls_loc_info-locindicator CA zif_gtt_ef_constants=>shp_addr_ind_man_all.

        zcl_gtt_tools=>get_address_from_db(
          EXPORTING
            iv_addrnumber = ls_loc_info-locaddrnum
          IMPORTING
            es_addr       = ls_loc_addr
            ev_email      = lv_loc_email
            ev_telephone  = lv_loc_tel ).

        zcl_gtt_tools=>get_address_detail_by_loctype(
          EXPORTING
            iv_loctype   = ls_loc_info-loctype
            iv_locid     = ls_loc_info-locid
          IMPORTING
            es_addr      = ls_loc_addr_tmp
            ev_email     = lv_loc_email_tmp
            ev_telephone = lv_loc_tel_tmp ).

        IF ls_loc_addr <> ls_loc_addr_tmp
          OR lv_loc_email <> lv_loc_email_tmp
          OR lv_loc_tel <> lv_loc_tel_tmp.

          ls_address_info-locid = ls_loc_info-locid.
          ls_address_info-loctype = ls_loc_info-loctype.
          ls_address_info-addr1 = ls_loc_addr.
          ls_address_info-email = lv_loc_email.
          ls_address_info-telephone = lv_loc_tel.
          APPEND ls_address_info TO lt_address_info.
        ENDIF.
        CLEAR:
          ls_address_info.
      ENDIF.
    ENDLOOP.

    CLEAR:ls_address_info.

*   Get one-time location from delivery itself(supplier)
    ASSIGN ir_vbpa->* TO <lt_vbpa>.
    IF <lt_vbpa> IS ASSIGNED.
      READ TABLE <lt_vbpa> ASSIGNING <ls_vbpa> WITH KEY vbeln = iv_vbeln
                                                        posnr = '000000'
                                                        parvw = zif_gtt_mia_app_constants=>cs_parvw-supplier.
      IF sy-subrc = 0 AND <ls_vbpa> IS ASSIGNED AND <ls_vbpa> IS NOT INITIAL
        AND <ls_vbpa>-adrnr CN '0 ' AND <ls_vbpa>-adrda CA zif_gtt_ef_constants=>vbpa_addr_ind_man_all.

*      For same one-Time location id and location type which exists in delivey and shipment,
*      use the shipment's address as one-Time location address
        READ TABLE lt_address_info TRANSPORTING NO FIELDS
          WITH KEY locid = |{ <ls_vbpa>-lifnr ALPHA = OUT }|
                   loctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
        IF sy-subrc <> 0.
          zcl_gtt_tools=>get_address_from_db(
            EXPORTING
              iv_addrnumber = <ls_vbpa>-adrnr
            IMPORTING
              es_addr       = ls_loc_addr
              ev_email      = lv_loc_email
              ev_telephone  = lv_loc_tel ).

          ls_address_info-locid = |{ <ls_vbpa>-lifnr ALPHA = OUT }|.
          ls_address_info-loctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
          ls_address_info-addr1 = ls_loc_addr.
          ls_address_info-email = lv_loc_email.
          ls_address_info-telephone = lv_loc_tel.
          APPEND ls_address_info TO lt_address_info.
          CLEAR:
            ls_address_info.
        ENDIF.
      ENDIF.

    ELSE.
      MESSAGE e002(zgtt) WITH 'VBPA' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

    LOOP AT lt_address_info INTO ls_address_info.
      APPEND ls_address_info-locid TO cs_dl_header-otl_locid.
      APPEND ls_address_info-loctype TO cs_dl_header-otl_loctype.
      APPEND ls_address_info-addr1-time_zone TO cs_dl_header-otl_timezone.
      APPEND ls_address_info-addr1-name1 TO cs_dl_header-otl_description.
      APPEND ls_address_info-addr1-country TO cs_dl_header-otl_country_code.
      APPEND ls_address_info-addr1-city1 TO cs_dl_header-otl_city_name.
      APPEND ls_address_info-addr1-region TO cs_dl_header-otl_region_code.
      APPEND ls_address_info-addr1-house_num1 TO cs_dl_header-otl_house_number.
      APPEND ls_address_info-addr1-street TO cs_dl_header-otl_street_name.
      APPEND ls_address_info-addr1-post_code1 TO cs_dl_header-otl_postal_code.
      APPEND ls_address_info-email TO cs_dl_header-otl_email_address.
      APPEND ls_address_info-telephone TO cs_dl_header-otl_phone_number.
      CLEAR:
        ls_address_info.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""FUNCTION zgtt_mia_aoid_dl_hdr.
      TRY.
      e_appobjid = zcl_gtt_ef_performer=>get_app_obj_type_id(
        EXPORTING
          is_definition         = VALUE #( maintab = zif_gtt_mia_app_constants=>cs_tabledef-dl_header_new )
          io_tp_factory         = NEW zcl_gtt_mia_tp_factory_dlh( )
          iv_appsys             = i_appsys
          is_app_obj_types      = i_app_obj_types
          it_all_appl_tables    = i_all_appl_tables
          it_app_objects        = VALUE #( ( i_app_object ) ) ).

    CATCH cx_udm_message.
  ENDTRY.
ENDFUNCTION.""",
    r"""FUNCTION zgtt_mia_aoid_sh_hdr.
      TRY.
      e_appobjid = zcl_gtt_ef_performer=>get_app_obj_type_id(
        EXPORTING
          is_definition         = VALUE #( maintab = zif_gtt_mia_app_constants=>cs_tabledef-sh_header_new )
          io_tp_factory         = NEW zcl_gtt_mia_tp_factory_shh( )
          iv_appsys             = i_appsys
          is_app_obj_types      = i_app_obj_types
          it_all_appl_tables    = i_all_appl_tables
          it_app_objects        = VALUE #( ( i_app_object ) ) ).

    CATCH cx_udm_message.
  ENDTRY.


ENDFUNCTION.""",
    r"""FUNCTION zgtt_mia_ctp_tor_to_dl.
      TRY.

      " *** collect F.O./F.U. changes that affects inbound deliveries ***
      DATA(lo_tor_changes)  = NEW zcl_gtt_mia_ctp_tor_changes(
                                it_tor_root_sstring        = it_tor_root_sstring
                                it_tor_root_before_sstring = it_tor_root_before_sstring
                                it_tor_item_sstring        = it_tor_item_sstring
                                it_tor_item_before_sstring = it_tor_item_before_sstring
                                it_tor_stop_sstring        = it_tor_stop_sstring
                                it_tor_stop_addr_sstring   = it_tor_stop_addr_sstring ).

      FIELD-SYMBOLS: <lt_delivery_chng> TYPE zif_gtt_mia_ctp_types=>tt_delivery_chng.

      DATA(lr_delivery_chng)  = lo_tor_changes->get_delivery_items( ).

      ASSIGN lr_delivery_chng->* TO <lt_delivery_chng>.

      IF <lt_delivery_chng> IS ASSIGNED AND <lt_delivery_chng> IS NOT INITIAL.

        " *** prepare and send DLV Header IDOC ***
        DATA(lo_dl_head_data) = NEW zcl_gtt_mia_ctp_dat_tor_to_dlh(
                                  it_delivery_chng = <lt_delivery_chng>
                                  it_tor_root      = it_tor_root_sstring
                                  it_tor_item      = it_tor_item_sstring ).

        DATA(lo_dl_head_sender) = zcl_gtt_mia_ctp_snd_tor_to_dlh=>get_instance( ).

        lo_dl_head_sender->prepare_idoc_data( io_dl_head_data = lo_dl_head_data ).

        lo_dl_head_sender->send_idoc_data( ).

        " *** prepare and send DLV Item IDOC ***
        DATA(lo_dlv_item_data) = NEW zcl_gtt_mia_ctp_dat_tor_to_dli(
                                   it_delivery_chng = <lt_delivery_chng> ).

        DATA(lo_dlv_item_sender) = zcl_gtt_mia_ctp_snd_tor_to_dli=>get_instance( ).

        lo_dlv_item_sender->prepare_idoc_data( io_dl_item_data = lo_dlv_item_data ).

        lo_dlv_item_sender->send_idoc_data( ).
      ENDIF.

    CATCH cx_udm_message INTO DATA(lo_udm_message).
      zcl_gtt_tools=>log_exception(
        EXPORTING
          io_udm_message = lo_udm_message
          iv_object      = zif_gtt_ef_constants=>cs_logs-object-tor_ctp
          iv_subobject   = zif_gtt_ef_constants=>cs_logs-subobject-tor_ctp ).
    CATCH cx_root.
  ENDTRY.

ENDFUNCTION.""",
    r"""FUNCTION zgtt_mia_ee_dl_hdr_gr.
      DATA: lo_udm_message TYPE REF TO cx_udm_message,
        ls_bapiret     TYPE bapiret2.

  TRY.
      zcl_gtt_ae_performer=>get_event_data(
        EXPORTING
          is_definition           = VALUE #( maintab   = zif_gtt_mia_app_constants=>cs_tabledef-md_material_header )
          io_ae_factory           = NEW zcl_gtt_mia_ae_factory_dlh_gr( )
          iv_appsys               = i_appsys
          is_event_type           = i_event_type
          it_all_appl_tables      = i_all_appl_tables
          it_event_type_cntl_tabs = i_event_type_cntl_tabs
          it_events               = i_events
        CHANGING
          ct_eventid_map          = c_eventid_map[]
          ct_trackingheader       = ct_trackingheader[]
          ct_tracklocation        = ct_tracklocation[]
          ct_trackreferences      = ct_trackreferences[]
          ct_trackparameters      = ct_trackparameters[] ).

    CATCH cx_udm_message INTO lo_udm_message.
      zcl_gtt_tools=>get_errors_log(
        EXPORTING
          io_umd_message = lo_udm_message
          iv_appsys      = i_appsys
        IMPORTING
          es_bapiret     = ls_bapiret ).

      " add error message
      APPEND ls_bapiret TO ct_logtable.

      " throw corresponding exception
      CASE lo_udm_message->textid.
        WHEN zif_gtt_ef_constants=>cs_errors-stop_processing.
          RAISE stop_processing.
        WHEN zif_gtt_ef_constants=>cs_errors-table_determination.
          RAISE event_data_error.
      ENDCASE.
  ENDTRY.

ENDFUNCTION.""",
    r"""FUNCTION zgtt_mia_ee_dl_item.
      DATA: lo_udm_message TYPE REF TO cx_udm_message,
        ls_bapiret     TYPE bapiret2.

  CLEAR e_logtable[].

  TRY.
      zcl_gtt_ef_performer=>get_planned_events(
        EXPORTING
          is_definition         = VALUE #(
                                    maintab   = zif_gtt_mia_app_constants=>cs_tabledef-dl_item_new
                                    mastertab = zif_gtt_mia_app_constants=>cs_tabledef-dl_header_new )
          io_tp_factory            = NEW zcl_gtt_mia_tp_factory_dli( )
          iv_appsys             = i_appsys
          is_app_obj_types      = i_app_obj_types
          it_all_appl_tables    = i_all_appl_tables
          it_app_type_cntl_tabs = i_app_type_cntl_tabs
          it_app_objects        = i_app_objects
        CHANGING
          ct_expeventdata       = e_expeventdata[]
          ct_measrmntdata       = e_measrmntdata[]
          ct_infodata           = e_infodata[] ).

    CATCH cx_udm_message INTO lo_udm_message.
      zcl_gtt_tools=>get_errors_log(
        EXPORTING
          io_umd_message = lo_udm_message
          iv_appsys      = i_appsys
        IMPORTING
          es_bapiret     = ls_bapiret ).

      " add error message
      APPEND ls_bapiret TO e_logtable.

      " throw corresponding exception
      CASE lo_udm_message->textid.
        WHEN zif_gtt_ef_constants=>cs_errors-stop_processing.
          RAISE stop_processing.
        WHEN zif_gtt_ef_constants=>cs_errors-table_determination.
          RAISE table_determination_error.
      ENDCASE.
  ENDTRY.
ENDFUNCTION.""",
    r"""  FUNCTION zgtt_mia_ee_dl_item_pkng_rel.
    DATA: lo_udm_message TYPE REF TO cx_udm_message,
        ls_bapiret     TYPE bapiret2.

  TRY.
      e_result  = zcl_gtt_ae_performer=>check_relevance(
        EXPORTING
          is_definition       = VALUE #(
                                  maintab   = zif_gtt_mia_app_constants=>cs_tabledef-dl_item_new
                                  mastertab = zif_gtt_mia_app_constants=>cs_tabledef-dl_header_new )
          io_ae_factory       = NEW zcl_gtt_mia_ae_factory_dli_pa( )
          iv_appsys           = i_appsys
          is_event_type       = i_event_types
          it_all_appl_tables  = i_all_appl_tables
          is_events           = i_event
      ).

    CATCH cx_udm_message INTO lo_udm_message.
      zcl_gtt_tools=>get_errors_log(
        EXPORTING
          io_umd_message = lo_udm_message
          iv_appsys      = i_appsys
        IMPORTING
          es_bapiret     = ls_bapiret ).

      " add error message
      APPEND ls_bapiret TO c_logtable.

      " throw corresponding exception
      CASE lo_udm_message->textid.
        WHEN zif_gtt_ef_constants=>cs_errors-stop_processing.
          RAISE stop_processing.
        WHEN zif_gtt_ef_constants=>cs_errors-table_determination.
          RAISE relevance_determ_error.
      ENDCASE.
  ENDTRY.

ENDFUNCTION.""",
    r"""FUNCTION zgtt_mia_ote_sh_hdr_rel.
      DATA: lt_app_objects TYPE trxas_appobj_ctabs,
        lo_udm_message TYPE REF TO cx_udm_message,
        ls_bapiret     TYPE bapiret2.

  lt_app_objects  = VALUE #( ( i_app_object ) ).

  TRY.
      e_result  = zcl_gtt_ef_performer=>check_relevance(
                    is_definition         = VALUE #( maintab = zif_gtt_mia_app_constants=>cs_tabledef-sh_header_new )
                    io_tp_factory         = NEW zcl_gtt_mia_tp_factory_shh( )
                    iv_appsys             = i_appsys
                    is_app_obj_types      = i_app_obj_types
                    it_all_appl_tables    = i_all_appl_tables
                    it_app_objects        = lt_app_objects ).

    CATCH cx_udm_message INTO lo_udm_message.
      zcl_gtt_tools=>get_errors_log(
        EXPORTING
          io_umd_message = lo_udm_message
          iv_appsys      = i_appsys
        IMPORTING
          es_bapiret     = ls_bapiret ).

      " add error message
      APPEND ls_bapiret TO c_logtable.

      " throw corresponding exception
      CASE lo_udm_message->textid.
        WHEN zif_gtt_ef_constants=>cs_errors-stop_processing.
          RAISE stop_processing.
        WHEN zif_gtt_ef_constants=>cs_errors-table_determination.
          RAISE parameter_error.
      ENDCASE.
  ENDTRY.
ENDFUNCTION.""",
    r"""FUNCTION zgtt_mia_ote_sh_hdr_tid.
      DATA: lo_udm_message TYPE REF TO cx_udm_message,
        ls_bapiret     TYPE bapiret2.

  TRY.
      zcl_gtt_ef_performer=>get_track_id_data(
        EXPORTING
          is_definition         = VALUE #( maintab = zif_gtt_mia_app_constants=>cs_tabledef-sh_header_new )
          io_tp_factory         = NEW zcl_gtt_mia_tp_factory_shh( )
          iv_appsys             = i_appsys
          is_app_obj_types      = i_app_obj_types
          it_all_appl_tables    = i_all_appl_tables
          it_app_type_cntl_tabs = i_app_type_cntl_tabs
          it_app_objects        = i_app_objects
        IMPORTING
          et_track_id_data      = e_trackiddata[]
      ).

    CATCH cx_udm_message INTO lo_udm_message.
      zcl_gtt_tools=>get_errors_log(
        EXPORTING
          io_umd_message = lo_udm_message
          iv_appsys      = i_appsys
        IMPORTING
          es_bapiret     = ls_bapiret ).

      " add error message
      APPEND ls_bapiret TO e_logtable.

      " throw corresponding exception
      CASE lo_udm_message->textid.
        WHEN zif_gtt_ef_constants=>cs_errors-stop_processing.
          RAISE stop_processing.
        WHEN zif_gtt_ef_constants=>cs_errors-table_determination.
          RAISE table_determination_error.
      ENDCASE.
  ENDTRY.
ENDFUNCTION.""",
    r"""INTERFACE zif_gtt_mia_app_constants
  PUBLIC .


  CONSTANTS:
    BEGIN OF cs_tabledef,
      md_material_header  TYPE /saptrx/strucdatadef VALUE 'MATERIAL_HEADER',
      md_material_segment TYPE /saptrx/strucdatadef VALUE 'MATERIAL_SEGMENT',
      md_update_control   TYPE /saptrx/strucdatadef VALUE 'UPDATE_CONTROL',
      dl_header_new       TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HEADER_NEW',
      dl_header_old       TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HEADER_OLD',
      dl_hdr_status_new   TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HDR_STATUS_NEW',
      dl_hdr_status_old   TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HDR_STATUS_OLD',
      dl_item_new         TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_NEW',
      dl_item_old         TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_OLD',
      dl_partners_new     TYPE /saptrx/strucdatadef VALUE 'PARTNERS_NEW',
      dl_partners_old     TYPE /saptrx/strucdatadef VALUE 'PARTNERS_OLD',
      dl_itm_status_new   TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_STATUS_NEW',
      dl_itm_status_old   TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_STATUS_OLD',
      dl_hu_item_new      TYPE /saptrx/strucdatadef VALUE 'HU_ITEM_NEW',
      dl_hu_item_old      TYPE /saptrx/strucdatadef VALUE 'HU_ITEM_OLD',
      sh_header_new       TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_HEADER_NEW',
      sh_header_old       TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_HEADER_OLD',
      sh_item_new         TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_ITEM_NEW',
      sh_item_old         TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_ITEM_OLD',
      sh_stage_new        TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_LEG_NEW',
      sh_stage_old        TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_LEG_OLD',
      sh_item_stage_new   TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_ITEM_LEG_NEW',
      sh_item_stage_old   TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_ITEM_LEG_OLD',
      sh_delivery_header  TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HEADER',
      sh_delivery_item    TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM',
    END OF cs_tabledef .

  CONSTANTS:
    BEGIN OF cs_delivery_stat,
      not_relevant    TYPE wbsta VALUE '',
      not_processed   TYPE wbsta VALUE 'A',
      partially_proc  TYPE wbsta VALUE 'B',
      completely_proc TYPE wbsta VALUE 'C',
    END OF cs_delivery_stat .
  CONSTANTS:
    BEGIN OF cs_md_type,
      goods_receipt TYPE mkpf-blart VALUE 'WE',
    END OF cs_md_type .
  CONSTANTS:
    BEGIN OF cs_parvw,
      supplier TYPE parvw VALUE 'LF',
    END OF cs_parvw .
  CONSTANTS:
    BEGIN OF cs_adrtype,
      organization TYPE ad_adrtype VALUE '1',
    END OF cs_adrtype .
  CONSTANTS:
    BEGIN OF cs_loccat,
      departure TYPE zif_gtt_mia_app_types=>tv_loccat VALUE 'S',
      arrival   TYPE zif_gtt_mia_app_types=>tv_loccat VALUE 'D',
    END OF cs_loccat .
  CONSTANTS:
    BEGIN OF cs_vbtyp,
      shipment TYPE vbtyp VALUE '8',
      delivery TYPE vbtyp VALUE '7',
    END OF cs_vbtyp .
  CONSTANTS:
    BEGIN OF cs_abfer,
      loaded_inb_ship  TYPE abfer VALUE '2',
      empty_inb_ship   TYPE abfer VALUE '4',
      loaded_outb_ship TYPE abfer VALUE '1', "Loaded outbound shipment
      empty_outb_ship  TYPE abfer VALUE '3', "Empty outbound shipment
    END OF cs_abfer .
  CONSTANTS:
    BEGIN OF cs_base_btd_tco,
      inb_dlv        TYPE /scmtms/base_btd_tco VALUE '58',
      outb_dlv       TYPE /scmtms/base_btd_tco VALUE '73',
      purchase_order TYPE /scmtms/base_btd_tco VALUE '001',
      sales_order    TYPE /scmtms/base_btd_tco VALUE '114',
    END OF cs_base_btd_tco .
  CONSTANTS:
    BEGIN OF cs_start_evtcnt,
      shipment TYPE i VALUE 2000000000,
      delivery TYPE i VALUE 2100000000,
    END OF cs_start_evtcnt .
  CONSTANTS cv_agent_id_type TYPE bu_id_type VALUE 'LBN001' ##NO_TEXT.
  CONSTANTS:
    cv_agent_id_prefix TYPE c LENGTH 4 VALUE 'LBN#' ##NO_TEXT.
  CONSTANTS:
    cv_scac_prefix TYPE char5 VALUE 'SCAC#'.
  CONSTANTS:
    BEGIN OF cs_shipment_kind,
      shipment TYPE zif_gtt_mia_app_types=>tv_shipment_kind VALUE 'SHP',
    END OF cs_shipment_kind.
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_mia_app_types
  PUBLIC .


  TYPES ts_likpvb TYPE likp .
  TYPES:
    tt_likpvb TYPE STANDARD TABLE OF ts_likpvb .
  TYPES ts_lipsvb TYPE lipsvb .
  TYPES:
    tt_lipsvb TYPE STANDARD TABLE OF ts_lipsvb .
  TYPES ts_vttkvb TYPE vttkvb .
  TYPES:
    tt_vttkvb TYPE STANDARD TABLE OF ts_vttkvb .
  TYPES ts_vttpvb TYPE vttpvb .
  TYPES:
    tt_vttpvb TYPE STANDARD TABLE OF ts_vttpvb .
  TYPES ts_vttsvb TYPE vttsvb .
  TYPES:
    tt_vttsvb TYPE STANDARD TABLE OF ts_vttsvb .
  TYPES ts_vtspvb TYPE vtspvb .
  TYPES:
    tt_vtspvb TYPE STANDARD TABLE OF ts_vtspvb .
  TYPES ts_mseg TYPE mseg .
  TYPES:
    tt_mseg TYPE STANDARD TABLE OF ts_mseg .
  TYPES:
    BEGIN OF ts_lipsvb_key,
      vbeln TYPE vbeln_vl,
      posnr TYPE posnr_vl,
    END OF ts_lipsvb_key .
  TYPES:
    tt_lipsvb_key TYPE STANDARD TABLE OF ts_lipsvb_key .
  TYPES tv_ship_type TYPE char2 .
  TYPES tv_trans_mode TYPE char2 .
  TYPES tv_departure_dt TYPE timestamp .
  TYPES tv_departure_tz TYPE timezone .
  TYPES tv_arrival_dt TYPE timestamp .
  TYPES tv_arrival_tz TYPE timezone .
  TYPES tv_deliv_cnt TYPE int4 .
  TYPES tv_trobj_res_id TYPE char20 .
  TYPES tv_trobj_res_val TYPE char20 .
  TYPES tv_resrc_cnt TYPE int4 .
  TYPES tv_resrc_tp_id TYPE char30 .
  TYPES tv_crdoc_ref_typ TYPE char3 .
  TYPES tv_crdoc_ref_val TYPE tndr_trkid .
  TYPES tv_stopnum TYPE int4 .
  TYPES tv_stopid TYPE char255 .
  TYPES tv_stopcnt TYPE int4 .
  TYPES tv_loccat TYPE char1 .
  TYPES tv_loctype TYPE char20 .
  TYPES tv_locid TYPE char10 .
  TYPES tv_lstelz_txt TYPE char20 .
  TYPES tv_kunablaz_txt TYPE char25 .
  TYPES tv_lgortaz_txt TYPE char16 .
  TYPES tv_lgnumaz TYPE char3 .
  TYPES tv_toraz TYPE char3 .
  TYPES tv_lgtraz_txt TYPE char50 .
  TYPES tv_tsrfo TYPE num4 .
  TYPES tv_pln_evt_datetime TYPE timestamp .
  TYPES tv_pln_evt_timezone TYPE char6 .
  TYPES:
    BEGIN OF ts_stops,
      stopid           TYPE tv_stopid,
      stopid_txt       TYPE tv_stopid,    "formated stopid
      stopcnt          TYPE tv_stopcnt,
      loccat           TYPE tv_loccat,
      loctype          TYPE tv_loctype,
      locid            TYPE tv_locid,
      lstelz_txt       TYPE tv_lstelz_txt,
      kunablaz_txt     TYPE tv_kunablaz_txt,
      lgortaz_txt      TYPE tv_lgortaz_txt,
      lgnumaz          TYPE tv_lgnumaz,
      toraz            TYPE tv_toraz,
      lgtraz_txt       TYPE tv_lgtraz_txt,
      tknum            TYPE tknum,
      tsnum            TYPE tsnum,
      tsrfo            TYPE tsrfo,
      pln_evt_datetime TYPE timestamp,
      pln_evt_timezone TYPE timezone,
    END OF ts_stops .
  TYPES:
    tt_stops TYPE STANDARD TABLE OF ts_stops
                    WITH EMPTY KEY .
  TYPES:
    BEGIN OF ts_dlv_watch_stops,
      vbeln      TYPE vbeln_vl,
      stopid     TYPE tv_stopid,
      stopid_txt TYPE tv_stopid,    "formated stopid
      loccat     TYPE tv_loccat,
    END OF ts_dlv_watch_stops .
  TYPES:
    tt_dlv_watch_stops TYPE STANDARD TABLE OF ts_dlv_watch_stops
                              WITH EMPTY KEY .

  TYPES:
    tv_shipment_kind TYPE char5.
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_mia_ctp_tor_constants
  PUBLIC .


  CONSTANTS cv_base_btd_tco_inb_dlv TYPE /scmtms/base_btd_tco VALUE '58' ##NO_TEXT.
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_mia_ctp_types
  PUBLIC .


  TYPES:
    BEGIN OF ts_fu_list,
      tor_id        TYPE /scmtms/tor_id,
      item_id       TYPE /scmtms/item_id,
      quantity      TYPE menge_d,
      quantityuom   TYPE meins,
      product_id    TYPE /scmtms/product_id,
      product_descr TYPE /scmtms/item_description,
      base_uom_val  TYPE /scmtms/qua_base_uom_val,
      base_uom_uni  TYPE /scmtms/qua_base_uom_uni,
      change_mode   TYPE /bobf/conf_change_mode,
    END OF ts_fu_list .
  TYPES:
    BEGIN OF ts_fu_id,
      tor_id    TYPE /scmtms/tor_id,
      lifecycle TYPE  /scmtms/tor_lc_status,
    END OF ts_fu_id .
  TYPES:
    tt_fu_list  TYPE STANDARD TABLE OF ts_fu_list
                       WITH EMPTY KEY .
  TYPES:
    tt_fu_id  TYPE STANDARD TABLE OF ts_fu_id
                       WITH EMPTY KEY .
  TYPES:
    BEGIN OF ts_delivery_item,
      vbeln   TYPE vbeln_vl,
      posnr   TYPE posnr_vl,
      likp    TYPE zif_gtt_mia_app_types=>ts_likpvb,
      lips    TYPE zif_gtt_mia_app_types=>ts_lipsvb,
      fu_list TYPE tt_fu_list,
    END OF ts_delivery_item .
  TYPES:
    tt_delivery_item TYPE STANDARD TABLE OF ts_delivery_item
                            WITH EMPTY KEY .
  TYPES:
    BEGIN OF ts_delivery,
      vbeln        TYPE vbeln_vl,
      fu_relevant  TYPE abap_bool,
      pod_relevant TYPE abap_bool,
      likp         TYPE zif_gtt_mia_app_types=>ts_likpvb,
      lips         TYPE STANDARD TABLE OF zif_gtt_mia_app_types=>ts_lipsvb
                      WITH EMPTY KEY,
    END OF ts_delivery .
  TYPES:
    tt_delivery TYPE STANDARD TABLE OF ts_delivery .
  TYPES:
    BEGIN OF ts_delivery_chng,
      vbeln         TYPE vbeln_vl,
      posnr         TYPE posnr_vl,
      tor_id        TYPE /scmtms/tor_id,
      item_id       TYPE /scmtms/item_id,
      quantity      TYPE /scmtms/qua_pcs_val,
      quantityuom   TYPE /scmtms/qua_pcs_uni,
      product_id    TYPE /scmtms/product_id,
      product_descr TYPE /scmtms/item_description,
      base_uom_val  TYPE /scmtms/qua_base_uom_val,
      base_uom_uni  TYPE /scmtms/qua_base_uom_uni,
      change_mode   TYPE /bobf/conf_change_mode,
    END OF ts_delivery_chng .
  TYPES:
    tt_delivery_chng TYPE SORTED TABLE OF ts_delivery_chng
                            WITH UNIQUE KEY vbeln posnr tor_id item_id.
  TYPES:
    tt_tor_type TYPE SORTED TABLE OF /scmtms/tor_type
                       WITH UNIQUE KEY table_line .
  TYPES:
    BEGIN OF ty_aotype,
      tor_type TYPE /scmtms/tor_type,
      aotype   TYPE /saptrx/aotype,
    END OF ty_aotype .
  TYPES:
    tt_aottype TYPE SORTED TABLE OF ty_aotype
                      WITH UNIQUE KEY tor_type .
  TYPES:
    BEGIN OF ty_aotype_item,
      obj_type TYPE /saptrx/trk_obj_type,
      aot_type TYPE /saptrx/aotype,
    END OF ty_aotype_item .
  TYPES:
    tt_aotype_item TYPE TABLE OF ty_aotype_item .
  TYPES:
    BEGIN OF ty_aotypes_new,
      trk_obj_type  TYPE /saptrx/aotypes-trk_obj_type,
      aotype        TYPE /saptrx/aotypes-aotype,
      trxservername TYPE /saptrx/aotypes-trxservername,
    END OF ty_aotypes_new .
  TYPES:
    tt_aotypes_new TYPE TABLE OF ty_aotypes_new .
  TYPES:
    BEGIN OF ty_trxserv,
      trx_server_id TYPE /saptrx/trxserv-trx_server_id,
      trx_server    TYPE /saptrx/trxserv-trx_server,
    END OF ty_trxserv .
  TYPES:
    tt_trxserv TYPE TABLE OF ty_trxserv .
  TYPES:
    BEGIN OF ty_likp,
      vbeln TYPE likp-vbeln, "Delivery
      kodat TYPE likp-kodat, "Picking Date
      kouhr TYPE likp-kouhr, "Picking Time (Local Time, with Reference to a Plant)
      vstel TYPE likp-vstel, "Shipping Point / Receiving Point
      wadat TYPE likp-wadat, "Planned Goods Movement Date
      wauhr TYPE likp-wauhr, "Time of Goods Issue (Local, Relating to a Plant)
      kunnr TYPE likp-kunnr, "Ship-to Party
    END OF ty_likp .
  TYPES:
    tt_likp TYPE TABLE OF ty_likp .
  TYPES:
    tt_aotype_rst   TYPE RANGE OF /saptrx/aotype .
  TYPES:
    tt_tor_type_rst TYPE RANGE OF /scmtms/tor_type .
ENDINTERFACE.""",
    r"""CLASS zcl_gtt_spof_ae_fact_po_itm_cf DEFINITION
  PUBLIC
  INHERITING FROM zcl_gtt_ae_factory
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_gtt_ae_factory~get_ae_filler
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SPOF_AE_FACT_PO_ITM_CF IMPLEMENTATION.


  METHOD zif_gtt_ae_factory~get_ae_filler.
    ro_ae_filler = NEW zcl_gtt_spof_ae_fill_po_itm_cf(
      io_ae_parameters = io_ae_parameters ).
  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SPOF_CTP_DLH_DATA definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IT_XLIKP type SHP_LIKP_T
      !IT_XLIPS type SHP_LIPS_T
      !IT_YLIPS type SHP_LIPS_T
      !IV_STO_IS_USED type BOOLEAN default ABAP_FALSE
    raising
      CX_UDM_MESSAGE .
  methods GET_RELATED_PO_DATA
    exporting
      !RR_EKKO type ref to DATA
      !RR_EKPO type ref to DATA
      !RR_EKET type ref to DATA
      !RR_REF_LIST type ref to DATA
      !RR_LIKP type ref to DATA
      !RR_LIPS type ref to DATA
      !EV_STO_IS_USED type BOOLEAN .
  PROTECTED SECTION.
private section.

  data MT_EKKO type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKKO .
  data MT_EKPO type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKPO .
  data MT_EKET type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKET .
  data MT_REF_LIST type ZIF_GTT_SPOF_APP_TYPES=>TT_REF_LIST .
  data MT_LIKP type VA_LIKPVB_T .
  data MT_LIPS type VA_LIPSVB_T .
  data MV_STO_IS_USED type BOOLEAN .

  methods INIT_PO_HEADER_DATA
    importing
      !IT_XLIKP type SHP_LIKP_T
      !IT_XLIPS type SHP_LIPS_T
      !IT_YLIPS type SHP_LIPS_T
    exporting
      !ET_EKKO type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKKO
      !ET_EKPO type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKPO
      !ET_EKET type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKET
      !ET_REF_LIST type ZIF_GTT_SPOF_APP_TYPES=>TT_REF_LIST
    raising
      CX_UDM_MESSAGE .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SPOF_CTP_DLH_DATA IMPLEMENTATION.


  METHOD constructor.

    CLEAR:
      mt_ekko,
      mt_ekpo,
      mt_eket,
      mt_ref_list,
      mt_likp,
      mt_lips,
      mv_sto_is_used.

    mv_sto_is_used = iv_sto_is_used.
    init_po_header_data(
      EXPORTING
        it_xlikp    = it_xlikp
        it_xlips    = it_xlips
        it_ylips    = it_ylips
      IMPORTING
        et_ekko     = mt_ekko
        et_ekpo     = mt_ekpo
        et_eket     = mt_eket
        et_ref_list = mt_ref_list ).

    mt_likp = it_xlikp.
    mt_lips = it_xlips.

  ENDMETHOD.


  METHOD get_related_po_data.

    rr_ekko     = REF #( mt_ekko ).
    rr_ekpo     = REF #( mt_ekpo ).
    rr_eket     = REF #( mt_eket ).
    rr_ref_list = REF #( mt_ref_list ).
    rr_likp     = REF #( mt_likp ).
    rr_lips     = REF #( mt_lips ).
    ev_sto_is_used = mv_sto_is_used.

  ENDMETHOD.


  METHOD init_po_header_data.

    DATA:
      ls_likp     TYPE likpvb,
      lt_lips     TYPE shp_lips_t,
      lt_ref_list TYPE zif_gtt_spof_app_types=>tt_ref_list,
      lt_ekko     TYPE zif_gtt_spof_app_types=>tt_uekko,
      lt_ekpo     TYPE zif_gtt_spof_app_types=>tt_uekpo,
      lt_eket     TYPE zif_gtt_spof_app_types=>tt_ueket.

    CLEAR:
      et_ekko,
      et_ekpo,
      et_eket,
      et_ref_list.

    LOOP AT it_xlips ASSIGNING FIELD-SYMBOL(<ls_xlips>).
      CLEAR ls_likp.
      READ TABLE it_xlikp INTO ls_likp WITH KEY vbeln = <ls_xlips>-vbeln.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF mv_sto_is_used IS INITIAL."Normal scenario(Inbound delivery)
        CHECK zcl_gtt_tools=>is_appropriate_dl_item( ir_likp = REF #( ls_likp ) ir_lips = REF #( <ls_xlips> ) ) = abap_true.
      ELSE."STO scenario(outbound delivery)
        CHECK zcl_gtt_tools=>is_appropriate_odlv_item( ir_likp = REF #( ls_likp ) ir_lips = REF #( <ls_xlips> ) ) = abap_true.
      ENDIF.
      APPEND <ls_xlips> TO lt_lips.
    ENDLOOP.

    CHECK lt_lips IS NOT INITIAL.

    zcl_gtt_tools=>get_po_so_by_delivery(
      EXPORTING
        iv_vgtyp       = if_sd_doc_category=>purchase_order
        it_xlikp       = it_xlikp
        it_xlips       = lt_lips
        iv_sto_is_used = mv_sto_is_used
      IMPORTING
        et_ref_list    = lt_ref_list
        ev_ref_chg_flg = DATA(lv_chg_flg) ).

*   The relationship between PO and IDLV or ODLV is not changed,no need send out the Cross TP IDOC
    IF lv_chg_flg IS INITIAL.
      RETURN.
    ENDIF.

    IF lt_ref_list IS NOT INITIAL.
      SELECT *
        INTO TABLE lt_ekko
        FROM ekko
         FOR ALL ENTRIES IN lt_ref_list
       WHERE ebeln = lt_ref_list-vgbel.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_ekpo
        FROM ekpo
         FOR ALL ENTRIES IN lt_ref_list
       WHERE ebeln = lt_ref_list-vgbel.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_eket
        FROM eket
         FOR ALL ENTRIES IN lt_ref_list
       WHERE ebeln = lt_ref_list-vgbel.
    ENDIF.

    LOOP AT lt_ekko INTO DATA(ls_ekko).
      CHECK zcl_gtt_spof_po_tools=>is_appropriate_po(
          ir_ekko = REF #( ls_ekko )
          ir_ekpo = REF #( lt_ekpo ) ) = abap_true.
      APPEND ls_ekko TO et_ekko.
    ENDLOOP.

    LOOP AT lt_ekpo INTO DATA(ls_ekpo).
      CLEAR ls_ekko.
      READ TABLE et_ekko INTO ls_ekko WITH KEY ebeln = ls_ekpo-ebeln.
      IF sy-subrc = 0.
        APPEND ls_ekpo TO et_ekpo.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_eket INTO DATA(ls_eket).
      CLEAR ls_ekko.
      READ TABLE et_ekko INTO ls_ekko WITH KEY ebeln = ls_eket-ebeln.
      IF sy-subrc = 0.
        APPEND ls_eket TO et_eket.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_ref_list INTO DATA(ls_ref_list).
      CLEAR ls_ekko.
      READ TABLE et_ekko INTO ls_ekko WITH KEY ebeln = ls_ref_list-vgbel.
      IF sy-subrc <> 0.
        DELETE lt_ref_list.
      ENDIF.
    ENDLOOP.

    et_ref_list = lt_ref_list.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SPOF_CTP_DLI_DATA definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IT_XLIKP type SHP_LIKP_T
      !IT_XLIPS type SHP_LIPS_T
      !IT_YLIPS type SHP_LIPS_T
    raising
      CX_UDM_MESSAGE .
  methods GET_RELATED_PO_DATA
    exporting
      !RR_LIKP type ref to DATA
      !RR_LIPS type ref to DATA
      !RR_EKKO type ref to DATA
      !RR_EKPO type ref to DATA
      !RR_EKES type ref to DATA
      !RR_EKET type ref to DATA .
  PROTECTED SECTION.
private section.

  data MT_EKKO type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKKO .
  data MT_EKPO type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKPO .
  data MT_EKES type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKES .
  data MT_EKET type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKET .
  data MT_LIKP type VA_LIKPVB_T .
  data MT_LIPS type VA_LIPSVB_T .

  methods INIT_PO_ITEM_DATA
    importing
      !IT_XLIKP type SHP_LIKP_T
      !IT_XLIPS type SHP_LIPS_T
      !IT_YLIPS type SHP_LIPS_T
    exporting
      !ET_EKES type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKES
      !ET_EKKO type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKKO
      !ET_EKPO type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKPO
      !ET_EKET type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKET
    raising
      CX_UDM_MESSAGE .
  methods MERGE_EKES
    importing
      !IS_XLIKP type LIKPVB
      !IS_XLIPS type LIPSVB
      !IS_YLIPS type LIPSVB
      !I_EBELN type EBELN
      !I_EBELP type EBELP
    changing
      !CT_EKES type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKES
    raising
      CX_UDM_MESSAGE .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SPOF_CTP_DLI_DATA IMPLEMENTATION.


  METHOD constructor.
    init_po_item_data(
      EXPORTING
        it_xlikp = it_xlikp
        it_xlips = it_xlips
        it_ylips = it_ylips
      IMPORTING
        et_ekes  = mt_ekes
        et_ekko  = mt_ekko
        et_ekpo  = mt_ekpo
        et_eket  = mt_eket
    ).

    mt_likp = it_xlikp.
    mt_lips = it_xlips.

  ENDMETHOD.


  METHOD get_related_po_data.

    rr_ekko   = REF #( mt_ekko ).
    rr_ekpo   = REF #( mt_ekpo ).
    rr_ekes   = REF #( mt_ekes ).
    rr_eket   = REF #( mt_eket ).
    rr_likp   = REF #( mt_likp ).
    rr_lips   = REF #( mt_lips ).

  ENDMETHOD.


  METHOD init_po_item_data.

    DATA: ls_ekko    TYPE ekko,
          ls_ekpo    TYPE ekpo,
          l_archived TYPE boole_d,
          lv_ebelp   TYPE ebelp.
    DATA: lt_eket  TYPE STANDARD TABLE OF eket,
          lt_ueket TYPE zif_gtt_spof_app_types=>tt_ueket,
          ls_likp  TYPE likpvb.
    CLEAR: et_ekko, et_ekpo, et_ekes, et_eket.

    LOOP AT it_xlips ASSIGNING FIELD-SYMBOL(<ls_xlips>).
      CLEAR ls_likp.
      READ TABLE it_xlikp INTO ls_likp WITH KEY vbeln = <ls_xlips>-vbeln.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      CHECK zcl_gtt_tools=>is_appropriate_dl_item( ir_likp = REF #( ls_likp ) ir_lips = REF #( <ls_xlips> ) ) = abap_true
         OR zcl_gtt_tools=>is_appropriate_odlv_item( ir_likp = REF #( ls_likp ) ir_lips = REF #( <ls_xlips> ) ) = abap_true."Support STO Scenario

      CHECK zcl_gtt_tools=>is_appropriate_dl_type( ir_likp = REF #( ls_likp ) ) = abap_true
         OR zcl_gtt_tools=>is_appropriate_odlv_type( ir_likp = REF #( ls_likp ) ) = abap_true."Support STO Scenario

      DATA(lv_ebeln)  = <ls_xlips>-vgbel.
      DATA(lv_vgpos)  = <ls_xlips>-vgpos.

      IF lv_ebeln IS NOT INITIAL AND lv_vgpos IS NOT INITIAL.
        zcl_gtt_spof_po_tools=>get_po_header(
          EXPORTING
            i_ebeln    = lv_ebeln
          IMPORTING
            es_ekko    = ls_ekko
            e_archived = l_archived
        ).

        IF ls_ekko IS INITIAL.
          CONTINUE.
        ENDIF.

        lv_ebelp = lv_vgpos+1(5).
        zcl_gtt_spof_po_tools=>get_po_item(
          EXPORTING
            i_ebeln    = lv_ebeln
            i_ebelp    = lv_ebelp
            i_archived = l_archived
            is_ekko    = ls_ekko
          IMPORTING
            es_ekpo    = ls_ekpo
        ).

        IF ls_ekpo IS NOT INITIAL AND
          zcl_gtt_spof_po_tools=>is_appropriate_po_item( ir_ekko = REF #( ls_ekko ) ir_ekpo = REF #( ls_ekpo ) ) = abap_true.
          TRY.
              DATA(ls_duplicate_ekko) = et_ekko[ ebeln = lv_ebeln ].
            CATCH cx_sy_itab_line_not_found.
              et_ekko   = VALUE #( BASE et_ekko
                             ( CORRESPONDING #( ls_ekko ) ) ).
          ENDTRY.

          et_ekpo   = VALUE #( BASE et_ekpo
                             ( CORRESPONDING #( ls_ekpo ) ) ).

          SELECT * FROM ekes APPENDING CORRESPONDING FIELDS OF TABLE et_ekes
                         WHERE ebeln EQ lv_ebeln AND
                               ebelp EQ lv_ebelp AND
                               loekz NE abap_true.
          TRY.
              DATA(ls_ylips) = it_ylips[ vbeln = <ls_xlips>-vbeln posnr = <ls_xlips>-posnr ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          merge_ekes(
            EXPORTING
              is_xlikp = ls_likp
              is_xlips = <ls_xlips>
              is_ylips = ls_ylips
              i_ebeln  = lv_ebeln
              i_ebelp  = lv_ebelp
            CHANGING
              ct_ekes  = et_ekes
          ).
          CLEAR: lt_eket,lt_ueket.
          CALL FUNCTION 'ME_EKET_SINGLE_READ_ITEM'
            EXPORTING
              pi_ebeln            = lv_ebeln
              pi_ebelp            = lv_ebelp
            TABLES
              pto_eket            = lt_eket
            EXCEPTIONS
              err_no_record_found = 1
              OTHERS              = 2.
          IF sy-subrc <> 0.
            CLEAR lt_eket.
          ENDIF.
          IF lt_eket IS NOT INITIAL.
            lt_ueket   = CORRESPONDING #( lt_eket ).
            APPEND LINES OF lt_ueket TO et_eket.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD merge_ekes.

    DATA: ls_ekes            TYPE zif_gtt_spof_app_types=>ts_uekes.

    ls_ekes-ebeln = i_ebeln.
    ls_ekes-ebelp = i_ebelp.
    ls_ekes-vbeln = CONV vbeln_vl( is_xlips-vbeln ).
    ls_ekes-vbelp = CONV posnr_vl( is_xlips-posnr ).
    ls_ekes-menge = CONV bbmng( is_xlips-lfimg ).

    CASE is_xlips-updkz.
      WHEN zif_gtt_ef_constants=>cs_change_mode-insert.
        ls_ekes-kz = zif_gtt_ef_constants=>cs_change_mode-insert.
      WHEN zif_gtt_ef_constants=>cs_change_mode-update OR
           zif_gtt_ef_constants=>cs_change_mode-undefined.

        IF is_ylips IS NOT INITIAL.

          IF is_xlips-lfimg <>  is_ylips-lfimg.
            LOOP AT ct_ekes ASSIGNING FIELD-SYMBOL(<ls_ekes>) WHERE vbeln = ls_ekes-vbeln AND
                                                                    vbelp = ls_ekes-vbelp.
              <ls_ekes>-kz    = zif_gtt_ef_constants=>cs_change_mode-update.
              <ls_ekes>-menge = ls_ekes-menge.
              EXIT.
            ENDLOOP.
          ENDIF.
        ENDIF.

      WHEN zif_gtt_ef_constants=>cs_change_mode-delete.
        ls_ekes-kz = zif_gtt_ef_constants=>cs_change_mode-delete.
        ls_ekes-loekz = 'X'.
        ls_ekes-menge = ls_ekes-menge * -1.
    ENDCASE.

    IF ls_ekes-kz IS NOT INITIAL.
      APPEND ls_ekes TO ct_ekes.
    ENDIF.


  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SPOF_CTP_SND_DL_TO_PO definition
  public
  inheriting from ZCL_GTT_CTP_SND
  create private .

public section.

  class-methods GET_INSTANCE
    returning
      value(RO_SENDER) type ref to ZCL_GTT_SPOF_CTP_SND_DL_TO_PO
    raising
      CX_UDM_MESSAGE .
  methods PREPARE_IDOC_DATA
    importing
      !IO_DLI_DATA type ref to ZCL_GTT_SPOF_CTP_DLI_DATA
    raising
      CX_UDM_MESSAGE .
protected section.

  methods GET_AOTYPE_RESTRICTION_ID
    redefinition .
  methods GET_OBJECT_TYPE
    redefinition .
  methods GET_EVTYPE_RESTRICTION_ID
    redefinition .
private section.

  constants:
    BEGIN OF cs_mapping,
      ebeln TYPE /saptrx/paramname VALUE 'YN_PO_NUMBER',
      ebelp TYPE /saptrx/paramname VALUE 'YN_PO_ITEM',
    END OF cs_mapping .

  methods PREPARE_IDOC_DATA_FOR_CF_EVENT
    importing
      !IO_DLI_DATA type ref to ZCL_GTT_SPOF_CTP_DLI_DATA
    raising
      CX_UDM_MESSAGE .
  methods PREPARE_IDOC_DATA_FOR_PLN_EVT
    importing
      !IO_DLI_DATA type ref to ZCL_GTT_SPOF_CTP_DLI_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_CF_EVENT
    importing
      !IS_EVTYPE type ZIF_GTT_CTP_TYPES=>TS_EVTYPE
      !IS_EKKO type ZIF_GTT_SPOF_APP_TYPES=>TS_UEKKO
      !IS_EKPO type ZIF_GTT_SPOF_APP_TYPES=>TS_UEKPO
      !IT_EKES type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKES
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_EVT_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_APPOBJ_CTABS
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_EKPO type ZIF_GTT_SPOF_APP_TYPES=>TS_UEKPO
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_CONTROL_DATA
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_EKKO type ZIF_GTT_SPOF_APP_TYPES=>TS_UEKKO
      !IS_EKPO type ZIF_GTT_SPOF_APP_TYPES=>TS_UEKPO
      !IT_EKES type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKES
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_EXP_EVENT
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_EKKO type ZIF_GTT_SPOF_APP_TYPES=>TS_UEKKO
      !IS_EKPO type ZIF_GTT_SPOF_APP_TYPES=>TS_UEKPO
      !IT_EKES type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKES
      !IT_EKET type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKET
      !IT_LIKP type VA_LIKPVB_T
      !IT_LIPS type VA_LIPSVB_T
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_TRACKING_ID
    importing
      !IS_AOTYPE type ZIF_GTT_CTP_TYPES=>TS_AOTYPE
      !IS_EKKO type ZIF_GTT_SPOF_APP_TYPES=>TS_UEKKO
      !IS_EKPO type ZIF_GTT_SPOF_APP_TYPES=>TS_UEKPO
      !IT_EKES type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKES
    changing
      !CS_IDOC_DATA type ZIF_GTT_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods IS_EKES_CHANGED
    importing
      !IT_EKES type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKES
    returning
      value(RV_RESULT) type BOOLE_D .
  methods IS_EKES_INSERT_OR_REMOVED
    importing
      !IT_EKES type ZIF_GTT_SPOF_APP_TYPES=>TT_UEKES
    returning
      value(RV_RESULT) type BOOLE_D .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SPOF_CTP_SND_DL_TO_PO IMPLEMENTATION.


  METHOD fill_idoc_appobj_ctabs.
    cs_idoc_data-appobj_ctabs = VALUE #( BASE cs_idoc_data-appobj_ctabs (
      trxservername = cs_idoc_data-trxserv-trx_server_id
      appobjtype    = is_aotype-aot_type
      appobjid      = zcl_gtt_spof_po_tools=>get_tracking_id_po_itm( ir_ekpo = REF #( is_ekpo ) )
    ) ).
  ENDMETHOD.


  METHOD FILL_IDOC_CF_EVENT.

    zcl_gtt_spof_ctp_tools=>get_po_itm_conf_event_data(
      EXPORTING
        iv_appsys          = mv_appsys
        is_evtype          = is_evtype
        is_ekko            = CORRESPONDING #( is_ekko )
        is_ekpo            = CORRESPONDING #( is_ekpo )
        it_ekes            = CORRESPONDING #( it_ekes )
      IMPORTING
        et_evt_ctabs       = cs_idoc_data-evt_ctabs
        et_eventid_map     = cs_idoc_data-eventid_map
        et_trackingheader  = cs_idoc_data-evt_header
        et_tracklocation   = cs_idoc_data-evt_locationid
        et_trackreferences = cs_idoc_data-evt_reference
        et_trackparameters = cs_idoc_data-evt_parameters
    ).

  ENDMETHOD.


  METHOD fill_idoc_control_data.

    DATA: lt_control    TYPE /saptrx/bapi_trk_control_tab .

    lt_control  = VALUE #(
      (
        paramname = cs_mapping-ebeln
        value     = zcl_gtt_spof_po_tools=>get_formated_po_number(
                      ir_ekko = REF #( is_ekpo ) )
      )
      (
        paramname = cs_mapping-ebelp
        value     = zcl_gtt_spof_po_tools=>get_formated_po_item_number(
                      ir_ekpo = REF #( is_ekpo ) )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_timezone
        value     = zcl_gtt_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_technical_timezone
        value     = zcl_gtt_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_technical_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-reported_by
        value     = sy-uname
      )
    ).

    " fill technical data into all control data records
    LOOP AT lt_control ASSIGNING FIELD-SYMBOL(<ls_control>).
      <ls_control>-appsys     = mv_appsys.
      <ls_control>-appobjtype = is_aotype-aot_type.
      <ls_control>-appobjid = zcl_gtt_spof_po_tools=>get_tracking_id_po_itm( ir_ekpo = REF #( is_ekpo ) ).
    ENDLOOP.

    cs_idoc_data-control  = VALUE #( BASE cs_idoc_data-control
                                     ( LINES OF lt_control ) ).

  ENDMETHOD.


  METHOD fill_idoc_exp_event.

    DATA: lt_exp_event      TYPE /saptrx/bapi_trk_ee_tab.

    zcl_gtt_spof_ctp_tools=>get_po_itm_planned_evt(
      EXPORTING
        iv_appsys    = mv_appsys
        is_aotype    = is_aotype
        is_ekko      = is_ekko
        is_ekpo      = is_ekpo
        it_ekes      = it_ekes
        it_eket      = it_eket
        it_likp      = it_likp
        it_lips      = it_lips
      IMPORTING
        et_exp_event = lt_exp_event
    ).

    IF lt_exp_event[] IS INITIAL.
      lt_exp_event = VALUE #( (
          milestone         = ''
          locid2            = ''
          loctype           = ''
          locid1            = ''
          evt_exp_datetime  = '000000000000000'
          evt_exp_tzone     = ''
      ) ).
    ENDIF.

    LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
      <ls_exp_event>-appsys         = mv_appsys.
      <ls_exp_event>-appobjtype     = is_aotype-aot_type.
      <ls_exp_event>-appobjid       = zcl_gtt_spof_po_tools=>get_tracking_id_po_itm( ir_ekpo = REF #( is_ekpo ) ).
      <ls_exp_event>-language       = sy-langu.
    ENDLOOP.

    cs_idoc_data-exp_event = VALUE #( BASE cs_idoc_data-exp_event
                                      ( LINES OF lt_exp_event ) ).

  ENDMETHOD.


  METHOD fill_idoc_tracking_id.

    cs_idoc_data-tracking_id    = VALUE #( BASE cs_idoc_data-tracking_id (
      appsys      = mv_appsys
      appobjtype  = is_aotype-aot_type
      appobjid    = zcl_gtt_spof_po_tools=>get_tracking_id_po_itm( ir_ekpo = REF #( is_ekpo ) )
      trxcod      = zif_gtt_ef_constants=>cs_trxcod-po_position
      trxid       = zcl_gtt_spof_po_tools=>get_tracking_id_po_itm( ir_ekpo = REF #( is_ekpo ) )
      timzon      = zcl_gtt_tools=>get_system_time_zone( )
    ) ).

  ENDMETHOD.


  METHOD get_aotype_restriction_id.

    rv_rst_id   = 'DL_TO_POIT'.
  ENDMETHOD.


  METHOD GET_EVTYPE_RESTRICTION_ID.

    rv_rst_id   = 'DL_TO_POIT'.

  ENDMETHOD.


  METHOD GET_INSTANCE.

    DATA(lt_trk_obj_type) = VALUE zif_gtt_ctp_types=>tt_trk_obj_type(
       ( zif_gtt_ef_constants=>cs_trk_obj_type-esc_purord )
       ( zif_gtt_ef_constants=>cs_trk_obj_type-esc_deliv )
    ).

    IF is_gtt_enabled( it_trk_obj_type = lt_trk_obj_type ) = abap_true.
      ro_sender  = NEW #( ).
      ro_sender->initiate( ).

    ENDIF.

  ENDMETHOD.


  METHOD GET_OBJECT_TYPE.

    rv_objtype  = zif_gtt_ef_constants=>cs_trk_obj_type-esc_purord.

  ENDMETHOD.


  METHOD IS_EKES_CHANGED.

    rv_result = abap_false.

    LOOP AT it_ekes TRANSPORTING NO FIELDS WHERE kz = zif_gtt_ef_constants=>cs_change_mode-insert or
                                                 kz = zif_gtt_ef_constants=>cs_change_mode-update or
                                                 kz = zif_gtt_ef_constants=>cs_change_mode-delete .
      rv_result = abap_true.
      exit.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_ekes_insert_or_removed.
    rv_result = abap_false.

    LOOP AT it_ekes TRANSPORTING NO FIELDS WHERE kz = zif_gtt_ef_constants=>cs_change_mode-insert OR
                                                 kz = zif_gtt_ef_constants=>cs_change_mode-delete .
      rv_result = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD prepare_idoc_data.

    prepare_idoc_data_for_cf_event( io_dli_data = io_dli_data ).

    prepare_idoc_data_for_pln_evt( io_dli_data = io_dli_data ).

  ENDMETHOD.


  METHOD prepare_idoc_data_for_cf_event.
    DATA: ls_idoc_data TYPE zif_gtt_ctp_types=>ts_idoc_evt_data,
          lt_ekes      TYPE zif_gtt_spof_app_types=>tt_uekes,
          ls_ekko      TYPE zif_gtt_spof_app_types=>ts_uekko.
    FIELD-SYMBOLS: <lt_ekko> TYPE zif_gtt_spof_app_types=>tt_uekko,
                   <lt_ekpo> TYPE zif_gtt_spof_app_types=>tt_uekpo,
                   <lt_ekes> TYPE zif_gtt_spof_app_types=>tt_uekes.

    DATA: lv_evtcnt TYPE /saptrx/evtcnt VALUE 1.

    io_dli_data->get_related_po_data(
      IMPORTING
        rr_ekko = DATA(lr_ekko)
        rr_ekpo = DATA(lr_ekpo)
        rr_ekes = DATA(lr_ekes)
    ).

    ASSIGN lr_ekko->* TO <lt_ekko>.
    ASSIGN lr_ekpo->* TO <lt_ekpo>.
    ASSIGN lr_ekes->* TO <lt_ekes>.

    LOOP AT <lt_ekpo> ASSIGNING FIELD-SYMBOL(<ls_ekpo>).
      CLEAR: lt_ekes, ls_ekko.
      LOOP AT <lt_ekes> ASSIGNING FIELD-SYMBOL(<ls_ekes>) WHERE ebeln = <ls_ekpo>-ebeln
                                                            AND ebelp = <ls_ekpo>-ebelp.
        lt_ekes = VALUE #( BASE lt_ekes ( <ls_ekes> ) ).
      ENDLOOP.

      LOOP AT <lt_ekko> INTO ls_ekko WHERE ebeln = <ls_ekpo>-ebeln.
        EXIT.
      ENDLOOP.
      IF is_ekes_changed( it_ekes = lt_ekes ) = abap_true.
        LOOP AT mt_evtype ASSIGNING FIELD-SYMBOL(<ls_evtype>).
          CLEAR: ls_idoc_data.
          <ls_evtype>-evtcnt = lv_evtcnt.
          fill_evt_idoc_trxserv(
            EXPORTING
              is_evtype    = <ls_evtype>
            CHANGING
              cs_idoc_data = ls_idoc_data
          ).
          fill_idoc_cf_event(
            EXPORTING
              is_evtype    = <ls_evtype>
              is_ekko      = ls_ekko
              is_ekpo      = <ls_ekpo>
              it_ekes      = lt_ekes
            CHANGING
              cs_idoc_data = ls_idoc_data
          ).

          IF ls_idoc_data-eventid_map IS NOT INITIAL AND
              ls_idoc_data-evt_parameters IS NOT INITIAL AND
              ls_idoc_data-evt_header IS NOT INITIAL.
            APPEND ls_idoc_data TO mt_idoc_evt_data.
            lv_evtcnt = lv_evtcnt + 1.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD prepare_idoc_data_for_pln_evt.
    DATA: ls_idoc_data TYPE zif_gtt_ctp_types=>ts_idoc_data,
          lt_ekes      TYPE zif_gtt_spof_app_types=>tt_uekes,
          lt_eket      TYPE zif_gtt_spof_app_types=>tt_ueket,
          ls_ekko      TYPE zif_gtt_spof_app_types=>ts_uekko.
    FIELD-SYMBOLS: <lt_ekko> TYPE zif_gtt_spof_app_types=>tt_uekko,
                   <lt_ekpo> TYPE zif_gtt_spof_app_types=>tt_uekpo,
                   <lt_ekes> TYPE zif_gtt_spof_app_types=>tt_uekes,
                   <lt_eket> TYPE zif_gtt_spof_app_types=>tt_ueket,
                   <lt_likp> TYPE va_likpvb_t,
                   <lt_lips> TYPE va_lipsvb_t.

    DATA: lv_evtcnt TYPE /saptrx/evtcnt VALUE 1.

    io_dli_data->get_related_po_data(
      IMPORTING
        rr_ekko = DATA(lr_ekko)
        rr_ekpo = DATA(lr_ekpo)
        rr_ekes = DATA(lr_ekes)
        rr_eket = DATA(lr_eket)
        rr_likp = DATA(lr_likp)
        rr_lips = DATA(lr_lips)
    ).

    ASSIGN lr_ekko->* TO <lt_ekko>.
    ASSIGN lr_ekpo->* TO <lt_ekpo>.
    ASSIGN lr_ekes->* TO <lt_ekes>.
    ASSIGN lr_eket->* TO <lt_eket>.
    ASSIGN lr_likp->* TO <lt_likp>.
    ASSIGN lr_lips->* TO <lt_lips>.

    LOOP AT <lt_ekpo> ASSIGNING FIELD-SYMBOL(<ls_ekpo>).
      CLEAR: lt_ekes, ls_ekko, lt_eket.
      LOOP AT <lt_ekes> ASSIGNING FIELD-SYMBOL(<ls_ekes>) WHERE ebeln = <ls_ekpo>-ebeln
                                                            AND ebelp = <ls_ekpo>-ebelp.
        lt_ekes = VALUE #( BASE lt_ekes ( <ls_ekes> ) ).
      ENDLOOP.

      LOOP AT <lt_eket> ASSIGNING FIELD-SYMBOL(<ls_eket>) WHERE ebeln = <ls_ekpo>-ebeln
                                                            AND ebelp = <ls_ekpo>-ebelp.
        lt_eket = VALUE #( BASE lt_eket ( <ls_eket> ) ).
      ENDLOOP.

      LOOP AT <lt_ekko> INTO ls_ekko WHERE ebeln = <ls_ekpo>-ebeln.
        EXIT.
      ENDLOOP.
      IF is_ekes_insert_or_removed( it_ekes = lt_ekes ) = abap_true.
        LOOP AT mt_aotype ASSIGNING FIELD-SYMBOL(<ls_aotype>).
          CLEAR: ls_idoc_data.
          ls_idoc_data-appsys   = mv_appsys.
          fill_idoc_trxserv(
            EXPORTING
              is_aotype    = <ls_aotype>
            CHANGING
              cs_idoc_data = ls_idoc_data ).

          fill_idoc_appobj_ctabs(
            EXPORTING
              is_aotype    = <ls_aotype>
              is_ekpo      = <ls_ekpo>
            CHANGING
              cs_idoc_data = ls_idoc_data ).

          fill_idoc_control_data(
            EXPORTING
              is_aotype    = <ls_aotype>
              is_ekko      = ls_ekko
              is_ekpo      = <ls_ekpo>
              it_ekes      = lt_ekes
            CHANGING
              cs_idoc_data = ls_idoc_data ).

          fill_idoc_exp_event(
            EXPORTING
              is_aotype    = <ls_aotype>
              is_ekko      = ls_ekko
              is_ekpo      = <ls_ekpo>
              it_ekes      = lt_ekes
              it_eket      = lt_eket
              it_likp      = <lt_likp>
              it_lips      = <lt_lips>
            CHANGING
              cs_idoc_data = ls_idoc_data ).

          fill_idoc_tracking_id(
            EXPORTING
              is_aotype    = <ls_aotype>
              is_ekko      = ls_ekko
              is_ekpo      = <ls_ekpo>
              it_ekes      = lt_ekes
            CHANGING
              cs_idoc_data = ls_idoc_data ).

          IF ls_idoc_data-appobj_ctabs[] IS NOT INITIAL AND
           ls_idoc_data-control[] IS NOT INITIAL AND
           ls_idoc_data-tracking_id[] IS NOT INITIAL.
            APPEND ls_idoc_data TO mt_idoc_data.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SPOF_IM_LE_SHIPPING definition
  public
  final
  create public .

*"* public components of class CL_EXM_IM_LE_SHP_DELIVERY_PROC
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_DELIVERY_PROC .
protected section.
*"* protected components of class CL_EXM_IM_LE_SHP_DELIVERY_PROC
*"* do not include other source files here!!!
private section.
*"* private components of class CL_EXM_IM_LE_SHP_DELIVERY_PROC
*"* do not include other source files here!!!
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SPOF_IM_LE_SHIPPING IMPLEMENTATION.


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_HEADER .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_HEADER


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_ITEM .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_ITEM


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FCODE_ATTRIBUTES .

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FCODE_ATTRIBUTES


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FIELD_ATTRIBUTES .

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FIELD_ATTRIBUTES


method IF_EX_LE_SHP_DELIVERY_PROC~CHECK_ITEM_DELETION .

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHECK_ITEM_DELETION


method IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_DELETION .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_DELETION


method IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_FINAL_CHECK .

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_FINAL_CHECK


method IF_EX_LE_SHP_DELIVERY_PROC~DOCUMENT_NUMBER_PUBLISH .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~DOCUMENT_NUMBER_PUBLISH


method IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_HEADER .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_HEADER


method IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_ITEM .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_ITEM


method IF_EX_LE_SHP_DELIVERY_PROC~INITIALIZE_DELIVERY .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~INITIALIZE_DELIVERY


method IF_EX_LE_SHP_DELIVERY_PROC~ITEM_DELETION .


endmethod.                    "IF_EX_LE_SHP_DELIVERY_PROC~ITEM_DELETION


method IF_EX_LE_SHP_DELIVERY_PROC~PUBLISH_DELIVERY_ITEM .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~PUBLISH_DELIVERY_ITEM


method IF_EX_LE_SHP_DELIVERY_PROC~READ_DELIVERY .

endmethod.                    "IF_EX_LE_SHP_DELIVERY_PROC~READ_DELIVERY


method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_BEFORE_OUTPUT.
endmethod.


METHOD if_ex_le_shp_delivery_proc~save_and_publish_document .

  CALL FUNCTION 'ZGTT_SPOF_CTP_DL_TO_PO'
    EXPORTING
      it_xlikp = it_xlikp
      it_xlips = it_xlips
      it_ylips = it_ylips.

ENDMETHOD. "IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_DOCUMENT


method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_DOCUMENT_PREPARE .

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~SAVE_DOCUMENT_PREPARE
ENDCLASS.""",
    r"""CLASS zcl_gtt_spof_pe_filler_po_hdr DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_gtt_pe_filler .

    METHODS constructor
      IMPORTING
        !io_ef_parameters TYPE REF TO zif_gtt_ef_parameters
        !io_bo_reader     TYPE REF TO zif_gtt_tp_reader .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_ef_parameters TYPE REF TO zif_gtt_ef_parameters .
    DATA mo_bo_reader TYPE REF TO zif_gtt_tp_reader .

    METHODS get_delivery_datetime
      IMPORTING
        !i_delivery_date   TYPE eindt
      RETURNING
        VALUE(rv_datetime) TYPE /saptrx/event_exp_datetime
      RAISING
        cx_udm_message .
    METHODS get_latest_delivery_date
      IMPORTING
        !iv_ebeln       TYPE ebeln
        !ir_ekpo        TYPE REF TO data
        !ir_eket        TYPE REF TO data
      RETURNING
        VALUE(rv_eindt) TYPE eindt
      RAISING
        cx_udm_message .
    METHODS add_planned_delivery_event
      IMPORTING
        !is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        !ct_expeventdata TYPE zif_gtt_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message .
    METHODS add_po_item_completed_event
      IMPORTING
        !is_app_objects  TYPE trxas_appobj_ctab_wa
        !ir_ekpo_data    TYPE REF TO data
      CHANGING
        !ct_expeventdata TYPE zif_gtt_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SPOF_PE_FILLER_PO_HDR IMPLEMENTATION.


  METHOD add_planned_delivery_event.
    DATA: lv_loekz       TYPE ekpo-loekz,
          lv_pd_conf     TYPE abap_bool,
          lv_latest_date TYPE eindt.

    lv_pd_conf = zcl_gtt_spof_po_tools=>is_appropriate_po(
      ir_ekko = is_app_objects-maintabref
      ir_ekpo = mo_ef_parameters->get_appl_table( iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_new )
    ).

    IF lv_pd_conf EQ abap_true.
      " clear expecting datetime and timezone when lv_latest_date is initial
      " to avoid generation of unwanted GTTOverdue events
      lv_latest_date = get_latest_delivery_date(
        EXPORTING
          iv_ebeln = CONV ebeln( zcl_gtt_tools=>get_field_of_structure(
                         ir_struct_data = is_app_objects-maintabref
                         iv_field_name  = 'EBELN'
                       ) )
          ir_ekpo  = mo_ef_parameters->get_appl_table(
                        iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_new )
          ir_eket  = mo_ef_parameters->get_appl_table(
                        iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_sched_new )
      ).

      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_ef_constants=>cs_milestone-po_planned_delivery
        evt_exp_tzone     = COND #( WHEN lv_latest_date IS NOT INITIAL
                                      THEN zcl_gtt_tools=>get_system_time_zone( ) )
        evt_exp_datetime  = COND #( WHEN lv_latest_date IS NOT INITIAL
                                      THEN get_delivery_datetime( i_delivery_date = lv_latest_date ) )
        milestonenum      = zcl_gtt_tools=>get_next_sequence_id(
                                      it_expeventdata = ct_expeventdata )
      ) ).

    ENDIF.
  ENDMETHOD.


  METHOD add_po_item_completed_event.
    DATA: lv_ebeln     TYPE ebeln.
    FIELD-SYMBOLS: <lt_ekpo> TYPE zif_gtt_spof_app_types=>tt_uekpo,
                   <ls_ekpo> TYPE zif_gtt_spof_app_types=>ts_uekpo.

    ASSIGN ir_ekpo_data->* TO <lt_ekpo>.

    lv_ebeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_objects-maintabref
      iv_field_name  = 'EBELN' ).
    IF <lt_ekpo> IS ASSIGNED.
      LOOP AT <lt_ekpo> ASSIGNING <ls_ekpo>
        WHERE ebeln = lv_ebeln.
        IF zcl_gtt_spof_po_tools=>is_appropriate_po_item( ir_ekko = is_app_objects-maintabref ir_ekpo = REF #( <ls_ekpo> ) ) = abap_true.

          ct_expeventdata = VALUE #( BASE ct_expeventdata (
                  appsys            = mo_ef_parameters->get_appsys(  )
                  appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
                  language          = sy-langu
                  appobjid          = is_app_objects-appobjid
                  milestone         = zif_gtt_ef_constants=>cs_milestone-po_itm_completed
                  locid2            = zcl_gtt_spof_po_tools=>get_tracking_id_po_itm( ir_ekpo = REF #( <ls_ekpo> ) )
                  milestonenum      = zcl_gtt_tools=>get_next_sequence_id(
                                      it_expeventdata = ct_expeventdata )
                ) ).
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.

    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.

  ENDMETHOD.


  METHOD get_delivery_datetime.
    rv_datetime = zcl_gtt_tools=>get_local_timestamp(
      iv_date = i_delivery_date
      iv_time = CONV t( '235959' ) ).
  ENDMETHOD.


  METHOD get_latest_delivery_date.
    DATA: lt_ebelp_rng TYPE RANGE OF ekpo-ebelp,
          lv_eindt_max TYPE eket-eindt,
          lv_eindt_set TYPE abap_bool VALUE abap_false.

    FIELD-SYMBOLS: <lt_ekpo> TYPE zif_gtt_spof_app_types=>tt_uekpo,
                   <ls_ekpo> TYPE zif_gtt_spof_app_types=>ts_uekpo,
                   <lt_eket> TYPE zif_gtt_spof_app_types=>tt_ueket,
                   <ls_eket> TYPE zif_gtt_spof_app_types=>ts_ueket.


    CLEAR: rv_eindt.

    ASSIGN ir_ekpo->* TO <lt_ekpo>.
    ASSIGN ir_eket->* TO <lt_eket>.

    IF <lt_ekpo> IS ASSIGNED AND
       <lt_eket> IS ASSIGNED.
      " Preparation of Active Items List
      LOOP AT <lt_ekpo> ASSIGNING <ls_ekpo>
        WHERE ebeln  = iv_ebeln
          AND loekz <> zif_gtt_spof_app_constants=>cs_loekz-deleted.

        lt_ebelp_rng  = VALUE #( BASE lt_ebelp_rng
                                 ( low    = <ls_ekpo>-ebelp
                                   option = 'EQ'
                                   sign   = 'I' ) ).
      ENDLOOP.

      " keep Latest Delivery Date in schedule lines per item.
      LOOP AT <lt_eket> ASSIGNING <ls_eket>
        WHERE ebeln  = iv_ebeln
          AND ebelp IN lt_ebelp_rng
          AND kz <> zif_gtt_ef_constants=>cs_change_mode-delete
        GROUP BY ( ebeln = <ls_eket>-ebeln
                   ebelp = <ls_eket>-ebelp )
        ASCENDING
        ASSIGNING FIELD-SYMBOL(<ls_eket_group>).

        CLEAR: lv_eindt_max.

        LOOP AT GROUP <ls_eket_group> ASSIGNING FIELD-SYMBOL(<ls_eket_items>).
          lv_eindt_max    = COND #( WHEN <ls_eket_items>-eindt > lv_eindt_max
                                      THEN <ls_eket_items>-eindt
                                      ELSE lv_eindt_max ).
        ENDLOOP.

        IF lv_eindt_set = abap_false.
          rv_eindt  = lv_eindt_max.
          lv_eindt_set        = abap_true.
        ELSEIF rv_eindt < lv_eindt_max.
          rv_eindt = lv_eindt_max.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD zif_gtt_pe_filler~check_relevance.

    DATA: lv_new_latest_date TYPE eindt,
          lv_old_latest_date TYPE eindt.

    rv_result   = zif_gtt_ef_constants=>cs_condition-false.
    IF zcl_gtt_spof_po_tools=>is_appropriate_po(
          ir_ekko = is_app_objects-maintabref
          ir_ekpo = mo_ef_parameters->get_appl_table( iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_new )
          ) = abap_true.

      IF is_app_objects-update_indicator = zif_gtt_ef_constants=>cs_change_mode-insert.
        rv_result = zif_gtt_ef_constants=>cs_condition-true.
      ELSE.
        DATA(lv_ebeln) = CONV ebeln( zcl_gtt_tools=>get_field_of_structure(
                           ir_struct_data = is_app_objects-maintabref
                           iv_field_name  = 'EBELN'
                         ) ).

        lv_new_latest_date = get_latest_delivery_date(
          EXPORTING
            iv_ebeln = lv_ebeln
            ir_ekpo  = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_new )
            ir_eket  = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_sched_new )
        ).

        lv_old_latest_date = get_latest_delivery_date(
          EXPORTING
            iv_ebeln = lv_ebeln
            ir_ekpo  = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_old )
            ir_eket  = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_sched_old )
        ).
        rv_result = COND #( WHEN lv_new_latest_date <> lv_old_latest_date
                                 THEN zif_gtt_ef_constants=>cs_condition-true
                                 ELSE zif_gtt_ef_constants=>cs_condition-false ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_gtt_pe_filler~get_planed_events.

    add_planned_delivery_event(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    add_po_item_completed_event(
      EXPORTING
        is_app_objects = is_app_objects
        ir_ekpo_data   = mo_ef_parameters->get_appl_table(
                           iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_new )
      CHANGING
        ct_expeventdata = ct_expeventdata
  ).

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SPOF_PE_FILLER_PO_ITM definition
  public
  create public .

public section.

  interfaces ZIF_GTT_PE_FILLER .

  methods CONSTRUCTOR
    importing
      !IO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS
      !IO_BO_READER type ref to ZIF_GTT_TP_READER .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ts_dl_item_id,
        vbeln TYPE vbeln_vl,
        posnr TYPE posnr_vl,
      END OF ts_dl_item_id .
  types:
    tt_dl_item_id  TYPE STANDARD TABLE OF ts_dl_item_id .

  types:
    BEGIN OF ts_dl_header,
      vbeln TYPE vbeln_vl,
      lfart TYPE lfart,
    END OF ts_dl_header.
  types:
    tt_dl_header TYPE STANDARD TABLE OF ts_dl_header.

  data MO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS .
  data MO_BO_READER type ref to ZIF_GTT_TP_READER .

  methods ADD_GOODS_RECEIPT_EVENT
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    raising
      CX_UDM_MESSAGE .
  methods ADD_CONFIRMATION_EVENT
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    raising
      CX_UDM_MESSAGE .
  methods GET_DELIVERY_DATETIME
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
    returning
      value(RV_DATETIME) type /SAPTRX/EVENT_EXP_DATETIME
    raising
      CX_UDM_MESSAGE .
  methods GET_OBJECT_FIELD_VALUE
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
      !IV_FIELDNAME type CLIKE
    returning
      value(RV_VALUE) type CHAR50
    raising
      CX_UDM_MESSAGE .
  methods ADD_DLV_ITEM_COMPLETED_EVENT
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
      !IR_EKES_DATA type ref to DATA
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    raising
      CX_UDM_MESSAGE .
  methods ADD_PLANNED_DELIVERY_EVENT
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    raising
      CX_UDM_MESSAGE .
  methods GET_PLANNED_DELIVERY_DATETIME
    importing
      !I_DELIVERY_DATE type EINDT
    returning
      value(RV_DATETIME) type /SAPTRX/EVENT_EXP_DATETIME
    raising
      CX_UDM_MESSAGE .
  methods IS_APPROPRIATE_DL
    importing
      !I_VBELN type VBELN_VL
      !I_POSNR type POSNR_VL
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  methods GET_LATEST_DELIVERY_DATE
    importing
      !IR_EKPO type ref to DATA
      !IR_EKET type ref to DATA
    returning
      value(RV_EINDT) type EINDT
    raising
      CX_UDM_MESSAGE .
  methods ADD_ODLV_ITEM_COMPLETED_EVENT
    importing
      !IS_APP_OBJECTS type TRXAS_APPOBJ_CTAB_WA
    changing
      !CT_EXPEVENTDATA type ZIF_GTT_EF_TYPES=>TT_EXPEVENTDATA
    raising
      CX_UDM_MESSAGE .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SPOF_PE_FILLER_PO_ITM IMPLEMENTATION.


  METHOD add_confirmation_event.
    DATA: lv_enable_conf TYPE abap_bool.

    lv_enable_conf = zcl_gtt_spof_po_tools=>is_enable_confirmation_po_item( ir_ekpo = is_app_objects-maintabref ).

    IF lv_enable_conf = abap_true AND zcl_gtt_spof_po_tools=>is_appropriate_po_item( ir_ekko = is_app_objects-mastertabref ir_ekpo = is_app_objects-maintabref ) = abap_true.
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_ef_constants=>cs_milestone-po_confirmation
      ) ).
    ENDIF.
  ENDMETHOD.


  METHOD add_goods_receipt_event.
    DATA: lv_loekz   TYPE ekpo-loekz,
          lv_gr_conf TYPE abap_bool.

    lv_gr_conf = zcl_gtt_spof_po_tools=>is_appropriate_po_item( ir_ekko = is_app_objects-mastertabref ir_ekpo = is_app_objects-maintabref ).

    IF lv_gr_conf EQ abap_true.
      " clear expecting datetime and timezone when Item is marked as deleted
      " to avoid generation of unwanted GTTOverdue events
      lv_loekz = zcl_gtt_tools=>get_field_of_structure(
        ir_struct_data = is_app_objects-maintabref
        iv_field_name  = 'LOEKZ' ).

      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_ef_constants=>cs_milestone-po_goods_receipt
        evt_exp_tzone     = COND #( WHEN lv_loekz IS INITIAL
                                      THEN zcl_gtt_tools=>get_system_time_zone( ) )
        evt_exp_datetime  = COND #( WHEN lv_loekz IS INITIAL
                                      THEN get_delivery_datetime( is_app_objects = is_app_objects ) )
        locid1            = zcl_gtt_tools=>get_field_of_structure(
                                ir_struct_data = is_app_objects-maintabref
                                iv_field_name  = 'WERKS' )
        loctype           = zif_gtt_ef_constants=>cs_loc_types-plant
      ) ).

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.

  ENDMETHOD.


  METHOD get_delivery_datetime.
    rv_datetime = zcl_gtt_tools=>get_local_timestamp(
                    iv_date = get_object_field_value(
                                is_app_objects = is_app_objects
                                iv_fieldname   = 'EINDT' )
                    iv_time = CONV t( '000000' ) ).
  ENDMETHOD.


  METHOD get_object_field_value.
    DATA: lr_data  TYPE REF TO data,
          lv_dummy TYPE char100.

    FIELD-SYMBOLS: <ls_data>  TYPE any,
                   <lv_value> TYPE any.

    lr_data = mo_bo_reader->get_data( is_app_object = is_app_objects ).

    ASSIGN lr_data->* TO <ls_data>.
    IF <ls_data> IS ASSIGNED.
      ASSIGN COMPONENT iv_fieldname OF STRUCTURE <ls_data> TO <lv_value>.
      IF <lv_value> IS ASSIGNED.
        rv_value = <lv_value>.
      ELSE.
        MESSAGE e001(zgtt) WITH iv_fieldname 'po item' INTO lv_dummy ##NO_TEXT.
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt) WITH 'po item' INTO lv_dummy ##NO_TEXT .
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD add_dlv_item_completed_event.

    DATA: lv_conf TYPE abap_bool.
    DATA: lv_ebeln TYPE ebeln,
          lv_ebelp TYPE ebelp.
    DATA: lt_dl_item         TYPE tt_dl_item_id,
          lt_dl_item_deleted TYPE tt_dl_item_id.
    DATA: ls_lips  TYPE lips.

    FIELD-SYMBOLS: <lt_ekes> TYPE zif_gtt_spof_app_types=>tt_uekes,
                   <ls_ekes> TYPE zif_gtt_spof_app_types=>ts_uekes.

    ASSIGN ir_ekes_data->* TO <lt_ekes>.

    lv_ebeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_objects-maintabref
      iv_field_name  = 'EBELN' ).

    lv_ebelp = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_objects-maintabref
      iv_field_name  = 'EBELP' ).

    lv_conf = zcl_gtt_spof_po_tools=>is_appropriate_po_item( ir_ekko = is_app_objects-mastertabref ir_ekpo = is_app_objects-maintabref ).

    IF <lt_ekes> IS ASSIGNED AND lv_conf = abap_true.
      LOOP AT <lt_ekes> ASSIGNING <ls_ekes>
        WHERE ebeln = lv_ebeln AND
              ebelp = lv_ebelp AND
              vbeln IS NOT INITIAL AND
              vbelp IS NOT INITIAL AND
              kz = zif_gtt_ef_constants=>cs_change_mode-delete.
        lt_dl_item_deleted  = VALUE #( BASE lt_dl_item_deleted (
              vbeln = <ls_ekes>-vbeln
              posnr = <ls_ekes>-vbelp
            ) ).
      ENDLOOP.
      LOOP AT <lt_ekes> ASSIGNING <ls_ekes>
        WHERE ebeln = lv_ebeln AND
              ebelp = lv_ebelp AND
              vbeln IS NOT INITIAL AND
              vbelp IS NOT INITIAL AND
              kz <> zif_gtt_ef_constants=>cs_change_mode-delete.
        IF NOT line_exists( lt_dl_item[ vbeln = <ls_ekes>-vbeln posnr = <ls_ekes>-vbelp ] ) AND
           NOT line_exists( lt_dl_item_deleted[ vbeln = <ls_ekes>-vbeln posnr = <ls_ekes>-vbelp ] )
          AND is_appropriate_dl( i_vbeln = <ls_ekes>-vbeln i_posnr = <ls_ekes>-vbelp ) = abap_true.

          lt_dl_item  = VALUE #( BASE lt_dl_item (
              vbeln = <ls_ekes>-vbeln
              posnr = <ls_ekes>-vbelp
            ) ).
        ENDIF.
      ENDLOOP.

      LOOP AT lt_dl_item ASSIGNING FIELD-SYMBOL(<ls_dl_item>).
        ct_expeventdata = VALUE #( BASE ct_expeventdata (
             appsys            = mo_ef_parameters->get_appsys(  )
             appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
             language          = sy-langu
             appobjid          = is_app_objects-appobjid
             milestone         = zif_gtt_ef_constants=>cs_milestone-dl_item_completed
             locid2            = zcl_gtt_spof_po_tools=>get_tracking_id_dl_item( ir_lips = REF #( <ls_dl_item> )  )
             milestonenum      = zcl_gtt_tools=>get_next_sequence_id(
                                 it_expeventdata = ct_expeventdata )
           ) ).
      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD is_appropriate_dl.

    FIELD-SYMBOLS:
      <ft_likp> TYPE va_likpvb_t,
      <ft_lips> TYPE va_lipsvb_t.

    DATA:
      ls_likp TYPE likp,
      ls_lips TYPE lips,
      lo_likp TYPE REF TO data,
      lo_lips TYPE REF TO data.

    rv_result = abap_false.

    SELECT SINGLE * INTO  ls_likp
      FROM likp
     WHERE vbeln = i_vbeln.

    IF sy-subrc = 0 AND
       zcl_gtt_tools=>is_appropriate_dl_type(
         ir_likp = REF #( ls_likp ) ) = abap_true.

      SELECT SINGLE * INTO ls_lips
         FROM lips
         WHERE vbeln = i_vbeln AND posnr = i_posnr.

      IF sy-subrc = 0 AND zcl_gtt_tools=>is_appropriate_dl_item( ir_likp = REF #( ls_likp ) ir_lips = REF #( ls_lips ) ) = abap_true.
        rv_result = abap_true.
      ENDIF.
    ELSE.

      lo_likp = mo_ef_parameters->get_appl_table( iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-dl_header_new ).
      ASSIGN lo_likp->* TO <ft_likp>.

      lo_lips = mo_ef_parameters->get_appl_table( iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-dl_item_new ).
      ASSIGN lo_lips->* TO <ft_lips>.

      CLEAR:
        ls_likp,
        ls_lips.
      READ TABLE <ft_likp> INTO ls_likp INDEX 1.
      IF zcl_gtt_tools=>is_appropriate_dl_type( ir_likp = REF #( ls_likp ) ) = abap_true.

        READ TABLE <ft_lips> INTO ls_lips WITH KEY vbeln = i_vbeln
                                                   posnr = i_posnr.
        IF sy-subrc = 0 AND zcl_gtt_tools=>is_appropriate_dl_item( ir_likp = REF #( ls_likp ) ir_lips = REF #( ls_lips ) ) = abap_true.
          rv_result = abap_true.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pe_filler~check_relevance.

    TYPES: tt_ekpo    TYPE STANDARD TABLE OF uekpo.
    DATA: lv_dummy    TYPE char100.

    FIELD-SYMBOLS: <ls_ekpo_new> TYPE uekpo,
                   <lt_ekpo_old> TYPE tt_ekpo,
                   <ls_ekpo_old> TYPE uekpo.

    rv_result = zif_gtt_ef_constants=>cs_condition-false.

    IF zcl_gtt_spof_po_tools=>is_appropriate_po_item( ir_ekko = is_app_objects-mastertabref ir_ekpo = is_app_objects-maintabref ) = abap_true.

      IF is_app_objects-update_indicator = zif_gtt_ef_constants=>cs_change_mode-insert.
        rv_result = zif_gtt_ef_constants=>cs_condition-true.
      ELSE.
        DATA(lr_ekpo) = mo_ef_parameters->get_appl_table(
          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_old ).

        ASSIGN is_app_objects-maintabref->* TO <ls_ekpo_new>.
        ASSIGN lr_ekpo->* TO <lt_ekpo_old>.

        IF <ls_ekpo_new> IS ASSIGNED AND
           <lt_ekpo_old> IS ASSIGNED AND
           ( <ls_ekpo_new>-kz = zif_gtt_ef_constants=>cs_change_mode-update OR
             <ls_ekpo_new>-kz = zif_gtt_ef_constants=>cs_change_mode-undefined ).

          READ TABLE <lt_ekpo_old> ASSIGNING <ls_ekpo_old>
            WITH KEY ebeln = <ls_ekpo_new>-ebeln
                     ebelp = <ls_ekpo_new>-ebelp.
          IF sy-subrc = 0.
            rv_result = COND #( WHEN <ls_ekpo_new>-kzabs <> <ls_ekpo_old>-kzabs OR
                                     <ls_ekpo_new>-wepos <> <ls_ekpo_old>-wepos OR
                                     <ls_ekpo_new>-loekz <> <ls_ekpo_old>-loekz
                                  THEN zif_gtt_ef_constants=>cs_condition-true
                                  ELSE zif_gtt_ef_constants=>cs_condition-false ).
          ELSE.
            MESSAGE e005(zgtt)
              WITH 'EKPO' |{ <ls_ekpo_new>-ebeln }{ <ls_ekpo_new>-ebelp }|
              INTO lv_dummy.
            zcl_gtt_tools=>throw_exception( ).
          ENDIF.
        ELSE.
          MESSAGE e002(zgtt) WITH 'EKPO' INTO lv_dummy.
          zcl_gtt_tools=>throw_exception( ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_gtt_pe_filler~get_planed_events.
    add_confirmation_event(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    add_goods_receipt_event(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    add_planned_delivery_event(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    add_dlv_item_completed_event(
      EXPORTING
        is_app_objects  = is_app_objects
        ir_ekes_data    = mo_ef_parameters->get_appl_table(
                         iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_vend_conf_new )
      CHANGING
        ct_expeventdata = ct_expeventdata ).

*   If it is a STO purchase order item,add outbound delivery item completed planned event
    add_odlv_item_completed_event(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

  ENDMETHOD.


  METHOD add_planned_delivery_event.
    DATA: lv_loekz       TYPE ekpo-loekz,
          lv_conf        TYPE abap_bool,
          lv_latest_date TYPE eindt.

    lv_conf = zcl_gtt_spof_po_tools=>is_appropriate_po_item( ir_ekko = is_app_objects-mastertabref ir_ekpo = is_app_objects-maintabref ).

    IF lv_conf EQ abap_true.
      lv_latest_date = get_latest_delivery_date(
        EXPORTING
          ir_ekpo  = is_app_objects-maintabref
          ir_eket  = mo_ef_parameters->get_appl_table(
                        iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_sched_new )
      ).
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_ef_constants=>cs_milestone-po_planned_delivery
        evt_exp_tzone     = COND #( WHEN lv_latest_date IS NOT INITIAL
                                      THEN zcl_gtt_tools=>get_system_time_zone( ) )
        evt_exp_datetime  = COND #( WHEN lv_latest_date IS NOT INITIAL
                                      THEN get_planned_delivery_datetime( i_delivery_date = lv_latest_date ) )
        milestonenum      = zcl_gtt_tools=>get_next_sequence_id(
                                      it_expeventdata = ct_expeventdata )
      ) ).

    ENDIF.
  ENDMETHOD.


  METHOD get_latest_delivery_date.
    DATA: lv_eindt_max TYPE eket-eindt,
          lv_eindt_set TYPE abap_bool VALUE abap_false.

    FIELD-SYMBOLS: <lt_eket> TYPE zif_gtt_spof_app_types=>tt_ueket,
                   <ls_eket> TYPE zif_gtt_spof_app_types=>ts_ueket.

    CLEAR: rv_eindt.

    DATA(lv_ebeln) = CONV ebeln( zcl_gtt_tools=>get_field_of_structure(
                         ir_struct_data = ir_ekpo
                         iv_field_name  = 'EBELN'
              ) ).

    DATA(lv_ebelp) = CONV ebelp( zcl_gtt_tools=>get_field_of_structure(
                         ir_struct_data = ir_ekpo
                         iv_field_name  = 'EBELP'
              ) ).
    ASSIGN ir_eket->* TO <lt_eket>.
    IF <lt_eket> IS ASSIGNED.
      " keep Latest Delivery Date in schedule lines per item.
      LOOP AT <lt_eket> ASSIGNING <ls_eket>
         WHERE ebeln  = lv_ebeln
          AND ebelp  = lv_ebelp
          AND  kz <> zif_gtt_ef_constants=>cs_change_mode-delete
        GROUP BY ( ebeln = <ls_eket>-ebeln
                   ebelp = <ls_eket>-ebelp )
        ASCENDING
        ASSIGNING FIELD-SYMBOL(<ls_eket_group>).

        CLEAR: lv_eindt_max.

        LOOP AT GROUP <ls_eket_group> ASSIGNING FIELD-SYMBOL(<ls_eket_items>).
          lv_eindt_max    = COND #( WHEN <ls_eket_items>-eindt > lv_eindt_max
                                      THEN <ls_eket_items>-eindt
                                      ELSE lv_eindt_max ).
        ENDLOOP.

        IF lv_eindt_set = abap_false.
          rv_eindt  = lv_eindt_max.
          lv_eindt_set        = abap_true.
        ELSEIF rv_eindt < lv_eindt_max.
          rv_eindt = lv_eindt_max.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_planned_delivery_datetime.
    rv_datetime = zcl_gtt_tools=>get_local_timestamp(
      iv_date = i_delivery_date
      iv_time = CONV t( '235959' ) ).
  ENDMETHOD.


  METHOD add_odlv_item_completed_event.

    FIELD-SYMBOLS:
      <lt_likp> TYPE va_likpvb_t,
      <lt_lips> TYPE va_lipsvb_t.

    DATA:
      lv_ebeln     TYPE ebeln,
      lv_ebelp     TYPE ebelp,
      lv_conf      TYPE abap_bool,
      lt_dl_item   TYPE tt_dl_item_id,
      lt_dl_header TYPE tt_dl_header,
      ls_dl_header TYPE ts_dl_header,
      lr_likp      TYPE REF TO data,
      lr_lips      TYPE REF TO data,
      ls_dlv_data  TYPE ts_dl_item_id,
      lt_dlv_data  TYPE tt_dl_item_id,
      lv_locid2    TYPE /saptrx/loc_id_2.

    lv_ebeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_objects-maintabref
      iv_field_name  = 'EBELN' ).

    lv_ebelp = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_objects-maintabref
      iv_field_name  = 'EBELP' ).

    lv_conf = zcl_gtt_spof_po_tools=>is_appropriate_po_item( ir_ekko = is_app_objects-mastertabref ir_ekpo = is_app_objects-maintabref ).

    CHECK lv_conf = abap_true.

    zcl_gtt_sof_toolkit=>get_delivery_type(
      RECEIVING
        rt_type = DATA(lt_delv_type) ).

*   PO item planned event also can be called from outbound delivery cross PO item(STO scenario)
*   (function module ZGTT_SPOF_CTP_DL_TO_PO -> ZGTT_SPOF_EE_PO_ITM -> Method ADD_ODLV_ITEM_COMPLETED_EVENT of class ZCL_GTT_SPOF_PE_FILLER_PO_ITM )
*   In this case,the outbound delivery header and item data were stored in the cache table,retrive data from cache table is a more efficient way.
    TRY.
        lr_likp = mo_ef_parameters->get_appl_table( iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-dl_header_new ).
        lr_lips = mo_ef_parameters->get_appl_table( iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-dl_item_new ).
      CATCH cx_udm_message.
    ENDTRY.
    IF lr_likp IS BOUND AND lr_lips IS BOUND.
      ASSIGN lr_likp->* TO <lt_likp>.
      ASSIGN lr_lips->* TO <lt_lips>.
    ENDIF.

*   Based on PO number,get delivery data from cache
    IF <lt_likp> IS ASSIGNED AND <lt_lips> IS ASSIGNED.
      LOOP AT <lt_lips> ASSIGNING FIELD-SYMBOL(<fs_lips>)
        WHERE vgbel = lv_ebeln
          AND vgpos = lv_ebelp
          AND vgtyp = if_sd_doc_category=>purchase_order.
        READ TABLE <lt_likp> ASSIGNING FIELD-SYMBOL(<fs_likp>)
          WITH KEY vbeln = <fs_lips>-vbeln
                   vbtyp = if_sd_doc_category=>delivery.
        IF sy-subrc = 0 AND <fs_likp>-lfart IN lt_delv_type.
          ls_dlv_data-vbeln = <fs_lips>-vbeln.
          ls_dlv_data-posnr = <fs_lips>-posnr.
          APPEND ls_dlv_data TO lt_dlv_data.
          CLEAR ls_dlv_data.
        ENDIF.
      ENDLOOP.
    ENDIF.

*   Based on PO number,get delivery data from DB
    SELECT vbeln
           posnr
      INTO TABLE lt_dl_item
      FROM lips
     WHERE vgbel = lv_ebeln
       AND vgpos = lv_ebelp
       AND vgtyp = if_sd_doc_category=>purchase_order.

    IF lt_dl_item IS NOT INITIAL.
      SELECT vbeln
             lfart
        INTO TABLE lt_dl_header
        FROM likp
         FOR ALL ENTRIES IN lt_dl_item
       WHERE vbeln = lt_dl_item-vbeln
         AND vbtyp = if_sd_doc_category=>delivery.
    ENDIF.

    LOOP AT lt_dl_item ASSIGNING FIELD-SYMBOL(<ls_dl_item>).
      CLEAR:ls_dl_header.
      READ TABLE lt_dl_header INTO ls_dl_header
        WITH KEY vbeln = <ls_dl_item>-vbeln.
      IF sy-subrc = 0 AND ls_dl_header-lfart IN lt_delv_type.
        ls_dlv_data-vbeln = <ls_dl_item>-vbeln.
        ls_dlv_data-posnr = <ls_dl_item>-posnr.
        APPEND ls_dlv_data TO lt_dlv_data.
        CLEAR ls_dlv_data.
      ENDIF.
    ENDLOOP.

*   Merge the delivery data and delete the duplicated record
    SORT lt_dlv_data BY vbeln posnr.
    DELETE ADJACENT DUPLICATES FROM lt_dlv_data COMPARING ALL FIELDS.

    LOOP AT lt_dlv_data INTO ls_dlv_data.
      lv_locid2 = |{ ls_dlv_data-vbeln ALPHA = OUT }{ ls_dlv_data-posnr ALPHA = IN }|.
      CONDENSE lv_locid2 NO-GAPS.
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
           appsys            = mo_ef_parameters->get_appsys(  )
           appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
           language          = sy-langu
           appobjid          = is_app_objects-appobjid
           milestone         = zif_gtt_sof_constants=>cs_milestone-dlv_item_completed
           locid2            = lv_locid2
           milestonenum      = zcl_gtt_tools=>get_next_sequence_id(
                               it_expeventdata = ct_expeventdata )   ) ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SPOF_PO_TOOLS definition
  public
  create public .

public section.

  class-methods IS_APPROPRIATE_PO
    importing
      !IR_EKKO type ref to DATA
      !IR_EKPO type ref to DATA
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  class-methods IS_ENABLE_CONFIRMATION_PO_ITEM
    importing
      !IR_EKPO type ref to DATA
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  class-methods IS_APPROPRIATE_PO_ITEM
    importing
      !IR_EKKO type ref to DATA
      !IR_EKPO type ref to DATA
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  class-methods GET_TRACKING_ID_PO_HDR
    importing
      !IR_EKKO type ref to DATA
    returning
      value(RV_TRACK_ID) type /SAPTRX/TRXID
    raising
      CX_UDM_MESSAGE .
  class-methods GET_TRACKING_ID_PO_ITM
    importing
      !IR_EKPO type ref to DATA
    returning
      value(RV_TRACK_ID) type /SAPTRX/TRXID
    raising
      CX_UDM_MESSAGE .
  class-methods GET_PLANT_ADDRESS_NUM_AND_NAME
    importing
      !IV_WERKS type WERKS_D
    exporting
      !EV_ADRNR type ADRNR
      !EV_NAME type NAME1
    raising
      CX_UDM_MESSAGE .
  class-methods GET_ADDRESS_INFO
    importing
      !IV_ADDR_TYPE type AD_ADRTYPE default ZIF_GTT_SPOF_APP_CONSTANTS=>CS_ADRTYPE-ORGANIZATION
      !IV_ADDR_NUMB type AD_ADDRNUM
    exporting
      !EV_ADDRESS type CLIKE
      !EV_EMAIL type CLIKE
      !EV_TELEPHONE type CLIKE
    raising
      CX_UDM_MESSAGE .
  class-methods GET_SUPPLIER_AGENT_ID
    importing
      !IV_LIFNR type ELIFN
    returning
      value(RV_ID_NUM) type /SAPTRX/PARAMVAL200 .
  class-methods GET_FORMATED_MATNR
    importing
      !IR_EKPO type ref to DATA
    returning
      value(RV_MATNR) type CHAR40
    raising
      CX_UDM_MESSAGE .
  class-methods GET_PO_ITEM
    importing
      !I_EBELN type EBELN
      !I_EBELP type EBELP
      !I_ARCHIVED type BOOLE_D
      !IS_EKKO type EKKO
    exporting
      !ES_EKPO type EKPO
    raising
      CX_UDM_MESSAGE .
  class-methods GET_PO_HEADER
    importing
      !I_EBELN type EBELN
    exporting
      !ES_EKKO type EKKO
      !E_ARCHIVED type BOOLE_D
    raising
      CX_UDM_MESSAGE .
  class-methods GET_TRACKING_ID_DL_ITEM
    importing
      !IR_LIPS type ref to DATA
    returning
      value(RV_TRACK_ID) type /SAPTRX/TRXID
    raising
      CX_UDM_MESSAGE .
  class-methods GET_FORMATED_PO_NUMBER
    importing
      !IR_EKKO type ref to DATA
    returning
      value(RV_EBELN) type EBELN
    raising
      CX_UDM_MESSAGE .
  class-methods GET_FORMATED_PO_ITEM_NUMBER
    importing
      !IR_EKPO type ref to DATA
    returning
      value(RV_EBELP) type EBELP
    raising
      CX_UDM_MESSAGE .
  class-methods GET_PO_TYPE
    returning
      value(RT_TYPE) type RSELOPTION .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_GTT_SPOF_PO_TOOLS IMPLEMENTATION.


  METHOD get_address_info.
    DATA: lt_address   TYPE szadr_printform_table,
          ls_addr_comp TYPE szadr_addr1_complete.


    IF ev_address IS REQUESTED.
      CLEAR: ev_address.

      CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
        EXPORTING
          address_type                   = iv_addr_type
          address_number                 = iv_addr_numb
        IMPORTING
          address_printform_table        = lt_address
        EXCEPTIONS
          address_blocked                = 1
          person_blocked                 = 2
          contact_person_blocked         = 3
          addr_to_be_formated_is_blocked = 4
          OTHERS                         = 5.

      IF sy-subrc = 0.
        LOOP AT lt_address ASSIGNING FIELD-SYMBOL(<ls_address>).
          ev_address  = COND #( WHEN ev_address IS INITIAL
                                  THEN <ls_address>-address_line
                                  ELSE |{ ev_address }${ <ls_address>-address_line }| ).
        ENDLOOP.
      ELSE.
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ENDIF.

    IF ev_email IS REQUESTED OR ev_telephone IS REQUESTED.
      CLEAR: ev_email, ev_telephone.

      CALL FUNCTION 'ADDR_GET_COMPLETE'
        EXPORTING
          addrnumber              = iv_addr_numb
        IMPORTING
          addr1_complete          = ls_addr_comp
        EXCEPTIONS
          parameter_error         = 1
          address_not_exist       = 2
          internal_error          = 3
          wrong_access_to_archive = 4
          address_blocked         = 5
          OTHERS                  = 6.

      IF sy-subrc = 0.
        ev_email      = VALUE #( ls_addr_comp-adsmtp_tab[ 1 ]-adsmtp-smtp_addr OPTIONAL ).
        ev_telephone  = VALUE #( ls_addr_comp-adtel_tab[ 1 ]-adtel-tel_number OPTIONAL ).
      ELSE.
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_formated_matnr.
    DATA: lv_matnr TYPE ekpo-matnr.

    lv_matnr = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_ekpo
      iv_field_name  = 'MATNR' ).

    rv_matnr   = |{ lv_matnr ALPHA = OUT }|.
  ENDMETHOD.


  METHOD get_plant_address_num_and_name.
    DATA: ls_t001w TYPE t001w.

    CALL FUNCTION 'WCB_T001W_SINGLE_READ'
      EXPORTING
        i_werks   = iv_werks
      IMPORTING
        e_t001w   = ls_t001w
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc = 0.
      ev_adrnr    = ls_t001w-adrnr.
      ev_name    = ls_t001w-name1.
    ELSE.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_supplier_agent_id.
    DATA: lv_forward_agt TYPE bu_partner,
          lt_bpdetail    TYPE STANDARD TABLE OF bapibus1006_id_details.

    CALL METHOD cl_site_bp_assignment=>select_bp_via_cvi_link
      EXPORTING
        i_lifnr = iv_lifnr
      IMPORTING
        e_bp    = lv_forward_agt.

    CALL FUNCTION 'BAPI_IDENTIFICATIONDETAILS_GET'
      EXPORTING
        businesspartner      = lv_forward_agt
      TABLES
        identificationdetail = lt_bpdetail.

    READ TABLE lt_bpdetail ASSIGNING FIELD-SYMBOL(<ls_bpdetail>)
      WITH KEY identificationtype = zif_gtt_spof_app_constants=>cv_agent_id_type
      BINARY SEARCH.

    rv_id_num   = COND #( WHEN sy-subrc = 0
                            THEN zif_gtt_spof_app_constants=>cv_agent_id_prefix &&
                                 <ls_bpdetail>-identificationnumber ).
  ENDMETHOD.


  METHOD get_tracking_id_po_hdr.

    DATA: lv_ebeln TYPE ekko-ebeln.

    lv_ebeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_ekko
      iv_field_name  = 'EBELN' ).

    rv_track_id   = |{ lv_ebeln ALPHA = OUT }|.
  ENDMETHOD.


  METHOD get_tracking_id_po_itm.

    DATA: lv_ebeln TYPE ekpo-ebeln,
          lv_ebelp TYPE ekpo-ebelp.

    lv_ebeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_ekpo
      iv_field_name  = 'EBELN' ).

    lv_ebelp = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_ekpo
      iv_field_name  = 'EBELP' ).

    rv_track_id   = |{ lv_ebeln ALPHA = OUT }{ lv_ebelp ALPHA = IN }|.

    CONDENSE rv_track_id NO-GAPS.


  ENDMETHOD.


  METHOD is_appropriate_po.

    FIELD-SYMBOLS: <lt_ekpo>  TYPE ANY TABLE,
                   <ls_ekpo>  TYPE any,
                   <lv_ebeln> TYPE ekpo-ebeln.

    DATA:
      lv_ebeln TYPE ekko-ebeln,
      lv_bsart TYPE ekko-bsart.

    rv_result = abap_false.

    zcl_gtt_spof_po_tools=>get_po_type(
      RECEIVING
        rt_type = DATA(lt_po_type) ).

    CHECK lt_po_type IS NOT INITIAL.
    lv_bsart = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_ekko
      iv_field_name  = 'BSART' ).
    IF lv_bsart NOT IN lt_po_type.
      RETURN.
    ENDIF.

    lv_ebeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_ekko
      iv_field_name  = 'EBELN' ).
    ASSIGN ir_ekpo->* TO <lt_ekpo>.
    IF <lt_ekpo> IS ASSIGNED.
      LOOP AT <lt_ekpo> ASSIGNING <ls_ekpo>.
        ASSIGN COMPONENT 'EBELN' OF STRUCTURE <ls_ekpo> TO <lv_ebeln>.
        IF <lv_ebeln> IS ASSIGNED.
          IF <lv_ebeln>  = lv_ebeln AND
             zcl_gtt_spof_po_tools=>is_appropriate_po_item( ir_ekko = ir_ekko ir_ekpo = REF #( <ls_ekpo> ) ) = abap_true.
            rv_result = abap_true.
            RETURN.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKPO' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD is_appropriate_po_item.

    DATA:
      lv_bsart TYPE ekko-bsart.

    rv_result = abap_false.

    zcl_gtt_spof_po_tools=>get_po_type(
      RECEIVING
        rt_type = DATA(lt_po_type) ).

    CHECK lt_po_type IS NOT INITIAL.
    lv_bsart = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_ekko
      iv_field_name  = 'BSART' ).
    IF lv_bsart NOT IN lt_po_type.
      RETURN.
    ENDIF.

    DATA(lv_wepos) = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_ekpo
      iv_field_name  = 'WEPOS' ).

    rv_result = boolc( lv_wepos = abap_true ).

  ENDMETHOD.


  METHOD is_enable_confirmation_po_item.

    DATA(lv_kzabs) = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_ekpo
      iv_field_name  = 'KZABS' ).

    rv_result = boolc( lv_kzabs = abap_true ).

  ENDMETHOD.


  METHOD get_formated_po_item_number.
    rv_ebelp = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_ekpo
      iv_field_name  = 'EBELP' ).

    rv_ebelp   = |{ rv_ebelp ALPHA = IN }|.
  ENDMETHOD.


  METHOD get_formated_po_number.
    DATA: lv_ebeln TYPE ekko-ebeln.

    lv_ebeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = ir_ekko
      iv_field_name  = 'EBELN' ).

    rv_ebeln   = |{ lv_ebeln ALPHA = OUT }|.
  ENDMETHOD.


  METHOD get_po_header.
    DATA lv_bstyp TYPE bstyp.

    CLEAR es_ekko.
    CALL FUNCTION 'ME_EKKO_SINGLE_READ'
      EXPORTING
        pi_ebeln         = i_ebeln
      IMPORTING
        po_ekko          = es_ekko
      EXCEPTIONS
        no_records_found = 1
        OTHERS           = 2.
    IF sy-subrc GT 0.
* archive integration                                       "v_1624571
      IF cl_mmpur_archive=>if_mmpur_archive~get_archive_handle( i_ebeln )
        GT 0.
        DO 3 TIMES.
          CASE sy-index.
            WHEN 1.
              lv_bstyp = 'F'.
            WHEN 2.
              lv_bstyp = 'K'.
            WHEN 3.
              lv_bstyp = 'L'.
          ENDCASE.
          TRY.
              cl_mmpur_archive=>if_mmpur_archive~get_archived_pd(
                EXPORTING
                  im_ebeln = i_ebeln
                  im_bstyp = lv_bstyp
                IMPORTING
                  ex_ekko  = es_ekko ).
            CATCH cx_mmpur_no_authority.
              CLEAR es_ekko.
            CATCH cx_mmpur_root.
              CLEAR es_ekko.
          ENDTRY.
          CHECK es_ekko IS NOT INITIAL.
          e_archived = abap_true.
          EXIT.
        ENDDO.
        IF es_ekko IS INITIAL.
          MESSAGE e005(zgtt) WITH 'EKKO' i_ebeln INTO DATA(lv_dummy).
          zcl_gtt_tools=>throw_exception( ).
        ENDIF.
      ENDIF.                                                "^_1624571
    ENDIF.
  ENDMETHOD.


  METHOD get_po_item.
    DATA lt_items TYPE STANDARD TABLE OF ekpo.
    CLEAR es_ekpo.

    CALL FUNCTION 'ME_EKPO_READ_WITH_EBELN'
      EXPORTING
        pi_ebeln             = i_ebeln
      TABLES
        pto_ekpo             = lt_items
      EXCEPTIONS
        err_no_records_found = 1
        OTHERS               = 2.
    IF sy-subrc GT 0.
      IF i_archived EQ abap_true.                          "v_1624571
        TRY.
            cl_mmpur_archive=>if_mmpur_archive~get_archived_pd(
              EXPORTING
                im_ebeln = i_ebeln
                im_bstyp = is_ekko-bstyp
              IMPORTING
                ex_ekpo  = lt_items ).
          CATCH cx_mmpur_no_authority.
            CLEAR lt_items.
          CATCH cx_mmpur_root.
            CLEAR lt_items.
        ENDTRY.
      ENDIF.
      IF lines( lt_items ) EQ 0.
        MESSAGE e005(zgtt) WITH 'EKPO' |{ i_ebeln }{ i_ebelp }|
         INTO DATA(lv_dummy).
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.                                                "^_1624571
    ENDIF.
    READ TABLE lt_items WITH KEY ebeln = i_ebeln
                                 ebelp = i_ebelp
                        INTO es_ekpo.
    IF sy-subrc NE 0.
      MESSAGE e005(zgtt) WITH 'EKPO' |{ i_ebeln }{ i_ebelp }|
          INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
ENDMETHOD.


  method GET_TRACKING_ID_DL_ITEM.
     DATA: lv_vbeln TYPE lips-vbeln,
          lv_posnr TYPE lips-posnr.

    lv_vbeln  = zcl_gtt_tools=>get_field_of_structure(
                  ir_struct_data = ir_lips
                  iv_field_name  = 'VBELN' ).

    lv_posnr  = zcl_gtt_tools=>get_field_of_structure(
                  ir_struct_data = ir_lips
                  iv_field_name  = 'POSNR' ).

    rv_track_id   = |{ lv_vbeln ALPHA = OUT }{ lv_posnr ALPHA = IN }|.

    CONDENSE rv_track_id NO-GAPS.
  endmethod.


  METHOD get_po_type.

    DATA:
      lt_potype TYPE TABLE OF zgtt_potype_rst,
      rs_type   TYPE rsdsselopt.

    CLEAR rt_type.

    SELECT *
      INTO TABLE lt_potype
      FROM zgtt_potype_rst
     WHERE active = abap_true.

    LOOP AT lt_potype INTO DATA(ls_potype).
      rs_type-sign = 'I'.
      rs_type-option = 'EQ'.
      rs_type-low = ls_potype-bsart.
      APPEND rs_type TO rt_type.
      CLEAR rs_type.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_gtt_spof_tp_factory_po_hdr DEFINITION
  PUBLIC
  INHERITING FROM zcl_gtt_tp_factory
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_gtt_tp_factory~get_tp_reader
        REDEFINITION .
    METHODS zif_gtt_tp_factory~get_pe_filler
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SPOF_TP_FACTORY_PO_HDR IMPLEMENTATION.


  METHOD zif_gtt_tp_factory~get_pe_filler.
    ro_pe_filler = NEW zcl_gtt_spof_pe_filler_po_hdr(
      io_ef_parameters = io_ef_parameters
      io_bo_reader     = io_bo_reader ).
  ENDMETHOD.


  METHOD zif_gtt_tp_factory~get_tp_reader.

    ro_bo_reader = NEW zcl_gtt_spof_tp_reader_po_hdr(
      io_ef_parameters = io_ef_parameters ).

  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SPOF_TP_FACTORY_PO_HDR IMPLEMENTATION.


  METHOD zif_gtt_tp_factory~get_pe_filler.
    ro_pe_filler = NEW zcl_gtt_spof_pe_filler_po_hdr(
      io_ef_parameters = io_ef_parameters
      io_bo_reader     = io_bo_reader ).
  ENDMETHOD.


  METHOD zif_gtt_tp_factory~get_tp_reader.

    ro_bo_reader = NEW zcl_gtt_spof_tp_reader_po_hdr(
      io_ef_parameters = io_ef_parameters ).

  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_gtt_spof_tp_factory_po_itm DEFINITION
  PUBLIC
  INHERITING FROM zcl_gtt_tp_factory
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_gtt_tp_factory~get_pe_filler
        REDEFINITION .
    METHODS zif_gtt_tp_factory~get_tp_reader
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SPOF_TP_FACTORY_PO_ITM IMPLEMENTATION.


  METHOD zif_gtt_tp_factory~get_pe_filler.
    ro_pe_filler = NEW zcl_gtt_spof_pe_filler_po_itm(
      io_ef_parameters = io_ef_parameters
      io_bo_reader     = io_bo_reader ).
  ENDMETHOD.


  METHOD zif_gtt_tp_factory~get_tp_reader.

    ro_bo_reader = NEW zcl_gtt_spof_tp_reader_po_itm(
      io_ef_parameters = io_ef_parameters ).

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SPOF_TP_READER_PO_HDR definition
  public
  create public .

public section.

  interfaces ZIF_GTT_TP_READER .

  methods CONSTRUCTOR
    importing
      !IO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS .
protected section.

  types tt_otl_locid TYPE STANDARD TABLE OF vbpa-lifnr WITH EMPTY KEY .
  types tt_otl_loctype TYPE STANDARD TABLE OF char30 WITH EMPTY KEY .
  types tt_otl_timezone TYPE STANDARD TABLE OF addr1_data-time_zone WITH EMPTY KEY .
  types tt_otl_description TYPE STANDARD TABLE OF addr1_data-name1 WITH EMPTY KEY .
  types tt_otl_country_code TYPE STANDARD TABLE OF addr1_data-country WITH EMPTY KEY .
  types tt_otl_city_name TYPE STANDARD TABLE OF addr1_data-city1 WITH EMPTY KEY .
  types tt_otl_region_code TYPE STANDARD TABLE OF addr1_data-region WITH EMPTY KEY .
  types tt_otl_house_number TYPE STANDARD TABLE OF addr1_data-house_num1 WITH EMPTY KEY .
  types tt_otl_street_name TYPE STANDARD TABLE OF addr1_data-street WITH EMPTY KEY .
  types tt_otl_postal_code TYPE STANDARD TABLE OF addr1_data-post_code1 WITH EMPTY KEY .
  types tt_otl_email_address TYPE STANDARD TABLE OF ad_smtpadr WITH EMPTY KEY .
  types tt_otl_phone_number TYPE STANDARD TABLE OF char50 WITH EMPTY KEY .
private section.

  types TV_ITEM_NUM type I .
  types TV_IND_PO_ITM_DEL type ABAP_BOOL .
  types TV_IND_PO_ITM_NO type CHAR20 .
  types TV_IND_PO_NO type CHAR10 .
  types TV_IN_DLV_LINE_NO type CHAR4 .
  types TV_IN_DLV_NO type VBELN_VL .
  types:
    tt_ind_po_itm_del   TYPE STANDARD TABLE OF tv_ind_po_itm_del
                            WITH EMPTY KEY .
  types:
    tt_ind_po_itm_no  TYPE STANDARD TABLE OF tv_ind_po_itm_no
                            WITH EMPTY KEY .
  types:
    tt_ind_po_no   TYPE STANDARD TABLE OF tv_ind_po_no
                            WITH EMPTY KEY .
  types:
    tt_item_num TYPE STANDARD TABLE OF tv_item_num WITH EMPTY KEY .
  types TV_EBELP type CHAR15 .
  types:
    tt_ebelp TYPE STANDARD TABLE OF tv_ebelp WITH EMPTY KEY .
  types:
    tt_in_dlv_line_no TYPE STANDARD TABLE OF tv_in_dlv_line_no WITH EMPTY KEY .
  types:
    tt_in_dlv_no      TYPE STANDARD TABLE OF tv_in_dlv_no WITH EMPTY KEY .
  types:
    BEGIN OF ts_po_header,
      ebeln             TYPE ekko-ebeln,
      bsart             TYPE ekko-bsart,
      lifnr             TYPE ekko-lifnr,
      lifnr_lt          TYPE /saptrx/loc_id_type,
      werks             TYPE ekpo-werks,
      werks_lt          TYPE /saptrx/loc_id_type,
      adrnr             TYPE ekpo-adrnr,
      eindt             TYPE eket-eindt,
      netwr             TYPE ekpo-netwr,
      waers             TYPE ekko-waers,
      inco1             TYPE ekko-inco1,
      incov             TYPE ekko-incov,
      inco2_l           TYPE ekko-inco2_l,
      bedat             TYPE ekko-bedat,
      ihrez             TYPE ekko-ihrez,
      aedat             TYPE ekko-aedat,
      item_num          TYPE tt_item_num,
      ebelp             TYPE tt_ebelp,
      supplier_lbn_id   TYPE /saptrx/paramval200,
      rec_addr          TYPE /saptrx/paramval200,
      ind_po_no         TYPE tt_ind_po_no,
      ind_po_itm_no     TYPE tt_ind_po_itm_no,
      ind_po_itm_del    TYPE tt_ind_po_itm_del,
      in_dlv_line_no    TYPE tt_in_dlv_line_no,
      in_dlv_no         TYPE tt_in_dlv_no,
      out_dlv_line_no   TYPE tt_in_dlv_line_no,
      out_dlv_no        TYPE tt_in_dlv_no,
      otl_locid         TYPE tt_otl_locid,
      otl_loctype       TYPE tt_otl_loctype,
      otl_timezone      TYPE tt_otl_timezone,
      otl_description   TYPE tt_otl_description,
      otl_country_code  TYPE tt_otl_country_code,
      otl_city_name     TYPE tt_otl_city_name,
      otl_region_code   TYPE tt_otl_region_code,
      otl_house_number  TYPE tt_otl_house_number,
      otl_street_name   TYPE tt_otl_street_name,
      otl_postal_code   TYPE tt_otl_postal_code,
      otl_email_address TYPE tt_otl_email_address,
      otl_phone_number  TYPE tt_otl_phone_number,
    END OF ts_po_header .

  constants:
    BEGIN OF cs_mapping,
      ebeln             TYPE /saptrx/paramname VALUE 'YN_PO_NUMBER',
      bsart             TYPE /saptrx/paramname VALUE 'YN_PO_DOCUMENT_TYPE',
      lifnr             TYPE /saptrx/paramname VALUE 'YN_PO_SUPPLIER_ID',
      lifnr_lt          TYPE /saptrx/paramname VALUE 'YN_PO_SUPPLIER_LOC_TYPE',
      werks             TYPE /saptrx/paramname VALUE 'YN_PO_RECEIVING_LOCATION',
      werks_lt          TYPE /saptrx/paramname VALUE 'YN_PO_RECEIVING_LOC_TYPE',
      adrnr             TYPE /saptrx/paramname VALUE 'YN_PO_ADDRESS_NUMBER',
      eindt             TYPE /saptrx/paramname VALUE 'YN_PO_DELIVERY_DATE',
      netwr             TYPE /saptrx/paramname VALUE 'YN_PO_NET_VALUE',
      waers             TYPE /saptrx/paramname VALUE 'YN_PO_CURRENCY',
      inco1             TYPE /saptrx/paramname VALUE 'YN_PO_INCOTERMS',
      incov             TYPE /saptrx/paramname VALUE 'YN_PO_INCOTERMS_VERSION',
      inco2_l           TYPE /saptrx/paramname VALUE 'YN_PO_INCOTERMS_LOCATION',
      bedat             TYPE /saptrx/paramname VALUE 'YN_PO_DOCUMENT_DATE',
      ihrez             TYPE /saptrx/paramname VALUE 'YN_PO_YOUR_REFERENCE',
      aedat             TYPE /saptrx/paramname VALUE 'YN_PO_CREATION_AT',
      item_num          TYPE /saptrx/paramname VALUE 'YN_PO_HDR_ITM_LINE_COUNT',
      ebelp             TYPE /saptrx/paramname VALUE 'YN_PO_HDR_ITM_NO',
      supplier_lbn_id   TYPE /saptrx/paramname VALUE 'YN_PO_SUPPLIER_LBN_ID',
      rec_addr          TYPE /saptrx/paramname VALUE 'YN_PO_RECEIVING_ADDRESS',
      ind_po_no         TYPE /saptrx/paramname VALUE 'YN_PO_IND_PO_NO',
      ind_po_itm_no     TYPE /saptrx/paramname VALUE 'YN_PO_IND_PO_ITEM_NO',
      ind_po_itm_del    TYPE /saptrx/paramname VALUE 'YN_PO_IND_PO_ITEM_DELETED',
      in_dlv_line_no    TYPE /saptrx/paramname VALUE 'YN_IDLV_LINE_NO',
      in_dlv_no         TYPE /saptrx/paramname VALUE 'YN_IDLV_NO',
      out_dlv_line_no   TYPE /saptrx/paramname VALUE 'YN_ODLV_LINE_NO',
      out_dlv_no        TYPE /saptrx/paramname VALUE 'YN_ODLV_NO',
      otl_locid         TYPE /saptrx/paramname VALUE 'GTT_OTL_LOCID',
      otl_loctype       TYPE /saptrx/paramname VALUE 'GTT_OTL_LOCTYPE',
      otl_timezone      TYPE /saptrx/paramname VALUE 'GTT_OTL_TIMEZONE',
      otl_description   TYPE /saptrx/paramname VALUE 'GTT_OTL_DESCRIPTION',
      otl_country_code  TYPE /saptrx/paramname VALUE 'GTT_OTL_COUNTRY_CODE',
      otl_city_name     TYPE /saptrx/paramname VALUE 'GTT_OTL_CITY_NAME',
      otl_region_code   TYPE /saptrx/paramname VALUE 'GTT_OTL_REGION_CODE',
      otl_house_number  TYPE /saptrx/paramname VALUE 'GTT_OTL_HOUSE_NUMBER',
      otl_street_name   TYPE /saptrx/paramname VALUE 'GTT_OTL_STREET_NAME',
      otl_postal_code   TYPE /saptrx/paramname VALUE 'GTT_OTL_POSTAL_CODE',
      otl_email_address TYPE /saptrx/paramname VALUE 'GTT_OTL_EMAIL_ADDRESS',
      otl_phone_number  TYPE /saptrx/paramname VALUE 'GTT_OTL_PHONE_NUMBER',
    END OF cs_mapping .
  data MO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS .

  methods IS_OBJECT_CHANGED
    importing
      !IS_APP_OBJECT type TRXAS_APPOBJ_CTAB_WA
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  methods FILL_HEADER_FROM_EKKO_STRUCT
    importing
      !IR_EKKO type ref to DATA
    changing
      !CS_PO_HEADER type TS_PO_HEADER
    raising
      CX_UDM_MESSAGE .
  methods FILL_HEADER_FROM_EKPO_TABLE
    importing
      !IV_EBELN type EBELN
      !IR_EKKO type ref to DATA
      !IR_EKPO type ref to DATA
    changing
      !CS_PO_HEADER type TS_PO_HEADER
    raising
      CX_UDM_MESSAGE .
  methods FILL_HEADER_FROM_EKET_TABLE
    importing
      !IV_EBELN type EBELN
      !IR_EKPO type ref to DATA
      !IR_EKET type ref to DATA
    changing
      !CS_PO_HEADER type TS_PO_HEADER
    raising
      CX_UDM_MESSAGE .
  methods FILL_HEADER_LOCATION_TYPES
    changing
      !CS_PO_HEADER type TS_PO_HEADER
    raising
      CX_UDM_MESSAGE .
  methods FILL_HEADER_FROM_EKKO_TABLE
    importing
      !IV_EBELN type EBELN
      !IR_EKKO type ref to DATA
    changing
      !CS_PO_HEADER type TS_PO_HEADER
    raising
      CX_UDM_MESSAGE .
  methods FILL_HEADER_SUPPLIER_LBN_ID
    changing
      !CS_PO_HEADER type TS_PO_HEADER .
  methods FILL_HEADER_RECEIVING_ADDRESS
    changing
      !CS_PO_HEADER type TS_PO_HEADER .
  methods FORMAT_TO_REMOVE_LEADING_ZERO
    changing
      !CS_PO_HEADER type TS_PO_HEADER .
  methods FILL_INBOUND_DLV_TABLE
    importing
      !IV_EBELN type EBELN
    changing
      !CS_PO_HEADER type TS_PO_HEADER
    raising
      CX_UDM_MESSAGE .
  methods FILL_ONE_TIME_LOCATION
    importing
      !IR_EKKO type ref to DATA
    changing
      !CS_PO_HEADER type TS_PO_HEADER
    raising
      CX_UDM_MESSAGE .
  methods FILL_ONE_TIME_LOCATION_OLD
    importing
      !IV_EBELN type EBELN
      !IR_EKKO type ref to DATA
    changing
      !CS_PO_HEADER type TS_PO_HEADER
    raising
      CX_UDM_MESSAGE .
  methods FILL_OUTBOUND_DLV_TABLE
    importing
      !IV_EBELN type EBELN
    changing
      !CS_PO_HEADER type TS_PO_HEADER
    raising
      CX_UDM_MESSAGE .
  methods FILL_DLV_TABLE
    importing
      !IV_EBELN type EBELN
      !IV_VBTYP type VBTYPL
    exporting
      !ET_LINE_NO type TT_IN_DLV_LINE_NO
      !ET_DLV_NO type TT_IN_DLV_NO
    raising
      CX_UDM_MESSAGE .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SPOF_TP_READER_PO_HDR IMPLEMENTATION.


  METHOD constructor.

    mo_ef_parameters    = io_ef_parameters.

  ENDMETHOD.


  METHOD fill_header_from_eket_table.

    DATA: lt_ebelp_rng TYPE RANGE OF ekpo-ebelp,
          lv_eindt_max TYPE eket-eindt,
          lv_eindt_set TYPE abap_bool VALUE abap_false.

    FIELD-SYMBOLS: <lt_ekpo> TYPE zif_gtt_spof_app_types=>tt_uekpo,
                   <ls_ekpo> TYPE zif_gtt_spof_app_types=>ts_uekpo,
                   <lt_eket> TYPE zif_gtt_spof_app_types=>tt_ueket,
                   <ls_eket> TYPE zif_gtt_spof_app_types=>ts_ueket.


    CLEAR: cs_po_header-eindt.

    ASSIGN ir_ekpo->* TO <lt_ekpo>.
    ASSIGN ir_eket->* TO <lt_eket>.

    IF <lt_ekpo> IS ASSIGNED AND
       <lt_eket> IS ASSIGNED.
      CLEAR cs_po_header-eindt.

      " Preparation of Active Items List
      LOOP AT <lt_ekpo> ASSIGNING <ls_ekpo>
        WHERE ebeln  = iv_ebeln
          AND loekz <> zif_gtt_spof_app_constants=>cs_loekz-deleted.

        lt_ebelp_rng  = VALUE #( BASE lt_ebelp_rng
                                 ( low    = <ls_ekpo>-ebelp
                                   option = 'EQ'
                                   sign   = 'I' ) ).
      ENDLOOP.

      " keep Latest Delivery Date in schedule lines per item.

      LOOP AT <lt_eket> ASSIGNING <ls_eket>
        WHERE ebeln  = iv_ebeln
          AND ebelp IN lt_ebelp_rng
          AND kz <> zif_gtt_ef_constants=>cs_change_mode-delete
        GROUP BY ( ebeln = <ls_eket>-ebeln
                   ebelp = <ls_eket>-ebelp )
        ASCENDING
        ASSIGNING FIELD-SYMBOL(<ls_eket_group>).

        CLEAR: lv_eindt_max.

        LOOP AT GROUP <ls_eket_group> ASSIGNING FIELD-SYMBOL(<ls_eket_items>).
          lv_eindt_max    = COND #( WHEN <ls_eket_items>-eindt > lv_eindt_max
                                      THEN <ls_eket_items>-eindt
                                      ELSE lv_eindt_max ).
        ENDLOOP.

        IF lv_eindt_set = abap_false.
          cs_po_header-eindt  = lv_eindt_max.
          lv_eindt_set        = abap_true.
        ELSEIF cs_po_header-eindt < lv_eindt_max.
          cs_po_header-eindt = lv_eindt_max.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKET' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_header_from_ekko_struct.
    FIELD-SYMBOLS: <ls_ekko>  TYPE any.

    ASSIGN ir_ekko->* TO <ls_ekko>.

    IF <ls_ekko> IS ASSIGNED.
      MOVE-CORRESPONDING <ls_ekko> TO cs_po_header.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKKO' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_header_from_ekko_table.
    DATA: lv_dummy    TYPE char100.

    FIELD-SYMBOLS: <lt_ekko>  TYPE ANY TABLE,
                   <ls_ekko>  TYPE any,
                   <lv_ebeln> TYPE any.

    CLEAR: cs_po_header-werks, cs_po_header-netwr.

    ASSIGN ir_ekko->* TO <lt_ekko>.

    IF <lt_ekko> IS ASSIGNED.
      LOOP AT <lt_ekko> ASSIGNING <ls_ekko>.
        ASSIGN COMPONENT 'EBELN' OF STRUCTURE <ls_ekko> TO <lv_ebeln>.

        IF <lv_ebeln> IS ASSIGNED.
          " is it a record I need?
          IF <lv_ebeln>  = iv_ebeln.
            MOVE-CORRESPONDING <ls_ekko> TO cs_po_header.
            EXIT.
          ENDIF.
        ELSE.
          MESSAGE e001(zgtt) WITH 'EBELN' 'EKKO' INTO lv_dummy.
          zcl_gtt_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKKO' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_header_from_ekpo_table.

    DATA: lv_item_num TYPE tv_item_num VALUE 0,
          lv_fname    TYPE char5,
          lv_dummy    TYPE char100.

    FIELD-SYMBOLS: <lt_ekpo>  TYPE ANY TABLE,
                   <lt_ekko>  TYPE ANY TABLE,
                   <ls_ekpo>  TYPE any,
                   <ls_ekko>  TYPE any,
                   <lv_ebeln> TYPE any,
                   <lv_ebelp> TYPE any,
                   <lv_loekz> TYPE any,
                   <lv_werks> TYPE any,
                   <lv_netwr> TYPE any,
                   <lv_adrnr> TYPE any.

    CLEAR: cs_po_header-werks,
           cs_po_header-adrnr,
           cs_po_header-netwr,
           cs_po_header-item_num[],
           cs_po_header-ebelp[],
           cs_po_header-ind_po_no[],
           cs_po_header-ind_po_itm_no[],
           cs_po_header-ind_po_itm_del[].

    ASSIGN ir_ekko->* TO <lt_ekko>.
    ASSIGN ir_ekpo->* TO <lt_ekpo>.

    IF <lt_ekpo> IS ASSIGNED AND <lt_ekko> IS ASSIGNED.
      LOOP AT <lt_ekpo> ASSIGNING <ls_ekpo>.
        ASSIGN COMPONENT 'EBELN' OF STRUCTURE <ls_ekpo> TO <lv_ebeln>.
        ASSIGN COMPONENT 'EBELP' OF STRUCTURE <ls_ekpo> TO <lv_ebelp>.
        ASSIGN COMPONENT 'LOEKZ' OF STRUCTURE <ls_ekpo> TO <lv_loekz>.
        ASSIGN COMPONENT 'WERKS' OF STRUCTURE <ls_ekpo> TO <lv_werks>.
        ASSIGN COMPONENT 'NETWR' OF STRUCTURE <ls_ekpo> TO <lv_netwr>.
        ASSIGN COMPONENT 'ADRNR' OF STRUCTURE <ls_ekpo> TO <lv_adrnr>.

        IF <lv_ebeln> IS ASSIGNED AND
           <lv_ebelp> IS ASSIGNED AND
           <lv_loekz> IS ASSIGNED AND
           <lv_werks> IS ASSIGNED AND
           <lv_netwr> IS ASSIGNED.

          LOOP AT <lt_ekko> ASSIGNING <ls_ekko>.
            ASSIGN COMPONENT 'EBELN' OF STRUCTURE <ls_ekko> TO FIELD-SYMBOL(<lv_ekko_ebeln>).
            IF <lv_ekko_ebeln> IS ASSIGNED.
              IF <lv_ekko_ebeln> = <lv_ebeln>.
                EXIT.
              ELSE.
                UNASSIGN <ls_ekko>.
              ENDIF.
            ENDIF.
          ENDLOOP.

          IF <lv_ebeln>  = iv_ebeln AND <ls_ekko> IS ASSIGNED AND
             zcl_gtt_spof_po_tools=>is_appropriate_po_item( ir_ekko = REF #( <ls_ekko> ) ir_ekpo = REF #( <ls_ekpo> ) ) = abap_true.

            " Add PO Item number into result table
            ADD 1 TO lv_item_num.
            APPEND lv_item_num TO cs_po_header-item_num.

            " Add composition (PO Number PO Item position) into result table

            APPEND zcl_gtt_spof_po_tools=>get_tracking_id_po_itm( ir_ekpo = REF #( <ls_ekpo> ) ) TO cs_po_header-ebelp.
            APPEND zcl_gtt_spof_po_tools=>get_tracking_id_po_hdr( ir_ekko = REF #( <ls_ekpo> ) ) TO cs_po_header-ind_po_no.
            APPEND <lv_ebelp> TO cs_po_header-ind_po_itm_no.
            " Is item not deleted (active or blocked)?
            IF <lv_loekz> <> zif_gtt_spof_app_constants=>cs_loekz-deleted.
              " Plant ID, keep empty in case of different receiving plants on item level
              cs_po_header-werks  = COND #( WHEN sy-tabix = 1 OR
                                                 <lv_werks>  = cs_po_header-werks
                                              THEN <lv_werks> ).

              cs_po_header-adrnr  = COND #( WHEN cs_po_header-werks IS NOT INITIAL AND
                                                 <lv_adrnr> IS ASSIGNED
                                              THEN <lv_adrnr> ).
              " Sum of net values on item level
              ADD <lv_netwr> TO cs_po_header-netwr.
              APPEND abap_false TO cs_po_header-ind_po_itm_del.
            ELSE.
              APPEND abap_true TO cs_po_header-ind_po_itm_del.
            ENDIF.
          ENDIF.
        ELSE.
          lv_fname  = COND #( WHEN <lv_ebeln> IS NOT ASSIGNED THEN 'EBELN'
                              WHEN <lv_ebelp> IS NOT ASSIGNED THEN 'EBELP'
                              WHEN <lv_loekz> IS NOT ASSIGNED THEN 'LOEKZ'
                              WHEN <lv_werks> IS NOT ASSIGNED THEN 'WERKS'
                                ELSE 'NETWR' ).
          MESSAGE e001(zgtt) WITH lv_fname 'EKPO' INTO lv_dummy.
          zcl_gtt_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.

      cs_po_header-netwr = zcl_gtt_tools=>convert_to_external_amount(
        iv_currency = cs_po_header-waers
        iv_internal = cs_po_header-netwr ).
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKPO' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_header_location_types.

    IF cs_po_header-lifnr IS NOT INITIAL.
      cs_po_header-lifnr_lt  = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
    ENDIF.
    cs_po_header-werks_lt  = zif_gtt_ef_constants=>cs_loc_types-plant.

  ENDMETHOD.


  METHOD fill_header_receiving_address.

    cs_po_header-adrnr  = COND #( WHEN cs_po_header-adrnr IS NOT INITIAL
                                    THEN cs_po_header-adrnr ).

    IF cs_po_header-adrnr IS INITIAL AND cs_po_header-werks IS NOT INITIAL.
      TRY.
          zcl_gtt_spof_po_tools=>get_plant_address_num_and_name(
            EXPORTING
              iv_werks = cs_po_header-werks
            IMPORTING
              ev_adrnr = cs_po_header-adrnr
          ).
        CATCH cx_udm_message.
      ENDTRY.
    ENDIF.

    IF cs_po_header-adrnr IS NOT INITIAL.
      TRY.
          zcl_gtt_spof_po_tools=>get_address_info(
            EXPORTING
              iv_addr_numb = cs_po_header-adrnr
            IMPORTING
              ev_address   = cs_po_header-rec_addr ).
        CATCH cx_udm_message.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD fill_header_supplier_lbn_id.
    IF cs_po_header-lifnr IS NOT INITIAL.

      cs_po_header-supplier_lbn_id = zcl_gtt_spof_po_tools=>get_supplier_agent_id( iv_lifnr = cs_po_header-lifnr ).

    ENDIF.
  ENDMETHOD.


  METHOD format_to_remove_leading_zero.
    TRY.
        cs_po_header-ebeln = zcl_gtt_spof_po_tools=>get_tracking_id_po_hdr( ir_ekko = REF #( cs_po_header ) ).
        cs_po_header-lifnr = zcl_gtt_tools=>get_pretty_location_id(
          iv_locid   = cs_po_header-lifnr
          iv_loctype = cs_po_header-lifnr_lt ).
        cs_po_header-werks = zcl_gtt_tools=>get_pretty_location_id(
          iv_locid   = cs_po_header-werks
          iv_loctype = cs_po_header-werks_lt ).
      CATCH cx_udm_message.
    ENDTRY.
  ENDMETHOD.


  METHOD is_object_changed.

    rv_result = zcl_gtt_tools=>is_object_changed(
      is_app_object    = is_app_object
      io_ef_parameters = mo_ef_parameters
      it_check_tables  = VALUE #( ( zif_gtt_spof_app_constants=>cs_tabledef-po_item_new )
                                  ( zif_gtt_spof_app_constants=>cs_tabledef-po_item_old )
                                  ( zif_gtt_spof_app_constants=>cs_tabledef-po_sched_new )
                                  ( zif_gtt_spof_app_constants=>cs_tabledef-po_sched_old ) )
      iv_key_field     = 'EBELN'
      iv_upd_field     = 'KZ' ).

  ENDMETHOD.


  METHOD zif_gtt_tp_reader~check_relevance.

    rv_result   = zif_gtt_ef_constants=>cs_condition-false.

    IF zcl_gtt_spof_po_tools=>is_appropriate_po(
          ir_ekko = is_app_object-maintabref
          ir_ekpo = mo_ef_parameters->get_appl_table( iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_new )
          ) = abap_true.

      CASE is_app_object-update_indicator.
        WHEN zif_gtt_ef_constants=>cs_change_mode-insert.
          rv_result   = zif_gtt_ef_constants=>cs_condition-true.
        WHEN zif_gtt_ef_constants=>cs_change_mode-update OR
             zif_gtt_ef_constants=>cs_change_mode-undefined.
          rv_result   = zcl_gtt_tools=>are_structures_different(
                          ir_data1  = zif_gtt_tp_reader~get_data(
                                        is_app_object = is_app_object )
                          ir_data2  = zif_gtt_tp_reader~get_data_old(
                                        is_app_object = is_app_object ) ).
      ENDCASE.

      IF rv_result = zif_gtt_ef_constants=>cs_condition-false.
        IF is_object_changed( is_app_object = is_app_object ) = abap_true.
          rv_result   = zif_gtt_ef_constants=>cs_condition-true.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  method ZIF_GTT_TP_READER~GET_APP_OBJ_TYPE_ID.
    rv_appobjid   = zcl_gtt_spof_po_tools=>get_tracking_id_po_hdr(
                        ir_ekko = is_app_object-maintabref ).
  endmethod.


  METHOD zif_gtt_tp_reader~get_data.
    FIELD-SYMBOLS: <ls_header>      TYPE ts_po_header.
    DATA:
      lv_ebeln TYPE ekko-ebeln.

    lv_ebeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_object-maintabref
      iv_field_name  = 'EBELN' ).

    rr_data   = NEW ts_po_header( ).

    ASSIGN rr_data->* TO <ls_header>.

    fill_header_from_ekko_struct(
      EXPORTING
        ir_ekko      = is_app_object-maintabref
      CHANGING
        cs_po_header = <ls_header> ).

    fill_header_from_ekpo_table(
      EXPORTING
        iv_ebeln      = <ls_header>-ebeln
        ir_ekko       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_header_new )
        ir_ekpo       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_new )
      CHANGING
        cs_po_header  = <ls_header> ).

    fill_header_from_eket_table(
      EXPORTING
        iv_ebeln      = <ls_header>-ebeln
        ir_ekpo       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_new )
        ir_eket       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_sched_new )
      CHANGING
        cs_po_header  = <ls_header> ).

    fill_header_location_types(
      CHANGING
        cs_po_header = <ls_header> ).

    fill_header_supplier_lbn_id(
      CHANGING
        cs_po_header = <ls_header> ).

    " Receiving address
    fill_header_receiving_address(
      CHANGING
        cs_po_header = <ls_header> ).

    fill_inbound_dlv_table(
      EXPORTING
        iv_ebeln     = lv_ebeln
      CHANGING
        cs_po_header = <ls_header> ).

    fill_outbound_dlv_table(
      EXPORTING
        iv_ebeln     = lv_ebeln
      CHANGING
        cs_po_header = <ls_header> ).

    fill_one_time_location(
      EXPORTING
        ir_ekko      = is_app_object-maintabref
      CHANGING
        cs_po_header = <ls_header> ).

    format_to_remove_leading_zero(
      CHANGING
        cs_po_header = <ls_header> ).

    IF <ls_header>-otl_locid IS INITIAL.
      APPEND INITIAL LINE TO <ls_header>-otl_locid.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_tp_reader~get_data_old.
    FIELD-SYMBOLS: <ls_header>      TYPE ts_po_header.
    DATA:
      lv_ebeln TYPE ekko-ebeln.

    lv_ebeln = zcl_gtt_tools=>get_field_of_structure(
      ir_struct_data = is_app_object-maintabref
      iv_field_name  = 'EBELN' ).

    rr_data   = NEW ts_po_header( ).
    ASSIGN rr_data->* TO <ls_header>.

    fill_header_from_ekko_table(
      EXPORTING
        iv_ebeln      = lv_ebeln
        ir_ekko       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_header_old )
      CHANGING
        cs_po_header  = <ls_header> ).

    fill_header_from_ekpo_table(
      EXPORTING
        iv_ebeln      = <ls_header>-ebeln
        ir_ekko       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_header_old )
        ir_ekpo       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_old )
      CHANGING
        cs_po_header  = <ls_header> ).

    fill_header_from_eket_table(
      EXPORTING
        iv_ebeln      = <ls_header>-ebeln
        ir_ekpo       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_old )
        ir_eket       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_sched_old )
      CHANGING
        cs_po_header  = <ls_header> ).

    fill_header_location_types(
      CHANGING
        cs_po_header = <ls_header> ).

    fill_header_supplier_lbn_id(
      CHANGING
        cs_po_header = <ls_header> ).

    " Receiving address
    fill_header_receiving_address(
      CHANGING
        cs_po_header = <ls_header> ).

    fill_inbound_dlv_table(
      EXPORTING
        iv_ebeln     = lv_ebeln
      CHANGING
        cs_po_header = <ls_header> ).

    fill_outbound_dlv_table(
      EXPORTING
        iv_ebeln     = lv_ebeln
      CHANGING
        cs_po_header = <ls_header> ).

    fill_one_time_location_old(
      EXPORTING
        iv_ebeln     = lv_ebeln
        ir_ekko      = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_header_old )
      CHANGING
        cs_po_header = <ls_header> ).

    format_to_remove_leading_zero(
      CHANGING
        cs_po_header = <ls_header> ).

    IF <ls_header>-otl_locid IS INITIAL.
      APPEND INITIAL LINE TO <ls_header>-otl_locid.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_tp_reader~get_field_parameter.
    CASE iv_parameter.
      WHEN zif_gtt_ef_constants=>cs_parameter_id-key_field.
        rv_result   = boolc( iv_field_name = cs_mapping-item_num ).
      WHEN OTHERS.
        CLEAR: rv_result.
    ENDCASE.
  ENDMETHOD.


  method ZIF_GTT_TP_READER~GET_MAPPING_STRUCTURE.
     rr_data   = REF #( cs_mapping ).
  endmethod.


  method ZIF_GTT_TP_READER~GET_TRACK_ID_DATA.
    CLEAR et_track_id_data.
    et_track_id_data = VALUE #( BASE et_track_id_data (
        appsys      = mo_ef_parameters->get_appsys( )
        appobjtype  = is_app_object-appobjtype
        appobjid    = is_app_object-appobjid
        trxcod      = zif_gtt_ef_constants=>cs_trxcod-po_number
        trxid       = zcl_gtt_spof_po_tools=>get_tracking_id_po_hdr(
                        ir_ekko = is_app_object-maintabref )
        timzon      = zcl_gtt_tools=>get_system_time_zone( )
        msrid       = space
      ) ).
  endmethod.


  METHOD fill_inbound_dlv_table.

    fill_dlv_table(
      EXPORTING
        iv_ebeln   = iv_ebeln
        iv_vbtyp   = if_sd_doc_category=>delivery_shipping_notif
      IMPORTING
        et_line_no = cs_po_header-in_dlv_line_no
        et_dlv_no  = cs_po_header-in_dlv_no ).

  ENDMETHOD.


  METHOD fill_one_time_location.

    FIELD-SYMBOLS:
      <ls_ekko> TYPE /saptrx/mm_po_hdr.

    DATA:
      ls_loc_addr  TYPE addr1_data,
      lv_loc_email TYPE ad_smtpadr,
      lv_loc_tel   TYPE char50.

*   Vendor address
    ASSIGN ir_ekko->* TO <ls_ekko>.
    IF <ls_ekko> IS ASSIGNED.
      IF <ls_ekko>-adrnr IS NOT INITIAL AND <ls_ekko>-lifnr IS NOT INITIAL.
        zcl_gtt_tools=>get_address_from_memory(
          EXPORTING
            iv_addrnumber = <ls_ekko>-adrnr
          IMPORTING
            es_addr       = ls_loc_addr
            ev_email      = lv_loc_email
            ev_telephone  = lv_loc_tel ).

        APPEND |{ <ls_ekko>-lifnr ALPHA = OUT }| TO cs_po_header-otl_locid.
        APPEND zif_gtt_ef_constants=>cs_loc_types-businesspartner TO cs_po_header-otl_loctype.
        APPEND ls_loc_addr-time_zone TO cs_po_header-otl_timezone.
        APPEND ls_loc_addr-name1 TO cs_po_header-otl_description.
        APPEND ls_loc_addr-country TO cs_po_header-otl_country_code.
        APPEND ls_loc_addr-city1 TO cs_po_header-otl_city_name.
        APPEND ls_loc_addr-region TO cs_po_header-otl_region_code.
        APPEND ls_loc_addr-house_num1 TO cs_po_header-otl_house_number.
        APPEND ls_loc_addr-street TO cs_po_header-otl_street_name.
        APPEND ls_loc_addr-post_code1 TO cs_po_header-otl_postal_code.
        APPEND lv_loc_email TO cs_po_header-otl_email_address.
        APPEND lv_loc_tel TO cs_po_header-otl_phone_number.
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKKO' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_one_time_location_old.

    FIELD-SYMBOLS:
      <ls_ekko> TYPE /saptrx/mm_po_hdr,
      <lt_ekko> TYPE ANY TABLE.

    DATA:
      ls_loc_addr  TYPE addr1_data,
      lv_loc_email TYPE ad_smtpadr,
      lv_loc_tel   TYPE char50.

*   Vendor address
    ASSIGN ir_ekko->* TO <lt_ekko>.
    IF <lt_ekko> IS ASSIGNED.
      LOOP AT <lt_ekko> ASSIGNING <ls_ekko>.
        IF <ls_ekko>-ebeln = iv_ebeln AND <ls_ekko>-adrnr IS NOT INITIAL AND <ls_ekko>-lifnr IS NOT INITIAL.
          zcl_gtt_tools=>get_address_from_db(
            EXPORTING
              iv_addrnumber = <ls_ekko>-adrnr
            IMPORTING
              es_addr       = ls_loc_addr
              ev_email      = lv_loc_email
              ev_telephone  = lv_loc_tel ).

          APPEND |{ <ls_ekko>-lifnr ALPHA = OUT }| TO cs_po_header-otl_locid.
          APPEND zif_gtt_ef_constants=>cs_loc_types-businesspartner TO cs_po_header-otl_loctype.
          APPEND ls_loc_addr-time_zone TO cs_po_header-otl_timezone.
          APPEND ls_loc_addr-name1 TO cs_po_header-otl_description.
          APPEND ls_loc_addr-country TO cs_po_header-otl_country_code.
          APPEND ls_loc_addr-city1 TO cs_po_header-otl_city_name.
          APPEND ls_loc_addr-region TO cs_po_header-otl_region_code.
          APPEND ls_loc_addr-house_num1 TO cs_po_header-otl_house_number.
          APPEND ls_loc_addr-street TO cs_po_header-otl_street_name.
          APPEND ls_loc_addr-post_code1 TO cs_po_header-otl_postal_code.
          APPEND lv_loc_email TO cs_po_header-otl_email_address.
          APPEND lv_loc_tel TO cs_po_header-otl_phone_number.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKKO' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_dlv_table.

    DATA:
      lt_vbeln   TYPE vbeln_vl_t,
      lv_count   TYPE i,
      lv_line_no TYPE char20.

    CLEAR:
      et_line_no,
      et_dlv_no.

    zcl_gtt_tools=>get_delivery_by_ref_doc(
      EXPORTING
        iv_vgbel = iv_ebeln
        iv_vbtyp = iv_vbtyp
      IMPORTING
        et_vbeln = lt_vbeln ).

    LOOP AT lt_vbeln INTO DATA(ls_vbeln).
      lv_count = lv_count + 1.
      lv_line_no = lv_count.
      CONDENSE lv_line_no NO-GAPS.
      APPEND lv_line_no TO et_line_no.

      SHIFT ls_vbeln LEFT DELETING LEADING '0'.
      APPEND ls_vbeln TO et_dlv_no.
      CLEAR lv_line_no.
    ENDLOOP.

    IF lt_vbeln IS INITIAL.
      APPEND '' TO et_line_no.
    ENDIF.

  ENDMETHOD.


  METHOD fill_outbound_dlv_table.

    fill_dlv_table(
      EXPORTING
        iv_ebeln   = iv_ebeln
        iv_vbtyp   = if_sd_doc_category=>delivery
      IMPORTING
        et_line_no = cs_po_header-out_dlv_line_no
        et_dlv_no  = cs_po_header-out_dlv_no ).

  ENDMETHOD.
ENDCLASS.""",
    r"""CLASS zcl_gtt_spof_tp_reader_po_itm DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_gtt_tp_reader .

    METHODS constructor
      IMPORTING
        !io_ef_parameters TYPE REF TO zif_gtt_ef_parameters .
protected section.

  types tt_otl_locid TYPE STANDARD TABLE OF vbpa-lifnr WITH EMPTY KEY .
  types tt_otl_loctype TYPE STANDARD TABLE OF char30 WITH EMPTY KEY .
  types tt_otl_timezone TYPE STANDARD TABLE OF addr1_data-time_zone WITH EMPTY KEY .
  types tt_otl_description TYPE STANDARD TABLE OF addr1_data-name1 WITH EMPTY KEY .
  types tt_otl_country_code TYPE STANDARD TABLE OF addr1_data-country WITH EMPTY KEY .
  types tt_otl_city_name TYPE STANDARD TABLE OF addr1_data-city1 WITH EMPTY KEY .
  types tt_otl_region_code TYPE STANDARD TABLE OF addr1_data-region WITH EMPTY KEY .
  types tt_otl_house_number TYPE STANDARD TABLE OF addr1_data-house_num1 WITH EMPTY KEY .
  types tt_otl_street_name TYPE STANDARD TABLE OF addr1_data-street WITH EMPTY KEY .
  types tt_otl_postal_code TYPE STANDARD TABLE OF addr1_data-post_code1 WITH EMPTY KEY .
  types tt_otl_email_address TYPE STANDARD TABLE OF ad_smtpadr WITH EMPTY KEY .
  types tt_otl_phone_number TYPE STANDARD TABLE OF char50 WITH EMPTY KEY .

  TYPES:
    BEGIN OF ts_address_info,
      locid     type char20,
      loctype   type char30,
      addr1     TYPE addr1_data,
      email     TYPE ad_smtpadr,
      telephone TYPE char50,
    END OF ts_address_info.
  TYPES:
    tt_address_info type TABLE OF ts_address_info.
private section.

  types TV_EBELP_TXT type CHAR5 .
  types TV_DELIV_NUM type I .
  types TV_DELIV_ITEM type CHAR20 .
  types TV_SCHED_NUM type I .        "char15
  types:
    tt_deliv_num   TYPE STANDARD TABLE OF tv_deliv_num
                            WITH EMPTY KEY .
  types:
    tt_deliv_item  TYPE STANDARD TABLE OF tv_deliv_item
                            WITH EMPTY KEY .
  types:
    tt_sched_num   TYPE STANDARD TABLE OF tv_sched_num
                            WITH EMPTY KEY .
  types:
    tt_sched_eindt TYPE STANDARD TABLE OF eket-eindt
                            WITH EMPTY KEY .
  types:
    tt_sched_menge TYPE STANDARD TABLE OF eket-menge
                            WITH EMPTY KEY .
  types:
    tt_sched_meins TYPE STANDARD TABLE OF ekpo-meins
                            WITH EMPTY KEY .
  types:
    BEGIN OF ts_po_item,
        ebeln             TYPE ekpo-ebeln,
        ebelp             TYPE tv_ebelp_txt,    "used char type to avoid leading zeros deletion
        lifnr             TYPE ekko-lifnr,
        lifnr_lt          TYPE /saptrx/loc_id_type,
        werks             TYPE ekpo-werks,
        werks_lt          TYPE /saptrx/loc_id_type,
        werks_name        TYPE t001w-name1,
        eindt             TYPE eket-eindt,
        matnr             TYPE ekpo-matnr,
        menge             TYPE ekpo-menge,
        meins             TYPE ekpo-meins,
        txz01             TYPE ekpo-txz01,
        ntgew             TYPE ekpo-ntgew,
        gewei             TYPE ekpo-gewei,
        brgew             TYPE ekpo-brgew,
        volum             TYPE ekpo-volum,
        voleh             TYPE ekpo-voleh,
        netwr             TYPE ekpo-netwr,
        waers             TYPE ekko-waers,
        inco1             TYPE ekpo-inco1,
        incov             TYPE ekko-incov,
        inco2_l           TYPE ekpo-inco2_l,
        elikz             TYPE abap_bool,
        untto             TYPE ekpo-untto,
        uebto             TYPE ekpo-uebto,
        uebtk             TYPE abap_bool,
        rec_addr          TYPE /saptrx/paramval200,
        menge_sum         TYPE ekes-menge,
        adrnr             TYPE ekpo-adrnr,
        deliv_num         TYPE tt_deliv_num,
        deliv_item        TYPE tt_deliv_item,
        sched_num         TYPE tt_sched_num,
        sched_eindt       TYPE tt_sched_eindt,
        sched_menge       TYPE tt_sched_menge,
        sched_meins       TYPE tt_sched_meins,
        umren             TYPE ekpo-umren,
        umrez             TYPE ekpo-umrez,
        otl_locid         TYPE tt_otl_locid,
        otl_loctype       TYPE tt_otl_loctype,
        otl_timezone      TYPE tt_otl_timezone,
        otl_description   TYPE tt_otl_description,
        otl_country_code  TYPE tt_otl_country_code,
        otl_city_name     TYPE tt_otl_city_name,
        otl_region_code   TYPE tt_otl_region_code,
        otl_house_number  TYPE tt_otl_house_number,
        otl_street_name   TYPE tt_otl_street_name,
        otl_postal_code   TYPE tt_otl_postal_code,
        otl_email_address TYPE tt_otl_email_address,
        otl_phone_number  TYPE tt_otl_phone_number,
      END OF ts_po_item .

  constants:
    BEGIN OF cs_mapping,
        ebeln             TYPE /saptrx/paramname VALUE 'YN_PO_NUMBER',
        ebelp             TYPE /saptrx/paramname VALUE 'YN_PO_ITEM',
        lifnr             TYPE /saptrx/paramname VALUE 'YN_PO_SUPPLIER_ID',
        lifnr_lt          TYPE /saptrx/paramname VALUE 'YN_PO_SUPPLIER_LOC_TYPE',
        werks             TYPE /saptrx/paramname VALUE 'YN_PO_RECEIVING_LOCATION',
        werks_lt          TYPE /saptrx/paramname VALUE 'YN_PO_RECEIVING_LOC_TYPE',
        werks_name        TYPE /saptrx/paramname VALUE 'YN_PO_PLANT_DESCR',
        eindt             TYPE /saptrx/paramname VALUE 'YN_PO_DELIVERY_DATE',
        matnr             TYPE /saptrx/paramname VALUE 'YN_PO_MATERIAL_ID',
        menge             TYPE /saptrx/paramname VALUE 'YN_PO_ORDER_QUANTITY',
        meins             TYPE /saptrx/paramname VALUE 'YN_PO_UNIT_OF_MEASURE',
        txz01             TYPE /saptrx/paramname VALUE 'YN_PO_MATERIAL_DESCR',
        ntgew             TYPE /saptrx/paramname VALUE 'YN_PO_NET_WEIGHT',
        gewei             TYPE /saptrx/paramname VALUE 'YN_PO_WEIGHT_UOM',
        brgew             TYPE /saptrx/paramname VALUE 'YN_PO_GROSS_WEIGHT',
        volum             TYPE /saptrx/paramname VALUE 'YN_PO_VOLUME',
        voleh             TYPE /saptrx/paramname VALUE 'YN_PO_VOLUME_UOM',
        netwr             TYPE /saptrx/paramname VALUE 'YN_PO_NET_VALUE',
        waers             TYPE /saptrx/paramname VALUE 'YN_PO_CURRENCY',
        inco1             TYPE /saptrx/paramname VALUE 'YN_PO_INCOTERMS',
        incov             TYPE /saptrx/paramname VALUE 'YN_PO_INCOTERMS_VERSION',
        inco2_l           TYPE /saptrx/paramname VALUE 'YN_PO_INCOTERMS_LOCATION',
        elikz             TYPE /saptrx/paramname VALUE 'YN_DL_COMPL_IND',
        untto             TYPE /saptrx/paramname VALUE 'YN_DL_UNDER_TOLERANCE',
        uebto             TYPE /saptrx/paramname VALUE 'YN_DL_OVER_TOLERANCE',
        uebtk             TYPE /saptrx/paramname VALUE 'YN_DL_UNLIMITED_ALLOWED',
        rec_addr          TYPE /saptrx/paramname VALUE 'YN_PO_RECEIVING_ADDRESS',
        menge_sum         TYPE /saptrx/paramname VALUE 'YN_PO_COMFIRMED_QUANTITY',
        adrnr             TYPE /saptrx/paramname VALUE 'YN_PO_ADDRESS_NUMBER',
        deliv_num         TYPE /saptrx/paramname VALUE 'YN_DL_HDR_ITM_LINE_COUNT',
        deliv_item        TYPE /saptrx/paramname VALUE 'YN_DL_HDR_ITM_NO',
        sched_num         TYPE /saptrx/paramname VALUE 'YN_PO_ITM_SCHED_LINE_COUNT',
        sched_eindt       TYPE /saptrx/paramname VALUE 'YN_PO_ITM_SCHED_DATE',
        sched_menge       TYPE /saptrx/paramname VALUE 'YN_PO_ITM_SCHED_QUANTITY',
        sched_meins       TYPE /saptrx/paramname VALUE 'YN_PO_ITM_SCHED_QUANT_UOM',
        umrez             TYPE /saptrx/paramname VALUE 'YN_PO_NUMERATOR_FACTOR',
        umren             TYPE /saptrx/paramname VALUE 'YN_PO_DENOMINATOR_DIVISOR',
        otl_locid         TYPE /saptrx/paramname VALUE 'GTT_OTL_LOCID',
        otl_loctype       TYPE /saptrx/paramname VALUE 'GTT_OTL_LOCTYPE',
        otl_timezone      TYPE /saptrx/paramname VALUE 'GTT_OTL_TIMEZONE',
        otl_description   TYPE /saptrx/paramname VALUE 'GTT_OTL_DESCRIPTION',
        otl_country_code  TYPE /saptrx/paramname VALUE 'GTT_OTL_COUNTRY_CODE',
        otl_city_name     TYPE /saptrx/paramname VALUE 'GTT_OTL_CITY_NAME',
        otl_region_code   TYPE /saptrx/paramname VALUE 'GTT_OTL_REGION_CODE',
        otl_house_number  TYPE /saptrx/paramname VALUE 'GTT_OTL_HOUSE_NUMBER',
        otl_street_name   TYPE /saptrx/paramname VALUE 'GTT_OTL_STREET_NAME',
        otl_postal_code   TYPE /saptrx/paramname VALUE 'GTT_OTL_POSTAL_CODE',
        otl_email_address TYPE /saptrx/paramname VALUE 'GTT_OTL_EMAIL_ADDRESS',
        otl_phone_number  TYPE /saptrx/paramname VALUE 'GTT_OTL_PHONE_NUMBER',
      END OF cs_mapping .
  data MO_EF_PARAMETERS type ref to ZIF_GTT_EF_PARAMETERS .

  methods IS_OBJECT_CHANGED
    importing
      !IS_APP_OBJECT type TRXAS_APPOBJ_CTAB_WA
    returning
      value(RV_RESULT) type ABAP_BOOL
    raising
      CX_UDM_MESSAGE .
  methods FILL_ITEM_FROM_EKKO_STRUCT
    importing
      !IR_EKKO type ref to DATA
    changing
      !CS_PO_ITEM type TS_PO_ITEM
    raising
      CX_UDM_MESSAGE .
  methods FILL_ITEM_FROM_EKPO_STRUCT
    importing
      !IR_EKPO type ref to DATA
    changing
      !CS_PO_ITEM type TS_PO_ITEM
    raising
      CX_UDM_MESSAGE .
  methods FILL_ITEM_FROM_EKET_TABLE
    importing
      !IR_EKPO type ref to DATA
      !IR_EKET type ref to DATA
    changing
      !CS_PO_ITEM type TS_PO_ITEM
    raising
      CX_UDM_MESSAGE .
  methods FILL_ITEM_LOCATION_TYPES
    changing
      !CS_PO_ITEM type TS_PO_ITEM
    raising
      CX_UDM_MESSAGE .
  methods GET_EKPO_RECORD
    importing
      !IR_EKPO type ref to DATA
      !IV_EBELN type EBELN
      !IV_EBELP type EBELP
    returning
      value(RV_EKPO) type ref to DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_ITEM_FROM_EKKO_TABLE
    importing
      !IR_EKKO type ref to DATA
      !IV_EBELN type EBELN
    changing
      !CS_PO_ITEM type TS_PO_ITEM
    raising
      CX_UDM_MESSAGE .
  methods FILL_ITEM_RECEIVING_ADDRESS
    changing
      !CS_PO_ITEM type TS_PO_ITEM .
  methods FILL_CONFIRMED_QUANTITY
    importing
      !IR_EKPO type ref to DATA
      !IR_EKES type ref to DATA
    changing
      !CS_PO_ITEM type TS_PO_ITEM .
  methods FILL_PLANT_DESCRIPTION
    changing
      !CS_PO_ITEM type TS_PO_ITEM .
  methods GET_CONFIRMATION_QUANTITY
    importing
      !IR_EKPO type ref to DATA
      !IR_EKES type ref to DATA
    returning
      value(RV_MENGE) type MENGE_D
    raising
      CX_UDM_MESSAGE .
  methods FORMAT_TO_REMOVE_LEADING_ZERO
    changing
      !CS_PO_ITEM type TS_PO_ITEM .
  methods FILL_ONE_TIME_LOCATION
    importing
      !IR_EKKO type ref to DATA
      !IR_EKPO type ref to DATA
    changing
      !CS_PO_ITEM type TS_PO_ITEM
    raising
      CX_UDM_MESSAGE .
  methods FILL_ONE_TIME_LOCATION_OLD
    importing
      !IV_EBELN type EBELN
      !IR_EKKO type ref to DATA
      !IR_EKPO type ref to DATA
    changing
      !CS_PO_ITEM type TS_PO_ITEM
    raising
      CX_UDM_MESSAGE .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SPOF_TP_READER_PO_ITM IMPLEMENTATION.


  METHOD constructor.

    mo_ef_parameters    = io_ef_parameters.

  ENDMETHOD.


  METHOD fill_confirmed_quantity.

    TRY.
        cs_po_item-menge_sum = get_confirmation_quantity(
          EXPORTING
            ir_ekpo = ir_ekpo
            ir_ekes = ir_ekes
        ).
      CATCH cx_udm_message.
    ENDTRY.
  ENDMETHOD.


  METHOD fill_item_from_eket_table.
    TYPES: tt_eket    TYPE STANDARD TABLE OF ueket.

    DATA: lv_sched_num   TYPE tv_sched_num VALUE 0,
          lv_sched_vrkme TYPE lips-vrkme,
          lv_meins_ext   TYPE ekpo-meins.

    FIELD-SYMBOLS: <lt_eket>  TYPE tt_eket.

    DATA(lv_ebeln) = CONV ebeln( zcl_gtt_tools=>get_field_of_structure(
                                   ir_struct_data = ir_ekpo
                                   iv_field_name  = 'EBELN' ) ).
    DATA(lv_ebelp) = CONV ebelp( zcl_gtt_tools=>get_field_of_structure(
                                   ir_struct_data = ir_ekpo
                                   iv_field_name  = 'EBELP' ) ).
    DATA(lv_meins) = CONV vrkme( zcl_gtt_tools=>get_field_of_structure(
                                   ir_struct_data = ir_ekpo
                                   iv_field_name  = 'MEINS' ) ).
    zcl_gtt_tools=>convert_unit_output(
      EXPORTING
        iv_input  = lv_meins
      RECEIVING
        rv_output = lv_meins_ext ).

    CLEAR: cs_po_item-eindt,
           cs_po_item-sched_num[],
           cs_po_item-sched_eindt[],
           cs_po_item-sched_menge[],
           cs_po_item-sched_meins[].

    ASSIGN ir_eket->* TO <lt_eket>.

    IF <lt_eket> IS ASSIGNED.
      LOOP AT <lt_eket> ASSIGNING FIELD-SYMBOL(<ls_eket>)
        WHERE ebeln = lv_ebeln
          AND ebelp = lv_ebelp.

        " Latest Delivery Date in schedule lines per item, keep empty in case of
        " different date on item level
        cs_po_item-eindt  = COND #( WHEN <ls_eket>-eindt > cs_po_item-eindt
                                      THEN <ls_eket>-eindt
                                      ELSE cs_po_item-eindt ).

        " add row to schedule line table
        ADD 1 TO lv_sched_num.
        APPEND lv_sched_num    TO cs_po_item-sched_num.
        APPEND <ls_eket>-eindt TO cs_po_item-sched_eindt.
        APPEND <ls_eket>-menge TO cs_po_item-sched_menge.
        APPEND lv_meins_ext    TO cs_po_item-sched_meins.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKET' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_item_from_ekko_struct.

    DATA: lv_fname TYPE char5,
          lv_dummy TYPE char100.

    FIELD-SYMBOLS: <ls_ekko>  TYPE any,
                   <lv_lifnr> TYPE ekko-lifnr,
                   <lv_waers> TYPE ekko-waers,
                   <lv_incov> TYPE ekko-incov.

    ASSIGN ir_ekko->* TO <ls_ekko>.

    IF <ls_ekko> IS ASSIGNED.
      ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <ls_ekko> TO <lv_lifnr>.
      ASSIGN COMPONENT 'WAERS' OF STRUCTURE <ls_ekko> TO <lv_waers>.
      ASSIGN COMPONENT 'INCOV' OF STRUCTURE <ls_ekko> TO <lv_incov>.

      IF <lv_lifnr> IS ASSIGNED AND
         <lv_waers> IS ASSIGNED AND
         <lv_incov> IS ASSIGNED.

        cs_po_item-lifnr  = <lv_lifnr>.
        cs_po_item-waers  = <lv_waers>.
        cs_po_item-incov  = <lv_incov>.

      ELSE.
        lv_fname  = COND #( WHEN <lv_lifnr> IS NOT ASSIGNED THEN 'LIFNR'
                            WHEN <lv_incov> IS NOT ASSIGNED THEN 'INCOV'
                              ELSE 'WAERS' ).
        MESSAGE e001(zgtt) WITH lv_fname 'EKKO' INTO lv_dummy.
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKKO' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_item_from_ekko_table.
    DATA: lv_fname TYPE char5,
          lv_dummy TYPE char100.

    FIELD-SYMBOLS: <lt_ekko>  TYPE ANY TABLE,
                   <ls_ekko>  TYPE any,
                   <lv_ebeln> TYPE ekko-ebeln,
                   <lv_lifnr> TYPE ekko-lifnr,
                   <lv_incov> TYPE ekko-incov,
                   <lv_waers> TYPE ekko-waers.

    CLEAR: cs_po_item-lifnr, cs_po_item-waers.

    ASSIGN ir_ekko->* TO <lt_ekko>.

    IF <lt_ekko> IS ASSIGNED.
      LOOP AT <lt_ekko> ASSIGNING <ls_ekko>.
        ASSIGN COMPONENT 'EBELN' OF STRUCTURE <ls_ekko> TO <lv_ebeln>.
        ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <ls_ekko> TO <lv_lifnr>.
        ASSIGN COMPONENT 'INCOV' OF STRUCTURE <ls_ekko> TO <lv_incov>.
        ASSIGN COMPONENT 'WAERS' OF STRUCTURE <ls_ekko> TO <lv_waers>.

        IF <lv_ebeln> IS ASSIGNED AND
           <lv_lifnr> IS ASSIGNED AND
           <lv_incov> IS ASSIGNED AND
           <lv_waers> IS ASSIGNED.

          IF <lv_ebeln> = iv_ebeln.
            cs_po_item-lifnr  = <lv_lifnr>.
            cs_po_item-waers  = <lv_waers>.
            cs_po_item-incov  = <lv_incov>.
          ENDIF.

        ELSE.
          lv_fname  = COND #( WHEN <lv_ebeln> IS NOT ASSIGNED THEN 'EBELN'
                              WHEN <lv_lifnr> IS NOT ASSIGNED THEN 'LIFNR'
                              WHEN <lv_incov> IS NOT ASSIGNED THEN 'INCOV'
                                ELSE 'WAERS' ).
          MESSAGE e001(zgtt) WITH lv_fname 'EKKO' INTO lv_dummy.
          zcl_gtt_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKKO' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_item_from_ekpo_struct.
    FIELD-SYMBOLS: <ls_ekpo>  TYPE any.
    DATA:
      lv_meins TYPE ekpo-meins,
      lv_voleh TYPE ekpo-voleh,
      lv_gewei TYPE ekpo-gewei.

    ASSIGN ir_ekpo->* TO <ls_ekpo>.

    IF <ls_ekpo> IS ASSIGNED.
      MOVE-CORRESPONDING <ls_ekpo> TO cs_po_item.

      cs_po_item-netwr = zcl_gtt_tools=>convert_to_external_amount(
        iv_currency = cs_po_item-waers
        iv_internal = cs_po_item-netwr ).

      zcl_gtt_tools=>convert_unit_output(
        EXPORTING
          iv_input  = cs_po_item-meins
        RECEIVING
          rv_output = lv_meins ).

      zcl_gtt_tools=>convert_unit_output(
        EXPORTING
          iv_input  = cs_po_item-voleh
        RECEIVING
          rv_output = lv_voleh ).

      zcl_gtt_tools=>convert_unit_output(
        EXPORTING
          iv_input  = cs_po_item-gewei
        RECEIVING
          rv_output = lv_gewei ).

      cs_po_item-meins = lv_meins.
      cs_po_item-voleh = lv_voleh.
      cs_po_item-gewei = lv_gewei.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKPO' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_item_location_types.
    cs_po_item-lifnr_lt  = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
    cs_po_item-werks_lt  = zif_gtt_ef_constants=>cs_loc_types-plant.
  ENDMETHOD.


  METHOD fill_item_receiving_address.

    IF cs_po_item-adrnr IS INITIAL AND cs_po_item-werks IS NOT INITIAL.
      TRY.
          zcl_gtt_spof_po_tools=>get_plant_address_num_and_name(
            EXPORTING
              iv_werks = cs_po_item-werks
            IMPORTING
              ev_adrnr = cs_po_item-adrnr
          ).
        CATCH cx_udm_message.
      ENDTRY.
    ENDIF.

    IF cs_po_item-adrnr IS NOT INITIAL.
      TRY.
          zcl_gtt_spof_po_tools=>get_address_info(
            EXPORTING
              iv_addr_numb = cs_po_item-adrnr
            IMPORTING
              ev_address   = cs_po_item-rec_addr ).
        CATCH cx_udm_message.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD fill_plant_description.
    IF cs_po_item-werks IS NOT INITIAL.
      TRY.
          zcl_gtt_spof_po_tools=>get_plant_address_num_and_name(
            EXPORTING
              iv_werks = cs_po_item-werks
            IMPORTING
              ev_name  = cs_po_item-werks_name
          ).
        CATCH cx_udm_message.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD format_to_remove_leading_zero.

    TRY.
        cs_po_item-ebeln = zcl_gtt_spof_po_tools=>get_tracking_id_po_hdr( ir_ekko = REF #( cs_po_item ) ).

        cs_po_item-lifnr = zcl_gtt_tools=>get_pretty_location_id(
          iv_locid   = cs_po_item-lifnr
          iv_loctype = cs_po_item-lifnr_lt ).

        cs_po_item-werks = zcl_gtt_tools=>get_pretty_location_id(
          iv_locid   = cs_po_item-werks
          iv_loctype = cs_po_item-werks_lt ).

        cs_po_item-matnr = zcl_gtt_spof_po_tools=>get_formated_matnr( ir_ekpo = REF #( cs_po_item ) ).
      CATCH cx_udm_message.
    ENDTRY.

  ENDMETHOD.


  METHOD get_confirmation_quantity.
    DATA: lv_has_conf TYPE abap_bool,
          lv_dummy    TYPE char100.

    TYPES: tt_ekes    TYPE STANDARD TABLE OF uekes.

    FIELD-SYMBOLS: <ls_ekpo> TYPE uekpo,
                   <lt_ekes> TYPE tt_ekes,
                   <ls_ekes> TYPE uekes.

    CLEAR rv_menge.

    ASSIGN ir_ekpo->* TO <ls_ekpo>.
    ASSIGN ir_ekes->* TO <lt_ekes>.

    IF <ls_ekpo> IS ASSIGNED AND
       <lt_ekes> IS ASSIGNED.
      LOOP AT <lt_ekes> ASSIGNING <ls_ekes>
        WHERE ebeln = <ls_ekpo>-ebeln
          AND ebelp = <ls_ekpo>-ebelp.

        ADD <ls_ekes>-menge TO rv_menge.
      ENDLOOP.

      DATA(lv_subrc) = sy-subrc.

      rv_menge    = COND #( WHEN lv_subrc <> 0 AND <ls_ekpo>-kzabs = abap_false
                              THEN <ls_ekpo>-menge ELSE rv_menge ).

      rv_menge    = COND #( WHEN lv_subrc <> 0 AND <ls_ekpo>-kzabs = abap_true
                              THEN 0 ELSE rv_menge ).

    ELSEIF <ls_ekpo> IS NOT ASSIGNED.
      MESSAGE e002(zgtt) WITH 'EKPO' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKET' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_ekpo_record.
    TYPES: tt_ekpo    TYPE STANDARD TABLE OF uekpo.

    DATA: lv_dummy    TYPE char100.

    FIELD-SYMBOLS: <lt_ekpo> TYPE tt_ekpo.

    ASSIGN ir_ekpo->* TO <lt_ekpo>.
    IF <lt_ekpo> IS ASSIGNED.
      READ TABLE <lt_ekpo> ASSIGNING FIELD-SYMBOL(<ls_ekpo>)
        WITH KEY ebeln = iv_ebeln
                 ebelp = iv_ebelp.
      IF sy-subrc = 0.
        rv_ekpo   = REF #( <ls_ekpo> ).
      ELSE.
        MESSAGE e005(zgtt) WITH 'EKPO' |{ iv_ebeln }{ iv_ebelp }|
          INTO lv_dummy.
        zcl_gtt_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKPO' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD is_object_changed.

    rv_result = zcl_gtt_tools=>is_object_changed(
      is_app_object    = is_app_object
      io_ef_parameters = mo_ef_parameters
      it_check_tables  = VALUE #( ( zif_gtt_spof_app_constants=>cs_tabledef-po_sched_new )
                                  ( zif_gtt_spof_app_constants=>cs_tabledef-po_sched_old ) )
      iv_key_field     = 'EBELN'
      iv_upd_field     = 'KZ' ).

  ENDMETHOD.


  METHOD zif_gtt_tp_reader~check_relevance.
    rv_result   = zif_gtt_ef_constants=>cs_condition-false.

    IF zcl_gtt_spof_po_tools=>is_appropriate_po_item( ir_ekko = is_app_object-mastertabref ir_ekpo = is_app_object-maintabref ) = abap_true.

      CASE is_app_object-update_indicator.
        WHEN zif_gtt_ef_constants=>cs_change_mode-insert.
          rv_result   = zif_gtt_ef_constants=>cs_condition-true.
        WHEN zif_gtt_ef_constants=>cs_change_mode-update OR
             zif_gtt_ef_constants=>cs_change_mode-undefined.
          rv_result   = zcl_gtt_tools=>are_structures_different(
                          ir_data1  = zif_gtt_tp_reader~get_data(
                                        is_app_object = is_app_object )
                          ir_data2  = zif_gtt_tp_reader~get_data_old(
                                        is_app_object = is_app_object ) ).
      ENDCASE.

      IF rv_result = zif_gtt_ef_constants=>cs_condition-false.
        IF is_object_changed( is_app_object = is_app_object ) = abap_true.
          rv_result = zif_gtt_ef_constants=>cs_condition-true.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_tp_reader~get_app_obj_type_id.
    rv_appobjid = zcl_gtt_spof_po_tools=>get_tracking_id_po_itm(
      ir_ekpo = is_app_object-maintabref ).
  ENDMETHOD.


  METHOD zif_gtt_tp_reader~get_data.
    FIELD-SYMBOLS: <ls_item>      TYPE ts_po_item.

    rr_data   = NEW ts_po_item( ).

    ASSIGN rr_data->* TO <ls_item>.

    fill_item_from_ekko_struct(
      EXPORTING
        ir_ekko    = is_app_object-mastertabref
      CHANGING
        cs_po_item = <ls_item> ).

    fill_item_from_ekpo_struct(
      EXPORTING
        ir_ekpo    = is_app_object-maintabref
      CHANGING
        cs_po_item = <ls_item> ).

    fill_item_from_eket_table(
      EXPORTING
        ir_ekpo       = is_app_object-maintabref
        ir_eket       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_sched_new )
      CHANGING
        cs_po_item  = <ls_item> ).

    fill_item_location_types(
      CHANGING
        cs_po_item = <ls_item> ).

    fill_item_receiving_address(
      CHANGING
        cs_po_item = <ls_item> ).

    fill_confirmed_quantity(
      EXPORTING
        ir_ekpo       = is_app_object-maintabref
        ir_ekes       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_vend_conf_new )
      CHANGING
        cs_po_item  = <ls_item> ).

    fill_plant_description(
      CHANGING
        cs_po_item = <ls_item> ).

    fill_one_time_location(
      EXPORTING
        ir_ekko    = is_app_object-mastertabref
        ir_ekpo    = is_app_object-maintabref
      CHANGING
        cs_po_item = <ls_item> ).

    format_to_remove_leading_zero(
      CHANGING
        cs_po_item = <ls_item> ).

    IF <ls_item>-otl_locid IS INITIAL.
      APPEND INITIAL LINE TO <ls_item>-otl_locid.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_tp_reader~get_data_old.
    FIELD-SYMBOLS: <ls_item>      TYPE ts_po_item.

    DATA(lv_ebeln)  = CONV ebeln( zcl_gtt_tools=>get_field_of_structure(
                                    ir_struct_data = is_app_object-maintabref
                                    iv_field_name  = 'EBELN' ) ).
    DATA(lv_ebelp)  = CONV ebelp( zcl_gtt_tools=>get_field_of_structure(
                                    ir_struct_data = is_app_object-maintabref
                                    iv_field_name  = 'EBELP' ) ).
    DATA(lr_ekpo)   = get_ekpo_record(
                        ir_ekpo  = mo_ef_parameters->get_appl_table(
                                     iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_item_old )
                        iv_ebeln = lv_ebeln
                        iv_ebelp = lv_ebelp ).
    DATA(lr_ekko) = mo_ef_parameters->get_appl_table(
      iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_header_old ).

    rr_data   = NEW ts_po_item( ).
    ASSIGN rr_data->* TO <ls_item>.

    fill_item_from_ekko_table(
      EXPORTING
        iv_ebeln   = lv_ebeln
        ir_ekko    = lr_ekko
      CHANGING
        cs_po_item = <ls_item> ).

    fill_item_from_ekpo_struct(
      EXPORTING
        ir_ekpo    = lr_ekpo
      CHANGING
        cs_po_item = <ls_item> ).

    fill_item_from_eket_table(
      EXPORTING
        ir_ekpo       = lr_ekpo
        ir_eket       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_sched_old )
      CHANGING
        cs_po_item  = <ls_item> ).

    fill_item_location_types(
      CHANGING
        cs_po_item = <ls_item> ).

    fill_item_receiving_address(
      CHANGING
        cs_po_item = <ls_item> ).

    fill_confirmed_quantity(
      EXPORTING
        ir_ekpo       = lr_ekpo
        ir_ekes       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_spof_app_constants=>cs_tabledef-po_vend_conf_old )
      CHANGING
        cs_po_item  = <ls_item> ).

    fill_plant_description(
      CHANGING
        cs_po_item = <ls_item> ).

    fill_one_time_location_old(
      EXPORTING
        iv_ebeln   = lv_ebeln
        ir_ekko    = lr_ekko
        ir_ekpo    = lr_ekpo
      CHANGING
        cs_po_item = <ls_item> ).

    format_to_remove_leading_zero(
      CHANGING
        cs_po_item = <ls_item> ).

    IF <ls_item>-otl_locid IS INITIAL.
      APPEND INITIAL LINE TO <ls_item>-otl_locid.
    ENDIF.

  ENDMETHOD.


  method ZIF_GTT_TP_READER~GET_FIELD_PARAMETER.
    CASE iv_parameter.
      WHEN zif_gtt_ef_constants=>cs_parameter_id-key_field.
        rv_result   = boolc( iv_field_name = cs_mapping-deliv_num OR
                             iv_field_name = cs_mapping-sched_num ).
      WHEN OTHERS.
        CLEAR: rv_result.
    ENDCASE.
  endmethod.


  METHOD zif_gtt_tp_reader~get_mapping_structure.

    rr_data   = REF #( cs_mapping ).

  ENDMETHOD.


  METHOD zif_gtt_tp_reader~get_track_id_data.
    FIELD-SYMBOLS: <ls_ekpo> TYPE uekpo.
    CLEAR et_track_id_data.

    DATA(lv_tzone)  = zcl_gtt_tools=>get_system_time_zone( ).
    ASSIGN is_app_object-maintabref->* TO <ls_ekpo>.

    IF <ls_ekpo> IS ASSIGNED.
      et_track_id_data = VALUE #( BASE et_track_id_data (
         appsys      = mo_ef_parameters->get_appsys( )
         appobjtype  = is_app_object-appobjtype
         appobjid    = is_app_object-appobjid
         trxcod      = zif_gtt_ef_constants=>cs_trxcod-po_position
         trxid       = zcl_gtt_spof_po_tools=>get_tracking_id_po_itm(
                         ir_ekpo = is_app_object-maintabref )
         timzon      = lv_tzone
         msrid       = space
       ) ).
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKPO' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_one_time_location.

    FIELD-SYMBOLS:
      <ls_ekko> TYPE any,
      <ls_ekpo> TYPE uekpo.

    DATA:
      ls_loc_addr     TYPE addr1_data,
      lv_loc_email    TYPE ad_smtpadr,
      lv_loc_tel      TYPE char50,
      ls_address_info TYPE ts_address_info,
      lt_address_info TYPE tt_address_info.

*   Vendor address
    ASSIGN ir_ekko->* TO <ls_ekko>.
    IF <ls_ekko> IS ASSIGNED.
      ASSIGN COMPONENT 'ADRNR' OF STRUCTURE <ls_ekko> TO FIELD-SYMBOL(<lv_adrnr>).
      ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <ls_ekko> TO FIELD-SYMBOL(<lv_lifnr>).
      IF <lv_adrnr> IS ASSIGNED AND <lv_lifnr> IS ASSIGNED
        AND <lv_adrnr> IS NOT INITIAL AND <lv_lifnr> IS NOT INITIAL.
        zcl_gtt_tools=>get_address_from_memory(
          EXPORTING
            iv_addrnumber = <lv_adrnr>
          IMPORTING
            es_addr       = ls_loc_addr
            ev_email      = lv_loc_email
            ev_telephone  = lv_loc_tel ).

        ls_address_info-locid = |{ <lv_lifnr> ALPHA = OUT }|.
        ls_address_info-loctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
        ls_address_info-addr1 = ls_loc_addr.
        ls_address_info-email = lv_loc_email.
        ls_address_info-telephone = lv_loc_tel.
        APPEND ls_address_info TO lt_address_info.
        CLEAR:
          ls_address_info.
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKKO' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

    CLEAR:
      ls_loc_addr,
      lv_loc_email,
      lv_loc_tel.

*   Receiving location(plant)
    ASSIGN ir_ekpo->* TO <ls_ekpo>.
    IF <ls_ekpo> IS ASSIGNED.
      IF <ls_ekpo>-adrnr IS NOT INITIAL.
        zcl_gtt_tools=>get_address_from_memory(
          EXPORTING
            iv_addrnumber = <ls_ekpo>-adrnr
          IMPORTING
            es_addr       = ls_loc_addr
            ev_email      = lv_loc_email
            ev_telephone  = lv_loc_tel ).

        ls_address_info-locid = <ls_ekpo>-werks.
        ls_address_info-loctype = zif_gtt_ef_constants=>cs_loc_types-plant.
        ls_address_info-addr1 = ls_loc_addr.
        ls_address_info-email = lv_loc_email.
        ls_address_info-telephone = lv_loc_tel.
        APPEND ls_address_info TO lt_address_info.
        CLEAR:
          ls_address_info.

      ENDIF.

    ELSE.
      MESSAGE e002(zgtt) WITH 'EKPO' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

    LOOP AT lt_address_info INTO ls_address_info.
      APPEND ls_address_info-locid TO cs_po_item-otl_locid.
      APPEND ls_address_info-loctype TO cs_po_item-otl_loctype.
      APPEND ls_address_info-addr1-time_zone TO cs_po_item-otl_timezone.
      APPEND ls_address_info-addr1-name1 TO cs_po_item-otl_description.
      APPEND ls_address_info-addr1-country TO cs_po_item-otl_country_code.
      APPEND ls_address_info-addr1-city1 TO cs_po_item-otl_city_name.
      APPEND ls_address_info-addr1-region TO cs_po_item-otl_region_code.
      APPEND ls_address_info-addr1-house_num1 TO cs_po_item-otl_house_number.
      APPEND ls_address_info-addr1-street TO cs_po_item-otl_street_name.
      APPEND ls_address_info-addr1-post_code1 TO cs_po_item-otl_postal_code.
      APPEND ls_address_info-email TO cs_po_item-otl_email_address.
      APPEND ls_address_info-telephone TO cs_po_item-otl_phone_number.
      CLEAR:
        ls_address_info.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_one_time_location_old.

    FIELD-SYMBOLS:
      <ls_ekpo> TYPE uekpo,
      <ls_ekko> TYPE any,
      <lt_ekko> TYPE ANY TABLE.

    DATA:
      ls_loc_addr     TYPE addr1_data,
      lv_loc_email    TYPE ad_smtpadr,
      lv_loc_tel      TYPE char50,
      ls_address_info TYPE ts_address_info,
      lt_address_info TYPE tt_address_info.

*   Vendor address
    ASSIGN ir_ekko->* TO <lt_ekko>.
    IF <lt_ekko> IS ASSIGNED.
      LOOP AT <lt_ekko> ASSIGNING <ls_ekko>.
        ASSIGN COMPONENT 'EBELN' OF STRUCTURE <ls_ekko> TO FIELD-SYMBOL(<lv_ebeln>).
        ASSIGN COMPONENT 'ADRNR' OF STRUCTURE <ls_ekko> TO FIELD-SYMBOL(<lv_adrnr>).
        ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <ls_ekko> TO FIELD-SYMBOL(<lv_lifnr>).
        IF <lv_ebeln> IS ASSIGNED AND <lv_adrnr> IS ASSIGNED AND <lv_lifnr> IS ASSIGNED.
          IF <lv_ebeln> = iv_ebeln AND <lv_adrnr> IS NOT INITIAL AND <lv_lifnr> IS NOT INITIAL.
            zcl_gtt_tools=>get_address_from_db(
              EXPORTING
                iv_addrnumber = <lv_adrnr>
              IMPORTING
                es_addr       = ls_loc_addr
                ev_email      = lv_loc_email
                ev_telephone  = lv_loc_tel ).

            ls_address_info-locid = |{ <lv_lifnr> ALPHA = OUT }|.
            ls_address_info-loctype = zif_gtt_ef_constants=>cs_loc_types-businesspartner.
            ls_address_info-addr1 = ls_loc_addr.
            ls_address_info-email = lv_loc_email.
            ls_address_info-telephone = lv_loc_tel.
            APPEND ls_address_info TO lt_address_info.
            CLEAR:
              ls_address_info.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt) WITH 'EKKO' INTO DATA(lv_dummy).
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

    CLEAR:
      ls_loc_addr,
      lv_loc_email,
      lv_loc_tel.

*   receiving location(plant) address
    ASSIGN ir_ekpo->* TO <ls_ekpo>.
    IF <ls_ekpo> IS ASSIGNED.
      IF <ls_ekpo>-adrnr IS NOT INITIAL.
        zcl_gtt_tools=>get_address_from_db(
          EXPORTING
            iv_addrnumber = <ls_ekpo>-adrnr
          IMPORTING
            es_addr       = ls_loc_addr
            ev_email      = lv_loc_email
            ev_telephone  = lv_loc_tel ).

        ls_address_info-locid = <ls_ekpo>-werks.
        ls_address_info-loctype = zif_gtt_ef_constants=>cs_loc_types-plant.
        ls_address_info-addr1 = ls_loc_addr.
        ls_address_info-email = lv_loc_email.
        ls_address_info-telephone = lv_loc_tel.
        APPEND ls_address_info TO lt_address_info.
        CLEAR:
          ls_address_info.
      ENDIF.

    ELSE.
      MESSAGE e002(zgtt) WITH 'EKPO' INTO lv_dummy.
      zcl_gtt_tools=>throw_exception( ).
    ENDIF.

    LOOP AT lt_address_info INTO ls_address_info.
      APPEND ls_address_info-locid TO cs_po_item-otl_locid.
      APPEND ls_address_info-loctype TO cs_po_item-otl_loctype.
      APPEND ls_address_info-addr1-time_zone TO cs_po_item-otl_timezone.
      APPEND ls_address_info-addr1-name1 TO cs_po_item-otl_description.
      APPEND ls_address_info-addr1-country TO cs_po_item-otl_country_code.
      APPEND ls_address_info-addr1-city1 TO cs_po_item-otl_city_name.
      APPEND ls_address_info-addr1-region TO cs_po_item-otl_region_code.
      APPEND ls_address_info-addr1-house_num1 TO cs_po_item-otl_house_number.
      APPEND ls_address_info-addr1-street TO cs_po_item-otl_street_name.
      APPEND ls_address_info-addr1-post_code1 TO cs_po_item-otl_postal_code.
      APPEND ls_address_info-email TO cs_po_item-otl_email_address.
      APPEND ls_address_info-telephone TO cs_po_item-otl_phone_number.
      CLEAR:
        ls_address_info.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""FUNCTION zgtt_spof_aoid_po_hdr.
      TRY.
      e_appobjid = zcl_gtt_ef_performer=>get_app_obj_type_id(
        EXPORTING
          is_definition      = VALUE #( maintab = zif_gtt_spof_app_constants=>cs_tabledef-po_header_new )
          io_tp_factory      = NEW zcl_gtt_spof_tp_factory_po_hdr( )
          iv_appsys          = i_appsys
          is_app_obj_types   = i_app_obj_types
          it_all_appl_tables = i_all_appl_tables
          it_app_objects     = VALUE #( ( i_app_object ) ) ).

    CATCH cx_udm_message.
  ENDTRY.
ENDFUNCTION.""",
    r"""FUNCTION zgtt_spof_ctp_dl_to_po .
      TRY.

*     Inbound delivery to PO Header
      DATA(lo_header) = zcl_gtt_spof_ctp_snd_dl_to_poh=>get_instance( ).
      DATA(lo_dlh) = NEW zcl_gtt_spof_ctp_dlh_data(
        it_xlikp = it_xlikp
        it_xlips = it_xlips
        it_ylips = it_ylips ).
      IF lo_header IS BOUND.
        lo_header->prepare_idoc_data( io_dlh_data = lo_dlh ).
        lo_header->send_idoc_data( ).
      ENDIF.

*     Outbound delivery to PO Header
      lo_dlh = NEW zcl_gtt_spof_ctp_dlh_data(
        it_xlikp       = it_xlikp
        it_xlips       = it_xlips
        it_ylips       = it_ylips
        iv_sto_is_used = abap_true ).
      IF lo_header IS BOUND.
        lo_header->prepare_idoc_data( io_dlh_data = lo_dlh ).
        lo_header->send_idoc_data( ).
      ENDIF.

*     Inbound/Outbound delivery to PO item
      DATA(lo_item) = zcl_gtt_spof_ctp_snd_dl_to_po=>get_instance( ).
      DATA(lo_dli) = NEW zcl_gtt_spof_ctp_dli_data(
        it_xlikp = it_xlikp
        it_xlips = it_xlips
        it_ylips = it_ylips
      ).
      IF lo_item IS BOUND.
        lo_item->prepare_idoc_data( io_dli_data = lo_dli ).

        lo_item->send_idoc_data( ).
      ENDIF.
    CATCH cx_udm_message INTO DATA(lo_udm_message).
      zcl_gtt_tools=>log_exception(
        EXPORTING
          io_udm_message = lo_udm_message
          iv_object      = zif_gtt_ef_constants=>cs_logs-object-delivery_ctp
          iv_subobject   = zif_gtt_ef_constants=>cs_logs-subobject-delivery_ctp ).
  ENDTRY.

ENDFUNCTION.""",
    r"""FUNCTION ZGTT_SPOF_OTE_PO_HDR.
      DATA: lo_udm_message    TYPE REF TO cx_udm_message,
        ls_bapiret        TYPE bapiret2.

  TRY.
      zcl_gtt_ef_performer=>get_control_data(
        EXPORTING
          is_definition         = VALUE #( maintab = zif_gtt_spof_app_constants=>cs_tabledef-po_header_new )
          io_tp_factory         = NEW zcl_gtt_spof_tp_factory_po_hdr( )
          iv_appsys             = i_appsys
          is_app_obj_types      = i_app_obj_types
          it_all_appl_tables    = i_all_appl_tables
          it_app_type_cntl_tabs = i_app_type_cntl_tabs
          it_app_objects        = i_app_objects
        CHANGING
          ct_control_data       = e_control_data[] ).

    CATCH cx_udm_message INTO lo_udm_message.
      zcl_gtt_tools=>get_errors_log(
        EXPORTING
          io_umd_message = lo_udm_message
          iv_appsys      = i_appsys
        IMPORTING
          es_bapiret     = ls_bapiret ).

      " add error message
      APPEND ls_bapiret TO e_logtable.

      " throw corresponding exception
      CASE lo_udm_message->textid.
        WHEN zif_gtt_ef_constants=>cs_errors-stop_processing.
          RAISE stop_processing.
        WHEN zif_gtt_ef_constants=>cs_errors-table_determination.
          RAISE table_determination_error.
      ENDCASE.
  ENDTRY.
ENDFUNCTION.""",
    r"""DATA:  BEGIN OF STATUS_ZGTT_POTYPE_RST               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGTT_POTYPE_RST               .
CONTROLS: TCTRL_ZGTT_POTYPE_RST
            TYPE TABLEVIEW USING SCREEN '9000'.
TABLES: *ZGTT_POTYPE_RST               .
TABLES: ZGTT_POTYPE_RST                .

  INCLUDE LSVIMTDT""",
    r"""INTERFACE zif_gtt_spof_app_constants
  PUBLIC .


  CONSTANTS:
    BEGIN OF cs_tabledef,
      po_header_new       TYPE /saptrx/strucdatadef VALUE 'PURCHASE_ORDER_HEADER_NEW',
      po_header_old       TYPE /saptrx/strucdatadef VALUE 'PURCHASE_ORDER_HEADER_OLD',
      po_item_new         TYPE /saptrx/strucdatadef VALUE 'PURCHASE_ITEM_NEW',
      po_item_old         TYPE /saptrx/strucdatadef VALUE 'PURCHASE_ITEM_OLD',
      po_sched_new        TYPE /saptrx/strucdatadef VALUE 'PO_SCHED_LINE_ITEM_NEW',
      po_sched_old        TYPE /saptrx/strucdatadef VALUE 'PO_SCHED_LINE_ITEM_OLD',
      po_vend_conf_new    TYPE /saptrx/strucdatadef VALUE 'VENDOR_CONFIRMATION_NEW',
      po_vend_conf_old    TYPE /saptrx/strucdatadef VALUE 'VENDOR_CONFIRMATION_OLD',
      md_material_segment TYPE /saptrx/strucdatadef VALUE 'MATERIAL_SEGMENT',
      md_material_header  TYPE /saptrx/strucdatadef VALUE 'MATERIAL_HEADER',
      dl_item_new         TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_NEW',
      dl_item_old         TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_OLD',
      dl_header_new       TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HEADER_NEW',
      dl_header_old       TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HEADER_OLD',
    END OF cs_tabledef .
  CONSTANTS:
    BEGIN OF cs_md_type,
      goods_receipt TYPE mkpf-blart VALUE 'WE',
    END OF cs_md_type .
  CONSTANTS:
    BEGIN OF cs_loekz,
      active  TYPE ekpo-loekz VALUE '',
      deleted TYPE ekpo-loekz VALUE 'L',
    END OF cs_loekz .
  CONSTANTS:
    BEGIN OF cs_adrtype,
      organization TYPE ad_adrtype VALUE '1',
    END OF cs_adrtype .
  CONSTANTS cv_agent_id_type TYPE bu_id_type VALUE 'LBN001' ##NO_TEXT.
  CONSTANTS:
    cv_agent_id_prefix TYPE c LENGTH 4 VALUE 'LBN#' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF cs_vbtyp,
      delivery TYPE vbtyp VALUE '7',
    END OF cs_vbtyp .
ENDINTERFACE.""",
    r"""INTERFACE zif_gtt_spof_app_types
  PUBLIC .


  TYPES ts_uekpo TYPE uekpo .
  TYPES:
    tt_uekpo TYPE STANDARD TABLE OF ts_uekpo .
  TYPES ts_ueket TYPE ueket .
  TYPES:
    tt_ueket TYPE STANDARD TABLE OF ts_ueket .

  TYPES ts_uekes TYPE uekes .
  TYPES:
    tt_uekes TYPE STANDARD TABLE OF ts_uekes .
  TYPES ts_uekko TYPE ekko .
  TYPES:
    tt_uekko TYPE STANDARD TABLE OF ts_uekko .
  types:
    BEGIN OF ts_ref_list,
      vgbel TYPE vgbel,
      vbeln TYPE vbeln_vl_t,
     END OF ts_ref_list .
  types:
    tt_ref_list TYPE STANDARD TABLE OF ts_ref_list .

ENDINTERFACE.""",
    r"""class ZCL_GTT_CTP_TOR_TO_DL_BASE definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_GTT_CTP_TOR_TO_DL .

  TYPES:
    BEGIN OF ts_delivery,
      vbeln TYPE likp-vbeln,
    END OF ts_delivery.

  TYPES:
    BEGIN OF ts_delivery_info,
      vbeln       TYPE likp-vbeln,
      dlv_items   TYPE zif_gtt_sof_ctp_types=>tt_delivery_chng,
      tor_id      TYPE /scmtms/t_tor_id,
      process_flg TYPE flag,
    END OF ts_delivery_info.
  TYPES:
    tt_delivery      TYPE TABLE OF ts_delivery,
    tt_delivery_info TYPE TABLE OF ts_delivery_info.
protected section.

  data MV_APPSYS type LOGSYS .
  data MV_BASE_BTD_TCO type /SCMTMS/BASE_BTD_TCO .
  data MT_AOTYPE type ZIF_GTT_CTP_TYPES=>TT_AOTYPE .
  data MT_TOR_ROOT type /SCMTMS/T_EM_BO_TOR_ROOT .
  data MT_TOR_ROOT_BEFORE type /SCMTMS/T_EM_BO_TOR_ROOT .
  data MT_TOR_ITEM type /SCMTMS/T_EM_BO_TOR_ITEM .
  data MT_TOR_ITEM_BEFORE type /SCMTMS/T_EM_BO_TOR_ITEM .
  data MT_TOR_STOP type /SCMTMS/T_EM_BO_TOR_STOP .
  data MT_TOR_STOP_BEFORE type /SCMTMS/T_EM_BO_TOR_STOP .
  data MT_DELIVERY_ITEM_CHNG type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG .
  data MT_DELIVERY type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY .
  constants:
    BEGIN OF cs_mapping,
      vbeln                 TYPE /saptrx/paramname VALUE 'YN_DLV_NO',
      posnr                 TYPE /saptrx/paramname VALUE 'YN_DLV_ITEM_NO',
      fu_relevant           TYPE /saptrx/paramname VALUE 'YN_DL_FU_RELEVANT',
      pod_relevant          TYPE /saptrx/paramname VALUE 'YN_DL_POD_RELEVANT',
      dlv_vbeln             TYPE /saptrx/paramname VALUE 'YN_DL_DELEVERY',
      dlv_posnr             TYPE /saptrx/paramname VALUE 'YN_DL_DELEVERY_ITEM',
      fu_lineno             TYPE /saptrx/paramname VALUE 'YN_DL_FU_LINE_COUNT',
      fu_freightunit        TYPE /saptrx/paramname VALUE 'YN_DL_FU_NO',
      fu_itemnumber         TYPE /saptrx/paramname VALUE 'YN_DL_FU_ITEM_NO',
      fu_quantity           TYPE /saptrx/paramname VALUE 'YN_DL_FU_QUANTITY',
      fu_quantityuom        TYPE /saptrx/paramname VALUE 'YN_DL_FU_UNITS',
      fu_product_id         TYPE /saptrx/paramname VALUE 'YN_DL_FU_PRODUCT',
      fu_product_descr      TYPE /saptrx/paramname VALUE 'YN_DL_FU_PRODUCT_DESCR',
      fu_freightunit_logsys TYPE /saptrx/paramname VALUE 'YN_DL_FU_NO_LOGSYS',
      fu_base_uom_val       TYPE /saptrx/paramname VALUE 'YN_FU_BASE_UOM_VAL',
      fu_base_uom_uni       TYPE /saptrx/paramname VALUE 'YN_FU_BASE_UOM_UNI',
      appsys                TYPE /saptrx/paramname VALUE 'E1EHPTID_APPSYS',
      trxcod                TYPE /saptrx/paramname VALUE 'E1EHPTID_TRXCOD',
      trxid                 TYPE /saptrx/paramname VALUE 'E1EHPTID_TRXID',
      action                TYPE /saptrx/paramname VALUE 'E1EHPTID_ACTION',
    END OF cs_mapping .
  data MT_IDOC_DATA type ZIF_GTT_SOF_CTP_TYPES=>TT_IDOC_DATA .
  data MT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_ITEM .
  data MT_FU_INFO type /SCMTMS/T_TOR_ROOT_K .

  methods GET_AOTYPE_RESTRICTION_ID
  abstract
    returning
      value(RV_RST_ID) type ZGTT_RST_ID .
  methods INITIATE_AOTYPES
    importing
      !IV_RST_ID type ZGTT_RST_ID .
  methods IS_GTT_ENABLED
    importing
      !IV_TRK_OBJ_TYPE type /SAPTRX/TRK_OBJ_TYPE
    returning
      value(RV_RESULT) type FLAG .
  methods IS_EXTRACTOR_EXIST
    importing
      !IV_TRK_OBJ_TYPE type /SAPTRX/TRK_OBJ_TYPE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods GET_DLV_ITEM_BASED_ON_TOR_ROOT
    changing
      !CT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG .
  methods ADD_DELIVERY_ITEMS_BY_TOR_ROOT
    importing
      !IS_TOR_ROOT type /SCMTMS/S_EM_BO_TOR_ROOT
      !IV_CHANGE_MODE type /BOBF/CONF_CHANGE_MODE
    changing
      !CT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG .
  methods ADD_DELIVERY_ITEM
    importing
      !IS_TOR_ROOT type /SCMTMS/S_EM_BO_TOR_ROOT
      !IS_TOR_ITEM type /SCMTMS/S_EM_BO_TOR_ITEM
      !IV_CHANGE_MODE type /BOBF/CONF_CHANGE_MODE
    changing
      !CT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG .
  methods GET_DLV_ITEM_BASED_ON_TOR_ITEM
    changing
      !CT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG .
  methods GET_DLV_ITEM_BASED_ON_TOR_STOP
    changing
      !CT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG .
  methods FILL_DELIVERY_HEADER_DATA .
  methods PREPARE_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_TRXSERV
    importing
      !IS_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TS_AOTYPE
    changing
      !CS_IDOC_DATA type ZIF_GTT_SOF_CTP_TYPES=>TS_IDOC_DATA .
  methods FILL_IDOC_APPOBJ_CTABS
    importing
      !IS_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TS_AOTYPE
      value(IS_LIKP) type ZIF_GTT_SOF_CTP_TYPES=>TS_DELIVERY optional
      value(IS_DELIVERY_ITEM) type ZIF_GTT_SOF_CTP_TYPES=>TS_DELIVERY_ITEM optional
    changing
      !CS_IDOC_DATA type ZIF_GTT_SOF_CTP_TYPES=>TS_IDOC_DATA .
  methods FILL_IDOC_CONTROL_DATA
    importing
      !IS_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TS_AOTYPE
      value(IS_DELIVERY) type ZIF_GTT_SOF_CTP_TYPES=>TS_DELIVERY optional
      value(IS_DELIVERY_ITEM) type ZIF_GTT_SOF_CTP_TYPES=>TS_DELIVERY_ITEM optional
    changing
      !CS_IDOC_DATA type ZIF_GTT_SOF_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_EXP_EVENT
    importing
      !IS_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TS_AOTYPE
      value(IS_DELIVERY) type ZIF_GTT_SOF_CTP_TYPES=>TS_DELIVERY optional
      value(IS_DLV_ITEM) type ZIF_GTT_SOF_CTP_TYPES=>TS_DELIVERY_ITEM optional
    changing
      !CS_IDOC_DATA type ZIF_GTT_SOF_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_TRACKING_ID
    importing
      !IS_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TS_AOTYPE
      value(IS_DELIVERY) type ZIF_GTT_SOF_CTP_TYPES=>TS_DELIVERY optional
      value(IS_DELIVERY_ITEM) type ZIF_GTT_SOF_CTP_TYPES=>TS_DELIVERY_ITEM optional
    changing
      !CS_IDOC_DATA type ZIF_GTT_SOF_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods SEND_IDOC_DATA
    exporting
      !ET_BAPIRET type BAPIRET2_T .
  methods FILL_DELIVERY_ITEM_DATA .
  methods CLEAR_DATA .
  methods DETERMINE_PROCESS_FLAG
    importing
      !IT_TOR_ID type /SCMTMS/T_TOR_ID
      !IT_FU_DB type /SCMTMS/T_TOR_ROOT_K
    exporting
      !EV_PROCESS_FLG type FLAG .
  methods CHECK_FU_STATUS
    exporting
      !ET_DELIVERY_INFO type TT_DELIVERY_INFO .
private section.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_CTP_TOR_TO_DL_BASE IMPLEMENTATION.


  METHOD add_delivery_item.

    DATA:
      ls_delivery_item TYPE zif_gtt_sof_ctp_types=>ts_delivery_chng,
      lv_matnr         TYPE mara-matnr.

    ls_delivery_item-vbeln        = |{ is_tor_item-base_btd_id ALPHA = IN }|.
    ls_delivery_item-posnr        = is_tor_item-base_btditem_id.
    ls_delivery_item-tor_id       = is_tor_root-tor_id.
    ls_delivery_item-item_id      = is_tor_item-item_id.
    ls_delivery_item-quantity     = is_tor_item-qua_pcs_val.
    ls_delivery_item-product_descr = is_tor_item-item_descr.
    ls_delivery_item-base_uom_val  = is_tor_item-base_uom_val.
    IF iv_change_mode IS NOT INITIAL.
      ls_delivery_item-change_mode  = iv_change_mode.
    ELSE.
      zcl_gtt_sof_toolkit=>get_fu_from_db(
        EXPORTING
          it_tor_id = VALUE #( ( is_tor_root-tor_id ) )
        IMPORTING
          et_fu     = DATA(lt_fu_db) ).
      IF lt_fu_db IS INITIAL.
        ls_delivery_item-change_mode  = /bobf/if_frw_c=>sc_modify_create.
      ELSE.
        ls_delivery_item-change_mode  = /bobf/if_frw_c=>sc_modify_update.
      ENDIF.
    ENDIF.

    zcl_gtt_sof_toolkit=>convert_unit_output(
      EXPORTING
        iv_input  = is_tor_item-qua_pcs_uni
      RECEIVING
        rv_output = ls_delivery_item-quantityuom ).

    zcl_gtt_sof_toolkit=>convert_unit_output(
      EXPORTING
        iv_input  = is_tor_item-base_uom_uni
      RECEIVING
        rv_output = ls_delivery_item-base_uom_uni ).

    zcl_gtt_tools=>convert_matnr_to_external_frmt(
      EXPORTING
        iv_material = is_tor_item-product_id
      IMPORTING
        ev_result   = lv_matnr ).
    ls_delivery_item-product_id = lv_matnr.
    CLEAR lv_matnr.

    INSERT ls_delivery_item INTO TABLE ct_delivery_item.

  ENDMETHOD.


  METHOD ADD_DELIVERY_ITEMS_BY_TOR_ROOT.

    DATA: ls_delivery_item TYPE zif_gtt_sof_ctp_types=>ts_delivery_chng.

    LOOP AT mt_tor_item ASSIGNING FIELD-SYMBOL(<ls_tor_item>)
      USING KEY item_parent WHERE parent_node_id = is_tor_root-node_id.

      IF <ls_tor_item>-base_btd_tco = mv_base_btd_tco AND
         <ls_tor_item>-base_btd_id     IS NOT INITIAL AND
         <ls_tor_item>-base_btditem_id IS NOT INITIAL AND
         <ls_tor_item>-item_cat = /scmtms/if_tor_const=>sc_tor_item_category-product.
        add_delivery_item(
          EXPORTING
            is_tor_root      = is_tor_root
            is_tor_item      = <ls_tor_item>
            iv_change_mode   = iv_change_mode
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD CLEAR_DATA.

    CLEAR:
      mv_appsys,
      mv_base_btd_tco,
      mt_aotype,
      mt_tor_root,
      mt_tor_root_before,
      mt_tor_item,
      mt_tor_item_before,
      mt_tor_stop,
      mt_tor_stop_before,
      mt_delivery_item_chng,
      mt_delivery,
      mt_idoc_data,
      mt_delivery_item,
      mt_fu_info.

  ENDMETHOD.


  METHOD fill_delivery_header_data.

    DATA:
      lt_likp        TYPE HASHED TABLE OF likp WITH UNIQUE KEY vbeln,
      lt_lips        TYPE SORTED TABLE OF lipsvb WITH UNIQUE KEY vbeln posnr,
      lt_vbfa        TYPE STANDARD TABLE OF vbfavb,
      ls_lips        TYPE lipsvb,
      ls_vbuk        TYPE vbukvb,
      ls_vbup        TYPE vbupvb,
      lv_base_btd_id TYPE /scmtms/base_btd_id,
      ls_dlv_head    TYPE zif_gtt_sof_ctp_types=>ts_delivery.

    CLEAR: mt_delivery.

*  collect DLV Headers from DLV Items
    LOOP AT mt_delivery_item_chng ASSIGNING FIELD-SYMBOL(<ls_delivery_item>).
      READ TABLE mt_delivery TRANSPORTING NO FIELDS
        WITH KEY vbeln  = <ls_delivery_item>-vbeln.

      IF sy-subrc <> 0.
        ls_dlv_head-vbeln = <ls_delivery_item>-vbeln.
        APPEND ls_dlv_head TO mt_delivery.
      ENDIF.
    ENDLOOP.

    IF mt_delivery IS NOT INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_likp
        FROM likp
        FOR ALL ENTRIES IN mt_delivery
        WHERE vbeln = mt_delivery-vbeln.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_lips
        FROM lips
        FOR ALL ENTRIES IN mt_delivery
        WHERE vbeln = mt_delivery-vbeln.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
        FROM vbfa
        FOR ALL ENTRIES IN mt_delivery
       WHERE vbeln = mt_delivery-vbeln.

    ENDIF.

    LOOP AT mt_delivery ASSIGNING FIELD-SYMBOL(<ls_delivery>).

*     Delivery Header Status
      READ TABLE lt_likp INTO DATA(ls_likp) WITH KEY vbeln = <ls_delivery>-vbeln.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_likp TO ls_vbuk.
        APPEND ls_vbuk TO <ls_delivery>-vbuk.
        CLEAR ls_vbuk.
      ENDIF.

*     copy selected delivery header
      <ls_delivery>-likp  = VALUE #( lt_likp[ KEY primary_key
                                              COMPONENTS vbeln = <ls_delivery>-vbeln ]
                                       DEFAULT VALUE #( vbeln = <ls_delivery>-vbeln ) ).

*     Copy document flow data
      LOOP AT lt_vbfa INTO DATA(ls_vbfa) USING KEY primary_key
        WHERE vbeln = <ls_delivery>-vbeln.
        APPEND ls_vbfa TO <ls_delivery>-vbfa.
      ENDLOOP.

*     copy selected delivery items
      LOOP AT lt_lips ASSIGNING FIELD-SYMBOL(<ls_lips>)
        USING KEY primary_key
        WHERE vbeln = <ls_delivery>-vbeln.

        APPEND <ls_lips> TO <ls_delivery>-lips.

*       Delivery Item Status
        MOVE-CORRESPONDING <ls_lips> TO ls_vbup.
        APPEND ls_vbup TO <ls_delivery>-vbup.
        CLEAR:
          ls_vbup.
      ENDLOOP.

*     Check if there is any freight unit assigned to the delivery
      <ls_delivery>-fu_relevant = abap_false.

      CLEAR lv_base_btd_id.
      lv_base_btd_id = |{ <ls_delivery>-vbeln ALPHA = IN }|.
      LOOP AT mt_fu_info INTO DATA(ls_fu_info)
        WHERE base_btd_id  = lv_base_btd_id
          AND lifecycle <> /scmtms/if_tor_status_c=>sc_root-lifecycle-v_canceled.
        <ls_delivery>-fu_relevant = abap_true.
        EXIT.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_delivery_item_data.

    DATA:
      ls_delivery_item TYPE zif_gtt_sof_ctp_types=>ts_delivery_item,
      lt_likp          TYPE HASHED TABLE OF likp WITH UNIQUE KEY vbeln,
      lt_lips          TYPE SORTED TABLE OF lipsvb WITH UNIQUE KEY vbeln posnr,
      lt_vbfa          TYPE STANDARD TABLE OF vbfavb,
      ls_vbuk          TYPE vbukvb,
      ls_lips          TYPE lipsvb,
      lv_base_btd_id   TYPE /scmtms/base_btd_id.

    CLEAR:mt_delivery_item.

    LOOP AT mt_delivery_item_chng ASSIGNING FIELD-SYMBOL(<ls_delivery_chng>).
      READ TABLE mt_delivery_item ASSIGNING FIELD-SYMBOL(<ls_delivery_item>)
        WITH KEY vbeln = <ls_delivery_chng>-vbeln
                 posnr = <ls_delivery_chng>-posnr.

      IF sy-subrc <> 0.
        CLEAR: ls_delivery_item.
        ls_delivery_item-vbeln  = <ls_delivery_chng>-vbeln.
        ls_delivery_item-posnr  = <ls_delivery_chng>-posnr.
        APPEND ls_delivery_item TO mt_delivery_item.

        ASSIGN mt_delivery_item[ vbeln = <ls_delivery_chng>-vbeln posnr = <ls_delivery_chng>-posnr ] TO <ls_delivery_item>.
      ENDIF.

      IF <ls_delivery_item> IS ASSIGNED.
        <ls_delivery_item>-fu_list  = VALUE #( BASE <ls_delivery_item>-fu_list (
          tor_id        = <ls_delivery_chng>-tor_id
          item_id       = <ls_delivery_chng>-item_id
          quantity      = <ls_delivery_chng>-quantity
          quantityuom   = <ls_delivery_chng>-quantityuom
          product_id    = <ls_delivery_chng>-product_id
          product_descr = <ls_delivery_chng>-product_descr
          change_mode   = <ls_delivery_chng>-change_mode
          base_uom_val  = <ls_delivery_chng>-base_uom_val
          base_uom_uni  = <ls_delivery_chng>-base_uom_uni
        ) ).

        UNASSIGN <ls_delivery_item>.
      ENDIF.
    ENDLOOP.

    IF mt_delivery_item IS NOT INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_likp
        FROM likp
        FOR ALL ENTRIES IN mt_delivery_item
        WHERE vbeln = mt_delivery_item-vbeln.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_lips
        FROM lips
        FOR ALL ENTRIES IN mt_delivery_item
        WHERE vbeln = mt_delivery_item-vbeln
          AND posnr = mt_delivery_item-posnr.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
        FROM vbfa
        FOR ALL ENTRIES IN mt_delivery_item
       WHERE vbeln = mt_delivery_item-vbeln.

      LOOP AT mt_delivery_item ASSIGNING <ls_delivery_item>.
        " copy selected delivery header
        <ls_delivery_item>-likp  = VALUE #(
          lt_likp[ KEY primary_key
                   COMPONENTS vbeln = <ls_delivery_item>-vbeln ]
          DEFAULT VALUE #( vbeln = <ls_delivery_item>-vbeln )
        ).

        " copy selected delivery item
        <ls_delivery_item>-lips  = VALUE #(
          lt_lips[ KEY primary_key
                   COMPONENTS vbeln = <ls_delivery_item>-vbeln
                              posnr = <ls_delivery_item>-posnr ]
          DEFAULT VALUE #( vbeln = <ls_delivery_item>-vbeln
                           posnr = <ls_delivery_item>-posnr )
        ).

*       Delivery Header Status
        READ TABLE lt_likp INTO DATA(ls_likp) WITH KEY vbeln = <ls_delivery_item>-vbeln.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING ls_likp TO <ls_delivery_item>-vbuk.
        ENDIF.

*       Delivery Item Status
        LOOP AT lt_lips ASSIGNING FIELD-SYMBOL(<ls_lips>)
          USING KEY primary_key
          WHERE vbeln = <ls_delivery_item>-vbeln
            AND posnr = <ls_delivery_item>-posnr.

          MOVE-CORRESPONDING <ls_lips> TO <ls_delivery_item>-vbup.
        ENDLOOP.

*       Copy document flow data
        LOOP AT lt_vbfa INTO DATA(ls_vbfa) USING KEY primary_key
          WHERE vbeln = <ls_delivery_item>-vbeln
            AND posnn = <ls_delivery_item>-posnr.
          APPEND ls_vbfa TO <ls_delivery_item>-vbfa.
        ENDLOOP.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  method FILL_IDOC_APPOBJ_CTABS.

    cs_idoc_data-appobj_ctabs = VALUE #( BASE cs_idoc_data-appobj_ctabs (
      trxservername = cs_idoc_data-trxserv-trx_server_id
      appobjtype    = is_aotype-aot_type
      appobjid      = |{ is_likp-vbeln ALPHA = OUT }|
    ) ).

  endmethod.


  METHOD FILL_IDOC_CONTROL_DATA.

    DATA: lt_control TYPE /saptrx/bapi_trk_control_tab,
          lv_count   TYPE i VALUE 0.

    lt_control  = VALUE #(
      (
        paramname = cs_mapping-vbeln
        value     = |{ is_delivery-vbeln ALPHA = OUT }|
      )
      (
        paramname = cs_mapping-fu_relevant
        value     = is_delivery-fu_relevant
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-actual_bisiness_timezone
        value     = zcl_gtt_sof_tm_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-actual_bisiness_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-actual_technical_timezone
        value     = zcl_gtt_sof_tm_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-actual_technical_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-reported_by
        value     = sy-uname
      )
    ).

    " fill technical data into all control data records
    LOOP AT lt_control ASSIGNING FIELD-SYMBOL(<ls_control>).
      <ls_control>-appsys     = mv_appsys.
      <ls_control>-appobjtype = is_aotype-aot_type.
      <ls_control>-appobjid   = |{ is_delivery-vbeln ALPHA = OUT }|.
    ENDLOOP.

    cs_idoc_data-control  = VALUE #( BASE cs_idoc_data-control
                                     ( LINES OF lt_control ) ).

  ENDMETHOD.


  METHOD FILL_IDOC_EXP_EVENT.

    DATA: lt_exp_event TYPE /saptrx/bapi_trk_ee_tab.

    " do not retrieve planned events when DLV is not stored in DB yet
    " (this is why all the fields of LIKP are empty, except VBELN)
    IF is_delivery-likp-lfart IS NOT INITIAL AND
       is_delivery-lips[] IS NOT INITIAL AND
       is_delivery-vbuk[] IS NOT INITIAL AND
       is_delivery-vbup[] IS NOT INITIAL AND
       is_delivery-vbfa[] IS NOT INITIAL.

      TRY.
          zcl_gtt_sof_tm_tools=>get_delivery_head_planned_evt(
            EXPORTING
              iv_appsys    = mv_appsys
              is_aotype    = is_aotype
              is_likp      = CORRESPONDING #( is_delivery-likp )
              it_lips      = CORRESPONDING #( is_delivery-lips )
              it_vbuk      = CORRESPONDING #( is_delivery-vbuk )
              it_vbup      = CORRESPONDING #( is_delivery-vbup )
              it_vbfa      = CORRESPONDING #( is_delivery-vbfa )
            IMPORTING
              et_exp_event = lt_exp_event ).

          IF lt_exp_event[] IS INITIAL.
            lt_exp_event = VALUE #( (
                milestone         = ''
                locid2            = ''
                loctype           = ''
                locid1            = ''
                evt_exp_datetime  = '000000000000000'
                evt_exp_tzone     = ''
            ) ).
          ENDIF.

          LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
            <ls_exp_event>-appsys         = mv_appsys.
            <ls_exp_event>-appobjtype     = is_aotype-aot_type.
            <ls_exp_event>-appobjid       = |{ is_delivery-vbeln ALPHA = OUT }|.
            <ls_exp_event>-language       = sy-langu.
            IF <ls_exp_event>-evt_exp_tzone IS INITIAL.
              <ls_exp_event>-evt_exp_tzone  = zcl_gtt_sof_tm_tools=>get_system_time_zone( ).
            ENDIF.
          ENDLOOP.

          cs_idoc_data-exp_event = VALUE #( BASE cs_idoc_data-exp_event
                                            ( LINES OF lt_exp_event ) ).

        CATCH cx_udm_message INTO DATA(lo_udm_message).
          " do not throw exception when DLV is not stored in DB yet
          " (this is why all the fields of LIKP are empty, except VBELN)
          IF is_delivery-likp-lfart IS NOT INITIAL.
            RAISE EXCEPTION TYPE cx_udm_message
              EXPORTING
                textid   = lo_udm_message->textid
                previous = lo_udm_message->previous
                m_msgid  = lo_udm_message->m_msgid
                m_msgty  = lo_udm_message->m_msgty
                m_msgno  = lo_udm_message->m_msgno
                m_msgv1  = lo_udm_message->m_msgv1
                m_msgv2  = lo_udm_message->m_msgv2
                m_msgv3  = lo_udm_message->m_msgv3
                m_msgv4  = lo_udm_message->m_msgv4.
          ENDIF.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD FILL_IDOC_TRACKING_ID.

    DATA:
      lv_dlvhdtrxcod     TYPE /saptrx/trxcod.

    lv_dlvhdtrxcod = zif_gtt_sof_constants=>cs_trxcod-out_delivery.

    " Delivery Header
    cs_idoc_data-tracking_id  = VALUE #( BASE cs_idoc_data-tracking_id (
      appsys      = mv_appsys
      appobjtype  = is_aotype-aot_type
      appobjid    = |{ is_delivery-vbeln ALPHA = OUT }|
      trxcod      = lv_dlvhdtrxcod
      trxid       = |{ is_delivery-vbeln ALPHA = OUT }|
    ) ).

  ENDMETHOD.


  method FILL_IDOC_TRXSERV.

    SELECT SINGLE *
      INTO cs_idoc_data-trxserv
      FROM /saptrx/trxserv
      WHERE trx_server_id = is_aotype-server_name.

  endmethod.


  METHOD GET_DLV_ITEM_BASED_ON_TOR_ITEM.

    DATA ls_delivery_item TYPE zif_gtt_sof_ctp_types=>ts_delivery_chng.

    LOOP AT mt_tor_item ASSIGNING FIELD-SYMBOL(<ls_tor_item>)
      WHERE base_btd_tco = mv_base_btd_tco AND
            ( change_mode = /bobf/if_frw_c=>sc_modify_delete OR
              change_mode = /bobf/if_frw_c=>sc_modify_create ) AND
              base_btd_id IS NOT INITIAL AND
              base_btditem_id IS NOT INITIAL AND
             item_cat = /scmtms/if_tor_const=>sc_tor_item_category-product.

      ASSIGN mt_tor_root[ node_id = <ls_tor_item>-parent_node_id ] TO FIELD-SYMBOL(<ls_tor_root>).
      IF sy-subrc = 0 AND
         <ls_tor_root>-tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit.

        add_delivery_item(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            is_tor_item      = <ls_tor_item>
            iv_change_mode   = <ls_tor_item>-change_mode
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_dlv_item_based_on_tor_root.

    LOOP AT mt_tor_root ASSIGNING FIELD-SYMBOL(<ls_tor_root>)
      WHERE tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit
        AND base_btd_tco = mv_base_btd_tco.

      add_delivery_items_by_tor_root(
        EXPORTING
          is_tor_root      = <ls_tor_root>
          iv_change_mode   = <ls_tor_root>-change_mode
        CHANGING
          ct_delivery_item = ct_delivery_item ).
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_DLV_ITEM_BASED_ON_TOR_STOP.

    DATA lv_tabix TYPE sy-tabix.

    LOOP AT mt_tor_stop ASSIGNING FIELD-SYMBOL(<ls_tor_stop>)
      GROUP BY ( parent_node_id = <ls_tor_stop>-parent_node_id
                 group_size     = GROUP SIZE ) ASSIGNING FIELD-SYMBOL(<lt_group>).

      LOOP AT GROUP <lt_group> ASSIGNING FIELD-SYMBOL(<ls_tor_stop_current>).
        lv_tabix += 1.
        CHECK lv_tabix = <lt_group>-group_size.

        ASSIGN mt_tor_stop_before[ node_id = <ls_tor_stop_current>-node_id ] TO FIELD-SYMBOL(<ls_tor_stop_before>).
        CHECK ( sy-subrc = 0 AND <ls_tor_stop_current>-log_locid <> <ls_tor_stop_before>-log_locid ) OR sy-subrc <> 0.

        " Freight Unit destination was changed => send IDOC
        ASSIGN mt_tor_root[ node_id = <ls_tor_stop_current>-parent_node_id ] TO FIELD-SYMBOL(<ls_tor_root>).
        CHECK sy-subrc = 0 AND <ls_tor_root>-tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit.

        add_delivery_items_by_tor_root(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = <ls_tor_root>-change_mode
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDLOOP.

      CLEAR lv_tabix.
    ENDLOOP.

  ENDMETHOD.


  METHOD INITIATE_AOTYPES.
    DATA:
      lt_aotype TYPE zif_gtt_ctp_types=>tt_aotype_rst.

    SELECT rst_option AS option,
           rst_sign   AS sign,
           rst_low    AS low,
           rst_high   AS high
      INTO CORRESPONDING FIELDS OF TABLE @lt_aotype
      FROM zgtt_aotype_rst
     WHERE rst_id = @iv_rst_id.

    " Prepare AOT list
    IF lt_aotype IS NOT INITIAL.
      SELECT trk_obj_type  AS obj_type
             aotype        AS aot_type
             trxservername AS server_name
        INTO TABLE mt_aotype
        FROM /saptrx/aotypes
        WHERE trk_obj_type  = zif_gtt_ef_constants=>cs_trk_obj_type-esc_deliv
          AND aotype       IN lt_aotype
          AND torelevant    = abap_true.

      IF sy-subrc <> 0.
        MESSAGE e008(zgtt) INTO DATA(lv_dummy).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD IS_EXTRACTOR_EXIST.

    DATA: lv_trk_obj_type  TYPE /saptrx/aotypes-trk_obj_type.

    SELECT SINGLE trk_obj_type
      INTO lv_trk_obj_type
      FROM /saptrx/aotypes
      WHERE trk_obj_type = iv_trk_obj_type
        AND torelevant   = abap_true.

    rv_result   = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD is_gtt_enabled.

    DATA: lv_extflag      TYPE flag.

    rv_result   = abap_false.

*   Check package dependent BADI disabling
    CALL FUNCTION 'GET_R3_EXTENSION_SWITCH'
      EXPORTING
        i_structure_package = zif_gtt_ef_constants=>cv_structure_pkg
      IMPORTING
        e_active            = lv_extflag
      EXCEPTIONS
        not_existing        = 1
        object_not_existing = 2
        no_extension_object = 3
        OTHERS              = 4.

    IF sy-subrc = 0 AND lv_extflag = abap_true.
*     Check if any tracking server defined
      CALL FUNCTION '/SAPTRX/EVENT_MGR_CHECK'
        EXCEPTIONS
          no_event_mgr_available = 1
          OTHERS                 = 2.

*     Check whether at least 1 active extractor exists for every object
      IF sy-subrc = 0.
        IF is_extractor_exist( iv_trk_obj_type = iv_trk_obj_type ) = abap_false.
          rv_result   = abap_false.
        ELSE.
          rv_result   = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD PREPARE_IDOC_DATA.

    DATA:
      ls_idoc_data    TYPE zif_gtt_sof_ctp_types=>ts_idoc_data.

    LOOP AT mt_aotype ASSIGNING FIELD-SYMBOL(<ls_aotype>).
      CLEAR: ls_idoc_data.

      ls_idoc_data-appsys   = mv_appsys.

      fill_idoc_trxserv(
        EXPORTING
          is_aotype    = <ls_aotype>
        CHANGING
          cs_idoc_data = ls_idoc_data ).

      LOOP AT mt_delivery ASSIGNING FIELD-SYMBOL(<ls_dlv>).

        IF <ls_dlv>-likp-lfart IS INITIAL. " Skip cross TP if DLV not stored in DB
          CONTINUE.
        ENDIF.

        fill_idoc_appobj_ctabs(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_likp      = <ls_dlv>
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_control_data(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_delivery  = <ls_dlv>
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_exp_event(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_delivery  = <ls_dlv>
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_tracking_id(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_delivery  = <ls_dlv>
          CHANGING
            cs_idoc_data = ls_idoc_data ).
      ENDLOOP.

      IF ls_idoc_data-appobj_ctabs[] IS NOT INITIAL AND
         ls_idoc_data-control[] IS NOT INITIAL AND
         ls_idoc_data-tracking_id[] IS NOT INITIAL.
        APPEND ls_idoc_data TO mt_idoc_data.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD SEND_IDOC_DATA.

    DATA:
      lt_bapiret1 TYPE bapiret2_t,
      lt_bapiret2 TYPE bapiret2_t.

    LOOP AT mt_idoc_data ASSIGNING FIELD-SYMBOL(<ls_idoc_data>).
      CLEAR: lt_bapiret1[], lt_bapiret2[].

      /saptrx/cl_send_idocs=>send_idoc_ehpost01(
        EXPORTING
          it_control      = <ls_idoc_data>-control
          it_info         = <ls_idoc_data>-info
          it_tracking_id  = <ls_idoc_data>-tracking_id
          it_exp_event    = <ls_idoc_data>-exp_event
          is_trxserv      = <ls_idoc_data>-trxserv
          iv_appsys       = <ls_idoc_data>-appsys
          it_appobj_ctabs = <ls_idoc_data>-appobj_ctabs
          iv_upd_task     = 'X'
        IMPORTING
          et_bapireturn   = lt_bapiret1 ).

      " when GTT.2 version
      IF /saptrx/cl_send_idocs=>st_idoc_data[] IS NOT INITIAL.
        /saptrx/cl_send_idocs=>send_idoc_gttmsg01(
          IMPORTING
            et_bapireturn = lt_bapiret2 ).
      ENDIF.

      " collect messages, if it is necessary
      IF et_bapiret IS REQUESTED.
        et_bapiret    = VALUE #( BASE et_bapiret
                                 ( LINES OF lt_bapiret1 )
                                 ( LINES OF lt_bapiret2 ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_gtt_ctp_tor_to_dl~check_relevance.

    DATA:
      lt_fu_db TYPE /scmtms/t_tor_root_k.

    CLEAR:
      rv_result,
      mt_delivery_item_chng.

    get_dlv_item_based_on_tor_root(
      CHANGING
        ct_delivery_item = mt_delivery_item_chng ).

    get_dlv_item_based_on_tor_stop(
      CHANGING
        ct_delivery_item = mt_delivery_item_chng ).

    check_fu_status(
      IMPORTING
        et_delivery_info = DATA(lt_delivery_info) ).

    LOOP AT mt_delivery_item_chng INTO DATA(ls_delivery_item_chng).
      READ TABLE lt_delivery_info INTO DATA(ls_delivery_info)
        WITH KEY vbeln = ls_delivery_item_chng-vbeln.
      IF sy-subrc = 0.
        IF ls_delivery_info-process_flg = abap_false.
          DELETE mt_delivery_item_chng WHERE vbeln = ls_delivery_info-vbeln.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF mt_delivery_item_chng IS NOT INITIAL.
      rv_result = zif_gtt_sts_ef_constants=>cs_condition-true.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_GTT_CTP_TOR_TO_DL~EXTRACT_DATA.

    fill_delivery_header_data( ).

  ENDMETHOD.


  METHOD zif_gtt_ctp_tor_to_dl~initiate.

    CLEAR rv_error_flag.

    mt_tor_root        = it_tor_root.
    mt_tor_root_before = it_tor_root_before.
    mt_tor_item        = it_tor_item.
    mt_tor_item_before = it_tor_item_before.
    mt_tor_stop        = it_tor_stop.
    mt_tor_stop_before = it_tor_stop_before.
    mt_fu_info         = it_fu_info.

    IF is_gtt_enabled( iv_trk_obj_type = zif_gtt_ef_constants=>cs_trk_obj_type-esc_deliv ) = abap_false.
      rv_error_flag = abap_true.
      RETURN.
    ENDIF.

*   Get current logical system
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = mv_appsys
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.

    IF sy-subrc <> 0.
      MESSAGE e007(zgtt_ssof) INTO DATA(lv_dummy).
      rv_error_flag = abap_true.
      RETURN.
    ENDIF.

*   Get Restriction ID
    get_aotype_restriction_id(
      RECEIVING
        rv_rst_id = DATA(lv_rst_id) ).

    initiate_aotypes( iv_rst_id = lv_rst_id ).

*   Get Base Document Type
    LOOP AT mt_tor_root ASSIGNING FIELD-SYMBOL(<ls_tor_root>)
      WHERE tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit
        AND ( base_btd_tco = /scmtms/if_common_c=>c_btd_tco-outbounddelivery
         OR base_btd_tco = /scmtms/if_common_c=>c_btd_tco-inbounddelivery ).
      mv_base_btd_tco = <ls_tor_root>-base_btd_tco.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_GTT_CTP_TOR_TO_DL~PROCESS_DATA.

    prepare_idoc_data( ).

    send_idoc_data(
      IMPORTING
        et_bapiret = et_bapiret ).

    clear_data( ).

  ENDMETHOD.


  METHOD check_fu_status.

    DATA:
      lt_delivery           TYPE tt_delivery,
      ls_delivery           TYPE ts_delivery,
      lt_delivery_info      TYPE tt_delivery_info,
      ls_delivery_info      TYPE ts_delivery_info,
      lv_delivery_exist_flg TYPE flag,
      lt_fu_db              TYPE /scmtms/t_tor_root_k.

    CLEAR et_delivery_info.

    LOOP AT mt_delivery_item_chng ASSIGNING FIELD-SYMBOL(<ls_delivery_item>).
      READ TABLE lt_delivery TRANSPORTING NO FIELDS
        WITH KEY vbeln = <ls_delivery_item>-vbeln.
      IF sy-subrc <> 0.
        ls_delivery-vbeln = <ls_delivery_item>-vbeln.
        APPEND ls_delivery TO lt_delivery.
        CLEAR ls_delivery.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_delivery INTO ls_delivery.
      CLEAR ls_delivery_info.
      LOOP AT mt_delivery_item_chng INTO DATA(ls_delivery_item)
        WHERE vbeln = ls_delivery-vbeln.
        APPEND ls_delivery_item TO ls_delivery_info-dlv_items.
        APPEND ls_delivery_item-tor_id TO ls_delivery_info-tor_id.
      ENDLOOP.
      IF ls_delivery_info IS NOT INITIAL.
        ls_delivery_info-vbeln = ls_delivery-vbeln.
        SORT ls_delivery_info-tor_id BY table_line.
        DELETE ADJACENT DUPLICATES FROM ls_delivery_info-tor_id COMPARING ALL FIELDS.
        APPEND ls_delivery_info TO lt_delivery_info.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_delivery_info ASSIGNING FIELD-SYMBOL(<fs_delivery_info>).
      CLEAR:
        lv_delivery_exist_flg,
        lt_fu_db.

      <fs_delivery_info>-process_flg = abap_false.

      SELECT COUNT(*)
        FROM likp
       WHERE vbeln = <fs_delivery_info>-vbeln.
      IF sy-dbcnt > 0.
        lv_delivery_exist_flg = abap_true.
      ENDIF.

      zcl_gtt_sof_toolkit=>get_fu_from_db(
        EXPORTING
          it_tor_id = <fs_delivery_info>-tor_id
        IMPORTING
          et_fu     = lt_fu_db ).

*     DLV in DB,TOR not in DB(new TOR), send the IDOC
      IF lv_delivery_exist_flg IS NOT INITIAL AND lt_fu_db IS INITIAL.
        <fs_delivery_info>-process_flg = abap_true.

*     DLV in DB,TOR in DB,and TOR in progress, do not sent out IDOC
*     FU is deleted or canceled, send the IDOC
      ELSEIF lv_delivery_exist_flg IS NOT INITIAL AND lt_fu_db IS NOT INITIAL.

        determine_process_flag(
          EXPORTING
            it_tor_id      = <fs_delivery_info>-tor_id
            it_fu_db       = lt_fu_db
          IMPORTING
            ev_process_flg = <fs_delivery_info>-process_flg ).

      ENDIF.

    ENDLOOP.

    et_delivery_info = lt_delivery_info.

  ENDMETHOD.


  METHOD DETERMINE_PROCESS_FLAG.

    DATA:
      lv_all_fu_in_db_flg   TYPE flag.

    CLEAR ev_process_flg.

    LOOP AT it_tor_id INTO DATA(ls_tor_id).
      READ TABLE it_fu_db TRANSPORTING NO FIELDS
        WITH KEY tor_id COMPONENTS tor_id = ls_tor_id.
      IF sy-subrc = 0.
        lv_all_fu_in_db_flg = abap_true.
      ELSE.
        lv_all_fu_in_db_flg = abap_false.
      ENDIF.
    ENDLOOP.

    IF lv_all_fu_in_db_flg = abap_true.
      LOOP AT it_tor_id INTO ls_tor_id.
        READ TABLE mt_tor_root INTO DATA(ls_tor_root)
          WITH KEY tor_id = ls_tor_id.
        IF sy-subrc = 0.
*         FU is deleted or canceled, send the IDOC
          IF ( ls_tor_root-change_mode = /bobf/if_frw_c=>sc_modify_delete
            OR ls_tor_root-lifecycle   = /scmtms/if_tor_status_c=>sc_root-lifecycle-v_canceled ).
            ev_process_flg = abap_true.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      ev_process_flg = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_CTP_TOR_TO_IDLVHD definition
  public
  inheriting from ZCL_GTT_CTP_TOR_TO_DL_BASE
  create public .

public section.
protected section.

  methods FILL_IDOC_EXP_EVENT
    redefinition .
  methods GET_AOTYPE_RESTRICTION_ID
    redefinition .
  methods FILL_IDOC_TRACKING_ID
    redefinition .
private section.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_CTP_TOR_TO_IDLVHD IMPLEMENTATION.


  METHOD fill_idoc_exp_event.

    DATA: lt_exp_event TYPE /saptrx/bapi_trk_ee_tab.

    " do not retrieve planned events when DLV is not stored in DB yet
    " (this is why all the fields of LIKP are empty, except VBELN)
    IF is_delivery-likp-lfart IS NOT INITIAL AND
       is_delivery-lips[] IS NOT INITIAL.

      TRY.
          zcl_gtt_mia_ctp_tools=>get_delivery_head_planned_evt(
            EXPORTING
              iv_appsys    = mv_appsys
              is_aotype    = is_aotype
              is_likp      = CORRESPONDING #( is_delivery-likp )
              it_lips      = CORRESPONDING #( is_delivery-lips )
            IMPORTING
              et_exp_event = lt_exp_event ).

          IF lt_exp_event[] IS INITIAL.
            lt_exp_event = VALUE #( (
                milestone         = ''
                locid2            = ''
                loctype           = ''
                locid1            = ''
                evt_exp_datetime  = '000000000000000'
                evt_exp_tzone     = ''
            ) ).
          ENDIF.

          LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
            <ls_exp_event>-appsys         = mv_appsys.
            <ls_exp_event>-appobjtype     = is_aotype-aot_type.
            <ls_exp_event>-appobjid = zcl_gtt_mia_dl_tools=>get_tracking_id_dl_header(
              ir_likp = REF #( is_delivery ) ).
            <ls_exp_event>-language       = sy-langu.
            IF <ls_exp_event>-evt_exp_tzone IS INITIAL.
              <ls_exp_event>-evt_exp_tzone  = zcl_gtt_tools=>get_system_time_zone( ).
            ENDIF.
          ENDLOOP.

          cs_idoc_data-exp_event = VALUE #( BASE cs_idoc_data-exp_event
                                            ( LINES OF lt_exp_event ) ).

        CATCH cx_udm_message INTO DATA(lo_udm_message).
          " do not throw exception when DLV is not stored in DB yet
          " (this is why all the fields of LIKP are empty, except VBELN)
          IF is_delivery-likp-lfart IS NOT INITIAL.
            RAISE EXCEPTION TYPE cx_udm_message
              EXPORTING
                textid   = lo_udm_message->textid
                previous = lo_udm_message->previous
                m_msgid  = lo_udm_message->m_msgid
                m_msgty  = lo_udm_message->m_msgty
                m_msgno  = lo_udm_message->m_msgno
                m_msgv1  = lo_udm_message->m_msgv1
                m_msgv2  = lo_udm_message->m_msgv2
                m_msgv3  = lo_udm_message->m_msgv3
                m_msgv4  = lo_udm_message->m_msgv4.
          ENDIF.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD fill_idoc_tracking_id.

    DATA:
      lv_dlvhdtrxcod     TYPE /saptrx/trxcod.

    lv_dlvhdtrxcod = zif_gtt_ef_constants=>cs_trxcod-dl_number.

    " Delivery Header
    cs_idoc_data-tracking_id  = VALUE #( BASE cs_idoc_data-tracking_id (
      appsys      = mv_appsys
      appobjtype  = is_aotype-aot_type
      appobjid    = |{ is_delivery-vbeln ALPHA = OUT }|
      trxcod      = lv_dlvhdtrxcod
      trxid       = |{ is_delivery-vbeln ALPHA = OUT }|

    ) ).

  ENDMETHOD.


  method GET_AOTYPE_RESTRICTION_ID.

    rv_rst_id   = 'FU_TO_IDLH'.

  endmethod.
ENDCLASS.""",
    r"""class ZCL_GTT_CTP_TOR_TO_IDLVIT definition
  public
  inheriting from ZCL_GTT_CTP_TOR_TO_DL_BASE
  create public .

public section.

  methods ZIF_GTT_CTP_TOR_TO_DL~EXTRACT_DATA
    redefinition .
protected section.

  methods FILL_IDOC_APPOBJ_CTABS
    redefinition .
  methods FILL_IDOC_CONTROL_DATA
    redefinition .
  methods FILL_IDOC_EXP_EVENT
    redefinition .
  methods FILL_IDOC_TRACKING_ID
    redefinition .
  methods GET_AOTYPE_RESTRICTION_ID
    redefinition .
  methods PREPARE_IDOC_DATA
    redefinition .
private section.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_CTP_TOR_TO_IDLVIT IMPLEMENTATION.


  METHOD fill_idoc_appobj_ctabs.

    DATA:
      lv_appobjid TYPE /saptrx/aoid.

    lv_appobjid = |{ is_delivery_item-lips-vbeln ALPHA = OUT }{ is_delivery_item-lips-posnr ALPHA = IN }|.
    CONDENSE lv_appobjid NO-GAPS.

    cs_idoc_data-appobj_ctabs = VALUE #( BASE cs_idoc_data-appobj_ctabs (
      trxservername = cs_idoc_data-trxserv-trx_server_id
      appobjtype    = is_aotype-aot_type
      appobjid      = lv_appobjid
    ) ).

  ENDMETHOD.


  METHOD fill_idoc_control_data.

    DATA:
      lt_control     TYPE /saptrx/bapi_trk_control_tab,
      lv_count       TYPE i VALUE 0,
      lv_appobjid    TYPE /saptrx/aoid,
      lt_tmp_fu_list TYPE zif_gtt_mia_ctp_types=>tt_fu_list,
      ls_fu_create   TYPE zif_gtt_sof_ctp_types=>ts_fu_list,
      ls_fu_update   TYPE zif_gtt_sof_ctp_types=>ts_fu_list,
      ls_fu_delete   TYPE zif_gtt_sof_ctp_types=>ts_fu_list,
      lt_fu_id       TYPE zif_gtt_sof_ctp_types=>tt_fu_id.

    " DL Item key data (obligatory)
    lt_control  = VALUE #(
      (
        paramname = cs_mapping-dlv_vbeln
        value     = |{ is_delivery_item-likp-vbeln ALPHA = OUT }|
      )
      (
        paramname = cs_mapping-dlv_posnr
        value     = is_delivery_item-lips-posnr
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_timezone
        value     = zcl_gtt_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_bisiness_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_technical_timezone
        value     = zcl_gtt_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-actual_technical_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_ef_constants=>cs_system_fields-reported_by
        value     = sy-uname
      )
    ).

    " fill F.U. table
    LOOP AT is_delivery_item-fu_list ASSIGNING FIELD-SYMBOL(<ls_fu_list>).
      IF <ls_fu_list>-change_mode = /bobf/if_frw_c=>sc_modify_delete.
        CONTINUE.
      ENDIF.
      APPEND <ls_fu_list> TO lt_tmp_fu_list.
      ADD 1 TO lv_count.

      lt_control  = VALUE #( BASE lt_control
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_lineno
          value       = zcl_gtt_tools=>get_pretty_value(
                          iv_value = lv_count )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_freightunit
          value       = zcl_gtt_mia_tm_tools=>get_formated_tor_id(
                          ir_data = REF #( <ls_fu_list> ) )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_itemnumber
          value       = zcl_gtt_mia_tm_tools=>get_formated_tor_item(
                          ir_data = REF #( <ls_fu_list> ) )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_quantity
          value       = zcl_gtt_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-quantity )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_quantityuom
          value       = zcl_gtt_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-quantityuom )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_product_id
          value       = zcl_gtt_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-product_id )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_product_descr
          value       = zcl_gtt_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-product_descr )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_base_uom_val
          value       = zcl_gtt_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-base_uom_val )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_base_uom_uni
          value       = zcl_gtt_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-base_uom_uni )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_freightunit_logsys
          value       = mv_appsys
        )
      ).
    ENDLOOP.

    " add deletion sign in case of emtpy is_delivery_item-FU_LIST table
    IF lt_tmp_fu_list IS INITIAL.
      lt_control  = VALUE #( BASE lt_control (
          paramindex = 1
          paramname  = cs_mapping-fu_lineno
          value      = ''
      ) ).
    ENDIF.

*   Add tracking ID information
    CLEAR lv_count.
    lt_fu_id =  CORRESPONDING #( is_delivery_item-fu_list ).
    SORT lt_fu_id BY tor_id.
    DELETE ADJACENT DUPLICATES FROM lt_fu_id COMPARING tor_id.

    LOOP AT lt_fu_id  ASSIGNING FIELD-SYMBOL(<ls_fu_id>).
      ADD 1 TO lv_count.
      CLEAR:
        ls_fu_create,
        ls_fu_update,
        ls_fu_delete.

      READ TABLE is_delivery_item-fu_list INTO ls_fu_create WITH KEY tor_id = <ls_fu_id>-tor_id change_mode = /bobf/if_frw_c=>sc_modify_create.
      READ TABLE is_delivery_item-fu_list INTO ls_fu_update WITH KEY tor_id = <ls_fu_id>-tor_id change_mode = /bobf/if_frw_c=>sc_modify_update.
      READ TABLE is_delivery_item-fu_list INTO ls_fu_delete WITH KEY tor_id = <ls_fu_id>-tor_id change_mode = /bobf/if_frw_c=>sc_modify_delete.

      IF ls_fu_create IS NOT INITIAL
        AND ls_fu_update IS INITIAL
        AND ls_fu_delete IS INITIAL.

        lt_control  = VALUE #( BASE lt_control
          (
            paramindex  = lv_count
            paramname   = cs_mapping-appsys
            value       = mv_appsys
          )
          (
            paramindex  = lv_count
            paramname   = cs_mapping-trxcod
            value       = zif_gtt_sof_constants=>cs_trxcod-fu_number
          )
          (
            paramindex  = lv_count
            paramname   = cs_mapping-trxid
            value       = zcl_gtt_sof_tm_tools=>get_formated_tor_id( ir_data = REF #( <ls_fu_id> ) )
          )
        ).
      ENDIF.

      IF ls_fu_delete IS NOT INITIAL
       AND ls_fu_create IS INITIAL
       AND ls_fu_update IS INITIAL.
        lt_control  = VALUE #( BASE lt_control
          (
            paramindex  = lv_count
            paramname   = cs_mapping-appsys
            value       = mv_appsys
          )
          (
            paramindex  = lv_count
            paramname   = cs_mapping-trxcod
            value       = zif_gtt_sof_constants=>cs_trxcod-fu_number
          )
          (
            paramindex  = lv_count
            paramname   = cs_mapping-trxid
            value       = zcl_gtt_sof_tm_tools=>get_formated_tor_id( ir_data = REF #( <ls_fu_id> ) )
          )
          (
            paramindex  = lv_count
            paramname   = cs_mapping-action
            value       = /bobf/if_frw_c=>sc_modify_delete
          )
        ).
      ENDIF.
    ENDLOOP.

    " fill technical data into all control data records
    lv_appobjid = |{ is_delivery_item-lips-vbeln ALPHA = OUT }{ is_delivery_item-lips-posnr ALPHA = IN }|.
    CONDENSE lv_appobjid NO-GAPS.
    LOOP AT lt_control ASSIGNING FIELD-SYMBOL(<ls_control>).
      <ls_control>-appsys     = mv_appsys.
      <ls_control>-appobjtype = is_aotype-aot_type.
      <ls_control>-appobjid   = lv_appobjid .
    ENDLOOP.

    cs_idoc_data-control  = VALUE #( BASE cs_idoc_data-control
                                     ( LINES OF lt_control ) ).
  ENDMETHOD.


  METHOD fill_idoc_exp_event.

    DATA:
      lt_exp_event TYPE /saptrx/bapi_trk_ee_tab,
      lv_appobjid  TYPE /saptrx/aoid,
      ls_likp      TYPE likp.

    " do not retrieve planned events when DLV is not stored in DB yet
    " (this is why all the fields of LIKP are empty, except VBELN)
    IF is_dlv_item-likp-lfart IS NOT INITIAL AND
       is_dlv_item-lips-pstyv IS NOT INITIAL.

      MOVE-CORRESPONDING is_dlv_item-likp TO ls_likp.

      TRY.
          zcl_gtt_mia_ctp_tools=>get_delivery_item_planned_evt(
            EXPORTING
              iv_appsys    = mv_appsys
              is_aotype    = is_aotype
              is_likp      = ls_likp
              is_lips      = is_dlv_item-lips
            IMPORTING
              et_exp_event = lt_exp_event ).

          IF lt_exp_event[] IS INITIAL.
            lt_exp_event = VALUE #( (
                milestone         = ''
                locid2            = ''
                loctype           = ''
                locid1            = ''
                evt_exp_datetime  = '000000000000000'
                evt_exp_tzone     = ''
            ) ).
          ENDIF.

          lv_appobjid = |{ is_dlv_item-lips-vbeln ALPHA = OUT }{ is_dlv_item-lips-posnr ALPHA = IN }|.
          CONDENSE lv_appobjid NO-GAPS.
          LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
            <ls_exp_event>-appsys         = mv_appsys.
            <ls_exp_event>-appobjtype     = is_aotype-aot_type.
            <ls_exp_event>-appobjid       = lv_appobjid.
            <ls_exp_event>-language       = sy-langu.
            IF <ls_exp_event>-evt_exp_tzone IS INITIAL.
              <ls_exp_event>-evt_exp_tzone  = zcl_gtt_tools=>get_system_time_zone(  ).
            ENDIF.
          ENDLOOP.

          cs_idoc_data-exp_event = VALUE #( BASE cs_idoc_data-exp_event
                                            ( LINES OF lt_exp_event ) ).

        CATCH cx_udm_message INTO DATA(lo_udm_message).
          RAISE EXCEPTION TYPE cx_udm_message
            EXPORTING
              textid   = lo_udm_message->textid
              previous = lo_udm_message->previous
              m_msgid  = lo_udm_message->m_msgid
              m_msgty  = lo_udm_message->m_msgty
              m_msgno  = lo_udm_message->m_msgno
              m_msgv1  = lo_udm_message->m_msgv1
              m_msgv2  = lo_udm_message->m_msgv2
              m_msgv3  = lo_udm_message->m_msgv3
              m_msgv4  = lo_udm_message->m_msgv4.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD fill_idoc_tracking_id.

    DATA:
      lv_appobjid    TYPE /saptrx/aoid,
      lv_dlvittrxcod TYPE /saptrx/trxcod,
      lv_futrxcod    TYPE /saptrx/trxcod,
      lt_tracking_id TYPE /saptrx/bapi_trk_trkid_tab,
      lt_fu_id       TYPE zif_gtt_mia_ctp_types=>tt_fu_id,
      ls_fu_create   TYPE zif_gtt_mia_ctp_types=>ts_fu_list,
      ls_fu_update   TYPE zif_gtt_mia_ctp_types=>ts_fu_list,
      ls_fu_delete   TYPE zif_gtt_mia_ctp_types=>ts_fu_list.

    lv_dlvittrxcod = zif_gtt_ef_constants=>cs_trxcod-dl_position.
    lv_futrxcod    = zif_gtt_ef_constants=>cs_trxcod-fu_number.

    " Delivery Item
    lv_appobjid = |{ is_delivery_item-lips-vbeln ALPHA = OUT }{ is_delivery_item-lips-posnr ALPHA = IN }|.
    CONDENSE lv_appobjid NO-GAPS.
    cs_idoc_data-tracking_id  = VALUE #( BASE cs_idoc_data-tracking_id (
      appsys      = mv_appsys
      appobjtype  = is_aotype-aot_type
      appobjid    = lv_appobjid
      trxcod      = lv_dlvittrxcod
      trxid       = lv_appobjid

    ) ).

  ENDMETHOD.


  METHOD get_aotype_restriction_id.

    rv_rst_id   = 'FU_TO_IDLI'.

  ENDMETHOD.


  METHOD prepare_idoc_data.

    DATA: ls_idoc_data    TYPE zif_gtt_ctp_types=>ts_idoc_data.

    LOOP AT mt_aotype ASSIGNING FIELD-SYMBOL(<ls_aotype>).
      CLEAR: ls_idoc_data.

      ls_idoc_data-appsys   = mv_appsys.

      fill_idoc_trxserv(
        EXPORTING
          is_aotype    = <ls_aotype>
        CHANGING
          cs_idoc_data = ls_idoc_data ).

      LOOP AT mt_delivery_item ASSIGNING FIELD-SYMBOL(<ls_dlv_item>).

        " Skip for new created delivery item.
        IF <ls_dlv_item>-lips-pstyv IS INITIAL.
          CONTINUE.
        ENDIF.
        fill_idoc_appobj_ctabs(
          EXPORTING
            is_aotype        = <ls_aotype>
            is_delivery_item = <ls_dlv_item>
          CHANGING
            cs_idoc_data     = ls_idoc_data ).

        fill_idoc_control_data(
          EXPORTING
            is_aotype        = <ls_aotype>
            is_delivery_item = <ls_dlv_item>
          CHANGING
            cs_idoc_data     = ls_idoc_data ).

        fill_idoc_exp_event(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_dlv_item  = <ls_dlv_item>
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_tracking_id(
          EXPORTING
            is_aotype        = <ls_aotype>
            is_delivery_item = <ls_dlv_item>
          CHANGING
            cs_idoc_data     = ls_idoc_data ).
      ENDLOOP.

      IF ls_idoc_data-appobj_ctabs[] IS NOT INITIAL AND
         ls_idoc_data-control[] IS NOT INITIAL AND
         ls_idoc_data-tracking_id[] IS NOT INITIAL.
        APPEND ls_idoc_data TO mt_idoc_data.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_gtt_ctp_tor_to_dl~extract_data.
    fill_delivery_item_data( ).
  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_CTP_TOR_TO_ODLVHD definition
  public
  inheriting from ZCL_GTT_CTP_TOR_TO_DL_BASE
  create public .

public section.
protected section.

  methods GET_AOTYPE_RESTRICTION_ID
    redefinition .
private section.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_CTP_TOR_TO_ODLVHD IMPLEMENTATION.


  METHOD get_aotype_restriction_id.

    rv_rst_id   = 'FU_TO_ODLH'.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_CTP_TOR_TO_ODLVIT definition
  public
  inheriting from ZCL_GTT_CTP_TOR_TO_DL_BASE
  create public .

public section.

  methods ZIF_GTT_CTP_TOR_TO_DL~EXTRACT_DATA
    redefinition .
protected section.

  methods FILL_IDOC_APPOBJ_CTABS
    redefinition .
  methods FILL_IDOC_CONTROL_DATA
    redefinition .
  methods FILL_IDOC_EXP_EVENT
    redefinition .
  methods GET_AOTYPE_RESTRICTION_ID
    redefinition .
  methods PREPARE_IDOC_DATA
    redefinition .
  methods FILL_IDOC_TRACKING_ID
    redefinition .
private section.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_CTP_TOR_TO_ODLVIT IMPLEMENTATION.


  METHOD fill_idoc_appobj_ctabs.

    DATA:
      lv_appobjid TYPE /saptrx/aoid.

    lv_appobjid = |{ is_delivery_item-lips-vbeln ALPHA = OUT }{ is_delivery_item-lips-posnr ALPHA = IN }|.
    CONDENSE lv_appobjid NO-GAPS.

    cs_idoc_data-appobj_ctabs = VALUE #( BASE cs_idoc_data-appobj_ctabs (
      trxservername = cs_idoc_data-trxserv-trx_server_id
      appobjtype    = is_aotype-aot_type
      appobjid      = lv_appobjid
    ) ).

  ENDMETHOD.


  METHOD fill_idoc_control_data.

    DATA:
      lt_control   TYPE /saptrx/bapi_trk_control_tab,
      lv_count     TYPE i VALUE 0,
      lv_appobjid  TYPE /saptrx/aoid,
      lt_tmp_fu    TYPE zif_gtt_sof_ctp_types=>tt_fu_list,
      ls_fu_create TYPE zif_gtt_sof_ctp_types=>ts_fu_list,
      ls_fu_update TYPE zif_gtt_sof_ctp_types=>ts_fu_list,
      ls_fu_delete TYPE zif_gtt_sof_ctp_types=>ts_fu_list,
      lt_fu_id     TYPE zif_gtt_sof_ctp_types=>tt_fu_id.

    " DL Item key data (obligatory)
    lt_control  = VALUE #(
      (
        paramname = cs_mapping-vbeln
        value     = |{ is_delivery_item-likp-vbeln ALPHA = OUT }|
      )
      (
        paramname = cs_mapping-posnr
        value     = is_delivery_item-lips-posnr
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-actual_bisiness_timezone
        value     = zcl_gtt_sof_tm_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-actual_bisiness_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-actual_technical_timezone
        value     = zcl_gtt_sof_tm_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-actual_technical_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-reported_by
        value     = sy-uname
      )
    ).

    " fill F.U. table
    LOOP AT is_delivery_item-fu_list ASSIGNING FIELD-SYMBOL(<ls_fu_list>).
      IF <ls_fu_list>-change_mode = /bobf/if_frw_c=>sc_modify_delete.
        CONTINUE.
      ENDIF.
      APPEND <ls_fu_list> TO lt_tmp_fu.

      ADD 1 TO lv_count.

      lt_control  = VALUE #( BASE lt_control
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_lineno
          value       = zcl_gtt_sof_tm_tools=>get_pretty_value(
                          iv_value = lv_count )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_freightunit
          value       = zcl_gtt_sof_tm_tools=>get_formated_tor_id(
                          ir_data = REF #( <ls_fu_list> ) )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_itemnumber
          value       = zcl_gtt_sof_tm_tools=>get_formated_tor_item(
                          ir_data = REF #( <ls_fu_list> ) )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_quantity
          value       = zcl_gtt_sof_tm_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-quantity )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_quantityuom
          value       = zcl_gtt_sof_tm_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-quantityuom )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_product_id
          value       = zcl_gtt_sof_tm_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-product_id )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_product_descr
          value       = zcl_gtt_sof_tm_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-product_descr )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_base_uom_val
          value       = zcl_gtt_sof_tm_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-base_uom_val )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_base_uom_uni
          value       = zcl_gtt_sof_tm_tools=>get_pretty_value(
                          iv_value = <ls_fu_list>-base_uom_uni )
        )
        (
          paramindex  = lv_count
          paramname   = cs_mapping-fu_freightunit_logsys
          value       = mv_appsys
        )
      ).
    ENDLOOP.

    " add deletion sign in case of emtpy is_delivery_item-FU_LIST table
    IF lt_tmp_fu IS INITIAL.
      lt_control  = VALUE #( BASE lt_control (
          paramindex = 1
          paramname  = cs_mapping-fu_lineno
          value      = ''
      ) ).
    ENDIF.

*   Add tracking ID information
    CLEAR lv_count.
    lt_fu_id =  CORRESPONDING #( is_delivery_item-fu_list ).
    SORT lt_fu_id BY tor_id.
    DELETE ADJACENT DUPLICATES FROM lt_fu_id COMPARING tor_id.

    LOOP AT lt_fu_id  ASSIGNING FIELD-SYMBOL(<ls_fu_id>).
      ADD 1 TO lv_count.
      CLEAR:
        ls_fu_create,
        ls_fu_update,
        ls_fu_delete.

      READ TABLE is_delivery_item-fu_list INTO ls_fu_create WITH KEY tor_id = <ls_fu_id>-tor_id change_mode = /bobf/if_frw_c=>sc_modify_create.
      READ TABLE is_delivery_item-fu_list INTO ls_fu_update WITH KEY tor_id = <ls_fu_id>-tor_id change_mode = /bobf/if_frw_c=>sc_modify_update.
      READ TABLE is_delivery_item-fu_list INTO ls_fu_delete WITH KEY tor_id = <ls_fu_id>-tor_id change_mode = /bobf/if_frw_c=>sc_modify_delete.

      IF ls_fu_create IS NOT INITIAL
        AND ls_fu_update IS INITIAL
        AND ls_fu_delete IS INITIAL.

        lt_control  = VALUE #( BASE lt_control
          (
            paramindex  = lv_count
            paramname   = cs_mapping-appsys
            value       = mv_appsys
          )
          (
            paramindex  = lv_count
            paramname   = cs_mapping-trxcod
            value       = zif_gtt_sof_constants=>cs_trxcod-fu_number
          )
          (
            paramindex  = lv_count
            paramname   = cs_mapping-trxid
            value       = zcl_gtt_sof_tm_tools=>get_formated_tor_id( ir_data = REF #( <ls_fu_id> ) )
          )
        ).
      ENDIF.

      IF ls_fu_delete IS NOT INITIAL
       AND ls_fu_create IS INITIAL
       AND ls_fu_update IS INITIAL.
        lt_control  = VALUE #( BASE lt_control
          (
            paramindex  = lv_count
            paramname   = cs_mapping-appsys
            value       = mv_appsys
          )
          (
            paramindex  = lv_count
            paramname   = cs_mapping-trxcod
            value       = zif_gtt_sof_constants=>cs_trxcod-fu_number
          )
          (
            paramindex  = lv_count
            paramname   = cs_mapping-trxid
            value       = zcl_gtt_sof_tm_tools=>get_formated_tor_id( ir_data = REF #( <ls_fu_id> ) )
          )
          (
            paramindex  = lv_count
            paramname   = cs_mapping-action
            value       = /bobf/if_frw_c=>sc_modify_delete
          )
        ).
      ENDIF.
    ENDLOOP.

    " fill technical data into all control data records
    lv_appobjid = |{ is_delivery_item-lips-vbeln ALPHA = OUT }{ is_delivery_item-lips-posnr ALPHA = IN }|.
    CONDENSE lv_appobjid NO-GAPS.
    LOOP AT lt_control ASSIGNING FIELD-SYMBOL(<ls_control>).
      <ls_control>-appsys     = mv_appsys.
      <ls_control>-appobjtype = is_aotype-aot_type.
      <ls_control>-appobjid   = lv_appobjid .
    ENDLOOP.

    cs_idoc_data-control  = VALUE #( BASE cs_idoc_data-control
                                     ( LINES OF lt_control ) ).

  ENDMETHOD.


  METHOD fill_idoc_exp_event.

    DATA:
      lt_exp_event TYPE /saptrx/bapi_trk_ee_tab,
      lv_appobjid  TYPE /saptrx/aoid.

    " do not retrieve planned events when DLV is not stored in DB yet
    " (this is why all the fields of LIKP are empty, except VBELN)
    IF is_dlv_item-likp-lfart IS NOT INITIAL AND
       is_dlv_item-lips-pstyv IS NOT INITIAL.

      TRY.
          zcl_gtt_sof_tm_tools=>get_delivery_item_planned_evt(
            EXPORTING
              iv_appsys    = mv_appsys
              is_aotype    = is_aotype
              is_likp      = is_dlv_item-likp
              is_lips      = is_dlv_item-lips
              is_dlv_item  = is_dlv_item
            IMPORTING
              et_exp_event = lt_exp_event ).

          IF lt_exp_event[] IS INITIAL.
            lt_exp_event = VALUE #( (
                milestone         = ''
                locid2            = ''
                loctype           = ''
                locid1            = ''
                evt_exp_datetime  = '000000000000000'
                evt_exp_tzone     = ''
            ) ).
          ENDIF.

          lv_appobjid = |{ is_dlv_item-lips-vbeln ALPHA = OUT }{ is_dlv_item-lips-posnr ALPHA = IN }|.
          CONDENSE lv_appobjid NO-GAPS.
          LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
            <ls_exp_event>-appsys         = mv_appsys.
            <ls_exp_event>-appobjtype     = is_aotype-aot_type.
            <ls_exp_event>-appobjid       = lv_appobjid.
            <ls_exp_event>-language       = sy-langu.
            IF <ls_exp_event>-evt_exp_tzone IS INITIAL.
              <ls_exp_event>-evt_exp_tzone  = zcl_gtt_sof_tm_tools=>get_system_time_zone(  ).
            ENDIF.
          ENDLOOP.

          cs_idoc_data-exp_event = VALUE #( BASE cs_idoc_data-exp_event
                                            ( LINES OF lt_exp_event ) ).

        CATCH cx_udm_message INTO DATA(lo_udm_message).
          RAISE EXCEPTION TYPE cx_udm_message
            EXPORTING
              textid   = lo_udm_message->textid
              previous = lo_udm_message->previous
              m_msgid  = lo_udm_message->m_msgid
              m_msgty  = lo_udm_message->m_msgty
              m_msgno  = lo_udm_message->m_msgno
              m_msgv1  = lo_udm_message->m_msgv1
              m_msgv2  = lo_udm_message->m_msgv2
              m_msgv3  = lo_udm_message->m_msgv3
              m_msgv4  = lo_udm_message->m_msgv4.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD fill_idoc_tracking_id.

    DATA:
      lv_tmp_dlvittrxcod TYPE /saptrx/trxcod,
      lv_dlvittrxcod     TYPE /saptrx/trxcod,
      lv_appobjid        TYPE /saptrx/aoid,
      lt_fu_id           TYPE zif_gtt_sof_ctp_types=>tt_fu_id,
      lv_futrxcod        TYPE /saptrx/trxcod,
      ls_fu_create       TYPE zif_gtt_sof_ctp_types=>ts_fu_list,
      ls_fu_update       TYPE zif_gtt_sof_ctp_types=>ts_fu_list,
      ls_fu_delete       TYPE zif_gtt_sof_ctp_types=>ts_fu_list.

    lv_dlvittrxcod = zif_gtt_sof_constants=>cs_trxcod-out_delivery_item.
    lv_futrxcod    = zif_gtt_sof_constants=>cs_trxcod-fu_number.

    " Delivery Item
    lv_appobjid = |{ is_delivery_item-lips-vbeln ALPHA = OUT }{ is_delivery_item-lips-posnr ALPHA = IN }|.
    CONDENSE lv_appobjid NO-GAPS.
    cs_idoc_data-tracking_id  = VALUE #( BASE cs_idoc_data-tracking_id (
      appsys      = mv_appsys
      appobjtype  = is_aotype-aot_type
      appobjid    = lv_appobjid
      trxcod      = lv_dlvittrxcod
      trxid       = lv_appobjid
    ) ).

  ENDMETHOD.


  METHOD get_aotype_restriction_id.

    rv_rst_id   = 'FU_TO_ODLI'.

  ENDMETHOD.


  METHOD prepare_idoc_data.

    DATA:
      ls_idoc_data    TYPE zif_gtt_sof_ctp_types=>ts_idoc_data.

    LOOP AT mt_aotype ASSIGNING FIELD-SYMBOL(<ls_aotype>).
      CLEAR: ls_idoc_data.

      ls_idoc_data-appsys   = mv_appsys.

      fill_idoc_trxserv(
        EXPORTING
          is_aotype    = <ls_aotype>
        CHANGING
          cs_idoc_data = ls_idoc_data ).

      LOOP AT mt_delivery_item ASSIGNING FIELD-SYMBOL(<ls_dlv_item>).

        IF <ls_dlv_item>-lips-pstyv IS INITIAL. " Skip the the item that don't stored in DB
          CONTINUE.
        ENDIF.

        fill_idoc_appobj_ctabs(
          EXPORTING
            is_aotype        = <ls_aotype>
            is_delivery_item = <ls_dlv_item>
          CHANGING
            cs_idoc_data     = ls_idoc_data ).

        fill_idoc_control_data(
          EXPORTING
            is_aotype        = <ls_aotype>
            is_delivery_item = <ls_dlv_item>
          CHANGING
            cs_idoc_data     = ls_idoc_data ).

        fill_idoc_exp_event(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_dlv_item  = <ls_dlv_item>
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_tracking_id(
          EXPORTING
            is_aotype        = <ls_aotype>
            is_delivery_item = <ls_dlv_item>
          CHANGING
            cs_idoc_data     = ls_idoc_data ).
      ENDLOOP.

      IF ls_idoc_data-appobj_ctabs[] IS NOT INITIAL AND
         ls_idoc_data-control[] IS NOT INITIAL AND
         ls_idoc_data-tracking_id[] IS NOT INITIAL.
        APPEND ls_idoc_data TO mt_idoc_data.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_gtt_ctp_tor_to_dl~extract_data.

    fill_delivery_item_data( ).

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SOF_CTP_DAT_TOR_TO_DLH definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IT_DELIVERY_CHNG type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG
      !IT_TOR_ROOT type /SCMTMS/T_EM_BO_TOR_ROOT
      !IT_TOR_ITEM type /SCMTMS/T_EM_BO_TOR_ITEM .
  methods GET_DELIVERIES
    returning
      value(RR_DELIVERIES) type ref to DATA .
  PROTECTED SECTION.
private section.

  data MT_DELIVERY type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY .

  methods FILL_DELIVERY_DATA
    importing
      !IT_DELIVERY_CHNG type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG
      !IT_TOR_ROOT type /SCMTMS/T_EM_BO_TOR_ROOT
      !IT_TOR_ITEM type /SCMTMS/T_EM_BO_TOR_ITEM
    changing
      !CT_DELIVERY type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY .
  methods INIT_DELIVERY_HEADERS
    importing
      !IT_DELIVERY_CHNG type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG
      !IT_TOR_ROOT type /SCMTMS/T_EM_BO_TOR_ROOT
      !IT_TOR_ITEM type /SCMTMS/T_EM_BO_TOR_ITEM .
ENDCLASS.



CLASS ZCL_GTT_SOF_CTP_DAT_TOR_TO_DLH IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    IF it_delivery_chng[] IS NOT INITIAL.
      init_delivery_headers(
        EXPORTING
          it_delivery_chng = it_delivery_chng
          it_tor_root      = it_tor_root
          it_tor_item      = it_tor_item ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_delivery_data.

    DATA: lt_likp        TYPE HASHED TABLE OF likp WITH UNIQUE KEY vbeln,
          lt_lips        TYPE SORTED TABLE OF lipsvb WITH UNIQUE KEY vbeln posnr,
          lt_vbfa        TYPE STANDARD TABLE OF vbfavb,
          ls_lips        TYPE lipsvb,
          ls_vbuk        TYPE vbukvb,
          ls_vbup        TYPE vbupvb,
          lv_base_btd_id TYPE /scmtms/base_btd_id.

    IF ct_delivery[] IS NOT INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_likp
        FROM likp
        FOR ALL ENTRIES IN ct_delivery
        WHERE vbeln = ct_delivery-vbeln.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_lips
        FROM lips
        FOR ALL ENTRIES IN ct_delivery
        WHERE vbeln = ct_delivery-vbeln.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
        FROM vbfa
        FOR ALL ENTRIES IN ct_delivery
       WHERE vbeln = ct_delivery-vbeln.

      LOOP AT ct_delivery ASSIGNING FIELD-SYMBOL(<ls_delivery>).

*       Delivery Header Status
        READ TABLE lt_likp INTO DATA(ls_likp) WITH KEY vbeln = <ls_delivery>-vbeln.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING ls_likp TO ls_vbuk.
          APPEND ls_vbuk TO <ls_delivery>-vbuk.
          CLEAR ls_vbuk.
        ENDIF.

        " copy selected delivery header
        <ls_delivery>-likp  = VALUE #( lt_likp[ KEY primary_key
                                                COMPONENTS vbeln = <ls_delivery>-vbeln ]
                                         DEFAULT VALUE #( vbeln = <ls_delivery>-vbeln ) ).

*       Copy document flow data
        LOOP AT lt_vbfa INTO DATA(ls_vbfa) USING KEY primary_key
          WHERE vbeln = <ls_delivery>-vbeln.
          APPEND ls_vbfa TO <ls_delivery>-vbfa.
        ENDLOOP.

        " copy selected delivery items
        LOOP AT lt_lips ASSIGNING FIELD-SYMBOL(<ls_lips>)
          USING KEY primary_key
          WHERE vbeln = <ls_delivery>-vbeln.

          APPEND <ls_lips> TO <ls_delivery>-lips.

*         Delivery Item Status
          MOVE-CORRESPONDING <ls_lips> TO ls_vbup.
          APPEND ls_vbup TO <ls_delivery>-vbup.
          CLEAR:
            ls_vbup.
        ENDLOOP.

        " when data is not stored in DB yet, just take it from TOR tables
        IF sy-subrc <> 0.
          lv_base_btd_id  = |{ <ls_delivery>-vbeln ALPHA = IN }|.

          LOOP AT it_tor_item ASSIGNING FIELD-SYMBOL(<ls_tor_item>)
            WHERE base_btd_tco = zif_gtt_sof_ctp_tor_constants=>cv_base_btd_tco_outb_dlv
              AND base_btd_id  = lv_base_btd_id
              AND base_btditem_id IS NOT INITIAL.

            ASSIGN it_tor_root[ node_id = <ls_tor_item>-parent_node_id ]
              TO FIELD-SYMBOL(<ls_tor_root>).

            IF sy-subrc = 0 AND
               <ls_tor_root>-tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit.

              ls_lips-vbeln   = |{ <ls_tor_item>-base_btd_id ALPHA = IN }|.
              ls_lips-posnr   = |{ <ls_tor_item>-base_btditem_id ALPHA = IN }|.

              " check whether DLV item already exists
              READ TABLE <ls_delivery>-lips
                WITH KEY vbeln = ls_lips-vbeln
                         posnr = ls_lips-posnr
                         TRANSPORTING NO FIELDS.

              " no row -> add it
              IF sy-subrc <> 0.
                APPEND ls_lips to <ls_delivery>-lips.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD GET_DELIVERIES.

    rr_deliveries   = REF #( mt_delivery ).

  ENDMETHOD.


  METHOD init_delivery_headers.

    DATA: ls_dlv_head TYPE zif_gtt_sof_ctp_types=>ts_delivery.

    CLEAR: mt_delivery[].

    " collect DLV Headers from DLV Items
    LOOP AT it_delivery_chng ASSIGNING FIELD-SYMBOL(<ls_delivery_chng>).
      READ TABLE mt_delivery TRANSPORTING NO FIELDS
        WITH KEY vbeln  = <ls_delivery_chng>-vbeln.

      IF sy-subrc <> 0.
        ls_dlv_head-vbeln = <ls_delivery_chng>-vbeln.
        APPEND ls_dlv_head TO mt_delivery.
      ENDIF.
    ENDLOOP.

    " enrich DLV Headers with data from DB and TOR internal tables
    fill_delivery_data(
      EXPORTING
        it_delivery_chng = it_delivery_chng
        it_tor_root      = it_tor_root
        it_tor_item      = it_tor_item
      CHANGING
        ct_delivery      = mt_delivery ).

    LOOP AT mt_delivery ASSIGNING FIELD-SYMBOL(<ls_delivery>).
      TRY.
          <ls_delivery>-fu_relevant = zcl_gtt_sof_tm_tools=>is_fu_relevant(
            it_lips = CORRESPONDING #( <ls_delivery>-lips ) ).
        CATCH cx_udm_message.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""

  data MT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_ITEM .

  methods CONVERT_DELIVERY_ITEMS
    importing
      !IT_DELIVERY_CHNG type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG
    exporting
      !ET_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_ITEM .
  methods FILL_DELIVERY_DATA
    changing
      !CT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_ITEM .
  methods FILL_FREIGHT_UNIT_DATA
    changing
      !CT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_ITEM
    raising
      CX_UDM_MESSAGE .
  methods INIT_DELIVERY_ITEMS
    importing
      !IT_DELIVERY_CHNG type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG
    raising
      CX_UDM_MESSAGE .
ENDCLASS.



CLASS ZCL_GTT_SOF_CTP_DAT_TOR_TO_DLI IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    IF it_delivery_chng[] IS NOT INITIAL.
      init_delivery_items(
        EXPORTING
          it_delivery_chng = it_delivery_chng ).
    ENDIF.

  ENDMETHOD.


  METHOD convert_delivery_items.

    DATA: ls_delivery_item TYPE zif_gtt_sof_ctp_types=>ts_delivery_item.

    CLEAR: et_delivery_item[].

    LOOP AT it_delivery_chng ASSIGNING FIELD-SYMBOL(<ls_delivery_chng>).
      READ TABLE et_delivery_item ASSIGNING FIELD-SYMBOL(<ls_delivery_item>)
        WITH KEY vbeln = <ls_delivery_chng>-vbeln
                 posnr = <ls_delivery_chng>-posnr.

      IF sy-subrc <> 0.
        CLEAR: ls_delivery_item.
        ls_delivery_item-vbeln  = <ls_delivery_chng>-vbeln.
        ls_delivery_item-posnr  = <ls_delivery_chng>-posnr.
        APPEND ls_delivery_item to et_delivery_item.

        ASSIGN et_delivery_item[ vbeln = <ls_delivery_chng>-vbeln posnr = <ls_delivery_chng>-posnr ] TO <ls_delivery_item>.
      ENDIF.

      <ls_delivery_item>-fu_list  = VALUE #( BASE <ls_delivery_item>-fu_list (
        tor_id        = <ls_delivery_chng>-tor_id
        item_id       = <ls_delivery_chng>-item_id
        quantity      = <ls_delivery_chng>-quantity
        quantityuom   = <ls_delivery_chng>-quantityuom
        product_id    = <ls_delivery_chng>-product_id
        product_descr = <ls_delivery_chng>-product_descr
        change_mode   = <ls_delivery_chng>-change_mode
      ) ).

      UNASSIGN <ls_delivery_item>.
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_delivery_data.

    DATA:
      lt_likp        TYPE HASHED TABLE OF likp WITH UNIQUE KEY vbeln,
      lt_lips        TYPE SORTED TABLE OF lipsvb WITH UNIQUE KEY vbeln posnr,
      lt_vbfa        TYPE STANDARD TABLE OF vbfavb,
      ls_vbuk        TYPE vbukvb,
      ls_lips        TYPE lipsvb,
      lv_base_btd_id TYPE /scmtms/base_btd_id.

    IF ct_delivery_item[] IS NOT INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_likp
        FROM likp
        FOR ALL ENTRIES IN ct_delivery_item
        WHERE vbeln = ct_delivery_item-vbeln.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_lips
        FROM lips
        FOR ALL ENTRIES IN ct_delivery_item
        WHERE vbeln = ct_delivery_item-vbeln
          AND posnr = ct_delivery_item-posnr.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
        FROM vbfa
        FOR ALL ENTRIES IN ct_delivery_item
       WHERE vbeln = ct_delivery_item-vbeln.

      LOOP AT ct_delivery_item ASSIGNING FIELD-SYMBOL(<ls_delivery_item>).
        " copy selected delivery header
        <ls_delivery_item>-likp  = VALUE #(
          lt_likp[ KEY primary_key
                   COMPONENTS vbeln = <ls_delivery_item>-vbeln ]
          DEFAULT VALUE #( vbeln = <ls_delivery_item>-vbeln )
        ).

        " copy selected delivery item
        <ls_delivery_item>-lips  = VALUE #(
          lt_lips[ KEY primary_key
                   COMPONENTS vbeln = <ls_delivery_item>-vbeln
                              posnr = <ls_delivery_item>-posnr ]
          DEFAULT VALUE #( vbeln = <ls_delivery_item>-vbeln
                           posnr = <ls_delivery_item>-posnr )
        ).

*       Delivery Header Status
        READ TABLE lt_likp INTO DATA(ls_likp) WITH KEY vbeln = <ls_delivery_item>-vbeln.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING ls_likp TO <ls_delivery_item>-vbuk.
        ENDIF.

*       Delivery Item Status
        LOOP AT lt_lips ASSIGNING FIELD-SYMBOL(<ls_lips>)
          USING KEY primary_key
          WHERE vbeln = <ls_delivery_item>-vbeln
            AND posnr = <ls_delivery_item>-posnr.

          MOVE-CORRESPONDING <ls_lips> TO <ls_delivery_item>-vbup.
        ENDLOOP.

*       Copy document flow data
        LOOP AT lt_vbfa INTO DATA(ls_vbfa) USING KEY primary_key
          WHERE vbeln = <ls_delivery_item>-vbeln
            AND posnn = <ls_delivery_item>-posnr.
          APPEND ls_vbfa TO <ls_delivery_item>-vbfa.
        ENDLOOP.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD fill_freight_unit_data.

    DATA: lt_fu_item  TYPE /scmtms/t_tor_item_tr_k.

    LOOP AT ct_delivery_item ASSIGNING FIELD-SYMBOL(<ls_delivery_item>).
      zcl_gtt_sof_tm_tools=>get_tor_items_for_dlv_items(
        EXPORTING
          it_lips    = VALUE #( ( vbeln = <ls_delivery_item>-vbeln
                                  posnr = <ls_delivery_item>-posnr ) )
          iv_tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit
        IMPORTING
          et_fu_item = lt_fu_item ).

      LOOP AT lt_fu_item ASSIGNING FIELD-SYMBOL(<ls_fu_item>).
        zcl_gtt_sof_tm_tools=>get_tor_root(
          EXPORTING
            iv_key = <ls_fu_item>-parent_key
          IMPORTING
            es_tor = DATA(ls_tor_data) ).

        IF ls_tor_data IS INITIAL OR ls_tor_data-lifecycle = /scmtms/if_tor_status_c=>sc_root-lifecycle-v_canceled.
          CONTINUE.
        ENDIF.

        <ls_delivery_item>-fu_list  = VALUE #( BASE <ls_delivery_item>-fu_list (
          tor_id       = ls_tor_data-tor_id
          item_id      = <ls_fu_item>-item_id
          quantity     = <ls_fu_item>-qua_pcs_val
          quantityuom  = <ls_fu_item>-qua_pcs_uni
          product_id   = <ls_fu_item>-product_id
          product_descr = <ls_fu_item>-item_descr
        ) ).
      ENDLOOP.

      SORT <ls_delivery_item>-fu_list BY tor_id item_id.

      DELETE ADJACENT DUPLICATES FROM <ls_delivery_item>-fu_list
        COMPARING tor_id item_id.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_DELIVERY_ITEMS.

    rr_delivery_item       = REF #( mt_delivery_item ).

  ENDMETHOD.


  METHOD INIT_DELIVERY_ITEMS.

    " enrich it_delivery_chng with fu_quantity fu_quantityUoM
    convert_delivery_items(
      EXPORTING
        it_delivery_chng = it_delivery_chng
      IMPORTING
        et_delivery_item = mt_delivery_item ).

    fill_delivery_data(
      CHANGING
        ct_delivery_item = mt_delivery_item ).

    fill_freight_unit_data(
      CHANGING
        ct_delivery_item = mt_delivery_item ).

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SOF_CTP_SND definition
  public
  abstract
  create public .

public section.

  methods SEND_IDOC_DATA
    exporting
      !ET_BAPIRET type BAPIRET2_T
    raising
      CX_UDM_MESSAGE .
protected section.

  data MV_APPSYS type LOGSYS .
  data MT_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TT_AOTYPE .
  data MT_IDOC_DATA type ZIF_GTT_SOF_CTP_TYPES=>TT_IDOC_DATA .

  methods GET_AOTYPE_RESTRICTION_ID
  abstract
    returning
      value(RV_RST_ID) type CHAR10 .
  methods GET_AOTYPE_RESTRICTIONS
    exporting
      !ET_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TT_AOTYPE_RST .
  methods GET_OBJECT_TYPE
  abstract
    returning
      value(RV_OBJTYPE) type /SAPTRX/TRK_OBJ_TYPE .
  methods FILL_IDOC_TRXSERV
    importing
      !IS_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TS_AOTYPE
    changing
      !CS_IDOC_DATA type ZIF_GTT_SOF_CTP_TYPES=>TS_IDOC_DATA .
  methods INITIATE
    raising
      CX_UDM_MESSAGE .
  methods INITIATE_AOTYPES
    raising
      CX_UDM_MESSAGE .
  class-methods IS_EXTRACTOR_EXIST
    importing
      !IV_TRK_OBJ_TYPE type CLIKE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  class-methods IS_GTT_ENABLED
    importing
      !IT_TRK_OBJ_TYPE type ZIF_GTT_SOF_CTP_TYPES=>TT_TRK_OBJ_TYPE
    returning
      value(RV_RESULT) type ABAP_BOOL .
  PRIVATE SECTION.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SOF_CTP_SND IMPLEMENTATION.


  METHOD fill_idoc_trxserv.

    SELECT SINGLE *
      INTO cs_idoc_data-trxserv
      FROM /saptrx/trxserv
      WHERE trx_server_id = is_aotype-server_name.

  ENDMETHOD.


  METHOD get_aotype_restrictions.

    DATA(rst_id)  = get_aotype_restriction_id( ).

    SELECT rst_option AS option,
           rst_sign   AS sign,
           rst_low    AS low,
           rst_high   AS high
      INTO CORRESPONDING FIELDS OF TABLE @et_aotype
      FROM zgtt_aotype_rst
      WHERE rst_id = @rst_id.

    IF sy-subrc <> 0.
      CLEAR: et_aotype[].
    ENDIF.

  ENDMETHOD.


  METHOD initiate.

    " Get current logical system
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = mv_appsys
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.

    IF sy-subrc <> 0.
      MESSAGE e007(zgtt_ssof) INTO DATA(lv_dummy).
      zcl_gtt_sof_tm_tools=>throw_exception( ).
    ENDIF.

    initiate_aotypes( ).

  ENDMETHOD.


  METHOD initiate_aotypes.

    DATA: lt_aotype_rst TYPE zif_gtt_sof_ctp_types=>tt_aotype_rst.

    DATA(lv_objtype)  = get_object_type(  ).

    get_aotype_restrictions(
      IMPORTING
        et_aotype = lt_aotype_rst ).

    " Prepare AOT list
    IF lt_aotype_rst IS NOT INITIAL.
      SELECT trk_obj_type  AS obj_type
             aotype        AS aot_type
             trxservername AS server_name
        INTO TABLE mt_aotype
        FROM /saptrx/aotypes
        WHERE trk_obj_type  = lv_objtype
          AND aotype       IN lt_aotype_rst
          AND torelevant    = abap_true.

      IF sy-subrc <> 0.
        MESSAGE e008(zgtt_ssof) INTO DATA(lv_dummy).
        zcl_gtt_sof_tm_tools=>throw_exception( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_extractor_exist.

    DATA: lv_trk_obj_type  TYPE /saptrx/aotypes-trk_obj_type.

    SELECT SINGLE trk_obj_type
      INTO lv_trk_obj_type
      FROM /saptrx/aotypes
      WHERE trk_obj_type = iv_trk_obj_type
        AND torelevant   = abap_true.

    rv_result   = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD is_gtt_enabled.

    DATA: lv_extflag      TYPE flag.

    rv_result   = abap_false.

    " Check package dependent BADI disabling
    CALL FUNCTION 'GET_R3_EXTENSION_SWITCH'
      EXPORTING
        i_structure_package = zif_gtt_sof_ctp_tor_constants=>cv_structure_pkg
      IMPORTING
        e_active            = lv_extflag
      EXCEPTIONS
        not_existing        = 1
        object_not_existing = 2
        no_extension_object = 3
        OTHERS              = 4.

    IF sy-subrc = 0 AND lv_extflag = abap_true.
*     Check if any tracking server defined
      CALL FUNCTION '/SAPTRX/EVENT_MGR_CHECK'
        EXCEPTIONS
          no_event_mgr_available = 1
          OTHERS                 = 2.

      "Check whether at least 1 active extractor exists for every object
      IF sy-subrc = 0.
        rv_result = boolc( it_trk_obj_type[] IS NOT INITIAL ).

        LOOP AT it_trk_obj_type ASSIGNING FIELD-SYMBOL(<lv_trk_obj_type>).
          IF is_extractor_exist( iv_trk_obj_type = <lv_trk_obj_type> ) = abap_false.
            rv_result   = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD send_idoc_data.

    DATA: lt_bapiret1 TYPE bapiret2_t,
          lt_bapiret2 TYPE bapiret2_t.

    LOOP AT mt_idoc_data ASSIGNING FIELD-SYMBOL(<ls_idoc_data>).
      CLEAR: lt_bapiret1[], lt_bapiret2[].

      /saptrx/cl_send_idocs=>send_idoc_ehpost01(
        EXPORTING
          it_control      = <ls_idoc_data>-control
          it_info         = <ls_idoc_data>-info
          it_tracking_id  = <ls_idoc_data>-tracking_id
          it_exp_event    = <ls_idoc_data>-exp_event
          is_trxserv      = <ls_idoc_data>-trxserv
          iv_appsys       = <ls_idoc_data>-appsys
          it_appobj_ctabs = <ls_idoc_data>-appobj_ctabs
          iv_upd_task     = 'X'
        IMPORTING
          et_bapireturn   = lt_bapiret1 ).

      " when GTT.2 version
      IF /saptrx/cl_send_idocs=>st_idoc_data[] IS NOT INITIAL.
        /saptrx/cl_send_idocs=>send_idoc_gttmsg01(
          IMPORTING
            et_bapireturn = lt_bapiret2 ).
      ENDIF.

      " collect messages, if it is necessary
      IF et_bapiret IS REQUESTED.
        et_bapiret    = VALUE #( BASE et_bapiret
                                 ( LINES OF lt_bapiret1 )
                                 ( LINES OF lt_bapiret2 ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SOF_CTP_SND_TOR_TO_DLH definition
  public
  inheriting from ZCL_GTT_SOF_CTP_SND
  create private .

public section.

  class-methods GET_INSTANCE
    returning
      value(RO_SENDER) type ref to ZCL_GTT_SOF_CTP_SND_TOR_TO_DLH
    raising
      CX_UDM_MESSAGE .
  methods PREPARE_IDOC_DATA
    importing
      !IO_DL_HEAD_DATA type ref to ZCL_GTT_SOF_CTP_DAT_TOR_TO_DLH
    raising
      CX_UDM_MESSAGE .
  PROTECTED SECTION.

    METHODS get_aotype_restriction_id
        REDEFINITION .
    METHODS get_object_type
        REDEFINITION .
private section.

  constants:
    BEGIN OF cs_mapping,
        vbeln        TYPE /saptrx/paramname VALUE 'YN_DLV_NO',
        fu_relevant  TYPE /saptrx/paramname VALUE 'YN_DL_FU_RELEVANT',
        pod_relevant TYPE /saptrx/paramname VALUE 'YN_DL_POD_RELEVANT',
      END OF cs_mapping .

  methods FILL_IDOC_APPOBJ_CTABS
    importing
      !IS_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TS_AOTYPE
      !IS_LIKP type ZIF_GTT_SOF_CTP_TYPES=>TS_DELIVERY
    changing
      !CS_IDOC_DATA type ZIF_GTT_SOF_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_CONTROL_DATA
    importing
      !IS_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TS_AOTYPE
      !IS_DELIVERY type ZIF_GTT_SOF_CTP_TYPES=>TS_DELIVERY
    changing
      !CS_IDOC_DATA type ZIF_GTT_SOF_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_EXP_EVENT
    importing
      !IS_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TS_AOTYPE
      !IS_DELIVERY type ZIF_GTT_SOF_CTP_TYPES=>TS_DELIVERY
    changing
      !CS_IDOC_DATA type ZIF_GTT_SOF_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
  methods FILL_IDOC_TRACKING_ID
    importing
      !IS_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TS_AOTYPE
      !IS_DELIVERY type ZIF_GTT_SOF_CTP_TYPES=>TS_DELIVERY
    changing
      !CS_IDOC_DATA type ZIF_GTT_SOF_CTP_TYPES=>TS_IDOC_DATA
    raising
      CX_UDM_MESSAGE .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SOF_CTP_SND_TOR_TO_DLH IMPLEMENTATION.


  METHOD fill_idoc_appobj_ctabs.

    cs_idoc_data-appobj_ctabs = VALUE #( BASE cs_idoc_data-appobj_ctabs (
      trxservername = cs_idoc_data-trxserv-trx_server_id
      appobjtype    = is_aotype-aot_type
      appobjid      = |{ is_likp-vbeln ALPHA = OUT }|
    ) ).

  ENDMETHOD.


  METHOD fill_idoc_control_data.

    DATA: lt_control TYPE /saptrx/bapi_trk_control_tab,
          lv_count   TYPE i VALUE 0.

    " PO Item key data (obligatory)
    lt_control  = VALUE #(
      (
        paramname = cs_mapping-vbeln
        value     = |{ is_delivery-vbeln ALPHA = OUT }|
      )
      (
        paramname = cs_mapping-fu_relevant
        value     = is_delivery-fu_relevant
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-actual_bisiness_timezone
        value     = zcl_gtt_sof_tm_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-actual_bisiness_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-actual_technical_timezone
        value     = zcl_gtt_sof_tm_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-actual_technical_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_sof_ctp_tor_constants=>cs_system_fields-reported_by
        value     = sy-uname
      )
    ).

    " fill technical data into all control data records
    LOOP AT lt_control ASSIGNING FIELD-SYMBOL(<ls_control>).
      <ls_control>-appsys     = mv_appsys.
      <ls_control>-appobjtype = is_aotype-aot_type.
      <ls_control>-appobjid   = |{ is_delivery-vbeln ALPHA = OUT }|.
    ENDLOOP.

    cs_idoc_data-control  = VALUE #( BASE cs_idoc_data-control
                                     ( LINES OF lt_control ) ).

  ENDMETHOD.


  METHOD fill_idoc_exp_event.

    DATA: lt_exp_event TYPE /saptrx/bapi_trk_ee_tab.

    " do not retrieve planned events when DLV is not stored in DB yet
    " (this is why all the fields of LIKP are empty, except VBELN)
    IF is_delivery-likp-lfart IS NOT INITIAL AND
       is_delivery-lips[] IS NOT INITIAL AND
       is_delivery-vbuk[] IS NOT INITIAL AND
       is_delivery-vbup[] IS NOT INITIAL AND
       is_delivery-vbfa[] IS NOT INITIAL.

      TRY.
          zcl_gtt_sof_tm_tools=>get_delivery_head_planned_evt(
            EXPORTING
              iv_appsys    = mv_appsys
              is_aotype    = is_aotype
              is_likp      = CORRESPONDING #( is_delivery-likp )
              it_lips      = CORRESPONDING #( is_delivery-lips )
              it_vbuk      = CORRESPONDING #( is_delivery-vbuk )
              it_vbup      = CORRESPONDING #( is_delivery-vbup )
              it_vbfa      = CORRESPONDING #( is_delivery-vbfa )
            IMPORTING
              et_exp_event = lt_exp_event ).

          IF lt_exp_event[] IS INITIAL.
            lt_exp_event = VALUE #( (
                milestone         = ''
                locid2            = ''
                loctype           = ''
                locid1            = ''
                evt_exp_datetime  = '000000000000000'
                evt_exp_tzone     = ''
            ) ).
          ENDIF.

          LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
            <ls_exp_event>-appsys         = mv_appsys.
            <ls_exp_event>-appobjtype     = is_aotype-aot_type.
            <ls_exp_event>-appobjid       = |{ is_delivery-vbeln ALPHA = OUT }|.
            <ls_exp_event>-language       = sy-langu.
            IF <ls_exp_event>-evt_exp_tzone IS INITIAL.
              <ls_exp_event>-evt_exp_tzone  = zcl_gtt_sof_tm_tools=>get_system_time_zone( ).
            ENDIF.
          ENDLOOP.

          cs_idoc_data-exp_event = VALUE #( BASE cs_idoc_data-exp_event
                                            ( LINES OF lt_exp_event ) ).

        CATCH cx_udm_message INTO DATA(lo_udm_message).
          " do not throw exception when DLV is not stored in DB yet
          " (this is why all the fields of LIKP are empty, except VBELN)
          IF is_delivery-likp-lfart IS NOT INITIAL.
            RAISE EXCEPTION TYPE cx_udm_message
              EXPORTING
                textid   = lo_udm_message->textid
                previous = lo_udm_message->previous
                m_msgid  = lo_udm_message->m_msgid
                m_msgty  = lo_udm_message->m_msgty
                m_msgno  = lo_udm_message->m_msgno
                m_msgv1  = lo_udm_message->m_msgv1
                m_msgv2  = lo_udm_message->m_msgv2
                m_msgv3  = lo_udm_message->m_msgv3
                m_msgv4  = lo_udm_message->m_msgv4.
          ENDIF.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD fill_idoc_tracking_id.

    DATA:
      lv_tmp_dlvhdtrxcod TYPE /saptrx/trxcod,
      lv_dlvhdtrxcod     TYPE /saptrx/trxcod.

    lv_dlvhdtrxcod = zif_gtt_sof_constants=>cs_trxcod-out_delivery.

    " Delivery Header
    cs_idoc_data-tracking_id  = VALUE #( BASE cs_idoc_data-tracking_id (
      appsys      = mv_appsys
      appobjtype  = is_aotype-aot_type
      appobjid    = |{ is_delivery-vbeln ALPHA = OUT }|
      trxcod      = lv_dlvhdtrxcod
      trxid       = |{ is_delivery-vbeln ALPHA = OUT }|
      timzon      = zcl_gtt_sof_tm_tools=>get_system_time_zone( )
    ) ).

  ENDMETHOD.


  METHOD get_aotype_restriction_id.

    rv_rst_id   = 'FU_TO_ODLH'.

  ENDMETHOD.


  METHOD get_instance.

    DATA(lt_trk_obj_type) = VALUE zif_gtt_sof_ctp_types=>tt_trk_obj_type(
       ( zif_gtt_sof_ctp_tor_constants=>cs_trk_obj_type-tms_tor )
       ( zif_gtt_sof_ctp_tor_constants=>cs_trk_obj_type-esc_deliv )
    ).

    IF is_gtt_enabled( it_trk_obj_type = lt_trk_obj_type ) = abap_true.
      ro_sender  = NEW #( ).

      ro_sender->initiate( ).
    ELSE.
      MESSAGE e006(zgtt_ssof) INTO DATA(lv_dummy).
      zcl_gtt_sof_tm_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_object_type.

    rv_objtype  = zif_gtt_sof_ctp_tor_constants=>cs_trk_obj_type-esc_deliv.

  ENDMETHOD.


  METHOD prepare_idoc_data.

    DATA: ls_idoc_data    TYPE zif_gtt_sof_ctp_types=>ts_idoc_data.

    DATA(lr_dlv)   = io_dl_head_data->get_deliveries( ).

    FIELD-SYMBOLS: <lt_dlv>  TYPE zif_gtt_sof_ctp_types=>tt_delivery.

    ASSIGN lr_dlv->*  TO <lt_dlv>.

    LOOP AT mt_aotype ASSIGNING FIELD-SYMBOL(<ls_aotype>).
      CLEAR: ls_idoc_data.

      ls_idoc_data-appsys   = mv_appsys.

      fill_idoc_trxserv(
        EXPORTING
          is_aotype    = <ls_aotype>
        CHANGING
          cs_idoc_data = ls_idoc_data ).

      LOOP AT <lt_dlv> ASSIGNING FIELD-SYMBOL(<ls_dlv>).

        IF <ls_dlv>-likp-lfart IS INITIAL. " Skip cross TP if DLV not stored in DB
          CONTINUE.
        ENDIF.

        fill_idoc_appobj_ctabs(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_likp      = <ls_dlv>
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_control_data(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_delivery  = <ls_dlv>
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_exp_event(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_delivery  = <ls_dlv>
          CHANGING
            cs_idoc_data = ls_idoc_data ).

        fill_idoc_tracking_id(
          EXPORTING
            is_aotype    = <ls_aotype>
            is_delivery  = <ls_dlv>
          CHANGING
            cs_idoc_data = ls_idoc_data ).
      ENDLOOP.

      IF ls_idoc_data-appobj_ctabs[] IS NOT INITIAL AND
         ls_idoc_data-control[] IS NOT INITIAL AND
         ls_idoc_data-tracking_id[] IS NOT INITIAL.
        APPEND ls_idoc_data TO mt_idoc_data.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SOF_CTP_TOR_CHANGES definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IT_TOR_ROOT_SSTRING type /SCMTMS/T_EM_BO_TOR_ROOT
      !IT_TOR_ROOT_BEFORE_SSTRING type /SCMTMS/T_EM_BO_TOR_ROOT
      !IT_TOR_ITEM_SSTRING type /SCMTMS/T_EM_BO_TOR_ITEM
      !IT_TOR_ITEM_BEFORE_SSTRING type /SCMTMS/T_EM_BO_TOR_ITEM
      !IT_TOR_STOP_SSTRING type /SCMTMS/T_EM_BO_TOR_STOP
      !IT_TOR_STOP_ADDR_SSTRING type /SCMTMS/T_EM_BO_LOC_ADDR .
  methods GET_DELIVERY_ITEMS
    returning
      value(RR_DELIVERIES) type ref to DATA .
  PROTECTED SECTION.
private section.

  data MT_TOR_ROOT type /SCMTMS/T_EM_BO_TOR_ROOT .
  data MT_TOR_ROOT_BEFORE type /SCMTMS/T_EM_BO_TOR_ROOT .
  data MT_TOR_ITEM type /SCMTMS/T_EM_BO_TOR_ITEM .
  data MT_TOR_ITEM_BEFORE type /SCMTMS/T_EM_BO_TOR_ITEM .
  data MT_TOR_STOP type /SCMTMS/T_EM_BO_TOR_STOP .
  data MT_TOR_STOP_BEFORE type /SCMTMS/T_EM_BO_TOR_STOP .
  data MT_TOR_STOP_ADDR type /SCMTMS/T_EM_BO_LOC_ADDR .
  data MT_ALLOWED_FU_TYPE type ZIF_GTT_SOF_CTP_TYPES=>TT_TOR_TYPE .
  data MT_DELIVERIES type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY .
  data MT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG .

  methods ADD_DELIVERY_ITEM
    importing
      !IS_TOR_ROOT type /SCMTMS/S_EM_BO_TOR_ROOT
      !IS_TOR_ITEM type /SCMTMS/S_EM_BO_TOR_ITEM
      !IV_CHANGE_MODE type /BOBF/CONF_CHANGE_MODE
    changing
      !CT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG .
  methods ADD_DELIVERY_ITEMS_BY_TOR_ROOT
    importing
      !IS_TOR_ROOT type /SCMTMS/S_EM_BO_TOR_ROOT
      !IV_CHANGE_MODE type /BOBF/CONF_CHANGE_MODE
    changing
      !CT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG .
  methods CONVERT_DLV_NUMBER
    importing
      !IV_DLV_NUM type CLIKE
    returning
      value(RV_VBELN) type VBELN_VL .
  methods GET_ALLOWED_FU_TYPES
    returning
      value(RT_FU_TYPE) type ZIF_GTT_SOF_CTP_TYPES=>TT_TOR_TYPE .
  methods GET_AOTYPE_RESTRICTIONS
    exporting
      !ET_AOTYPE type ZIF_GTT_SOF_CTP_TYPES=>TT_AOTYPE_RST .
  methods GET_DLV_ITEM_BASED_ON_TOR_ROOT
    changing
      !CT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG .
  methods GET_DLV_ITEM_BASED_ON_TOR_ITEM
    changing
      !CT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG .
  methods GET_DLV_ITEM_BASED_ON_TOR_STOP
    changing
      !CT_DELIVERY_ITEM type ZIF_GTT_SOF_CTP_TYPES=>TT_DELIVERY_CHNG .
  methods GET_TOR_TYPES
    exporting
      !ET_TOR_TYPES type ZIF_GTT_SOF_CTP_TYPES=>TT_TOR_TYPE_RST .
  methods GET_TOR_STOP_BEFORE
    returning
      value(RT_TOR_STOP_BEFORE) type /SCMTMS/T_EM_BO_TOR_STOP .
  methods INIT_DELIVERIES .
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SOF_CTP_TOR_CHANGES IMPLEMENTATION.


  METHOD add_delivery_item.

    DATA:
      ls_delivery_item TYPE zif_gtt_sof_ctp_types=>ts_delivery_chng,
      lv_matnr         TYPE mara-matnr.

    ls_delivery_item-vbeln        = |{ is_tor_item-base_btd_id ALPHA = IN }|.
    ls_delivery_item-posnr        = is_tor_item-base_btditem_id.
    ls_delivery_item-tor_id       = is_tor_root-tor_id.
    ls_delivery_item-item_id      = is_tor_item-item_id.
    ls_delivery_item-quantity     = is_tor_item-qua_pcs_val.
    ls_delivery_item-product_descr = is_tor_item-item_descr.
    ls_delivery_item-base_uom_val  = is_tor_item-base_uom_val.
    ls_delivery_item-change_mode  = iv_change_mode.

    zcl_gtt_sof_toolkit=>convert_unit_output(
      EXPORTING
        iv_input  = is_tor_item-qua_pcs_uni
      RECEIVING
        rv_output = ls_delivery_item-quantityuom ).

    zcl_gtt_sof_toolkit=>convert_unit_output(
      EXPORTING
        iv_input  = is_tor_item-base_uom_uni
      RECEIVING
        rv_output = ls_delivery_item-base_uom_uni ).

    zcl_gtt_tools=>convert_matnr_to_external_frmt(
      EXPORTING
        iv_material = is_tor_item-product_id
      IMPORTING
        ev_result   = lv_matnr ).
    ls_delivery_item-product_id = lv_matnr.
    CLEAR lv_matnr.

    INSERT ls_delivery_item INTO TABLE ct_delivery_item.

  ENDMETHOD.


  METHOD add_delivery_items_by_tor_root.

    DATA: ls_delivery_item TYPE zif_gtt_sof_ctp_types=>ts_delivery_chng.

    LOOP AT mt_tor_item ASSIGNING FIELD-SYMBOL(<ls_tor_item>)
      USING KEY item_parent WHERE parent_node_id = is_tor_root-node_id.

      IF <ls_tor_item>-base_btd_tco = zif_gtt_sof_ctp_tor_constants=>cv_base_btd_tco_outb_dlv AND
         <ls_tor_item>-base_btd_id     IS NOT INITIAL AND
         <ls_tor_item>-base_btditem_id IS NOT INITIAL AND
         <ls_tor_item>-item_cat = /scmtms/if_tor_const=>sc_tor_item_category-product.

        add_delivery_item(
          EXPORTING
            is_tor_root      = is_tor_root
            is_tor_item      = <ls_tor_item>
            iv_change_mode   = iv_change_mode
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    mt_tor_root        = it_tor_root_sstring.
    mt_tor_root_before = it_tor_root_before_sstring.
    mt_tor_item        = it_tor_item_sstring.
    mt_tor_item_before = it_tor_item_before_sstring.
    mt_tor_stop        = it_tor_stop_sstring.
    mt_tor_stop_addr   = it_tor_stop_addr_sstring.
    mt_tor_stop_before = get_tor_stop_before( ).

    init_deliveries( ).

  ENDMETHOD.


  METHOD CONVERT_DLV_NUMBER.

    rv_vbeln    = |{ iv_dlv_num ALPHA = IN }|.

  ENDMETHOD.


  METHOD GET_ALLOWED_FU_TYPES.

  ENDMETHOD.


  METHOD GET_AOTYPE_RESTRICTIONS.

  ENDMETHOD.


  METHOD GET_DELIVERY_ITEMS.

    rr_deliveries   = REF #( mt_delivery_item ).

  ENDMETHOD.


  METHOD GET_DLV_ITEM_BASED_ON_TOR_ITEM.

    DATA ls_delivery_item TYPE zif_gtt_sof_ctp_types=>ts_delivery_chng.

    LOOP AT mt_tor_item ASSIGNING FIELD-SYMBOL(<ls_tor_item>)
      WHERE base_btd_tco = zif_gtt_sof_ctp_tor_constants=>cv_base_btd_tco_outb_dlv AND
            ( change_mode = /bobf/if_frw_c=>sc_modify_delete OR
              change_mode = /bobf/if_frw_c=>sc_modify_create ) AND
              base_btd_id IS NOT INITIAL AND
              base_btditem_id IS NOT INITIAL AND
             item_cat = /scmtms/if_tor_const=>sc_tor_item_category-product.

      ASSIGN mt_tor_root[ node_id = <ls_tor_item>-parent_node_id ] TO FIELD-SYMBOL(<ls_tor_root>).
      IF sy-subrc = 0 AND
         <ls_tor_root>-tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit.

        add_delivery_item(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            is_tor_item      = <ls_tor_item>
            iv_change_mode   = <ls_tor_item>-change_mode
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_DLV_ITEM_BASED_ON_TOR_ROOT.

    LOOP AT mt_tor_root ASSIGNING FIELD-SYMBOL(<ls_tor_root>)
      WHERE tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit
        AND base_btd_tco = zif_gtt_sof_ctp_tor_constants=>cv_base_btd_tco_outb_dlv.

      IF <ls_tor_root>-change_mode = /bobf/if_frw_c=>sc_modify_delete.
        add_delivery_items_by_tor_root(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = /bobf/if_frw_c=>sc_modify_delete
          CHANGING
            ct_delivery_item = ct_delivery_item ).
        CONTINUE.
      ENDIF.

      ASSIGN mt_tor_root_before[ node_id = <ls_tor_root>-node_id ] TO FIELD-SYMBOL(<ls_tor_root_before>).
      IF sy-subrc = 0.
        CHECK <ls_tor_root_before>-base_btd_tco <> zif_gtt_sof_ctp_tor_constants=>cv_base_btd_tco_outb_dlv.

        add_delivery_items_by_tor_root(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = /bobf/if_frw_c=>sc_modify_update
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ELSE.
        add_delivery_items_by_tor_root(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = /bobf/if_frw_c=>sc_modify_create
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD GET_DLV_ITEM_BASED_ON_TOR_STOP.

    DATA lv_tabix TYPE sy-tabix.

    LOOP AT mt_tor_stop ASSIGNING FIELD-SYMBOL(<ls_tor_stop>)
      GROUP BY ( parent_node_id = <ls_tor_stop>-parent_node_id
                 group_size     = GROUP SIZE ) ASSIGNING FIELD-SYMBOL(<lt_group>).

      LOOP AT GROUP <lt_group> ASSIGNING FIELD-SYMBOL(<ls_tor_stop_current>).
        lv_tabix += 1.
        CHECK lv_tabix = <lt_group>-group_size.

        ASSIGN mt_tor_stop_before[ node_id = <ls_tor_stop_current>-node_id ] TO FIELD-SYMBOL(<ls_tor_stop_before>).
        CHECK ( sy-subrc = 0 AND <ls_tor_stop_current>-log_locid <> <ls_tor_stop_before>-log_locid ) OR sy-subrc <> 0.

        " Freight Unit destination was changed => send IDOC
        ASSIGN mt_tor_root[ node_id = <ls_tor_stop_current>-parent_node_id ] TO FIELD-SYMBOL(<ls_tor_root>).
        CHECK sy-subrc = 0 AND <ls_tor_root>-tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit.

        add_delivery_items_by_tor_root(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = /bobf/if_frw_c=>sc_modify_update
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDLOOP.

      CLEAR lv_tabix.
    ENDLOOP.


  ENDMETHOD.


  METHOD GET_TOR_STOP_BEFORE.

    DATA: ls_tor_stop_before TYPE /scmtms/s_em_bo_tor_stop.

    /scmtms/cl_tor_helper_stop=>get_stop_sequence(
      EXPORTING
        it_root_key     = VALUE #( FOR <ls_tor_root> IN mt_tor_root ( key = <ls_tor_root>-node_id ) )
        iv_before_image = abap_true
      IMPORTING
        et_stop_seq_d   = DATA(lt_stop_seq_d) ).

    LOOP AT lt_stop_seq_d ASSIGNING FIELD-SYMBOL(<ls_stop_seq_d>).

      LOOP AT <ls_stop_seq_d>-stop_seq ASSIGNING FIELD-SYMBOL(<ls_stop_seq>).
        DATA(lv_tabix) = sy-tabix.
        MOVE-CORRESPONDING <ls_stop_seq> TO ls_tor_stop_before.

        ls_tor_stop_before-parent_node_id = <ls_stop_seq>-root_key.

        ASSIGN <ls_stop_seq_d>-stop_map[ tabix = lv_tabix ] TO FIELD-SYMBOL(<ls_stop_map>).
        ls_tor_stop_before-node_id = <ls_stop_map>-stop_key.

        INSERT ls_tor_stop_before INTO TABLE rt_tor_stop_before.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD GET_TOR_TYPES.

    et_tor_types = VALUE #(
      FOR <ls_tor_root> IN mt_tor_root
        WHERE ( tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit )
        ( low     = <ls_tor_root>-tor_type
          option  = 'EQ'
          sign    = 'I' )
    ).

  ENDMETHOD.


  METHOD INIT_DELIVERIES.

    CLEAR: mt_delivery_item.

    get_dlv_item_based_on_tor_root(
      CHANGING
        ct_delivery_item = mt_delivery_item ).

    get_dlv_item_based_on_tor_item(
      CHANGING
        ct_delivery_item = mt_delivery_item ).

    get_dlv_item_based_on_tor_stop(
      CHANGING
        ct_delivery_item = mt_delivery_item ).

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SOF_HELPER definition
  public
  final
  create public .

public section.

  types:
    TT_AOTREFD TYPE STANDARD TABLE OF /saptrx/aotrefd .
  types:
    tt_bapi_evm_header     TYPE STANDARD TABLE OF /saptrx/bapi_evm_header .
  types:
    tt_bapi_evm_locationid TYPE STANDARD TABLE OF /saptrx/bapi_evm_locationid .
  types:
    tt_bapi_evm_parameters TYPE STANDARD TABLE OF /saptrx/bapi_evm_parameters .

  constants CV_PARTNER_TYPE type EDI_RCVPRT value 'LS' ##NO_TEXT.
  constants CV_MSG_TYPE_AOPOST type EDI_MESTYP value 'AOPOST' ##NO_TEXT.
  constants CV_IDOC_TYPE_EHPOST01 type EDI_IDOCTP value 'EHPOST01' ##NO_TEXT.
  constants CV_SEG_E1EHPAO type EDILSEGTYP value 'E1EHPAO' ##NO_TEXT.
  constants CV_SEG_E1EHPTID type EDILSEGTYP value 'E1EHPTID' ##NO_TEXT.
  constants CV_SEG_E1EHPCP type EDILSEGTYP value 'E1EHPCP' ##NO_TEXT.
  constants CV_SEG_E1EHPEE type EDILSEGTYP value 'E1EHPEE' ##NO_TEXT.
  constants:
    begin of CV_TT_VERSION,
    GTT10 type /SAPTRX/EM_VERSION value 'GTT1.0',
    GTT20 type /SAPTRX/EM_VERSION value 'GTT2.0',
  end of CV_TT_VERSION .
  constants CV_MSG_TYPE_GTTMSG type EDI_MESTYP value 'GTTMSG' ##NO_TEXT.
  constants CV_IDOC_TYPE_GTTMSG01 type EDI_IDOCTP value 'GTTMSG01' ##NO_TEXT.
  class-data ST_IDOC_DATA type /SAPTRX/T_LOGSYS_IDOC_MAP .

  class-methods SEND_IDOC_EHPOST01
    importing
      !IT_CONTROL type /SAPTRX/BAPI_TRK_CONTROL_TAB
      !IT_TRACKING_ID type /SAPTRX/BAPI_TRK_TRKID_TAB
      !IT_EXP_EVENT type /SAPTRX/BAPI_TRK_EE_TAB optional
      !IS_TRXSERV type /SAPTRX/TRXSERV
      !IV_APPSYS type LOGSYS
      !IT_APPOBJ_CTABS type TRXAS_APPOBJ_CTABS
    exporting
      !ET_BAPIRETURN type BAPIRET2_T .
  class-methods CHECK_AND_CORRECT_TIMESTAMP
    importing
      !IV_TIMESTAMP type TZNTSTMPSL
    returning
      value(RV_TIMESTAMP) type TZNTSTMPSL .
  class-methods SEND_IDOC_GTTMSG01
    exporting
      !ET_BAPIRETURN type BAPIRET2_T .
protected section.
private section.
ENDCLASS.""",
    r"""CLASS ZCL_GTT_SOF_HELPER IMPLEMENTATION.


  METHOD CHECK_AND_CORRECT_TIMESTAMP.
    DATA: lv_date_wrong   TYPE d,
          lv_date_correct TYPE d,
          lv_time_correct TYPE t.

    IF iv_timestamp+9(6) = '240000'.
      lv_date_wrong = iv_timestamp+1(8).
      lv_time_correct = '000000'.
      CALL METHOD cl_abap_tstmp=>td_add
        EXPORTING
          date     = lv_date_wrong
          time     = lv_time_correct
          secs     = 86400
        IMPORTING
          res_date = lv_date_correct
          res_time = lv_time_correct.

      CONCATENATE iv_timestamp(1) lv_date_correct lv_time_correct INTO rv_timestamp.
    ELSE.
      rv_timestamp = iv_timestamp.
    ENDIF.
  ENDMETHOD.


  METHOD SEND_IDOC_EHPOST01.
    FIELD-SYMBOLS:
      <ls_idoc_data> TYPE /saptrx/s_logsys_idoc_map.
    DATA: ls_master_idoc_control        TYPE edidc,
          lt_master_idoc_data           TYPE edidd_tt,
          ls_master_idoc_data           TYPE edidd,
          ls_e1ehpao                    TYPE e1ehpao,
          ls_e1ehptid                   TYPE e1ehptid,
          ls_e1ehpcp                    TYPE e1ehpcp,
          ls_e1ehpee                    TYPE e1ehpee,
          lt_communication_idoc_control TYPE edidc_tt,
          lr_appobj_ctabs               TYPE REF TO trxas_appobj_ctab_wa,
          lr_control                    TYPE REF TO /saptrx/bapi_trk_control_data,
          lr_tracking_id                TYPE REF TO /saptrx/bapi_trk_track_id,
          lr_exp_events                 TYPE REF TO /saptrx/bapi_trk_exp_events,
          lv_timestamp                  TYPE tzntstmpsl.

* fill idoc control
    ls_master_idoc_control-rcvprt = cv_partner_type.
    ls_master_idoc_control-rcvprn = is_trxserv-trx_server.
    ls_master_idoc_control-mestyp = cv_msg_type_aopost.
    ls_master_idoc_control-idoctp = cv_idoc_type_ehpost01.

    LOOP AT it_appobj_ctabs REFERENCE INTO lr_appobj_ctabs
      WHERE trxservername = is_trxserv-trx_server_id.
      CLEAR ls_e1ehpao.
*     fill idoc header segment
      ls_master_idoc_data-mandt = sy-mandt.
      ls_master_idoc_data-segnam = cv_seg_e1ehpao.

* supported fields have to be populated in the IDOC; the ones not used here are NOT supported in GT&T
      ls_e1ehpao-appsys     = iv_appsys.
      ls_e1ehpao-appobjtype = lr_appobj_ctabs->appobjtype.
      ls_e1ehpao-appobjid   = lr_appobj_ctabs->appobjid.
      ls_master_idoc_data-sdata = ls_e1ehpao.
      INSERT ls_master_idoc_data INTO TABLE lt_master_idoc_data.

*     fill control parameters segment
      LOOP AT it_control REFERENCE INTO lr_control
          WHERE appsys = iv_appsys AND
                appobjtype = lr_appobj_ctabs->appobjtype AND
                appobjid   = lr_appobj_ctabs->appobjid.
        CLEAR ls_e1ehpcp.
        ls_master_idoc_data-segnam = cv_seg_e1ehpcp.
* supported fields have to be populated in the IDOC; the ones not used here are NOT supported in GT&T
        ls_e1ehpcp-paramname      = lr_control->paramname.
        ls_e1ehpcp-paramindex     = lr_control->paramindex.
        ls_e1ehpcp-value          = lr_control->value.
        IF lr_control->action = 'D'. "only used in deletion case
          ls_e1ehpcp-action         = lr_control->action.
        ENDIF.
        ls_master_idoc_data-sdata = ls_e1ehpcp.
        INSERT ls_master_idoc_data INTO TABLE lt_master_idoc_data.
      ENDLOOP.

*     fill expected events segment
      LOOP AT it_exp_event REFERENCE INTO lr_exp_events
          WHERE appsys = iv_appsys AND
                appobjtype = lr_appobj_ctabs->appobjtype AND
                appobjid   = lr_appobj_ctabs->appobjid.
        CLEAR ls_e1ehpee.
        ls_master_idoc_data-segnam = cv_seg_e1ehpee.

* supported fields have to be populated in the IDOC; the ones not used here are NOT supported in GT&T
        ls_e1ehpee-milestonenum       = lr_exp_events->milestonenum.
        ls_e1ehpee-milestone          = lr_exp_events->milestone.
*        ls_e1ehpee-carrtype           = lr_exp_events->carrtype.
        ls_e1ehpee-carrid             = lr_exp_events->carrid.
        ls_e1ehpee-loctype            = lr_exp_events->loctype.
        ls_e1ehpee-locid1             = lr_exp_events->locid1.
        ls_e1ehpee-locid2             = lr_exp_events->locid2.
*        ls_e1ehpee-sndtype            = lr_exp_events->sndtype.
        ls_e1ehpee-sndid              = lr_exp_events->sndid.

        ls_e1ehpee-msg_exp_datetime   = check_and_correct_timestamp( lr_exp_events->msg_exp_datetime ).
        ls_e1ehpee-msg_exp_tzone      = lr_exp_events->msg_exp_tzone.
        ls_e1ehpee-msg_er_exp_dtime   = check_and_correct_timestamp( lr_exp_events->msg_er_exp_dtime ).
        ls_e1ehpee-msg_lt_exp_dtime   = check_and_correct_timestamp( lr_exp_events->msg_lt_exp_dtime ).
        ls_e1ehpee-evt_exp_datetime   = check_and_correct_timestamp( lr_exp_events->evt_exp_datetime ).
        ls_e1ehpee-evt_exp_tzone      = lr_exp_events->evt_exp_tzone.
        ls_e1ehpee-evt_er_exp_dtime   = check_and_correct_timestamp( lr_exp_events->evt_er_exp_dtime ).
        ls_e1ehpee-evt_lt_exp_dtime   = check_and_correct_timestamp( lr_exp_events->evt_lt_exp_dtime ).
        ls_e1ehpee-itemident          = lr_exp_events->itemident.
        ls_e1ehpee-datacs             = lr_exp_events->datacs.
        ls_e1ehpee-dataid             = lr_exp_events->dataid.

        ls_master_idoc_data-sdata = ls_e1ehpee.
        INSERT ls_master_idoc_data INTO TABLE lt_master_idoc_data.
      ENDLOOP.

*     fill tracking IDs segment
      LOOP AT it_tracking_id REFERENCE INTO lr_tracking_id
          WHERE appsys = iv_appsys AND
                appobjtype = lr_appobj_ctabs->appobjtype AND
                appobjid   = lr_appobj_ctabs->appobjid.
        CLEAR ls_e1ehptid.
        ls_master_idoc_data-segnam = cv_seg_e1ehptid.
* supported fields have to be populated in the IDOC; the ones not used here are NOT supported in GT&T
        ls_e1ehptid-trxcod          = lr_tracking_id->trxcod.
        ls_e1ehptid-trxid           = lr_tracking_id->trxid.
        ls_e1ehptid-start_date      = check_and_correct_timestamp( lr_tracking_id->start_date ).
        ls_e1ehptid-end_date        = check_and_correct_timestamp( lr_tracking_id->end_date ).
        ls_e1ehptid-timzon          = lr_tracking_id->timzon.
        IF lr_tracking_id->action = 'D'. "only used in deletion case
          ls_e1ehptid-action          = lr_tracking_id->action.
        ENDIF.

        ls_master_idoc_data-sdata = ls_e1ehptid.
        INSERT ls_master_idoc_data INTO TABLE lt_master_idoc_data.
      ENDLOOP.

    ENDLOOP.

    CHECK lt_master_idoc_data IS NOT INITIAL.

    CASE is_trxserv-em_version.
*     EM version = GTT1.0 -> distribute IDOC EHPOST
      WHEN cv_tt_version-gtt10.

        CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
          EXPORTING
            master_idoc_control            = ls_master_idoc_control
          TABLES
            communication_idoc_control     = lt_communication_idoc_control
            master_idoc_data               = lt_master_idoc_data
          EXCEPTIONS
            error_in_idoc_control          = 1
            error_writing_idoc_status      = 2
            error_in_idoc_data             = 3
            sending_logical_system_unknown = 4
            OTHERS                         = 5.
        IF sy-subrc <> 0.
          " fill bapireturn
        ENDIF.
*     EM version = GTT2.0 -> collect the content grouped by logical system to compose combined IDOC GTTMSG later
      WHEN cv_tt_version-gtt20.
        READ TABLE st_idoc_data WITH KEY trx_server = is_trxserv-trx_server ASSIGNING <ls_idoc_data>.

        IF sy-subrc IS INITIAL.
          APPEND LINES OF lt_master_idoc_data TO <ls_idoc_data>-idoc_data.
        ELSE.
          APPEND INITIAL LINE TO st_idoc_data ASSIGNING <ls_idoc_data>.
          <ls_idoc_data>-trx_server = is_trxserv-trx_server.
          APPEND LINES OF lt_master_idoc_data TO <ls_idoc_data>-idoc_data.
        ENDIF.

      WHEN OTHERS.
        " do nothing
    ENDCASE.

  ENDMETHOD.


  METHOD SEND_IDOC_GTTMSG01.
**********************************************************************
* This method creates and distributes the new IDOC type GTTMSG01 to
* SAP Logistics Busiss Network - Global Track & Trace Option
* Content of ST_IDOC_DATA must not be empty and
* is expected to be filled by methods
*   - ZCL_GTT_SOF_UPD_XTP_REFERENCES=>SEND_IDOC_EHPOST01
* Expected EM version is GTT2.0
**********************************************************************
    DATA: ls_master_idoc_control        TYPE edidc,
          lt_communication_idoc_control TYPE edidc_tt,
          lt_master_idoc_data           TYPE edidd_tt.

    FIELD-SYMBOLS:
          <ls_idoc_data> TYPE /saptrx/s_logsys_idoc_map.

    " Check if data is provided
    CHECK st_idoc_data IS NOT INITIAL.

    " Create IDOC header
    ls_master_idoc_control-rcvprt = cv_partner_type.
    ls_master_idoc_control-mestyp = cv_msg_type_gttmsg.
    ls_master_idoc_control-idoctp = cv_idoc_type_gttmsg01.

    " Send message to each logical system separately
    LOOP AT st_idoc_data ASSIGNING <ls_idoc_data>.
      CLEAR: lt_master_idoc_data.

      " Set Tracking Server (Logical System)
      ls_master_idoc_control-rcvprn = <ls_idoc_data>-trx_server.
      " Set IDOC Content
      lt_master_idoc_data = <ls_idoc_data>-idoc_data.

      " Distribute IDOC
      CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
        EXPORTING
          master_idoc_control            = ls_master_idoc_control
        TABLES
          communication_idoc_control     = lt_communication_idoc_control
          master_idoc_data               = lt_master_idoc_data
        EXCEPTIONS
          error_in_idoc_control          = 1
          error_writing_idoc_status      = 2
          error_in_idoc_data             = 3
          sending_logical_system_unknown = 4
          OTHERS                         = 5.

      IF sy-subrc IS NOT INITIAL.
        " fill bapireturn
      ENDIF.
    ENDLOOP.

    " Clear data for next iteration
    CLEAR st_idoc_data.

  ENDMETHOD.
ENDCLASS.""",
    r"""class ZCL_GTT_SOF_IMP_LE_SHIPPING definition
  public
  final
  create public .

*"* public components of class CL_EXM_IM_LE_SHP_DELIVERY_PROC
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_DELIVERY_PROC .

  methods SEND_SO_HD_IDOC
    importing
      !IV_APPSYS type LOGSYS
      !IV_TZONE type TIMEZONE
      !IT_SO_TYPE type RSELOPTION
      !IT_DLV_TYPE type RSELOPTION
      !IT_XLIKP type SHP_LIKP_T
      !IT_XLIPS type SHP_LIPS_T .
protected section.
*"* protected components of class CL_EXM_IM_LE_SHP_DELIVERY_PROC
*"* do not include other source files here!!!
private section.

  methods CHECK_DLV_ITEM
    importing
      !IV_PSTYV type PSTYV_VL
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods PREPARE_PLN_EVT
    importing
      !IV_APPSYS type LOGSYS
      !IV_OBJ_TYPE type /SAPTRX/TRK_OBJ_TYPE
      !IV_AOT_TYPE type /SAPTRX/AOTYPE
      !IV_SERVER_NAME type /SAPTRX/TRXSERVERNAME
      !IS_VBAK type /SAPTRX/SD_SDS_HDR
      !IT_VBAP type VA_VBAPVB_T
    exporting
      !ET_EXP_EVENT type /SAPTRX/BAPI_TRK_EE_TAB .
*"* private components of class CL_EXM_IM_LE_SHP_DELIVERY_PROC
*"* do not include other source files here!!!
ENDCLASS.""",
    r"""INTERFACE zif_gtt_sof_ctp_types
  PUBLIC .


  TYPES:
    BEGIN OF ts_fu_list,
      tor_id        TYPE /scmtms/tor_id,
      item_id       TYPE /scmtms/item_id,
      quantity      TYPE menge_d,
      quantityuom   TYPE meins,
      product_id    TYPE /scmtms/product_id,
      product_descr TYPE /scmtms/item_description,
      base_uom_val  TYPE /scmtms/qua_base_uom_val,
      base_uom_uni  TYPE /scmtms/qua_base_uom_uni,
      change_mode   TYPE /bobf/conf_change_mode,
    END OF ts_fu_list .
  TYPES:
    tt_fu_list  TYPE STANDARD TABLE OF ts_fu_list
                       WITH EMPTY KEY .
  TYPES:
    BEGIN OF ts_delivery_item,
      vbeln   TYPE vbeln_vl,
      posnr   TYPE posnr_vl,
      likp    TYPE likpvb,
      lips    TYPE lipsvb,
      vbuk    TYPE vbukvb,
      vbup    TYPE vbupvb,
      vbfa    TYPE STANDARD TABLE OF vbfavb WITH EMPTY KEY,
      fu_list TYPE tt_fu_list,
    END OF ts_delivery_item .
  TYPES:
    tt_delivery_item TYPE STANDARD TABLE OF ts_delivery_item
                            WITH EMPTY KEY .
  TYPES:
    BEGIN OF ts_delivery,
      vbeln        TYPE vbeln_vl,
      fu_relevant  TYPE abap_bool,
      pod_relevant TYPE abap_bool,
      likp         TYPE likpvb,
      lips         TYPE STANDARD TABLE OF lipsvb WITH EMPTY KEY,
      vbuk         TYPE STANDARD TABLE OF vbukvb WITH EMPTY KEY,
      vbup         TYPE STANDARD TABLE OF vbupvb WITH EMPTY KEY,
      vbfa         TYPE STANDARD TABLE OF vbfavb WITH EMPTY KEY,
    END OF ts_delivery .
  TYPES:
    tt_delivery TYPE STANDARD TABLE OF ts_delivery .
  TYPES:
    BEGIN OF ts_delivery_chng,
      vbeln         TYPE vbeln_vl,
      posnr         TYPE posnr_vl,
      tor_id        TYPE /scmtms/tor_id,
      item_id       TYPE /scmtms/item_id,
      quantity      TYPE /scmtms/qua_pcs_val,
      quantityuom   TYPE /scmtms/qua_pcs_uni,
      product_id    TYPE /scmtms/product_id,
      product_descr TYPE /scmtms/item_description,
      base_uom_val  TYPE /scmtms/qua_base_uom_val,
      base_uom_uni  TYPE /scmtms/qua_base_uom_uni,
      change_mode   TYPE /bobf/conf_change_mode,
    END OF ts_delivery_chng .
  TYPES:
    tt_delivery_chng TYPE SORTED TABLE OF ts_delivery_chng
                            WITH UNIQUE KEY vbeln posnr tor_id item_id.
  TYPES:
    tt_tor_type TYPE SORTED TABLE OF /scmtms/tor_type
                       WITH UNIQUE KEY table_line .
  TYPES:
    BEGIN OF ty_aotype,
      tor_type TYPE /scmtms/tor_type,
      aotype   TYPE /saptrx/aotype,
    END OF ty_aotype .
  TYPES:
    tt_aottype TYPE SORTED TABLE OF ty_aotype
                      WITH UNIQUE KEY tor_type .
  TYPES:
    BEGIN OF ty_aotype_item,
      obj_type TYPE /saptrx/trk_obj_type,
      aot_type TYPE /saptrx/aotype,
    END OF ty_aotype_item .
  TYPES:
    tt_aotype_item TYPE TABLE OF ty_aotype_item .
  TYPES:
    BEGIN OF ty_aotypes_new,
      trk_obj_type  TYPE /saptrx/aotypes-trk_obj_type,
      aotype        TYPE /saptrx/aotypes-aotype,
      trxservername TYPE /saptrx/aotypes-trxservername,
    END OF ty_aotypes_new .
  TYPES:
    tt_aotypes_new TYPE TABLE OF ty_aotypes_new .
  TYPES:
    BEGIN OF ty_trxserv,
      trx_server_id TYPE /saptrx/trxserv-trx_server_id,
      trx_server    TYPE /saptrx/trxserv-trx_server,
    END OF ty_trxserv .
  TYPES:
    tt_trxserv TYPE TABLE OF ty_trxserv .
  TYPES:
    BEGIN OF ty_likp,
      vbeln TYPE likp-vbeln, "Delivery
      kodat TYPE likp-kodat, "Picking Date
      kouhr TYPE likp-kouhr, "Picking Time (Local Time, with Reference to a Plant)
      vstel TYPE likp-vstel, "Shipping Point / Receiving Point
      wadat TYPE likp-wadat, "Planned Goods Movement Date
      wauhr TYPE likp-wauhr, "Time of Goods Issue (Local, Relating to a Plant)
      kunnr TYPE likp-kunnr, "Ship-to Party
    END OF ty_likp .
  TYPES:
    tt_likp TYPE TABLE OF ty_likp .
  TYPES:
    tt_aotype_rst   TYPE RANGE OF /saptrx/aotype .
  TYPES:
    tt_tor_type_rst TYPE RANGE OF /scmtms/tor_type .
  TYPES:
    BEGIN OF ts_lipsvb_key,
      vbeln TYPE vbeln_vl,
      posnr TYPE posnr_vl,
    END OF ts_lipsvb_key .
  TYPES:
    tt_lipsvb_key TYPE STANDARD TABLE OF ts_lipsvb_key .
  TYPES:
    BEGIN OF ts_aotype,
      obj_type    TYPE /saptrx/trk_obj_type,
      aot_type    TYPE /saptrx/aotype,
      server_name TYPE /saptrx/trxservername,
    END OF ts_aotype .
  TYPES:
    tt_aotype TYPE STANDARD TABLE OF ts_aotype WITH EMPTY KEY .
  TYPES ts_lipsvb TYPE lipsvb .
  TYPES:
    tt_lipsvb TYPE STANDARD TABLE OF ts_lipsvb .
  TYPES:
    tt_trxas_appobj_ctab TYPE STANDARD TABLE OF trxas_appobj_ctab_wa
                                WITH EMPTY KEY .
  TYPES:
    BEGIN OF ts_idoc_data,
      control      TYPE /saptrx/bapi_trk_control_tab,
      info         TYPE /saptrx/bapi_trk_info_tab,
      tracking_id  TYPE /saptrx/bapi_trk_trkid_tab,
      exp_event    TYPE /saptrx/bapi_trk_ee_tab,
      trxserv      TYPE /saptrx/trxserv,
      appsys       TYPE logsys,
      appobj_ctabs TYPE tt_trxas_appobj_ctab,
    END OF ts_idoc_data .
  TYPES:
    tt_idoc_data TYPE STANDARD TABLE OF ts_idoc_data
                   WITH EMPTY KEY .
  TYPES:
    tt_trk_obj_type TYPE STANDARD TABLE OF /saptrx/trk_obj_type
                           WITH EMPTY KEY .
  TYPES:
    BEGIN OF ts_fu_id,
      tor_id TYPE /scmtms/tor_id,
    END OF ts_fu_id .
  TYPES:
    tt_fu_id  TYPE STANDARD TABLE OF ts_fu_id
                       WITH EMPTY KEY .
ENDINTERFACE.""",
    r""""""

]

id_counter = 1
dataset = []
for code in safe_templates:
    dataset.append({
        "id": f"ABAP-rap-sample-{id_counter}",
        "cwe": "safe",
        "label": "safe",
        "code": code
    })
    id_counter += 1

with open("./abap_dataset.jsonl", "a", encoding="utf-8") as f:
    for item in dataset:
        f.write(json.dumps(item, ensure_ascii=False) + "\n")