import json

dataset = []

safe_templates = [
    r"""MODULE STATUS_0100 OUTPUT.
        SET PF-STATUS '0100'.
        SET TITLEBAR '0100' WITH TEXT-T02.
    ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_OBJECT OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE SET_OBJECT OUTPUT.

  IF GO_100-O_DOCK IS INITIAL.
    PERFORM SET_OBJECT_100.
    PERFORM SET_ATTRIBUTES_100.
  ENDIF.
ENDMODULE.""",
    r"""TABLES : EBAN, MARC , MAKT, MARA, EKKO, EKPO, EINE, EINA.

*----------------------------------------------------------------------*
*&  CLASS
*----------------------------------------------------------------------*
CLASS : LCL_EVENT_RECEIVER DEFINITION DEFERRED,
            LCL_ALV_GRID       DEFINITION DEFERRED.
*----------------------------------------------------------------------*
*&  TYPE-POOLS
*----------------------------------------------------------------------*
TYPE-POOLS ICON.
TYPE-POOLS SLIS.
TYPE-POOLS SYM.
TYPE-POOLS LINE.
TYPE-POOLS COL.
*----------------------------------------------------------------------*
* VARIANT
*----------------------------------------------------------------------*
DATA : GV_OKCODE   TYPE SY-UCOMM,
       GV_SAVEOK   TYPE SY-DATUM,
       GV_ANS,
       GV_CHG,
       GV_ERR_FLAG,
       GV_CNT      TYPE I,
       GV_CODE     TYPE SY-UCOMM,
       GT_TEXT     TYPE TABLE OF LINE,
       GS_TEXT     LIKE LINE OF GT_TEXT,
       GV_TEXT     TYPE STRING.

DATA : GV_LCHR(1000).
DATA : GT_HTML LIKE W3HTML OCCURS 100 WITH HEADER LINE,
       GT_MIME LIKE W3MIME OCCURS 100 WITH HEADER LINE.

*&--------------------------------------------------------------------*
*&  CONSTANTS
*&--------------------------------------------------------------------*
CONSTANTS : C_1 TYPE N           VALUE '1',
            C_2 TYPE N           VALUE '2',
            C_3 TYPE N           VALUE '3',
            C_4 TYPE N           VALUE '4',
            C_5 TYPE N           VALUE '5',
            C_6 TYPE N           VALUE '6',
            C_7 TYPE N           VALUE '7',
            C_8 TYPE N           VALUE '8',
            C_9 TYPE N           VALUE '9',
            C_0 TYPE N           VALUE '0',

            C_A TYPE C           VALUE 'A',
            C_B TYPE C           VALUE 'B',
            C_C TYPE C           VALUE 'C',
            C_D TYPE C           VALUE 'D',
            C_E TYPE C           VALUE 'E',
            C_F TYPE C           VALUE 'F',
            C_G TYPE C           VALUE 'G',
            C_H TYPE C           VALUE 'H',
            C_I TYPE C           VALUE 'I',
            C_J TYPE C           VALUE 'J',
            C_K TYPE C           VALUE 'K',
            C_L TYPE C           VALUE 'L',
            C_M TYPE C           VALUE 'M',
            C_N TYPE C           VALUE 'N',
            C_O TYPE C           VALUE 'O',
            C_P TYPE C           VALUE 'P',
            C_Q TYPE C           VALUE 'Q',
            C_R TYPE C           VALUE 'R',
            C_S TYPE C           VALUE 'S',
            C_T TYPE C           VALUE 'T',
            C_U TYPE C           VALUE 'U',
            C_V TYPE C           VALUE 'V',
            C_W TYPE C           VALUE 'W',
            C_X TYPE C           VALUE 'X',
            C_Y TYPE C           VALUE 'Y',
            C_Z TYPE C           VALUE 'Z'.

CONSTANTS : C_EQ(2) TYPE C       VALUE 'EQ',
            C_NE(2) TYPE C       VALUE 'NE'.

CONSTANTS : GC_SPACE  TYPE C           VALUE SPACE,
            GV_GSFCAT TYPE CHAR12      VALUE 'GS_FIELDCAT-'.

CONSTANTS : C_INSR(4) TYPE C           VALUE 'INSR',
            C_DELE(4) TYPE C           VALUE 'DELE',
            C_SAVE(4) TYPE C           VALUE 'SAVE',
            C_BACK(4) TYPE C           VALUE 'BACK',
            C_EXIT(4) TYPE C           VALUE 'EXIT',
            C_ENTR(4) TYPE C           VALUE 'ENTR',
            C_CANC(4) TYPE C           VALUE 'CANC',
            C_SALL(4) TYPE C           VALUE 'SALL',
            C_DALL(4) TYPE C           VALUE 'DALL',
            C_TEXT(4) TYPE C           VALUE 'TEXT'.

CONSTANTS : C_BTN1(4) TYPE C           VALUE 'BTN1', "수량등록
            C_BTN2(4) TYPE C           VALUE 'BTN2'. "명세서출력

CONSTANTS : C_LGORT_2300 TYPE LGORT_D    VALUE '2300',
            C_LGORT_1200 TYPE LGORT_D    VALUE '1200',
            C_LGORT_1293 TYPE LGORT_D    VALUE '1293',
            C_LGORT_1292 TYPE LGORT_D    VALUE '1292'.

CONSTANTS : C_KSCHL_PB00 TYPE KSCHA      VALUE 'PB00',
            C_MTART_ROH2 TYPE MTART      VALUE 'ROH2'.
*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES : BEGIN OF TY_100.
TYPES : EKGRP      LIKE EINE-EKGRP,    "구매그룹
        EKORG      LIKE A017-EKORG,    "구매조직
        LIFNR      LIKE EBAN-FLIEF,    "구매처
        WERKS      LIKE MARC-WERKS,    "플랜트

        BANFN      LIKE EBAN-BANFN,    "구매요청
        BNFPO      LIKE EBAN-BNFPO,    "구매요청품목
        ICON(4),
        STATUS,
        ERMSG(100),                   "오류설명

        EKORG_TX   LIKE T024E-EKOTX,   "구매조직내역

        EKGRP_TX   LIKE T024-EKNAM,    "구매그룹내역

        WERKS_TX   LIKE T001W-NAME1,   "플랜트내역

        LIFNR_TX   LIKE LFA1-NAME1,    "구매처내역
        MTART      LIKE MARA-MTART,    "자재유형
        MTART_TX   LIKE T134T-MTBEZ,   "자재유형 내역
        MATKL      LIKE MARA-MATKL,    "자재그룹
        MATKL_TX   LIKE T023T-WGBEZ,   "자재그룹 내역
        MATNR      LIKE MARA-MATNR,    "자재코드
        MAKTG      LIKE MAKT-MAKTG,    "자재내역

        EBELN      LIKE EKPO-EBELN,    "구매오더
        EBELP      LIKE EKPO-EBELP,    "구매오더 품목

        MINBM      LIKE EINE-MINBM,    "최소오더수량 MOQ

        MENGE      LIKE EBAN-MENGE,    "구매요청 수량
        MEINS      LIKE EBAN-MEINS,    "구매요청 단위
        INQTY      LIKE EBAN-MENGE,    "실구매수량 Input
        BADAT      LIKE EBAN-BADAT,    "구매요청일
        LFDAT      LIKE EBAN-LFDAT,    "납품요철일

        KBETR      LIKE KONP-KBETR,    "구매단가
        KONWA      LIKE KONP-KONWA,    "조건단위

        ZZMBMSTAT  LIKE MARC-ZZMBMSTAT, "BM사양상태
        ZZMBMTEXT  LIKE MARC-ZZMBMTEXT, "BM사양상태 내역
        ZZMDNSTAT  LIKE MARC-ZZMDNSTAT, "DN사양상태
        ZZMDNTEXT  LIKE MARC-ZZMDNTEXT, "DN사양상태 내역
        MATNR2     LIKE MARC-ZZMATNR3, "금형코드
        MINBM2     LIKE EINE-MINBM,    "최소오더수량 MOQ
        MAKTG2     LIKE MAKT-MAKTG,     "금형코드 내역
        WAERS      LIKE EBAN-WAERS,     "통화

        ZZVSTAT    LIKE LFM1-ZZVSTAT,  "구매처상태
        ZZVSTAT_TX LIKE DD07V-DDTEXT,  "구매처 내역

        MSG(100),                       "처리결과 메세지

        LAND1      LIKE LFA1-LAND1,     "국가코드
        WEBAZ      LIKE MARC-WEBAZ,
        PLIFZ      LIKE MARC-PLIFZ,           "EINE-APLFZ  로 대체,  납품소요일수.
        MEINS2     LIKE EINA-MEINS,     "moq단위

        KNTTP      TYPE KNTTP,          "계정 지정 범주
        SAKTO      TYPE SAKTO,          "GL계정
        KOSTL      TYPE KOSTL,          "코드스센터
        SPERR      LIKE LFB1-SPERR,
        LOEVM      LIKE LFB1-LOEVM,
        LABST      LIKE MARD-LABST,    "1100/1200 재고.
        MEINS_LABST  LIKE MARA-MEINS,

        "20190930 추가
        LABST_2300 LIKE MARD-LABST, "2300재고
        LABST_RES  LIKE MARD-LABST, "미착(PO)합 재고
        LABST_SUM  LIKE MARD-LABST, "1200 + 2300 + 미착

        BSAKZ      LIKE EKKO-BSAKZ,
        BISMT      LIKE MARA-BISMT,
        FERTH      LIKE MARA-FERTH,

        BPRME      LIKE EINE-BPRME, "금형코드 단위
        STYL       TYPE LVC_T_STYL,    " attribute ##.
        SCOL       TYPE LVC_T_SCOL.    " color
TYPES  END OF TY_100.


TYPES : BEGIN OF TY_ALV,
          O_DOCK        TYPE REF TO CL_GUI_DOCKING_CONTAINER,
          O_GRID        TYPE REF TO LCL_ALV_GRID,
          O_CONT        TYPE REF TO CL_GUI_CONTAINER,
          O_CUST        TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
          O_MR_TOOLBAR  TYPE REF TO CL_GUI_TOOLBAR,
          T_FIELDCAT    TYPE LVC_T_FCAT,
          S_FIELDCAT    TYPE LVC_S_FCAT,
          S_LAYOUT      TYPE LVC_S_LAYO,
          S_SORT        TYPE LVC_S_SORT,
          T_SORT        TYPE LVC_T_SORT,
          T_DROP_DOWN   TYPE LVC_T_DROP,
          S_DROP_DOWN   TYPE LVC_S_DROP,
          T_CELLTAB     TYPE LVC_T_STYL,
          S_CELLTAB     TYPE LVC_S_STYL,
          T_EXCLUDE     TYPE UI_FUNCTIONS,
          S_TOOLBAR     TYPE STB_BUTTON,
          S_F4          TYPE LVC_S_F4,
          T_F4          TYPE LVC_T_F4,
          S_VARIANT     TYPE DISVARIANT,
          S_VARIANT_OUT TYPE DISVARIANT,
        END OF TY_ALV.


DATA : GO_100  TYPE TY_ALV.

*----------------------------------------------------------------------*
* Structure
*----------------------------------------------------------------------*
DATA : GT_DISP_100 TYPE TABLE OF TY_100 WITH HEADER LINE.


DATA : GT_EINA LIKE TABLE OF EINA WITH HEADER LINE,
       GT_EINE LIKE TABLE OF EINE WITH HEADER LINE.

DATA : GT_ZMMT0020 LIKE TABLE OF ZMMT0020 WITH HEADER LINE.

*마스터 Itab
DATA : GT_T024E LIKE TABLE OF T024E WITH HEADER LINE, "구매조직
       GT_T001W LIKE TABLE OF T001W WITH HEADER LINE, "플랜트
       GT_T024  LIKE TABLE OF T024  WITH HEADER LINE, "구매그룹
       GT_LFA1  LIKE TABLE OF LFA1  WITH HEADER LINE, "구매처
       GT_T134T LIKE TABLE OF T134T WITH HEADER LINE, "자재유형
       GT_T023T LIKE TABLE OF T023T WITH HEADER LINE, "자재그룹
       GT_MAKT  LIKE TABLE OF MAKT  WITH HEADER LINE. "자재내역

*  DATA : BEGIN OF GT_LIFNR OCCURS 0,
*           LIFNR LIKE LFA1-LIFNR,
*         END OF GT_LIFNR.

DATA : BEGIN OF GT_LABST_RES OCCURS 0,
         MATNR TYPE MATNR,
         MENGE TYPE LABST,
       END OF GT_LABST_RES.

DATA : BEGIN OF GT_MOQ OCCURS 0,
         MATNR LIKE EINA-MATNR,
         LIFNR LIKE EINA-LIFNR,
         EKORG LIKE EINE-EKORG,
         WERKS LIKE EINE-WERKS,
         MINBM LIKE EINE-MINBM, "최소 구매 오더 수량
         BPRME LIKE EINE-BPRME,
         MEINS LIKE EINA-MEINS,
         INFNR LIKE EINE-INFNR,
       END OF GT_MOQ.

DATA : BEGIN OF GT_KONP OCCURS 0,
         MATNR LIKE EINA-MATNR,
         LIFNR LIKE EINA-LIFNR,
         EKORG LIKE EINE-EKORG,
         WERKS LIKE EINE-WERKS,
         KBETR LIKE KONP-KBETR,  "구매단가
         KONWA LIKE KONP-KONWA,  "조건단위
       END OF GT_KONP.

DATA : BEGIN OF GT_LFM1 OCCURS 0,
         EKORG LIKE LFM1-EKORG,
         LIFNR LIKE LFM1-LIFNR,
*-S 구매오더 통화 필드 추가 20191007 by PWC096
         WAERS LIKE LFM1-WAERS,
*-E
       END OF GT_LFM1.

*----------------------------------------------------------------------*
* BAPI
*----------------------------------------------------------------------*
DATA : GS_POHEADER  TYPE BAPIMEPOHEADER,
       GS_POHEADERX TYPE BAPIMEPOHEADERX,
       G_EBELN      TYPE EBELN,
       GV_EBELP     TYPE EBELP.

DATA : GS_RETURN_PO     TYPE BAPIRET2,
       GS_POITEM        TYPE BAPIMEPOITEM,
       GS_POITEMX       TYPE BAPIMEPOITEMX,
       GS_POACCOUNT     TYPE BAPIMEPOACCOUNT,
       GS_POACCOUNTX    TYPE BAPIMEPOACCOUNTX,
       GS_POSCHEDULE    TYPE BAPIMEPOSCHEDULE,
       GS_POSCHEDULEX   TYPE BAPIMEPOSCHEDULX,
       GS_POCOND        TYPE BAPIMEPOCOND,
       GS_POCONDX       TYPE BAPIMEPOCONDX,
       GS_POCOMPONENTS  TYPE BAPIMEPOCOMPONENT,
       GS_POCOMPONENTSX TYPE BAPIMEPOCOMPONENTX,
       GS_POSERVICES    TYPE BAPIESLLC,
       GS_POSRVACCES    LIKE BAPIESKLC,
       GS_EXTENSIONIN   TYPE BAPIPAREX.

DATA : GT_RETURN_PO     TYPE TABLE OF BAPIRET2,
       GT_POITEM        TYPE TABLE OF BAPIMEPOITEM,
       GT_POITEMX       TYPE TABLE OF BAPIMEPOITEMX,
       GT_POACCOUNT     TYPE TABLE OF BAPIMEPOACCOUNT,
       GT_POACCOUNTX    TYPE TABLE OF BAPIMEPOACCOUNTX,
       GT_POSCHEDULE    TYPE TABLE OF BAPIMEPOSCHEDULE,
       GT_POSCHEDULEX   TYPE TABLE OF BAPIMEPOSCHEDULX,
       GT_POCOND        TYPE TABLE OF BAPIMEPOCOND,
       GT_POCONDX       TYPE TABLE OF BAPIMEPOCONDX,
       GT_POCOMPONENTS  TYPE TABLE OF BAPIMEPOCOMPONENT,
       GT_POCOMPONENTSX TYPE TABLE OF BAPIMEPOCOMPONENTX,
       GT_POSERVICES    TYPE TABLE OF BAPIESLLC,
       GT_POSRVACCES    LIKE TABLE OF BAPIESKLC,
       GT_EXTENSIONIN   TYPE TABLE OF BAPIPAREX,
       GT_POHTEXT       TYPE TABLE OF BAPIMEPOTEXTHEADER WITH HEADER LINE.

DATA : GS_RETURN_PR     TYPE BAPIRET2,
       GS_PRITEM        TYPE BAPIMEREQITEMIMP,
       GS_PRITEMX       TYPE BAPIMEREQITEMX.

DATA : GT_RETURN_PR     TYPE TABLE OF BAPIRET2,
       GT_PRITEM        TYPE TABLE OF BAPIMEREQITEMIMP,
       GT_PRITEMX       TYPE TABLE OF BAPIMEREQITEMX.


  DATA : BEGIN OF GT_EKPO OCCURS 0,
           EBELN LIKE EKPO-EBELN,
           EBELP LIKE EKPO-EBELP,
           BANFN LIKE EKPO-BANFN,
           BNFPO LIKE EKPO-BNFPO,
         END OF GT_EKPO.


DATA : G_LINE_NO  TYPE BAPIESLLC-LINE_NO,
       G_EXT_LINE TYPE BAPIESLLC-EXT_LINE,
       G_PCKGNO   TYPE EBAN-PACKNO.


*----------------------------------------------------------------------*
* Ranges
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* For Table control
*----------------------------------------------------------------------*

CONTROLS: TC_110 TYPE TABLEVIEW USING SCREEN 0110.

DATA : GV_TC_110_LINES LIKE SY-LOOPC,
       GV_CARRID       LIKE SCARR-CARRID,
       GV_CARRNAME     LIKE SCARR-CARRNAME.
*----------------------------------------------------------------------*
* Define : Objects
*----------------------------------------------------------------------*
DATA : GO_DOCKING          TYPE REF TO CL_GUI_DOCKING_CONTAINER.
DATA : GO_DOCKING2         TYPE REF TO CL_GUI_DOCKING_CONTAINER.
DATA : GO_CONT_100         TYPE REF TO CL_GUI_CONTAINER.
DATA : GO_CONT_101         TYPE REF TO CL_GUI_CONTAINER.
DATA : GO_CONT_110         TYPE REF TO CL_GUI_CONTAINER.
DATA : GO_CONT_120         TYPE REF TO CL_GUI_CONTAINER.
DATA : GO_CONT_130         TYPE REF TO CL_GUI_CONTAINER.
DATA : GO_GRID_100         TYPE REF TO CL_GUI_ALV_GRID.
DATA : GO_GRID_110         TYPE REF TO CL_GUI_ALV_GRID.
DATA : GO_GRID_120         TYPE REF TO CL_GUI_ALV_GRID.
DATA : GO_SPLIT            TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
DATA : GO_SPLIT2           TYPE REF TO CL_GUI_SPLITTER_CONTAINER.
DATA : GO_EDIT_200         TYPE REF TO CL_GUI_TEXTEDIT.

*****
DATA: G_TREE             TYPE REF TO CL_GUI_ALV_TREE,
      G_HIERARCHY_HEADER TYPE TREEV_HHDR,
      G_TOOLBAR          TYPE REF TO CL_GUI_TOOLBAR,
      GT_LIST_COMMENTARY TYPE SLIS_T_LISTHEADER,
      G_LOGO             TYPE SDYDO_VALUE.

DATA : GT_FIELDCAT TYPE LVC_T_FCAT,
       GS_FIELDCAT TYPE LVC_S_FCAT,
       GS_VARIANT  TYPE DISVARIANT.
DATA : GT_STYL     TYPE LVC_T_STYL.
*DATA : gt_sort100          TYPE lvc_t_sort,
*       gt_sort300          TYPE lvc_t_sort,
*       gt_sort400          TYPE lvc_t_sort.
*DATA : gs_sort             TYPE lvc_s_sort.
DATA : GS_LAYO    TYPE LVC_S_LAYO,
       GT_EXCLUDE TYPE UI_FUNCTIONS. "Tool Bar Button##

DATA : GS_SCROLL           TYPE LVC_S_STBL.
*HTML Document Viewer
DATA : GO_DOCUMENT         TYPE REF TO CL_DD_DOCUMENT.
DATA : GO_HTML             TYPE REF TO CL_GUI_HTML_VIEWER.
CONSTANTS : GC_ALV_GRID    TYPE SDYDO_ATTRIBUTE VALUE 'ALV_GRID',
            GC_LINE_LENGTH TYPE I VALUE 72.
DATA : GO_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.
DATA : GT_ROW              TYPE LVC_T_ROW.
DATA : GS_ROW              TYPE LVC_S_ROW.

*---------------------------------
DATA : GT_COL TYPE LVC_T_SCOL,
       GS_COL TYPE LVC_S_SCOL.

*---------------------------------

*----------------------------------------------------------------------*
* ADD TEXT                                                         *
*----------------------------------------------------------------------*
DEFINE _ADD_TEXT_HEAD.

  call method &1->add_text
    exporting
      text         = &2
      sap_style    = cl_dd_document=>heading
      sap_fontsize = cl_dd_area=>large
      sap_color    = cl_dd_area=>list_heading_int.

  call method &1->new_line.
*list_background_inv
END-OF-DEFINITION.
DEFINE _ADD_TEXT.

  call method &1->add_text
    exporting
      text         = &2
      sap_style    = cl_dd_document=>list_background_inv
      sap_fontsize = cl_dd_area=>large
      sap_color    = cl_dd_area=>list_heading_int.

  call method &1->new_line.
*list_background_inv
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* MACRO DEFINE                                                         *
*----------------------------------------------------------------------*
DEFINE __CLEAR_TAB.

  clear &1.
  refresh &1.

END-OF-DEFINITION.
*----------------------------------------------------------------------
* MACRO REFRESH ALV
*----------------------------------------------------------------------
DEFINE _REFRESHTAB.

  clear gs_scroll.
  gs_scroll-row = &3.  gs_scroll-col = &4.
  call method &1->refresh_table_display
    exporting
      i_soft_refresh = &2
      is_stable      = gs_scroll.

END-OF-DEFINITION.
*----------------------------------------------------------------------
* MACRO REFRESH ALV
*----------------------------------------------------------------------
DEFINE _ALPHA_INPUT.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = &1
    importing
      output = &2.

END-OF-DEFINITION.
*----------------------------------------------------------------------
* confirm popup
*----------------------------------------------------------------------
DEFINE _CONFIRM2.

  clear &4.
  call function 'POPUP_TO_CONFIRM_STEP'
    exporting
      textline1 = &1
      textline2 = &2
      titel     = &3
    importing
      answer    = &4.


END-OF-DEFINITION.

DEFINE _CONFIRM.
  CALL FUNCTION 'RSOS_POPUP_TO_CONFIRM'
    EXPORTING
      titlebar      = &3
      text_question = &1
      text_button_1 = TEXT-t07
      text_button_2 = TEXT-t08
    IMPORTING
      answer        = &2.
END-OF-DEFINITION.
*----------------------------------------------------------------------
* MACRO REFRESH ALV
*----------------------------------------------------------------------
DEFINE _REFRESHTAB.

  clear gs_scroll.
  gs_scroll-row = &3.  gs_scroll-col = &4.
  call method &1->refresh_table_display
    exporting
      i_soft_refresh = &2
      is_stable      = gs_scroll.

END-OF-DEFINITION.

*----------------------------------------------------------------------
* MACRO Modify cell
*----------------------------------------------------------------------
DEFINE _MODI_CELL.

  call method &1->modify_cell
    exporting
      i_row_id    = &2
      i_fieldname = &3
      i_value     = &4.

END-OF-DEFINITION.
*----------------------------------------------------------------------
* MACRO GET cell
*----------------------------------------------------------------------
DEFINE _GET_CELL.

  call method &1->GET_CELL_VALUE
    exporting
      i_row_id    = &2
      i_fieldname = &3
      IMPORTING
      E_value     = &4.

END-OF-DEFINITION.
*----------------------------------------------------------------------
* MACRO Modify STYLE
*----------------------------------------------------------------------
DEFINE _MODI_STYLE.
  CASE &4.
    WHEN GC_1.
    call method &1->modify_STYLE
      exporting
        i_row_id    = &2
        i_fieldname = &3
        i_STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    WHEN GC_2.
    call method &1->modify_STYLE
      exporting
        i_row_id    = &2
        i_fieldname = &3
        i_STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
   ENDCASE.

END-OF-DEFINITION.
*----------------------------------------------------------------------
* MACRO READ cell
*----------------------------------------------------------------------
DEFINE _READ_CELL.
  CASE &3.
    WHEN GC_1.
       READ TABLE &1->MT_GOOD_CELLS
               WITH KEY ROW_ID = &5
                        FIELDNAME = &2
                        STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED
               TRANSPORTING NO FIELDS.
    WHEN GC_2.
       READ TABLE &1->MT_GOOD_CELLS
               WITH KEY ROW_ID = &5
                        FIELDNAME = &2
                        STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED
               TRANSPORTING NO FIELDS.
   ENDCASE.
&4 = SY-SUBRC.
END-OF-DEFINITION.

*----------------------------------------------------------------------
* MACRO Message popup
*----------------------------------------------------------------------
DEFINE _MESSAGE_POPUP.
  clear &6.

  call function 'POPUP_TO_CONFIRM_STEP'
    exporting
      defaultoption  = &1
      titel          = &2
      textline1      = &3
      textline2      = &4
      cancel_display = &5
    importing
      answer         = &6.               " J# N### A##

END-OF-DEFINITION.

*----------------------------------------------------------------------
* MACRO Clear Itab
*----------------------------------------------------------------------
DEFINE _CLEAR.

  CLEAR : &1.
  REFRESH : &1.

END-OF-DEFINITION.

*----------------------------------------------------------------------
* MACRO Append Itab
*----------------------------------------------------------------------
DEFINE _APPEND.

  append : &1.
  clear : &1.

END-OF-DEFINITION.

DEFINE _COLLECT.

  collect : &1.
  clear : &1.

END-OF-DEFINITION.

*----------------------------------------------------------------------
* MACRO RANGES
*----------------------------------------------------------------------
DEFINE _RANGES.

  CLEAR : &1.
     &1-SIGN   = &2.
     &1-OPTION = &3.
     &1-LOW    = &4.
     &1-HIGH   = &5.
    COLLECT : &1.

END-OF-DEFINITION.
DEFINE _GET_APPEND.

  IF &2 is not initial.
     append : &1.
     clear : &1.
  endif.

END-OF-DEFINITION.
DEFINE _CONV.
  case &1.
    when 'I'.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = &2
        importing
          output = &3.
    when 'O'.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = &2
        importing
          output = &3.
  endcase.
END-OF-DEFINITION.

DATA : MOUNT_EX LIKE  BAPICURR-BAPICURR.

DEFINE _CONV_WAERS.
  clear : mount_ex.
  case &1.
    when 'I'.
      mount_ex = &3.
      call function 'BAPI_CURRENCY_CONV_TO_INTERNAL'
        exporting
          currency                 =  &2
          amount_external         =  mount_ex
          max_number_of_digits    = 13
       importing
         amount_internal   = &4.

    when 'O'.
      call function 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        exporting
          currency              = &2
          amount_internal       =  &3
       importing
         amount_external       =     mount_ex.

      &4 = mount_ex.

  endcase.
END-OF-DEFINITION.


DEFINE _SET_LAYOUT.

  CALL METHOD &1->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = &2.

END-OF-DEFINITION.""",
    r"""CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    DATA : MV_OBJECT_TEXT TYPE CHAR30.

    METHODS :
*CONSTRUCTOR
      CONSTRUCTOR    IMPORTING  IV_OBJECT_TEXT TYPE C,

*TOP-OF-PAGE
      HANDLE_TOP_OF_PAGE
                    FOR EVENT TOP_OF_PAGE   OF CL_GUI_ALV_GRID
        IMPORTING E_DYNDOC_ID,

*DATA CHANGED
      HANDLE_DATA_CHANGED
                    FOR EVENT DATA_CHANGED  OF CL_GUI_ALV_GRID
        IMPORTING SENDER ER_DATA_CHANGED,

*Data change finishe
      HANDLE_DATA_CHANGED_FINISHED
                    FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
        IMPORTING SENDER E_MODIFIED ET_GOOD_CELLS,

*Toolbar create
      HANDLE_TOOLBAR
                    FOR EVENT TOOLBAR       OF CL_GUI_ALV_GRID
        IMPORTING SENDER E_OBJECT E_INTERACTIVE,

*User command
      HANDLE_USER_COMMAND
                    FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING SENDER E_UCOMM,

*Hot spot
      HANDLE_HOTSPOT_CLICK
                    FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING SENDER E_ROW_ID E_COLUMN_ID,

*Double click
*HANDLE_DOUBLE_CLICK
      HANDLE_DOUBLE_CLICK
                    FOR EVENT DOUBLE_CLICK            OF CL_GUI_ALV_GRID
        IMPORTING SENDER
                    E_ROW
                    E_COLUMN
                    ES_ROW_NO,

*F4 search help
      HANDLE_ON_F4
                    FOR EVENT ONF4 OF CL_GUI_ALV_GRID
        IMPORTING SENDER
                    E_FIELDNAME
                    E_FIELDVALUE
                    ES_ROW_NO
                    ER_EVENT_DATA
                    ET_BAD_CELLS
                    E_DISPLAY.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
*CONSTRUCTOR
  METHOD  CONSTRUCTOR.
    CALL METHOD SUPER->CONSTRUCTOR.
    MV_OBJECT_TEXT = IV_OBJECT_TEXT.
  ENDMETHOD.                    "lcl_event_receiver

*TOP-OF-PAGE
  METHOD HANDLE_TOP_OF_PAGE.
*    PERFORM GRID110_TO_OF_PAGE.
  ENDMETHOD.                    "HANDLE_TOP_OF_PAGE

*DATA CHANGED
  METHOD HANDLE_DATA_CHANGED.
    PERFORM DATA_CHANGED USING SENDER ER_DATA_CHANGED.
  ENDMETHOD.                    "handle_data_changed

*Data changed finished
  METHOD HANDLE_DATA_CHANGED_FINISHED.
    PERFORM DATA_CHANGED_FINISHED USING SENDER E_MODIFIED ET_GOOD_CELLS.
  ENDMETHOD.                    "handle_data_changed_finished

*Toolbar create
  METHOD HANDLE_TOOLBAR.
*    PERFORM HANDLE_TOOLBAR_PART  USING SENDER E_OBJECT E_INTERACTIVE.
  ENDMETHOD.                     "HANDLE_TOOLBAR

*User command
  METHOD HANDLE_USER_COMMAND .
*    PERFORM HANDLE_USER_COMMAND  USING SENDER E_UCOMM .
  ENDMETHOD.                      "HANDLE_USER_COMMAND

*Hot spot
  METHOD HANDLE_HOTSPOT_CLICK.
*    PERFORM HOTSPOT_CLICK USING SENDER E_ROW_ID E_COLUMN_ID.
  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM HANDLE_DOUBLE_CLICK  USING SENDER E_ROW E_COLUMN .
  ENDMETHOD.                                        "HANDLE_DOUBLE_CLICK

  METHOD HANDLE_ON_F4.
*    PERFORM ON_F4 USING SENDER
*                        E_FIELDNAME
*                        E_FIELDVALUE
*                        ES_ROW_NO
*                        ER_EVENT_DATA
*                        ET_BAD_CELLS
*                        E_DISPLAY.
  ENDMETHOD.
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION




CLASS LCL_ALV_GRID DEFINITION INHERITING FROM CL_GUI_ALV_GRID.
  PUBLIC SECTION.
    METHODS: SET_OPTIMIZE_ALL_COLS,
      SET_CURSOR
        IMPORTING
          ROW TYPE I
          COL TYPE I,
      SET_FIXED_COLUMN,
      SET_ROW_RESIZE.

ENDCLASS.                    "lcl_alv_grid DEFINITION

CLASS LCL_ALV_GRID IMPLEMENTATION.
  METHOD SET_OPTIMIZE_ALL_COLS.
    CALL METHOD ME->OPTIMIZE_ALL_COLS
      EXPORTING
        INCLUDE_HEADER = 1.
  ENDMETHOD.                 " set_optimize_all_cols

  METHOD SET_CURSOR.
    CALL METHOD ME->SET_CURRENT_CELL_BASE
      EXPORTING
        ROW = ROW
        COL = COL.
  ENDMETHOD.                    " SET_CURSOR

  METHOD SET_FIXED_COLUMN.
    CALL METHOD ME->SET_FIXED_COLS
      EXPORTING
        COLS = 3.
  ENDMETHOD.                    " SET_FIXED_COLUMN

  METHOD SET_ROW_RESIZE.
    CALL METHOD ME->SET_RESIZE_ROWS
      EXPORTING
        ENABLE = 1.
  ENDMETHOD.                    " SET_ROW_RESIZE

ENDCLASS.                    " lcl_alv_grid IMPLEMENTATION
*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SENDER
*&      --> E_ROW
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM HANDLE_DOUBLE_CLICK  USING   PO_SENDER
                                   PV_ROW      TYPE LVC_S_ROW
                                   PV_COLUMN   TYPE LVC_S_COL.

  CASE PO_SENDER.
    WHEN GO_100-O_GRID.
      PERFORM HANDLE_DOUBLECLICK_100 USING PV_ROW PV_COLUMN.

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLECLICK_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV_ROW
*&      --> PV_COLUMN
*&---------------------------------------------------------------------*
FORM HANDLE_DOUBLECLICK_100  USING   PV_ROW      TYPE LVC_S_ROW
                                     PV_COLUMN   TYPE LVC_S_COL.


  DATA : LV_STRING TYPE STRING.
  READ TABLE GT_DISP_100 INDEX PV_ROW-INDEX.
  CHECK SY-SUBRC EQ 0.
  CASE PV_COLUMN.
    WHEN 'MATNR'. "자재코드
      CHECK GT_DISP_100-MATNR IS NOT INITIAL AND
            GT_DISP_100-WERKS IS NOT INITIAL.
      SET PARAMETER ID 'MAT' FIELD GT_DISP_100-MATNR.
      SET PARAMETER ID 'WRK' FIELD GT_DISP_100-WERKS.
      SET PARAMETER ID 'MXX' FIELD 'E'."구매 View
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    WHEN 'MATNR2'. "금형코드
      CHECK GT_DISP_100-MATNR2 IS NOT INITIAL AND
            GT_DISP_100-WERKS IS NOT INITIAL.
      SET PARAMETER ID 'MAT' FIELD GT_DISP_100-MATNR2.
      SET PARAMETER ID 'WRK' FIELD GT_DISP_100-WERKS.
      SET PARAMETER ID 'MXX' FIELD 'E'."구매 View
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    WHEN 'BANFN'. "구매요청
      CHECK GT_DISP_100-BANFN IS NOT INITIAL.
      SET PARAMETER ID 'BAN' FIELD GT_DISP_100-BANFN.
      CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.

    WHEN 'EBELN'. "구매오더
      CHECK GT_DISP_100-EBELN IS NOT INITIAL.
      SET PARAMETER ID 'BES' FIELD GT_DISP_100-EBELN.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_CHANGED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SENDER
*&      --> ER_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM DATA_CHANGED  USING PO_SENDER
                           PO_DATA_CHANGED
                            TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.



  CASE PO_SENDER.
    WHEN GO_100-O_GRID.
      PERFORM HANDLE_CHANGED_100 USING PO_DATA_CHANGED.

  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_CHANGED_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PO_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM HANDLE_CHANGED_100  USING PO_DATA_CHANGED
                            TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.


  DATA : LS_MOD_CELLS TYPE LVC_S_MODI.
  DATA : LS_DEL_ROWS  TYPE LVC_S_MOCE.

  DATA : LS_MODI TYPE LVC_S_MODI.
  DATA : OREF TYPE REF TO CX_ROOT.
  DATA : LV_CHG.
  DATA : LV_MANDT LIKE SY-MANDT.

  LOOP AT PO_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.

    GV_CHG = ABAP_TRUE.

    READ TABLE GT_DISP_100 INDEX LS_MOD_CELLS-ROW_ID.
    CHECK SY-SUBRC EQ 0.


    CASE LS_MOD_CELLS-FIELDNAME.
      WHEN 'ZGRMQTY' OR 'ZGRSQTY' OR 'ZGRVQTY' OR 'ZGRDIFFQTY'.
*        PERFORM SET_CHANGED_STATUS USING PO_DATA_CHANGED LS_MOD_CELLS.

        MODIFY GT_DISP_100 INDEX LS_MOD_CELLS-ROW_ID TRANSPORTING STATUS ICON.

    ENDCASE.


  ENDLOOP.

  IF NOT PO_DATA_CHANGED->MT_PROTOCOL[] IS INITIAL.
    PO_DATA_CHANGED->DISPLAY_PROTOCOL( ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SENDER
*&      --> E_MODIFIED
*&      --> ET_GOOD_CELLS
*&---------------------------------------------------------------------*
FORM DATA_CHANGED_FINISHED  USING    PO_SENDER
                                     PV_MODIFIED   TYPE CHAR01
                                     PT_GOOD_CELLS TYPE LVC_T_MODI.


  CASE PO_SENDER.
    WHEN GO_100-O_GRID.

      PERFORM DATA_CHANGED_FINISHED_100 USING PV_MODIFIED PT_GOOD_CELLS.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_CHANGED_FINISHED_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV_MODIFIED
*&      --> PT_GOOD_CELLS
*&---------------------------------------------------------------------*
FORM DATA_CHANGED_FINISHED_100  USING PV_MODIFIED   TYPE CHAR01
                                      PT_GOOD_CELLS TYPE LVC_T_MODI.

  DATA : LS_GOOD_CELLS LIKE LINE OF  PT_GOOD_CELLS.


  LOOP AT PT_GOOD_CELLS INTO LS_GOOD_CELLS.

    CASE LS_GOOD_CELLS-FIELDNAME.
      WHEN 'ZGRMQTY' OR 'ZGRSQTY' OR 'ZGRVQTY' OR 'ZGRDIFFQTY'.

*        READ TABLE GT_DISP_100 INDEX LS_GOOD_CELLS-ROW_ID.
*        CHECK SY-SUBRC EQ 0.
*
*        PERFORM CHECK_QTY USING GT_DISP_100 CHANGING GT_DISP_100-STATUS
*                                                     GT_DISP_100-ICON.
*
*        _MODI_CELL PO_DATA_CHANGED LS_GOOD_CELLS-ROW_ID 'STATUS' GT_DISP_100-STATUS.
*        _MODI_CELL PO_DATA_CHANGED LS_GOOD_CELLS-ROW_ID 'ICON' GT_DISP_100-ICON.
*
*        MODIFY GT_DISP_100 INDEX LS_GOOD_CELLS-ROW_ID TRANSPORTING STATUS ICON.


    ENDCASE.

  ENDLOOP.


ENDFORM.""",
    r"""SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-T01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(10) TEXT-T10.
SELECTION-SCREEN COMMENT 15(6) TEXT-T03 FOR FIELD P_R1.
PARAMETERS : P_R1 RADIOBUTTON GROUP RA1 DEFAULT 'X' USER-COMMAND U1.
SELECTION-SCREEN COMMENT 31(6) TEXT-T04 FOR FIELD P_R2.
PARAMETERS : P_R2 RADIOBUTTON GROUP RA1.
SELECTION-SCREEN COMMENT 47(6) TEXT-T05 FOR FIELD P_R3.
PARAMETERS : P_R3 RADIOBUTTON GROUP RA1.
SELECTION-SCREEN COMMENT 63(10) TEXT-T06 FOR FIELD P_R4.
PARAMETERS : P_R4 RADIOBUTTON GROUP RA1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN ULINE.

SELECT-OPTIONS :
*                 S_LFDAT   FOR EBAN-LFDAT,
                 S_BADAT   FOR EBAN-BADAT DEFAULT SY-DATLO.

SELECT-OPTIONS : S_EKORG   FOR EBAN-EKORG,
                 S_EKGRP   FOR EBAN-EKGRP,
                 S_WERKS   FOR EBAN-WERKS,
                 S_FLIEF   FOR EBAN-FLIEF,
                 S_MTART   FOR MARA-MTART,
                 S_MATKL   FOR MARA-MATKL,
                 S_MATNR   FOR MARA-MATNR,
                 S_MAKTG   FOR MAKT-MAKTG,
                 S_MATNR2  FOR MARA-MATNR,
                 S_LFDAT   FOR EBAN-LFDAT,
*                 S_BADAT   FOR EBAN-BADAT,
                 S_BANFN   FOR EBAN-BANFN,
                 S_EBELN   FOR EBAN-EBELN,
                 S2_EKGRP   FOR EBAN-EKGRP. "구매그룹 9*도 추출해야 하는 경우 있음. 20190522.


SELECTION-SCREEN END OF BLOCK A1.""",
    r"""MODULE EXIT INPUT.

  CLEAR GV_SAVEOK.
  GV_SAVEOK = GV_OKCODE.
  CASE GV_SAVEOK.
    WHEN C_BACK OR C_EXIT OR C_CANC.

      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  GV_CODE = GV_OKCODE.
  CLEAR GV_OKCODE.

  CASE GV_CODE.
    WHEN C_BTN1. "구매오더 생성
      PERFORM PORC_BTN1.


  ENDCASE.

ENDMODULE.""",
    r"""FORM INI_DATA .

  DATA : BEGIN OF LT_MTART OCCURS 0,
           MTART LIKE T134T-MTART,
         END OF LT_MTART.

  DATA : BEGIN OF LT_LIFNR OCCURS 0,
           LIFNR LIKE LFA1-LIFNR,
         END OF LT_LIFNR.

  DATA : BEGIN OF LT_EKGRP OCCURS 0,
           EKGRP LIKE T024-EKGRP,
         END OF LT_EKGRP.

  DATA : BEGIN OF LT_WERKS OCCURS 0,
           WERKS LIKE T001W-WERKS,
         END OF LT_WERKS.

  DATA : BEGIN OF LT_EKORG OCCURS 0,
           EKORG LIKE T024E-EKORG,
         END OF LT_EKORG.

  DATA : BEGIN OF LT_MATKL OCCURS 0,
           MATKL LIKE T023T-MATKL,
         END OF LT_MATKL.

  DATA : BEGIN OF LT_MATNR OCCURS 0,
           MATNR LIKE MAKT-MATNR,
         END OF LT_MATNR.

  DATA : BEGIN OF LT_MOQ OCCURS 0,
           MATNR LIKE EINA-MATNR,
           LIFNR LIKE EINA-LIFNR,
           EKORG LIKE EINE-EKORG,
           WERKS LIKE EINE-WERKS,
         END OF LT_MOQ.
  DATA : LT_LFM1 LIKE TABLE OF GT_LFM1 WITH HEADER LINE.

  DATA : R_EBELN TYPE RANGE OF EBAN-EBELN WITH HEADER LINE,
         R_FLIEF TYPE RANGE OF EBAN-FLIEF WITH HEADER LINE.

  DATA : BEGIN OF LT_TAB OCCURS 0,
           MATNR TYPE MATNR,
           MENGE TYPE MENGE_D,
         END OF LT_TAB.

  DATA : BEGIN OF LT_MATNR_RES OCCURS 0,
           MATNR TYPE MATNR,
         END OF LT_MATNR_RES.


*오더 상태
  CASE C_X.
    WHEN P_R1. "생성예정
      _RANGES R_EBELN C_I C_EQ '' ''.
*      R_FLIEF[] = S_FLIEF[].
    WHEN P_R2. "전체
      _CLEAR R_EBELN.
*      R_FLIEF[] = S_FLIEF[].
    WHEN P_R3. "생성완료
      _RANGES R_EBELN C_I C_NE '' ''.
*      R_FLIEF[] = S_FLIEF[].
    WHEN P_R4.
*      _RANGES R_FLIEF C_I C_EQ '' ''.
  ENDCASE.

*Clear
  _CLEAR : GT_DISP_100.

*텍스트 Clear
  _CLEAR : GT_T024E, GT_T001W, GT_T024, GT_LFA1, GT_LFM1,
           GT_T134T, GT_T023T, GT_MOQ, GT_MAKT, GT_KONP.

*Main Data  발췌
  SELECT A~BANFN,  "구매요청
         A~BNFPO,  "품목
         A~EKORG,  "구매조직
         A~EKGRP,  "구매그룹
         A~WERKS,  "플랜트
         A~WAERS,  "통화
         A~FLIEF AS LIFNR,  "구매처
         B~MTART,  "자재유형
         B~MATKL,  "자재그룹
         B~MATNR,  "자재코드
         A~EBELN,  "구매오더
         A~EBELP,  "구매오더품목
         A~MENGE,  "구매요청수량
         A~MEINS,  "단위
         A~BADAT,  "구매요청일
         A~LFDAT,  "납품요청일
         C~ZZMBMSTAT, "BM사양상태
         C~ZZMBMTEXT, "BM사양상태 내역
         C~ZZMDNSTAT,  "DN사양상태
         C~ZZMDNTEXT, "DN사양상태 내역
         C~ZZMATNR3 AS MATNR2, "금형코드
         D~MAKTG,     "자재내역
*         C~WEBAZ,
         A~WEBAZ,        "EBAN. 구매요청의 데이터로 표시.
*         C~PLIFZ,
*         E~APLFZ AS PLIFZ,
         A~PLIFZ,           "EBAN. 구매요청의 데이터로 표시.
         A~BSAKZ,
         B~BISMT,
         B~FERTH,
         CASE WHEN B~MTART = 'PMIQ'
              THEN '0051230111'
              END AS SAKTO,
         CASE WHEN B~MTART = 'PMIQ'
              THEN 'C15760'
              END AS KOSTL
    INTO CORRESPONDING FIELDS OF TABLE @GT_DISP_100
    FROM EBAN AS A INNER JOIN MARA AS B
                         ON A~MATNR = B~MATNR
                   INNER JOIN MARC AS C
                         ON C~MATNR = A~MATNR
                        AND C~WERKS = A~WERKS
                   INNER JOIN MAKT AS D
                         ON D~MATNR = C~MATNR
                        AND D~SPRAS = @SY-LANGU
*                   LEFT JOIN EINE AS E
*                         ON E~INFNR = A~INFNR
   WHERE A~WERKS IN @S_WERKS
     AND A~EKORG IN @S_EKORG
     AND A~EKGRP IN @S_EKGRP
     AND A~FLIEF IN @S_FLIEF
     AND A~EBELN IN @R_EBELN
     AND A~LOEKZ =  @SPACE        "20190426. 추가 .
     AND A~EBAKZ = @SPACE         "20191105. 추가.
     AND B~MTART IN @S_MTART
     AND B~MATKL IN @S_MATKL
     AND B~MATNR IN @S_MATNR
     AND D~MAKTG IN @S_MAKTG
     AND C~ZZMATNR3 IN @S_MATNR2
     AND A~LFDAT IN @S_LFDAT
     AND A~BADAT IN @S_BADAT
     AND A~BANFN IN @S_BANFN
     AND A~EBELN IN @S_EBELN.
  IF SY-SUBRC NE 0.
    MESSAGE S001.
    LEAVE LIST-PROCESSING.
  ENDIF.

*구매단가 및 텍스트를 가져오기위한 Itab생성
  LOOP AT GT_DISP_100.

    IF GT_DISP_100-MATNR IS NOT INITIAL AND
       GT_DISP_100-LIFNR IS NOT  INITIAL AND
       GT_DISP_100-EKORG IS NOT INITIAL AND
       GT_DISP_100-WERKS IS NOT  INITIAL.
      LT_MOQ-MATNR = GT_DISP_100-MATNR.
      LT_MOQ-LIFNR = GT_DISP_100-LIFNR.
      LT_MOQ-EKORG = GT_DISP_100-EKORG.
      LT_MOQ-WERKS = GT_DISP_100-WERKS.
      _COLLECT LT_MOQ.
    ENDIF.

    IF GT_DISP_100-MATNR2 IS NOT INITIAL AND
       GT_DISP_100-LIFNR IS NOT  INITIAL AND
       GT_DISP_100-EKORG IS NOT INITIAL AND
       GT_DISP_100-WERKS IS NOT  INITIAL.
      LT_MOQ-MATNR = GT_DISP_100-MATNR2.
      LT_MOQ-LIFNR = GT_DISP_100-LIFNR.
      LT_MOQ-EKORG = GT_DISP_100-EKORG.
      LT_MOQ-WERKS = GT_DISP_100-WERKS.
      _COLLECT LT_MOQ.
    ENDIF.

    IF GT_DISP_100-MATNR IS NOT INITIAL.
      LT_MATNR-MATNR = GT_DISP_100-MATNR.
      _COLLECT LT_MATNR.
    ENDIF.

    IF GT_DISP_100-MATNR2 IS NOT INITIAL.
      LT_MATNR-MATNR = GT_DISP_100-MATNR2.
      _COLLECT LT_MATNR.
    ENDIF.

    IF GT_DISP_100-MTART IS NOT INITIAL.
      LT_MTART-MTART = GT_DISP_100-MTART.
      _COLLECT LT_MTART.
    ENDIF.

    IF GT_DISP_100-MATKL IS NOT INITIAL.
      LT_MATKL-MATKL = GT_DISP_100-MATKL.
      _COLLECT LT_MATKL.
    ENDIF.

    IF GT_DISP_100-LIFNR IS NOT INITIAL.
      LT_LIFNR-LIFNR = GT_DISP_100-LIFNR.
      _COLLECT LT_LIFNR.
    ENDIF.

    IF GT_DISP_100-EKORG IS NOT INITIAL.
      LT_EKORG-EKORG = GT_DISP_100-EKORG.
      _COLLECT LT_EKORG.
    ENDIF.

    IF GT_DISP_100-WERKS IS NOT INITIAL.
      LT_WERKS-WERKS = GT_DISP_100-WERKS.
      _COLLECT LT_WERKS.
    ENDIF.

    IF GT_DISP_100-EKGRP IS NOT INITIAL.
      LT_EKGRP-EKGRP = GT_DISP_100-EKGRP.
      _COLLECT LT_EKGRP.
    ENDIF.

    IF GT_DISP_100-EKORG IS NOT INITIAL AND
       GT_DISP_100-LIFNR IS NOT INITIAL.
      LT_LFM1-LIFNR = GT_DISP_100-LIFNR.
      LT_LFM1-EKORG = GT_DISP_100-EKORG.
      _COLLECT LT_LFM1.
    ENDIF.

    IF GT_DISP_100-MATNR IS NOT INITIAL.
      LT_MATNR_RES-MATNR = GT_DISP_100-MATNR.
      _COLLECT LT_MATNR_RES.
    ENDIF.

  ENDLOOP.

  _CLEAR GT_LABST_RES.
  IF LT_MATNR_RES[] IS NOT INITIAL.
    LOOP AT LT_MATNR_RES.
*미착po합 구하기 20190930
      SELECT A~MATNR, SUM( B~MENGE - B~WEMNG ) AS MENGE
        FROM EKPO AS A
        JOIN EKET AS B
          ON A~EBELN = B~EBELN
         AND A~EBELP = B~EBELP
       WHERE A~MATNR EQ @LT_MATNR_RES-MATNR
         AND A~LOEKZ EQ @SPACE
         AND A~ELIKZ EQ @SPACE
        GROUP BY A~MATNR
        APPENDING CORRESPONDING FIELDS OF TABLE @GT_LABST_RES.

    ENDLOOP.
    IF SY-SUBRC EQ 0.
      SORT GT_LABST_RES BY MATNR.
    ENDIF.
  ENDIF.




*구매
  IF LT_LFM1[] IS NOT   INITIAL.
    SELECT EKORG, LIFNR,
*-S 구매 오더통화 추가 20191007 by PWC096
           WAERS
*-E
      FROM LFM1
      INTO CORRESPONDING FIELDS OF TABLE @GT_LFM1
       FOR ALL ENTRIES IN @LT_LFM1
     WHERE EKORG = @LT_LFM1-EKORG
       AND LIFNR = @LT_LFM1-LIFNR.
    IF SY-SUBRC EQ 0.
      SORT GT_LFM1 BY EKORG LIFNR.
    ENDIF.

  ENDIF.


*금형코드 자재내역
  IF LT_MATNR[] IS NOT INITIAL.
    SELECT *
      FROM MAKT
      INTO TABLE GT_MAKT
        FOR ALL ENTRIES IN LT_MATNR
     WHERE MATNR EQ LT_MATNR-MATNR
       AND SPRAS EQ SY-LANGU.
    IF SY-SUBRC EQ 0.
      SORT GT_MAKT BY MATNR.
    ENDIF.
  ENDIF.

  IF LT_MOQ[] IS NOT INITIAL.
*자재 및 금형코드의 MOQ 최소오더수량
    SELECT A~MATNR "자재코드
           A~LIFNR "구매처
           A~MEINS
           B~EKORG "구매조직
           B~WERKS "플랜트
           B~MINBM "최소오더수량
           B~BPRME "단위
           A~INFNR
      INTO CORRESPONDING FIELDS OF TABLE GT_MOQ
      FROM EINA AS A INNER JOIN EINE AS B
                        ON A~INFNR = B~INFNR
      FOR ALL ENTRIES IN LT_MOQ
     WHERE A~MATNR = LT_MOQ-MATNR
       AND A~LIFNR = LT_MOQ-LIFNR
       AND A~LOEKZ = SPACE
       AND B~EKORG = LT_MOQ-EKORG
       AND B~ESOKZ = C_0
       AND B~WERKS = LT_MOQ-WERKS
       AND B~LOEKZ = SPACE.
    IF SY-SUBRC EQ 0.
      SORT GT_MOQ BY MATNR LIFNR EKORG WERKS.
    ENDIF.

*구매단가
    SELECT A~MATNR
           A~LIFNR
           A~EKORG
           A~WERKS
           B~KBETR
           B~KONWA
      INTO CORRESPONDING FIELDS OF TABLE GT_KONP
      FROM A017 AS A INNER JOIN KONP AS B
                             ON A~KNUMH = B~KNUMH
                            AND A~KSCHL = B~KSCHL
      FOR ALL ENTRIES IN LT_MOQ
     WHERE A~KAPPL EQ C_M
       AND A~KSCHL EQ C_KSCHL_PB00
       AND A~LIFNR EQ LT_MOQ-LIFNR
       AND A~MATNR EQ LT_MOQ-MATNR
       AND A~EKORG EQ LT_MOQ-EKORG
       AND A~WERKS EQ LT_MOQ-WERKS
       AND A~ESOKZ EQ C_0
       AND B~LOEVM_KO EQ SPACE
       AND A~DATAB LE SY-DATLO
       AND A~DATBI GE SY-DATLO.
    IF SY-SUBRC EQ 0.
      SORT GT_KONP BY MATNR LIFNR EKORG WERKS.
    ENDIF.
  ENDIF.


*자재유형 텍스트
  IF LT_MTART[] IS NOT INITIAL.
    SELECT *
      FROM T134T
      INTO CORRESPONDING FIELDS OF TABLE GT_T134T
       FOR ALL ENTRIES IN LT_MTART
     WHERE SPRAS EQ SY-LANGU
       AND MTART EQ LT_MTART-MTART.
    IF SY-SUBRC EQ 0.
      SORT GT_T134T BY MTART.
    ENDIF.
  ENDIF.

*구매조직 텍스트
  IF LT_EKORG[] IS NOT INITIAL.
    SELECT EKORG EKOTX
      FROM T024E
      INTO CORRESPONDING FIELDS OF TABLE GT_T024E
       FOR ALL ENTRIES IN LT_EKORG
     WHERE EKORG EQ LT_EKORG-EKORG.
    IF SY-SUBRC EQ 0.
      SORT GT_T024E BY EKORG.
    ENDIF.
  ENDIF.

*구매처 텍스트
*- 중국어로 LOG ON한 경우에는 공급업체 주소정보를 International version으로 제공.
*  IF SY-LANGU <> '3'.  "한국어.
*      GT_LIFNR[] = LT_LIFNR[].
*      PERFORM GET_LFA1_NAME1_CAM.  "CAM : Central address Management.
*      CLEAR: GT_LIFNR[], GT_LIFNR.

      DATA:  LT_LFA1      LIKE ZMMS0047 OCCURS 0 WITH HEADER LINE.

      CLEAR: GT_LFA1[], GT_LFA1.
      CALL FUNCTION 'ZMM_LFA1_GET_CAM'
*          CALL FUNCTION 'MM_VENDOR_SEARCH'
*            EXPORTING
*              i_string        = LV_superfield   "공급업체 코드.
*            IMPORTING
*              e_vendors_found = l_vendors_found
            TABLES
              T_LIFNR = LT_LIFNR
              T_LFA1  = LT_LFA1.
      LOOP AT LT_LFA1.
          MOVE-CORRESPONDING LT_LFA1 TO GT_LFA1.
          APPEND GT_LFA1.
      ENDLOOP.

*  ELSE.
*      IF LT_LIFNR[] IS NOT INITIAL.
*        SELECT LIFNR NAME1
*          FROM LFA1
*          INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
*           FOR ALL ENTRIES IN LT_LIFNR
*         WHERE LIFNR EQ LT_LIFNR-LIFNR.
*        IF SY-SUBRC EQ 0.
*          SORT GT_LFA1 BY LIFNR.
*        ENDIF.
*      ENDIF.
*
*  ENDIF.


*플랜트 텍스트
  IF LT_WERKS[] IS NOT INITIAL.
    SELECT WERKS NAME1
      FROM T001W
      INTO CORRESPONDING FIELDS OF TABLE GT_T001W
       FOR ALL ENTRIES IN LT_WERKS
     WHERE WERKS EQ LT_WERKS-WERKS.
    IF SY-SUBRC EQ 0.
      SORT GT_T001W BY WERKS.
    ENDIF.
  ENDIF.

*구매그룹 텍스트
  IF LT_EKGRP[] IS NOT INITIAL.
    SELECT EKGRP
           EKNAM
      FROM T024
      INTO CORRESPONDING FIELDS OF TABLE GT_T024
       FOR ALL ENTRIES IN LT_EKGRP
     WHERE EKGRP EQ LT_EKGRP-EKGRP.
    IF SY-SUBRC EQ 0.
      SORT  GT_T024 BY EKGRP.
    ENDIF.
  ENDIF.

*자재그룹 텍스트
  IF LT_MATKL[] IS NOT INITIAL.
    SELECT MATKL
           WGBEZ
      FROM T023T
      INTO CORRESPONDING FIELDS OF TABLE GT_T023T
       FOR ALL ENTRIES IN LT_MATKL
     WHERE SPRAS EQ SY-LANGU
       AND MATKL EQ LT_MATKL-MATKL.
    IF SY-SUBRC EQ 0.
      SORT  GT_T023T BY MATKL.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM MODIFY_DATA .

  DATA : LV_CNT   TYPE I,
         LV_DIV   TYPE I,
         LV_MOD   TYPE I,
         LV_TABIX LIKE SY-TABIX.
  DATA : LV_MINBM TYPE MINBM.

*데이터 조합
  LOOP AT GT_DISP_100.
    CLEAR LV_TABIX. LV_TABIX = SY-TABIX.

    IF P_R1 EQ C_X. "생성예정은 구매조직없는건은 안나오게
      IF GT_DISP_100-EKORG IS INITIAL.
        DELETE GT_DISP_100 INDEX LV_TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.

*구매조직 텍스트
    READ TABLE GT_T024E WITH KEY EKORG = GT_DISP_100-EKORG BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DISP_100-EKORG_TX = GT_T024E-EKOTX.
    ENDIF.

*플랜트 텍스트
    READ TABLE GT_T001W WITH KEY WERKS = GT_DISP_100-WERKS BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DISP_100-WERKS_TX = GT_T001W-NAME1.
    ENDIF.

*구매그룹 텍스트
    READ TABLE GT_T024 WITH KEY EKGRP = GT_DISP_100-EKGRP BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DISP_100-EKGRP_TX = GT_T024-EKNAM.
    ENDIF.

*구매처 텍스트
    READ TABLE GT_LFA1 WITH KEY LIFNR = GT_DISP_100-LIFNR BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DISP_100-LIFNR_TX = GT_LFA1-NAME1.
    ENDIF.

*자재유형 내역
    READ TABLE GT_T134T WITH KEY MTART = GT_DISP_100-MTART BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DISP_100-MTART_TX = GT_T134T-MTBEZ.
    ENDIF.

*자재그룹 내역
    READ TABLE GT_T023T WITH KEY MATKL = GT_DISP_100-MATKL BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DISP_100-MATKL_TX = GT_T023T-WGBEZ.
    ENDIF.

*금형코드 내역
    READ TABLE GT_MAKT WITH KEY MATNR = GT_DISP_100-MATNR2 BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DISP_100-MAKTG2 = GT_MAKT-MAKTG..
    ENDIF.

*자재코드 MOQ 최소구매오더 수량
    READ TABLE GT_MOQ WITH KEY MATNR = GT_DISP_100-MATNR
                               LIFNR = GT_DISP_100-LIFNR
                               EKORG = GT_DISP_100-EKORG
                               WERKS = GT_DISP_100-WERKS BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DISP_100-MINBM = GT_MOQ-MINBM.
*      GT_DISP_100-BPRME = GT_MOQ-BPRME.
      GT_DISP_100-MEINS2 = GT_MOQ-MEINS. "자재코드MOQ의 수량단위
    ENDIF.

*금형코드 MOQ 최소구매오더 수량
    READ TABLE GT_MOQ WITH KEY MATNR = GT_DISP_100-MATNR2
                               LIFNR = GT_DISP_100-LIFNR
                               EKORG = GT_DISP_100-EKORG
                               WERKS = GT_DISP_100-WERKS BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DISP_100-MINBM2 = GT_MOQ-MINBM.
      GT_DISP_100-BPRME = GT_MOQ-BPRME.

    ENDIF.

*구매단가
    READ TABLE GT_KONP WITH KEY MATNR = GT_DISP_100-MATNR
                                LIFNR = GT_DISP_100-LIFNR
                                EKORG = GT_DISP_100-EKORG
                                WERKS = GT_DISP_100-WERKS BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      GT_DISP_100-KBETR = GT_KONP-KBETR.
      GT_DISP_100-KONWA = GT_KONP-KONWA.
    ENDIF.

*구매처 상태
    PERFORM GET_ZZVSTAT_TEXT USING GT_DISP_100-LIFNR GT_DISP_100-EKORG
                          CHANGING GT_DISP_100-ZZVSTAT GT_DISP_100-ZZVSTAT_TX.

*구매처 삭제여부
    PERFORM GET_LFB1 USING GT_DISP_100-WERKS GT_DISP_100-LIFNR
                          CHANGING GT_DISP_100-SPERR GT_DISP_100-LOEVM.

*구매처코드의 국가코드
    PERFORM GET_LAND_FROM_LIFNR USING GT_DISP_100-LIFNR CHANGING GT_DISP_100-LAND1.

*현재가용재고
    PERFORM GET_LABST USING GT_DISP_100 CHANGING GT_DISP_100-LABST.

*상태 및 아이콘
    PERFORM SET_STATUS_ICON CHANGING GT_DISP_100-STATUS GT_DISP_100-ICON.

*원료는 요청수량보다큰 moq의 배수
*부자재는 요청수량보다 작으면 moq 크면 요청수량
    IF GT_DISP_100-STATUS NE C_R.
      IF GT_DISP_100-MINBM GT 0.

        IF GT_DISP_100-MEINS NE GT_DISP_100-MEINS2. "구매요청단위와 MOQ단위가 다르면 환산
          PERFORM SET_CALC_MENGE USING GT_DISP_100
                              CHANGING GT_DISP_100-MEINS GT_DISP_100-MENGE.
        ENDIF.

        IF GT_DISP_100-MTART = 'ROH1'. "원료
          IF GT_DISP_100-MINBM GE GT_DISP_100-MENGE.
            GT_DISP_100-INQTY = GT_DISP_100-MINBM.
          ELSE.
            CLEAR : LV_DIV, LV_MOD.
            LV_DIV = GT_DISP_100-MENGE DIV GT_DISP_100-MINBM.
            LV_MOD = GT_DISP_100-MENGE MOD GT_DISP_100-MINBM.
            IF LV_MOD = 0.
              GT_DISP_100-INQTY = GT_DISP_100-MINBM * LV_DIV.
            ELSE.
              LV_DIV = LV_DIV + 1.
              GT_DISP_100-INQTY = GT_DISP_100-MINBM * LV_DIV.
            ENDIF.
          ENDIF.

        ELSE. "그이외유형 모두
          IF GT_DISP_100-MINBM LT GT_DISP_100-MENGE.
            GT_DISP_100-INQTY = GT_DISP_100-MENGE.
          ELSEIF GT_DISP_100-MINBM GE GT_DISP_100-MENGE.
            GT_DISP_100-INQTY = GT_DISP_100-MINBM.
          ENDIF.
        ENDIF.

*        IF GT_DISP_100-MEINS NE GT_DISP_100-MEINS2. "구매요청단위와 MOQ단위가 다르면 환산
*          PERFORM GET_CALC_KPEIN USING GT_DISP_100-MINBM GT_DISP_100-MEINS2
*                                       GT_DISP_100-MEINS GT_DISP_100-MATNR
*                              CHANGING LV_MINBM.
*        ELSE.
*          LV_MINBM = GT_DISP_100-MINBM.
*        ENDIF.
*
*        IF GT_DISP_100-MTART = 'ROH1'. "원료
*          IF LV_MINBM GE GT_DISP_100-MENGE.
*            GT_DISP_100-INQTY = LV_MINBM.
*          ELSE.
*            CLEAR : LV_DIV, LV_MOD.
*            LV_DIV = GT_DISP_100-MENGE DIV LV_MINBM.
*            LV_MOD = GT_DISP_100-MENGE MOD LV_MINBM.
*            IF LV_MOD = 0.
*              GT_DISP_100-INQTY = LV_MINBM * LV_DIV.
*            ELSE.
*              LV_DIV = LV_DIV + 1.
*              GT_DISP_100-INQTY = LV_MINBM * LV_DIV.
*            ENDIF.
*          ENDIF.
*
*        ELSEIF GT_DISP_100-MTART = 'ROH2'. "부자재
*          IF LV_MINBM LT GT_DISP_100-MENGE.
*            GT_DISP_100-INQTY = GT_DISP_100-MENGE.
*          ELSEIF LV_MINBM GE GT_DISP_100-MENGE.
*            GT_DISP_100-INQTY = LV_MINBM.
*          ENDIF.
*        ENDIF.

      ENDIF.
    ENDIF.

    "생성완료건은 구매오더수량
    IF GT_DISP_100-EBELN IS NOT INITIAL.
      SELECT SINGLE MENGE
        FROM EKPO
        INTO GT_DISP_100-INQTY
       WHERE EBELN = GT_DISP_100-EBELN
         AND EBELP = GT_DISP_100-EBELP.
    ENDIF.

    MODIFY GT_DISP_100 INDEX LV_TABIX.
  ENDLOOP.


  IF GT_DISP_100[] IS INITIAL.
    MESSAGE S001.
    LEAVE LIST-PROCESSING.
  ENDIF.


  SORT GT_DISP_100 BY EKORG EKGRP LIFNR WERKS LFDAT BADAT BANFN BNFPO.

  LV_CNT = LINES( GT_DISP_100[] ).

  MESSAGE S002 WITH LV_CNT.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form END_OF_SELECTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM END_OF_SELECTION .

  CALL SCREEN '0100'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_OBJECT_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_OBJECT_100 .

  " Create docking
  PERFORM CREATE_DOCKING CHANGING GO_100-O_DOCK.


  "SPLIT & ASSIGN CONTAINER
  PERFORM SPLIT_N_ASSIGN_CONTAINER USING GO_100-O_DOCK.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_DOCKING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GO_100_O_DOCK
*&---------------------------------------------------------------------*
FORM CREATE_DOCKING  CHANGING PO_DOCK TYPE REF TO CL_GUI_DOCKING_CONTAINER.

  CHECK PO_DOCK IS NOT BOUND.

  CREATE OBJECT PO_DOCK
    EXPORTING
      REPID                       = SY-REPID
      DYNNR                       = SY-DYNNR
      PARENT                      = CL_GUI_CONTAINER=>SCREEN0
      SIDE                        = CL_GUI_DOCKING_CONTAINER=>DOCK_AT_LEFT
      EXTENSION                   = 2000
      CAPTION                     = 'Revision validity date early warning'
*     ratio                       = 100
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SPLIT_N_ASSIGN_CONTAINER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GO_100_O_DOCK
*&---------------------------------------------------------------------*
FORM SPLIT_N_ASSIGN_CONTAINER  USING
                            PO_DOCK TYPE REF TO CL_GUI_DOCKING_CONTAINER.

* GRID
  CREATE OBJECT GO_100-O_GRID
    EXPORTING
      I_PARENT      = PO_DOCK
      I_APPL_EVENTS = C_X.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_ATTRIBUTES_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_100 .

* Tool Bar ## ##
  PERFORM EXCLUDE_TOOLBAR_100.

* Field Catalog ##
  PERFORM SET_FIELD_CATALOG_100.

* SORT
*  PERFORM SORT_TAB_100.

* SET F4
*  PERFORM SET_F4_100.

*SET LAYOUT
  PERFORM SET_LAYOUT_100.

*Style color
  PERFORM SET_STYLE_COLOR_100.

* Edit Event # Event Handler ##
  PERFORM EVENT_HANDLER_REGISTER_100.


* ALV Grid Display
  PERFORM ALV_GRID_DISPLAY_100 TABLES GT_DISP_100
                                USING GO_100.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCLUDE_TOOLBAR_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM EXCLUDE_TOOLBAR_100 .

  REFRESH GO_100-T_EXCLUDE.

* ### ## ##
  PERFORM ADD_EXCLUDE_TOOLBAR_BUTTON
          TABLES  GO_100-T_EXCLUDE
          USING : CL_GUI_ALV_GRID=>MC_FC_DETAIL,   " DETAIL VIEW
                  CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO, " ####&LOCAL&UNDO
                  CL_GUI_ALV_GRID=>MC_FC_AUF,      " #### &AUF
                  CL_GUI_ALV_GRID=>MC_FC_AVERAGE,  " &AVERAGE
                  CL_GUI_ALV_GRID=>MC_FC_GRAPH,
                  CL_GUI_ALV_GRID=>MC_FC_HELP,
                  CL_GUI_ALV_GRID=>MC_FC_INFO,
                  CL_GUI_ALV_GRID=>MC_FC_CHECK,
                  CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,          " # ##.
                  CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,      " # ##.
                  CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,           " ##.
                  CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,    " ###.
                  CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,    " ###.
                  CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,    " ####.
                  CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,         " ####.
                  CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW, " ####.
                  CL_GUI_ALV_GRID=>MC_FC_REFRESH,
                  CL_GUI_ALV_GRID=>MC_FC_REPREP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_EXCLUDE_TOOLBAR_BUTTON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GO_100_T_EXCLUDE
*&      --> CL_GUI_ALV_GRID=>MC_FC_DETAIL
*&---------------------------------------------------------------------*
FORM ADD_EXCLUDE_TOOLBAR_BUTTON  TABLES   PT_TABLE
                                 USING    PV_VALUE.

  DATA: LV_EXCLUDE TYPE UI_FUNC.

  LV_EXCLUDE = PV_VALUE.
  APPEND LV_EXCLUDE TO PT_TABLE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_DISPLAY_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DISP_100
*&      --> GO_100
*&---------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY_100  TABLES PT_DISP
                            USING PO_ALV TYPE TY_ALV.

  DATA : LV_TABLE(30).
  FIELD-SYMBOLS : <FS_TABLE> TYPE ANY TABLE.

  ASSIGN PT_DISP[] TO <FS_TABLE>.

* Activity Create GRID
  CALL METHOD PO_ALV-O_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = PO_ALV-S_LAYOUT
      IT_TOOLBAR_EXCLUDING = PO_ALV-T_EXCLUDE
      I_SAVE               = C_A
      IS_VARIANT           = PO_ALV-S_VARIANT  " ## ## display
      I_DEFAULT            = C_X
    CHANGING
      IT_OUTTAB            = <FS_TABLE>[]
      IT_FIELDCATALOG      = PO_ALV-T_FIELDCAT[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELD_CATALOG_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_FIELD_CATALOG_100 .

  REFRESH GO_100-T_FIELDCAT.
  PERFORM FILL_FIELD_CATALOGS
  USING:
        C_S 'FIELDNAME' 'ICON',      " '' 'KEY'  C_X,
        C_E 'COLTEXT'   TEXT-F12,
        C_S 'FIELDNAME' 'ERMSG',
        C_E 'COLTEXT'   TEXT-F01,
        C_S 'FIELDNAME' 'MSG',
        C_E 'COLTEXT'   TEXT-F37,
        C_S 'FIELDNAME' 'BANFN',
        C_E 'COLTEXT'   TEXT-F02,
        C_S 'FIELDNAME' 'BNFPO',
        C_E 'COLTEXT'   TEXT-F03,
        C_S 'FIELDNAME' 'EKORG',
        C_E 'COLTEXT'   TEXT-F04,
        C_S 'FIELDNAME' 'EKORG_TX',
        C_E 'COLTEXT'   TEXT-F05,
        C_S 'FIELDNAME' 'EKGRP',
        C_E 'COLTEXT'   TEXT-F06,
        C_S 'FIELDNAME' 'EKGRP_TX',
        C_E 'COLTEXT'   TEXT-F07,

        C_S 'FIELDNAME' 'WERKS',
        C_E 'COLTEXT'   TEXT-F15,
        C_S 'FIELDNAME' 'WERKS_TX',
        C_E 'COLTEXT'   TEXT-F16,

        C_S 'FIELDNAME' 'LIFNR',
        C_E 'COLTEXT'   TEXT-F08,

        C_S 'FIELDNAME' 'LAND1',
        C_E 'COLTEXT'   TEXT-F39,
        C_S 'FIELDNAME' 'LIFNR_TX',
        C_E 'COLTEXT'   TEXT-F09,

        C_S 'FIELDNAME' 'MTART',
        C_E 'COLTEXT'   TEXT-F10,
        C_S 'FIELDNAME' 'MTART_TX',
        C_E 'COLTEXT'   TEXT-F13,

        C_S 'FIELDNAME' 'MATKL',
        C_E 'COLTEXT'   TEXT-F17,
        C_S 'FIELDNAME' 'MATKL_TX',
        C_E 'COLTEXT'   TEXT-F18,

        C_S 'FIELDNAME' 'MATNR',
        C_E 'COLTEXT'   TEXT-F11,

        C_S 'FIELDNAME' 'BISMT',
        C_E 'COLTEXT'   TEXT-F46,
        C_S 'FIELDNAME' 'FERTH',
        C_E 'COLTEXT'   TEXT-F47,

        C_S 'FIELDNAME' 'MAKTG',
        C_E 'COLTEXT'   TEXT-F14,

        C_S 'FIELDNAME' 'EBELN',
        C_E 'COLTEXT'   TEXT-F19,
        C_S 'FIELDNAME' 'EBELP',
        C_E 'COLTEXT'   TEXT-F20,

        C_S 'FIELDNAME' 'MINBM2',   "최소오더수량(금형코드)
        C_E 'COLTEXT'   TEXT-F34,

        C_S 'FIELDNAME' 'MINBM',    "최소오더수량(자재코드)
        C_E 'COLTEXT'   TEXT-F21,

        C_S 'FIELDNAME' 'LABST',
        C_E 'COLTEXT'   TEXT-F45,

*
        C_S 'FIELDNAME' 'MEINS_LABST',
        C_E 'COLTEXT'   TEXT-F51,
*

        C_S 'FIELDNAME' 'LABST_2300',
        C_E 'COLTEXT'   TEXT-F48,

        C_S 'FIELDNAME' 'LABST_RES',
        C_E 'COLTEXT'   TEXT-F49,

        C_S 'FIELDNAME' 'LABST_SUM',
        C_E 'COLTEXT'   TEXT-F50,

        C_S 'FIELDNAME' 'MEINS2',
        C_E 'COLTEXT'   TEXT-F42,   "최소오더단위(자재코드)

        C_S 'FIELDNAME' 'BPRME',    "최소오더단위(금형코드)
        C_E 'COLTEXT'   TEXT-F36,

        C_S 'FIELDNAME' 'MENGE',
        C_E 'COLTEXT'   TEXT-F22,

        C_S 'FIELDNAME' 'MEINS',
        C_E 'COLTEXT'   TEXT-F35,

        C_S 'FIELDNAME' 'INQTY',
        C_E 'COLTEXT'   TEXT-F23,

        C_S 'FIELDNAME' 'SAKTO',
        C_E 'COLTEXT'   TEXT-F43,
        C_S 'FIELDNAME' 'KOSTL',
        C_E 'COLTEXT'   TEXT-F44,

        C_S 'FIELDNAME' 'BADAT',
        C_E 'COLTEXT'   TEXT-F24,

        C_S 'FIELDNAME' 'LFDAT',
        C_E 'COLTEXT'   TEXT-F25,

        C_S 'FIELDNAME' 'WEBAZ',
        C_E 'COLTEXT'   TEXT-F40,
        C_S 'FIELDNAME' 'PLIFZ',
        C_E 'COLTEXT'   TEXT-F41,

        C_S 'FIELDNAME' 'KBETR',
        C_E 'COLTEXT'   TEXT-F26,
        C_S 'FIELDNAME' 'KONWA',
        C_E 'COLTEXT'   TEXT-F27,


        C_S 'FIELDNAME' 'ZZVSTAT_TX',
        C_E 'COLTEXT'   TEXT-F38,
        C_S 'FIELDNAME' 'ZZMBMSTAT',
        C_E 'COLTEXT'   TEXT-F28,
        C_S 'FIELDNAME' 'ZZMBMTEXT',
        C_E 'COLTEXT'   TEXT-F29,

        C_S 'FIELDNAME' 'ZZMDNSTAT',
        C_E 'COLTEXT'   TEXT-F30,
        C_S 'FIELDNAME' 'ZZMDNTEXT',
        C_E 'COLTEXT'   TEXT-F31,

        C_S 'FIELDNAME' 'MATNR2',
        C_E 'COLTEXT'   TEXT-F32,
        C_S 'FIELDNAME' 'MAKTG2',
        C_E 'COLTEXT'   TEXT-F33.

*
*        C_S 'FIELDNAME' 'BPRME',
*        C_E 'COLTEXT'   TEXT-F36.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_FIELD_CATALOGS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_S
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM FILL_FIELD_CATALOGS USING PV_GUBUN PV_FNAME PV_VALUE.

* gc_s -> Structure# ## ## # Setting# Start ##
* gc_e -> Structure# ### ## #### #### ## ##

  IF PV_GUBUN = C_S.
    CLEAR GS_FIELDCAT.

    GS_FIELDCAT-COL_OPT   = C_X.

    CASE PV_VALUE.
      WHEN 'ICON'. "상태
        GS_FIELDCAT-ICON = C_X.
      WHEN 'ERMSG'. "오류 설명

      WHEN 'BANFN'. "구매요청
*        GS_FIELDCAT-QFIELDNAME = 'MEINS'.
        GS_FIELDCAT-REF_TABLE = 'EBAN'.
        GS_FIELDCAT-REF_FIELD = 'BANFN'.

      WHEN 'BNFPO'. "구매요청품목
        GS_FIELDCAT-REF_TABLE = 'EBAN'.
        GS_FIELDCAT-REF_FIELD = 'BNFPO'.

      WHEN 'EKORG'. "구매조직
        GS_FIELDCAT-REF_TABLE = 'EBAN'.
        GS_FIELDCAT-REF_FIELD = 'EKORG'.

      WHEN 'EKORG_TX'. "구매조직 내역

      WHEN 'EKGRP'. "구매그룹
        GS_FIELDCAT-REF_TABLE = 'EBAN'.
        GS_FIELDCAT-REF_FIELD = 'EKGRP'.

      WHEN 'EKGRP_TX'.  " 구매그룹 내역

      WHEN 'WERKS'.  "플랜트
        GS_FIELDCAT-REF_TABLE = 'EBAN'.
        GS_FIELDCAT-REF_FIELD = 'WERKS'.

      WHEN 'WERKS_TX'. "플랜트내역

      WHEN 'LIFNR'. "구매처
        GS_FIELDCAT-REF_TABLE = 'EBAN'.
        GS_FIELDCAT-REF_FIELD = 'LIFNR'.
      WHEN 'LAND1'.
      WHEN 'LIFNR_TX'. "구매처내역

      WHEN 'MTART'. "자재유형
        GS_FIELDCAT-REF_TABLE = 'MARA'.
        GS_FIELDCAT-REF_FIELD = 'MTART'.

      WHEN 'MTART_TX'. "자재유형내역

      WHEN 'MATKL'. "자재그룹
        GS_FIELDCAT-REF_TABLE = 'MARA'.
        GS_FIELDCAT-REF_FIELD = 'MATKL'.

      WHEN 'MATKL_TX'. "자재그룹내역

      WHEN 'MATNR'. "자재코드
        GS_FIELDCAT-REF_TABLE = 'MARA'.
        GS_FIELDCAT-REF_FIELD = 'MATNR'.

      WHEN 'BISMT'. "기존자재코드
        GS_FIELDCAT-REF_TABLE = 'MARA'.
        GS_FIELDCAT-REF_FIELD = 'BISMT'.

      WHEN 'FERTH'. "생산메모
        GS_FIELDCAT-REF_TABLE = 'MARA'.
        GS_FIELDCAT-REF_FIELD = 'FERTH'.

      WHEN 'MAKTG'. "자재코드 내역

      WHEN 'EBELN'.  "구매오더
        GS_FIELDCAT-REF_TABLE = 'EBAN'.
        GS_FIELDCAT-REF_FIELD = 'EBELN'.

      WHEN 'EBELP'. "품목
        GS_FIELDCAT-REF_TABLE = 'EBAN'.
        GS_FIELDCAT-REF_FIELD = 'EBELP'.

      WHEN 'MINBM'. "자재코드 MOQ
        GS_FIELDCAT-REF_TABLE = 'EINE'.
        GS_FIELDCAT-REF_FIELD = 'MINBM'.
        GS_FIELDCAT-QFIELDNAME = 'MEINS2'.

      WHEN 'MEINS2'. "자재코드 MOQ

      WHEN 'LABST'. "현재가용재고
        GS_FIELDCAT-REF_TABLE = 'MARD'.
        GS_FIELDCAT-REF_FIELD = 'LABST'.
        GS_FIELDCAT-QFIELDNAME = 'MEINS'.

      WHEN 'LABST_2300'. "현재가용재고
        GS_FIELDCAT-REF_TABLE = 'MARD'.
        GS_FIELDCAT-REF_FIELD = 'LABST'.
        GS_FIELDCAT-QFIELDNAME = 'MEINS'.

      WHEN 'LABST_RES'. "미착(PO)합
        GS_FIELDCAT-REF_TABLE = 'MARD'.
        GS_FIELDCAT-REF_FIELD = 'LABST'.
        GS_FIELDCAT-QFIELDNAME = 'MEINS'.

      WHEN 'LABST_SUM'. "
        GS_FIELDCAT-REF_TABLE = 'MARD'.
        GS_FIELDCAT-REF_FIELD = 'LABST'.
        GS_FIELDCAT-QFIELDNAME = 'MEINS'.

      WHEN 'MENGE'. "구매요청 수량
        GS_FIELDCAT-REF_TABLE = 'EBAN'.
        GS_FIELDCAT-REF_FIELD = 'MENGE'.
        GS_FIELDCAT-QFIELDNAME = 'MEINS'.

      WHEN 'MEINS'. "자재코드 MOQ

      WHEN 'INQTY'. "실구매수량 입력값
        GS_FIELDCAT-QFIELDNAME = 'MEINS'.
        GS_FIELDCAT-REF_TABLE = 'EBAN'.
        GS_FIELDCAT-REF_FIELD = 'MENGE'.

      WHEN 'SAKTO'. "계정과목
        GS_FIELDCAT-REF_TABLE = 'SKA1'.
        GS_FIELDCAT-REF_FIELD = 'SAKNR'.

      WHEN 'KOSTL'. "코스트센터
        GS_FIELDCAT-REF_TABLE = 'CSKS'.
        GS_FIELDCAT-REF_FIELD = 'KOSTL'.

      WHEN 'BADAT'. "구매요청일
        GS_FIELDCAT-REF_TABLE = 'EBAN'.
        GS_FIELDCAT-REF_FIELD = 'BADAT'.

      WHEN 'LFDAT'. "납품요청일
        GS_FIELDCAT-REF_TABLE = 'EBAN'.
        GS_FIELDCAT-REF_FIELD = 'LFDAT'.

      WHEN 'WEBAZ'. "입고소요일수
        GS_FIELDCAT-REF_TABLE = 'MARC'.
        GS_FIELDCAT-REF_FIELD = 'WEBAZ'.

      WHEN 'PLIFZ'. "계획
        GS_FIELDCAT-REF_TABLE = 'MARC'.
        GS_FIELDCAT-REF_FIELD = 'PLIFZ'.

      WHEN 'KBETR'. "구매단가
        GS_FIELDCAT-REF_TABLE = 'KONP'.
        GS_FIELDCAT-REF_FIELD = 'KBETR'.
        GS_FIELDCAT-CFIELDNAME = 'KONWA'.

      WHEN 'KONWA'.  "단위
        GS_FIELDCAT-REF_TABLE = 'KONP'.
        GS_FIELDCAT-REF_FIELD = 'KONWA'.

      WHEN 'ZZVSTAT_TX'. "구매처 상태
*        GS_FIELDCAT-REF_TABLE = 'MARC'.
*        GS_FIELDCAT-REF_FIELD = 'ZZMBMSTAT'.

      WHEN 'ZZMBMSTAT'. "BN상태
        GS_FIELDCAT-REF_TABLE = 'MARC'.
        GS_FIELDCAT-REF_FIELD = 'ZZMBMSTAT'.

      WHEN 'ZZMBMTEXT'. "내역

      WHEN 'ZZMDNSTAT'. "DN상태
        GS_FIELDCAT-REF_TABLE = 'MARC'.
        GS_FIELDCAT-REF_FIELD = 'ZZMDNSTAT'.

      WHEN 'ZZMDNTEXT'. "내역

      WHEN 'MATNR2'. "금형코드
        GS_FIELDCAT-REF_TABLE = 'MARA'.
        GS_FIELDCAT-REF_FIELD = 'MATNR'.

      WHEN 'MAKTG2'. "금형코드 내역
        GS_FIELDCAT-REF_TABLE = 'MAKT'.
        GS_FIELDCAT-REF_FIELD = 'MAKTG'.

      WHEN 'MINBM2'. "금형코드 MOQ
        GS_FIELDCAT-REF_TABLE = 'EINE'.
        GS_FIELDCAT-REF_FIELD = 'MINBM'.
        GS_FIELDCAT-QFIELDNAME = 'BPRME'.

      WHEN 'MSG'.    "처리결과 메세지

      WHEN OTHERS.
        GS_FIELDCAT-NO_OUT = C_X.
    ENDCASE.

  ENDIF.

* ### Field ### #### ## #### #### ##
  DATA LV_FNAME(40).
  FIELD-SYMBOLS <LV_FS> TYPE ANY.
  CONCATENATE GV_GSFCAT PV_FNAME INTO LV_FNAME.

  ASSIGN (LV_FNAME) TO <LV_FS>.
  <LV_FS> = PV_VALUE.

  IF PV_GUBUN = C_E.
    APPEND GS_FIELDCAT TO GO_100-T_FIELDCAT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LAYOUT_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_LAYOUT_100 .

* At least field REPORT of this structure has to be filled!
  DATA: LV_GRID_TITLE TYPE TEXT200.

  DATA: LV_REPORT      TYPE REPID.

  CLEAR: LV_REPORT.
  CONCATENATE SY-REPID '_' 'GO_100-O_GRID' INTO LV_REPORT.

  CLEAR GO_100-S_VARIANT.
  GO_100-S_VARIANT-REPORT     = LV_REPORT.
  GO_100-S_VARIANT-USERNAME   = SY-UNAME.
*  GO_100-S_VARIANT-VARIANT    = P_VAR.



  CLEAR GO_100-S_LAYOUT.
  GO_100-S_LAYOUT-SEL_MODE   = C_D.
  GO_100-S_LAYOUT-ZEBRA      = C_X.
*  GO_100-S_LAYOUT-EDIT       = GC_X.
*  GO_100-S_LAYOUT-GRID_TITLE = 'Inventory Comparision Report ( GMES vs WMS ) - SUMMAY'.
  GO_100-S_LAYOUT-CWIDTH_OPT = C_X.
  GO_100-S_LAYOUT-STYLEFNAME = 'STYL'.
*  GO_100-S_LAYOUT-CTAB_FNAME = 'SCOL'.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_HANDLER_REGISTER_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM EVENT_HANDLER_REGISTER_100 .

* Create Event
  CREATE OBJECT GO_EVENT_RECEIVER
    EXPORTING
      IV_OBJECT_TEXT = 'GO_100-O_GRID'.
*
** Event Handler ##
  SET HANDLER : GO_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK   FOR GO_100-O_GRID,
                GO_EVENT_RECEIVER->HANDLE_DATA_CHANGED   FOR GO_100-O_GRID,
                GO_EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED
                                                         FOR GO_100-O_GRID.


  CALL METHOD GO_100-O_GRID->SET_OPTIMIZE_ALL_COLS.

  CALL METHOD GO_100-O_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD GO_100-O_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  CALL METHOD GO_100-O_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_STYLE_COLOR_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_STYLE_COLOR_100 .

  DATA : LV_TABIX LIKE SY-TABIX.
  DATA : LV_GUBUN.

  LOOP AT GT_DISP_100.
    CLEAR LV_TABIX. LV_TABIX = SY-TABIX.
    _CLEAR GT_STYL.
    CLEAR LV_GUBUN.
    IF GT_DISP_100-STATUS = C_Y.
      LV_GUBUN = C_1.
    ELSE.
      LV_GUBUN = C_0.
    ENDIF.

    PERFORM SET_APPLY_STYLE  USING GT_STYL LV_GUBUN 'INQTY'.
    IF GT_DISP_100-MTART = 'PMIQ'.
      PERFORM SET_APPLY_STYLE  USING GT_STYL LV_GUBUN 'SAKTO'.
      PERFORM SET_APPLY_STYLE  USING GT_STYL LV_GUBUN 'KOSTL'.
    ENDIF.

    GT_DISP_100-STYL[] = GT_STYL[].

    MODIFY GT_DISP_100 INDEX LV_TABIX TRANSPORTING STYL.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_APPLY_STYLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_STYL
*&      --> C_1
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_APPLY_STYLE  USING PT_STYL TYPE LVC_T_STYL
                           PV_GUBUN
                           PV_FIELD.

  DATA : LS_STYL TYPE LVC_S_STYL.
  DATA : LT_STYL TYPE LVC_T_STYL.

  LS_STYL-FIELDNAME = PV_FIELD.
  CASE PV_GUBUN.
    WHEN C_1.
      LS_STYL-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    WHEN C_0.
      LS_STYL-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
  ENDCASE.

  INSERT LS_STYL     INTO TABLE PT_STYL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CHANGED_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_CHANGED_DATA .

  DATA: LV_VALID(1),
        LV_REFRESH(1) VALUE 'X'.

  "변경 확인 및 DATA_CHANGED 이벤트 발생
  CALL METHOD GO_100-O_GRID->CHECK_CHANGED_DATA
    IMPORTING
      E_VALID   = LV_VALID
    CHANGING
      C_REFRESH = LV_REFRESH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_MANDATORY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_MANDATORY .


  CASE C_X.
    WHEN P_R4.
      IF S_BADAT[] IS INITIAL.
        MESSAGE S008 WITH TEXT-M02 DISPLAY LIKE C_E.
        LEAVE LIST-PROCESSING.
      ENDIF.

      IF S_WERKS[] IS INITIAL.
        MESSAGE S008 WITH TEXT-M16 DISPLAY LIKE C_E.
        LEAVE LIST-PROCESSING.
      ENDIF.

      IF S_EKGRP[] IS INITIAL.
        MESSAGE S008 WITH TEXT-M04 DISPLAY LIKE C_E.
        LEAVE LIST-PROCESSING.
      ENDIF.


    WHEN OTHERS.
      IF S_BADAT[] IS INITIAL.
        MESSAGE S008 WITH TEXT-M02 DISPLAY LIKE C_E.
        LEAVE LIST-PROCESSING.
      ENDIF.


      IF S_EKORG[] IS INITIAL.
        MESSAGE S008 WITH TEXT-M14 DISPLAY LIKE C_E.
        LEAVE LIST-PROCESSING.
      ENDIF.


      IF S_EKGRP[] IS INITIAL.
        MESSAGE S008 WITH TEXT-M04 DISPLAY LIKE C_E.
        LEAVE LIST-PROCESSING.
      ENDIF.

  ENDCASE.

*  IF S_LFDAT[] IS INITIAL.
*    MESSAGE S008 WITH TEXT-M01 DISPLAY LIKE C_E.
*    LEAVE LIST-PROCESSING.
*  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LIFNR_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DISP_100_LIFNR2
*&      <-- GT_DISP_100_LIFNR2_TX
*&---------------------------------------------------------------------*
FORM GET_LIFNR_TEXT  USING    PV_LIFNR
                     CHANGING PV_LIFNR_TX.

  CHECK PV_LIFNR IS NOT INITIAL.
  SELECT SINGLE NAME1
    FROM LFA1
    INTO PV_LIFNR_TX
   WHERE LIFNR EQ PV_LIFNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_PLIFZ_FROM_MARC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DISP_!00_MATNR
*&      --> GT_DISP_100_WERKS
*&      <-- GT_DISP_100_PLIFZ
*&---------------------------------------------------------------------*
FORM GET_PLIFZ_FROM_MARC  USING    PV_MATNR
                                   PV_WERKS
                          CHANGING PV_PLIFZ.

  CHECK PV_MATNR IS NOT INITIAL AND
        PV_WERKS IS NOT INITIAL.

  SELECT SINGLE PLIFZ
    FROM MARC
    INTO PV_PLIFZ
   WHERE MATNR EQ PV_MATNR
     AND WERKS EQ PV_WERKS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_EKGRP_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_EINE_EKGRP
*&      <-- GT_DISP_100_EKGRP_TX
*&---------------------------------------------------------------------*
FORM GET_EKGRP_TEXT  USING    PV_EKGRP
                     CHANGING PV_EKGRP_TX.

  CHECK PV_EKGRP IS NOT INITIAL.

  SELECT SINGLE EKNAM
    FROM T024
    INTO PV_EKGRP_TX
   WHERE EKGRP EQ PV_EKGRP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_GABAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DELETE_GABAGE .

  DATA : LT_0020      LIKE TABLE OF ZMMT0020 WITH HEADER LINE,
         LT_DELE_0020 LIKE TABLE OF ZMMT0020 WITH HEADER LINE,
         LT_KONP      LIKE TABLE OF ZMMT0020 WITH HEADER LINE.

  SELECT *
    FROM ZMMT0020
    INTO TABLE LT_0020.
  CHECK SY-SUBRC EQ 0.

  SELECT KNUMH KOPOS
    INTO CORRESPONDING FIELDS OF TABLE LT_KONP
    FROM KONP
     FOR ALL ENTRIES IN LT_0020
   WHERE KNUMH EQ LT_0020-KNUMH
     AND KOPOS EQ LT_0020-KOPOS.

  CHECK SY-SUBRC EQ 0.

  SORT LT_KONP BY KNUMH KOPOS.

  LOOP AT LT_0020.

    READ TABLE LT_KONP WITH KEY KNUMH = LT_0020-KNUMH
                                KOPOS = LT_0020-KOPOS BINARY SEARCH.
    IF SY-SUBRC NE 0.
      MOVE-CORRESPONDING LT_0020 TO LT_DELE_0020.
      _APPEND LT_DELE_0020.
    ENDIF.

  ENDLOOP.

  CHECK LT_DELE_0020[] IS NOT INITIAL.

  DELETE ZMMT0020 FROM TABLE LT_DELE_0020.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_STATUS_ICON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_DISP_100_STATUS
*&      <-- GT_DISP_100_ICON
*&---------------------------------------------------------------------*
FORM SET_STATUS_ICON  CHANGING PV_STATUS
                               PV_ICON.

  IF GT_DISP_100-EBELN IS NOT INITIAL.
    GT_DISP_100-STATUS = C_G.
    GT_DISP_100-ICON   = ICON_LED_GREEN.
    EXIT.
  ENDIF.

  IF GT_DISP_100-LIFNR IS INITIAL.
    GT_DISP_100-STATUS = C_R.
    GT_DISP_100-ICON   = ICON_LED_RED.
    GT_DISP_100-ERMSG  = TEXT-M12.
    EXIT.
  ENDIF.

*구매처상태가 SPACE가 아니면 빨강
*20190510 구매처 상태체크제외 LFB1으로 대체
*  IF GT_DISP_100-ZZVSTAT IS NOT INITIAL.
*    GT_DISP_100-STATUS = C_R.
*    GT_DISP_100-ICON   = ICON_LED_RED.
*    GT_DISP_100-ERMSG  = TEXT-M11.
*    EXIT.
*  ENDIF.
  IF GT_DISP_100-SPERR = C_X OR
     GT_DISP_100-LOEVM = C_X.
    GT_DISP_100-STATUS = C_R.
    GT_DISP_100-ICON   = ICON_LED_RED.
    GT_DISP_100-ERMSG  = TEXT-M15.
    EXIT.
  ENDIF.


**단가가없거나 1원 BM / DN 사양상태가 X이면 빨강
*BM사양상태 체크
  IF GT_DISP_100-ZZMBMSTAT IS NOT INITIAL.
    GT_DISP_100-STATUS = C_R.
    GT_DISP_100-ICON   = ICON_LED_RED.
    GT_DISP_100-ERMSG  = TEXT-M06.
    EXIT.
  ENDIF.

*BM사양상태 체크
  IF GT_DISP_100-ZZMDNSTAT IS NOT INITIAL.
    GT_DISP_100-STATUS = C_R.
    GT_DISP_100-ICON   = ICON_LED_RED.
    GT_DISP_100-ERMSG  = TEXT-M07.
    EXIT.
  ENDIF.

*단가 체크
  IF GT_DISP_100-KBETR = 0 OR GT_DISP_100-KBETR = '0.01'.
    GT_DISP_100-STATUS = C_R.
    GT_DISP_100-ICON   = ICON_LED_RED.
    GT_DISP_100-ERMSG  = TEXT-M08.
    EXIT.
  ENDIF.

  CLEAR GT_LFM1.
  READ TABLE GT_LFM1 WITH KEY EKORG = GT_DISP_100-EKORG
                              LIFNR = GT_DISP_100-LIFNR BINARY SEARCH.
  IF SY-SUBRC NE 0.
    GT_DISP_100-STATUS = C_R.
    GT_DISP_100-ICON   = ICON_LED_RED.
    GT_DISP_100-ERMSG  = TEXT-M13.
    EXIT.
  ENDIF.


**구매오더생성시 소스리스트와 구매정보레코드를 join으로
**읽어서 없으면 에러 20190716

  IF GT_DISP_100-LIFNR IS NOT INITIAL AND
     GT_DISP_100-EKORG IS NOT INITIAL AND
     GT_DISP_100-WERKS IS NOT INITIAL AND
     GT_DISP_100-MATNR IS NOT INITIAL AND
     GT_DISP_100-BSAKZ IS INITIAL.

    SELECT COUNT(*)
      FROM EINA AS A
      JOIN EINE AS B
        ON A~INFNR = B~INFNR
      JOIN EORD AS C
        ON A~MATNR = C~MATNR
       AND B~WERKS = C~WERKS
     WHERE A~MATNR = @GT_DISP_100-MATNR
       AND A~LIFNR = @GT_DISP_100-LIFNR
       AND A~LOEKZ = @SPACE
       AND B~EKORG = @GT_DISP_100-EKORG
       AND B~ESOKZ = '0'
       AND B~WERKS = @GT_DISP_100-WERKS
       AND B~LOEKZ = @SPACE.
    IF SY-SUBRC NE 0.
      GT_DISP_100-STATUS = C_R.
      GT_DISP_100-ICON   = ICON_LED_RED.
      GT_DISP_100-ERMSG  = TEXT-M17.
      EXIT.
    ENDIF.
  ENDIF.

**자재마스터의 표준원가가 없으면 에러 20190716
*자재유형PMIQ를 제외하고 MBEW에 없으면 에러
  IF GT_DISP_100-WERKS   IS NOT INITIAL AND
     GT_DISP_100-MATNR   IS NOT INITIAL AND
     GT_DISP_100-BSAKZ IS INITIAL.
    SELECT COUNT(*)
      FROM MARA
     WHERE MATNR EQ @GT_DISP_100-MATNR
       AND MTART EQ 'PMIQ'.
    IF SY-SUBRC NE 0.
      SELECT COUNT(*)
        FROM MBEW
       WHERE MATNR EQ @GT_DISP_100-MATNR
         AND BWKEY EQ @GT_DISP_100-WERKS.
      IF SY-SUBRC NE 0.
        GT_DISP_100-STATUS = C_R.
        GT_DISP_100-ICON   = ICON_LED_RED.
        GT_DISP_100-ERMSG  = TEXT-M18.
        EXIT.
      ENDIF.
    ENDIF.

*구매그룹이 9로시작하지않고 표준원가가 없으면 에러
    SELECT SINGLE EKGRP
      INTO @DATA(LV_EKGRP)
      FROM MARC
     WHERE MATNR EQ @GT_DISP_100-MATNR
       AND WERKS EQ @GT_DISP_100-WERKS.
    IF LV_EKGRP+0(1) NE '9'.
      SELECT SINGLE *
        INTO @DATA(LS_MBEW)
        FROM MBEW
       WHERE MATNR EQ @GT_DISP_100-MATNR
         AND BWKEY EQ @GT_DISP_100-WERKS
         AND VPRSV EQ 'S'.
      IF SY-SUBRC EQ 0.
        IF LS_MBEW-PPRDL IS INITIAL AND
           LS_MBEW-PDATL IS INITIAL AND
           LS_MBEW-LPLPR LE 0.
          GT_DISP_100-STATUS = C_R.
          GT_DISP_100-ICON   = ICON_LED_RED.
          GT_DISP_100-ERMSG  = TEXT-M19.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*구매요청있고 구매오더없으면 노랑
  IF GT_DISP_100-EBELN IS INITIAL.
    GT_DISP_100-STATUS = C_Y.
    GT_DISP_100-ICON   = ICON_LED_YELLOW.
    EXIT.
  ENDIF.

*구매요청 구매오더 다있으면 파랑
  IF GT_DISP_100-EBELN IS NOT INITIAL.
    GT_DISP_100-STATUS = C_G.
    GT_DISP_100-ICON   = ICON_LED_GREEN.
    EXIT.
  ENDIF.

*-S 구매오더통화와 구매단가의 통화비교 추가 20191007 by PWC096
  IF GT_LFM1-WAERS <> GT_DISP_100-KONWA.
    GT_DISP_100-STATUS = C_R.
    GT_DISP_100-ICON   = ICON_LED_RED.
    GT_DISP_100-ERMSG  = TEXT-M20.
    EXIT.
  ENDIF.
*-E

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PORC_BTN1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PORC_BTN1 .

  DATA : LV_CNT TYPE I.
  DATA : LV_ANSWER,
         LV_ERR,
         LV_MSG(100).
  DATA : LT_DISP_100 TYPE TABLE OF TY_100 WITH HEADER LINE,
         LS_DISP_100 TYPE TY_100.


  DATA : BEGIN OF LT_EKPO OCCURS 0,
           EBELN LIKE EKPO-EBELN,
           EBELP LIKE EKPO-EBELP,
           BANFN LIKE EKPO-BANFN,
           BNFPO LIKE EKPO-BNFPO,
         END OF LT_EKPO.

  PERFORM CHECK_CHANGED_DATA.


  _CLEAR GT_ROW.
  CALL METHOD GO_100-O_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROW.

  LV_CNT = LINES( GT_ROW ).

  IF LV_CNT EQ 0.
    LOOP AT GT_DISP_100.

      IF GT_DISP_100-STATUS NE C_Y.
        CONTINUE.
      ENDIF.

      IF GT_DISP_100-INQTY = 0.
        CONTINUE.
      ENDIF.

      IF GT_DISP_100-MTART = 'PMIQ'.
        IF GT_DISP_100-SAKTO IS INITIAL OR
           GT_DISP_100-KOSTL IS INITIAL.
          LV_ERR = C_X.
          LV_MSG = TEXT-E04.
          EXIT.
        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING GT_DISP_100 TO LT_DISP_100.
      _APPEND LT_DISP_100.

    ENDLOOP.
  ELSE.
    LOOP AT GT_ROW INTO GS_ROW.
      READ TABLE GT_DISP_100 INDEX GS_ROW-INDEX.
      IF SY-SUBRC = 0.


        IF GT_DISP_100-STATUS NE C_Y.
          LV_ERR = C_X.
          LV_MSG = TEXT-E01.
          EXIT.
        ENDIF.

        IF GT_DISP_100-INQTY = 0.
          LV_ERR = C_X.
          LV_MSG = TEXT-E02.
          EXIT.
        ENDIF.

        IF GT_DISP_100-MTART = 'PMIQ'.
          IF GT_DISP_100-SAKTO IS INITIAL OR
             GT_DISP_100-KOSTL IS INITIAL.
            LV_ERR = C_X.
            LV_MSG = TEXT-E04.
            EXIT.
          ENDIF.
        ENDIF.

        MOVE-CORRESPONDING GT_DISP_100 TO LT_DISP_100.
        _APPEND LT_DISP_100.

      ENDIF.

    ENDLOOP.
  ENDIF.





  IF LV_ERR IS NOT INITIAL.
    MESSAGE S000 WITH LV_MSG DISPLAY LIKE C_E.
    EXIT.
  ENDIF.

  IF LT_DISP_100[] IS INITIAL.
    MESSAGE S000 WITH TEXT-E03 DISPLAY LIKE C_E.
    EXIT.
  ENDIF.

  _CONFIRM TEXT-M09 LV_ANSWER TEXT-T09.

  CHECK LV_ANSWER EQ '1'.

*PO Create

  SORT LT_DISP_100 BY EKGRP EKORG LIFNR WERKS.

  LOOP AT LT_DISP_100.

* Item
    PERFORM SET_PO_ITEM      USING  LT_DISP_100.

    IF LT_DISP_100-MTART = 'PMIQ'.
* Item
      PERFORM SET_PO_ACCOUNT   USING  LT_DISP_100.
    ENDIF.

    CLEAR LS_DISP_100.
    MOVE-CORRESPONDING LT_DISP_100 TO LS_DISP_100.

    AT END OF LIFNR.

* Header
      PERFORM SET_PO_HEADER      USING  LS_DISP_100.

* Header Text
* a. 헤더 텍스트
      PERFORM SET_PO_HTEXT_F01   USING  LS_DISP_100.



* 실행
      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          POHEADER         = GS_POHEADER
          POHEADERX        = GS_POHEADERX
        IMPORTING
          EXPPURCHASEORDER = G_EBELN
        TABLES
          RETURN           = GT_RETURN_PO
          POITEM           = GT_POITEM
          POITEMX          = GT_POITEMX
          POACCOUNT        = GT_POACCOUNT
          POACCOUNTX       = GT_POACCOUNTX.
*          POSCHEDULE        = GT_POSCHEDULE
*          POSCHEDULEX       = GT_POSCHEDULEX
*          POCOND            = GT_POCOND
*          POCONDX           = GT_POCONDX
*          POSERVICES        = GT_POSERVICES
*          POSRVACCESSVALUES = GT_POSRVACCES
*          EXTENSIONIN       = GT_EXTENSIONIN
*          POTEXTHEADER      = GT_POHTEXT.

      IF  G_EBELN  IS  INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        LOOP AT GT_RETURN_PO INTO GS_RETURN_PO WHERE ID   NE 'BAPI'
                                                   AND TYPE =  'E'.
          CHECK GS_RETURN_PO-ID NE 'BAPI'.
          IF GS_RETURN_PO-ID EQ 'MEPO'.
            CHECK GS_RETURN_PO-NUMBER NE '000'  AND
                  GS_RETURN_PO-NUMBER NE '002'.
          ENDIF.
          EXIT.
        ENDLOOP.

        IF GS_RETURN_PO-MESSAGE IS NOT INITIAL.
          LOOP AT GT_POITEM INTO GS_POITEM.
            GT_DISP_100-MSG = GS_RETURN_PO-MESSAGE.
            MODIFY GT_DISP_100 TRANSPORTING MSG WHERE BANFN = GS_POITEM-PREQ_NO
                                                  AND BNFPO = GS_POITEM-PREQ_ITEM.

          ENDLOOP.
        ENDIF.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        LOOP AT GT_RETURN_PO INTO GS_RETURN_PO WHERE TYPE    EQ  'S'
                                                 AND ID      EQ  '06'
                                                 AND NUMBER  EQ  '017'.
        ENDLOOP.

        DO 100 TIMES.
          _CLEAR LT_EKPO.
          SELECT EBELN EBELP BANFN BNFPO
            INTO CORRESPONDING FIELDS OF TABLE LT_EKPO
            FROM EKPO
           WHERE EBELN = G_EBELN.
          IF SY-SUBRC = 0.
            EXIT.
          ELSE.
            COMMIT WORK AND WAIT.
          ENDIF.
        ENDDO.

        LOOP AT LT_EKPO.
          GT_DISP_100-STATUS = C_G.
          GT_DISP_100-ICON   = ICON_LED_GREEN.
          GT_DISP_100-EBELN  = LT_EKPO-EBELN.
          GT_DISP_100-EBELP  = LT_EKPO-EBELP.
          CLEAR GT_DISP_100-MSG.
          MODIFY GT_DISP_100 TRANSPORTING MSG ICON STATUS EBELN EBELP WHERE BANFN = LT_EKPO-BANFN
                                                                                                                          AND BNFPO = LT_EKPO-BNFPO.




        ENDLOOP.


* beg.     -구매요청 잔량이 있는 경우, 구매요청 마감을 처리함. 2019.10.10.
          GT_EKPO[] = LT_EKPO[].
          PERFORM BAPI_PR_UPDATE_CLOSE.
* end.     -구매요청 잔량이 있는 경우, 구매요청 마감을 처리함. 2019.10.10.

       ENDIF.

      PERFORM CLEAR_OBJECT.
    ENDAT.
  ENDLOOP.

  MESSAGE S000 WITH TEXT-M10.

  _SET_LAYOUT GO_100-O_GRID GO_100-S_LAYOUT.

*Refresh
  _REFRESHTAB GO_100-O_GRID SPACE C_X C_X.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_OBJECT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CLEAR_OBJECT .

  CLEAR : GT_POITEM,       GT_POITEM[],        GT_POITEMX,       GT_POITEMX[],
           GT_POACCOUNT,    GT_POACCOUNT[],     GT_POACCOUNTX,    GT_POACCOUNTX[],
           GT_POSCHEDULE,   GT_POSCHEDULE[],    GT_POSCHEDULEX,   GT_POSCHEDULEX[],
           GT_POCOND,       GT_POCOND[],        GT_POCONDX,       GT_POCONDX[],
           GT_POCOMPONENTS, GT_POCOMPONENTS[],  GT_POCOMPONENTSX, GT_POCOMPONENTSX[],
           GT_POSERVICES,   GT_POSERVICES[],    GT_POSRVACCES,    GT_POSRVACCES[],
           GT_EXTENSIONIN,  GT_EXTENSIONIN[],   GT_POHTEXT,       GT_POHTEXT[],
           GT_RETURN_PO,    GT_RETURN_PO[],
           GS_POHEADER,     GS_POHEADERX,       G_EBELN, GV_EBELP.

  CLEAR : GS_RETURN_PO,
          GS_POITEM,       GS_POITEMX,
          GS_POACCOUNT,    GS_POACCOUNTX,
          GS_POSCHEDULE,   GS_POSCHEDULEX,
          GS_POCOND,       GS_POCONDX,
          GS_POCOMPONENTS, GS_POCOMPONENTSX,
          GS_POSERVICES,   GS_POSRVACCES,
          GS_EXTENSIONIN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PO_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DISP_100
*&---------------------------------------------------------------------*
FORM SET_PO_ITEM  USING    PS_DISP_100 TYPE TY_100.


  CLEAR : GS_POITEM, GS_POITEMX.

  IF GV_EBELP IS INITIAL.
    GV_EBELP = 10.
  ELSE.
    GV_EBELP = GV_EBELP + 10.
  ENDIF.

*


  GS_POITEM-PO_ITEM       = GV_EBELP.
*  GS_POITEM-ACCTASSCAT    = PS_DISP_100-KNTTP.  "계정지정범주
  GS_POITEM-PLANT         = PS_DISP_100-WERKS.
*  GS_POITEM-STGE_LOC      = P_ITEM_PO-LGORT.
  GS_POITEM-QUANTITY      = PS_DISP_100-INQTY.
  GS_POITEM-PO_UNIT       = PS_DISP_100-MEINS.
  GS_POITEM-ORDERPR_UN    = PS_DISP_100-MEINS.
*  GS_POITEM-PCKG_NO       = G_PCKGNO .
*  GS_POITEM-INFO_UPD      = ' '.

  IF PS_DISP_100-MTART = 'PMIQ'.
    GS_POITEM-ACCTASSCAT = 'K'.
  ENDIF.

***  gs_poitem-over_dlv_tol  = p_item_po-uebto.
***  GS_POITEM-UNLIMITED_DLV = P_ITEM_PO-UEBTK.
***  GS_POITEM-UNDER_DLV_TOL = P_ITEM_PO-UNTTO.




*  IF P_ITEM_PO-LOEKZ IS NOT INITIAL.
*    GS_POITEM-DELETE_IND  = 'L'.
*  ELSE.
*    GS_POITEM-DELETE_IND  = ''.
*  ENDIF.



* Net Price


*  IF P_ITEM_PO-KNTTP NE 'U'  AND  P_ITEM_PO-PSTYP NE '9'.
*    GS_POITEM-NET_PRICE   = P_ITEM_PO-NETPR.
*  ENDIF.

*  IF P_ITEM_PO-PEINH IS INITIAL.
  GS_POITEM-PRICE_UNIT  = 1.
*  ELSE.
*    GS_POITEM-PRICE_UNIT  = P_ITEM_PO-PEINH.
*  ENDIF.

*  IF P_ITEM_PO-MATNR IS INITIAL.
*    GS_POITEM-SHORT_TEXT  = P_ITEM_PO-TXZ01.
*    GS_POITEM-MATL_GROUP  = P_ITEM_PO-MATKL.
*  ELSE.
  GS_POITEM-MATERIAL    = PS_DISP_100-MATNR.
*  ENDIF.

*  IF P_ITEM_PO-PSTYP IS NOT INITIAL.
  GS_POITEM-ITEM_CAT    = 0.
*    IF P_ITEM_PO-PSTYP EQ '9'.
*      GS_POITEM-SRV_BASED_IV = 'X'.
*    ENDIF.
*  ENDIF.

*  IF P_ITEM_PO-KNTTP IS NOT INITIAL.
*    GS_POITEM-ACCTASSCAT  = P_ITEM_PO-KNTTP.
*  ENDIF.

*  IF P_ITEM_PO-BANFN IS NOT INITIAL.
  GS_POITEM-PREQ_NO     = PS_DISP_100-BANFN.
  GS_POITEM-PREQ_ITEM   = PS_DISP_100-BNFPO.
*  ENDIF.



* 세금 코드
*  IF P_ITEM_PO-MWSKZ IS NOT INITIAL.
*    GS_POITEM-TAX_CODE    = P_ITEM_PO-MWSKZ.
*  ENDIF.

*  IF P_ITEM_PO-WEBRE IS NOT INITIAL.
*    GS_POITEM-GR_BASEDIV  = 'X'.
*  ENDIF.

  APPEND GS_POITEM TO GT_POITEM.


*
*


  GS_POITEMX-PO_ITEM       = GV_EBELP.
  GS_POITEMX-PO_ITEMX      = 'X'.
  GS_POITEMX-PLANT         = 'X'.
*  GS_POITEMX-STGE_LOC      = 'X'.
  GS_POITEMX-QUANTITY      = 'X'.
  GS_POITEMX-PO_UNIT       = 'X'.
*  GS_POITEMX-NET_PRICE     = 'X'.
  GS_POITEMX-PRICE_UNIT    = 'X'.
*  GS_POITEMX-OVER_DLV_TOL  = 'X'.
*  GS_POITEMX-ORDERPR_UN    = 'X'.
*  GS_POITEMX-INFO_UPD      = 'X'.


***  gs_poitemx-over_dlv_tol  = 'X'.
***  GS_POITEMX-UNLIMITED_DLV = 'X'.
***  GS_POITEMX-UNDER_DLV_TOL = 'X'.


  IF PS_DISP_100-MTART = 'PMIQ'.
    GS_POITEMX-ACCTASSCAT = 'X'.
  ENDIF.


*  IF P_ITEM_PO-LOEKZ IS NOT INITIAL.
*    GS_POITEMX-DELETE_IND  = 'X'.
*  ENDIF.

*  IF P_ITEM_PO-MATNR IS INITIAL.
*    GS_POITEMX-SHORT_TEXT  = 'X'.
*    GS_POITEMX-MATL_GROUP  = 'X'.
*  ELSE.
  GS_POITEMX-MATERIAL    = 'X'.
*  ENDIF.

*  IF P_ITEM_PO-PSTYP IS NOT INITIAL.
  GS_POITEMX-ITEM_CAT    = 'X'.
*    IF P_ITEM_PO-PSTYP EQ '9'.
*      GS_POITEMX-SRV_BASED_IV = 'X'.
*    ENDIF.
*  ENDIF.

*  IF P_ITEM_PO-KNTTP IS NOT INITIAL.
*    GS_POITEMX-ACCTASSCAT = 'X'.
*  ENDIF.
*
*  IF GS_POITEM-PCKG_NO IS NOT INITIAL.
*    GS_POITEMX-PCKG_NO    = 'X' .
*  ENDIF.

*  IF P_ITEM_PO-BANFN IS NOT INITIAL.
  GS_POITEMX-PREQ_NO    = 'X' .
  GS_POITEMX-PREQ_ITEM  = 'X' .
*  ENDIF.

*  IF P_ITEM_PO-MWSKZ IS NOT INITIAL.
*    GS_POITEMX-TAX_CODE   = 'X'.
*  ENDIF.
*
*  IF P_ITEM_PO-WEBRE IS NOT INITIAL.
*    GS_POITEMX-GR_BASEDIV = 'X'.
*  ENDIF.

  APPEND GS_POITEMX TO GT_POITEMX.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PO_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP_100
*&---------------------------------------------------------------------*
FORM SET_PO_HEADER  USING   PS_DISP_100 TYPE TY_100.

  DATA : LV_BSART TYPE BSART,
         LV_WAERS TYPE WAERS.


*구매처의 오더통화
  SELECT SINGLE WAERS
    FROM LFM1
    INTO @LV_WAERS
   WHERE EKORG = @PS_DISP_100-EKORG
     AND LIFNR = @PS_DISP_100-LIFNR.

**플랜트와 구매처의 국가코드가 같으면 YNB 다르면 YIM
*구매문서유형
  PERFORM GET_DOC_TYPE USING PS_DISP_100-LIFNR PS_DISP_100-WERKS
                     CHANGING LV_BSART.

*  GS_POHEADER-PO_NUMBER    = P_HEAD_PO-EBELN.
*  GS_POHEADER-COMP_CODE    = P_HEAD_PO-BUKRS.
  GS_POHEADER-DOC_TYPE     = LV_BSART.
  GS_POHEADER-PURCH_ORG    = PS_DISP_100-EKORG.
  GS_POHEADER-PUR_GROUP    = PS_DISP_100-EKGRP.
  GS_POHEADER-VENDOR       = PS_DISP_100-LIFNR.
  GS_POHEADER-CURRENCY     = LV_WAERS.
  GS_POHEADER-DOC_DATE     = SY-DATLO.
*  GS_POHEADER-VPER_START   = P_HEAD_PO-KDATB.
*  GS_POHEADER-VPER_END    = P_HEAD_PO-KDATE.
*  GS_POHEADER-INCOTERMS1   = P_HEAD_PO-INCO1.   "인도조건1
*  GS_POHEADER-INCOTERMS2   = P_HEAD_PO-INCO2.   "인도조건2
*  GS_POHEADER-PMNTTRMS     = P_HEAD_PO-ZTERM.   "지급조건


*
*


*  IF GS_POHEADER-PO_NUMBER IS NOT INITIAL.
*    GS_POHEADERX-PO_NUMBER = 'X'.
*  ENDIF.


*


*  GS_POHEADERX-COMP_CODE   = 'X'.
  GS_POHEADERX-DOC_TYPE    = 'X'.
  GS_POHEADERX-PURCH_ORG   = 'X'.
  GS_POHEADERX-PUR_GROUP   = 'X'.
  GS_POHEADERX-VENDOR      = 'X'.
  GS_POHEADERX-CURRENCY    = 'X'.
  GS_POHEADERX-DOC_DATE    = 'X'.

*  IF GS_POHEADER-VPER_START IS NOT INITIAL.
*    GS_POHEADERX-VPER_START  = 'X'.
*    GS_POHEADERX-VPER_END    = 'X'.
*  ENDIF.

*  IF GS_POHEADER-INCOTERMS1 IS NOT INITIAL.
*    GS_POHEADERX-INCOTERMS1  = 'X'.
*    GS_POHEADERX-INCOTERMS2  = 'X'.
*  ENDIF.
*
*  IF GS_POHEADER-PMNTTRMS IS NOT INITIAL.
*    GS_POHEADERX-PMNTTRMS    = 'X'.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PO_HTEXT_F01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP_100
*&---------------------------------------------------------------------*
FORM SET_PO_HTEXT_F01 USING   PS_DISP_100 TYPE TY_100.


*
*    GT_POHTEXT-PO_NUMBER = PS_HEAD_PO-EBELN.
*    GT_POHTEXT-PO_ITEM   = '00000'.
*    GT_POHTEXT-TEXT_ID   = 'F01'.
*
*    IF SY-TABIX = 1.
*      GT_POHTEXT-TEXT_FORM = '*'.
*    ELSE.
*      GT_POHTEXT-TEXT_FORM = '/'.
*    ENDIF.
*
*    GT_POHTEXT-TEXT_LINE = LT_LINES-TDLINE.
*    APPEND GT_POHTEXT.   CLEAR GT_POHTEXT.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DOC_TYPE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PS_DISP_100_LIFNR
*&      --> PS_DISP_100_WERKS
*&      <-- LV_BSART
*&---------------------------------------------------------------------*
FORM GET_DOC_TYPE  USING    PV_LIFNR
                            PV_WERKS
                   CHANGING PV_BSART.

  DATA : LV_LAND1 TYPE LAND1_GP,
         LV_LAND2 TYPE LAND1_GP.

*구매처의 국가코드
  SELECT SINGLE LAND1
    FROM LFA1
    INTO LV_LAND1
   WHERE LIFNR EQ PV_LIFNR.
  CHECK SY-SUBRC EQ 0.

*플랜트의 국가코드
  SELECT SINGLE LAND1
    FROM T001W
    INTO LV_LAND2
   WHERE WERKS EQ PV_WERKS.
  CHECK SY-SUBRC EQ 0.

  IF LV_LAND1 EQ LV_LAND2.
    PV_BSART = 'YNB'.
  ELSE.
    PV_BSART = 'YIM'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ZZVSTAT_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DISP_100_LIFNR
*&      --> GT_DISP_100_EKORG
*&      <-- GT_DISP_100_ZZVSTAT
*&      <-- GT_DISP_100_ZZVSTAT_TX
*&---------------------------------------------------------------------*
FORM GET_ZZVSTAT_TEXT  USING    PV_LIFNR
                                PV_EKORG
                       CHANGING PV_ZZVSTAT
                                PV_ZZVSTAT_TX.

  SELECT SINGLE ZZVSTAT
    FROM LFM1
    INTO PV_ZZVSTAT
   WHERE LIFNR EQ PV_LIFNR
     AND EKORG EQ PV_EKORG.
  CHECK SY-SUBRC EQ 0.

  PERFORM GET_DOMAIN_VAL USING 'ZZDFSTAT' PV_ZZVSTAT
                                  CHANGING PV_ZZVSTAT_TX.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DOMAIN_VAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> PV_ZZVSTAT
*&      <-- PV_ZZVSTAT_TX
*&---------------------------------------------------------------------*
FORM GET_DOMAIN_VAL  USING P_DOMNAME
                           P_DOMVALUE
                  CHANGING P_OUTTX.

  DATA: L_DOM LIKE DD07V-DOMNAME,
        L_VAL LIKE DD07V-DOMVALUE_L,
        L_TXT LIKE DD07V-DDTEXT.

  MOVE: P_DOMNAME  TO L_DOM,
        P_DOMVALUE TO L_VAL.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      I_DOMNAME  = L_DOM
      I_DOMVALUE = L_VAL
    IMPORTING
      E_DDTEXT   = L_TXT
    EXCEPTIONS
      NOT_EXIST  = 1
      OTHERS     = 2.

  P_OUTTX = L_TXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LAND_FROM_LIFNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DISP_100_LIFNR
*&      <-- GT_DISP_100_LAND1
*&---------------------------------------------------------------------*
FORM GET_LAND_FROM_LIFNR  USING    PV_LIFNR
                          CHANGING PV_LAND1.

  SELECT SINGLE LAND1
    FROM LFA1
    INTO PV_LAND1
   WHERE LIFNR EQ PV_LIFNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CALC_KPEIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DISP_100_MINBM
*&      --> GT_DISP_100_MEINS2
*&      --> GT_DISP_100_MEINS
*&      <-- LV_MINBM
*&---------------------------------------------------------------------*
FORM GET_CALC_KPEIN  USING    PV_MINBM
                              PV_MEINS2
                              PV_MEINS
                              PV_MATNR
                     CHANGING PV_MINBM_OUT.
  DATA :I_MENGE        TYPE  WB2_COMPLETE_QUAN,
        I_MEINS        TYPE  MEINS,
        I_TARGET_MEINS TYPE  MEINS,
        I_UMREZ        TYPE  UMBSZ,
        I_UMREN        TYPE  UMREN,
        I_MATNR        TYPE  MATNR,
        E_CALC_MENGE   TYPE  WB2_COMPLETE_QUAN.

  CLEAR PV_MINBM_OUT.

  I_MENGE = PV_MINBM.
  I_MEINS = PV_MEINS2.
  I_TARGET_MEINS = PV_MEINS.
  I_MATNR = PV_MATNR.

  CALL FUNCTION 'WB2_CALCULATE_QUAN'
    EXPORTING
      I_MENGE          = I_MENGE
      I_MEINS          = I_MEINS
      I_TARGET_MEINS   = I_TARGET_MEINS
*     I_UMREZ          =
*     I_UMREN          =
      I_MATNR          = I_MATNR
    IMPORTING
      E_CALC_MENGE     = E_CALC_MENGE
    EXCEPTIONS
      CONVERSION_ERROR = 1
      OTHERS           = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ELSE.
    PV_MINBM_OUT = E_CALC_MENGE.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_CALC_MENGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DISP_100
*&      <-- GT_DISP_100_MEINS
*&      <-- GT_DISP_100_MENGE
*&---------------------------------------------------------------------*
FORM SET_CALC_MENGE  USING    PS_DISP_100 TYPE TY_100
                     CHANGING PV_MEINS
                              PV_MENGE.

  DATA :I_MENGE        TYPE  WB2_COMPLETE_QUAN,
        I_MEINS        TYPE  MEINS,
        I_TARGET_MEINS TYPE  MEINS,
        I_UMREZ        TYPE  UMBSZ,
        I_UMREN        TYPE  UMREN,
        I_MATNR        TYPE  MATNR,
        E_CALC_MENGE   TYPE  WB2_COMPLETE_QUAN.

  I_MENGE = PV_MENGE.
  I_MEINS = PV_MEINS.
  I_TARGET_MEINS = PS_DISP_100-MEINS2.
  I_MATNR = PS_DISP_100-MATNR.

  CALL FUNCTION 'WB2_CALCULATE_QUAN'
    EXPORTING
      I_MENGE          = I_MENGE
      I_MEINS          = I_MEINS
      I_TARGET_MEINS   = I_TARGET_MEINS
*     I_UMREZ          =
*     I_UMREN          =
      I_MATNR          = I_MATNR
    IMPORTING
      E_CALC_MENGE     = E_CALC_MENGE
    EXCEPTIONS
      CONVERSION_ERROR = 1
      OTHERS           = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ELSE.
    PV_MENGE = E_CALC_MENGE.
    PV_MEINS = PS_DISP_100-MEINS2.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PO_ACCOUNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DISP_100
*&---------------------------------------------------------------------*
FORM SET_PO_ACCOUNT  USING    PS_DISP_100 TYPE TY_100.

  DATA: LT_EBKN  LIKE  TABLE OF EBKN WITH HEADER LINE.


*


  CLEAR: GS_POACCOUNT,  GS_POACCOUNTX.


*

*
*  CHECK NOT P_ITEM_PO-BANFN IS INITIAL  AND
*             P_ITEM_PO-PSTYP EQ '9'.
*
*  SELECT *
*    INTO TABLE LT_EBKN
*    FROM EBKN
*   WHERE BANFN  EQ  P_ITEM_PO-BANFN
*     AND BNFPO  EQ  P_ITEM_PO-BNFPO.


*
  _CONV C_I PS_DISP_100-SAKTO PS_DISP_100-SAKTO.

*  LOOP AT LT_EBKN.
  GS_POACCOUNT-PO_ITEM      = GV_EBELP.
  GS_POACCOUNT-GL_ACCOUNT   = PS_DISP_100-SAKTO.
  GS_POACCOUNT-COSTCENTER   = PS_DISP_100-KOSTL.
  APPEND GS_POACCOUNT TO GT_POACCOUNT.


*
*


  GS_POACCOUNTX-PO_ITEM     = GV_EBELP.

  GS_POACCOUNTX-GL_ACCOUNT  = 'X'.
  GS_POACCOUNTX-COSTCENTER  = 'X'.


  APPEND GS_POACCOUNTX TO GT_POACCOUNTX.
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SELECTION_SCREEN_OUTPUT .

  LOOP AT SCREEN.

    IF SCREEN-NAME = 'S_BADAT-LOW' OR
       SCREEN-NAME = 'S_EKORG-LOW' OR
       SCREEN-NAME = 'S_EKGRP-LOW'.
      SCREEN-REQUIRED = '2'.
    ENDIF.


    IF SCREEN-NAME = 'S2_EKGRP-LOW' OR
*        SCREEN-NAME = 'S2_MTART-LOW' OR
        SCREEN-NAME = 'S2_EKGRP-HIGH'.   " OR
*        SCREEN-NAME = 'S2_MTART-HIGH'.
      SCREEN-INPUT = '0'.
    ENDIF.

    IF P_R4 IS NOT INITIAL.
*      IF SCREEN-NAME+0(7) = 'S_FLIEF'.
*        SCREEN-INPUT = '0'.
*      ENDIF.
      IF  SCREEN-NAME(7) = 'S_EKORG'.
        SCREEN-INPUT = '0'.
        SCREEN-REQUIRED = '0'.
      ENDIF.

      CLEAR: S_EKORG[], S_EKORG.

    ELSE.
      IF  SCREEN-NAME = 'S_EKORG-LOW'.
        SCREEN-INPUT = '1'.
        SCREEN-REQUIRED = '2'.
      ENDIF.

    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LFB1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DISP_100_WERKS
*&      --> GT_DISP_100_LIFNR
*&      <-- GT_DISP_100_SPERR
*&      <-- GT_DISP_100_LOEVM
*&---------------------------------------------------------------------*
FORM GET_LFB1  USING    PV_WERKS
                        PV_LIFNR
               CHANGING PV_SPERR
                        PV_LOEVM.


  CHECK PV_WERKS IS NOT INITIAL AND
        PV_LIFNR IS NOT INITIAL.

  SELECT SINGLE A~SPERR,
                A~LOEVM
    FROM LFB1 AS A
    JOIN T001K AS B
      ON A~BUKRS = B~BUKRS
   WHERE A~LIFNR EQ @PV_LIFNR
     AND B~BWKEY EQ @PV_WERKS
        INTO ( @PV_SPERR, @PV_LOEVM ).



ENDFORM.
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INITIALIZATION .


*-901의 경우, 구매오더를 생성하지는 않지만,
*-소요량을 무상OEM업체에 통보해야 하는 경우가 있어서 추가. 20190522. 음성 .
*구매그룹.
  _RANGES S2_EKGRP C_E C_EQ '900' ''.
  _RANGES S2_EKGRP C_E C_EQ '901' ''.

** ('ROH1', 'ROH2', 'FERT', 'HAWA', 'PMIQ', 'PMIV').
**자재유형 포함.
*  _RANGES S2_MTART C_I C_EQ 'ROH1' ''.
*  _RANGES S2_MTART C_I C_EQ 'ROH2' ''.
*  _RANGES S2_MTART C_I C_EQ 'FERT' ''.
*  _RANGES S2_MTART C_I C_EQ 'HAWA' ''.
*  _RANGES S2_MTART C_I C_EQ 'PMIQ' ''.
*  _RANGES S2_MTART C_I C_EQ 'PMIV' ''.


**   SET PARAMETER ID 'WRK' FIELD s_dwerk-low.
*   GET PARAMETER ID 'EKO' FIELD S_EKORG-LOW.
*   APPEND s_EKORG.
*   SET PARAMETER ID 'WRK' FIELD s_dwerk-low.
  GET PARAMETER ID 'EKO' FIELD S_EKORG-LOW.
  _GET_APPEND : S_EKORG S_EKORG-LOW.
*  APPEND s_dwerk.


*   SET PARAMETER ID 'WRK' FIELD s_dwerk-low.
  GET PARAMETER ID 'WRK' FIELD S_WERKS-LOW.
  _GET_APPEND : S_WERKS S_WERKS-LOW.
*  APPEND s_dwerk.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LABST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DISP_100
*&      <-- GT_DISP_100_LABST
*&---------------------------------------------------------------------*
FORM GET_LABST  USING    PS_DISP_100 TYPE TY_100
                CHANGING PV_LABST.

 DATA: LV_LGORT  LIKE MARD-LGORT.

 CLEAR: LV_LGORT.
 CASE PS_DISP_100-MTART.
    WHEN 'ROH1'.  LV_LGORT = '1100'.
    WHEN 'ROH2'.  LV_LGORT = '1200'.
    WHEN OTHERS.  EXIT.
 ENDCASE.

*  IF PS_DISP_100-MTART = 'ROH1'.
*    SELECT SINGLE LABST
*      FROM MARD
*      INTO @PV_LABST
*     WHERE MATNR EQ @PS_DISP_100-MATNR
*       AND WERKS EQ @PS_DISP_100-WERKS
*       AND LGORT EQ '1100'.
*  ELSEIF PS_DISP_100-MTART = 'ROH2'.
*    SELECT SINGLE LABST
*      FROM MARD
*      INTO @PV_LABST
*     WHERE MATNR EQ @PS_DISP_100-MATNR
*       AND WERKS EQ @PS_DISP_100-WERKS
*       AND LGORT EQ '1200'.
*  ENDIF.

    SELECT SINGLE D~LABST
      FROM MARD as D
      INTO @PV_LABST
     WHERE D~MATNR EQ @PS_DISP_100-MATNR
       AND D~WERKS EQ @PS_DISP_100-WERKS
       AND D~LGORT EQ @LV_LGORT.  "'1200'.

    SELECT SINGLE MEINS
        FROM MARA
        INTO @PS_DISP_100-MEINS_LABST
    WHERE MATNR EQ @PS_DISP_100-MATNR.


*20190930 미착재고 추가
  SELECT SINGLE LABST
    FROM MARD
    INTO @PS_DISP_100-LABST_2300
     WHERE MATNR EQ @PS_DISP_100-MATNR
       AND WERKS EQ @PS_DISP_100-WERKS
       AND LGORT EQ '2300'.


  READ TABLE GT_LABST_RES WITH KEY MATNR = PS_DISP_100-MATNR BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    PS_DISP_100-LABST_RES = GT_LABST_RES-MENGE.
  ENDIF.


*합계
  PS_DISP_100-LABST_SUM = PS_DISP_100-LABST_RES + PS_DISP_100-LABST +
                          PS_DISP_100-LABST_2300.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_AUTH .

  DATA : LV_RETURN  TYPE  BAPI_MTYPE,
         LV_MESSAGE TYPE  BAPI_MSG.
*1:마스터 2:오더/송장 3:이/출고 4:조회

  CALL FUNCTION 'ZMM_AUTH_CHECK'
    EXPORTING
      IV_GUBUN  = '2'
*     IV_EKORG  =
*     IV_WERKS  =
*     IV_BUKRS  =
    IMPORTING
      EV_RETURN = LV_RETURN
      EV_MSG    = LV_MESSAGE
    TABLES
*     T_BUKRS   =
      T_EKORG   = S_EKORG[]
      T_WERKS   = S_WERKS[].
  IF LV_RETURN = 'E'.
    MESSAGE LV_MESSAGE TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LFA1_NAME1_CAM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM GET_LFA1_NAME1_CAM .
*
*  DATA:
*        LV_superfield like mepo_topline-superfield,
*        lt_vendors      LIKE vendor_found OCCURS 0 WITH HEADER LINE,
*        l_vendors_found LIKE sy-tabix.
*
*        LOOP AT GT_LIFNR.
*
*          LV_superfield = GT_LIFNR-LIFNR.  "공급업체 코드.
*
*          CALL FUNCTION 'MM_VENDOR_SEARCH'
*            EXPORTING
*              i_string        = LV_superfield   "공급업체 코드.
*            IMPORTING
*              e_vendors_found = l_vendors_found
*            TABLES
*              t_vendor_found  = lt_vendors.
*          IF l_vendors_found EQ 1.  "공급업체 Found.
*            READ TABLE lt_vendors INDEX 1.
**            im_topline_new-lifnr = lt_vendors-lifnr.
*            GT_LFA1-lifnr     = lt_vendors-lifnr.
*            GT_LFA1-name1 = lt_vendors-name.  "공급업체 명.
*            APPEND GT_LFA1.
*
*          else. "IF l_vendors_found EQ 0.  "공급업체 Not found
*          endif.
*
*      ENDLOOP.
*
**      IF LT_LIFNR[] IS NOT INITIAL.
**        SELECT LIFNR NAME1
**          FROM LFA1
**          INTO CORRESPONDING FIELDS OF TABLE GT_LFA1
**           FOR ALL ENTRIES IN LT_LIFNR
**         WHERE LIFNR EQ LT_LIFNR-LIFNR.
**        IF SY-SUBRC EQ 0.
**          SORT GT_LFA1 BY LIFNR.
**        ENDIF.
**      ENDIF.
*
*
*          SORT GT_LFA1 BY LIFNR.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form BAPI_PR_UPDATE_CLOSE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_EKPO
*&---------------------------------------------------------------------*
FORM BAPI_PR_UPDATE_CLOSE.

*  DATA : BEGIN OF LT_EKPO OCCURS 0,
*           EBELN LIKE EKPO-EBELN,
*           EBELP LIKE EKPO-EBELP,
*           BANFN LIKE EKPO-BANFN,
*           BNFPO LIKE EKPO-BNFPO,
*         END OF LT_EKPO.


  DATA : LV_BSMNG  TYPE BSMNG,  "구매요청에 대한 구매오더 수량.
             LV_MENGE  TYPE BAMNG.  "구매요청수량

*      LT_EKPO[] = PT_EKPO[].

      LOOP AT GT_EKPO.
* beg.     -구매요청 잔량이 있는 경우, 구매요청 마감을 처리함. 2019.10.10.
*        구매오더가 만들어진 경우이므로, 잔량비교가 의미가 없음. 구매오더번호가 있으면, CLOSE 처리하면 됨.
          CLEAR: LV_BSMNG, LV_MENGE.
          SELECT SINGLE BSMNG    "구매요청에 대한 구매오더 수량.
                                  MENGE    "구매요청수량
                                   INTO ( LV_BSMNG, LV_MENGE )   "구매요청 마감.
            FROM EBAN
          WHERE BANFN = GT_EKPO-BANFN
               AND BNFPO = GT_EKPO-BNFPO
               AND LOEKZ = SPACE
               AND EBAKZ = SPACE.
          IF SY-SUBRC = 0 AND LV_BSMNG < LV_MENGE. "구매요청에 대한 구매오더 수량 < 구매요청수량인 경우, 구매요청을 마감처리함.
* 실행-PR Change.
               CLEAR : GS_PRITEM, GS_PRITEMX, GT_PRITEM[], GT_PRITEMX.
               GS_PRITEM-PREQ_ITEM   = GS_PRITEMX-PREQ_ITEM     = GT_EKPO-BNFPO.  "구매요청 품목번호.
               GS_PRITEM-CLOSED        = GS_PRITEMX-CLOSED          = 'X'.
               APPEND GS_PRITEM   TO GT_PRITEM.
               APPEND GS_PRITEMX TO GT_PRITEMX.
               CALL FUNCTION 'BAPI_PR_CHANGE'
                 EXPORTING
                   NUMBER        = GT_EKPO-BANFN  "구매요청번호.
*                 IMPORTING
*                   EXPPURCHASEORDER = G_EBELN
                 TABLES
                   RETURN           = GT_RETURN_PR
                   PRITEM           = GT_PRITEM
                   PRITEMX          = GT_PRITEMX.

          ENDIF.
     ENDLOOP.


             CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  WAIT = 'X'.


ENDFORM."""
]

id_counter = 1
dataset = []
for code in safe_templates:
    dataset.append({
        "id": f"ABAP-real-{id_counter}",
        "cwe": "NONE",
        "label": "safe",
        "code": code
    })
    id_counter += 1

with open("abap_dataset.jsonl", "a", encoding="utf-8") as f:
    for item in dataset:
        f.write(json.dumps(item, ensure_ascii=False) + "\n")