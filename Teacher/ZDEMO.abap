*&---------------------------------------------------------------------*
*& Report ZDEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZDEMO.



TABLES:BKPF.

SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME TITLE T0.

PARAMETERS:    P_BUKRS TYPE BKPF-BUKRS OBLIGATORY.
SELECT-OPTIONS:S_BELNR FOR BKPF-BELNR,
               S_GJAHR FOR BKPF-GJAHR.

SELECTION-SCREEN END OF BLOCK BLK.

*----------------------------------------------------------------------*
*       全局变量，表定义
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_ALV,
         SEL   TYPE C,
         BUKRS TYPE BKPF-BUKRS,
         GJAHR TYPE BKPF-GJAHR,
         MONAT TYPE BKPF-MONAT,
         BELNR TYPE BKPF-BELNR,
         BLART TYPE BKPF-BLART,
         BLDAT TYPE BKPF-BLDAT,
         BUDAT TYPE BKPF-BUDAT,
         XSTOV TYPE BKPF-XSTOV,
         STBLG TYPE BKPF-STBLG,
         PPNAM TYPE BKPF-PPNAM,
         USNAM TYPE BKPF-USNAM,
       END OF TY_ALV.
DATA:GT_ALV TYPE TABLE OF TY_ALV,
     GS_ALV TYPE TY_ALV.

DATA:GT_ITEM TYPE TABLE OF ZSFI005_ITEM,
     GS_ITEM TYPE ZSFI005_ITEM,
     GS_HEAD TYPE ZSFI005_HEAD,
     GT_SKAT TYPE TABLE OF SKAT,
     GS_SKAT TYPE SKAT.

*----------------------------------------------------------------------*
*       ALV层级关系定义
*----------------------------------------------------------------------*
DATA: GT_FIELDCAT TYPE LVC_T_FCAT,
      GS_FIELDCAT TYPE LVC_S_FCAT,
      GT_EVENTS   TYPE SLIS_T_EVENT,   "事件存储内表
      GS_EVENTS   TYPE SLIS_ALV_EVENT.
DATA GS_LAYOUT TYPE LVC_S_LAYO.

*&---------------------------------------------------------------------*
*&      Define marco
*&---------------------------------------------------------------------*
DEFINE  MACRO_FILL_FCAT.
  CLEAR gs_fieldcat.
  &1 = &1 + 1.
  gs_fieldcat-col_pos       = &1.
  gs_fieldcat-fieldname     = &2.
  gs_fieldcat-coltext     = &3.
  gs_fieldcat-icon       = &4.
  gs_fieldcat-no_zero       = &5.
  gs_fieldcat-key           = &6.
  gs_fieldcat-ref_table   = &7.
  gs_fieldcat-ref_field = &8.   " 内表中数量参照字段
  gs_fieldcat-edit = &9.   "
  APPEND gs_fieldcat TO gt_fieldcat.
END-OF-DEFINITION.


START-OF-SELECTION.

  PERFORM FRM_GET_DATA.
  PERFORM FRM_DIS_ALV.





FORM FRM_GET_DATA.

  SELECT
         BUKRS
         GJAHR
         MONAT
         BELNR
         BLART
         BLDAT
         BUDAT
         XSTOV
         STBLG
         PPNAM
         USNAM
   INTO CORRESPONDING FIELDS OF TABLE GT_ALV
   FROM BKPF
   WHERE BUKRS = P_BUKRS
    AND  BELNR IN S_BELNR
    AND  GJAHR IN S_GJAHR.

  IF GT_ALV IS NOT INITIAL.
    SELECT
           BUKRS
           GJAHR
           BELNR
           BUZEI
           HKONT
           KOSTL
           SHKZG
           PRCTR
           WRBTR
           DMBTR
           TXT20 AS ZMX
           H_HWAER AS HWAER
    INTO CORRESPONDING FIELDS OF TABLE  GT_ITEM
    FROM BSEG AS A JOIN SKAT AS B ON A~HKONT = B~SAKNR
    FOR ALL ENTRIES IN GT_ALV
    WHERE BUKRS = GT_ALV-BUKRS
      AND GJAHR = GT_ALV-GJAHR
      AND BELNR = GT_ALV-BELNR
      AND B~SPRAS = SY-LANGU.
  ENDIF.

ENDFORM.

FORM FRM_DIS_ALV.


  DATA: L_COLPOS TYPE LVC_S_FCAT-COL_POS VALUE 0.


  CLEAR GS_LAYOUT.

  GS_LAYOUT-BOX_FNAME   = 'SEL'.
  GS_LAYOUT-CWIDTH_OPT  = 'X'.   "设置Grid的字段列宽度自动适应


  CLEAR GT_FIELDCAT.

  MACRO_FILL_FCAT:
    L_COLPOS 'BUKRS'    '公司代码'     '' ' ' ' ' '' '' '',
    L_COLPOS 'GJAHR'    '会计年度'     '' ' ' ' ' '' '' '',
    L_COLPOS 'MONAT'    '会计期间'        '' ' ' ' ' ' ' ' ' '',
    L_COLPOS 'BELNR'    '凭证编号'     '' ' ' ' ' ' ' ' ' '',
    L_COLPOS 'BLART'    '凭证类型' ' ' ' ' ' ' ' ' ' ' ' ',
    L_COLPOS 'BLDAT'    '凭证日期'        '' ' ' ' ' '' '' '',
    L_COLPOS 'BUDAT'    '过账日期'     '' ' ' ' ' ' ' ' ' '',
    L_COLPOS 'XSTOV'    '冲销标识'     ' ' ' ' ' ' '' '' ' ',
    L_COLPOS 'STBLG'    '冲销凭证'     ' ' ' ' ' ' ' ' ' ' ' ',
    L_COLPOS 'PPNAM'    '制单人'     '' ' ' ' ' '' '' '',
    L_COLPOS 'USNAM'    '过账人'        '' ' ' ' ' ' ' ' ' ''
    .


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'FRM_SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'FRM_USER_COMMAND'
      IS_LAYOUT_LVC            = GS_LAYOUT
      IT_FIELDCAT_LVC          = GT_FIELDCAT
*     IT_EVENTS                = I_EVENTS[]
      I_SAVE                   = 'A'
    TABLES
      T_OUTTAB                 = GT_ALV
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text  设置alv状态
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM FRM_SET_PF_STATUS USING PT_EXTAB TYPE SLIS_T_EXTAB.


  SET PF-STATUS 'STANDARD_FULLSCREEN'  .

ENDFORM.                    "_SET_PF_STATUS

FORM FRM_USER_COMMAND USING PV_UCOMM LIKE SY-UCOMM
                             PS_SELFIELD TYPE SLIS_SELFIELD.
  DATA: LV_GRID  TYPE REF TO CL_GUI_ALV_GRID.
  DATA: LV_FLAG  TYPE C.
  DATA:BEGIN OF LS_MBLNR,
         MBLNR TYPE MSEG-MBLNR,
       END OF LS_MBLNR.
  DATA LT_MBLNR LIKE TABLE OF LS_MBLNR.
  DATA NUM TYPE I.


*&将变更的数据刷新
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = LV_GRID.
  CALL METHOD LV_GRID->CHECK_CHANGED_DATA.
  PS_SELFIELD-REFRESH = 'X'.

  TRY.
      DATA(LS_ALV) = GT_ALV[ PS_SELFIELD-TABINDEX ].
    CATCH CX_SY_ITAB_LINE_NOT_FOUND.

  ENDTRY.




  CASE PV_UCOMM.
    WHEN 'PRINT' .  "打印
      IF LINE_EXISTS( GT_ALV[ SEL = 'X' ] ).
        PERFORM FRM_DATA_PRINT.
      ELSE.
        MESSAGE '请至少选择一行数据进行打印' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    WHEN '&IC1'.
      CASE PS_SELFIELD-FIELDNAME.
        WHEN 'BELNR'.
          SET PARAMETER ID 'BLN' FIELD LS_ALV-BELNR.
          SET PARAMETER ID 'BUK' FIELD LS_ALV-BUKRS.
          SET PARAMETER ID 'GJR' FIELD LS_ALV-GJAHR.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ENDCASE.

  ENDCASE.

  CALL METHOD LV_GRID->REFRESH_TABLE_DISPLAY.
ENDFORM.

FORM FRM_DATA_PRINT.

  DATA:LT_ITEM TYPE  TABLE OF ZSFI005_ITEM,
       LT_HEAD TYPE  TABLE OF  ZSFI005_HEAD.

  DATA:LINES      TYPE I,
       CTRL_PARAM TYPE SSFCTRLOP,
       OUT_OPTION TYPE SSFCOMPOP,
       FM_NAME    TYPE RS38L_FNAM.



  LOOP AT GT_ALV  INTO GS_ALV WHERE SEL = 'X'.

    LOOP AT GT_ITEM INTO GS_ITEM WHERE  BELNR = GS_ALV-BELNR
                                    AND GJAHR = GS_ALV-GJAHR
                                    AND BUKRS = GS_ALV-BUKRS.
      APPEND GS_ITEM TO LT_ITEM.
    ENDLOOP.


    MOVE-CORRESPONDING GS_ALV TO GS_HEAD.
    append gs_head to LT_HEAD.

    LINES = LINES( LT_ITEM ).

   clear gs_head.
  ENDLOOP.


  "获取SMARTFOMRS函数
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = 'ZDEMO' "Smart Form名称
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.


  "调用SMARTFOMRS打印
  CALL FUNCTION FM_NAME
    EXPORTING
      CONTROL_PARAMETERS = CTRL_PARAM
      ZHEAD              = GS_HEAD
      ZLINES             = LINES
    TABLES
      zhead              = LT_HEAD
      ZITEM              = LT_ITEM
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.


ENDFORM.