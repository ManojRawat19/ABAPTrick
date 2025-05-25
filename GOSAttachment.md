" Blog post 2024/07/31:
" https://community.sap.com/t5/technology-blogs-by-members/yet-another-post-about-gos-attachment-programming/ba-p/13778915#M168561
"
" The parameters and START-OF-SELECTION are located at the end of the source code.
"
" CREDITS: https://github.com/keremkoseoglu
"
" zcl_bc_gos_toolkit
"     Permalink: https://github.com/keremkoseoglu/ABAP-Library/blob/2fc3eb2f3ecc891cd7e7d8c1bba291526d42779a/document/zcl_bc_gos_toolkit.abap
"     Latest: https://github.com/keremkoseoglu/ABAP-Library/blob/master/document/zcl_bc_gos_toolkit.abap
"
" ycx_addict_function_subrc
"     Permalink: https://github.com/keremkoseoglu/addict/blob/fd21abd36432639165daa0e3c1888fcd1f559032/src/ycx_addict_function_subrc.clas.abap
"     Latest: https://github.com/keremkoseoglu/addict/blob/main/src/ycx_addict_function_subrc.clas.abap
"
" zcx_bc_function_subrc
"     Permalink: https://github.com/keremkoseoglu/ABAP-Library/blob/2316ce29378faf5a04b78d5ceda80715325b2f3d/abap_oo/zcx_bc_function_subrc.abap
"     Latest: https://github.com/keremkoseoglu/ABAP-Library/blob/master/abap_oo/zcx_bc_function_subrc.abap
"
" Other classes missing in https://github.com/keremkoseoglu, recreated manually by me.

REPORT z_test_of_zcl_bc_gos_toolkit.

CLASS zcx_bc_gos_doc_attach DEFINITION DEFERRED.
CLASS zcx_bc_gos_url_attach DEFINITION DEFERRED.
CLASS zcx_bc_gos_doc_delete DEFINITION DEFERRED.
CLASS zcx_bc_gos_doc_content DEFINITION DEFERRED.
CLASS ycx_addict_function_subrc DEFINITION DEFERRED.
CLASS zcx_bc_function_subrc DEFINITION DEFERRED.
CLASS zcl_bc_gos_toolkit DEFINITION DEFERRED.

CLASS zcx_bc_gos_doc_attach DEFINITION
  INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING textid    LIKE textid   OPTIONAL
                !previous LIKE previous OPTIONAL
                objectid  TYPE csequence.

    DATA objectid TYPE string READ-ONLY.
ENDCLASS.


CLASS zcx_bc_gos_doc_attach IMPLEMENTATION.
  METHOD constructor.
    super->constructor( textid   = textid
                        previous = previous ).
    me->objectid = objectid.
  ENDMETHOD.
ENDCLASS.


CLASS zcx_bc_gos_url_attach DEFINITION
  INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING textid    LIKE textid   OPTIONAL
                !previous LIKE previous OPTIONAL
                objectid  TYPE csequence
                url       TYPE csequence.

    DATA objectid TYPE string READ-ONLY.
    DATA url      TYPE string.
ENDCLASS.


CLASS zcx_bc_gos_url_attach IMPLEMENTATION.
  METHOD constructor.
    super->constructor( textid   = textid
                        previous = previous ).
    me->objectid = objectid.
    me->url      = url.
  ENDMETHOD.
ENDCLASS.


CLASS zcx_bc_gos_doc_delete DEFINITION
  INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING textid    LIKE textid   OPTIONAL
                !previous LIKE previous OPTIONAL
                folder_id TYPE soodk
                object_id TYPE soodk.

    DATA folder_id TYPE soodk READ-ONLY.
    DATA object_id TYPE soodk READ-ONLY.
ENDCLASS.


CLASS zcx_bc_gos_doc_delete IMPLEMENTATION.
  METHOD constructor.
    super->constructor( textid   = textid
                        previous = previous ).
    me->folder_id = folder_id.
    me->object_id = object_id.
  ENDMETHOD.
ENDCLASS.


CLASS zcx_bc_gos_doc_content DEFINITION
  INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING textid    LIKE textid   OPTIONAL
                !previous LIKE previous OPTIONAL
                objectid  TYPE csequence.

    DATA objectid TYPE string READ-ONLY.
ENDCLASS.


CLASS zcx_bc_gos_doc_content IMPLEMENTATION.
  METHOD constructor.
    super->constructor( textid   = textid
                        previous = previous ).
    me->objectid = objectid.
  ENDMETHOD.
ENDCLASS.


CLASS zcl_bc_text_toolkit DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS remove_text_in_string
      IMPORTING iv_string     TYPE string
                iv_remove     TYPE csequence
      RETURNING VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_bc_text_toolkit IMPLEMENTATION.
  METHOD remove_text_in_string.
    result = replace( val  = iv_string
                      sub  = iv_remove
                      with = `` ).
  ENDMETHOD.
ENDCLASS.


CLASS ycx_addict_function_subrc DEFINITION
*  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    CONSTANTS:
      BEGIN OF ycx_addict_function_subrc,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '324',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'SUBRC',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF ycx_addict_function_subrc.
    CONSTANTS:
      BEGIN OF subrc_error,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '130',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'SUBRC',
        attr3 TYPE scx_attrname VALUE 'PARAM',
        attr4 TYPE scx_attrname VALUE 'STEXT',
      END OF subrc_error.
    CONSTANTS:
      BEGIN OF function_returned_error,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '382',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF function_returned_error.
    CONSTANTS:
      BEGIN OF function_returned_error_txt,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '456',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'ERROR_TEXT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF function_returned_error_txt.
    CONSTANTS:
      BEGIN OF no_result_returned,
        msgid TYPE symsgid      VALUE 'YADDICT',
        msgno TYPE symsgno      VALUE '584',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'STEXT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_result_returned.

    DATA funcname   TYPE funct-funcname.
    DATA subrc      TYPE sysubrc.
    DATA param      TYPE funct-parameter.
    DATA stext      TYPE funct-stext.
    DATA error_text TYPE string.

    CLASS-METHODS raise_if_sysubrc_not_initial
      IMPORTING funcname TYPE funct-funcname
      RAISING   ycx_addict_function_subrc.

    METHODS constructor
      IMPORTING textid     LIKE if_t100_message=>t100key OPTIONAL
                !previous  LIKE previous                 OPTIONAL
                funcname   TYPE funct-funcname           OPTIONAL
                !subrc     TYPE sysubrc                  OPTIONAL
                param      TYPE funct-parameter          OPTIONAL
                stext      TYPE funct-stext              OPTIONAL
                error_text TYPE string                   OPTIONAL.
ENDCLASS.


CLASS ycx_addict_function_subrc IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    me->funcname   = funcname.
    me->subrc      = subrc.
    me->param      = param.
    me->stext      = stext.
    me->error_text = error_text.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = ycx_addict_function_subrc.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise_if_sysubrc_not_initial.
    CHECK sy-subrc IS NOT INITIAL.

    DATA(subrc_bak) = sy-subrc.

    SELECT SINGLE parameter FROM fupararef              "#EC CI_NOORDER
      WHERE funcname  = @funcname
        AND paramtype = @abap_true
        AND pposition = @subrc_bak
      INTO @DATA(parameter).

    SELECT SINGLE stext FROM funct
      WHERE spras     = @sy-langu
        AND funcname  = @funcname             "#EC CI_NOORDER
        AND parameter = @parameter
        AND kind      = @abap_true
      INTO @DATA(stext).

    IF sy-subrc <> 0.
      SELECT SINGLE stext FROM funct                    "#EC CI_GENBUFF
        WHERE funcname  = @funcname           "#EC CI_NOORDER
          AND parameter = @parameter
          AND kind      = @abap_true
        INTO @stext.
    ENDIF.

    RAISE EXCEPTION TYPE ycx_addict_function_subrc
      EXPORTING funcname = funcname
                param    = parameter
                stext    = stext
                subrc    = subrc_bak
                textid   = ycx_addict_function_subrc=>subrc_error.
  ENDMETHOD.
ENDCLASS.


CLASS zcx_bc_function_subrc DEFINITION
*  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    CONSTANTS:
      BEGIN OF zcx_bc_function_subrc,
        msgid TYPE symsgid      VALUE 'ZBC',
        msgno TYPE symsgno      VALUE '324',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'SUBRC',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_bc_function_subrc.
    CONSTANTS:
      BEGIN OF subrc_error,
        msgid TYPE symsgid      VALUE 'ZBC',
        msgno TYPE symsgno      VALUE '130',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'SUBRC',
        attr3 TYPE scx_attrname VALUE 'PARAM',
        attr4 TYPE scx_attrname VALUE 'STEXT',
      END OF subrc_error.
    CONSTANTS:
      BEGIN OF function_returned_error,
        msgid TYPE symsgid      VALUE 'ZBC',
        msgno TYPE symsgno      VALUE '382',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF function_returned_error.
    CONSTANTS:
      BEGIN OF function_returned_error_txt,
        msgid TYPE symsgid      VALUE 'ZBC',
        msgno TYPE symsgno      VALUE '456',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'ERROR_TEXT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF function_returned_error_txt.
    CONSTANTS:
      BEGIN OF no_result_returned,
        msgid TYPE symsgid      VALUE 'ZBC',
        msgno TYPE symsgno      VALUE '584',
        attr1 TYPE scx_attrname VALUE 'FUNCNAME',
        attr2 TYPE scx_attrname VALUE 'STEXT',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_result_returned.

    DATA funcname   TYPE funct-funcname.
    DATA subrc      TYPE sysubrc.
    DATA param      TYPE funct-parameter.
    DATA stext      TYPE funct-stext.
    DATA error_text TYPE string.

    CLASS-METHODS raise_if_sysubrc_not_initial
      IMPORTING iv_funcname TYPE funct-funcname
      RAISING   zcx_bc_function_subrc.

    CLASS-METHODS raise_from_addict
      IMPORTING !error TYPE REF TO ycx_addict_function_subrc
      RAISING   zcx_bc_function_subrc.

    METHODS constructor
      IMPORTING textid     LIKE if_t100_message=>t100key OPTIONAL
                !previous  LIKE previous                 OPTIONAL
                funcname   TYPE funct-funcname           OPTIONAL
                !subrc     TYPE sysubrc                  OPTIONAL
                param      TYPE funct-parameter          OPTIONAL
                stext      TYPE funct-stext              OPTIONAL
                error_text TYPE string                   OPTIONAL.
ENDCLASS.


CLASS zcx_bc_function_subrc IMPLEMENTATION.
  METHOD raise_if_sysubrc_not_initial.
    CHECK sy-subrc IS NOT INITIAL.

    DATA(lv_subrc_bak) = sy-subrc.

    SELECT SINGLE parameter INTO @DATA(lv_parameter)
      FROM fupararef
      WHERE funcname  = @iv_funcname
        AND paramtype = @abap_true
        AND pposition = @lv_subrc_bak.

    SELECT SINGLE stext INTO @DATA(lv_stext)
      FROM funct
      WHERE spras     = @sy-langu
        AND funcname  = @iv_funcname
        AND parameter = @lv_parameter
        AND kind      = @abap_true.

    IF sy-subrc <> 0.
      SELECT SINGLE stext INTO @lv_stext
        FROM funct
        WHERE funcname  = @iv_funcname
          AND parameter = @lv_parameter
          AND kind      = @abap_true.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_bc_function_subrc
      EXPORTING funcname = iv_funcname
                param    = lv_parameter
                stext    = lv_stext
                subrc    = lv_subrc_bak
                textid   = zcx_bc_function_subrc=>subrc_error.
  ENDMETHOD.

  METHOD raise_from_addict.
    RAISE EXCEPTION TYPE zcx_bc_function_subrc
      EXPORTING textid     = error->if_t100_message~t100key
                previous   = error
                funcname   = error->funcname
                subrc      = error->subrc
                param      = error->param
                stext      = error->stext
                error_text = error->error_text.
  ENDMETHOD.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    me->funcname   = funcname.
    me->subrc      = subrc.
    me->param      = param.
    me->stext      = stext.
    me->error_text = error_text.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_bc_function_subrc.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS zcl_bc_gos_toolkit DEFINITION
*  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_bdn_con TYPE STANDARD TABLE OF bdn_con WITH DEFAULT KEY.

    TYPES: BEGIN OF t_doc_content,
             gos_conn    TYPE bdn_con,
             doc_data    TYPE sofolenti1,
             obj_header  TYPE ccrctt_text_tab,
             filename    TYPE string,
             extension   TYPE char10,
             txt_content TYPE ccrctt_text_tab,
             hex_content TYPE solix_tab,
             hex_string  TYPE xstring,
             mime_type   TYPE mimetypes-type,
           END OF t_doc_content,

           tt_doc_content TYPE STANDARD TABLE OF t_doc_content WITH DEFAULT KEY.

    TYPES: BEGIN OF t_doc_content_key,
             classname TYPE bapibds01-classname,
             objkey    TYPE swotobjid-objkey,
           END OF t_doc_content_key.

    TYPES: BEGIN OF t_url,
             address     TYPE string,
             description TYPE string,
           END OF t_url,

           tt_url TYPE STANDARD TABLE OF t_url WITH DEFAULT KEY.

    CLASS-METHODS attach_doc
      IMPORTING is_key         TYPE t_doc_content_key
                iv_filename    TYPE clike
                iv_description TYPE clike
                iv_hex_string  TYPE xstring
      RAISING   zcx_bc_gos_doc_attach.

    CLASS-METHODS attach_url
      IMPORTING is_key TYPE t_doc_content_key
                is_url TYPE t_url
      RAISING   zcx_bc_gos_url_attach.

    CLASS-METHODS delete_doc
      IMPORTING is_key       TYPE t_doc_content_key
                is_folder_id TYPE soodk
                is_object_id TYPE soodk
      RAISING   zcx_bc_gos_doc_delete.

    CLASS-METHODS get_doc_content
      IMPORTING is_key            TYPE t_doc_content_key
      RETURNING VALUE(rt_content) TYPE tt_doc_content
      RAISING   zcx_bc_gos_doc_content.

    CLASS-METHODS get_url_list
      IMPORTING is_key        TYPE t_doc_content_key
      RETURNING VALUE(rt_url) TYPE tt_url
      RAISING   zcx_bc_gos_doc_content.

  PRIVATE SECTION.
    TYPES: BEGIN OF t_doc_content_cache,
             content_key TYPE t_doc_content_key,
             content     TYPE tt_doc_content,
             cx          TYPE REF TO zcx_bc_gos_doc_content,
           END OF t_doc_content_cache,

           tt_doc_content_cache TYPE HASHED TABLE OF t_doc_content_cache
             WITH UNIQUE KEY primary_key COMPONENTS content_key.

    TYPES tt_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    CONSTANTS c_object_type_url TYPE soodk-objtp VALUE 'URL'.
    CONSTANTS c_url_prefix      TYPE char5       VALUE '&KEY&'.

    CLASS-DATA gt_doc_content_cache TYPE tt_doc_content_cache.

    CLASS-METHODS extract_file_from_obj_header
      IMPORTING it_head      TYPE ccrctt_text_tab
      EXPORTING ev_filename  TYPE string
                ev_extension TYPE clike
                ev_mimetype  TYPE mimetypes-type.

    CLASS-METHODS split_filename
      IMPORTING iv_filename  TYPE clike
      EXPORTING ev_name      TYPE clike
                ev_extension TYPE clike.

ENDCLASS.


CLASS zcl_bc_gos_toolkit IMPLEMENTATION.
  METHOD attach_doc.
    DATA ls_folder_id        TYPE sofdk.
    DATA ls_object_hd_change TYPE sood1.
    DATA ls_object_id        TYPE soodk.
    DATA lt_data             TYPE soli_tab.
    DATA lt_xdata            TYPE solix_tab.

    TRY.

        " ______________________________
        " Attach et

        CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
          EXPORTING  region                = 'B'
          IMPORTING  folder_id             = ls_folder_id
          EXCEPTIONS communication_failure = 1
                     owner_not_exist       = 2
                     system_failure        = 3
                     x_error               = 4
                     OTHERS                = 5
          ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'SO_FOLDER_ROOT_ID_GET' ).

        cl_bcs_convert=>xstring_to_xtab( EXPORTING iv_xstring = iv_hex_string
                                         IMPORTING et_xtab    = lt_xdata ).

        CALL FUNCTION 'SO_SOLIXTAB_TO_SOLITAB'
          EXPORTING ip_solixtab = lt_xdata
          IMPORTING ep_solitab  = lt_data.

        split_filename( EXPORTING iv_filename  = iv_filename
                        IMPORTING ev_extension = ls_object_hd_change-file_ext ).

        ls_object_hd_change-objdes = iv_description.
        ls_object_hd_change-objla  = sy-langu.
        ls_object_hd_change-objlen = xstrlen( iv_hex_string ).
        ls_object_hd_change-objpri = 5.

        DATA(lt_obj_header) = VALUE ccrctt_text_tab( ( line = |&SO_FILENAME={ iv_filename }| )
                                                     ( line = '&SO_FORMAT=BIN' ) ).

        CALL FUNCTION 'SO_OBJECT_INSERT'
          EXPORTING  folder_id                  = ls_folder_id
                     object_hd_change           = ls_object_hd_change
                     object_type                = 'EXT'
                     owner                      = sy-uname
          IMPORTING  object_id                  = ls_object_id
          TABLES     objcont                    = lt_data
                     objhead                    = lt_obj_header
          EXCEPTIONS active_user_not_exist      = 1
                     communication_failure      = 2
                     component_not_available    = 3
                     dl_name_exist              = 4
                     folder_not_exist           = 5
                     folder_no_authorization    = 6
                     object_type_not_exist      = 7
                     operation_no_authorization = 8
                     owner_not_exist            = 9
                     parameter_error            = 10
                     substitute_not_active      = 11
                     substitute_not_defined     = 12
                     system_failure             = 13
                     x_error                    = 14
                     OTHERS                     = 15
          ##FM_SUBRC_OK ##NUMBER_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'SO_OBJECT_INSERT' ).

        " ______________________________
        " Nesne ile bağlantıyı sağla

        DATA(ls_obj_rolea) = VALUE borident( objkey  = is_key-objkey
                                             objtype = is_key-classname ).

        DATA(ls_obj_roleb) = VALUE borident(
            objkey  = |{ ls_folder_id-foltp }{ ls_folder_id-folyr  }{ ls_folder_id-folno }{ ls_object_id-objtp }{ ls_object_id-objyr }{ ls_object_id-objno }|
            objtype = 'MESSAGE' ).

        CALL FUNCTION 'BINARY_RELATION_CREATE'
          EXPORTING  obj_rolea      = ls_obj_rolea
                     obj_roleb      = ls_obj_roleb
                     relationtype   = 'ATTA'
          EXCEPTIONS no_model       = 1
                     internal_error = 2
                     unknown        = 3
                     OTHERS         = 4
          ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'BINARY_RELATION_CREATE' ).

        COMMIT WORK AND WAIT.

      CATCH zcx_bc_gos_doc_attach INTO DATA(lo_attach_error).
        ROLLBACK WORK.                                 "#EC CI_ROLLBACK
        RAISE EXCEPTION lo_attach_error.
      CATCH cx_root INTO DATA(lo_diaper).
        ROLLBACK WORK.                                 "#EC CI_ROLLBACK
        RAISE EXCEPTION TYPE zcx_bc_gos_doc_attach
          EXPORTING previous = lo_diaper
                    objectid = |{ is_key-classname } { is_key-objkey }|.
    ENDTRY.

    " https://blogs.sap.com/2013/05/23/the-gos-generic-object-services-class-that-does-all-the-work/
  ENDMETHOD.

  METHOD attach_url.
    DATA ls_folder_id        TYPE sofdk.
    DATA ls_object_hd_change TYPE sood1.
    DATA ls_object_id        TYPE soodk.

    TRY.

        " ______________________________
        " Folder ID'yi belirle

        CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
          EXPORTING  region                = 'B'
          IMPORTING  folder_id             = ls_folder_id
          EXCEPTIONS communication_failure = 1
                     owner_not_exist       = 2
                     system_failure        = 3
                     x_error               = 4
                     OTHERS                = 5
          ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'SO_FOLDER_ROOT_ID_GET' ).

        " ______________________________
        " URL'yi kaydet

        ls_object_hd_change-objdes = is_url-description.
        ls_object_hd_change-objla  = sy-langu.
        ls_object_hd_change-objsns = 'O'.

        DATA(lt_obj_header) = VALUE ccrctt_text_tab( ).
        DATA(lt_data)       = VALUE soli_tab( ( line = |{ c_url_prefix }{ is_url-address }| ) ).

        CALL FUNCTION 'SO_OBJECT_INSERT'
          EXPORTING  folder_id                  = ls_folder_id
                     object_hd_change           = ls_object_hd_change
                     object_type                = c_object_type_url
                     owner                      = sy-uname
          IMPORTING  object_id                  = ls_object_id
          TABLES     objcont                    = lt_data
                     objhead                    = lt_obj_header
          EXCEPTIONS active_user_not_exist      = 1
                     communication_failure      = 2
                     component_not_available    = 3
                     dl_name_exist              = 4
                     folder_not_exist           = 5
                     folder_no_authorization    = 6
                     object_type_not_exist      = 7
                     operation_no_authorization = 8
                     owner_not_exist            = 9
                     parameter_error            = 10
                     substitute_not_active      = 11
                     substitute_not_defined     = 12
                     system_failure             = 13
                     x_error                    = 14
                     OTHERS                     = 15
          ##FM_SUBRC_OK ##NUMBER_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'SO_OBJECT_INSERT' ).

        " ______________________________
        " Bağlantıyı kaydet

        DATA(ls_obj_rolea) = VALUE borident( objkey  = is_key-objkey
                                             objtype = is_key-classname ).

        DATA(ls_obj_roleb) = VALUE borident(
            objkey  = |{ ls_folder_id-foltp }{ ls_folder_id-folyr  }{ ls_folder_id-folno }{ ls_object_id-objtp }{ ls_object_id-objyr }{ ls_object_id-objno }|
            objtype = 'MESSAGE' ).

        DATA(lv_reltype) = CONV breltyp-reltype( c_object_type_url ).

        CALL FUNCTION 'BINARY_RELATION_CREATE'
          EXPORTING  obj_rolea      = ls_obj_rolea
                     obj_roleb      = ls_obj_roleb
                     relationtype   = lv_reltype
          EXCEPTIONS no_model       = 1
                     internal_error = 2
                     unknown        = 3
                     OTHERS         = 4
          ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'BINARY_RELATION_CREATE' ).

        COMMIT WORK AND WAIT.

      CATCH zcx_bc_gos_url_attach INTO DATA(lo_attach_error).
        ROLLBACK WORK.                                 "#EC CI_ROLLBACK
        RAISE EXCEPTION lo_attach_error.
      CATCH cx_root INTO DATA(lo_diaper).
        RAISE EXCEPTION TYPE zcx_bc_gos_url_attach
          EXPORTING previous = lo_diaper
                    objectid = |{ is_key-classname } { is_key-objkey }|
                    url      = is_url-address.
    ENDTRY.

    " https://subrc0.wordpress.com/2012/11/13/gos-adding-an-external-link-to-an-object/
  ENDMETHOD.

  METHOD delete_doc.
    TRY.

        DATA(ls_obj_rolea) = VALUE borident( objkey  = is_key-objkey
                                             objtype = is_key-classname ).

        DATA(ls_obj_roleb) = VALUE borident(
            objkey  = |{ is_folder_id-objtp }{ is_folder_id-objyr  }{ is_folder_id-objno }{ is_object_id-objtp }{ is_object_id-objyr }{ is_object_id-objno }|
            objtype = 'MESSAGE' ).

        CALL FUNCTION 'BINARY_RELATION_DELETE'
          EXPORTING  obj_rolea          = ls_obj_rolea
                     obj_roleb          = ls_obj_roleb
                     relationtype       = 'ATTA'
          EXCEPTIONS entry_not_existing = 1
                     internal_error     = 2
                     no_relation        = 3
                     no_role            = 4
                     OTHERS             = 5
          ##FM_SUBRC_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'BINARY_RELATION_DELETE' ).

        CALL FUNCTION 'SO_OBJECT_DELETE'
          EXPORTING  folder_id                  = is_folder_id
                     object_id                  = is_object_id
          EXCEPTIONS communication_failure      = 1
                     folder_not_empty           = 2
                     folder_not_exist           = 3
                     folder_no_authorization    = 4
                     forwarder_not_exist        = 5
                     object_not_exist           = 6
                     object_no_authorization    = 7
                     operation_no_authorization = 8
                     owner_not_exist            = 9
                     substitute_not_active      = 10
                     substitute_not_defined     = 11
                     system_failure             = 12
                     x_error                    = 13
                     OTHERS                     = 14
          ##FM_SUBRC_OK ##NUMBER_OK.

        zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'SO_OBJECT_DELETE' ).

        COMMIT WORK AND WAIT.

      CATCH zcx_bc_gos_doc_delete INTO DATA(lo_del_error).
        RAISE EXCEPTION lo_del_error.

      CATCH cx_root INTO DATA(lo_diaper).

        RAISE EXCEPTION TYPE zcx_bc_gos_doc_delete
          EXPORTING previous  = lo_diaper
                    folder_id = is_folder_id
                    object_id = is_object_id.

    ENDTRY.
  ENDMETHOD.

  METHOD extract_file_from_obj_header.
    " ______________________________
    " Hazırlık

    CLEAR:
      ev_filename,
      ev_extension,
      ev_mimetype.

    " ______________________________
    " Dosya adı

    LOOP AT it_head
         ASSIGNING FIELD-SYMBOL(<ls_head>)
         WHERE line+0(13) = '&SO_FILENAME='.

      ev_filename = <ls_head>-line.
      SHIFT ev_filename LEFT BY 13 PLACES ##NUMBER_OK.
      EXIT.

    ENDLOOP.

    IF ev_filename IS INITIAL.
      RETURN.
    ENDIF.

    " ______________________________
    " Uzantı

    IF    ev_extension IS REQUESTED
       OR ev_mimetype  IS REQUESTED.

      split_filename( EXPORTING iv_filename  = ev_filename
                      IMPORTING ev_extension = ev_extension ).

    ENDIF.

    " ______________________________
    " Mimetype

    IF     ev_mimetype  IS REQUESTED
       AND ev_extension IS NOT INITIAL.

      CALL FUNCTION 'SDOK_MIMETYPE_GET'
        EXPORTING extension = ev_extension
        IMPORTING mimetype  = ev_mimetype.

    ENDIF.
  ENDMETHOD.

  METHOD get_doc_content.
    DATA lt_bdn_con TYPE tt_bdn_con.

    ASSIGN gt_doc_content_cache[ KEY primary_key COMPONENTS content_key = is_key ]
           TO FIELD-SYMBOL(<ls_cache>).

    IF sy-subrc <> 0.

      DATA(ls_cache) = VALUE t_doc_content_cache( content_key = is_key ).

      TRY.

          CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
            EXPORTING  classname          = ls_cache-content_key-classname " LIKP
                       objkey             = ls_cache-content_key-objkey " 8800000077
            TABLES     gos_connections    = lt_bdn_con
            EXCEPTIONS no_objects_found   = 1
                       internal_error     = 2
                       internal_gos_error = 3
                       OTHERS             = 4
            ##FM_SUBRC_OK.

          zcx_bc_function_subrc=>raise_if_sysubrc_not_initial( 'BDS_GOS_CONNECTIONS_GET' ).

          ls_cache-content = VALUE #( FOR ls_bdn_con IN lt_bdn_con
                                      ( gos_conn = ls_bdn_con ) ).

          LOOP AT ls_cache-content ASSIGNING FIELD-SYMBOL(<ls_content>).

            DATA(lv_docid) = CONV sofolenti1-doc_id( <ls_content>-gos_conn-loio_id ).

            CALL FUNCTION 'SO_DOCUMENT_READ_API1'
              EXPORTING  document_id                = lv_docid
              IMPORTING  document_data              = <ls_content>-doc_data
              TABLES     object_header              = <ls_content>-obj_header
                         object_content             = <ls_content>-txt_content
                         contents_hex               = <ls_content>-hex_content
              EXCEPTIONS document_id_not_exist      = 1
                         operation_no_authorization = 2
                         x_error                    = 3
                         OTHERS                     = 4.

            IF sy-subrc <> 0.
              DELETE ls_cache-content.
              CONTINUE. " Ilımlı yaklaşıyoruz
            ENDIF.

            extract_file_from_obj_header( EXPORTING it_head      = <ls_content>-obj_header
                                          IMPORTING ev_filename  = <ls_content>-filename
                                                    ev_extension = <ls_content>-extension
                                                    ev_mimetype  = <ls_content>-mime_type ).

            <ls_content>-hex_string = cl_bcs_convert=>solix_to_xstring(
                                          it_solix = <ls_content>-hex_content
                                          iv_size  = CONV #( <ls_content>-doc_data-doc_size ) ).

          ENDLOOP.

        CATCH zcx_bc_gos_doc_content INTO ls_cache-cx ##NO_HANDLER.
        CATCH cx_root INTO DATA(lo_diaper).
          ls_cache-cx = NEW #( objectid = |{ is_key-classname } { is_key-objkey }|
                               previous = lo_diaper ).
      ENDTRY.

      INSERT ls_cache
             INTO TABLE gt_doc_content_cache
             ASSIGNING <ls_cache>.

    ENDIF.

    IF <ls_cache>-cx IS NOT INITIAL.
      RAISE EXCEPTION <ls_cache>-cx.
    ENDIF.

    rt_content = <ls_cache>-content.
  ENDMETHOD.

  METHOD split_filename.
    DATA lt_split TYPE tt_string.

    CLEAR:
      ev_name,
      ev_extension.

    SPLIT iv_filename AT '.' INTO TABLE lt_split.

    LOOP AT lt_split ASSIGNING FIELD-SYMBOL(<lv_split>).

      IF sy-tabix = lines( lt_split ).
        ev_extension = <lv_split>.
      ELSE.
        ev_name =
          |{ ev_name }| &&
          |{ COND #( WHEN ev_name IS NOT INITIAL THEN '.' ) }| &&
          |{ <lv_split> }|.
      ENDIF.

    ENDLOOP.

    IF ev_name IS INITIAL.
      ev_name = iv_filename.
      ev_extension = space.
    ENDIF.

    ev_extension = to_upper( ev_extension ).
  ENDMETHOD.

  METHOD get_url_list.
    rt_url = VALUE #( FOR _ls_content IN get_doc_content( is_key )
                      WHERE ( doc_data-obj_type = c_object_type_url )
                      ( description = _ls_content-doc_data-obj_descr
                        address     = zcl_bc_text_toolkit=>remove_text_in_string(
                                          iv_string = VALUE #( _ls_content-txt_content[ 1 ]-line DEFAULT space )
                                          iv_remove = c_url_prefix ) ) ).
  ENDMETHOD.
ENDCLASS.

PARAMETERS obj_type TYPE string.
PARAMETERS obj_key TYPE string.
PARAMETERS path TYPE string LOWER CASE.

START-OF-SELECTION.
  TRY.
      DATA(solix_tab) = VALUE solix_tab( ).
      cl_gui_frontend_services=>gui_upload( EXPORTING  filename   = path
                                                       filetype   = 'BIN'
                                            IMPORTING  filelength = DATA(filelength)
                                            CHANGING   data_tab   = solix_tab
                                            EXCEPTIONS OTHERS     = 19 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                DISPLAY LIKE sy-msgty.
        STOP.
      ENDIF.
      DATA(xstring) = cl_bcs_convert=>solix_to_xstring( solix_tab ).

      zcl_bc_gos_toolkit=>attach_doc( is_key         = VALUE #( classname = obj_type
                                                                objkey    = obj_key )
                                      iv_filename    = substring_after( val = path
                                                                        sub = '\'
                                                                        occ = -1 ) " substring after last occurrence of '\'
                                      iv_description = 'dummy'
                                      iv_hex_string  = xstring  ).
      COMMIT WORK.

    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
