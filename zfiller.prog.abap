
*&---------------------------------------------------------------------**&---------------------------------------------------------------------*
*& Report  ZFILLER
*&
*&---------------------------------------------------------------------*
*& Developed by Markin Andrey 24.06.2008
*& Changed by Mathias Schollweck 05.01.2011 - "Draw" situation find
*&---------------------------------------------------------------------*
REPORT  ZFILLER NO STANDARD PAGE HEADING.

TYPES: BEGIN OF wa_move,
         left, right, up, down,
       END OF wa_move,
       BEGIN OF wa_color,
         x     TYPE i,
         y     TYPE i,
         color TYPE i,
         claw  TYPE c,
         check TYPE c,
         discrx TYPE i,
         discry TYPE i,
         cost  TYPE p DECIMALS 1.
INCLUDE TYPE wa_move.
TYPES: END OF wa_color,
       tab_color TYPE TABLE OF wa_color.
CONSTANTS: human VALUE 'H',
           alien VALUE 'A',
           draw  VALUE 'D'.                           "INSERT MS

DATA: lt_colors TYPE TABLE OF wa_color WITH KEY x y,
      ls_color TYPE wa_color,
      ls_move TYPE wa_move.

DATA: xpos TYPE i,
      next_color TYPE i.

DATA: human_color TYPE i,
      alien_color TYPE i,
      turn.

DATA: rnd LIKE  datatype-integer2,
      compsumm TYPE i, humsumm TYPE i.

DATA: x TYPE i, y TYPE i, l TYPE i.

DATA: row TYPE i, col TYPE i, control_line TYPE i.
DATA: comppoint TYPE TABLE OF wa_color,
      currpoint TYPE wa_color,
      alx TYPE i, aly TYPE i,
      summ TYPE i, pointsumm TYPE i.

DATA: BEGIN OF dim_color OCCURS 0,
        color TYPE i,
        summ  TYPE i,
        step  TYPE i,
        cost  TYPE i,
      END OF dim_color,
      currcolor LIKE dim_color OCCURS 0 WITH HEADER LINE.

DATA: tmp_wa TYPE wa_color,
      tmpx TYPE i, tmpy TYPE i,
      fieldname TYPE string.
FIELD-SYMBOLS: <fs>.

DATA: x1 TYPE i, x2 TYPE i, x3 TYPE i,
      y1 TYPE i, y2 TYPE i, y3 TYPE i.
DATA: in.

PARAMETERS: min RADIOBUTTON GROUP size,  "Text: 12*12 fields
            med RADIOBUTTON GROUP size,  "Text: 24*18 fields
            max RADIOBUTTON GROUP size.  "Text: 48*24 fields

DEFINE write_color.
  data: fx type i.
  fx = &1 * 2.
  case &2.
    when 1. write: at fx '  ' color 1.
    when 2. write: at fx '  ' color 2.
    when 3. write: at fx '  ' color 3.
    when 4. write: at fx '  ' color 4.
    when 5. write: at fx '  ' color 5.
    when 6. write: at fx '  ' color 6.
    when 7. write: at fx '  ' color 7.
  endcase.
END-OF-DEFINITION.

DEFINE check_color.
  case &1.
    when 4 or 5.   next_color = 1.
    when 12 or 13. next_color = 2.
    when 20 or 21. next_color = 3.
    when 28 or 29. next_color = 4.
    when 36 or 37. next_color = 5.
    when 44 or 45. next_color = 6.
    when 52 or 53. next_color = 7.
  endcase.
END-OF-DEFINITION.

DEFINE write_frame.
  x = sy-colno. y = sy-linno.
  write: at /x '|' , &1 color &2 hotspot on, '|' .
  l = sy-colno - x - 1.
  y = y - 1. skip to line y. position x.
  uline at x(l).
  y = y + 2. skip to line y. position x.
  uline at x(l).
  y = y - 1. x = sy-colno. skip to line y. position x.
END-OF-DEFINITION.

DEFINE check_position.
  case &1.
    when 'LEFT'.  tmpx = &2-x - 1.  tmpy = &2-y.
    when 'RIGHT'. tmpx = &2-x + 1.  tmpy = &2-y.
    when 'DOWN'.  tmpx = &2-x.  tmpy = &2-y - 1.
    when 'UP'.    tmpx = &2-x.  tmpy = &2-y + 1.
  endcase.
  concatenate &3 '-' &1 into fieldname.
  assign (fieldname) to <fs>.
  check <fs> is assigned.
  if tmpx < 1 or tmpx > col or tmpy < 1 or tmpy > row.
    <fs> = 'X'.
  else.
    read table &4 into tmp_wa with key x = tmpx y = tmpy.
    if sy-subrc = 0.
      if tmp_wa-color = &2-color.
        <fs> = 'X'.
        tmp_wa-color = &2-color.
        tmp_wa-claw = &5.
      endif.

      clear in.
      perform get_point_cost using tmpx tmpy
                             changing in.
      if in is not initial.
        tmp_wa-cost = '1.5'.
      else.
        tmp_wa-cost = 1.
      endif.

      modify &4 from tmp_wa
        transporting left right up down color claw
        where x = tmp_wa-x and y = tmp_wa-y.
    endif.
  endif.
END-OF-DEFINITION.


INITIALIZATION.
  turn = human.

START-OF-SELECTION.
  SET TITLEBAR 'NEW'. "Text: Filler by John Doe
  PERFORM generate_level.
  PERFORM draw_level.
  PERFORM get_color.
  PERFORM check_complete.

* User-commands processing
AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'COMP_TURN'.
      PERFORM analize_color TABLES lt_colors
                            CHANGING next_color.
      PERFORM set_color USING alien next_color.
      alien_color = next_color.
      PERFORM draw_level.
      PERFORM check_complete.
      turn = human.

  ENDCASE.

AT LINE-SELECTION.
  CHECK turn EQ human.
  control_line = row + 5.
  IF sy-lilli EQ control_line.
    CLEAR next_color.
    xpos = sy-cucol - 1.
    check_color xpos.
    CHECK next_color IS NOT INITIAL.
    IF next_color = human_color OR next_color = alien_color.
      MESSAGE 'Can not set this color. Re-select another color.' TYPE 'S'.
      EXIT.
    ENDIF.
    PERFORM set_color USING human next_color.
    human_color = next_color.
    PERFORM draw_level.
    PERFORM check_complete.

    turn = alien.
    CALL FUNCTION 'RFC_PING_AND_WAIT' STARTING NEW TASK '001'
      PERFORMING task_end ON END OF TASK
      EXPORTING
        seconds               = 1 " Refresh time
        busy_waiting          = space
      EXCEPTIONS
        resource_failure      = 1
        communication_failure = 2
        system_failure        = 3
        OTHERS                = 4.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  TASK_END
*&---------------------------------------------------------------------*
FORM task_end USING task.
  SET USER-COMMAND 'COMP_TURN'.
ENDFORM.                    "TASK_END

*&---------------------------------------------------------------------*
*&      Form  CREATE_LEVEL
*&---------------------------------------------------------------------*
FORM generate_level.
  DATA: pv1 TYPE i, pv2 TYPE p,
        value TYPE p DECIMALS 2.
  IF min IS NOT INITIAL.
    row = 12. col = 12.
  ELSEIF med IS NOT INITIAL.
    row = 18. col = 24.
  ELSEIF max IS NOT INITIAL.
    row = 24. col = 48.
  ENDIF.
  DO row TIMES.
    ls_color-y = ls_color-y + 1.
    CLEAR ls_color-x.
    DO col TIMES.
      ls_color-x = ls_color-x + 1.
      CALL FUNCTION 'RANDOM_I2'
        EXPORTING
          rnd_min   = 1
          rnd_max   = 7
        IMPORTING
          rnd_value = rnd.
      ls_color-color = rnd.

      value = ls_color-x / 4.
      pv1 = TRUNC( value ).
      pv2 = ls_color-x MOD 4.
      IF pv2 IS NOT INITIAL.
        ls_color-discrx = pv1 + 1.
      ELSE.
        ls_color-discrx = pv1.
      ENDIF.

      value = ls_color-y / 4.
      pv1 = TRUNC( value ).
      pv2 = ls_color-y MOD 4.
      IF pv2 IS NOT INITIAL.
        ls_color-discry = pv1 + 1.
      ELSE.
        ls_color-discry = pv1.
      ENDIF.

      APPEND ls_color TO lt_colors.
    ENDDO.
  ENDDO.

* Check colors are different.
  READ TABLE lt_colors INTO ls_color WITH KEY x = col y = 1.
  alien_color = ls_color-color.

  READ TABLE lt_colors INTO ls_color WITH KEY x = 1 y = row.
  human_color = ls_color-color.

  WHILE alien_color = human_color.
    CALL FUNCTION 'RANDOM_I2'
      EXPORTING
        rnd_min   = 1
        rnd_max   = 7
      IMPORTING
        rnd_value = rnd.
    human_color = rnd.
  ENDWHILE.

  ls_color-color = human_color.
  MODIFY TABLE lt_colors FROM ls_color.

  DESCRIBE TABLE lt_colors LINES pointsumm.

ENDFORM.                    " CREATE_LEVEL

*&---------------------------------------------------------------------*
*&      Form  DRAW_LEVEL
*&---------------------------------------------------------------------*
FORM draw_level .
  DATA: nx TYPE i,
        ypos TYPE i,
        curr_y TYPE i.

  sy-lsind = 0.
* Create color level
  SORT lt_colors BY y x.
  l = col * 2 + 2.
  ULINE AT 1(l).
  LOOP AT lt_colors INTO ls_color.
    IF ls_color-y <> curr_y .
      WRITE AT 1 '|'.
      curr_y = ls_color-y.
    ENDIF.
    ypos = ls_color-y + 1.
    SKIP TO LINE ypos.
    POSITION ls_color-x.
    write_color ls_color-x ls_color-color.
    nx = ls_color-x * 2 + 2.
    WRITE AT nx '|'.
  ENDLOOP.
  POSITION 1. WRITE '|'. NEW-LINE.
  ULINE AT 1(l).

* Set color for selection
  WRITE: /, /.
  IF alien_color NE 1 AND human_color NE 1.
    sy-colno = 2. write_frame '  ' 1.
  ENDIF.
  IF alien_color NE 2 AND human_color NE 2.
    sy-colno = 10. write_frame '  ' 2.
  ENDIF.
  IF alien_color NE 3 AND human_color NE 3.
    sy-colno = 18. write_frame '  ' 3.
  ENDIF.
  IF alien_color NE 4 AND human_color NE 4.
    sy-colno = 26. write_frame '  ' 4.
  ENDIF.
  IF alien_color NE 5 AND human_color NE 5.
    sy-colno = 34. write_frame '  ' 5.
  ENDIF.
  IF alien_color NE 6 AND human_color NE 6.
    sy-colno = 42. write_frame '  ' 6.
  ENDIF.
  IF alien_color NE 7 AND human_color NE 7.
    sy-colno = 50. write_frame '  ' 7.
  ENDIF.

ENDFORM.                    " DRAW_LEVEL


*&---------------------------------------------------------------------*
*&      Form  FILL_COLOR
*&---------------------------------------------------------------------*
FORM fill_color  USING    claw.
  DATA: flag.
  CLEAR flag.
  WHILE flag IS INITIAL.
    flag = 'X'.
    LOOP AT lt_colors INTO ls_color
      WHERE claw = claw AND check IS INITIAL.
      MOVE-CORRESPONDING ls_color TO ls_move.
      CLEAR flag.
      " Check position
      IF ls_move NE 'XXXX'.
        check_position 'LEFT' ls_color 'LS_COLOR' lt_colors claw.
        check_position 'RIGHT' ls_color 'LS_COLOR' lt_colors claw.
        check_position 'DOWN' ls_color 'LS_COLOR' lt_colors claw.
        check_position 'UP' ls_color 'LS_COLOR' lt_colors claw.
      ENDIF.
      ls_color-check = 'X'.
      MODIFY lt_colors FROM ls_color.
    ENDLOOP.
  ENDWHILE.
  ls_color-check = space.
  MODIFY lt_colors FROM ls_color TRANSPORTING check
    WHERE claw = claw.
ENDFORM.                    " FILL_COLOR


*&---------------------------------------------------------------------*
*&      Form  GET_COLOR
*&---------------------------------------------------------------------*
FORM get_color .
* Get current colors.
  READ TABLE lt_colors INTO ls_color WITH KEY x = col y = 1.
  alien_color = ls_color-color.
  ls_color-claw = alien.
  MODIFY TABLE lt_colors FROM ls_color.
  PERFORM fill_color USING alien.

  READ TABLE lt_colors INTO ls_color WITH KEY x = 1 y = row.
  human_color = ls_color-color.
  ls_color-claw = human.
  MODIFY TABLE lt_colors FROM ls_color.
  PERFORM fill_color USING human.
ENDFORM.                    " GET_COLOR


*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
FORM set_color USING claw color.
  LOOP AT lt_colors INTO ls_color WHERE claw = claw.
    ls_color-color = color.
    MODIFY lt_colors FROM ls_color.
  ENDLOOP.
  PERFORM fill_color USING claw.
ENDFORM.                    " SET_COLOR


*&---------------------------------------------------------------------*
*&      Form  ANALIZE_COLOR
*&---------------------------------------------------------------------*
FORM analize_color  TABLES dimdata TYPE tab_color
                    CHANGING p_next_color.

  DATA: flag, xline TYPE i, aliensumm TYPE i.
  DATA: colortab TYPE tab_color.

  CLEAR flag.

  CLEAR p_next_color.
  LOOP AT dimdata INTO ls_color
    WHERE claw EQ alien.
    aliensumm = aliensumm + 1.
    APPEND ls_color TO colortab.
  ENDLOOP.

  SORT colortab BY discrx discry.
  DELETE ADJACENT DUPLICATES FROM colortab COMPARING discrx discry.

  LOOP AT colortab INTO currpoint.
    alx = currpoint-discrx.
    aly = currpoint-discry.
    LOOP AT dimdata INTO ls_color
      WHERE discrx EQ alx AND discry EQ aly
      AND   claw NE alien. "EQ SPACE.
      currcolor-color = ls_color-color.
      APPEND currcolor.
    ENDLOOP.
  ENDLOOP.
  SORT currcolor BY color.

  LOOP AT currcolor.
    summ = summ + 1.
    AT END OF color.
      dim_color-color = currcolor-color.
      dim_color-summ = summ.
      APPEND dim_color.
      CLEAR summ.
    ENDAT.
  ENDLOOP.

  LOOP AT dim_color.
    comppoint[] = dimdata[].
    CLEAR flag.
    WHILE flag IS INITIAL.
      flag = 'X'.
      LOOP AT comppoint INTO currpoint
        WHERE claw = alien
        AND ( up IS INITIAL OR down IS INITIAL
                OR left IS INITIAL OR right IS INITIAL )
        AND check IS INITIAL.
        CLEAR flag.
        currpoint-color = dim_color-color.
        check_position 'LEFT' currpoint 'CURRPOINT' comppoint alien.
        check_position 'RIGHT' currpoint 'CURRPOINT' comppoint alien.
        check_position 'DOWN' currpoint 'CURRPOINT' comppoint alien.
        check_position 'UP' currpoint 'CURRPOINT' comppoint alien.
        currpoint-check = 'X'.
        MODIFY comppoint FROM currpoint TRANSPORTING check.
      ENDLOOP.
    ENDWHILE.

    DELETE comppoint WHERE claw NE alien.
    DESCRIBE TABLE comppoint LINES xline.
    dim_color-step = xline - aliensumm.
    CLEAR dim_color-cost.
    LOOP AT comppoint INTO currpoint.
      dim_color-cost = dim_color-cost + currpoint-cost.
    ENDLOOP.
    MODIFY dim_color.
  ENDLOOP.

  DELETE dim_color WHERE color EQ alien_color.
  DELETE dim_color WHERE color EQ human_color.

  DATA: mostpoint LIKE dim_color,
        mostcost LIKE dim_color.

  SORT dim_color BY step DESCENDING.
  READ TABLE dim_color INTO mostpoint INDEX 1.

  SORT dim_color BY cost DESCENDING.
  READ TABLE dim_color INTO mostcost INDEX 1.

  IF mostpoint-step > mostcost-cost.
    p_next_color = mostpoint-color.
  ELSE.
    p_next_color = mostcost-color.
  ENDIF.
ENDFORM.                    " ANALIZE_COLOR


*&---------------------------------------------------------------------*
*&      Form  CHECK_COMPLETE
*&---------------------------------------------------------------------*
FORM check_complete .
  DATA: fullhumsumm TYPE i,
        fullcompsum TYPE i.
  DATA: win.

  CLEAR: compsumm, humsumm.
  LOOP AT lt_colors INTO ls_color.
    IF ls_color-claw EQ human.
      humsumm = humsumm + 1.
    ELSEIF ls_color-claw EQ alien.
      compsumm = compsumm + 1.
    ENDIF.
  ENDLOOP.

  DATA: xtext TYPE string,
        proc TYPE p DECIMALS 2,
        tproc TYPE string.
  SKIP TO LINE 2.
  MOVE humsumm TO xtext.
  proc = humsumm / pointsumm * 100. MOVE proc TO tproc.
  IF proc > '50.00'.
    win = human.
  ELSEIF proc = '50.00'.                                 "INSERT MS
    win = draw.                                          "INSERT MS
  ENDIF.
  CONCATENATE tproc '%' INTO tproc.
  CONCATENATE 'Human:' xtext '/' tproc
    INTO xtext SEPARATED BY space.
  sy-colno = col * 2 + 6.
  PERFORM write_color_box USING human_color.
  sy-colno = col * 2 + 14. write_frame xtext 2.

  SKIP TO LINE 5.
  MOVE compsumm TO xtext.
  proc = compsumm / pointsumm * 100. MOVE proc TO tproc.
  IF proc > '50.00'.
    win = alien.
  ENDIF.
  IF win = draw AND proc <> '50.00'.                   "INSERT MS
    CLEAR win.                                         "INSERT MS
  ENDIF.                                               "INSERT MS

  CONCATENATE tproc '%' INTO tproc.
  CONCATENATE 'Computer:' xtext '/' tproc
    INTO xtext SEPARATED BY space.
  sy-colno = col * 2 + 6.
  PERFORM write_color_box USING alien_color.
  sy-colno = col * 2 + 14. write_frame xtext 2.

  IF win = human.
    MESSAGE 'Human win!' TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ELSEIF win = alien.
    MESSAGE 'Computer win!' TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ELSEIF win = draw.                                  "INSERT MS
    MESSAGE 'Draw!!' TYPE 'I'.                        "INSERT MS
    LEAVE LIST-PROCESSING.                            "INSERT MS
  ENDIF.

ENDFORM.                    " CHECK_COMPLETE

*&---------------------------------------------------------------------*
*&      Form  WRITE_COLOR_BOX
*&---------------------------------------------------------------------*
FORM write_color_box  USING    color.
  CASE color.
    WHEN 1. write_frame '  ' 1.
    WHEN 2. write_frame '  ' 2.
    WHEN 3. write_frame '  ' 3.
    WHEN 4. write_frame '  ' 4.
    WHEN 5. write_frame '  ' 5.
    WHEN 6. write_frame '  ' 6.
    WHEN 7. write_frame '  ' 7.
  ENDCASE.
ENDFORM.                    " WRITE_COLOR_BOX

*&---------------------------------------------------------------------*
*&      Form  GET_POINT_COST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_point_cost USING point_x point_y
                    CHANGING point_in.

  DATA: a TYPE p DECIMALS 3,
        b TYPE p DECIMALS 3,
        c TYPE p DECIMALS 3,
        p TYPE p DECIMALS 3,
        s TYPE i,
        s1 TYPE p DECIMALS 3,
        s2 TYPE p DECIMALS 3,
        s3 TYPE p DECIMALS 3,
        summs TYPE i.

  DATA: x TYPE i, y TYPE i.

  DEFINE get_vektor.
    x = &1 - &3. x = x * x.
    y = &2 - &4. y = y * y.
    &5 = x + y. &5 = sqrt( &5 ).
  END-OF-DEFINITION.

  DEFINE get_square.
    clear: a, b, c, p.
    get_vektor &1 &2 &3 &4 a.
    get_vektor &1 &2 &5 &6 b.
    get_vektor &5 &6 &3 &4 c.
    p = a + b + c. p = p / 2.
    &7 = p *  ( p - a ) * ( p - b ) * ( p - c ).
    &7 = sqrt( &7 ).
  END-OF-DEFINITION.

  x1 = col. y1 = 0.
  x2 = 0. y2 = row / 2.
  x3 = col / 2. y3 = row.

  " Get big triangle triangle square
  get_square x1 y1 x2 y2 x3 y3 s.

  get_square point_x point_y x2 y2 x3 y3 s1.
  get_square x1 y1 point_x point_y x3 y3 s2.
  get_square x1 y1 x2 y2 point_x point_y s3.

  summs = s1 + s2 + s3.

  IF s = summs.
    point_in = 'X'.
  ENDIF.

ENDFORM.                    "GET_POINT_COST
