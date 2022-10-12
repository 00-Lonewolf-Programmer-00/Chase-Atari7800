  ;https://atariage.com/forums/topic/272112-sprite-data-to-color-mapping-updated/
;https://atariage.com/forums/topic/270167-7800basic-for-newbies-a-tutorial-part-1/
;https://atariage.com/forums/topic/270168-7800basic-for-newbies-a-tutorial-part-2/
;https://atariage.com/forums/topic/270275-7800basic-for-newbies-a-tutorial-part-3/
;https://atariage.com/forums/blogs/entry/11915-7800basic-tutorial-sprite-and-tile-interactions/


;https://atariage.com/forums/topic/283284-how-to-use-tiles-maps/?do=findComment&comment=4120119
;https://atariage.com/forums/topic/296568-7800basic-and-png-images/?do=findComment&comment=4452771
;sprite width
;https://atariage.com/forums/topic/290705-7800basic-graphics-questions/?do=findComment&comment=4270632
;tilemap in ram
;https://atariage.com/forums/topic/311732-newbie-7800basic-questions/?do=findComment&comment=4643137
;tiled width
;https://atariage.com/forums/topic/296568-7800basic-and-png-images/?do=findComment&comment=4643815
;graphics weird storage
;https://atariage.com/forums/topic/290705-7800basic-graphics-questions/?do=findComment&comment=4271466
  
  
  
  
  
  
  set pokeysupport on
  set hssupport $1234
 ;set romsize 48k ; Formats larger than 48k are bankswitching formats
 set romsize 128kRAM ;extra 16k of RAM from $4000-$7fff.
 set dlmemory $5000 $7fff
 set tv ntsc
 set pauseroutine off
 set zoneheight 16
 set tallsprite on
 set collisionwrap off
 set doublewide on ;This tells MARIA to fetch 2 bytes of character data for each character you plot, effectively making the characters twice as wide.
 set screenheight 224 ;208
 set mcpdevcart off
 set canary off
 ;set canary off
 ;set deprecated frameheight
   ;adjustvisible 1 14
 set plotvalueonscreen on
 /*
 This tells 7800basic that when the plotvalue command is used, it should update the screen immediately, 
 instead of waiting for the screen to complete drawing. This will save precious off-screen cycles for plotting other objects, 
 like sprites.
 */

 set basepath graphics ; This sets the compiler to look for all of your image files in <root folder>\basepath.

 displaymode 320B ;Sets the MARIA display mode for the game
 

 bank 1
_restart
/*
 ;blue tile
 P0C1 = $00
 P0C2 = $00
 P0C3 = $00
 ;purple tile
 P1C1 = $00
 P1C2 = $00
 P1C3 = $00
 ;dots
 P2C1 = $00
 P2C2 = $00
 P2C3 = $00
 ;player
 P3C1 = $00
 P3C2 = $d9
 P3C3 = $0f
 ;enemy
 P4C1 = $00
 P4C2 = $39 ;orange
 P4C3 = $00 ;$0f

 P5C1 = $00
 P5C2 = $59 ;lila
 P5C3 = $0f

 P6C1 = $00
 P6C2 = $99 ;blå
 P6C3 = $0f
*/
 rem characters
; remember that 320B only has two palettes dummy, 0 and 4
 incgraphic numbers.png 320B 0 3 0 0 4 
 incgraphic text.png 320B 0 2 0 0 0 

 ;for titlescreen only
 rem empty


;#region " MACROS "
 asm
 ; ** import the module. This one can go in the middle of the code flow.
 ;include updateobjects.asm
end

 asm
 ; --------------------------------------------------------------------------
; UpdateObjects assembly code provided by RevEng (Mike Saarna)
;   
;   allows enabling, disabling, setting palette, or graphic for any object
;   in the display lists, given the zone number (Y) and the number of the 
;   object in that zone. (X)
;   
; updated 20200529 - clean-up for release, renaming and changing of routines
; to macros, support for different zone heights in DisableObject and 
; EnableObject.  Addition of SetObjectX and SetObjectY macros.
;   
; updated 20200301 - Update to support full extended screen height, and up
; to 40 objects in the zone. Extend the x5table if you need more objects, 
; but really, Maria won't be able to likely draw more than 40.
; -------------------------------------------------------------------------- 





 ; SetObjectY - X=object_X Y=object_Y {1}=y coordinate in zone
 MAC SetObjectY
 jsr SetObjectPointer
 ldy #2 ; High Address of graphics
 lda (temp1),y
 if WZONEHEIGHT = 8
     and #%11111000
 endif
 if WZONEHEIGHT = 16
     and #%11110000
 endif
 ora {1}
 sta (temp1),y
 ENDM





 MAC SetObjectPalette
  ; SetObjectPalette - X=object_X Y=object_Y {1}=palette
 jsr SetObjectPointer
 ldy #3 ; palette+width is the 4th byte in the dl object
 lda (temp1),y
 and #%00011111
 ldx {1}
 ora x32table,x
 sta (temp1),y
 ENDM




 MAC SetObjectImageLo
  ; SetObjectImageLo - X=object_X Y=object_Y {1}=index in gfx block
 jsr SetObjectPointer
 ldy #0 ; low address of gfx is the 1st byte in the dl object
 lda {1}
 sta (temp1),y
 ENDM


  ; SetObjectImage - X=object_X Y=object_Y {1}=gfx_label
 MAC SetObjectImage
 jsr SetObjectPointer
 ldy #0 ; lo Address of graphics
 lda #<{1}
 sta (temp1),y
 ldy #2 ; hi Address of graphics
 lda #>{1}
 sta (temp1),y
 ENDM

 ; ********* utility functions and structures follow *********

 jmp UpdateObjectsEnd

SetObjectPointer ; called with X=object_X Y=object_Y
 clc
 lda ZONESTARTLO,y
 adc x5table,x
 sta temp1 
 lda ZONESTARTHI,y
 adc #0
 sta temp2
 rts

x5table ; enough entries to support 40 objects per dl
MULT5IDX SET 0
 REPEAT 40
 .byte MULT5IDX
MULT5IDX SET (MULT5IDX+5)
 REPEND

x32table ; 32 multiplaction for palette values
MULT32IDX SET 0
 REPEAT 8
 .byte MULT32IDX
MULT32IDX SET (MULT32IDX+32)
 REPEND

ZONESTARTLO
 .byte <ZONE0ADDRESS
 .byte <ZONE1ADDRESS
 .byte <ZONE2ADDRESS
 .byte <ZONE3ADDRESS
 .byte <ZONE4ADDRESS
 .byte <ZONE5ADDRESS
 .byte <ZONE6ADDRESS
 .byte <ZONE7ADDRESS
 .byte <ZONE8ADDRESS
 .byte <ZONE9ADDRESS
 .byte <ZONE10ADDRESS
 .byte <ZONE11ADDRESS
 ifconst ZONE12ADDRESS
   .byte <ZONE12ADDRESS
 endif
 ifconst ZONE13ADDRESS
   .byte <ZONE13ADDRESS
 endif
 ifconst ZONE14ADDRESS
   .byte <ZONE14ADDRESS
 endif
 ifconst ZONE15ADDRESS
   .byte <ZONE15ADDRESS
 endif
 ifconst ZONE16ADDRESS
   .byte <ZONE16ADDRESS
 endif
 ifconst ZONE17ADDRESS
   .byte <ZONE17ADDRESS
 endif
 ifconst ZONE18ADDRESS
   .byte <ZONE18ADDRESS
 endif
 ifconst ZONE19ADDRESS
   .byte <ZONE19ADDRESS
 endif
 ifconst ZONE20ADDRESS
   .byte <ZONE20ADDRESS
 endif
 ifconst ZONE21ADDRESS
   .byte <ZONE21ADDRESS
 endif
 ifconst ZONE22ADDRESS
   .byte <ZONE22ADDRESS
 endif
 ifconst ZONE23ADDRESS
   .byte <ZONE23ADDRESS
 endif
 ifconst ZONE24ADDRESS
   .byte <ZONE24ADDRESS
 endif
 ifconst ZONE25ADDRESS
   .byte <ZONE25ADDRESS
 endif
 ifconst ZONE26ADDRESS
   .byte <ZONE26ADDRESS
 endif
 ifconst ZONE27ADDRESS
   .byte <ZONE27ADDRESS
 endif

ZONESTARTHI
 .byte >ZONE0ADDRESS
 .byte >ZONE1ADDRESS
 .byte >ZONE2ADDRESS
 .byte >ZONE3ADDRESS
 .byte >ZONE4ADDRESS
 .byte >ZONE5ADDRESS
 .byte >ZONE6ADDRESS
 .byte >ZONE7ADDRESS
 .byte >ZONE8ADDRESS
 .byte >ZONE9ADDRESS
 .byte >ZONE10ADDRESS
 .byte >ZONE11ADDRESS
 ifconst ZONE12ADDRESS
   .byte >ZONE12ADDRESS
 endif
 ifconst ZONE13ADDRESS
   .byte >ZONE13ADDRESS
 endif
 ifconst ZONE14ADDRESS
   .byte >ZONE14ADDRESS
 endif
 ifconst ZONE15ADDRESS
   .byte >ZONE15ADDRESS
 endif
 ifconst ZONE16ADDRESS
   .byte >ZONE16ADDRESS
 endif
 ifconst ZONE17ADDRESS
   .byte >ZONE17ADDRESS
 endif
 ifconst ZONE18ADDRESS
   .byte >ZONE18ADDRESS
 endif
 ifconst ZONE19ADDRESS
   .byte >ZONE19ADDRESS
 endif
 ifconst ZONE20ADDRESS
   .byte >ZONE20ADDRESS
 endif
 ifconst ZONE21ADDRESS
   .byte >ZONE21ADDRESS
 endif
 ifconst ZONE22ADDRESS
   .byte >ZONE22ADDRESS
 endif
 ifconst ZONE23ADDRESS
   .byte >ZONE23ADDRESS
 endif
 ifconst ZONE24ADDRESS
   .byte >ZONE24ADDRESS
 endif
 ifconst ZONE25ADDRESS
   .byte >ZONE25ADDRESS
 endif
 ifconst ZONE26ADDRESS
   .byte >ZONE26ADDRESS
 endif
 ifconst ZONE27ADDRESS
   .byte >ZONE27ADDRESS
 endif

UpdateObjectsEnd
end
;#endregion

 ;characterset tiles1
 incmapfile titleScreen.tmx 

 dim tileMap = $2200 ;8704 in dec
 ;dim topScreenMap = $22D0 ;8912 in dec (8704 + 208)

 ;dim rememberDotPosX = $4000 
 ;dim rememberDotPosY = $5000 

 dim var100 = $22D0
 dim var101 = $22D1

 dim playerXSpeed = var100.var101

 dim var102 = $22D2
 dim var103 = $22D3

 dim enemy1XSpeed = var102.var103

 dim var104 = $22D4
 dim var105 = $22D5

 dim enemy2XSpeed = var104.var105

 dim var106 = $22D6
 dim var107 = $22D7

 dim enemy3XSpeed = var106.var107



 dim var108 = $22D8
 dim var109 = $22D9

 dim playerYSpeed = var108.var109

 dim var110 = $22DA
 dim var111 = $22DB

 dim enemy1YSpeed = var110.var111

 dim var112 = $22DC
 dim var113 = $22DD

 dim enemy2YSpeed = var112.var113

 dim var114 = $22DE
 dim var115 = $22DF ;8927

 dim enemy3YSpeed = var114.var115
 ;************


 dim var116 = $22E0
 dim var117 = $22E1
 dim var118 = $22E2
 dim score3 = var116


  dim lives2 = score3+2
  dim lives1 = score3+1

 dim livesDigits0 = $22E3
 dim livesDigits1 = livesDigits0 + 1
 dim livesDigits2 = livesDigits0 + 2

 dim bonusStageFlag = $22E6

 dim score4 = $22E7

  dim totaldots1 = score4+1 ;e8
  dim totaldots2 = score4+2 ;e9

 dim score5 = $22EA

  dim totaldots_1 = score5+1 ;eb
  dim totaldots_2 = score5+2 ;ec

 dim totaldigits0 = $22ED
 dim totaldigits1 = totaldigits0 + 1 ;ef
 dim totaldigits2 = totaldigits0 + 2 ;f0
 dim totaldigits3 = totaldigits0 + 3 ;f1
 dim totaldigits4 = totaldigits0 + 4 ;f2
 dim totaldigits5 = totaldigits0 + 5 ;f3

 dim _P0C1 = $22f4
 dim _P0C2 = $22f5
 dim _P0C3 = $22f6

 dim POKEYADR = $450
 dim TUNEAREA = $22f7

 const POKEY_ATARITODAY = 0
 const POKEY_GAMECOMPLETE = 1
 const POKEY_LEVEL1 = 2
 const POKEY_SKULL = 3
 const POKEY_TOTALTUNES = 4



  ;memcpy tileMap level1 208 ;copy entire tilemap to RAM
  ;memcpy topScreenMap topScreen1 28 ;copy top row tilemap to RAM
  

 dim customTemp1 = a
 dim customTemp2 = b
 dim customTemp3 = c
 dim customTemp4 = d
 dim customTemp5 = e

 dim customTemp6 = x
 dim customTemp7 = y

 dim doubleTemp = temp5.temp6

 dim tileMapX    = customTemp1
 dim tileMapY    = customTemp2
 dim charValue   = customTemp3
 dim spriteTileX = customTemp4
 dim spriteTileY = customTemp5

 dim enemyY = customTemp4
 dim enemyX = customTemp5


  dim playerY = f : f = 72
  dim playerX = g : g = 56

  dim playerYdouble = f.var72
  dim playerXdouble = g.var73 

  dim frameCounter = h
  dim frame = i

 dim yMoveBuff = j : j = 128
 dim xMoveBuff = k : k = 128

  dim frameCounter2 = l



 dim SOUNDZP = m.n
  dim tuneId = o
 ;dim _P0C1 = m ;: m = $85 - 5
 ;dim _P0C2 = n ;: n = $89 - 9
 ;dim _P0C3 = o ;: o = $8d - 13

 dim _P1C1 = p ;: p = $a5 - 5
 dim _P1C2 = q ;: q = $a9 - 9
 dim _P1C3 = r ;: r = $ad - 13

 dim _P2C1 = s
 dim _P2C2 = t
 dim _P2C3 = u

 
 dim _P3C3 = var3 ; used to fade in all white

 dim _P4C2 = var4
 dim _P5C2 = var5
 dim _P6C2 = var6
 dim _P3C2 = var7

 dim streakCounter = var8 : streakCounter = 0
  ;dim _lives = var8 : var8 = 3
  dim numberOfEnemies = var9 : var9 = 0

  dim level = v : v = 0
  dim dotCounter = w : w = 0


 ;gems quickscore
 dim qsdigits0 = var90 : rem ** change this to some location where you have 7 bytes free
 dim qsdigits1 = qsdigits0 + 1
 dim qsdigits2 = qsdigits0 + 2

 dim qsdigits3 = qsdigits0 + 3 : rem the slash character

 dim qsdigits4 = qsdigits0 + 4
 dim qsdigits5 = qsdigits0 + 5
 dim qsdigits6 = qsdigits0 + 6

  dim dotseaten2 = score0+2 : rem ** +2 for the lowest byte of score0 
  dim dotseaten1 = score0+1 : rem ** +1 for the second-lowest byte of score0 

  dim dotsremain2 = score1+2 : rem ** +2 for the lowest byte of score1
  dim dotsremain1 = score1+1 : rem ** +1 for the second-lowest byte of score1

   dim _sc1_1 = score1
   dim _sc1_2 = score1+1
   dim _sc1_3 = score1+2

 ;level quickscore
   dim score2 = var97 : rem ** var98 and var99 are also used for score2
   dim _sc2_1 = score2
   dim _sc2_2 = score2+1
   dim _sc2_3 = score2+2

 dim qsdigits7 = var87 : rem ** change this to some location where you have 7 bytes free
 dim qsdigits8 = qsdigits7 + 1
 dim qsdigits9 = qsdigits7 + 2



/*
 rem vars
 dim scoreText0 = var0                        
 dim scoreText1 = var1
 dim scoreText2 = var2
 ;dim scoreText3 = var3
 ;dim scoreText4 = var4
 ;dim scoreText5 = var5

 dim scoreVar0 = score0
 dim scoreVar1 = score0+1
 dim scoreVar2 = score0+2
*/
 dim enemy1X = var10 : dim enemyXarray = enemy1X
 dim enemy1Y = var20 : dim enemyYarray = enemy1Y

 dim enemy2X = var11
 dim enemy2Y = var21

 dim enemy3X = var12
 dim enemy3Y = var22

 dim enemy1Xdouble = var10.var50 : dim enemyXfractArray = var50
 dim enemy1Ydouble = var20.var60 : dim enemyYfractArray = var60

 dim enemy2Xdouble = var11.var51
 dim enemy2Ydouble = var21.var61

 dim enemy3Xdouble = var12.var52
 dim enemy3Ydouble = var22.var62

 dim enemyDirArray = var30 : var30 = 0 ;remember to zero at level change



  const _left = 1
  const _right = 2
  const _up = 3
  const _down = 4

  ;dim bottomMode = var41 : bottomMode = 16

  dim enemyPlantCountdownArray = var40 ;vad
  ;dim topMode = var44 : topMode = 16
  ;dim _320modeLength = z
  dim _P6C3 = z

  dim rememberXpos = var70
  dim rememberYpos = var71
  ;var72 playerX
  ;var73 playery
  ;var74 topscreen inttemp
  dim playerBubbleCountdown = var75
  dim restrainer = var76 : restrainer = 0

 dim wallHits = var77 ;zero level change
 dim wallLeft = var77
 dim wallRight = var77
 dim wallUp = var77
 dim wallDown = var77

 dim playerUpDown = var77
 dim playerLeftRight = var77


   dim _P7C2 = var78
  dim _P7C3 = var79




 






 const playerHeight = 16
 const playerWidth = 8

 const playerHeightMinusOne = 15
 const playerWidthMinusOne = 7

 const tileHeight = 16
 const tileWidth = 8

 const tileHeightMask = %00001111
 const tileWidthMask = %00000111




 ;tiles you can move over
 const _dotCentered = #<(dot+0)
 const _dotUp       = #<(dot2+0)
 const _bubbleCentered = #<(bubble+0)
 const _bubbleUp = #<(bubble2+0)
 const _emptyTile = #<(tiles0+0)
 const _enemy1SpawnPoint = #<(esp1+0)
 const _enemy2SpawnPoint = #<(esp2+0)
 const _enemy3SpawnPoint = #<(esp3+0)
 const _playerSpawnPoint = #<(psp+0)
 const _dotGone   = #<(tiles1+0) :  const bgTile = #<(tiles1+0) ; if peekchar > bgTile then wall
 ;wall tiles
 const _mazeTile1 = #<(tiles2+0)
 const _mazeTile2 = #<(tiles3+0)
 ;titlescreen
 const _mazeTile3 = #<(tiles4+0)

 ;number tiles
 /*
 const _zero =  #<(zero+0)
 const _one =   #<(one+0)
 const _two =   #<(two+0)
 const _three = #<(three+0)

 const _four =  #<(numbers+8)
 const _five =  #<(numbers+10)
 const _six =   #<(numbers+12)
 const _seven = #<(numbers+14)
 const _eight = #<(numbers+16)
 const _nine =  #<(numbers+18)
*/
 const screenYoffset = 16 ; start plotting the characters after the 320b topscreen
 const screenXoffset = 16

 ;const mapWidth = 16 ; two empty columns on each side
 ;const mapHeight = 13 ; one empty row on top
 ;doublebuffer on



 alphachars '0123456789LEVGMSI:/'
 tuneId = 1
 gosub PlayPokeySound
 goto _title ;bank5
 rem font


 ;gosub loadLevel

/*
 for customTemp2 = 0 to 64 ; 4
 restorescreen
 drawscreen
 next

 ;blue tile
 P0C1 = $85
 P0C2 = $89
 P0C3 = $8d
 ;purple tile
 P1C1 = $a5
 P1C2 = $a9
 P1C3 = $8d
 ;dots
 P2C1 = $d1
 P2C2 = $29 
 P2C3 = $2d
 */


main

 if playerBubbleCountdown > 0 then playerBubbleCountdown = playerBubbleCountdown - 1 : goto trapped
 ;rememberXpos = playerX
 ;rememberYpos = playerY

 if  playerUpDown{4} then gosub leftRight : gosub upDown else gosub upDown : gosub leftRight

trapped



  frameCounter = frameCounter + 1
  if frameCounter = 20 then customTemp5 = _dotCentered : customTemp4 = _bubbleUp : gosub dotsUp
  if frameCounter = 40 then frameCounter = 0 : customTemp5 = _dotUp : customTemp4 = _bubbleCentered : gosub dotsDown

  frameCounter2 = frameCounter2 + 1
  if frameCounter2 = 10 then frameCounter2 = 0 : frame{0} = !frame{0}

 ;if dotCounter > 0 && frameCounter = 1 then gosub _pull

 ;score0 = playerX
 gosub UpdateScore
 
    temp1 = rand

 customTemp2 = numberOfEnemies - 1
 if customTemp2 = 255 then skipEnemyMovementAndHitboxes
 for customTemp1 = 0 to customTemp2
 enemyY = enemyYarray[customTemp1]
 enemyX = enemyXarray[customTemp1]
 gosub enemyMovement
 ;enemyYarray[customTemp1] = enemyY
 ;enemyXarray[customTemp1] = enemyX
 next
skipEnemyMovement

 if customTemp2 >= 0 && boxcollision(playerX, playerY, 5, 9, enemy1X, enemy1Y, 5, 9) then goto looseLifeAndRestart
 if customTemp2 >= 1 && boxcollision(playerX, playerY, 5, 9, enemy2X, enemy2Y, 5, 9) then goto looseLifeAndRestart
 if customTemp2 >= 2 && boxcollision(playerX, playerY, 5, 9, enemy3X, enemy3Y, 5, 9) then goto looseLifeAndRestart
skipEnemyMovementAndHitboxes

 if playerBubbleCountdown > 0 then P3C2 = $b5 : P3C3 = $b9 else P3C2 = $d9 : P3C3 = $0f

 if numberOfEnemies = 1 then gosub restoreScreen_plotStuff_drawScreen1
 if numberOfEnemies = 2 then gosub restoreScreen_plotStuff_drawScreen2
 if numberOfEnemies = 3 then gosub restoreScreen_plotStuff_drawScreen3
 ;restorescreen
  ;gosub plotStuff

 ;drawscreen

   if switchreset then goto _restart
   if streakCounter = 200 then gosub _streakOneUp
    if dotseaten1 <> dotsremain1 then goto main
    if dotseaten2 <> dotsremain2 then goto main
   ;if dotCounter <> numberOfDots[level] then goto main 


   playsfx sfx_yahoo 
 for customTemp1 = 0 to 48
  ;restorescreen
  ;gosub plotStuff
 if numberOfEnemies = 1 then gosub restoreScreen_plotStuff_drawScreen1
 if numberOfEnemies = 2 then gosub restoreScreen_plotStuff_drawScreen2
 if numberOfEnemies = 3 then gosub restoreScreen_plotStuff_drawScreen3
  ;drawscreen
  next

   playsfx sfx_cavalry
   gosub fadeOut 
   gosub clearPalettes ;palettes are allready black, not needed
   gosub addCollectedDotsToTotal
   gosub loadLevel
 goto main


_streakOneUp
 playsfx sfx_oneup
 streakCounter = 0
 for customTemp5 = 0 to 48
 ;restorescreen
 ;gosub plotStuff
 customTemp1 = (224 / 2) - 24
 customTemp2 = (160 / 2) - 24
 if numberOfEnemies = 1 then gosub restoreScreen_plotStuff_streak_drawScreen1
 if numberOfEnemies = 2 then gosub restoreScreen_plotStuff_streak_drawScreen2
 if numberOfEnemies = 3 then gosub restoreScreen_plotStuff_streak_drawScreen3
 ;restorescreen
 ;gosub plotStuff
 ;both sprites are 32 pixel positions wide
 ;plotsprite streak 7 customTemp2  customTemp1 0 2


 ;drawscreen
 next
 score3 = score3 + 1
 livesDigits2 = (lives2 & $0f) * 2
 livesDigits1 = (lives2 / 16) * 2
 livesDigits0 = (lives1 & $0f) * 2
 return thisbank

/*
 data sfx_yahoo
 $10,$10,$00 ; version, priority, frames per chunk
 $01,$0c,$02 ; first chunk of freq,channel,volume
 $18,$04,$08
 $07,$0c,$0f
 $16,$04,$0f
 $03,$0c,$05
 $0a,$04,$06
 $15,$04,$08
 $18,$04,$04
 $18,$04,$04
 $1c,$04,$04
 $1c,$04,$04
 $1c,$04,$02
 $0b,$0c,$02
 $19,$04,$01
 $19,$04,$00
 $1c,$04,$01
 $1b,$04,$03
 $19,$04,$06
 $18,$04,$04
 $1e,$04,$06
 $19,$04,$0f
 $18,$04,$0f
 $18,$04,$0f
 $18,$04,$0b
 $18,$04,$0f
 $18,$04,$0f
 $18,$04,$0f
 $18,$04,$05
 $18,$04,$08
 $19,$04,$0f
 $1b,$04,$08
 $1b,$04,$06
 $1c,$04,$0f
 $1c,$04,$0f
 $1e,$04,$08
 $1e,$04,$0e
 $1e,$04,$0b
 $0a,$0c,$08
 $0a,$0c,$0f
 $0b,$0c,$0f
 $0b,$0c,$0e
 $0b,$0c,$09
 $0c,$0c,$06
 $0c,$0c,$02
 $0d,$0c,$03
 $0d,$0c,$03
 $0d,$0c,$01
 $0e,$0c,$00
 $17,$0c,$00
 $00,$00,$00
end
*/




looseLifeAndRestart
  score0 = 0
 playsfx sfx_ouch
 for customTemp1 = 0 to 32

     ;restorescreen
     ;gosub plotStuff
 ;drawscreen
 if numberOfEnemies = 1 then gosub restoreScreen_plotStuff_drawScreen1
 if numberOfEnemies = 2 then gosub restoreScreen_plotStuff_drawScreen2
 if numberOfEnemies = 3 then gosub restoreScreen_plotStuff_drawScreen3
 next

  playsfx sfx_hahaha2
 gosub fadeOut
 gosub clearPalettes ;not needed
 ;if _lives = 0 then goto _gameOver 
  streakCounter = 0
  if score3 = 0 && lives1 = 0 && lives2 = 0 then goto _gameOver
 ;_lives = _lives - 1
 score3 = score3 - 1
 level = level - 1
 gosub loadLevel

 goto main

upDown
 if joy0up && yMoveBuff = 128 && xMoveBuff = 128 then yMoveBuff = 128 + 16 ;then skipUp

   if yMoveBuff <= 128 then skipUp
   playerUpDown{4} = 1
   customTemp5 = playerY
   playerYdouble = playerYdouble - playerYSpeed
   if playerY <> customTemp5 then yMoveBuff = yMoveBuff - (customTemp5 - playerY)  ;else skipUp
   if yMoveBuff < 128 then playerY = playerY + (128 - yMoveBuff) : yMoveBuff = 128

   customTemp1 = tileHeight - playerY & tileHeightMask

   y = (playerY - screenYoffset) / tileHeight
   x = (playerX - screenXoffset) / tileWidth
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; top left
   if charValue > bgTile then playerY = playerY + customTemp1 : yMoveBuff = 128 : goto skipUpCheck2



   x = ((playerX - screenXoffset) + playerWidthMinusOne) / tileWidth
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; top right
   if charValue > bgTile then playerY = playerY + customTemp1 : yMoveBuff = 128
skipUpCheck2
 gosub removeCoin
 return thisbank
skipUp


   if joy0down && yMoveBuff = 128  && xMoveBuff = 128 then yMoveBuff = 128 - 16 ;then skipDown

   if yMoveBuff >= 128 then skipDown
   playerUpDown{4} = 1
   customTemp5 = playerY 
   playerYdouble = playerYdouble + playerYSpeed
   if playerY <> customTemp5 then yMoveBuff = yMoveBuff + (playerY - customTemp5)  ;else skipDown
   if yMoveBuff > 128 then playerY = playerY - (yMoveBuff - 128) : yMoveBuff = 128 ; re-align if you move 2 pixels on the 16'th step

   customTemp1 =  playerY & tileHeightMask 

   y = ((playerY - screenYoffset) + playerHeight) / tileHeight
   x = (playerX - screenXoffset) / tileWidth
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; bottom left
   if charValue > bgTile then playerY = playerY - customTemp1 : yMoveBuff = 128 : goto skipDownCheck2



   x = ((playerX - screenXoffset) + playerWidthMinusOne) / tileWidth
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; bottom right
   if charValue > bgTile then playerY = playerY - customTemp1 : yMoveBuff = 128
skipDownCheck2
 gosub removeCoin
skipDown

 return thisbank

leftRight
   if joy0left && xMoveBuff = 128 && yMoveBuff = 128 then xMoveBuff = 128 + 8 ; then skipLeft

   ;if xMoveBuff > 128 then playerX = playerX - 1 : xMoveBuff = xMoveBuff - 1
      if xMoveBuff <= 128 then skipLeft
      playerUpDown{4} = 0 ;turn on upDown priority
      customTemp5 = playerX ;remember x pos
      playerXdouble = playerXdouble - playerXSpeed ;move
      if playerX <> customTemp5 then xMoveBuff = xMoveBuff - (customTemp5 - playerX) ;if not the same, remove distance moved from buff
      if xMoveBuff < 128 then playerX = playerX + (128 - xMoveBuff) : xMoveBuff = 128 ;if moved to much, move back

;skipLeftMove
     customTemp1 = tileWidth - playerX & tileWidthMask


   x = (playerX - screenXoffset) / tileWidth
   y = (playerY - screenYoffset) / tileHeight
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; top left
   if charValue > bgTile then playerX = playerX + customTemp1 : xMoveBuff = 128 : goto skipLeftCheck2



   y = ((playerY - screenYoffset) + playerHeightMinusOne) / tileHeight
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; bottom left
   if charValue > bgTile then playerX = playerX + customTemp1 : xMoveBuff = 128
skipLeftCheck2
 gosub removeCoin
 return thisbank
skipLeft


   if joy0right && xMoveBuff = 128 && yMoveBuff = 128 then xMoveBuff = 128 - 8 ;then skipRight
  

   ;if xMoveBuff < 128 then playerXdouble = playerXdouble + 0.58 : if CARRY then xMoveBuff = xMoveBuff + 1
   
      if xMoveBuff >= 128 then skipRight
      playerUpDown{4} = 0
      customTemp5 = playerX
      playerXdouble = playerXdouble + playerXSpeed
      if playerX <> customTemp5 then xMoveBuff = xMoveBuff + (playerX - customTemp5) 
      if xMoveBuff > 128 then playerX = playerX - (xMoveBuff - 128) : xMoveBuff = 128 ; re-align if you move 2 pixels on the 8'th step
;skipRightMove

   customTemp1 =  playerX & tileWidthMask 

   x = ((playerX - screenXoffset) + playerWidth) / tileWidth
   y = (playerY - screenYoffset) / tileHeight
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; top right
   if charValue > bgTile then playerX = playerX - customTemp1 : xMoveBuff = 128 : goto skipRightCheck2


   y = ((playerY - screenYoffset) + playerHeightMinusOne) / tileHeight
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; bottom right
   if charValue > bgTile then playerX = playerX - customTemp1 : xMoveBuff = 128
skipRightCheck2
 gosub removeCoin
skipRight

 return thisbank
 
setStaticScore
 ;level
   _sc2_2 = _sc2_2 & 240 : _sc2_3 = 0
   if customTemp5 >= 100 then _sc2_2 = _sc2_2 + 1 : customTemp5 = customTemp5 - 100
   if customTemp5 >= 100 then _sc2_2 = _sc2_2 + 1 : customTemp5 = customTemp5 - 100
   if customTemp5 >= 50 then _sc2_3 = _sc2_3 + 80 : customTemp5 = customTemp5 - 50
   if customTemp5 >= 30 then _sc2_3 = _sc2_3 + 48 : customTemp5 = customTemp5 - 30
   if customTemp5 >= 20 then _sc2_3 = _sc2_3 + 32 : customTemp5 = customTemp5 - 20
   if customTemp5 >= 10 then _sc2_3 = _sc2_3 + 16 : customTemp5 = customTemp5 - 10
   _sc2_3 = _sc2_3 | customTemp5

 qsdigits9 = (_sc2_3 & $0f) * 2
 qsdigits8 = (_sc2_3 / 16) * 2
 qsdigits7 = (_sc2_2 & $0f) * 2

 ; dots in level
   _sc1_2 = _sc1_2 & 240 : _sc1_3 = 0
   if temp4 >= 100 then _sc1_2 = _sc1_2 + 1 : temp4 = temp4 - 100
   if temp4 >= 100 then _sc1_2 = _sc1_2 + 1 : temp4 = temp4 - 100
   if temp4 >= 50 then _sc1_3 = _sc1_3 + 80 : temp4 = temp4 - 50
   if temp4 >= 30 then _sc1_3 = _sc1_3 + 48 : temp4 = temp4 - 30
   if temp4 >= 20 then _sc1_3 = _sc1_3 + 32 : temp4 = temp4 - 20
   if temp4 >= 10 then _sc1_3 = _sc1_3 + 16 : temp4 = temp4 - 10
   _sc1_3 = _sc1_3 | temp4


  qsdigits3 = 19 * 2 : rem ** ascii character index for an empty tile





 livesDigits2 = (lives2 & $0f) * 2
 livesDigits1 = (lives2 / 16) * 2
 livesDigits0 = (lives1 & $0f) * 2

UpdateScore


 qsdigits2 = (dotseaten2 & $0f) * 2
 qsdigits1 = (dotseaten2 / 16) * 2
 qsdigits0 = (dotseaten1 & $0f) * 2


 qsdigits6 = (dotsremain2 & $0f) * 2
 qsdigits5 = (dotsremain2 / 16) * 2
 qsdigits4 = (dotsremain1 & $0f) * 2

 
 return thisbank
 /*
userinterrupt
 WSYNC=1 : displaymode 320B
 ;WSYNC=1 : BACKGRND=$00

 rem ** skip over the 320B text area...
 for inttemp1=0 to 5
   WSYNC=1
 next

 rem ** go to 160A
 if bottomMode = 16 then WSYNC=1 : displaymode 160A else WSYNC=1 : displaymode 320B
 ;WSYNC=1 : BACKGRND=$26


 return thisbank
 */
/*
topscreenroutine
 rem ** topscreenroutine is called by the display interrupt every
 rem ** frame. This is where we can change the display as it's being
 rem ** drawn, 2600 style.
 rem ** This technique may not work right if you completely max out
 rem ** the number of objects that Maria can display.
 rem **
 rem ** We hit the WSYNC register to skip/complete scanlines.
 rem ** Any register changes must immediately follow the previous WSYNC,
 rem ** or else the changes will show up mid-scanline.
 rem **
 rem ** You shouldn't use the temp1-temp9 variables for these routines.
 rem ** Instead you can use the interupt-safe inttemp1-inttemp6 variables.
 rem **
 rem ** Similarly, you should avoid writing to your own important game
 rem ** game variables during this routine.
 ;WSYNC=1
 ;if topMode = 16 then WSYNC=1 : displaymode 160A else WSYNC=1 : displaymode 320B
 WSYNC=1 : displaymode 320B
 ;WSYNC=1 : BACKGRND=$00

 rem ** skip over the 320B text area...
 for inttemp1=0 to 5
   WSYNC=1
 next

 rem ** go to 160A
 ;if bottomMode = 16 then WSYNC=1 : displaymode 160A else WSYNC=1 : displaymode 320B
 WSYNC=1 : displaymode 160A
 ;WSYNC=1 : BACKGRND=$26


 return thisbank
*/


/*
topscreenroutine

  WSYNC=1  : BACKGRND=$00;: displaymode 320B
 rem ** skip over the 320B text area...
 for var74 = 0 to 14 ;_320modeLength
   WSYNC=1
 next

  WSYNC=1 : displaymode 160A : BACKGRND=$26
    ;WSYNC=1
      ;WSYNC=1
        ;WSYNC=1
          ;WSYNC=1 : displaymode 160A ;: BACKGRND=$26
          ;WSYNC = 1


  return thisbank

bottomscreenroutine

  WSYNC=1 : displaymode 320B

   return thisbank

*/
 ;dim saveCB = inttemp1


_gameOver
/*
 temp5 = totaldots_1 & $0f
 asm
	SED
 clc

 lda score5+2
 adc score0+2
 sta score5+2

 lda score5+1
 adc score0+1
 sta score5+1

 lda score5
 adc score0
 sta score5

	CLD

end
 if totaldots_1 & $0f < temp5 then score4 = score4 + 1

 score0 = 0

 totaldigits0 = (totaldots1 & $0f) * 2
 totaldigits1 = (totaldots2 / 16) * 2
 totaldigits2 = (totaldots2 & $0f) * 2


 totaldigits3 = (totaldots_1 & $0f) * 2
 totaldigits4 = (totaldots_2 / 16) * 2
 totaldigits5 = (totaldots_2 & $0f) * 2
*/


  gosub StopAllSounds

 ;delay
 for temp6 = 0 to 32
  restorescreen
  drawscreen
  next

   tuneId = 1
   gosub PlayPokeySound

  ;playsfx sfx_longgongsilver
  ;_320modeLength = 0
  ;adjustvisible 0 8
    ;adjustvisible 0 5
    adjustvisible 0 5

 clearscreen
 customTemp1 = (224 / 2) - 32
 ;both sprites are 32 pixel positions wide
 plotsprite game_ 0 44  customTemp1 0 2
 plotsprite over 0 84  customTemp1 0 2

  plotsprite you 4 30 112 0 1
  plotsprite collected 4 46 112 0 1
  plotchars totaldigits0 4 86 7 6
  plotsprite gems2 4 114 112 0 1




 savescreen

 restorescreen ;one extra frame to not get artifacts
 drawscreen
 ;customTemp2 = $6d
 ;customTemp3 = $20
 ;customTemp2 = $36
 ;customTemp3 = $56
  customTemp2 = $00 ;total score color
  _P4C2 = $50 ;you collected.. gems color

 customTemp6 = $0f - $0d
 customTemp5 = $0f - $0f
 for customTemp4 = 0 to 17
 restorescreen
  ;you collected..gems fade in
  if _P4C2 & #$0F < #$0d &&  customTemp6 = 0 then P4C2 = _P4C2 : _P4C2 = _P4C2 + 1 
  if customTemp6 > 0 then customTemp6 =  customTemp6 - 1

  ;total score fade in
  if customTemp2 & #$0F < #$0f &&  customTemp5 = 0 then P4C3 = customTemp2 : customTemp2 = customTemp2 + 1 
  if customTemp5 > 0 then customTemp5 =  customTemp5 - 1

  ;game over blink
  customTemp1 =  customTemp1 + 1
  if customTemp1{1} then  P0C3 = $36 else P0C3 = $56
 drawscreen
 drawwait
 next

gameOverLoop
 customTemp1 =  customTemp1 + 1
 if customTemp1{1} then  P0C3 = $36 else P0C3 = $56
 restorescreen
 drawscreen
  if !joy0fire then restrainer = 0
  if restrainer = 0 && joy0fire then restrainer = 1 : goto gameOverFadeOut
 goto gameOverLoop

gameOverFadeOut
 customTemp4 = $36 ;game over color 1
 customTemp3 = $56 ;game over color 2
 for x = 0 to 16
 ;game over fade out
 customTemp1 =  customTemp1 + 1
 if customTemp1{1} then  P0C3 = customTemp4 else P0C3 = customTemp3
 if customTemp4 & #$0F > 0 then customTemp4 = customTemp4 - 1 else customTemp4 = $00
 if customTemp3 & #$0F > 0 then customTemp3 = customTemp3 - 1 else customTemp3 = $00

 ;you collected.. gems fade out
  P4C2 = _P4C2
  if _P4C2 & #$0F > 0 then _P4C2 = _P4C2 - 1 else _P4C2 = $00

 ;total score fade out
  P4C3 = customTemp2
  if customTemp2 & #$0F > 0 then customTemp2 = customTemp2 - 1 else customTemp2 = $00

 restorescreen
 drawscreen
 next

 ;delay
 for customTemp1 = 0 to 16
 restorescreen
 drawscreen
 next
 goto _title

clearPalettes
 BACKGRND = $00
 ;before screen change
 P0C1 = $00 : P0C2 = $00 : P0C3 = $00 : P1C1 = $00 : P1C2 = $00 : P1C3 = $00 : P2C1 = $00 : P2C2 = $00 : P2C3 = $00
 P3C1 = $00 : P3C2 = $00 : P3C3 = $00 : P4C1 = $00 : P4C2 = $00 : P4C3 = $00 : P5C1 = $00 : P5C2 = $00 : P5C3 = $00
 P6C1 = $00 : P6C2 = $00 : P6C3 = $00 : P7C1 = $00 : P7C2 = $00 : P7C3 = $00

 return thisbank

_title
  ;topMode = 16
  ;bottomMode = 16
  ;_320modeLength = 0
  ;adjustvisible 0 8
    ;adjustvisible 1 8
    adjustvisible 2 8

  ;topModeLength = 50
 ;characterset tiles0char

 gosub clearPalettes


  clearscreen
 customTemp1 = 16 * 11 - 8
 ;both sprites are 20 pixel positions wide
 customTemp2 = ((160 / 2) - 20) - 4
 customTemp3 = (160 / 2) + 4

  ;plotmapfile titleScreen.tmx titleScreen 0 2 20 9
 plotsprite press 4 customTemp2  customTemp1 0 1
 plotsprite start_ 4 customTemp3  customTemp1 0 1
  ;memcpy tileMap titleScreen 90 ;copy entire tilemap to RAM


 for tileMapY = 0 to 5 ; 6 horisontal rows
     for tileMapX = 0 to 14 ; 15 vertical columns

        charValue = peekchar( titleScreen, tileMapX, tileMapY, 15, 6 )
        spriteTileX = (tileMapX * 8) + 20 ; convert character column to pixel x pos
        spriteTileY = (tileMapY * 16) + 32 ; convert character row to pixel y pos
        ; plot tiles from tiled map as sprites
        if charValue = _mazeTile1 then plotsprite tiles2 0 spriteTileX spriteTileY 0 1
        if charValue = _mazeTile2 then y = spriteTileY + 8 : plotsprite tiles3 1 spriteTileX y 0 1
        if charValue = _mazeTile3 then plotsprite tiles4 2 spriteTileX spriteTileY 0 1
        ;if charValue = _dotCentered then plotsprite dot 2 spriteTileX spriteTileY 0 1
        ;need to fill in nothing with empty tiles
        ;so the display list index is the same on each row
        ;and it's simple to change tiles in it
        ;if charValue = _emptyTile then plotsprite tiles0 2 spriteTileX spriteTileY 0 1 


     next
 next



  savescreen

 ;an extra frame to get rid of artifacts
 restorescreen
 drawscreen

 restorescreen
 drawscreen
 


  ;turqoise tile
 P0C1 = $a5 :  _P0C1 = $a5
 P0C2 = $a9 :  _P0C2 = $a9
 P0C3 = $ad :  _P0C3 = $ad
 ;pink tile
 P2C1 = $55 :  _P2C1 = $55
 P2C2 = $59 :  _P2C2 = $59
 P2C3 = $5d :  _P2C3 = $5d

 P1C1 = $75 :  _P1C1 = $75
 P1C2 = $79 :  _P1C2 = $79
 P1C3 = $7d :  _P1C3 = $7d

 ;white text
 P4C3 = $00



 for customTemp1 = 0 to 4
 shakescreen hi
  restorescreen
  drawscreen
  drawwait
  ;drawscreen
  ;drawscreen
 next
 ;shakescreen off

 for customTemp1 = 0 to 6
 shakescreen med
  restorescreen
  drawscreen
  drawwait
  ;drawscreen
  ;drawscreen
 next
 ;shakescreen off

 for customTemp1 = 0 to 8
 shakescreen lo
  restorescreen
  drawscreen
  drawwait
  ;drawscreen
  ;drawscreen
 next
  shakescreen off
 

 restorescreen ; cleanup shakescreen?
 drawscreen

/*
bounceText
 ;for tileMapY = 0 to 128 step 16 ; 6 horisontal rows
     ;for tileMapX = 20 to 140 step 8 ; 15 vertical columns

 for tileMapY = 0 to 7 ;step 16 ; 6 horisontal rows
     for tileMapX = 2 to 16 ;step 8 ; 15 vertical columns

     asm
     ldx tileMapX
     ldy tileMapY
     SetObjectY #16 ; 8 pixels lower
end
     next
 next
 drawscreen
 restorescreen
 goto bounceText
*/
 customTemp3 = 0
titleLoop
  restorescreen
 ;shakescreen off
  drawscreen
  if !joy0fire then restrainer = 0
   ;white text
    customTemp3 = customTemp3 + 1
   if customTemp3 = 16 then  P4C3 = $0f
   if customTemp3 = 32 then  P4C3 = $00 : customTemp3 = 0

  if restrainer = 1 || !joy0fire then goto titleLoop
  ;if !joy0fire then goto lp  
  level = 0
  ;_lives = 3 
  score3 = 5

  score5 = 000
  score4 = 000
  playsfx sfx_strum


  ;clearscreen
  ;savescreen

 for customTemp1 = 0 to 32 ;blink push start
 restorescreen
 drawscreen
 if customTemp1{2} then P4C3 = $0f else P4C3 = $00


 next
 
  for customTemp1 = 0 to 32 ;fade out
  restorescreen
  drawscreen
  drawwait
  gosub fadeBGtiles
  next
  ;_320modeLength = 5
  adjustvisible 1 14

  gosub StopAllSounds

 for temp5 = 0 to 16
  restorescreen
  drawscreen
  next
  ;restorescreen
  ;drawscreen
  ;MÅSTE TÖMMA STACKEN
/*
 for customTemp1 = 0 to 1
  pop
  next
*/
  gosub clearPalettes
  gosub loadLevel


   tuneId = 2
   gosub PlayPokeySound
  goto main



loadLevel

     ;stuff that needs to be reset between levels
     dotCounter = 0 : frameCounter = 0 : frameCounter2 = 0 : frame = 0 : playerBubbleCountdown = 0 : numberOfEnemies = 0 : bonusStageFlag = 0
     
     for temp5 = 0 to 9 ; so the game behaves exactly the same at the start of each level
     enemyDirArray[temp5] = 0
     enemyXarray[temp5] = 0
     enemyYarray[temp5] = 0
     enemyXfractArray[temp5] = 0
     enemyYfractArray[temp5] = 0
     enemyPlantCountdownArray[temp5] = 0
     next
     
     xMoveBuff = 128 : yMoveBuff = 128



 playerYSpeed = 1.5 ;128
 playerXSpeed = 0.8751458576429405 ;1.5 / 1.714

 enemy1XSpeed = 0.375 ;96 / 256
 enemy1YSpeed = 0.64275 ;0.375 * 1.714

 enemy2XSpeed = 0.4375 ;112 / 256
 enemy2YSpeed = 0.749875 ;0.4375 * 1.714

 enemy3XSpeed = 0.5 ;128 / 256
 enemy3YSpeed = 0.867 ;0.5 * 1.714
 characterset numbers
 clearscreen

 level = level + 1
 if level >= 29 then level = 1
 if level < 100 then gosub levels bank5


   plotsprite LEVEL 0 16 0 0
   plotsprite GEMS 0 56 0 0
   plotsprite LIVES 0 108 0 0
   plotsprite SLASH 0 88 0 0


 customTemp6 = 0
 for tileMapY = 0 to 12 ; 13 horisontal rows
     for tileMapX = 0 to 15 ; 16 vertical columns

        charValue = peekchar( tileMap, tileMapX, tileMapY, 16, 13 )
        spriteTileX = tileMapX * 8 + screenXoffset ; convert character column to pixel x pos
        spriteTileY = tileMapY * 16 + screenYoffset ; convert character row to pixel y pos
        ; plot tiles from tiled map as sprites
        if charValue = _mazeTile1 then plotsprite tiles2 0 spriteTileX spriteTileY 0 1
        if charValue = _mazeTile2 then plotsprite tiles3 1 spriteTileX spriteTileY 0 1
        if charValue = _dotCentered then plotsprite dot 2 spriteTileX spriteTileY 0 1 : customTemp6 = customTemp6 + 1 ;: score0 = score0 + 1
        if charValue = _dotGone then plotsprite tiles1 2 spriteTileX spriteTileY 0 1
        if charValue = _enemy1SpawnPoint then plotsprite esp1 7 spriteTileX spriteTileY 0 1 : enemy1X = spriteTileX : enemy1Y = spriteTileY : numberOfEnemies = numberOfEnemies + 1
        if charValue = _enemy2SpawnPoint then plotsprite esp2 7 spriteTileX spriteTileY 0 1 : enemy2X = spriteTileX : enemy2Y = spriteTileY : numberOfEnemies = numberOfEnemies + 1
        if charValue = _enemy3SpawnPoint then plotsprite esp3 7 spriteTileX spriteTileY 0 1 : enemy3X = spriteTileX : enemy3Y = spriteTileY : numberOfEnemies = numberOfEnemies + 1
        if charValue = _playerSpawnPoint then plotsprite psp 7 spriteTileX spriteTileY 0 1 : playerX = spriteTileX : playerY = spriteTileY
        ;need to fill in nothing with empty tiles
        ;so the display list index is the same on each row
        ;and it's simple to change tiles in it
        if charValue = _emptyTile then plotsprite tiles0 2 spriteTileX spriteTileY 0 1 


     next
 next

   ;temp4 = numberOfDots[level]
   temp4 = customTemp6
   customTemp5 = level
   gosub setStaticScore
   plotchars qsdigits7 4 40 0 3
   plotchars qsdigits0 4 76 0 7
   plotchars livesDigits0 4 132 0 3

 savescreen

 for customTemp1 = 0 to 5
  restorescreen
  drawscreen
  drawwait
 next

  ;playerX = player1Xpos[level]
  ;playerY = player1Ypos[level]

  ;enemy3X = 104
  ;enemy3Y = 176

  ;enemy1X = enemy1Xpos[level]
  ;enemy1Y = enemy1Ypos[level]

  ;enemy2X = enemy2Xpos[level]
  ;enemy2Y = enemy2Ypos[level]

  ;enemy3X = enemy3Xpos[level]
  ;enemy3Y = enemy3Ypos[level]
 gosub clearPalettes ;allways clear before fade-in
 gosub fadeIn

 playsfx sfx_bubbleup
 for customTemp1 = 0 to 14
 restorescreen
      ;plotsprite zero 4 140 0 _lives 1
 if customTemp1{0}  then  plotsprite player 3 playerX playerY frame 1
 drawscreen
 next

 customTemp2 = numberOfEnemies - 1
 if customTemp2 = 255 then return thisbank
 for customTemp1 = 0 to customTemp2
 enemyY = enemyYarray[customTemp1]
 enemyX = enemyXarray[customTemp1]
 gosub checkPossibleDirections
   customTemp6 = rand & 3
   temp5 = 0
   ;if    customTemp6{0} then gosub setDir else gosub setDir2 ;left right priority
   if    customTemp6 = 0 then gosub setDir ;left right priority
   if    customTemp6 = 1 then gosub setDir2 ;up down priority
   if    customTemp6 = 2 then gosub setDir3 ;rand updown
   if    customTemp6 = 3 then gosub setDir4 ;rand leftright
 next

 if numberOfEnemies < 1 then return thisbank
 playsfx sfx_bubbleup
 for customTemp1 = 0 to 14
 restorescreen
      ;plotsprite zero 4 140 0 _lives 1
 if customTemp1{0}  then   plotsprite enemy 4 enemy1X enemy1Y frame 1
 plotsprite player 3 playerX playerY frame 1
 drawscreen
 next



 if numberOfEnemies < 2 then return thisbank
 playsfx sfx_bubbleup
 for customTemp1 = 0 to 14
 restorescreen
      ;plotsprite zero 4 140 0 _lives 1
 if customTemp1{0}  then   plotsprite enemy 5 enemy2X enemy2Y frame 1
 plotsprite player 3 playerX playerY frame 1
 plotsprite enemy 4 enemy1X enemy1Y frame 1
 drawscreen
 next

 if numberOfEnemies < 3 then return thisbank
 playsfx sfx_bubbleup
 for customTemp1 = 0 to 14
 restorescreen
      ;plotsprite zero 4 140 0 _lives 1
 if customTemp1{0}  then   plotsprite enemy 6 enemy3X enemy3Y frame 1
 plotsprite player 3 playerX playerY frame 1
 plotsprite enemy 4 enemy1X enemy1Y frame 1
 plotsprite enemy 5 enemy2X enemy2Y frame 1
 drawscreen
 next



 return thisbank
 








removeCoin
 ;if rememberYpos = playerY then return thisbank
 ;if rememberYpos > playerY then temp5 = playerY - 1 : customTemp4 = rememberYpos else  customTemp4 = playerY : temp5 = rememberYpos
 ;if rememberXpos > playerX then temp5 = playerX : temp6 = rememberXpos else  temp6 = playerX : temp5 = rememberXpos
 ;for customTemp3 = temp5 to customTemp4 step 1
  ;for customTemp4 = temp5 to temp6
   ;if rememberXpos & tileWidth
   ;temp5 = playerX
   ;temp2 = playerX - 16
   ;for temp1 = 0 to temp2 step 8
   ;temp5 = temp5 / 8
   ;next
   ;temp6 = temp5 * 8
   customTemp1 = tileHeight - playerY & tileHeightMask ; we only want the three lower bits, &7, 0-7
   customTemp2 = tileWidth - playerX & tileWidthMask
   y = (playerY - screenYoffset) / tileHeight
   x = (playerX - screenXoffset) / tileWidth


   charValue = peekchar( tileMap, x, y, 16, 13 ) ; top left
   if charValue >= _enemy1SpawnPoint && charValue <= _playerSpawnPoint then return thisbank
   if charValue < bgTile && customTemp1 = 0 && customTemp2 = 0 then gosub removeDot
   ;if charValue < bgTile then gosub removeDot 
  ;next
 ;next

 return thisbank



/*
_pull
 ;pull from top
  x = rememberDotPosX
  y = rememberDotPosY
 


 for customTemp1 = 0 to 120
 customTemp3 = customTemp1 + 1

 rememberDotPosX[customTemp1] =  rememberDotPosX[customTemp3] ;0 = 2
 next



 for customTemp1 = 0 to 120
 customTemp3 = customTemp1 + 1

 rememberDotPosY[customTemp1] =  rememberDotPosY[customTemp3] ;0 = 1

 next

 dotCounter = dotCounter - 1
 score0 = score0 - 1
 pokechar tileMap x y 16 13 _dotCentered

     rem ** set a new image
     asm
     inc y
     ldx x ;column
     ldy y ;row
     SetObjectImageLo #<dot
     ;SetObjectImage tiles1
end


 ;playsfx sfx_jumpman


 return thisbank
 */
 

removeDot
   if charValue = _bubbleCentered then playerBubbleCountdown = 32 : playsfx sfx_denied : goto skipDotStuff2
 ;save positions for respawning dots
 ;rememberDotPosX[dotCounter] = x
 ;rememberDotPosY[dotCounter] = y


 playsfx sfx_jumpman
 score0 = score0 + 1
 streakCounter = streakCounter + 1
 dotCounter = dotCounter + 1
skipDotStuff2
 pokechar tileMap x y 16 13 bgTile 

     rem ** set a new image
     asm
     inc y
     ldx x ;column
     ldy y ;row
     SetObjectImageLo #<tiles1
     ;SetObjectImage tiles1
end
     
     ;not needed if changing tile re-sets palette to?

     asm
     ldx x
     ;inc y
     ldy y
     ;dec y
     SetObjectPalette #2
end

 return thisbank

dotsDown
dotsUp
/*
only these tiles can be dots
because of the maze outline
*/

     for x = 1 to 14 ; 16 vertical columns
      for y = 1 to 11 ; 13 horisontal rows

        charValue = peekchar( tileMap, x, y, 16, 13 )

        if charValue <> _dotCentered then skipDotChange
        customTemp2 = y + 1
     rem ** set a new image
     asm
     ldx x ;column
     ldy customTemp2 ;row
     SetObjectImageLo customTemp5
     ;SetObjectImage dot2
end
skipDotChange
        if charValue <> _bubbleCentered then skipBubbleChange
        customTemp2 = y + 1
     rem ** set a new image
     asm
     ldx x ;column
     ldy customTemp2 ;row
     SetObjectImageLo customTemp4
     ;SetObjectImage dot2
end
skipBubbleChange

     next
 next

 return thisbank
/*
dotsDown

     for x = 1 to 14 ; 16 vertical columns
      for y = 1 to 11 ; 13 horisontal rows

        charValue = peekchar( tileMap, x, y, 16, 13 )

        if charValue <> _dotCentered then skipTileChange2
        customTemp2 = y + 1
     rem ** set a new image
     asm
     ldx x ;column
     ldy customTemp2 ;row
     SetObjectImageLo #<tiles1
     ;SetObjectImage dot
end
 
skipTileChange2
     next
 next

 return thisbank
*/
 ;REMEMBER TO MAKE THIS MORE RANDOM SO ENEMY DOESN'T GET STUCK IN A LOOP
setDir
 ;prefered directions towards player
 if temp5 <> _right && !wallLeft{0} && playerX <= enemyX  then enemyDirArray[customTemp1] = _left :  goto foundDir
 if temp5 <> _left && !wallRight{1} && playerX > enemyX  then enemyDirArray[customTemp1] = _right  : goto foundDir

 if temp5 <> _down && !wallUp{2} && playerY <= enemyY     then enemyDirArray[customTemp1] = _up :  goto foundDir
 if temp5 <> _up && !wallDown{3} && playerY > enemyY     then enemyDirArray[customTemp1] = _down :  goto foundDir
 
setDir4
 ;emergency directions
 if temp5 <> _right && !wallLeft{0}   then enemyDirArray[customTemp1] = _left :  goto foundDir
 if temp5 <> _left && !wallRight{1}   then enemyDirArray[customTemp1] = _right :  goto foundDir

 if temp5 <> _down && !wallUp{2}      then enemyDirArray[customTemp1] = _up :  goto foundDir
 if temp5 <> _up && !wallDown{3}      then enemyDirArray[customTemp1] = _down  : goto foundDir

 ;really emergency, when you have to go opposite direction
 if !wallLeft{0}   then enemyDirArray[customTemp1] = _left :  goto foundDir
 if !wallRight{1}  then enemyDirArray[customTemp1] = _right :  goto foundDir

 if !wallUp{2}     then enemyDirArray[customTemp1] = _up :  goto foundDir
 if !wallDown{3}   then enemyDirArray[customTemp1] = _down  : goto foundDir

setDir2
 ;MAYBE EVERY TWO LINES ALSO SHOULD BE FLIPPED
 ;prefered directions towards player
 if temp5 <> _up && !wallDown{3} && playerY >= enemyY     then enemyDirArray[customTemp1] = _down :  goto foundDir
 if temp5 <> _down && !wallUp{2} && playerY < enemyY     then enemyDirArray[customTemp1] = _up :  goto foundDir

 if temp5 <> _left && !wallRight{1} && playerX >= enemyX  then enemyDirArray[customTemp1] = _right  : goto foundDir
 if temp5 <> _right && !wallLeft{0} && playerX < enemyX  then enemyDirArray[customTemp1] = _left :  goto foundDir



setDir3
 ;emergency directions
 if temp5 <> _up && !wallDown{3}      then enemyDirArray[customTemp1] = _down  : goto foundDir
 if temp5 <> _down && !wallUp{2}      then enemyDirArray[customTemp1] = _up :  goto foundDir

 if temp5 <> _left && !wallRight{1}   then enemyDirArray[customTemp1] = _right :  goto foundDir
 if temp5 <> _right && !wallLeft{0}   then enemyDirArray[customTemp1] = _left :  goto foundDir




 ;really emergency, when you have to go opposite direction
 if !wallDown{3}   then enemyDirArray[customTemp1] = _down  : goto foundDir
 if !wallUp{2}     then enemyDirArray[customTemp1] = _up :  goto foundDir

 if !wallRight{1}  then enemyDirArray[customTemp1] = _right :  goto foundDir
 if !wallLeft{0}   then enemyDirArray[customTemp1] = _left :  goto foundDir

 


foundDir
 ;rememberDirTests[customTemp1] = wallHits
 return thisbank
/*
 data sfx_bling
 $10,$10,$00 ; version, priority, frames per chunk
 $1c,$04,$07
 $1b,$04,$07
 $04,$0f,$05
 $15,$04,$09
 $16,$04,$07
 $03,$0f,$04
 $11,$04,$08
 $11,$04,$08
 $11,$04,$04
 $0e,$04,$09
 $0e,$04,$07
 $0e,$04,$04
 $1c,$04,$07
 $1b,$04,$05
 $1c,$04,$04
 $1b,$04,$02
 $00,$00,$00
end
*/

plantBubble
  pokechar tileMap x y 16 13 _bubbleCentered
     asm
     ldx x
     inc y
     ldy y
     ;dec y
     SetObjectPalette #7
end

     asm
     ;inc y
     ldx x ;column
     ldy y ;row
     SetObjectImageLo #<bubble
     ;SetObjectImage tiles1
end
 ;playsfx sfx_doorpound
 ; playsfx sfx_nonobounce ;good
 playsfx sfx_echo2

 goto skipDotStuff
plantDot
 dotCounter = dotCounter - 1
 ;score0 = score0 - 1
  score1 = score1 + 1
/*
 asm
	SED
	CLC
	LDA score0+6
	ADC #$01
	STA score0+6
	LDA score0+5
	ADC #$00
	STA score0+5
	LDA score0
	ADC #$00
	STA score0+4
	CLD
end
*/
 ;playsfx sfx_bling
 ;playsfx sfx_spawn
 ;playsfx sfx_dropped
 ;playsfx sfx_spawn
 playsfx sfx_squeek

 pokechar tileMap x y 16 13 _dotCentered



     rem ** set a new image
     asm
     inc y
     ldx x ;column
     ldy y ;row
     SetObjectImageLo #<dot
     ;SetObjectImage tiles1
end

skipDotStuff

 enemyPlantCountdownArray[customTemp1] = 32
 return thisbank

enemyMovement

 if bonusStageFlag = 1 then skipDotPlant
 temp7 = rand
 if temp7 > 8 then skipDotPlant
   y = (enemyY - screenYoffset) / tileHeight
   x = (enemyX - screenXoffset) / tileWidth
   temp5 = peekchar( tileMap, x, y, 16, 13 ) ; top left
   if temp5 >= _enemy1SpawnPoint && temp5 <= _playerSpawnPoint then skipDotPlant

   temp2 = tileHeight - enemyY & tileHeightMask ; we only want the three lower bits, &7, 0-7
   temp4 = tileWidth - enemyX & tileWidthMask
   if customTemp1 = 1 && temp5 = bgTile && temp4 = 0 && temp2 = 0 then customTemp5 = _dotCentered : gosub plantDot ;else moveEnemy
   if customTemp1 = 2 && temp5 = bgTile && temp4 = 0 && temp2 = 0 then customTemp5 = _bubbleCentered : gosub plantBubble
skipDotPlant
/*
   temp2 = tileHeight - enemyY & tileHeightMask ; we only want the three lower bits, &7, 0-7
   temp4 = tileWidth - enemyX & tileWidthMask
   if temp4 = 0 && temp2 = 0 then gosub checkPossibleDirections ;else goto moveEnemy
*/
   gosub checkPossibleDirections
       temp5 = enemyDirArray[customTemp1]
   ;if wallHits = 0 then goto moveEnemy
   customTemp6 = rand & 3
   ;customTemp6 = rand & 2
 ;check previous direction, can't go from up/down to down/up or left/right to right/left
   ;if    customTemp6{0}  then gosub setDir else gosub setDir2
   if    customTemp6 = 0 then gosub setDir ;left right priority
   if    customTemp6 = 1 then gosub setDir2 ;up down priority
   if    customTemp6 = 2 then gosub setDir3 ;rand updown
   if    customTemp6 = 3 then gosub setDir4 ;rand leftright

moveEnemy
 customTemp6 = enemyDirArray[customTemp1]

 on customTemp1 goto _e1 _e2 _e3
_e1
 temp1 = enemy1X ;remember for compare
 temp2 = enemy1Y

   if customTemp6 = _left   then enemy1Xdouble = enemy1Xdouble -  enemy1XSpeed
   if customTemp6 = _right  then enemy1Xdouble = enemy1Xdouble +  enemy1XSpeed
   if customTemp6 = _up     then enemy1Ydouble = enemy1Ydouble - enemy1YSpeed
   if customTemp6 = _down   then enemy1Ydouble = enemy1Ydouble + enemy1YSpeed
jj
   if temp1 = enemy1X && temp2 = enemy1Y then enemyDirArray[customTemp1] = temp5
   return thisbank
  ;goto _cont
   ;the comparison is so you don't change directions while not moving
   ;it can lead to changing from down to left, and if youre not moving
   ;that frame, it can change to up again, so it's to avoid moving the
   ;opposite of the previous direction
 ;it restores the previous direction if you don't move, requires starting direction to be set in init
_e2
 temp1 = enemy2X ;remember for compare
 temp2 = enemy2Y
   if enemyPlantCountdownArray[customTemp1] > 0 then  enemyPlantCountdownArray[customTemp1] =  enemyPlantCountdownArray[customTemp1] - 1 : goto kk
   if customTemp6 = _left   then enemy2Xdouble = enemy2Xdouble - enemy2XSpeed
   if customTemp6 = _right  then enemy2Xdouble = enemy2Xdouble + enemy2XSpeed
   if customTemp6 = _up     then enemy2Ydouble = enemy2Ydouble - enemy2YSpeed
   if customTemp6 = _down   then enemy2Ydouble = enemy2Ydouble + enemy2YSpeed
kk
   if temp1 = enemy2X && temp2 = enemy2Y then enemyDirArray[customTemp1] = temp5
   return thisbank
   
   ;goto _cont
_e3
 temp1 = enemy3X ;remember for compare
 temp2 = enemy3Y
   if enemyPlantCountdownArray[customTemp1] > 0 then  enemyPlantCountdownArray[customTemp1] =  enemyPlantCountdownArray[customTemp1] - 1 : goto ll
   if customTemp6 = _left   then enemy3Xdouble = enemy3Xdouble - enemy3XSpeed
   if customTemp6 = _right  then enemy3Xdouble = enemy3Xdouble + enemy3XSpeed
   if customTemp6 = _up     then enemy3Ydouble = enemy3Ydouble - enemy3YSpeed
   if customTemp6 = _down   then enemy3Ydouble = enemy3Ydouble + enemy3YSpeed
ll
   if temp1 = enemy3X && temp2 = enemy3Y then enemyDirArray[customTemp1] = temp5
   return thisbank
_cont


 ;0.875 ;224 / 256
 ;0.75 ;192 / 256;

checkPossibleDirections

 wallHits = 0


   y = ((enemyY - 1) - screenYoffset) / tileHeight
   x = (enemyX - screenXoffset) / tileWidth
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; above top left
   if charValue > bgTile then wallUp{2} = 1 : goto skipUp2


   x = ((enemyX - screenXoffset) + playerWidthMinusOne) / tileWidth
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; above top right
   if charValue > bgTile then wallUp{2} = 1

skipUp2


   y = ((enemyY -screenYoffset) + playerHeight) / tileHeight
   x = (enemyX - screenXoffset) / tileWidth
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; below bottom left
   if charValue > bgTile then wallDown{3} = 1 : goto skipDown2


   x = ((enemyX - screenXoffset )+ playerWidthMinusOne) / tileWidth
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; below bottom right
   if charValue > bgTile then wallDown{3} = 1

skipDown2


   x = ((enemyX - 1) - screenXoffset) / tileWidth
   y = (enemyY - screenYoffset) / tileHeight
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; beside top left
   if charValue > bgTile then wallLeft{0} = 1 : goto skipLeft2


   y = ((enemyY - screenYoffset) + playerHeightMinusOne) / tileHeight
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; beside bottom left
   if charValue > bgTile then  wallLeft{0} = 1

skipLeft2
 

   x = ((enemyX - screenXoffset) + playerWidth) / tileWidth
   y = (enemyY - screenYoffset) / tileHeight
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; beside top right
   if charValue > bgTile then wallRight{1} = 1 : goto skipRight2


   y = ((enemyY - screenYoffset) + playerHeightMinusOne) / tileHeight
   charValue = peekchar( tileMap, x, y, 16, 13 ) ; beside bottom right
   if charValue > bgTile then wallRight{1} = 1

skipRight2

 return thisbank



 

 bank 2

 bank 3

 bank 4

 bank 5

 incmapfile bonus.tmx 

 incmapfile level1.tmx 
 incmapfile level2.tmx 
 incmapfile level3.tmx 
 incmapfile level4.tmx 

 incmapfile level6.tmx 
 incmapfile level7.tmx 
 incmapfile level8.tmx 
 incmapfile level9.tmx 

 incmapfile level11.tmx 
 incmapfile level12.tmx 
 incmapfile level13.tmx 
 incmapfile level14.tmx 

 incmapfile level16.tmx 
 incmapfile level17.tmx 
 incmapfile level18.tmx 
 incmapfile level19.tmx 

 incmapfile level21.tmx 
 incmapfile level22.tmx 
 incmapfile level23.tmx 
 incmapfile level24.tmx 

 incmapfile level26.tmx 
 incmapfile level27.tmx 
 incmapfile level28.tmx 

  ;0, 1, 2, 2, 3, 0, 3, 1, 2, 3, 0, 2, 0, 1, 3, 0, 3, 3, 3, 3, 2, 0,
levels
 on level goto _1 _1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22 _23 _24 _25 _26 _27 _28

_1 

/*
  playerYSpeed = 1.46875 ;120
  playerXSpeed = 0.8569136522753792 ;1.46875 / 1.714

  enemy1XSpeed = 0.34375 ;88 / 256
  enemy1YSpeed = 0.5891875 ;0,34375 * 1.714

 enemy2XSpeed = 0.40625 ;104 / 256
 enemy2YSpeed = 0.6963125 ;0.40625 * 1.714

 enemy3XSpeed = 0.46875 ;120 / 256
 enemy3YSpeed = 0.8034375 ;0.46875 * 1.714
*/
  memcpy tileMap level1 208 ;copy entire tilemap to RAM
  return otherbank
_2

/*
 playerYSpeed = 1.5 ;128
 playerXSpeed = 0.8751458576429405 ;1.5 / 1.714

 enemy1XSpeed = 0.375 ;96 / 256
 enemy1YSpeed = 0.64275 ;0.375 * 1.714

 enemy2XSpeed = 0.4375 ;112 / 256
 enemy2YSpeed = 0.749875 ;0.4375 * 1.714

 enemy3XSpeed = 0.5 ;128 / 256
 enemy3YSpeed = 0.867 ;0.5 * 1.714
*/
  memcpy tileMap level2 208 ;copy entire tilemap to RAM
  return otherbank

_3
/*
 playerYSpeed = 1.53125 ;136
 playerXSpeed = 0.8933780630105018 ;0,53125 / 1.714

 enemy1XSpeed = 0.40625 ;104 / 256
 enemy1YSpeed = 0.6963125 ;0.40625 * 1.714

 enemy2XSpeed = 0.46875 ;120 / 256
 enemy2YSpeed = 0.8034375 ;0.46875 * 1.714

 enemy3XSpeed = 0.53125 ;136 / 256
 enemy3YSpeed = 0.9105625
*/
  memcpy tileMap level3 208 ;copy entire tilemap to RAM
  return otherbank
_4
  memcpy tileMap level4 208 ;copy entire tilemap to RAM
  return otherbank
_5
  memcpy tileMap bonus 208 ;copy entire tilemap to RAM
  bonusStageFlag = 1
  return otherbank
_6
  memcpy tileMap level6 208 ;copy entire tilemap to RAM
  return otherbank
_7
  memcpy tileMap level7 208 ;copy entire tilemap to RAM
  return otherbank
_8
  memcpy tileMap level8 208 ;copy entire tilemap to RAM
  return otherbank
_9
  memcpy tileMap level9 208 ;copy entire tilemap to RAM
  return otherbank
_10
  memcpy tileMap bonus 208 ;copy entire tilemap to RAM
  bonusStageFlag = 1
  return otherbank
_11
  memcpy tileMap level11 208 ;copy entire tilemap to RAM
  return otherbank
_12
  memcpy tileMap level12 208 ;copy entire tilemap to RAM
  return otherbank
_13
  memcpy tileMap level13 208 ;copy entire tilemap to RAM
  return otherbank
_14
  memcpy tileMap level14 208 ;copy entire tilemap to RAM
  return otherbank
_15
  memcpy tileMap bonus 208 ;copy entire tilemap to RAM
  bonusStageFlag = 1
  return otherbank
_16
  memcpy tileMap level16 208 ;copy entire tilemap to RAM
  return otherbank
_17
  memcpy tileMap level17 208 ;copy entire tilemap to RAM
  return otherbank
_18
  memcpy tileMap level18 208 ;copy entire tilemap to RAM
  return otherbank
_19
  memcpy tileMap level19 208 ;copy entire tilemap to RAM
  return otherbank
_20
  memcpy tileMap bonus 208 ;copy entire tilemap to RAM
  bonusStageFlag = 1
  return otherbank
_21
  memcpy tileMap level21 208 ;copy entire tilemap to RAM
  return otherbank
_22
  memcpy tileMap level22 208 ;copy entire tilemap to RAM
  return otherbank
_23
  memcpy tileMap level23 208 ;copy entire tilemap to RAM
  return otherbank
_24
  memcpy tileMap level24 208 ;copy entire tilemap to RAM
  return otherbank
_25
  memcpy tileMap bonus 208 ;copy entire tilemap to RAM
  bonusStageFlag = 1
  return otherbank
_26
  memcpy tileMap level26 208 ;copy entire tilemap to RAM
  return otherbank
_27
  memcpy tileMap level27 208 ;copy entire tilemap to RAM
  return otherbank
_28
  memcpy tileMap level28 208 ;copy entire tilemap to RAM
  return otherbank


 bank 6

 bank 7

 bank 8
 rem dot centered
 incgraphic dot.png 160A 0 1 2 3
 rem dot up
 incgraphic dot2.png 160A 0 1 2 3
 ;bubble
 incgraphic bubble.png 160A 0 1 2 3
 incgraphic bubble2.png 160A 0 1 2 3
 rem empty
 incgraphic tiles0.png 160A 0 1 2 3

  incgraphic esp1.png 160A 0 1 2 3
  incgraphic esp2.png 160A 0 1 2 3
  incgraphic esp3.png 160A 0 1 2 3
  incgraphic psp.png 160A 0 1 2 3
 rem deep bg without dot
 incgraphic tiles1.png 160A 0 1 2 3


 rem blue tile
 incgraphic tiles2.png 160A 0 1 2 3
 rem purple tile
 incgraphic tiles3.png 160A 0 1 2 3



 ;sprites
  incgraphic enemy.png 160A 0 2 1 3
  incgraphic enemy2.png 160A 0 2 1 3
  incgraphic player.png 160A 0 2 3 1 
  incgraphic player2.png 160A 0 2 3 1 
  incgraphic bubbleOverlay.png 160A 0 2
  incgraphic bubbleOverlay2.png 160A 0 2

  incgraphic streak.png 160A 0 3 2 2
 ;lives sprite
  ;incgraphic zero.png 320B 0 3 2 1 
  ;incgraphic one.png 320B 0 3 2 1 
  ;incgraphic two.png 320B 0 3 2 1
  ;incgraphic three.png 320B 0 3 2 1
 ;game over
  incgraphic game_.png 320B 0 3 2 1
  incgraphic over.png 320B 0 3 2 1
 ;titlescreen
  incgraphic press.png 320B 0 3 2 1
  incgraphic start_.png 320B 0 3 2 1
  ;purple tile
  incgraphic tiles4.png 160A 0 1 2 3 2 
 ;topscreen 
    incgraphic LEVEL.png 320B 0 2 3 1
    incgraphic GEMS.png 320B 0 2 3 1
    incgraphic SLASH.png 320B 0 2 3 1
    incgraphic LIVES.png 320B 0 2 3 1

    incgraphic you.png 320B 0 2 3 1
    incgraphic collected.png 320B 0 2 3 1
    incgraphic gems2.png 320B 0 2 3 1


 ;newblock
 /*
 data numberOfDots
 0, 22, 42, 50, 52, 99, 76
end
*/
/*
 data numberOfDots
 0, 20, 39, 47, 48, 95, 72, 48, 31, 76, 49, 100, 100, 100, 100, 100,
end

 data numberOfEnemies
 0, 1, 2, 2, 3, 0, 3, 1, 2, 3, 0, 2, 0, 1, 3, 0, 3, 3, 3, 3, 2, 0,
end
/*
 /* xpos
 0 
 8
 16 ;1 första rutan
 24 ;2 första möjliga pos
 32 
 40
 48
 56
 64
 72 
 80
 88
 96
 104
 112 
 120
 128 ; 15 sista möjliga pos
 136 ; 16 sista rutan
 */

 /* ypos
 0 
 16 ;1 första rutan
 32 ;2 första möjliga pos
 48
 64 
 80
 96
 112
 128
 144 
 160
 176
 192 ;12 sista möjliga pos
 208 ;13 sista rutan
 224
 */

 data player1Xpos
 0,
 56,
 48,
 48,
 48,
 40,
 24,
 24,
 24,
 24,
 48,
 0
end

 data player1Ypos
 0,
 80,
 64,
 64,
 48,
 32,
 32,
 32,
 96,
 32,
 48,
 32,
 0
end

 data enemy1Xpos
 0,
 96,
 104,
 104,
 120,
 112,
 128,
 128,
 128,
 128,
 104,
 0
end

 data enemy1Ypos
 0,
 144,
 64,
 64,
 64,
 32,
 32,
 32,
 96,
 32,
 176,
 0
end

 data enemy2Xpos
 0,
 0, 
 48, 
 104,
 32,
 40,
 128,
 0,
 128,
 128,
 92,
 0
end

 data enemy2Ypos
 0,
 0,
 160, 
 160,
 160,
 192,
 192,
 0,
 128,
 192,
 176,
 0
end

 data enemy3Xpos
 0,
 0, 
 0, 
 0, 
 104, 
 112,
 24,
 0,
 0,
 24,
 0,
 0
end

 data enemy3Ypos
 0,
 0,
 0,
 0,
 176, 
 192,
 192,
 0,
 0,
 192,
 0,
 0
end
/*
 data tile1color
 0
 $a0
 $90
 $40
 $90
 $90
 $70
end

 data tile2color
 0
 $80
 $60
 $90
 $40
 $40
 $40
end
*/

 data tile1color
 0
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70 ;20
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70
 $70 ;30
end

 data tile2color
 0
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40 ;20
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40
 $40 ;30
end
 data deepBackgroundColor
 0
 $20 ;$D0
 $20
 $20 ;ska vara ljusare
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20 ;20
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20
 $20 ;30
end

  data sfx_dropmedium
 $10,$10,$00 ; version, priority, frames per chunk
 $00,$04,$00 ; first chunk of freq,channel,volume
 $03,$06,$0c
 $0d,$0c,$0f
 $1b,$04,$04
 $06,$0c,$00
 $00,$06,$00
 $07,$06,$00
 $10,$0c,$00
 $0d,$0c,$00
 $10,$0c,$00
 $03,$06,$00
 $10,$0c,$00
 $1b,$04,$00
 $10,$0c,$00
 $10,$0c,$00
 $03,$06,$00
 $00,$00,$00
end

 data sfx_ouch
 $10,$10,$00 ; version, priority, frames per chunk
 $07,$0c,$0f ; first chunk of freq,channel,volume
 $07,$0c,$0f
 $07,$0c,$0f
 $18,$04,$07
 $19,$04,$04
 $07,$0c,$09
 $19,$04,$0f
 $19,$04,$0d
 $19,$04,$0f
 $19,$04,$0f
 $1b,$04,$0f
 $1b,$04,$0f
 $1b,$04,$0f
 $1b,$04,$0f
 $1b,$04,$09
 $1b,$04,$05
 $1b,$04,$03
 $1b,$04,$02
 $1c,$04,$01
 $1c,$04,$01
 $1c,$04,$01
 $1b,$04,$02
 $19,$04,$00
 $1b,$04,$00
 $19,$04,$01
 $00,$00,$00
end

 data sfx_spawn
 $10,$10,$01 ; version, priority, frames per chunk
  $16, $04, $0e
  $15, $04, $0e
  $15, $04, $0e
  $12, $04, $0e
  $0e, $04, $0e
  $0c, $04, $0e
  $0e, $04, $0e
  $12, $04, $0e
  $15, $04, $0a
  $15, $04, $08
 $00,$00,$00
end

 data sfx_bubbleup
 $10,$10,$00 ; version, priority, frames per chunk
 $1e,$04,$0f ; first chunk of freq,channel,volume
 $1e,$04,$0f
 $12,$0c,$0e
 $19,$04,$06
 $10,$0c,$0f
 $10,$0c,$0d
 $1c,$04,$0f
 $1c,$04,$0e
 $10,$0c,$0f
 $10,$0c,$0f
 $10,$0c,$0f
 $1b,$04,$0a
 $1b,$04,$0e
 $1b,$04,$0b
 $0e,$0c,$0f
 $0e,$0c,$0e
 $10,$0c,$07
 $19,$04,$0c
 $19,$04,$08
 $0d,$0c,$07
 $0e,$0c,$07
 $0d,$0c,$0d
 $0d,$0c,$08
 $18,$04,$06
 $07,$0c,$04
 $0d,$0c,$07
 $0d,$0c,$0b
 $13,$04,$01
 $16,$04,$04
 $16,$04,$06
 $16,$04,$03
 $0c,$0c,$04
 $0c,$0c,$06
 $0c,$0c,$04
 $15,$04,$03
 $15,$04,$03
 $06,$0c,$01
 $0b,$0c,$02
 $0b,$0c,$04
 $0b,$0c,$02
 $00,$00,$00
end

 data sfx_yahoo
 $10,$10,$00 ; version, priority, frames per chunk
 $01,$0c,$02 ; first chunk of freq,channel,volume
 $18,$04,$08
 $07,$0c,$0f
 $16,$04,$0f
 $03,$0c,$05
 $0a,$04,$06
 $15,$04,$08
 $18,$04,$04
 $18,$04,$04
 $1c,$04,$04
 $1c,$04,$04
 $1c,$04,$02
 $0b,$0c,$02
 $19,$04,$01
 $19,$04,$00
 $1c,$04,$01
 $1b,$04,$03
 $19,$04,$06
 $18,$04,$04
 $1e,$04,$06
 $19,$04,$0f
 $18,$04,$0f
 $18,$04,$0f
 $18,$04,$0b
 $18,$04,$0f
 $18,$04,$0f
 $18,$04,$0f
 $18,$04,$05
 $18,$04,$08
 $19,$04,$0f
 $1b,$04,$08
 $1b,$04,$06
 $1c,$04,$0f
 $1c,$04,$0f
 $1e,$04,$08
 $1e,$04,$0e
 $1e,$04,$0b
 $0a,$0c,$08
 $0a,$0c,$0f
 $0b,$0c,$0f
 $0b,$0c,$0e
 $0b,$0c,$09
 $0c,$0c,$06
 $0c,$0c,$02
 $0d,$0c,$03
 $0d,$0c,$03
 $0d,$0c,$01
 $0e,$0c,$00
 $17,$0c,$00
 $00,$00,$00
end

 data sfx_hahaha2
 $10,$10,$01 ; version, priority, frames per chunk
 $14,$0f,$00 ; first chunk of freq,channel,volume
 $06,$07,$02
 $0a,$04,$0e
 $0b,$04,$0f
 $00,$06,$0f
 $0b,$04,$07
 $10,$04,$04
 $0d,$04,$02
 $10,$04,$02
 $10,$04,$0f
 $0f,$04,$0f
 $0d,$04,$07
 $00,$06,$02
 $10,$04,$04
 $00,$06,$02
 $00,$06,$0f
 $00,$06,$0f
 $00,$06,$0f
 $10,$04,$09
 $0c,$04,$08
 $00,$06,$07
 $11,$04,$06
 $10,$04,$05
 $00,$06,$04
 $10,$04,$03
 $00,$06,$02
 $0b,$04,$01
 $00,$00,$00
end

 data sfx_strum
 $10,$10,$00 ; version, priority, frames per chunk
 $0c,$0c,$09 ; first chunk of freq,channel,volume
 $0c,$0c,$0f
 $0c,$0c,$0f
 $0c,$0c,$0f
 $0c,$0c,$0f
 $1e,$04,$0f
 $1e,$04,$0e
 $07,$0c,$0f
 $07,$0c,$0f
 $07,$0c,$0b
 $1e,$04,$0b
 $07,$0c,$0c
 $0c,$0c,$09
 $07,$0c,$07
 $07,$0c,$06
 $07,$0c,$05
 $07,$0c,$04
 $07,$0c,$04
 $07,$0c,$03
 $07,$0c,$03
 $07,$0c,$02
 $07,$0c,$02
 $07,$0c,$01
 $07,$0c,$01
 $00,$00,$00
end

   data sfx_jumpman
   16, 5, 1 ; version, priority, frames per chunk
   $1E,$04,$08 ; 1st chunk of freq,channel,volume data
   $1B,$04,$08 ; 2nd chunk
   $18,$04,$08 ; 3rd chunk
   $00,$00,$00 ; End Of Sound marker
end

  data sfx_cavalry
  $10,$07,$05 ; version, priority, frames per chunk
  $1D,$04,$08 ; first chunk of freq,channel,volume data 
  $1A,$04,$08
  $17,$04,$08
  $13,$04,$08
  $17,$04,$08
  $13,$04,$08
  $13,$04,$08
  $00,$00,$00 
end

 data sfx_dropped
 $10,$10,$00 ; version, priority, frames per chunk
 $0a,$06,$0f ; first chunk of freq,channel,volume
 $0a,$06,$0f
 $0a,$06,$06
 $0c,$04,$02
 $1b,$0c,$01
 $07,$06,$0f
 $0a,$06,$0f
 $0a,$06,$0f
 $0a,$06,$07
 $07,$04,$06
 $07,$04,$04
 $07,$06,$0f
 $07,$06,$0d
 $07,$04,$0e
 $07,$04,$06
 $07,$04,$03
 $0a,$06,$09
 $0a,$06,$0f
 $07,$04,$0f
 $07,$04,$05
 $07,$04,$05
 $1b,$0c,$08
 $07,$04,$0d
 $07,$04,$07
 $07,$04,$07
 $07,$04,$03
 $07,$04,$07
 $07,$04,$05
 $07,$04,$03
 $07,$04,$01
 $00,$00,$00
end

 data sfx_longgongsilver
 $10,$10,$00 ; version, priority, frames per chunk
 $1b,$0c,$0b ; first chunk of freq,channel,volume
 $06,$0c,$07
 $1b,$0c,$0f
 $0a,$06,$0d
 $1b,$0c,$0b
 $0a,$06,$0f
 $0a,$06,$0c
 $12,$0c,$0a
 $1b,$0c,$0f
 $12,$0c,$0b
 $1b,$0c,$0f
 $1b,$0c,$0f
 $12,$0c,$0c
 $1b,$0c,$0f
 $0a,$06,$0d
 $12,$0c,$07
 $0a,$06,$0f
 $1b,$0c,$0f
 $0a,$06,$07
 $1b,$0c,$0f
 $0a,$06,$06
 $1b,$0c,$0f
 $0a,$06,$0f
 $06,$0c,$07
 $0a,$06,$0e
 $1b,$0c,$0f
 $12,$0c,$08
 $1b,$0c,$0f
 $1b,$0c,$0c
 $1b,$0c,$0f
 $1b,$0c,$0e
 $12,$0c,$08
 $0a,$06,$0c
 $1b,$0c,$0d
 $12,$0c,$08
 $1b,$0c,$0f
 $1b,$0c,$0f
 $1b,$0c,$0a
 $1b,$0c,$0f
 $0a,$06,$0a
 $0a,$06,$08
 $0a,$06,$0f
 $1b,$0c,$09
 $0a,$06,$0c
 $1b,$0c,$0f
 $06,$0c,$05
 $1b,$0c,$0f
 $0a,$06,$0b
 $12,$0c,$06
 $0a,$06,$0c
 $1b,$0c,$08
 $12,$0c,$0a
 $1b,$0c,$0d
 $1b,$0c,$06
 $1b,$0c,$0c
 $1b,$0c,$0b
 $1b,$0c,$04
 $0a,$06,$06
 $0a,$06,$07
 $0a,$06,$04
 $1b,$0c,$09
 $1b,$0c,$07
 $1b,$0c,$06
 $1b,$0c,$07
 $12,$0c,$07
 $1b,$0c,$06
 $12,$0c,$07
 $12,$0c,$05
 $0a,$06,$05
 $1b,$0c,$06
 $0a,$0c,$02
 $1b,$0c,$08
 $0a,$06,$03
 $1b,$0c,$04
 $0a,$06,$06
 $12,$0c,$02
 $1b,$0c,$02
 $1b,$0c,$05
 $12,$0c,$02
 $1b,$0c,$05
 $0a,$06,$04
 $1b,$0c,$02
 $1b,$0c,$04
 $1b,$0c,$01
 $12,$0c,$03
 $0a,$06,$04
 $1b,$0c,$02
 $1b,$0c,$01
 $1b,$0c,$04
 $0a,$06,$02
 $1b,$0c,$02
 $0a,$06,$02
 $00,$00,$00
end

 data sfx_uncovered
 $10,$10,$00 ; version, priority, frames per chunk
 $1e,$0c,$02 ; first chunk of freq,channel,volume
 $1b,$0c,$06
 $1b,$0c,$0c
 $1b,$0c,$0e
 $1b,$0c,$0c
 $0c,$0c,$04
 $17,$0c,$0f
 $17,$0c,$05
 $1b,$0c,$07
 $17,$0c,$05
 $17,$0c,$0f
 $17,$0c,$0f
 $17,$0c,$0f
 $03,$06,$0f
 $03,$06,$0f
 $1e,$06,$0a
 $17,$0c,$0c
 $10,$0c,$0f
 $10,$0c,$0f
 $10,$0c,$07
 $10,$0c,$03
 $17,$0c,$03
 $12,$0c,$07
 $03,$06,$04
 $03,$06,$02
 $0e,$0c,$01
 $12,$0c,$02
 $12,$0c,$03
 $03,$06,$01
 $03,$06,$01
 $10,$0c,$01
 $12,$0c,$01
 $10,$0c,$01
 $10,$0c,$01
 $00,$00,$00
end

 data sfx_squeek
 $10,$10,$00 ; version, priority, frames per chunk
 $06,$0c,$0f ; first chunk of freq,channel,volume
 $15,$04,$0f
 $06,$0c,$0f
 $06,$0c,$0f
 $0a,$04,$0f
 $0a,$04,$0f
 $0c,$04,$0f
 $0d,$04,$0f
 $03,$0c,$0f
 $06,$0c,$0d
 $03,$0c,$07
 $0d,$04,$06
 $06,$0c,$04
 $15,$04,$08
 $06,$0c,$03
 $06,$0c,$05
 $15,$04,$04
 $15,$04,$04
 $06,$0c,$04
 $06,$0c,$03
 $03,$0c,$01
 $15,$04,$02
 $06,$0c,$03
 $06,$0c,$03
 $0c,$04,$00
 $15,$04,$02
 $15,$04,$01
 $00,$00,$00
end

 data sfx_doorpound
 $10,$10,$00 ; version, priority, frames per chunk
 $0f,$06,$0f ; first chunk of freq,channel,volume
 $1e,$0c,$0f
 $0f,$06,$0f
 $1e,$06,$0f
 $0a,$06,$0f
 $07,$06,$0b
 $0f,$06,$0f
 $0f,$06,$0c
 $07,$06,$0a
 $07,$06,$09
 $0f,$06,$0b
 $1e,$06,$07
 $0a,$06,$06
 $0a,$06,$08
 $0f,$06,$06
 $0a,$06,$05
 $1e,$06,$03
 $0f,$06,$02
 $00,$00,$00
end


  data sfx_nonobounce
 $10,$10,$01 ; version, priority, frames per chunk
 $0f,$0c,$04
 $00,$0e,$08
 $10,$0c,$08
 $02,$06,$06
 $10,$0c,$06
 $02,$06,$06
 $00,$0e,$08
 $10,$0c,$08
 $02,$06,$08
 $0f,$0c,$06
 $10,$0c,$06
 $02,$06,$06
 $10,$0c,$06
 $0f,$0c,$04
 $10,$0c,$04
 $0f,$0c,$04
 $00,$0e,$04
 $10,$0c,$04
 $10,$0c,$02
 0,0,0
end

 data sfx_denied
 $10,$10,$00 ; version, priority, frames per chunk
 $0e,$0c,$0d ; first chunk of freq,channel,volume
 $10,$0c,$0a
 $10,$0c,$0c
 $1e,$06,$05
 $1e,$06,$09
 $12,$0c,$07
 $12,$0c,$0b
 $12,$0c,$0f
 $12,$0c,$0b
 $12,$0c,$07
 $03,$06,$05
 $03,$06,$04
 $03,$06,$09
 $1e,$06,$03
 $12,$0c,$07
 $12,$0c,$07
 $12,$0c,$05
 $1e,$06,$05
 $1f,$0c,$03
 $1f,$0c,$02
 $1f,$0c,$01
 $00,$00,$00
end

  data sfx_echo2
  $10,$05,$04 ; version, priority, frames per chunk
  $1F,$04,$0A ; first chunk of freq,channel,volume data 
  $01,$00,$00
  $1F,$04,$05
  $01,$00,$00
  $1F,$04,$02
  $00,$00,$00 
end

 data sfx_oneup
 $10,$10,$00 ; version, priority, frames per chunk
 $16,$04,$0f ; first chunk of freq,channel,volume
 $16,$04,$0f
 $16,$04,$08
 $13,$04,$04
 $13,$04,$02
 $12,$06,$01
 $12,$06,$01
 $12,$04,$0f
 $12,$04,$0f
 $12,$04,$08
 $11,$04,$04
 $10,$04,$02
 $10,$04,$01
 $10,$04,$01
 $0a,$04,$08
 $0a,$04,$08
 $1e,$06,$00
 $1e,$06,$00
 $1e,$06,$00
 $1e,$06,$00
 $0d,$04,$08
 $0d,$04,$08
 $0d,$04,$08
 $0d,$04,$06
 $1e,$06,$00
 $1e,$06,$00
 $1e,$06,$00
 $0c,$04,$08
 $0c,$04,$04
 $1e,$06,$00
 $1e,$06,$00
 $1e,$06,$00
 $1e,$06,$00
 $09,$04,$06
 $01,$04,$02
 $09,$04,$04
 $09,$04,$02
 $09,$04,$04
 $09,$04,$04
 $01,$04,$02
 $09,$04,$04
 $01,$04,$00
 $09,$04,$04
 $01,$04,$02
 $09,$04,$04
 $09,$04,$02
 $09,$04,$02
 $09,$04,$04
 $01,$04,$02
 $01,$04,$01
 $00,$00,$00
end

fadeIn 
 temp1 = tile1color[level]
 temp2 = tile2color[level]
 temp3 = deepBackgroundColor[level]
 _P0C1 = temp1 : _P0C2 = temp1 : _P0C3 = temp1
 _P1C1 = temp2 : _P1C2 = temp2 : _P1C3 = temp2

 _P2C1 = temp3 
 _P2C2 = $20 : _P2C3 = $20
 ;_P2C2 = $30 : _P2C3 = $20
 ;_P2C2 = $00 : _P2C3 = $30 ;bubblor

 _P3C3 = $01 ;all white colors re-use this
 /*
 _P3C2 = $d0

 _P4C2 = $30

 _P5C2 = $50

 _P6C2 = $90
*/
 _P3C2 = $d9

 _P4C2 = $39

 _P5C2 = $59

 _P6C2 = $99

/*grym färg
  P7C1 = $21
 P7C2 = $b6
 P7C3 = $b9
*/
 _P7C2 = $70


 customTemp2 = $0f - $09 ;set delay counters
 customTemp4 = $0f - $05
 customTemp5 = $0f - $01
 customTemp6 = $0f - $0d




 for customTemp1 = 0 to 17 ; 16?

  restorescreen
     ;plotsprite zero 4 140 0 _lives 1 ;drawwait, will update colors offscreen

 
  ;I use less than OR EQUAL so the target value gets stored in the register
  ;since it gets stored first then incremented, which it has to to show the first darkest value
  if _P0C1 & #$0F <= #$05 &&  customTemp4 = 0 then P0C1 = _P0C1 : _P0C1 = _P0C1 + 1 
  if _P0C2 & #$0F <= #$09 &&  customTemp2 = 0 then P0C2 = _P0C2 : _P0C2 = _P0C2 + 1
  if _P0C3 & #$0F <= #$0d &&  customTemp6 = 0 then P0C3 = _P0C3 : _P0C3 = _P0C3 + 1 

  if _P1C1 & #$0F <= #$05 &&  customTemp4 = 0 then P1C1 = _P1C1 : _P1C1 = _P1C1 + 1
  if _P1C2 & #$0F <= #$09 &&  customTemp2 = 0 then P1C2 = _P1C2 : _P1C2 = _P1C2 + 1
  if _P1C3 & #$0F <= #$0d &&  customTemp6 = 0 then P1C3 = _P1C3 : _P1C3 = _P1C3 + 1

  if _P2C1 & #$0F <= #$01 &&  customTemp5 = 0 then P2C1 = _P2C1 : P7C1 = _P2C1 : _P2C1 = _P2C1 + 1 
  if _P2C2 & #$0F <= #$09 &&  customTemp2 = 0 then P2C2 = _P2C2 : _P2C2 = _P2C2 + 1
  if _P2C3 & #$0F <= #$0d &&  customTemp6 = 0 then P2C3 = _P2C3 : _P2C3 = _P2C3 + 1

   P3C3 =  _P3C3  
   P4C3 =  _P3C3 
   P5C3 =  _P3C3 
   P6C3 =  _P3C3 
     if _P3C3 & #$0F < #$0F then _P3C3 = _P3C3 + 1

  ;these doesn't need to fade in, only 4, used for text
  if _P3C2 & #$0F <= #$09 &&  customTemp2 = 0 then P3C2 = _P3C2 : _P3C2 = _P3C2 + 1
  if _P4C2 & #$0F <= #$09 &&  customTemp2 = 0 then P4C2 = _P4C2 : _P4C2 = _P4C2 + 1
  if _P5C2 & #$0F <= #$09 &&  customTemp2 = 0 then P5C2 = _P5C2 : _P5C2 = _P5C2 + 1
  if _P6C2 & #$0F <= #$09 &&  customTemp2 = 0 then P6C2 = _P6C2 : _P6C2 = _P6C2 + 1

  P7C2 = _P7C2
  if _P7C2 & #$0F < #$0f then _P7C2 = _P7C2 + 1

   if customTemp2 > 0 then customTemp2 =  customTemp2 - 1
   if customTemp4 > 0 then customTemp4 =  customTemp4 - 1
   if customTemp5 > 0 then customTemp5 =  customTemp5 - 1
   if customTemp6 > 0 then customTemp6 =  customTemp6 - 1


 drawscreen
 drawwait
 next
 ;P7C1 = $21
 ;P7C2 = $7f ;$4f
 P7C3 = $b2 ;20
 return thisbank

fadeOut
 ;gosub clearPalettes
 temp1 = tile1color[level]
 temp2 = tile2color[level]
 temp3 = deepBackgroundColor[level]

 _P0C1 = temp1 + $05 ;color + brightness
  _P0C2 = temp1 + $09 
   _P0C3 = temp1 + $0d

 _P1C1 = temp2 + $05 
  _P1C2 = temp2 + $09
   _P1C3 = temp2 + $0d

 _P2C1 = temp3 + $01
  _P2C2 = $29 
   _P2C3 = $2d

   if playerBubbleCountdown > 0 then _P3C2 = $b5 : _P3C3 = $b9 else _P3C2 = $d9 : _P3C3 = $0f

 _P6C3 = $0f ; all white colors re-use this
 
 ;player and enemy colors
 ;_P3C2 = $d9

 _P4C2 = $39

 _P5C2 = $59

 _P6C2 = $99
 

;bubble
 P7C1 = $21
 _P7C2 = $7f ;4f
 _P7C3 = $b2 ;20

 for customTemp1 = 0 to 15
 restorescreen ;restore saved bg

 ;plot routines include something like drawwait so having them first avoids tearing when updating palettes
 ;gosub plotStuff
     plotsprite player 3 playerX playerY frame 1
     if playerBubbleCountdown > 0 then plotsprite bubbleOverlay 7 playerX playerY frame 1 ;: P3C2 = $b5 : P3C3 = $b9 else P3C2 = $d9 : P3C3 = $0f
     if numberOfEnemies >= 1 then plotsprite enemy 4 enemy1X enemy1Y frame 1
     if numberOfEnemies >= 2 then plotsprite enemy 5 enemy2X enemy2Y frame 1
     if numberOfEnemies >= 3 then plotsprite enemy 6 enemy3X enemy3Y frame 1
 

 gosub fadeBGtiles



  ;fade out player, enemy and numbers white
 P3C3 = _P3C3 
 if _P3C3 & #$0F > 0 then  _P3C3 = _P3C3 - 1 else _P3C3 = $00

 P4C3 = _P6C3 
 P5C3 = _P6C3 
 P6C3 = _P6C3 
 if _P6C3 & #$0F > 0 then  _P6C3 = _P6C3 - 1 else _P6C3 = $00 


 ;fade out player and enemy colors
 P3C2 = _P3C2
 P4C2 = _P4C2
 P5C2 = _P5C2
 P6C2 = _P6C2 
 if _P3C2 & #$0F > 0 then   _P3C2 = _P3C2 - 1 else _P3C2 = $00
 if _P4C2 & #$0F > 0 then   _P4C2 = _P4C2 - 1 else _P4C2 = $00
 if _P5C2 & #$0F > 0 then   _P5C2 = _P5C2 - 1 else _P5C2 = $00
 if _P6C2 & #$0F > 0 then   _P6C2 = _P6C2 - 1 else _P6C2 = $00


 ;fade out bubbles
 ;P7C1 same as dot bg
 P7C2 = _P7C2
 P7C3 = _P7C3
 ;if _P7C1 & #$0F > 0 then _P7C1 = _P7C1 - 1 else _P7C1 = $00
 if _P7C2 & #$0F > 0 then _P7C2 = _P7C2 - 1 else _P7C2 = $00
 if _P7C3 & #$0F > 0 then _P7C3 = _P7C3 - 1 else _P7C3 = $00

 drawscreen
 next




 /*
 ;delay until next screen
  for customTemp1 = 0 to 60
 restorescreen
 drawscreen
 next
 */

 return thisbank

fadeBGtiles
 P0C1 = _P0C1
 P0C2 = _P0C2
 P0C3 = _P0C3
 if _P0C1 & #$0F > 0 then _P0C1 = _P0C1 - 1 else _P0C1 = $00
 if _P0C2 & #$0F > 0 then _P0C2 = _P0C2 - 1 else _P0C2 = $00
 if _P0C3 & #$0F > 0 then _P0C3 = _P0C3 - 1 else _P0C3 = $00


 P1C1 = _P1C1
 P1C2 = _P1C2
 P1C3 = _P1C3
 if _P1C1 & #$0F > 0 then   _P1C1 = _P1C1 - 1 else _P1C1 = $00
 if _P1C2 & #$0F > 0 then   _P1C2 = _P1C2 - 1 else _P1C2 = $00
 if _P1C3 & #$0F > 0 then   _P1C3 = _P1C3 - 1 else _P1C3 = $00


 P2C1 = _P2C1 :  P7C1 = _P2C1 ; bubble and dot bg allways the same
 P2C2 = _P2C2
 P2C3 = _P2C3
 if _P2C1 & #$0F > 0 then   _P2C1 = _P2C1 - 1 else _P2C1 = $00
 if _P2C2 & #$0F > 0 then   _P2C2 = _P2C2 - 1 else _P2C2 = $00
 if _P2C3 & #$0F > 0 then   _P2C3 = _P2C3 - 1 else _P2C3 = $00
 return thisbank
/*
plotStuff

     plotsprite player 3 playerX playerY frame 1
     if playerBubbleCountdown > 0 then plotsprite bubbleOverlay 7 playerX playerY frame 1 ;: P3C2 = $b5 : P3C3 = $b9 else P3C2 = $d9 : P3C3 = $0f
     if numberOfEnemies >= 1 then plotsprite enemy 4 enemy1X enemy1Y frame 1
     if numberOfEnemies >= 2 then plotsprite enemy 5 enemy2X enemy2Y frame 1
     if numberOfEnemies >= 3 then plotsprite enemy 6 enemy3X enemy3Y frame 1

 return thisbank
*/


restoreScreen_plotStuff_drawScreen1
     restorescreen
     plotsprite player 3 playerX playerY frame 1
     if playerBubbleCountdown > 0 then plotsprite bubbleOverlay 7 playerX playerY frame 1 ;: P3C2 = $b5 : P3C3 = $b9 else P3C2 = $d9 : P3C3 = $0f
     plotsprite enemy 4 enemy1X enemy1Y frame 1
     ;plotsprite enemy 5 enemy2X enemy2Y frame 1
     ;plotsprite enemy 6 enemy3X enemy3Y frame 1
     drawscreen
 return thisbank

restoreScreen_plotStuff_drawScreen2
     restorescreen
     plotsprite player 3 playerX playerY frame 1
     if playerBubbleCountdown > 0 then plotsprite bubbleOverlay 7 playerX playerY frame 1 ;: P3C2 = $b5 : P3C3 = $b9 else P3C2 = $d9 : P3C3 = $0f
     plotsprite enemy 4 enemy1X enemy1Y frame 1
     plotsprite enemy 5 enemy2X enemy2Y frame 1
     ;plotsprite enemy 6 enemy3X enemy3Y frame 1
     drawscreen
 return thisbank

restoreScreen_plotStuff_drawScreen3
     restorescreen
     plotsprite player 3 playerX playerY frame 1
     if playerBubbleCountdown > 0 then plotsprite bubbleOverlay 7 playerX playerY frame 1 ;: P3C2 = $b5 : P3C3 = $b9 else P3C2 = $d9 : P3C3 = $0f
     plotsprite enemy 4 enemy1X enemy1Y frame 1
     plotsprite enemy 5 enemy2X enemy2Y frame 1
     plotsprite enemy 6 enemy3X enemy3Y frame 1
     drawscreen
 return thisbank


;streak

restoreScreen_plotStuff_streak_drawScreen1
     restorescreen
     plotsprite player 3 playerX playerY frame 1
     if playerBubbleCountdown > 0 then plotsprite bubbleOverlay 7 playerX playerY frame 1 ;: P3C2 = $b5 : P3C3 = $b9 else P3C2 = $d9 : P3C3 = $0f
     plotsprite enemy 4 enemy1X enemy1Y frame 1
     ;plotsprite enemy 5 enemy2X enemy2Y frame 1
     ;plotsprite enemy 6 enemy3X enemy3Y frame 1
      plotsprite streak 7 customTemp2  customTemp1 0 2
     drawscreen
 return thisbank

restoreScreen_plotStuff_streak_drawScreen2
     restorescreen
     plotsprite player 3 playerX playerY frame 1
     if playerBubbleCountdown > 0 then plotsprite bubbleOverlay 7 playerX playerY frame 1 ;: P3C2 = $b5 : P3C3 = $b9 else P3C2 = $d9 : P3C3 = $0f
     plotsprite enemy 4 enemy1X enemy1Y frame 1
     plotsprite enemy 5 enemy2X enemy2Y frame 1
     ;plotsprite enemy 6 enemy3X enemy3Y frame 1
      plotsprite streak 7 customTemp2  customTemp1 0 2
     drawscreen
 return thisbank

restoreScreen_plotStuff_streak_drawScreen3
     restorescreen
     plotsprite player 3 playerX playerY frame 1
     if playerBubbleCountdown > 0 then plotsprite bubbleOverlay 7 playerX playerY frame 1 ;: P3C2 = $b5 : P3C3 = $b9 else P3C2 = $d9 : P3C3 = $0f
     plotsprite enemy 4 enemy1X enemy1Y frame 1
     plotsprite enemy 5 enemy2X enemy2Y frame 1
     plotsprite enemy 6 enemy3X enemy3Y frame 1
      plotsprite streak 7 customTemp2  customTemp1 0 2
     drawscreen
 return thisbank

userinterrupt
  asm
    jsr SERVICEPOKEYSFX
end
 return thisbank

bottomscreenroutine


  WSYNC=1


 displaymode 320B



   return thisbank

topscreenroutine


  WSYNC=1


 displaymode 160A

  asm
    ;jsr SERVICEPOKEYSFX
end

  return thisbank

addCollectedDotsToTotal
 temp5 = totaldots_1 & $0f
 asm
	SED
 clc

 lda score5+2
 adc score0+2
 sta score5+2

 lda score5+1
 adc score0+1
 sta score5+1

 lda score5
 adc score0
 sta score5

	CLD

end
 if totaldots_1 & $0f < temp5 then score4 = score4 + 1

 score0 = 0

 totaldigits0 = (totaldots1 & $0f) * 2
 totaldigits1 = (totaldots2 / 16) * 2
 totaldigits2 = (totaldots2 & $0f) * 2
/*
 totaldigits3 = (totaldots_1 / 16) * 2
 totaldigits4 = (totaldots_2 & $0f) * 2
 totaldigits5 = (totaldots_2 / 16) * 2
*/

 totaldigits3 = (totaldots_1 & $0f) * 2
 totaldigits4 = (totaldots_2 / 16) * 2
 totaldigits5 = (totaldots_2 & $0f) * 2




/*
 for temp6 = 0 to 32
  restorescreen
  drawscreen
  next
  ;playsfx sfx_longgongsilver

    adjustvisible 0 5

 clearscreen
 customTemp1 = (224 / 2) - 32
  P4C3 = $0e
   plotchars totaldigits0 4 76 6 6
 ;both sprites are 32 pixel positions wide
 ;plotsprite game_ 0 44  customTemp1 0 2
 ;plotsprite over 0 84  customTemp1 0 2
 savescreen

 restorescreen ;one extra frame to not get artifacts
 drawscreen

 customTemp2 = $36
 customTemp3 = $56
 for customTemp1 = 0 to 64
 ;customTemp1 =  customTemp1 + 1
 if customTemp1{1} then  P0C3 = customTemp2 else P0C3 = customTemp3
 restorescreen
 drawscreen
 next

    gosub clearPalettes

  adjustvisible 1 14

 for temp5 = 0 to 16
  restorescreen
  drawscreen
  next
 */
 return thisbank

 rem --------------------------------------------------------------------------
 rem POKEY
 rem --------------------------------------------------------------------------
PlayPokeySound
 rem validate
 if pokeydetected then temp1 = PLAYPOKEYSFX(tuneId)
 return

StopAllSounds
 if pokeydetected then temp1 = STOPPOKEYSFX()
 return

 rem --------------------------------------------------------------------------
 rem ASSEMBLY
 rem --------------------------------------------------------------------------
 inline pokeyconfig.asm
 inline pokeymusic.asm