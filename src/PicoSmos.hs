{-# LANGUAGE OverloadedStrings #-}
module PicoSmos
    ( picoSmos
    ) where

import Data.Maybe

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.Exit

import Cursor.Text

import Brick as Brick
import Brick.Main as Brick
import Brick.Widgets.Border as Brick
import Brick.Widgets.Center as Brick
import Brick.Widgets.Core as Brick

import Graphics.Vty.Attributes as Vty
import Graphics.Vty.Input.Events as Vty

picoSmos :: IO ()
picoSmos = do
    (file:_) <- getArgs
    exists <- doesFileExist file
    contents <-
        if exists
            then T.readFile file
            else pure T.empty
    case makeTextCursor contents of
        Nothing -> die "Contents did not represent a valid single line of text"
        Just tc -> do
            tc' <- Brick.defaultMain picoSmosApp tc
            T.writeFile file $ rebuildTextCursor tc'

picoSmosApp :: App TextCursor e Text
picoSmosApp =
    App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap Vty.defAttr []
    }

draw :: TextCursor -> [Widget Text]
draw tc =
    [ centerLayer $
      border $
      padAll 1 $
      showCursor "cursor" (Location (textCursorIndex tc, 0)) $
      txtWrap (rebuildTextCursor tc)
    ]

handleEvent :: TextCursor -> BrickEvent Text e -> EventM Text (Next TextCursor)
handleEvent tc e =
    case e of
        VtyEvent ve ->
            case ve of
                EvKey key mods ->
                    let mDo func = continue . fromMaybe tc $ func tc
                    in case key of
                           KChar c -> mDo $ textCursorInsert c
                           KLeft -> mDo textCursorSelectPrev
                           KRight -> mDo textCursorSelectNext
                           KBS -> mDo textCursorRemove
                           KHome -> continue $ textCursorSelectStart tc
                           KEnd -> continue $ textCursorSelectEnd tc
                           KDel -> mDo textCursorDelete
                           KEsc -> halt tc
                           KEnter -> halt tc
                           _ -> continue tc
                _ -> continue tc
        _ -> continue tc
