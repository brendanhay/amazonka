{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimecodeBurninPosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimecodeBurninPosition where

import Network.AWS.Prelude

-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
data TimecodeBurninPosition
  = BottomCenter
  | BottomLeft
  | BottomRight
  | MiddleCenter
  | MiddleLeft
  | MiddleRight
  | TopCenter
  | TopLeft
  | TopRight
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText TimecodeBurninPosition where
  parser =
    takeLowerText >>= \case
      "bottom_center" -> pure BottomCenter
      "bottom_left" -> pure BottomLeft
      "bottom_right" -> pure BottomRight
      "middle_center" -> pure MiddleCenter
      "middle_left" -> pure MiddleLeft
      "middle_right" -> pure MiddleRight
      "top_center" -> pure TopCenter
      "top_left" -> pure TopLeft
      "top_right" -> pure TopRight
      e ->
        fromTextError $
          "Failure parsing TimecodeBurninPosition from value: '" <> e
            <> "'. Accepted values: bottom_center, bottom_left, bottom_right, middle_center, middle_left, middle_right, top_center, top_left, top_right"

instance ToText TimecodeBurninPosition where
  toText = \case
    BottomCenter -> "BOTTOM_CENTER"
    BottomLeft -> "BOTTOM_LEFT"
    BottomRight -> "BOTTOM_RIGHT"
    MiddleCenter -> "MIDDLE_CENTER"
    MiddleLeft -> "MIDDLE_LEFT"
    MiddleRight -> "MIDDLE_RIGHT"
    TopCenter -> "TOP_CENTER"
    TopLeft -> "TOP_LEFT"
    TopRight -> "TOP_RIGHT"

instance Hashable TimecodeBurninPosition

instance NFData TimecodeBurninPosition

instance ToByteString TimecodeBurninPosition

instance ToQuery TimecodeBurninPosition

instance ToHeader TimecodeBurninPosition

instance ToJSON TimecodeBurninPosition where
  toJSON = toJSONText

instance FromJSON TimecodeBurninPosition where
  parseJSON = parseJSONText "TimecodeBurninPosition"
