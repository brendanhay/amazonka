{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleFontColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninSubtitleFontColor where

import Network.AWS.Prelude

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
data BurninSubtitleFontColor
  = BSFCBlack
  | BSFCBlue
  | BSFCGreen
  | BSFCRed
  | BSFCWhite
  | BSFCYellow
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

instance FromText BurninSubtitleFontColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure BSFCBlack
      "blue" -> pure BSFCBlue
      "green" -> pure BSFCGreen
      "red" -> pure BSFCRed
      "white" -> pure BSFCWhite
      "yellow" -> pure BSFCYellow
      e ->
        fromTextError $
          "Failure parsing BurninSubtitleFontColor from value: '" <> e
            <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText BurninSubtitleFontColor where
  toText = \case
    BSFCBlack -> "BLACK"
    BSFCBlue -> "BLUE"
    BSFCGreen -> "GREEN"
    BSFCRed -> "RED"
    BSFCWhite -> "WHITE"
    BSFCYellow -> "YELLOW"

instance Hashable BurninSubtitleFontColor

instance NFData BurninSubtitleFontColor

instance ToByteString BurninSubtitleFontColor

instance ToQuery BurninSubtitleFontColor

instance ToHeader BurninSubtitleFontColor

instance ToJSON BurninSubtitleFontColor where
  toJSON = toJSONText

instance FromJSON BurninSubtitleFontColor where
  parseJSON = parseJSONText "BurninSubtitleFontColor"
