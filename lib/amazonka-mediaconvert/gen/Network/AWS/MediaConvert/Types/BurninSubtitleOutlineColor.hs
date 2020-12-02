{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleOutlineColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninSubtitleOutlineColor where

import Network.AWS.Prelude

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
data BurninSubtitleOutlineColor
  = BSOCBlack
  | BSOCBlue
  | BSOCGreen
  | BSOCRed
  | BSOCWhite
  | BSOCYellow
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

instance FromText BurninSubtitleOutlineColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure BSOCBlack
      "blue" -> pure BSOCBlue
      "green" -> pure BSOCGreen
      "red" -> pure BSOCRed
      "white" -> pure BSOCWhite
      "yellow" -> pure BSOCYellow
      e ->
        fromTextError $
          "Failure parsing BurninSubtitleOutlineColor from value: '" <> e
            <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText BurninSubtitleOutlineColor where
  toText = \case
    BSOCBlack -> "BLACK"
    BSOCBlue -> "BLUE"
    BSOCGreen -> "GREEN"
    BSOCRed -> "RED"
    BSOCWhite -> "WHITE"
    BSOCYellow -> "YELLOW"

instance Hashable BurninSubtitleOutlineColor

instance NFData BurninSubtitleOutlineColor

instance ToByteString BurninSubtitleOutlineColor

instance ToQuery BurninSubtitleOutlineColor

instance ToHeader BurninSubtitleOutlineColor

instance ToJSON BurninSubtitleOutlineColor where
  toJSON = toJSONText

instance FromJSON BurninSubtitleOutlineColor where
  parseJSON = parseJSONText "BurninSubtitleOutlineColor"
