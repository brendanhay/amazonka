{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleFontColor where

import Network.AWS.Prelude

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
data DvbSubtitleFontColor
  = DSFCBlack
  | DSFCBlue
  | DSFCGreen
  | DSFCRed
  | DSFCWhite
  | DSFCYellow
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

instance FromText DvbSubtitleFontColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure DSFCBlack
      "blue" -> pure DSFCBlue
      "green" -> pure DSFCGreen
      "red" -> pure DSFCRed
      "white" -> pure DSFCWhite
      "yellow" -> pure DSFCYellow
      e ->
        fromTextError $
          "Failure parsing DvbSubtitleFontColor from value: '" <> e
            <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText DvbSubtitleFontColor where
  toText = \case
    DSFCBlack -> "BLACK"
    DSFCBlue -> "BLUE"
    DSFCGreen -> "GREEN"
    DSFCRed -> "RED"
    DSFCWhite -> "WHITE"
    DSFCYellow -> "YELLOW"

instance Hashable DvbSubtitleFontColor

instance NFData DvbSubtitleFontColor

instance ToByteString DvbSubtitleFontColor

instance ToQuery DvbSubtitleFontColor

instance ToHeader DvbSubtitleFontColor

instance ToJSON DvbSubtitleFontColor where
  toJSON = toJSONText

instance FromJSON DvbSubtitleFontColor where
  parseJSON = parseJSONText "DvbSubtitleFontColor"
