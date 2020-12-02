{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleOutlineColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleOutlineColor where

import Network.AWS.Prelude

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
data DvbSubtitleOutlineColor
  = DSOCBlack
  | DSOCBlue
  | DSOCGreen
  | DSOCRed
  | DSOCWhite
  | DSOCYellow
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

instance FromText DvbSubtitleOutlineColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure DSOCBlack
      "blue" -> pure DSOCBlue
      "green" -> pure DSOCGreen
      "red" -> pure DSOCRed
      "white" -> pure DSOCWhite
      "yellow" -> pure DSOCYellow
      e ->
        fromTextError $
          "Failure parsing DvbSubtitleOutlineColor from value: '" <> e
            <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText DvbSubtitleOutlineColor where
  toText = \case
    DSOCBlack -> "BLACK"
    DSOCBlue -> "BLUE"
    DSOCGreen -> "GREEN"
    DSOCRed -> "RED"
    DSOCWhite -> "WHITE"
    DSOCYellow -> "YELLOW"

instance Hashable DvbSubtitleOutlineColor

instance NFData DvbSubtitleOutlineColor

instance ToByteString DvbSubtitleOutlineColor

instance ToQuery DvbSubtitleOutlineColor

instance ToHeader DvbSubtitleOutlineColor

instance ToJSON DvbSubtitleOutlineColor where
  toJSON = toJSONText

instance FromJSON DvbSubtitleOutlineColor where
  parseJSON = parseJSONText "DvbSubtitleOutlineColor"
