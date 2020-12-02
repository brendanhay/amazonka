{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationFontColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationFontColor where

import Network.AWS.Prelude

-- | Dvb Sub Destination Font Color
data DvbSubDestinationFontColor
  = DSDFCBlack
  | DSDFCBlue
  | DSDFCGreen
  | DSDFCRed
  | DSDFCWhite
  | DSDFCYellow
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

instance FromText DvbSubDestinationFontColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure DSDFCBlack
      "blue" -> pure DSDFCBlue
      "green" -> pure DSDFCGreen
      "red" -> pure DSDFCRed
      "white" -> pure DSDFCWhite
      "yellow" -> pure DSDFCYellow
      e ->
        fromTextError $
          "Failure parsing DvbSubDestinationFontColor from value: '" <> e
            <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText DvbSubDestinationFontColor where
  toText = \case
    DSDFCBlack -> "BLACK"
    DSDFCBlue -> "BLUE"
    DSDFCGreen -> "GREEN"
    DSDFCRed -> "RED"
    DSDFCWhite -> "WHITE"
    DSDFCYellow -> "YELLOW"

instance Hashable DvbSubDestinationFontColor

instance NFData DvbSubDestinationFontColor

instance ToByteString DvbSubDestinationFontColor

instance ToQuery DvbSubDestinationFontColor

instance ToHeader DvbSubDestinationFontColor

instance ToJSON DvbSubDestinationFontColor where
  toJSON = toJSONText

instance FromJSON DvbSubDestinationFontColor where
  parseJSON = parseJSONText "DvbSubDestinationFontColor"
