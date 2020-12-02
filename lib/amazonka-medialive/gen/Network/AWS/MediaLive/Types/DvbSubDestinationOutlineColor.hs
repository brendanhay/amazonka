{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor where

import Network.AWS.Prelude

-- | Dvb Sub Destination Outline Color
data DvbSubDestinationOutlineColor
  = Black
  | Blue
  | Green
  | Red
  | White
  | Yellow
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

instance FromText DvbSubDestinationOutlineColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure Black
      "blue" -> pure Blue
      "green" -> pure Green
      "red" -> pure Red
      "white" -> pure White
      "yellow" -> pure Yellow
      e ->
        fromTextError $
          "Failure parsing DvbSubDestinationOutlineColor from value: '" <> e
            <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText DvbSubDestinationOutlineColor where
  toText = \case
    Black -> "BLACK"
    Blue -> "BLUE"
    Green -> "GREEN"
    Red -> "RED"
    White -> "WHITE"
    Yellow -> "YELLOW"

instance Hashable DvbSubDestinationOutlineColor

instance NFData DvbSubDestinationOutlineColor

instance ToByteString DvbSubDestinationOutlineColor

instance ToQuery DvbSubDestinationOutlineColor

instance ToHeader DvbSubDestinationOutlineColor

instance ToJSON DvbSubDestinationOutlineColor where
  toJSON = toJSONText

instance FromJSON DvbSubDestinationOutlineColor where
  parseJSON = parseJSONText "DvbSubDestinationOutlineColor"
