{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationAlignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationAlignment where

import Network.AWS.Prelude

-- | Dvb Sub Destination Alignment
data DvbSubDestinationAlignment
  = Centered
  | Left'
  | Smart
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

instance FromText DvbSubDestinationAlignment where
  parser =
    takeLowerText >>= \case
      "centered" -> pure Centered
      "left" -> pure Left'
      "smart" -> pure Smart
      e ->
        fromTextError $
          "Failure parsing DvbSubDestinationAlignment from value: '" <> e
            <> "'. Accepted values: centered, left, smart"

instance ToText DvbSubDestinationAlignment where
  toText = \case
    Centered -> "CENTERED"
    Left' -> "LEFT"
    Smart -> "SMART"

instance Hashable DvbSubDestinationAlignment

instance NFData DvbSubDestinationAlignment

instance ToByteString DvbSubDestinationAlignment

instance ToQuery DvbSubDestinationAlignment

instance ToHeader DvbSubDestinationAlignment

instance ToJSON DvbSubDestinationAlignment where
  toJSON = toJSONText

instance FromJSON DvbSubDestinationAlignment where
  parseJSON = parseJSONText "DvbSubDestinationAlignment"
