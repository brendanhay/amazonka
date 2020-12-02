{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationMaximumBitrate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationMaximumBitrate where

import Network.AWS.Prelude

-- | Maximum bitrate in megabits per second
data ReservationMaximumBitrate
  = RMBMax10Mbps
  | RMBMax20Mbps
  | RMBMax50Mbps
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

instance FromText ReservationMaximumBitrate where
  parser =
    takeLowerText >>= \case
      "max_10_mbps" -> pure RMBMax10Mbps
      "max_20_mbps" -> pure RMBMax20Mbps
      "max_50_mbps" -> pure RMBMax50Mbps
      e ->
        fromTextError $
          "Failure parsing ReservationMaximumBitrate from value: '" <> e
            <> "'. Accepted values: max_10_mbps, max_20_mbps, max_50_mbps"

instance ToText ReservationMaximumBitrate where
  toText = \case
    RMBMax10Mbps -> "MAX_10_MBPS"
    RMBMax20Mbps -> "MAX_20_MBPS"
    RMBMax50Mbps -> "MAX_50_MBPS"

instance Hashable ReservationMaximumBitrate

instance NFData ReservationMaximumBitrate

instance ToByteString ReservationMaximumBitrate

instance ToQuery ReservationMaximumBitrate

instance ToHeader ReservationMaximumBitrate

instance FromJSON ReservationMaximumBitrate where
  parseJSON = parseJSONText "ReservationMaximumBitrate"
