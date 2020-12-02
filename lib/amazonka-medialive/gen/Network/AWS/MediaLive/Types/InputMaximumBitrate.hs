{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputMaximumBitrate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputMaximumBitrate where

import Network.AWS.Prelude

-- | Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are supported currently.
data InputMaximumBitrate
  = Max10Mbps
  | Max20Mbps
  | Max50Mbps
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

instance FromText InputMaximumBitrate where
  parser =
    takeLowerText >>= \case
      "max_10_mbps" -> pure Max10Mbps
      "max_20_mbps" -> pure Max20Mbps
      "max_50_mbps" -> pure Max50Mbps
      e ->
        fromTextError $
          "Failure parsing InputMaximumBitrate from value: '" <> e
            <> "'. Accepted values: max_10_mbps, max_20_mbps, max_50_mbps"

instance ToText InputMaximumBitrate where
  toText = \case
    Max10Mbps -> "MAX_10_MBPS"
    Max20Mbps -> "MAX_20_MBPS"
    Max50Mbps -> "MAX_50_MBPS"

instance Hashable InputMaximumBitrate

instance NFData InputMaximumBitrate

instance ToByteString InputMaximumBitrate

instance ToQuery InputMaximumBitrate

instance ToHeader InputMaximumBitrate

instance ToJSON InputMaximumBitrate where
  toJSON = toJSONText

instance FromJSON InputMaximumBitrate where
  parseJSON = parseJSONText "InputMaximumBitrate"
