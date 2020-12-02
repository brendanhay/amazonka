{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpAdMarkers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpAdMarkers where

import Network.AWS.Prelude

-- | Rtmp Ad Markers
data RtmpAdMarkers = OnCuePointSCTE35
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

instance FromText RtmpAdMarkers where
  parser =
    takeLowerText >>= \case
      "on_cue_point_scte35" -> pure OnCuePointSCTE35
      e ->
        fromTextError $
          "Failure parsing RtmpAdMarkers from value: '" <> e
            <> "'. Accepted values: on_cue_point_scte35"

instance ToText RtmpAdMarkers where
  toText = \case
    OnCuePointSCTE35 -> "ON_CUE_POINT_SCTE35"

instance Hashable RtmpAdMarkers

instance NFData RtmpAdMarkers

instance ToByteString RtmpAdMarkers

instance ToQuery RtmpAdMarkers

instance ToHeader RtmpAdMarkers

instance ToJSON RtmpAdMarkers where
  toJSON = toJSONText

instance FromJSON RtmpAdMarkers where
  parseJSON = parseJSONText "RtmpAdMarkers"
