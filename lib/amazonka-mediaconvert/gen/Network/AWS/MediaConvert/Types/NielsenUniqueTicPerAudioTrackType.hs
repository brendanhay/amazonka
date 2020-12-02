{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType where

import Network.AWS.Prelude

-- | To create assets that have the same TIC values in each audio track, keep the default value Share TICs (SAME_TICS_PER_TRACK). To create assets that have unique TIC values for each audio track, choose Use unique TICs (RESERVE_UNIQUE_TICS_PER_TRACK).
data NielsenUniqueTicPerAudioTrackType
  = ReserveUniqueTicsPerTrack
  | SameTicsPerTrack
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

instance FromText NielsenUniqueTicPerAudioTrackType where
  parser =
    takeLowerText >>= \case
      "reserve_unique_tics_per_track" -> pure ReserveUniqueTicsPerTrack
      "same_tics_per_track" -> pure SameTicsPerTrack
      e ->
        fromTextError $
          "Failure parsing NielsenUniqueTicPerAudioTrackType from value: '" <> e
            <> "'. Accepted values: reserve_unique_tics_per_track, same_tics_per_track"

instance ToText NielsenUniqueTicPerAudioTrackType where
  toText = \case
    ReserveUniqueTicsPerTrack -> "RESERVE_UNIQUE_TICS_PER_TRACK"
    SameTicsPerTrack -> "SAME_TICS_PER_TRACK"

instance Hashable NielsenUniqueTicPerAudioTrackType

instance NFData NielsenUniqueTicPerAudioTrackType

instance ToByteString NielsenUniqueTicPerAudioTrackType

instance ToQuery NielsenUniqueTicPerAudioTrackType

instance ToHeader NielsenUniqueTicPerAudioTrackType

instance ToJSON NielsenUniqueTicPerAudioTrackType where
  toJSON = toJSONText

instance FromJSON NielsenUniqueTicPerAudioTrackType where
  parseJSON = parseJSONText "NielsenUniqueTicPerAudioTrackType"
