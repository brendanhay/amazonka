{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationSpecialFeature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationSpecialFeature where

import Network.AWS.Prelude

-- | Special features, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
data ReservationSpecialFeature
  = AdvancedAudio
  | AudioNormalization
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

instance FromText ReservationSpecialFeature where
  parser =
    takeLowerText >>= \case
      "advanced_audio" -> pure AdvancedAudio
      "audio_normalization" -> pure AudioNormalization
      e ->
        fromTextError $
          "Failure parsing ReservationSpecialFeature from value: '" <> e
            <> "'. Accepted values: advanced_audio, audio_normalization"

instance ToText ReservationSpecialFeature where
  toText = \case
    AdvancedAudio -> "ADVANCED_AUDIO"
    AudioNormalization -> "AUDIO_NORMALIZATION"

instance Hashable ReservationSpecialFeature

instance NFData ReservationSpecialFeature

instance ToByteString ReservationSpecialFeature

instance ToQuery ReservationSpecialFeature

instance ToHeader ReservationSpecialFeature

instance FromJSON ReservationSpecialFeature where
  parseJSON = parseJSONText "ReservationSpecialFeature"
