{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionRf
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionRf where

import Network.AWS.Prelude

-- | Specify how the service limits the audio dynamic range when compressing the audio.
data Eac3DynamicRangeCompressionRf
  = EDRCRFilmLight
  | EDRCRFilmStandard
  | EDRCRMusicLight
  | EDRCRMusicStandard
  | EDRCRNone
  | EDRCRSpeech
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

instance FromText Eac3DynamicRangeCompressionRf where
  parser =
    takeLowerText >>= \case
      "film_light" -> pure EDRCRFilmLight
      "film_standard" -> pure EDRCRFilmStandard
      "music_light" -> pure EDRCRMusicLight
      "music_standard" -> pure EDRCRMusicStandard
      "none" -> pure EDRCRNone
      "speech" -> pure EDRCRSpeech
      e ->
        fromTextError $
          "Failure parsing Eac3DynamicRangeCompressionRf from value: '" <> e
            <> "'. Accepted values: film_light, film_standard, music_light, music_standard, none, speech"

instance ToText Eac3DynamicRangeCompressionRf where
  toText = \case
    EDRCRFilmLight -> "FILM_LIGHT"
    EDRCRFilmStandard -> "FILM_STANDARD"
    EDRCRMusicLight -> "MUSIC_LIGHT"
    EDRCRMusicStandard -> "MUSIC_STANDARD"
    EDRCRNone -> "NONE"
    EDRCRSpeech -> "SPEECH"

instance Hashable Eac3DynamicRangeCompressionRf

instance NFData Eac3DynamicRangeCompressionRf

instance ToByteString Eac3DynamicRangeCompressionRf

instance ToQuery Eac3DynamicRangeCompressionRf

instance ToHeader Eac3DynamicRangeCompressionRf

instance ToJSON Eac3DynamicRangeCompressionRf where
  toJSON = toJSONText

instance FromJSON Eac3DynamicRangeCompressionRf where
  parseJSON = parseJSONText "Eac3DynamicRangeCompressionRf"
