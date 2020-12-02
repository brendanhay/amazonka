{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf where

import Network.AWS.Prelude

-- | Specify how the service limits the audio dynamic range when compressing the audio.
data Eac3AtmosDynamicRangeCompressionRf
  = EADRCRFilmLight
  | EADRCRFilmStandard
  | EADRCRMusicLight
  | EADRCRMusicStandard
  | EADRCRNone
  | EADRCRSpeech
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

instance FromText Eac3AtmosDynamicRangeCompressionRf where
  parser =
    takeLowerText >>= \case
      "film_light" -> pure EADRCRFilmLight
      "film_standard" -> pure EADRCRFilmStandard
      "music_light" -> pure EADRCRMusicLight
      "music_standard" -> pure EADRCRMusicStandard
      "none" -> pure EADRCRNone
      "speech" -> pure EADRCRSpeech
      e ->
        fromTextError $
          "Failure parsing Eac3AtmosDynamicRangeCompressionRf from value: '" <> e
            <> "'. Accepted values: film_light, film_standard, music_light, music_standard, none, speech"

instance ToText Eac3AtmosDynamicRangeCompressionRf where
  toText = \case
    EADRCRFilmLight -> "FILM_LIGHT"
    EADRCRFilmStandard -> "FILM_STANDARD"
    EADRCRMusicLight -> "MUSIC_LIGHT"
    EADRCRMusicStandard -> "MUSIC_STANDARD"
    EADRCRNone -> "NONE"
    EADRCRSpeech -> "SPEECH"

instance Hashable Eac3AtmosDynamicRangeCompressionRf

instance NFData Eac3AtmosDynamicRangeCompressionRf

instance ToByteString Eac3AtmosDynamicRangeCompressionRf

instance ToQuery Eac3AtmosDynamicRangeCompressionRf

instance ToHeader Eac3AtmosDynamicRangeCompressionRf

instance ToJSON Eac3AtmosDynamicRangeCompressionRf where
  toJSON = toJSONText

instance FromJSON Eac3AtmosDynamicRangeCompressionRf where
  parseJSON = parseJSONText "Eac3AtmosDynamicRangeCompressionRf"
