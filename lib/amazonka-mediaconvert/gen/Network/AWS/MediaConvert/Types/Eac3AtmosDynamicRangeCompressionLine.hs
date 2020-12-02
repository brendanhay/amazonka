{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine where

import Network.AWS.Prelude

-- | Specify the absolute peak level for a signal with dynamic range compression.
data Eac3AtmosDynamicRangeCompressionLine
  = EADRCLFilmLight
  | EADRCLFilmStandard
  | EADRCLMusicLight
  | EADRCLMusicStandard
  | EADRCLNone
  | EADRCLSpeech
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

instance FromText Eac3AtmosDynamicRangeCompressionLine where
  parser =
    takeLowerText >>= \case
      "film_light" -> pure EADRCLFilmLight
      "film_standard" -> pure EADRCLFilmStandard
      "music_light" -> pure EADRCLMusicLight
      "music_standard" -> pure EADRCLMusicStandard
      "none" -> pure EADRCLNone
      "speech" -> pure EADRCLSpeech
      e ->
        fromTextError $
          "Failure parsing Eac3AtmosDynamicRangeCompressionLine from value: '" <> e
            <> "'. Accepted values: film_light, film_standard, music_light, music_standard, none, speech"

instance ToText Eac3AtmosDynamicRangeCompressionLine where
  toText = \case
    EADRCLFilmLight -> "FILM_LIGHT"
    EADRCLFilmStandard -> "FILM_STANDARD"
    EADRCLMusicLight -> "MUSIC_LIGHT"
    EADRCLMusicStandard -> "MUSIC_STANDARD"
    EADRCLNone -> "NONE"
    EADRCLSpeech -> "SPEECH"

instance Hashable Eac3AtmosDynamicRangeCompressionLine

instance NFData Eac3AtmosDynamicRangeCompressionLine

instance ToByteString Eac3AtmosDynamicRangeCompressionLine

instance ToQuery Eac3AtmosDynamicRangeCompressionLine

instance ToHeader Eac3AtmosDynamicRangeCompressionLine

instance ToJSON Eac3AtmosDynamicRangeCompressionLine where
  toJSON = toJSONText

instance FromJSON Eac3AtmosDynamicRangeCompressionLine where
  parseJSON = parseJSONText "Eac3AtmosDynamicRangeCompressionLine"
