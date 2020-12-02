{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionLine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionLine where

import Network.AWS.Prelude

-- | Specify the absolute peak level for a signal with dynamic range compression.
data Eac3DynamicRangeCompressionLine
  = EDRCLFilmLight
  | EDRCLFilmStandard
  | EDRCLMusicLight
  | EDRCLMusicStandard
  | EDRCLNone
  | EDRCLSpeech
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

instance FromText Eac3DynamicRangeCompressionLine where
  parser =
    takeLowerText >>= \case
      "film_light" -> pure EDRCLFilmLight
      "film_standard" -> pure EDRCLFilmStandard
      "music_light" -> pure EDRCLMusicLight
      "music_standard" -> pure EDRCLMusicStandard
      "none" -> pure EDRCLNone
      "speech" -> pure EDRCLSpeech
      e ->
        fromTextError $
          "Failure parsing Eac3DynamicRangeCompressionLine from value: '" <> e
            <> "'. Accepted values: film_light, film_standard, music_light, music_standard, none, speech"

instance ToText Eac3DynamicRangeCompressionLine where
  toText = \case
    EDRCLFilmLight -> "FILM_LIGHT"
    EDRCLFilmStandard -> "FILM_STANDARD"
    EDRCLMusicLight -> "MUSIC_LIGHT"
    EDRCLMusicStandard -> "MUSIC_STANDARD"
    EDRCLNone -> "NONE"
    EDRCLSpeech -> "SPEECH"

instance Hashable Eac3DynamicRangeCompressionLine

instance NFData Eac3DynamicRangeCompressionLine

instance ToByteString Eac3DynamicRangeCompressionLine

instance ToQuery Eac3DynamicRangeCompressionLine

instance ToHeader Eac3DynamicRangeCompressionLine

instance ToJSON Eac3DynamicRangeCompressionLine where
  toJSON = toJSONText

instance FromJSON Eac3DynamicRangeCompressionLine where
  parseJSON = parseJSONText "Eac3DynamicRangeCompressionLine"
