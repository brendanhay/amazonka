{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3DrcLine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3DrcLine where

import Network.AWS.Prelude

-- | Eac3 Drc Line
data Eac3DrcLine
  = EDLFilmLight
  | EDLFilmStandard
  | EDLMusicLight
  | EDLMusicStandard
  | EDLNone
  | EDLSpeech
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

instance FromText Eac3DrcLine where
  parser =
    takeLowerText >>= \case
      "film_light" -> pure EDLFilmLight
      "film_standard" -> pure EDLFilmStandard
      "music_light" -> pure EDLMusicLight
      "music_standard" -> pure EDLMusicStandard
      "none" -> pure EDLNone
      "speech" -> pure EDLSpeech
      e ->
        fromTextError $
          "Failure parsing Eac3DrcLine from value: '" <> e
            <> "'. Accepted values: film_light, film_standard, music_light, music_standard, none, speech"

instance ToText Eac3DrcLine where
  toText = \case
    EDLFilmLight -> "FILM_LIGHT"
    EDLFilmStandard -> "FILM_STANDARD"
    EDLMusicLight -> "MUSIC_LIGHT"
    EDLMusicStandard -> "MUSIC_STANDARD"
    EDLNone -> "NONE"
    EDLSpeech -> "SPEECH"

instance Hashable Eac3DrcLine

instance NFData Eac3DrcLine

instance ToByteString Eac3DrcLine

instance ToQuery Eac3DrcLine

instance ToHeader Eac3DrcLine

instance ToJSON Eac3DrcLine where
  toJSON = toJSONText

instance FromJSON Eac3DrcLine where
  parseJSON = parseJSONText "Eac3DrcLine"
