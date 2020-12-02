{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3DrcRf
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3DrcRf where

import Network.AWS.Prelude

-- | Eac3 Drc Rf
data Eac3DrcRf
  = EDRFilmLight
  | EDRFilmStandard
  | EDRMusicLight
  | EDRMusicStandard
  | EDRNone
  | EDRSpeech
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

instance FromText Eac3DrcRf where
  parser =
    takeLowerText >>= \case
      "film_light" -> pure EDRFilmLight
      "film_standard" -> pure EDRFilmStandard
      "music_light" -> pure EDRMusicLight
      "music_standard" -> pure EDRMusicStandard
      "none" -> pure EDRNone
      "speech" -> pure EDRSpeech
      e ->
        fromTextError $
          "Failure parsing Eac3DrcRf from value: '" <> e
            <> "'. Accepted values: film_light, film_standard, music_light, music_standard, none, speech"

instance ToText Eac3DrcRf where
  toText = \case
    EDRFilmLight -> "FILM_LIGHT"
    EDRFilmStandard -> "FILM_STANDARD"
    EDRMusicLight -> "MUSIC_LIGHT"
    EDRMusicStandard -> "MUSIC_STANDARD"
    EDRNone -> "NONE"
    EDRSpeech -> "SPEECH"

instance Hashable Eac3DrcRf

instance NFData Eac3DrcRf

instance ToByteString Eac3DrcRf

instance ToQuery Eac3DrcRf

instance ToHeader Eac3DrcRf

instance ToJSON Eac3DrcRf where
  toJSON = toJSONText

instance FromJSON Eac3DrcRf where
  parseJSON = parseJSONText "Eac3DrcRf"
