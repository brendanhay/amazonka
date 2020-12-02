{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.MediaFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.MediaFormat where

import Network.AWS.Prelude

data MediaFormat
  = Amr
  | Flac
  | MP3
  | MP4
  | Ogg
  | Wav
  | Webm
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

instance FromText MediaFormat where
  parser =
    takeLowerText >>= \case
      "amr" -> pure Amr
      "flac" -> pure Flac
      "mp3" -> pure MP3
      "mp4" -> pure MP4
      "ogg" -> pure Ogg
      "wav" -> pure Wav
      "webm" -> pure Webm
      e ->
        fromTextError $
          "Failure parsing MediaFormat from value: '" <> e
            <> "'. Accepted values: amr, flac, mp3, mp4, ogg, wav, webm"

instance ToText MediaFormat where
  toText = \case
    Amr -> "amr"
    Flac -> "flac"
    MP3 -> "mp3"
    MP4 -> "mp4"
    Ogg -> "ogg"
    Wav -> "wav"
    Webm -> "webm"

instance Hashable MediaFormat

instance NFData MediaFormat

instance ToByteString MediaFormat

instance ToQuery MediaFormat

instance ToHeader MediaFormat

instance ToJSON MediaFormat where
  toJSON = toJSONText

instance FromJSON MediaFormat where
  parseJSON = parseJSONText "MediaFormat"
