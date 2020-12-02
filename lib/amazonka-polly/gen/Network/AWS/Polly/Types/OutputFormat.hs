{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.OutputFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.OutputFormat where

import Network.AWS.Prelude

data OutputFormat
  = JSON
  | MP3
  | OggVorbis
  | Pcm
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

instance FromText OutputFormat where
  parser =
    takeLowerText >>= \case
      "json" -> pure JSON
      "mp3" -> pure MP3
      "ogg_vorbis" -> pure OggVorbis
      "pcm" -> pure Pcm
      e ->
        fromTextError $
          "Failure parsing OutputFormat from value: '" <> e
            <> "'. Accepted values: json, mp3, ogg_vorbis, pcm"

instance ToText OutputFormat where
  toText = \case
    JSON -> "json"
    MP3 -> "mp3"
    OggVorbis -> "ogg_vorbis"
    Pcm -> "pcm"

instance Hashable OutputFormat

instance NFData OutputFormat

instance ToByteString OutputFormat

instance ToQuery OutputFormat

instance ToHeader OutputFormat

instance ToJSON OutputFormat where
  toJSON = toJSONText

instance FromJSON OutputFormat where
  parseJSON = parseJSONText "OutputFormat"
