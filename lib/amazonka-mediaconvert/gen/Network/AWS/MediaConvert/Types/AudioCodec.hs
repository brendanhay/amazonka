{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioCodec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioCodec where

import Network.AWS.Prelude

-- | Type of Audio codec.
data AudioCodec
  = ACAC3
  | ACAac
  | ACAiff
  | ACEAC3
  | ACEAC3Atmos
  | ACMP2
  | ACMP3
  | ACOpus
  | ACPassthrough
  | ACVorbis
  | ACWav
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

instance FromText AudioCodec where
  parser =
    takeLowerText >>= \case
      "ac3" -> pure ACAC3
      "aac" -> pure ACAac
      "aiff" -> pure ACAiff
      "eac3" -> pure ACEAC3
      "eac3_atmos" -> pure ACEAC3Atmos
      "mp2" -> pure ACMP2
      "mp3" -> pure ACMP3
      "opus" -> pure ACOpus
      "passthrough" -> pure ACPassthrough
      "vorbis" -> pure ACVorbis
      "wav" -> pure ACWav
      e ->
        fromTextError $
          "Failure parsing AudioCodec from value: '" <> e
            <> "'. Accepted values: ac3, aac, aiff, eac3, eac3_atmos, mp2, mp3, opus, passthrough, vorbis, wav"

instance ToText AudioCodec where
  toText = \case
    ACAC3 -> "AC3"
    ACAac -> "AAC"
    ACAiff -> "AIFF"
    ACEAC3 -> "EAC3"
    ACEAC3Atmos -> "EAC3_ATMOS"
    ACMP2 -> "MP2"
    ACMP3 -> "MP3"
    ACOpus -> "OPUS"
    ACPassthrough -> "PASSTHROUGH"
    ACVorbis -> "VORBIS"
    ACWav -> "WAV"

instance Hashable AudioCodec

instance NFData AudioCodec

instance ToByteString AudioCodec

instance ToQuery AudioCodec

instance ToHeader AudioCodec

instance ToJSON AudioCodec where
  toJSON = toJSONText

instance FromJSON AudioCodec where
  parseJSON = parseJSONText "AudioCodec"
