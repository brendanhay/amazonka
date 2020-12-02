{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsAudioOnlyHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsAudioOnlyHeader where

import Network.AWS.Prelude

-- | Ignore this setting unless you are using FairPlay DRM with Verimatrix and you encounter playback issues. Keep the default value, Include (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to remove the audio-only headers from your audio segments.
data HlsAudioOnlyHeader
  = HAOHExclude
  | HAOHInclude
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

instance FromText HlsAudioOnlyHeader where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure HAOHExclude
      "include" -> pure HAOHInclude
      e ->
        fromTextError $
          "Failure parsing HlsAudioOnlyHeader from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText HlsAudioOnlyHeader where
  toText = \case
    HAOHExclude -> "EXCLUDE"
    HAOHInclude -> "INCLUDE"

instance Hashable HlsAudioOnlyHeader

instance NFData HlsAudioOnlyHeader

instance ToByteString HlsAudioOnlyHeader

instance ToQuery HlsAudioOnlyHeader

instance ToHeader HlsAudioOnlyHeader

instance ToJSON HlsAudioOnlyHeader where
  toJSON = toJSONText

instance FromJSON HlsAudioOnlyHeader where
  parseJSON = parseJSONText "HlsAudioOnlyHeader"
