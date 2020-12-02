{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType where

import Network.AWS.Prelude

-- | Audio Only Hls Track Type
data AudioOnlyHlsTrackType
  = AlternateAudioAutoSelect
  | AlternateAudioAutoSelectDefault
  | AlternateAudioNotAutoSelect
  | AudioOnlyVariantStream
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

instance FromText AudioOnlyHlsTrackType where
  parser =
    takeLowerText >>= \case
      "alternate_audio_auto_select" -> pure AlternateAudioAutoSelect
      "alternate_audio_auto_select_default" -> pure AlternateAudioAutoSelectDefault
      "alternate_audio_not_auto_select" -> pure AlternateAudioNotAutoSelect
      "audio_only_variant_stream" -> pure AudioOnlyVariantStream
      e ->
        fromTextError $
          "Failure parsing AudioOnlyHlsTrackType from value: '" <> e
            <> "'. Accepted values: alternate_audio_auto_select, alternate_audio_auto_select_default, alternate_audio_not_auto_select, audio_only_variant_stream"

instance ToText AudioOnlyHlsTrackType where
  toText = \case
    AlternateAudioAutoSelect -> "ALTERNATE_AUDIO_AUTO_SELECT"
    AlternateAudioAutoSelectDefault -> "ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT"
    AlternateAudioNotAutoSelect -> "ALTERNATE_AUDIO_NOT_AUTO_SELECT"
    AudioOnlyVariantStream -> "AUDIO_ONLY_VARIANT_STREAM"

instance Hashable AudioOnlyHlsTrackType

instance NFData AudioOnlyHlsTrackType

instance ToByteString AudioOnlyHlsTrackType

instance ToQuery AudioOnlyHlsTrackType

instance ToHeader AudioOnlyHlsTrackType

instance ToJSON AudioOnlyHlsTrackType where
  toJSON = toJSONText

instance FromJSON AudioOnlyHlsTrackType where
  parseJSON = parseJSONText "AudioOnlyHlsTrackType"
